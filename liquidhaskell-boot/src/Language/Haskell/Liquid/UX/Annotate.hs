{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

---------------------------------------------------------------------------
-- | This module contains the code that uses the inferred types to generate
-- 1. HTMLized source with Inferred Types in mouseover annotations.
-- 2. Annotations files (e.g. for vim/emacs)
-- 3. JSON files for the web-demo etc.
---------------------------------------------------------------------------

module Language.Haskell.Liquid.UX.Annotate
  ( mkOutput
  , annotate
  , tokeniseWithLoc
  , annErrors
  , dropErrorLoc
  ) where

import           Data.Hashable
import           Data.String
import           GHC                                          ( SrcSpan (..)
                                          , srcSpanStartCol
                                          , srcSpanEndCol
                                          , srcSpanStartLine
                                          , srcSpanEndLine)
import           GHC.Exts                                     (groupWith, sortWith)
import           Prelude                                      hiding (error)
import           Text.PrettyPrint.HughesPJ                    hiding (first)
import           Text.Printf

import           Data.Char                                    (isSpace)
import           Data.Function                                (on)
import           Data.List                                    (sortBy)
import           Data.Maybe                                   (mapMaybe)
import           Data.Typeable                                (Proxy (Proxy), Typeable)
import qualified Data.Typeable                                as Typeable

import           Control.Arrow                                hiding ((<+>))
-- import           Control.Applicative      ((<$>))
import           Control.Monad                                (when, forM_)

import           System.Exit                                  (ExitCode (..))
import           System.FilePath                              (takeFileName, dropFileName, (</>))
import           System.Directory                             (findExecutable)
import qualified System.Directory                             as Dir
import qualified Data.List                                    as L
import qualified Data.ByteString                              as BS
import qualified Data.Text                                    as T
import qualified Data.HashMap.Strict                          as M
import           Text.JSON                                    ( JSON (readJSON, showJSON)
                                                              , JSValue (JSString, JSArray))
import qualified Text.JSON                                    as JSON
import qualified Language.Haskell.Liquid.Misc                 as Misc
import qualified Language.Haskell.Liquid.UX.ACSS              as ACSS
import           Language.Haskell.HsColour.Classify
import           Language.Fixpoint.Utils.Files
import           Language.Fixpoint.Misc
import           Language.Haskell.Liquid.GHC.Misc
import qualified Liquid.GHC.API              as SrcLoc
import           Language.Fixpoint.Types                      hiding (panic, Error, Loc, Constant (..), Located (..))
import           Language.Fixpoint.Utils.JSON ((.=), (.:))
import qualified Language.Fixpoint.Utils.JSON as LiquidJSON
import           Language.Haskell.Liquid.Misc
import           Language.Haskell.Liquid.Types.PrettyPrint
import           Language.Haskell.Liquid.Types.RefType

import           Language.Haskell.Liquid.UX.Tidy
import           Language.Haskell.Liquid.Types                hiding (Located(..), Def(..))
-- import           Language.Haskell.Liquid.Types.Specifications
import qualified Text.Read as TR

-- | @output@ creates the pretty printed output
--------------------------------------------------------------------------------------------
mkOutput :: Config -> ErrorResult -> FixSolution -> AnnInfo (Annot SpecType) -> Output Doc
--------------------------------------------------------------------------------------------
mkOutput cfg res sol anna
  = O { o_vars   = Nothing
      -- , o_errors = []
      , o_types  = toDoc <$> annTy
      , o_templs = toDoc <$> annTmpl
      , o_bots   = mkBots    annTy
      , o_result = res
      }
  where
    annTmpl      = closeAnnots anna
    annTy        = tidySpecType Lossy <$> applySolution sol annTmpl
    toDoc        = rtypeDoc tidy
    tidy         = if shortNames cfg then Lossy else Full

-- | @annotate@ actually renders the output to files
-------------------------------------------------------------------
annotate :: Config -> [FilePath] -> Output Doc -> IO ACSS.AnnMap
-------------------------------------------------------------------
annotate cfg srcFs out
  -- TODO(matt.walker): Make this obey json!
  = do when showWarns  $ forM_ bots (printf "WARNING: Found false in %s\n" . showPpr)
       when doAnnotate $ mapM_ (doGenerate cfg tplAnnMap typAnnMap annTyp) srcFs
       return typAnnMap
    where
       tplAnnMap  = mkAnnMap cfg res annTpl
       typAnnMap  = mkAnnMap cfg res annTyp
       annTpl     = o_templs out
       annTyp     = o_types  out
       res        = o_result out
       bots       = o_bots   out
       showWarns  = not $ nowarnings    cfg
       doAnnotate = not $ noannotations cfg

doGenerate :: Config -> ACSS.AnnMap -> ACSS.AnnMap -> AnnInfo Doc -> FilePath -> IO ()
doGenerate cfg tplAnnMap typAnnMap annTyp srcF
  = do generateHtml pandocF srcF tpHtmlF tplAnnMap
       generateHtml pandocF srcF tyHtmlF typAnnMap
       writeFile         vimF  $ vimAnnot cfg annTyp
       BS.writeFile      jsonF $ LiquidJSON.encode typAnnMap
    where
       pandocF    = pandocHtml cfg
       tyHtmlF    = extFileName Html                   srcF
       tpHtmlF    = extFileName Html $ extFileName Cst srcF
       _annF      = extFileName Annot srcF
       jsonF      = extFileName Json  srcF
       vimF       = extFileName Vim   srcF

mkBots :: Reftable r => AnnInfo (RType c tv r) -> [GHC.SrcSpan]
mkBots (AI m) = [ src | (src, (Just _, t) : _) <- sortBy (ordSrcSpan `on` fst) $ M.toList m
                      , isFalse (rTypeReft t) ]

-- | Like 'copyFile' from 'System.Directory', but ensure that the parent /temporary/ directory
-- (i.e. \".liquid\") exists on disk, creating it if necessary.
copyFileCreateParentDirIfMissing :: FilePath -> FilePath -> IO ()
copyFileCreateParentDirIfMissing src tgt = do
  Dir.createDirectoryIfMissing False $ tempDirectory tgt
  Dir.copyFile src tgt

writeFilesOrStrings :: FilePath -> [Either FilePath String] -> IO ()
writeFilesOrStrings tgtFile = mapM_ $ either (`copyFileCreateParentDirIfMissing` tgtFile) (tgtFile `appendFile`)

generateHtml :: Bool -> FilePath -> FilePath -> ACSS.AnnMap -> IO ()
generateHtml pandocF srcF htmlF annm = do
  src     <- Misc.sayReadFile srcF
  let lhs  = isExtFile LHs srcF
  let body      = {-# SCC "hsannot" #-} ACSS.hsannot False (Just tokAnnot) lhs (src, annm)
  cssFile <- getCssPath
  copyFileCreateParentDirIfMissing cssFile (dropFileName htmlF </> takeFileName cssFile)
  renderHtml (pandocF && lhs) htmlF srcF (takeFileName cssFile) body

renderHtml :: Bool -> FilePath -> String -> String -> String -> IO ()
renderHtml True  = renderPandoc
renderHtml False = renderDirect

-------------------------------------------------------------------------
-- | Pandoc HTML Rendering (for lhs + markdown source) ------------------
-------------------------------------------------------------------------
renderPandoc :: FilePath -> String -> String -> String -> IO ()
renderPandoc htmlFile srcFile css body = do
  renderFn <- maybe renderDirect renderPandoc' <$> findExecutable "pandoc"
  renderFn htmlFile srcFile css body

renderPandoc' :: FilePath -> FilePath -> FilePath -> String -> String -> IO ()
renderPandoc' pandocPath htmlFile srcFile css body = do
  _  <- writeFile mdFile $ pandocPreProc body
  ec <- executeShellCommand "pandoc" cmd
  writeFilesOrStrings htmlFile [Right (cssHTML css)]
  checkExitCode cmd ec
  where
    mdFile = extFileName Mkdn srcFile
    cmd    = pandocCmd pandocPath mdFile htmlFile

checkExitCode :: Monad m => String -> ExitCode -> m ()
checkExitCode _    ExitSuccess    = return ()
checkExitCode cmd (ExitFailure n) = panic Nothing $ "cmd: " ++ cmd ++ " failure code " ++ show n

pandocCmd :: FilePath -> FilePath -> FilePath -> String
pandocCmd -- pandocPath mdFile htmlFile
  = printf "%s -f markdown -t html %s > %s" -- pandocPath mdFile htmlFile

pandocPreProc :: String -> String
pandocPreProc  = T.unpack
               . strip beg code
               . strip end code
               . strip beg spec
               . strip end spec
               . T.pack
  where
    beg, end, code, spec :: String
    beg        = "begin"
    end        = "end"
    code       = "code"
    spec       = "spec"
    strip x y  = T.replace (T.pack $ printf "\\%s{%s}" x y) T.empty


-------------------------------------------------------------------------
-- | Direct HTML Rendering (for non-lhs/markdown source) ----------------
-------------------------------------------------------------------------

-- More or less taken from hscolour

renderDirect :: FilePath -> String -> String -> String -> IO ()
renderDirect htmlFile srcFile css body
  = writeFile htmlFile $! (topAndTail full srcFile css $! body)
    where full = True -- False  -- TODO: command-line-option

-- | @topAndTail True@ is used for standalone HTML; @topAndTail False@ for embedded HTML
topAndTail :: Bool -> String -> String -> String -> String
topAndTail True  title css = (htmlHeader title css ++) . (++ htmlClose)
topAndTail False _    _    = id

-- Use this for standalone HTML
htmlHeader :: String -> String -> String
htmlHeader title css = unlines
  [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">"
  , "<html>"
  , "<head>"
  , "<title>" ++ title ++ "</title>"
  , "</head>"
  , cssHTML css
  , "<body>"
  , "<hr>"
  , "Put mouse over identifiers to see inferred types"
  ]

htmlClose :: IsString a => a
htmlClose  = "\n</body>\n</html>"

cssHTML :: String -> String
cssHTML css = unlines
  [ "<head>"
  , "<link type='text/css' rel='stylesheet' href='"++ css ++ "' />"
  , "</head>"
  ]

------------------------------------------------------------------------------
-- | Building Annotation Maps ------------------------------------------------
------------------------------------------------------------------------------

-- | This function converts our annotation information into that which
--   is required by `Language.Haskell.Liquid.ACSS` to generate mouseover
--   annotations.

mkAnnMap :: Config -> ErrorResult -> AnnInfo Doc -> ACSS.AnnMap
mkAnnMap cfg res ann     = ACSS.Ann
                             { ACSS.types   = mkAnnMapTyp cfg ann
                             , ACSS.errors  = mkAnnMapErr res
                             , ACSS.status  = mkStatus res
                             , ACSS.sptypes = mkAnnMapBinders cfg ann
                             }

mkStatus :: FixResult t -> ACSS.Status
mkStatus (Safe _)        = ACSS.Safe
mkStatus (Unsafe _ _)    = ACSS.Unsafe
mkStatus (Crash _ _)     = ACSS.Error



mkAnnMapErr :: PPrint (TError t)
            => FixResult (TError t) -> [(Loc, Loc, String)]
mkAnnMapErr (Unsafe _ ls) = mapMaybe cinfoErr ls
mkAnnMapErr (Crash ls _)  = mapMaybe (cinfoErr . fst) ls
mkAnnMapErr _             = []

cinfoErr :: PPrint (TError t) => TError t -> Maybe (Loc, Loc, String)
cinfoErr e = case pos e of
               SrcLoc.RealSrcSpan l _ -> Just (srcSpanStartLoc l, srcSpanEndLoc l, showpp e)
               _                      -> Nothing


-- mkAnnMapTyp :: (RefTypable a c tv r, RefTypable a c tv (), PPrint tv, PPrint a) =>Config-> AnnInfo (RType a c tv r) -> M.HashMap Loc (String, String)
mkAnnMapTyp :: Config -> AnnInfo Doc -> M.HashMap Loc (String, String)
mkAnnMapTyp cfg z = M.fromList $ map (first srcSpanStartLoc) $ mkAnnMapBinders cfg z

mkAnnMapBinders :: Config -> AnnInfo Doc -> [(SrcLoc.RealSrcSpan, (String, String))]
mkAnnMapBinders cfg (AI m)
  = map (second bindStr . head . sortWith (srcSpanEndCol . fst))
  $ groupWith (lineCol . fst) locBinds
  where
    locBinds       = [ (l, x) | (SrcLoc.RealSrcSpan l _, x:_) <- M.toList m, oneLine l]
    bindStr (x, v) = (maybe "_" (symbolString . shorten . symbol) x, render v)
    shorten        = if shortNames cfg then dropModuleNames else id

closeAnnots :: AnnInfo (Annot SpecType) -> AnnInfo SpecType
closeAnnots = closeA . filterA . collapseA

closeA :: AnnInfo (Annot b) -> AnnInfo b
closeA a@(AI m)   = cf <$> a
  where
    cf (AnnLoc l)  = case m `mlookup` l of
                      [(_, AnnUse t)] -> t
                      [(_, AnnDef t)] -> t
                      [(_, AnnRDf t)] -> t
                      _               -> panic Nothing $ "malformed AnnInfo: " ++ showPpr l
    cf (AnnUse t) = t
    cf (AnnDef t) = t
    cf (AnnRDf t) = t

filterA :: AnnInfo (Annot t) -> AnnInfo (Annot t)
filterA (AI m) = AI (M.filter ff m)
  where
    ff [(_, AnnLoc l)] = l `M.member` m
    ff _               = True

collapseA :: AnnInfo (Annot t) -> AnnInfo (Annot t)
collapseA (AI m) = AI (fmap pickOneA m)

pickOneA :: [(t, Annot t1)] -> [(t, Annot t1)]
pickOneA xas = case (rs, ds, ls, us) of
                 (x:_, _, _, _) -> [x]
                 (_, x:_, _, _) -> [x]
                 (_, _, x:_, _) -> [x]
                 (_, _, _, x:_) -> [x]
                 (_, _, _, _  ) -> [ ]
  where
    rs = [x | x@(_, AnnRDf _) <- xas]
    ds = [x | x@(_, AnnDef _) <- xas]
    ls = [x | x@(_, AnnLoc _) <- xas]
    us = [x | x@(_, AnnUse _) <- xas]

------------------------------------------------------------------------------
-- | Tokenizing Refinement Type Annotations in @-blocks ----------------------
------------------------------------------------------------------------------

-- | The token used for refinement symbols inside the highlighted types in @-blocks.
refToken :: TokenType
refToken = Keyword

-- | The top-level function for tokenizing @-block annotations. Used to
-- tokenize comments by ACSS.
tokAnnot :: String -> [(TokenType, String)]
tokAnnot s
  = case trimLiquidAnnot s of
      Just (l, body, r) -> [(refToken, l)] ++ tokBody body ++ [(refToken, r)]
      Nothing           -> [(Comment, s)]

trimLiquidAnnot :: String -> Maybe (String, String, String)
trimLiquidAnnot ('{':'-':'@':ss)
  | drop (length ss - 3) ss == "@-}"
  = Just (liquidBegin, take (length ss - 3) ss, liquidEnd)
trimLiquidAnnot _
  = Nothing

tokBody :: String -> [(TokenType, String)]
tokBody s
  | isData s  = tokenise s
  | isType s  = tokenise s
  | isIncl s  = tokenise s
  | isMeas s  = tokenise s
  | otherwise = tokeniseSpec s

isMeas :: String -> Bool
isMeas = spacePrefix "measure"

isData :: String -> Bool
isData = spacePrefix "data"

isType :: String -> Bool
isType = spacePrefix "type"

isIncl :: String -> Bool
isIncl = spacePrefix "include"

{-@ spacePrefix :: String -> s:String -> Bool / [len s] @-}
spacePrefix :: String -> String -> Bool
spacePrefix str s@(c:cs)
  | isSpace c   = spacePrefix str cs
  | otherwise   = take (length str) s == str
spacePrefix _ _ = False


tokeniseSpec :: String -> [(TokenType, String)]
tokeniseSpec       = tokAlt . chopAltDBG
  where
    tokAlt (s:ss)  = tokenise s ++ tokAlt' ss
    tokAlt _       = []
    tokAlt' (s:ss) = (refToken, s) : tokAlt ss
    tokAlt' _      = []

chopAltDBG :: String -> [String]
chopAltDBG y = filter (/= "")
             $ concatMap (chopAlts [("{", ":"), ("|", "}")])
             $ chopAlts [("<{", "}>"), ("{", "}")] y


------------------------------------------------------------------------
-- | JSON: Annotation Data Types ---------------------------------------
------------------------------------------------------------------------

newtype Assoc k a = Asc (M.HashMap k a)
type AnnTypes     = Assoc Int (Assoc Int Annot1)
newtype AnnErrors = AnnErrors [(Loc, Loc, String)]
data Annot1       = A1  { ident :: String
                        , ann   :: String
                        , row   :: Int
                        , col   :: Int
                        }

------------------------------------------------------------------------
-- | Creating Vim Annotations ------------------------------------------
------------------------------------------------------------------------
vimAnnot     :: Config -> AnnInfo Doc -> String
vimAnnot cfg = L.intercalate "\n" . map vimBind . mkAnnMapBinders cfg

vimBind :: (Show a, PrintfType t) => (SrcLoc.RealSrcSpan, (String, a)) -> t
vimBind (sp, (v, ann)) = printf "%d:%d-%d:%d::%s" l1 c1 l2 c2 (v ++ " :: " ++ show ann)
  where
    l1  = srcSpanStartLine sp
    c1  = srcSpanStartCol  sp
    l2  = srcSpanEndLine   sp
    c2  = srcSpanEndCol    sp

------------------------------------------------------------------------
-- | JSON Instances ----------------------------------------------------
------------------------------------------------------------------------

instance JSON ACSS.Status where
  showJSON ACSS.Safe   = "safe"
  showJSON ACSS.Unsafe = "unsafe"
  showJSON ACSS.Error  = "error"
  showJSON ACSS.Crash  = "crash"

  readJSON (JSString s) = case s of
    "safe" -> JSON.Ok ACSS.Safe
    "unsafe" -> JSON.Ok ACSS.Unsafe
    "error" -> JSON.Ok ACSS.Error
    "crash" -> JSON.Ok ACSS.Crash
    other -> JSON.Error $ "Expected one of (safe | unsafe | error | crash), received: " ++ show other
  readJSON other = JSON.Error $ "Expected string, received: " ++ show other

instance JSON Annot1 where
  showJSON (A1 i a r c) =
    JSON.makeObj [ "ann"   .= a
                 , "col"   .= c
                 , "ident" .= i
                 , "row"   .= r
                 ]

  readJSON = LiquidJSON.readJSONObj $ \v ->
    A1
      <$> v .: "ident"
      <*> v .: "ann"
      <*> v .: "row"
      <*> v .: "col"

instance JSON Loc where
  showJSON (L (l, c)) =
    JSON.makeObj [ "column"   .= c
                 , "line"     .= l
                 ]

  readJSON = LiquidJSON.readJSONObj $ \v ->
    L <$>
      ( (,)
          <$> v .: "line"
          <*> v .: "column"
      )

instance JSON AnnErrors where
  showJSON (AnnErrors errors) = JSArray (toJ <$> errors)
    where
      toJ (l,l',s) =
        JSON.makeObj [ "message" .= dropErrorLoc s
                     , "start"   .= l
                     , "stop"    .= l'
                     ]

  readJSON (JSArray arr) = AnnErrors <$> traverse fromJ arr
    where
      fromJ = LiquidJSON.readJSONObj $ \v ->
        (,,)
          <$> v .: "start"
          <*> v .: "stop"
          <*> v .: "message"
  readJSON other = JSON.Error $ "Expected array, received: " ++ show other

dropErrorLoc :: String -> String
dropErrorLoc msg
  | null msg' = msg
  | otherwise = tail msg'
  where
    (_, msg') = break (' ' ==) msg

-- Typeable constraint for better error message. Can be removed if it is
-- cumbersome, especially since readJSON is not currently in-use.
instance (Hashable k, JSON a, Read k, Show k, Typeable k) => JSON (Assoc k a) where
  showJSON (Asc kas) = JSON.makeObj [ tshow' k .= a | (k, a) <- M.toList kas ]
    where
      tshow'       = fromString . show

  readJSON (JSON.JSObject obj) = do
    let assocXs :: [(String, JSValue)] = JSON.fromJSObject obj
    assocXs' :: [(k', a)] <- traverse readKey assocXs
    pure $ Asc . M.fromList $ assocXs'
    where
      readKey :: (String, JSValue) -> JSON.Result (k, a)
      readKey (k, v) =
        -- read key first
        case TR.readEither k of
          Left err ->
            JSON.Error $
              mconcat
                [ "Could not read json key '",
                  k,
                  "' as type '",
                  show (Typeable.typeRep (Proxy :: Proxy k)),
                  "': ",
                  err
                ]
          -- if successful, read json value
          (Right k') -> (k',) <$> readJSON v
  readJSON bad = fail $  "Expected object, received: " ++ show bad

-- | Auxillary type on which to hang AnnMap.sptypes' JSON instance.
-- This is really only useful for implementing the currently unused
-- readJSON function.
newtype SpanTypeJSON =
  MkSpanTypeJSON { unMkSpanTypeJSON :: (SrcLoc.RealSrcSpan, (String, String)) }

instance JSON SpanTypeJSON where
  showJSON (MkSpanTypeJSON (sp, (ident, ann))) =
    JSON.makeObj
      [ "ann"   .= ann
      , "ident" .= ident
      , "start" .= start
      , "stop"  .= stop
      ]
    where
      start = srcSpanStartLoc sp
      stop = srcSpanEndLoc sp

  readJSON = LiquidJSON.readJSONObj $ \v -> do
    start <- v .: "start"
    stop <- v .: "stop"
    ident <- v .: "ident"
    ann <- v .: "ann"

    pure $ MkSpanTypeJSON (locsSrcSpan start stop, (ident, ann))

instance JSON ACSS.AnnMap where
  showJSON a = JSON.makeObj
    [ "errors"  .= annErrors    a
    , "sptypes" .= (MkSpanTypeJSON <$> ACSS.sptypes a)
    , "status"  .= ACSS.status  a
    , "types"   .= annTypes     a
    ]

  readJSON = LiquidJSON.readJSONObj $ \v ->
    ACSS.Ann
      <$> (unAnnTypes <$> v .: "types")
      <*> (unAnnErrors <$> v .: "errors")
      <*> v .: "status"
      <*> (fmap unMkSpanTypeJSON <$> v .: "sptypes")

annErrors :: ACSS.AnnMap -> AnnErrors
annErrors = AnnErrors . ACSS.errors

unAnnErrors :: AnnErrors -> [(Loc, Loc, String)]
unAnnErrors (AnnErrors errs) = errs

annTypes         :: ACSS.AnnMap -> AnnTypes
annTypes a       = grp [(l, c, ann1 l c x s) | (l, c, x, s) <- binders']
  where
    ann1 :: Int -> Int -> String -> String -> Annot1
    ann1 l c x s = A1 x s l c

    grp :: [(Int, Int, Annot1)] -> Assoc Int (Assoc Int Annot1)
    grp          = L.foldl' (\m (r,c,x) -> ins r c x m) (Asc M.empty)

    binders' :: [(Int, Int, String, String)]
    binders'     = [(l, c, x, s) | (L (l, c), (x, s)) <- M.toList $ ACSS.types a]

ins :: (Eq k, Eq k1, Hashable k, Hashable k1)
    => k -> k1 -> a -> Assoc k (Assoc k1 a) -> Assoc k (Assoc k1 a)
ins r c x (Asc m)  = Asc (M.insert r (Asc (M.insert c x rm)) m)
  where
    Asc rm         = M.lookupDefault (Asc M.empty) r m

-- The inverse of 'annTypes' above.
unAnnTypes :: AnnTypes -> M.HashMap Loc (String, String)
unAnnTypes = M.fromList . toLocList
  where
    toLocList :: AnnTypes -> [(Loc, (String, String))]
    toLocList (Asc m) = M.foldlWithKey' f [] m
      where
        f :: [(Loc, (String, String))] -> Int -> Assoc Int Annot1 -> [(Loc, (String, String))]
        f acc k (Asc m1) =
          acc ++ ((\(k1, A1 x s _ _) -> (L (k, k1), (x, s))) <$> M.toList m1)

tokeniseWithLoc :: String -> [(TokenType, String, Loc)]
tokeniseWithLoc = ACSS.tokeniseWithLoc (Just tokAnnot)

--------------------------------------------------------------------------------
-- | LH Related Stuff ----------------------------------------------------------
--------------------------------------------------------------------------------

{-@ LIQUID "--diffcheck" @-}

{-@ type ListNE a    = {v:[a] | 0 < len v}  @-}
{-@ type ListN  a N  = {v:[a] | len v == N} @-}
{-@ type ListXs a Xs = ListN a {len Xs}     @-}

{-@ assume GHC.Exts.sortWith :: Ord b => (a -> b) -> xs:[a] -> ListXs a xs @-}
{-@ assume GHC.Exts.groupWith :: Ord b => (a -> b) -> [a] -> [ListNE a] @-}

--------------------------------------------------------------------------------
-- | A Little Unit Test --------------------------------------------------------
--------------------------------------------------------------------------------

_anns :: AnnTypes
_anns =
  mkAssoc
    [ (5, mkAssoc
            [ ( 14, A1 { ident = "foo"
                       , ann   = "int -> int"
                       , row   = 5
                       , col   = 14
                       })
            ]
      )
    , (9, mkAssoc
            [ ( 22, A1 { ident = "map"
                       , ann   = "(a -> b) -> [a] -> [b]"
                       , row   = 9
                       , col   = 22
                       })
            , ( 28, A1 { ident = "xs"
                       , ann   = "[b]"
                       , row   = 9
                       , col   = 28
                       })
            ])
    ]

mkAssoc :: (Eq k, Hashable k) => [(k, a)] -> Assoc k a
mkAssoc = Asc . M.fromList
