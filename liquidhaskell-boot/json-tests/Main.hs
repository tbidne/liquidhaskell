{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (sequenceA_)
import qualified Data.HashMap.Strict as M
import Data.String (IsString (fromString))
import qualified Gens
import Hedgehog (Property, (===))
import qualified Hedgehog as H
import Language.Fixpoint.Solver.Stats
  ( Stats (Stats, numBrkt, numChck, numCstr, numIter, numVald),
  )
import Language.Fixpoint.Types (SourcePos (SourcePos))
import qualified Language.Fixpoint.Types as F
import Language.Fixpoint.Types.Spans (mkPos)
import qualified Language.Fixpoint.Utils.JSON as LiquidJSON
import Language.Haskell.Liquid.GHC.Misc
  ( Loc (L),
    locsSrcSpan,
    realSrcSpanToSrcSpan,
  )
import Language.Haskell.Liquid.Types.Errors (UserError, errSaved, renderTError)
import Language.Haskell.Liquid.Types.Types
  ( AnnInfo (AI),
    ErrorResult,
    Output
      ( O,
        o_bots,
        o_result,
        o_templs,
        o_types,
        o_vars
      ),
    TError (ErrFail, ErrSaved, msg),
  )
import Language.Haskell.Liquid.UX.ACSS
  ( AnnMap
      ( Ann,
        errors,
        sptypes,
        status,
        types
      ),
    Status (Safe),
  )
import Language.Haskell.Liquid.UX.Annotate (dropErrorLoc)
import Language.Haskell.Liquid.UX.DiffCheck ()
import Liquid.GHC.API
  ( SrcSpan (UnhelpfulSpan),
    UnhelpfulSpanReason (UnhelpfulNoLocationInfo, UnhelpfulOther),
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass), goldenVsFile)
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.Hedgehog as TH
import Text.JSON (Result (Ok))
import qualified Text.JSON as JSON
import Text.PrettyPrint.HughesPJ (Doc)

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.localOption OnPass $
      testGroup
        "JSON Tests"
        [ specs,
          properties
        ]

specs :: TestTree
specs =
  testGroup
    "Specs"
    [ testErrorSavedEncodeAeson,
      testErrorSavedEncode,
      testSourcePosEncodeAeson,
      testSourcePosEncode,
      testOutputDocEncodeAeson,
      testOutputDocEncode,
      testAnnMapEncodeAeson,
      testAnnMapEncode
    ]

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ testSourcePosRoundtrip,
      testOutputDocRoundtrip,
      testAnnMapRoundtrip
    ]

testErrorSavedEncodeAeson :: TestTree
testErrorSavedEncodeAeson = goldenVsFile "Aeson TError Saved encode" gpath apath $ do
  let encoded = Aeson.encode terror
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "terror-saved"

    terror :: TError ()
    terror = ErrSaved (toSrcSpan (0, 3) (1, 4)) "a name" "message"

testErrorSavedEncode :: TestTree
testErrorSavedEncode = goldenVsFile "JSON TError Saved encode" gpath apath $ do
  let encoded = LiquidJSON.encode terror
  BS.writeFile apath encoded
  where
    (gpath, apath) = getGoldenPaths "terror-saved"

    terror :: TError ()
    terror = ErrSaved (toSrcSpan (0, 3) (1, 4)) "a name" "message"

testSourcePosEncodeAeson :: TestTree
testSourcePosEncodeAeson = goldenVsFile "Aeson SourcePos encode" gpath apath $ do
  let encoded = Aeson.encode sourcePos
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "source-pos"

    sourcePos :: SourcePos
    sourcePos = SourcePos "file" (mkPos 4) (mkPos 3)

testSourcePosEncode :: TestTree
testSourcePosEncode = goldenVsFile "JSON SourcePos encode" gpath apath $ do
  let encoded = LiquidJSON.encode sourcePos
  Ok sourcePos @=? LiquidJSON.decode encoded
  BS.writeFile apath encoded
  where
    (gpath, apath) = getGoldenPaths "source-pos"

    sourcePos :: SourcePos
    sourcePos = SourcePos "file" (mkPos 4) (mkPos 3)

testOutputDocEncodeAeson :: TestTree
testOutputDocEncodeAeson = goldenVsFile "Aeson Output Doc encode" gpath apath $ do
  let encoded = Aeson.encode outputDoc
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "output-doc"

testOutputDocEncode :: TestTree
testOutputDocEncode = goldenVsFile "JSON Output Doc encode" gpath apath $ do
  let encoded = LiquidJSON.encode outputDoc
  BS.writeFile apath encoded
  where
    (gpath, apath) = getGoldenPaths "output-doc"

testAnnMapEncodeAeson :: TestTree
testAnnMapEncodeAeson = goldenVsFile "Aeson AnnMap encode" gpath apath $ do
  let encoded = Aeson.encode annMap
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "ann-map"

testAnnMapEncode :: TestTree
testAnnMapEncode = goldenVsFile "JSON AnnMap encode" gpath apath $ do
  let encoded = LiquidJSON.encode annMap
  BS.writeFile apath encoded
  where
    (gpath, apath) = getGoldenPaths "ann-map"

testSourcePosRoundtrip :: TestTree
testSourcePosRoundtrip = TH.testPropertyNamed "decode . encode SourcePos" "testSourcePosRoundtrip" $
  H.property $ do
    srcPos <- H.forAll Gens.genSourcePos
    let encoded = LiquidJSON.encode srcPos
        result = LiquidJSON.decode encoded

    aesonJsonHedgehog srcPos

    H.annotateShow encoded

    Ok srcPos === result

testOutputDocRoundtrip :: TestTree
testOutputDocRoundtrip = TH.testPropertyNamed desc "testOutputDocRoundtrip" $
  H.property $ do
    doc <- H.forAll Gens.genOutputDoc
    let encoded = LiquidJSON.encode doc
        expected = fixOutputDeserializeFields doc
        result = fixOutputDeserializeFields <$> LiquidJSON.decode encoded

    aesonJsonHedgehog doc

    H.annotateShow encoded

    Ok expected === result
  where
    desc = "decode . encode Output __mostly__ roundtrips"

testAnnMapRoundtrip :: TestTree
testAnnMapRoundtrip = TH.testPropertyNamed desc "testAnnMapRoundtrip" $
  H.property $ do
    amap <- H.forAll Gens.genAnnMap
    let encoded = LiquidJSON.encode amap
        expected = fixAnnMapDeserializeFields amap
        result = fixAnnMapDeserializeFields <$> LiquidJSON.decode encoded

    aesonJsonHedgehog amap

    H.annotateShow encoded

    Ok expected === result
  where
    desc = "decode . encode AnnMap __mostly__ roundtrips"

outputDoc :: Output Doc
outputDoc =
  O
    { o_vars = Just ["str", "another"],
      o_types =
        AI $
          M.fromList
            [ (toSrcSpan (0, 0) (1, 2), [(Nothing, "doc")]),
              (toSrcSpan (4, 2) (6, 3), [(Just "text", "doc2"), (Just "text", "another")]),
              (UnhelpfulSpan (UnhelpfulOther $ fromString "other"), [(Just "is", "other")]),
              (toSrcSpan (8, 2) (7, 3), [])
            ],
      o_templs =
        AI $
          M.fromList
            [ (toSrcSpan (4, 2) (6, 3), [(Just "t", "d"), (Just "t2", "d2")]),
              (UnhelpfulSpan UnhelpfulNoLocationInfo, [(Just "bad", "doc")]),
              (toSrcSpan (3, 7) (6, 1), []),
              (toSrcSpan (4, 7) (1, 2), [(Nothing, "doc")])
            ],
      o_bots =
        [ toSrcSpan (0, 0) (3, 4),
          toSrcSpan (1, 5) (2, 4)
        ],
      o_result = F.Safe stats
    }
  where
    stats =
      Stats
        { numCstr = 1,
          numIter = 2,
          numBrkt = 3,
          numChck = 4,
          numVald = 5
        }

toSrcSpan :: (Int, Int) -> (Int, Int) -> SrcSpan
toSrcSpan s e = realSrcSpanToSrcSpan $ locsSrcSpan (L s) (L e)

annMap :: AnnMap
annMap =
  Ann
    { types =
        M.fromList
          [ (L (0, 5), ("a", "b")),
            (L (2, 5), ("foo", "bar baz"))
          ],
      errors =
        [ (L (2, 4), L (2, 9), "a"),
          (L (3, 7), L (1, 7), "\n"),
          (L (2, 4), L (0, 5), "ccc")
        ],
      status = Safe,
      sptypes =
        [ (locsSrcSpan (L (1, 3)) (L (0, 7)), ("str", "")),
          (locsSrcSpan (L (5, 2)) (L (4, 9)), ("a", "another str"))
        ]
    }

-- | Output serialization is not injective. In particular, UnhelpfulSpan
-- SrcSpans are mapped to UnhelpfulNoLocationInfo. Thus we need to perform the
-- same transformation here, so we can check that the rest of the fields are
-- de(serialized) correctly.
fixOutputDeserializeFields :: Output Doc -> Output Doc
fixOutputDeserializeFields (O {o_vars, o_bots, o_result, o_types, o_templs}) =
  O
    { o_vars,
      o_types = mapAnnInfo o_types,
      o_templs = mapAnnInfo o_templs,
      o_bots = fixSrcSpan <$> o_bots,
      o_result = mapResult o_result
    }
  where
    mapAnnInfo :: AnnInfo a -> AnnInfo a
    mapAnnInfo (AI mp) = AI (M.mapKeys fixSrcSpan mp)

    mapResult :: ErrorResult -> ErrorResult
    mapResult s@(F.Safe _) = s
    mapResult (F.Unsafe stats xs) = F.Unsafe stats (mapUserError <$> xs)
    mapResult (F.Crash xs str) = F.Crash (mapCrash <$> xs) str

    mapCrash :: (UserError, Maybe String) -> (UserError, Maybe String)
    mapCrash (err, mstr) = (mapUserError err, mstr)

    mapUserError :: UserError -> UserError
    mapUserError err =
      errSaved (UnhelpfulSpan UnhelpfulNoLocationInfo) (renderTError err)

    fixSrcSpan :: SrcSpan -> SrcSpan
    fixSrcSpan (UnhelpfulSpan _) = UnhelpfulSpan UnhelpfulNoLocationInfo
    fixSrcSpan other = other

fixAnnMapDeserializeFields :: AnnMap -> AnnMap
fixAnnMapDeserializeFields (Ann {types, errors, status, sptypes}) =
  Ann
    { types,
      errors = mapError <$> errors,
      status,
      sptypes
    }
  where
    mapError :: (Loc, Loc, String) -> (Loc, Loc, String)
    mapError (x, y, s) = (x, y, dropErrorLoc s)

aesonJsonHedgehog :: (JSON.JSON a, Aeson.ToJSON a) => a -> H.PropertyT IO ()
aesonJsonHedgehog = sequenceA_ . LiquidJSON.aesonJsonEq (===) hpass hfail
  where
    hpass = (pure ())
    hfail s = do
      H.annotate s
      H.failure

getGoldenPaths :: FilePath -> (FilePath, FilePath)
getGoldenPaths fileName = (golden, actual)
  where
    prefix = jsonDir </> fileName
    golden = prefix ++ "-golden.json"
    actual = prefix ++ "-actual.json"

jsonDir :: FilePath
jsonDir = "json-tests" </> "data"
