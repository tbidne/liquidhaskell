{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as M
import Data.String (IsString (fromString))
import qualified Gens
import qualified Hedgehog as H
import Language.Fixpoint.Solver.Stats
  ( Stats (Stats, numBrkt, numChck, numCstr, numIter, numVald),
  )
import Language.Fixpoint.Types (SourcePos (SourcePos))
import qualified Language.Fixpoint.Types as F
import Language.Fixpoint.Types.Spans (mkPos)
import Language.Haskell.Liquid.GHC.Misc
  ( Loc (L),
    locsSrcSpan,
    realSrcSpanToSrcSpan,
  )
import Language.Haskell.Liquid.Types.Types
  ( AnnInfo (AI),
    Output (O, o_bots, o_result, o_templs, o_types, o_vars),
    TError (ErrSaved),
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
import Language.Haskell.Liquid.UX.Annotate ()
import Language.Haskell.Liquid.UX.DiffCheck ()
import Liquid.GHC.API
  ( SrcSpan (UnhelpfulSpan),
    UnhelpfulSpanReason (UnhelpfulNoLocationInfo, UnhelpfulOther),
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass), goldenVsFile)
import qualified Test.Tasty.Hedgehog as TH
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
    [ testErrorSavedEncode,
      testSourcePosEncode,
      testOutputDocEncode,
      testAnnMapEncode
    ]

testErrorSavedEncode :: TestTree
testErrorSavedEncode = goldenVsFile "Tests TError Saved encode" gpath apath $ do
  let encoded = Aeson.encode terror
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "terror-saved"

    terror :: TError ()
    terror = ErrSaved (toSrcSpan (0, 3) (1, 4)) "a name" "message"

testSourcePosEncode :: TestTree
testSourcePosEncode = goldenVsFile "Tests SourcePos encode" gpath apath $ do
  let encoded = Aeson.encode sourcePos
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "source-pos"

    sourcePos :: SourcePos
    sourcePos = SourcePos "file" (mkPos 4) (mkPos 3)

testOutputDocEncode :: TestTree
testOutputDocEncode = goldenVsFile "Tests Output Doc encode" gpath apath $ do
  let encoded = Aeson.encode outputDoc
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "output-doc"

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

testAnnMapEncode :: TestTree
testAnnMapEncode = goldenVsFile "Tests AnnMap encode" gpath apath $ do
  let encoded = Aeson.encode annMap
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "ann-map"

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

getGoldenPaths :: FilePath -> (FilePath, FilePath)
getGoldenPaths fileName = (golden, actual)
  where
    prefix = jsonDir </> fileName
    golden = prefix ++ "-golden.json"
    actual = prefix ++ "-actual.json"

jsonDir :: FilePath
jsonDir = "json-tests" </> "data"

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ testEncodeSourcePos,
      testEncodeOutputDoc,
      testEncodeAnnMap
    ]

testEncodeSourcePos :: TestTree
testEncodeSourcePos = TH.testPropertyNamed desc "testEncodeSourcePos" $
  H.property $ do
    srcPos <- H.forAll Gens.genSourcePos
    let encoded = BSL.toStrict $ Aeson.encode srcPos

    void $ H.evalNF encoded
  where
    desc = "encode SourcePos succeeds"

testEncodeOutputDoc :: TestTree
testEncodeOutputDoc = TH.testPropertyNamed desc "testEncodeOutputDoc" $
  H.property $ do
    odoc <- H.forAll Gens.genOutputDoc
    let encoded = BSL.toStrict $ Aeson.encode odoc

    void $ H.evalNF encoded
  where
    desc = "encode Output Doc succeeds"

testEncodeAnnMap :: TestTree
testEncodeAnnMap = TH.testPropertyNamed desc "testEncodeAnnMap" $
  H.property $ do
    amap <- H.forAll Gens.genAnnMap
    let encoded = BSL.toStrict $ Aeson.encode amap

    void $ H.evalNF encoded
  where
    desc = "encode arbitrary AnnMap succeeds"
