{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Gens
  ( genSourcePos,
    genOutputDoc,
    genAnnMap,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Fixpoint.Solver.Stats (Stats (Stats))
import Language.Fixpoint.Types (SubcId, Symbol)
import qualified Language.Fixpoint.Types.Errors as FixErrors
import Language.Fixpoint.Types.Refinements (Expr (ESym), SymConst (SL))
import Language.Fixpoint.Types.Spans (SourcePos (SourcePos), mkPos)
import Language.Haskell.Liquid.GHC.Misc (Loc (L), locsSrcSpan, realSrcSpanToSrcSpan)
import Language.Haskell.Liquid.Types (AnnInfo (AI), Oblig, Output (O))
import Language.Haskell.Liquid.Types.Errors (Oblig (OCons, OInv, OTerm), TError (..), WithModel (NoModel, WithModel))
import qualified Language.Haskell.Liquid.Types.Errors as Errors
import Language.Haskell.Liquid.Types.Types (ErrorResult, UserError)
import Language.Haskell.Liquid.UX.ACSS
  ( AnnMap (Ann),
    Status
      ( Crash,
        Error,
        Safe,
        Unsafe
      ),
  )
import Liquid.GHC.API
  ( RealSrcSpan,
    SrcSpan (UnhelpfulSpan),
    UnhelpfulSpanReason
      ( UnhelpfulGenerated,
        UnhelpfulInteractive,
        UnhelpfulNoLocationInfo,
        UnhelpfulOther,
        UnhelpfulWiredIn
      ),
  )
import qualified Text.Megaparsec as MP
import qualified Text.PrettyPrint as Pretty
import Text.PrettyPrint.HughesPJ (Doc)

genSourcePos :: Gen SourcePos
genSourcePos =
  SourcePos
    <$> genString
    -- Pos required to be > 0
    <*> (mkPos <$> genPosInt)
    <*> (mkPos <$> genPosInt)

genOutputDoc :: Gen (Output Doc)
genOutputDoc =
  O
    <$> genVars
    <*> genAnnInfo
    <*> genAnnInfo
    <*> genBots
    <*> genResult
  where
    genVars :: Gen (Maybe [String])
    genVars =
      Gen.choice
        [ pure Nothing,
          Just <$> listOf genString
        ]

    genAnnInfo :: Gen (AnnInfo Doc)
    genAnnInfo =
      AI . M.fromList
        <$> listOf ((,) <$> genSrcSpan <*> listOf genEntry)

    genEntry :: Gen (Maybe Text, Doc)
    genEntry =
      (,)
        <$> Gen.choice [pure Nothing, Just <$> genText]
        <*> genDoc

    genBots :: Gen [SrcSpan]
    genBots = listOf genSrcSpan

    genResult :: Gen ErrorResult
    genResult =
      Gen.choice
        [ FixErrors.Safe <$> genStats,
          FixErrors.Unsafe <$> genStats <*> listOf genUserError,
          FixErrors.Crash <$> listOf genUserErrorEntry <*> genString
        ]

    genUserErrorEntry :: Gen (UserError, Maybe String)
    genUserErrorEntry =
      (,)
        <$> genUserError
        <*> Gen.choice [pure Nothing, Just <$> genString]

    genStats :: Gen Stats
    genStats =
      Stats
        <$> genInt
        <*> genInt
        <*> genInt
        <*> genInt
        <*> genInt

genAnnMap :: Gen AnnMap
genAnnMap =
  Ann
    <$> genTypes
    <*> genErrors
    <*> genStatus
    <*> genSpTypes
  where
    genTypes :: Gen (M.HashMap Loc (String, String))
    genTypes =
      M.fromList <$> listOf ((,) <$> genLoc <*> genStringPair)

    genErrors :: Gen [(Loc, Loc, String)]
    genErrors = listOf ((,,) <$> genLoc <*> genLoc <*> genString)

    genStatus :: Gen Status
    genStatus =
      Gen.element
        [ Safe,
          Unsafe,
          Error,
          Crash
        ]

    genSpTypes :: Gen [(RealSrcSpan, (String, String))]
    genSpTypes = listOf ((,) <$> genRealSrcSpan <*> genStringPair)

genUserError :: Gen UserError
genUserError =
  Gen.choice
    [ ErrSubType <$> genSrcSpan <*> genDoc <*> genMSubcId <*> genCtx <*> genDoc <*> genDoc,
      ErrSubTypeModel <$> genSrcSpan <*> genDoc <*> genMSubcId <*> genCtxM <*> genWithModelDoc <*> genDoc,
      ErrFCrash <$> genSrcSpan <*> genDoc <*> genCtx <*> genDoc <*> genDoc,
      ErrHole <$> genSrcSpan <*> genDoc <*> genCtx <*> genSymbol <*> genDoc,
      ErrHoleCycle <$> genSrcSpan <*> listOf genSymbol,
      ErrAssType <$> genSrcSpan <*> genOblig <*> genDoc <*> genCtx <*> genDoc,
      ErrParse <$> genSrcSpan <*> genDoc <*> genParseError,
      ErrTySpec <$> genSrcSpan <*> Gen.maybe genDoc <*> genDoc <*> genDoc <*> genDoc,
      ErrTermSpec <$> genSrcSpan <*> genDoc <*> genDoc <*> genExpr <*> genDoc <*> genDoc,
      ErrDupAlias <$> genSrcSpan <*> genDoc <*> genDoc <*> listOf genSrcSpan,
      ErrDupSpecs <$> genSrcSpan <*> genDoc <*> listOf genSrcSpan,
      ErrDupIMeas <$> genSrcSpan <*> genDoc <*> genDoc <*> listOf genSrcSpan,
      ErrDupMeas <$> genSrcSpan <*> genDoc <*> listOf genSrcSpan,
      ErrDupField <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrDupNames <$> genSrcSpan <*> genDoc <*> listOf genDoc,
      ErrBadData <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrDataCon <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrDataConMismatch <$> genSrcSpan <*> genDoc <*> listOf genDoc <*> listOf genDoc,
      ErrInvt <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrIAl <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrIAlMis <$> genSrcSpan <*> genDoc <*> genDoc <*> genDoc,
      ErrMeas <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrHMeas <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrUnbound <$> genSrcSpan <*> genDoc,
      ErrUnbPred <$> genSrcSpan <*> genDoc,
      ErrGhc <$> genSrcSpan <*> genDoc,
      ErrResolve <$> genSrcSpan <*> genDoc <*> genDoc <*> genDoc,
      ErrMismatch
        <$> genSrcSpan
        <*> genDoc
        <*> genDoc
        <*> genDoc
        <*> genDoc
        <*> Gen.maybe ((,) <$> genDoc <*> genDoc)
        <*> genSrcSpan,
      ErrPartPred <$> genSrcSpan <*> genDoc <*> genDoc <*> genInt <*> genInt <*> genInt,
      ErrAliasCycle <$> genSrcSpan <*> genSpanDocListNE,
      ErrIllegalAliasApp <$> genSrcSpan <*> genDoc <*> genSrcSpan,
      ErrAliasApp <$> genSrcSpan <*> genDoc <*> genSrcSpan <*> genDoc,
      ErrTermin <$> genSrcSpan <*> listOf genDoc <*> genDoc,
      ErrStTerm <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrILaw <$> genSrcSpan <*> genDoc <*> genDoc <*> genDoc,
      ErrRClass <$> genSrcSpan <*> genDoc <*> genSpanDocList,
      ErrMClass <$> genSrcSpan <*> genDoc,
      ErrBadQual <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrSaved <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrFilePragma <$> genSrcSpan,
      ErrTyCon <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrLiftExp <$> genSrcSpan <*> genDoc,
      ErrParseAnn <$> genSrcSpan <*> genDoc,
      ErrNoSpec <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrFail <$> genSrcSpan <*> genDoc,
      ErrFailUsed <$> genSrcSpan <*> genDoc <*> listOf genDoc,
      ErrRewrite <$> genSrcSpan <*> genDoc,
      ErrPosTyCon <$> genSrcSpan <*> genDoc <*> genDoc,
      ErrOther <$> genSrcSpan <*> genDoc
    ]
  where
    genMSubcId :: Gen (Maybe SubcId)
    genMSubcId =
      Gen.choice
        [ pure Nothing,
          Just <$> genInteger
        ]

    genCtx :: Gen (M.HashMap Symbol Doc)
    genCtx =
      M.fromList
        <$> listOf ((,) <$> genSymbol <*> genDoc)

    genCtxM :: Gen (M.HashMap Symbol (WithModel Doc))
    genCtxM =
      M.fromList
        <$> listOf ((,) <$> genSymbol <*> genWithModelDoc)

    genOblig :: Gen Oblig
    genOblig =
      Gen.element
        [ OTerm,
          OInv,
          OCons
        ]

    -- Only generating ESym because there are many expressions
    -- and ESym is non-trivial. See liquid-fixpoint's Arbitrary instances
    -- for more info.
    genExpr :: Gen Expr
    genExpr = ESym <$> genSymConst

    genSymConst :: Gen SymConst
    genSymConst = SL <$> genText

    genParseError :: Gen Errors.ParseError
    genParseError =
      MP.TrivialError
        <$> genInt
        <*> Gen.choice [pure Nothing, Just <$> genErrorItem]
        <*> (S.fromList <$> listOf genErrorItem)

    genErrorItem :: Gen (MP.ErrorItem Char)
    genErrorItem =
      Gen.choice
        [ MP.Tokens <$> Gen.nonEmpty (Range.linearFrom 1 1 20) Gen.unicode,
          MP.Label <$> Gen.nonEmpty (Range.linearFrom 1 1 20) Gen.unicode,
          pure MP.EndOfInput
        ]

    genSymbol :: Gen Symbol
    genSymbol = fromString <$> genString

    genWithModelDoc :: Gen (WithModel Doc)
    genWithModelDoc = genWithModelT genDoc

    genWithModelT :: Gen t -> Gen (WithModel t)
    genWithModelT gen =
      Gen.choice
        [ NoModel <$> gen,
          WithModel <$> genDoc <*> gen
        ]

    -- Needs to be non-empty because its user, ErrAliasCycle, requires its
    -- argument to be non-empty.
    genSpanDocListNE :: Gen [(SrcSpan, Doc)]
    genSpanDocListNE = Gen.list (Range.linearFrom 1 1 20) ((,) <$> genSrcSpan <*> genDoc)

    genSpanDocList :: Gen [(SrcSpan, Doc)]
    genSpanDocList = listOf ((,) <$> genSrcSpan <*> genDoc)

listOf :: Gen a -> Gen [a]
listOf g = Gen.list (Range.linearFrom 0 0 20) g

genDoc :: Gen Doc
genDoc = Pretty.text <$> genString

genSrcSpan :: Gen SrcSpan
genSrcSpan =
  Gen.choice
    [ realSrcSpanToSrcSpan <$> genRealSrcSpan,
      pure $ UnhelpfulSpan UnhelpfulNoLocationInfo,
      pure $ UnhelpfulSpan UnhelpfulWiredIn,
      pure $ UnhelpfulSpan UnhelpfulInteractive,
      pure $ UnhelpfulSpan UnhelpfulGenerated,
      UnhelpfulSpan . UnhelpfulOther . fromString <$> genString
    ]

genRealSrcSpan :: Gen RealSrcSpan
genRealSrcSpan = locsSrcSpan <$> genLoc <*> genLoc

genLoc :: Gen Loc
genLoc = L <$> ((,) <$> genInt <*> genInt)

genStringPair :: Gen (String, String)
genStringPair = (,) <$> genString <*> genString

genText :: Gen Text
genText = T.pack <$> genString

genString :: Gen String
genString = Gen.string (Range.linearFrom 0 0 20) Gen.unicode

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linearFrom 0 0 100)

genInt :: Gen Int
genInt = Gen.integral (Range.linearFrom 0 0 100)

genPosInt :: Gen Int
genPosInt = Gen.integral (Range.linearFrom 1 1 100)
