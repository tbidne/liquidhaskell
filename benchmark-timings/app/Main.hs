{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception (throwIO)
import Data.String (fromString)
import Prelude hiding (writeFile)
import Data.Csv hiding (Options, Parser, (.:), (.=))
import GHC.Generics (Generic)
import Options.Applicative
import Data.Traversable (for)
import Data.Maybe (catMaybes)

import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.List (foldl', intersperse, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.JSON (JSON (showJSON, readJSON), JSObject, JSValue (JSObject))
import qualified Text.JSON as JSON


data Phase = Phase
  { phaseTime :: Double
  , phaseName :: String
  , phaseModule :: String
  , phaseAlloc :: Int
  } deriving stock (Eq, Ord, Show, Generic)

instance JSON Phase where
  showJSON (Phase time name md alloc) =
    JSON.makeObj
      [ "phaseTime"   .= time
      , "phaseName"   .= name
      , "phaseModule" .= md
      , "phaseAlloc"  .= alloc
      ]

  readJSON = readJSONObj $ \v ->
      Phase
        <$> v .: "phaseTime"
        <*> v .: "phaseName"
        <*> v .: "phaseModule"
        <*> v .: "phaseAlloc"

data PhasesSummary = PhasesSummary
  { test :: String
  , time :: Double
  , result :: Bool
  } deriving stock (Eq, Ord, Show, Generic)

instance ToField Bool where
  toField b = fromString $ show b

instance ToNamedRecord PhasesSummary
instance DefaultOrdered PhasesSummary

data Options = Options
  { optsFilesToParse :: [FilePath]
  , optsPhasesToCount :: [String]
  , optsOutputFile :: FilePath
  } deriving stock (Eq, Ord, Show)

options :: Parser Options
options = Options <$>
  many (argument
         str
          (metavar "FILEPATH..."
           <> help "The files you wish to process."))
  <*> many (strOption (long "phase"
                        <> short 'p'
                        <> metavar "PHASE"
                        <> help "Phase to include in summary.  Can be specified more thance once."))
  <*> strOption (long "output"
                  <> short 'o'
                  <> metavar "OUTPUTFILEPATH"
                  <> help "File to which to output CSV contents.")

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc
   <> progDesc "Summarize timing info.")

-- | Parse the original filename from the .dump-timings filename
dumpFilenameParser :: ReadP.ReadP FilePath
dumpFilenameParser = do
  pathPieces <- ReadP.sepBy (ReadP.many ReadP.get) (ReadP.string "--")
  _ <- ReadP.string ".dump-timings.json"
  rest <- case pathPieces of
    _arch : _ghcVersion : _pkg : _ : p : rest | isSuffixOf "-tmp" p ->
      pure rest
    _arch : _ghcVersion : _pkg : rest ->
      pure rest
    _ ->
      ReadP.pfail
  ReadP.eof
  pure . mconcat $ intersperse "/" rest

program :: Options -> IO ()
program Options {..} = do
  csvFields <- for optsFilesToParse $ \fp ->
    case ReadP.readP_to_S dumpFilenameParser fp of
      (originalFilename, _):_ -> do
        contents <- BS.readFile fp
        phases <- case TEnc.decodeUtf8' contents of
          Left ex -> throwIO ex
          Right contentsUtf8 -> case JSON.decode (T.unpack contentsUtf8) of
            JSON.Error err -> error $ "Cannot decode phase json: " ++ err
            JSON.Ok ps -> pure ps

        let time = foldl' (+) 0 [ phaseTime p | p <- phases, elem (init (phaseName p)) optsPhasesToCount ]
        -- convert milliseconds -> seconds
        pure . Just $ PhasesSummary originalFilename (time / 1000) True
      _ ->
        error $ "can't parse: " ++ show fp
  let csvData = encodeDefaultOrderedByNameWith (defaultEncodeOptions { encUseCrLf = False }) $ catMaybes csvFields
  writeFile optsOutputFile csvData

main :: IO ()
main = program =<< execParser opts

-- following utils vendored from liquid-fixpoint's
-- Language.Fixpoint.Utils.JSON, as to not incur the extra dependency.
readJSONObj :: (JSObject JSValue -> JSON.Result a) -> JSValue -> JSON.Result a
readJSONObj onObj (JSObject obj) = onObj obj
readJSONObj _ other = JSON.Error $ "Expected json obj, received: " ++ show other

(.=) :: JSON v => k -> v -> (k, JSValue)
key .= val = (key, showJSON val)

(.:) :: JSON a => JSObject JSValue -> String -> JSON.Result a
(.:) = flip JSON.valFromObj