{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Bench.Space.Data.DeBruijn.Index qualified (benchmarks)
import Bench.Space.Data.DeBruijn.Thinning qualified
import Control.Applicative (Alternative (..))
import Data.ByteString.Lazy qualified as BSL (putStr, writeFile)
import Data.Csv (Header, NamedRecord, ToField (..), ToNamedRecord (..), encodeByName, header, namedRecord, (.=))
import Options.Applicative qualified as Opt
import Weigh (Column (..), Grouped (..), Weigh, Weight (..), setColumns, weighResults)

data BenchmarkOpts = BenchmarkOpts
  { groups :: [Group]
  , maybeCsv :: Maybe FilePath
  }

newtype Group = Group String
  deriving newtype (Eq, Show)

benchmarkOpts :: Opt.ParserInfo BenchmarkOpts
benchmarkOpts =
  Opt.info
    (parseBenchmarkOpts Opt.<**> Opt.helper)
    (Opt.progDesc "Run space benchmarks using Weigh")
 where
  parseBenchmarkOpts :: Opt.Parser BenchmarkOpts
  parseBenchmarkOpts =
    BenchmarkOpts
      <$> parseGroups
      <*> Opt.optional parseOutputFile

  parseOutputFile :: Opt.Parser FilePath
  parseOutputFile =
    Opt.strOption
      ( Opt.long "csv"
          <> Opt.metavar "FILE"
          <> Opt.help "Set the CSV output file."
      )

  parseGroups :: Opt.Parser [Group]
  parseGroups = many parseGroup

  parseGroup :: Opt.Parser Group
  parseGroup =
    fmap Group . Opt.strArgument . mconcat $
      [ Opt.metavar "GROUP"
      , Opt.help "Set the benchmark groups that should run."
      , Opt.completeWith [groupName | (Group groupName, _) <- benchmarkGroups]
      ]

benchmarkGroups :: [(Group, Weigh ())]
benchmarkGroups =
  [
    ( Group "Data.DeBruijn.Index"
    , Bench.Space.Data.DeBruijn.Index.benchmarks
    )
  ,
    ( Group "Data.DeBruijn.Thinning"
    , Bench.Space.Data.DeBruijn.Thinning.benchmarks
    )
  ]

main :: IO ()
main = do
  BenchmarkOpts{..} <- Opt.execParser benchmarkOpts
  let benchmarks =
        [ benchmark
        | (group, benchmark) <- benchmarkGroups
        , null groups || group `elem` groups
        ]
  (groupedWeightsOrErrors, _config) <-
    weighResults $ do
      setColumns [Case, Allocated, Max]
      sequence_ benchmarks
  let groupedWeights :: [Grouped Weight]
      groupedWeights = fmap (fmap fst) groupedWeightsOrErrors
  let results = ungroup =<< groupedWeights
  let csvData = encodeByName weightHeader (WeightRecord <$> results)
  case maybeCsv of
    Nothing -> BSL.putStr csvData
    Just csv -> BSL.writeFile csv csvData

weightHeader :: Header
weightHeader =
  header
    [ "Label"
    , "AllocatedBytes"
    , "GCs"
    , "LiveBytes"
    , "MaxBytes"
    , "MaxOSBytes"
    , "WallTime"
    ]

newtype WeightRecord = WeightRecord Weight

instance ToNamedRecord WeightRecord where
  toNamedRecord :: WeightRecord -> NamedRecord
  toNamedRecord (WeightRecord Weight{..}) =
    namedRecord
      [ "Label" .= toField weightLabel
      , "AllocatedBytes" .= toField weightAllocatedBytes
      , "GCs" .= toField weightGCs
      , "LiveBytes" .= toField weightLiveBytes
      , "MaxBytes" .= toField weightMaxBytes
      , "MaxOSBytes" .= toField weightMaxOSBytes
      , "WallTime" .= toField weightWallTime
      ]

ungroup :: Grouped Weight -> [Weight]
ungroup (Grouped _label grouped) = ungroup =<< grouped
ungroup (Singleton weight) = [weight]
