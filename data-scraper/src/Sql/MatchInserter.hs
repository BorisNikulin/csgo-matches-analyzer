{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Sql.MatchInserter where

import Data.Foldable

import qualified Data.Text.IO as T
import Data.Time
import Database.SQLite.Simple
import System.Directory


import Data.Csgo

createDb :: Connection -> IO ()
createDb con = do
	let rootDir = "data/sql/create-db/"
	filePaths <- (fmap (rootDir ++)) <$> listDirectory rootDir
	for_ filePaths $ (execute_ con =<<) . loadQuery

insertMatch :: Connection -> CsgoMatch -> IO ()
insertMatch con CsgoMatch{..} = do
	insertMatchQ <- loadQuery "data/sql/insert-match.sql"
	--let insertMatchQ = loadQueryInsert

	execute con insertMatchQ
		( matchMap
		, startTime
		, realToFrac @NominalDiffTime @Double waitTime
		, realToFrac @NominalDiffTime @Double matchDuration
		, scoreTeamA
		, scoreTeamB
		)

	let matchId = (matchMap, startTime, matchDuration)

	for_ teamA $ insertPlayer con matchId False
	for_ teamB $ insertPlayer con matchId True

type MatchId = (CsgoMap, UTCTime, NominalDiffTime)

insertPlayer
	:: Connection
	-> MatchId -- ^ The map and match timestamp
	-> Bool -- ^ Team (False for A, True for B)
	-> PlayerStats
	-> IO ()
insertPlayer con (matchMap, startTime, matchDuration) team PlayerStats{..} = do
	insertPlayerQ <- loadQuery "data/sql/insert-player.sql"
	insertPlayedInQ <- loadQuery "data/sql/insert-played_in.sql"

	execute con insertPlayerQ (Only userId)

	executeNamed con insertPlayedInQ
		[ ":pid" := userId
		, ":map" := matchMap
		, ":start_time" := startTime
		, ":duration" := realToFrac @NominalDiffTime @Double matchDuration
		, ":team" := fromEnum team
		, ":ping" := ping
		, ":kills" := kills
		, ":assists" := assists
		, ":deaths" := deaths
		, ":mvps" := mvps
		, ":hsp" := hsp
		, ":score" := score
		]

loadQuery :: FilePath -> IO Query
loadQuery = fmap Query . T.readFile
