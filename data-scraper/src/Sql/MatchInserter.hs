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

import Debug.Trace

createDb :: Connection -> IO ()
createDb con = do
	let rootDir = "data/sql/create-db/"
	filePaths <- (fmap (rootDir ++)) <$> listDirectory rootDir
	for_ filePaths $ (execute_ con =<<) . loadQuery

insertMatch :: Connection -> CsgoMatch -> IO ()
insertMatch con CsgoMatch{..} = do
	insertMatchQ <- loadQuery "data/sql/insert-match.sql"

	let
		MatchId (mid1, mid2) = traceShowId matchId

	execute con insertMatchQ
		( mid1
		, mid2
		, matchMap
		, startTime
		, realToFrac @NominalDiffTime @Double waitTime
		, realToFrac @NominalDiffTime @Double matchDuration
		, scoreTeamA
		, scoreTeamB
		)

	for_ teamA $ insertPlayer con matchId False
	for_ teamB $ insertPlayer con matchId True

insertPlayer
	:: Connection
	-> MatchId
	-> Bool -- ^ Team (False for A, True for B)
	-> PlayerStats
	-> IO ()
insertPlayer con (MatchId (mid1, mid2)) team PlayerStats{..} = do
	insertPlayerQ <- loadQuery "data/sql/insert-player.sql"
	insertPlayedInQ <- loadQuery "data/sql/insert-played_in.sql"

	execute con insertPlayerQ (Only userId)

	executeNamed con insertPlayedInQ
		[ ":mid1" := mid1
		, ":mid2" := mid2
		, ":pid" := userId
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
