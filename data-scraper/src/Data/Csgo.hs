{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Data.Csgo
	(
	-- * Match types
	  CsgoMatch(..)
	, MatchId
	, CsgoMap(..)
	-- * Player types
	, PlayerStats(..)
	, SteamId64(..)
	-- * Parsing
	, parseMatchId
	) where

import Data.Int

import qualified Data.Text as T
import Data.Time

import Debug.Trace

-- TODO
data MatchId = MatchId (Int64, Int64)
	deriving (Show, Eq)

-- | Record type for a single csgo competitive match
data CsgoMatch = CsgoMatch
	{ matchId :: MatchId -- ^ based on the replay url such as ../123_456.dem.bz2 where 123456 would be the id
	, matchMap :: CsgoMap
	, startTime :: UTCTime
	, waitTime :: NominalDiffTime
	, matchDuration :: NominalDiffTime
	, scoreTeamA :: Int
	, scoreTeamB :: Int
	, teamA :: [PlayerStats] -- ^ team of 5
	, teamB :: [PlayerStats] -- ^ team of 5
	} deriving (Show)

-- | Enum for csgo maps
data CsgoMap = Dust2 | Mirage | Cache -- the other maps dont exist ;)
	deriving (Show, Enum, Bounded)

newtype SteamId64 = SteamId64 { getSteamId64 :: Int64 }
	deriving(Show, Eq, Enum, Bounded)

data PlayerStats = PlayerStats
	{ userId :: SteamId64
	, ping :: Int
	, kills :: Int
	, assists :: Int
	, deaths :: Int
	, mvps :: Maybe Int -- ^ mvp stars
	, hsp :: Int -- ^ headshot percentage
	, score :: Int
	-- ban data from plugin but ill add that later?
	} deriving (Show)

-- | Takes in raw gotv url text
parseMatchId :: T.Text -> MatchId
parseMatchId s = MatchId (textToInt64 first, textToInt64 second)
	where
		(firstWithUnderscore, second) = T.breakOnEnd "_" . fst . T.breakOn "." . snd $ T.breakOnEnd "/" s
		first = T.dropEnd 1 firstWithUnderscore
		textToInt64 = read @Int64 . T.unpack


