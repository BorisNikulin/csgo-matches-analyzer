{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Data.Csgo
	(
	-- * Match types
	  CsgoMatch(..)
	, CsgoMap(..)
	-- * Player types
	, PlayerStats(..)
	, SteamId64(..)
	-- * Parsing
	, parseMap
	) where

import Data.Int
import Data.Maybe
import Data.Foldable
import Control.Applicative

import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple.ToField

-- | Record type for a single csgo competitive match
data CsgoMatch = CsgoMatch
	{ matchMap :: CsgoMap
	, startTime :: UTCTime
	, waitTime :: NominalDiffTime
	, matchDuration :: NominalDiffTime
	, scoreTeamA :: Int
	, scoreTeamB :: Int
	, teamA :: [PlayerStats] -- ^ team of 5
	, teamB :: [PlayerStats] -- ^ team of 5
	} deriving (Show)

-- | Enum for csgo maps
data CsgoMap = Dust2 | Mirage | Cache | Cobblestone | Train | Overpass | Nuke | Inferno
	deriving (Show, Enum, Bounded)

instance ToField CsgoMap where
	toField = toField . show
	{-# INLINE toField #-}

newtype SteamId64 = SteamId64 { getSteamId64 :: Int64 }
	deriving(Show, Eq, Enum, Bounded)

instance ToField SteamId64 where
	toField = toField . getSteamId64
	{-# INLINE toField #-}

data PlayerStats = PlayerStats
	{ userId :: SteamId64
	, ping :: Int
	, kills :: Int
	, assists :: Int
	, deaths :: Int
	, mvps :: Maybe Int -- ^ mvp stars
	, hsp :: Maybe Int -- ^ headshot percentage
	, score :: Int
	-- ban data from plugin but ill add that later?
	} deriving (Show)

parseMap :: T.Text -> CsgoMap
parseMap s = fromJust . asum $
	boolToMaybeMap
	<$> ZipList [minBound .. maxBound]
	<*> (T.isSuffixOf <$> maps <*> pure (T.stripEnd s))
	where
		-- | must be in the same order as the CsgoMap enum
		maps :: ZipList T.Text
		maps = ZipList
			[ "Dust II"
			, "Mirage"
			, "Cache"
			, "Cobblestone"
			, "Train"
			, "Overpass"
			, "Nuke"
			, "Inferno"
			]
		boolToMaybeMap :: a -> Bool -> Maybe a
		boolToMaybeMap map b
			| b = Just map
			| otherwise = Nothing
