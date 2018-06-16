{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language RecordWildCards #-}

module Text.MatchParser
	( scrapeFile
	, csgoScraper
	) where

import Data.Int
import Data.Char
import Data.Maybe
import Text.Read
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Text.HTML.Scalpel.Core

import Data.Csgo

filePath :: FilePath
filePath = "/data/Documents/Misc/Steam Community   Counter-Strike  Global Offensive   Personal Game Data.html"

-- | Scrapes csgo competitive matches from an html file provided by steam.
scrapeFile
	:: FilePath -- ^ Filepath to html containing csgo competitive match data
	-> IO [CsgoMatch]
scrapeFile = fmap (fromMaybe [] . flip scrapeStringLike csgoScraper) . T.readFile

-- | Scraper type from scalpel for csgo competitive matches.
csgoScraper :: Scraper T.Text [CsgoMatch]
csgoScraper = chroots (root // "tr") scrapeMatch
	where
		root = ("div" @: ["id" @= "personaldata_elements_container"]) // "table"

scrapeMatch :: Scraper T.Text CsgoMatch
scrapeMatch = do
	(scoreTeamA, scoreTeamB, playerStats)
		<- chroot ("table" @: [hasClass "csgo_scoreboard_inner_right"] // "tbody") scrapeScoreAndPlayers
	(theMap:theStartTime:theWaitTime:theMatchDuration:_)
		<- chroot ("table" @: [hasClass "csgo_scoreboard_inner_left"] // "tbody") (texts "tr")

	let
		parseNominalDiffTime :: T.Text -> NominalDiffTime
		parseNominalDiffTime s = (minutesToSeconds minutes) + (fromIntegral seconds)
			where
				[_, minutes, seconds] = fmap (read @Int . T.unpack . T.strip) $ T.splitOn ":" s
				minutesToSeconds :: (Num a, Integral a) => a -> NominalDiffTime
				minutesToSeconds mins = fromIntegral $ mins * 60

		matchMap = parseMap theMap
		startTime = read @UTCTime $ T.unpack theStartTime
		waitTime = parseNominalDiffTime theWaitTime
		matchDuration = parseNominalDiffTime theMatchDuration
		(teamA, rest) = splitAt 5 playerStats
		teamB = take 5 rest

	return CsgoMatch{..}

scrapeScoreAndPlayers :: Scraper T.Text (Int, Int, [PlayerStats])
scrapeScoreAndPlayers = liftA2 (uncurry (,,))
	(parseScore <$> text scoreSelector)
	(chroots playersSelector scrapePlayers)
	where
		scoreSelector = "td" @: [hasClass "csgo_scoreboard_score"]
		playersSelector = "tr" @: [match $ const . ("data-steamid64"==)]
		parseScore :: T.Text -> (Int, Int)
		parseScore = (\[a, b] -> (a, b)) . fmap (read @Int . T.unpack) . T.splitOn ":"

scrapePlayers :: Scraper T.Text PlayerStats
scrapePlayers = do
	userId <- (SteamId64 . read @Int64 . T.unpack) <$> attr "data-steamid64" anySelector
	(thePing:theKills:theAssists:theDeaths:theMvps:theHsp:theScore:_) <-
		texts ("td" @: [notP $ hasClass "inner_name"])

	let
		stripNonNumeric = T.dropAround (not . isDigit)
		parseNumWith :: (String -> a) -> T.Text -> a
		parseNumWith f = f . T.unpack . stripNonNumeric
		parseInt = parseNumWith (read @Int)
		parseIntMaybe = parseNumWith (readMaybe @Int)
		parseMvp s
			| s == "★"
				= Just 1
			| otherwise = readMaybe @Int . T.unpack $ stripNonNumeric s

		ping = parseInt thePing
		kills = parseInt theKills
		assists = parseInt theAssists
		deaths = parseInt theDeaths
		mvps = parseMvp theMvps -- always one being the odd one out
		hsp = parseIntMaybe theHsp
		score = parseInt theScore

	return PlayerStats{..}
