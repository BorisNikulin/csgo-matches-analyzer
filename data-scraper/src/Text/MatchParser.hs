{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language RecordWildCards #-}

module Text.MatchParser where

import Data.Int
import Data.Char
import Data.Maybe
import Data.Foldable
import Text.Read
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Text.HTML.Scalpel.Core

import Data.Csgo

import Debug.Trace

filePath :: FilePath
filePath = "/data/Documents/Misc/Steam Community   Counter-Strike  Global Offensive   Personal Game Data.html"

test = flip (scrapeStringLike @T.Text) scraper <$> T.readFile filePath

scraper =
	chroot (("div" @: ["id" @= "personaldata_elements_container"]) // "table" // "tbody")
	-- . chroot "tbody"
	$ chroots "tr" scrapeMatch

{-scrapeFirstMatches :: Scraper T.Text ((Int, Int))-}
{-scrapeFirstMatches = do-}
	{-pos <- position-}
	{-a <- scrapeMatch-}
	{-return (pos, a)-}
	{-if pos == 0 -- header-}
		{-then return Nothing-}
		{-else Just <$> scrapeMatch-}

scrapeMatch :: Scraper T.Text CsgoMatch
scrapeMatch = do
	(scoreTeamA, scoreTeamB, playerStats)
		<- chroot ("table" @: [hasClass "csgo_scoreboard_inner_right"] // "tbody") scrapeScoreAndPlayers
	(theMap:theStartTime:theWaitTime:theMatchDuration:_)
		<- chroot ("table" @: [hasClass "csgo_scoreboard_inner_left"] // "tbody") (texts "tr")
	gotvLink <- attr "href" "a"

	let
		{-parseMatchId :: T.Text -> MatchId-}
		-- the two numbers together are too large for an Int64
		-- store as a tuple of 2 numbers? as text?
		-- TODO
		{-parseMatchId = {-read @Int64 . T.unpack . T.filter isDigit .-} fst . T.breakOn "." . snd . T.breakOnEnd "/"-}

		parseMap :: T.Text -> CsgoMap
		parseMap s = fromJust . asum $
			boolToMaybeMap
			<$> ZipList [minBound .. maxBound]
			<*> (T.isSuffixOf <$> maps <*> pure (T.stripEnd s))
			where
				-- | must be in the same order as the CsgoMap enum
				maps :: ZipList T.Text
				maps = ZipList ["Dust II", "Mirage", "Cache"]
				boolToMaybeMap :: a -> Bool -> Maybe a
				boolToMaybeMap map b
					| b = Just map
					| otherwise = Nothing

		parseNominalDiffTime :: T.Text -> NominalDiffTime
		parseNominalDiffTime s = (minutesToSeconds minutes) + (fromIntegral seconds)
			where
				[_, minutes, seconds] = fmap (read @Int . T.unpack . T.strip) $ T.splitOn ":" s
				minutesToSeconds :: (Num a, Integral a) => a -> NominalDiffTime
				minutesToSeconds mins = fromIntegral $ mins * 60

		matchId = parseMatchId gotvLink
		matchMap = parseMap theMap -- TODO actually do it

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
		parseInt = read @Int . T.unpack . stripNonNumeric
		parseMvp s
			| s == "★"
				= Just 1
			| otherwise = readMaybe @Int . T.unpack $ stripNonNumeric s

		ping = parseInt thePing
		kills = parseInt theKills
		assists = parseInt theAssists
		deaths = parseInt theDeaths
		mvps = parseMvp theMvps -- always one being the odd one out
		hsp = parseInt theHsp
		score = parseInt theScore

	return PlayerStats{..}
