module Main
	( main
	) where

import Data.Foldable
import System.Environment

import Control.Concurrent.Async
import Database.SQLite.Simple

import Text.MatchParser
import Sql.MatchInserter

main :: IO ()
main = do
	inputArgs <- getArgs
	case inputArgs of
		[fp, db] -> run fp db
		_ -> printHelp

run :: FilePath -> FilePath -> IO ()
run fp db = do
	matches <- scrapeFile fp
	withConnection db $ \con -> do
		createDb con
		forConcurrently_ matches $ insertMatch con

printHelp :: IO ()
printHelp = do
	progName <- getProgName
	putStrLn
		$  "Usage: "
		++ progName
		++ " <csgo data html> <sqlite db write path>"
