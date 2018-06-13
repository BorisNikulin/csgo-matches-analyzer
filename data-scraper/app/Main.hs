module Main
	( main
	) where

import Data.Foldable

import Database.SQLite.Simple

import Data.Csgo
import Text.MatchParser
import Sql.MatchInserter

import Debug.Trace

main :: IO ()
main = do
	Just matches <- test
	withConnection "test.db" $ \con -> do
		createDb con
		for_ matches $ insertMatch con . traceShowId
