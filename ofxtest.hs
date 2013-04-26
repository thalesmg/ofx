{-# LANGUAGE OverloadedStrings #-}
-- | A little program that reads in the given file and pretty prints
-- the tree and the transactions to standard output.

module Main where

import Data.OFX
import qualified Text.Parsec as P
import qualified System.IO as IO
import qualified System.Exit as Exit
import System.Environment (getArgs)

import Text.PrettyPrint

main :: IO ()
main = do
  fn:[] <- getArgs
  cntnts <- readFile fn
  f <- case P.parse ofxFile "" cntnts of
    Left e -> do
      IO.hPutStrLn IO.stderr . show $ e
      Exit.exitFailure
    Right g -> return g
  putStrLn . render . pFile $ f
  putStrLn
    . render
    . pExceptional text (pList . map pTransaction)
    . transactions
    $ f
  putStrLn . render . pMaybe text . fiName $ f
  putStrLn . render . pMaybe text . accountNumber $ f

