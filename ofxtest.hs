-- | A little program that reads in the given file and pretty prints
-- the tree and the transactions to standard output.

module Main where

import qualified Data.OFX as OFX
import qualified Text.Parsec as P
import qualified System.IO as IO
import qualified System.Exit as Exit
import System.Environment (getArgs)

-- From the package pretty-show
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
  fn:[] <- getArgs
  cntnts <- readFile fn
  f <- case P.parse OFX.ofxFile "" cntnts of
    Left e -> do
      IO.hPutStrLn IO.stderr . show $ e
      Exit.exitFailure
    Right g -> return g
  let txns = mapM OFX.transaction . OFX.transactions . OFX.fTag $ f
  putStrLn . ppShow $ txns

