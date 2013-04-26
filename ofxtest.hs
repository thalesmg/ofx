{-# LANGUAGE OverloadedStrings #-}
-- | A little program that reads in the given file and pretty prints
-- the tree and the transactions to standard output.

module Main where

import Data.OFX
import qualified Text.Parsec as P
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified Control.Monad.Exception.Synchronous as Ex
import System.Environment (getArgs)

import Text.PrettyPrint

pPayee :: Payee -> Doc
pPayee p = hang "Payee:" 2 ls
  where
    ls = sep [ label "Name" (text . peNAME $ p)
             , label "Addr1" (text . peADDR1 $ p)
             , label "Addr2" (pMaybe text . peADDR2 $ p)
             , label "Addr3" (pMaybe text . peADDR3 $ p)
             , label "City" (text . peCITY $ p)
             , label "State" (text . peSTATE $ p)
             , label "Postal" (text . pePOSTALCODE $ p)
             , label "Country" (pMaybe text . peCOUNTRY $ p)
             , label "Phone" (text . pePHONE $ p)
             ]

pTransaction :: Transaction -> Doc
pTransaction a = hang "Transaction:" 2 ls
  where
    ls = sep [ label "TRNTYPE" (text . show . txTRNTYPE $ a)
             , label "DTPOSTED" (text . show . txDTPOSTED $ a)
             , label "DTUSER" (text . show . txDTUSER $ a)
             , label "DTAVAIL" (text . show . txDTAVAIL $ a)
             , label "TRNAMT" (text . txTRNAMT $ a)
             , label "FITID" (text . txFITID $ a)
             , label "CORRECTFITID"
               (pMaybe text . txCORRECTFITID $ a)
             , label "CORRECTACTION"
               (text . show . txCORRECTACTION $ a)
             , label "SRVRTID" (pMaybe text . txSRVRTID $ a)
             , label "CHECKNUM" (pMaybe text . txCHECKNUM $ a)
             , label "REFNUM" (pMaybe text . txREFNUM $ a)
             , label "SIC" (pMaybe text . txSIC $ a)
             , label "PAYEEID" (pMaybe text . txPAYEEID $ a)
             , label "PAYEEINFO"
               (pMaybe (pEither text (text . show)) . txPayeeInfo $ a)
             , label "ACCOUNTTO"
               (pMaybe id . fmap (text . show)
                          . txAccountTo $ a)
             , label "MEMO" (pMaybe text . txMEMO $ a)
             , label "CURRENCY"
               (pMaybe (text . show) . txCurrency $ a)
             ]

pTag :: Tag -> Doc
pTag (Tag n ei) = case ei of
  Left d -> "<" <> text n <> ">" <> text d
  Right ts -> vcat $ "<" <> text n <> ">"
                   : map (nest 2 . pTag) ts
                   ++ ["</" <> text n <> ">"]

pHeader :: OfxHeader -> Doc
pHeader (OfxHeader t v) = text t <> ": " <> text v

pFile :: OfxFile -> Doc
pFile (OfxFile hs t)
  = "OFX file:"
  $$ nest 2 (vcat [ list . map pHeader $ hs
                   , empty
                   , pTag t ])

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
    . exceptional
    . Ex.mapExceptional text (list . map pTransaction)
    . transactions
    $ f
  putStrLn . render . pMaybe text . fiName $ f
  putStrLn . render . pMaybe text . accountNumber $ f


pEither :: (a -> Doc) -> (b -> Doc) -> Either a b -> Doc
pEither fa fb = either (\l -> "Left" <+> parens (fa l))
                       (\r -> "Right" <+> parens (fb r))

pMaybe :: (a -> Doc) -> Maybe a -> Doc
pMaybe f = maybe "Nothing" (\x -> "Just" <+> parens (f x))

list :: [Doc] -> Doc
list ds = case ds of
  [] -> "[]"
  x:[] -> brackets x
  x:xs -> sep $ hang "[" 2 x
              : map (\d -> hang "," 2 d) xs
              ++ [ "]" ]

label :: String -> Doc -> Doc
label s = hang (text (s ++ ":")) (length s + 2)

exceptional :: Ex.Exceptional Doc Doc -> Doc
exceptional = Ex.switch (\e -> hang "Exception:" 2 $ parens e)
                        (\g -> hang "Success:" 2 $ parens g)
