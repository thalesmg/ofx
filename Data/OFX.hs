-- | Parser for downloaded OFX files.
--
-- This parser was written based on the OFX version 1.03
-- specification, which is available at
--
-- http://www.ofx.net
--
-- It will probably work on earlier versions of OFX without
-- incident. However, it likely will not work on newer versions of
-- OFX, which are XML based (this version of OFX is SGML based.)
--
-- It will also parse QFX files, which are OFX files with minor
-- proprietary additions by Intuit, the maker of Quicken.
--
-- An OFX file consists of three parts: the HTTP headers (which this
-- parser does NOT handle because typically they will not show up in
-- files downloaded to disk), the OFX headers, and the OFX data. This
-- parser handles the OFX headers and the OFX data.
--
-- The parser in this module simply parses the tags and data into a
-- tree, which you can manipulate with other functions.

module Data.OFX where

import Control.Applicative
  ( (<$), (<|>), (*>), many, optional,
    (<$>), (<*), (<*>) )
import Control.Monad (replicateM)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Time as T
  
import Text.Parsec.String (Parser)
import Text.Parsec
  ( lookAhead, char, manyTill, anyChar, (<?>), eof, string,
    try, digit, many1 )
import qualified Text.Parsec as P
import qualified Data.Monoid as M

-- | Headers consists of simple @tag:value@ pairs; this represents the
-- tag.
type HeaderTag = String

-- | The value in an OFX header.
type HeaderValue = String

-- | An OFX file starts with a number of headers, which take the form
-- @tag:value@ followed by a newline. These are followed by a blank
-- line.
data OfxHeader = OfxHeader HeaderTag HeaderValue
  deriving (Eq, Show)

-- | The name of an OFX tag
type TagName = String

-- | The data accompanying an OFX tag.
type TagData = String

-- | The main OFX data consists of a series of tags. OFX 1.03 is SGML,
-- not XML. This means that opening tags need not have closing
-- tags. In OFX, a tag either has data and no child elements, or it
-- has no data and it has child elements.
data Tag = Tag TagName (Either TagData [Tag])
  deriving (Eq, Show)

-- | All the data from an OFX file.
data OfxFile = OfxFile
  { fHeader :: [OfxHeader]

  , fTag :: Tag
  -- ^ All the data will be contained in a root tag with the TagName
  -- @OFX@.

  } deriving (Eq, Show)

--
-- Parsers
--

-- | Parses either a UNIX or an MS-DOS newline.
newline :: Parser ()
newline = () <$ char '\n' <|> () <$ (char '\r' *> char '\n')
          <?> "newline"

-- | Parses a character, possibly with an escape sequence. @<@, @>@,
-- and @&@ must be entered with escape sequences.
escChar :: Parser Char
escChar =
  do
    c <- anyChar
    case c of
      '&' -> do
        cs <- manyTill anyChar (char ';')
        case cs of
          "lt" -> return '<'
          "gt" -> return '>'
          "amp" -> return '&'
          _ -> fail $ "unrecognized esacpe sequence: \"&"
                      ++ cs ++ ";\""
      _ -> return c
  <?> "character"

header :: Parser OfxHeader
header
  = OfxHeader
  <$> manyTill anyChar (char ':')
  <*  optional (many (char ' '))
  <*> manyTill anyChar newline
  <?> "OFX header"
  
-- | Parses any tag. The tag itself must be followed by at least one
-- character: either the next tag if this is an aggregate tag, or the
-- data if this is a data tag. OFX does not allow empty tags.
--
-- The OFX spec seems to say that OFX files do not include trailing
-- newlines after tags or data, but I have seen these newlines in QFX
-- files, so this parses optional trailing newlines.
tag :: Parser Tag
tag =
  do
    _ <- char '<'
    n <- escChar
    case n of
      '/' -> fail "this is a closing tag"
      x -> inner x
  <?> "OFX tag"
  where
    inner c =
      do
        n <- manyTill escChar (char '>')
        let name = c:n
        _ <- optional newline
        next <- lookAhead anyChar
        case next of
          '<' -> do
            ts <- many (try tag)
            _ <- string $ "</" ++ name ++ ">"
            _ <- optional newline
            return $ Tag name (Right ts)
          _ -> do
            v <- manyTill escChar (eof <|> lookAhead (() <$ char '<'))
            return $ Tag name (Left v)
      <?> "OFX tag"

-- | Parses an entire OFX file, including headers.
ofxFile :: Parser OfxFile
ofxFile
  = OfxFile
  <$> manyTill header newline
  <*> tag
  <* many newline
  <* eof

-- | Parses an OFX date. Fails if the date is not valid or if there is
-- no date to be parsed.
date :: Parser T.ZonedTime
date =
  do
    ys <- fmap read $ replicateM 4 digit
    ms <- fmap read $ replicateM 2 digit
    ds <- fmap read $ replicateM 2 digit
    day <- case T.fromGregorianValid ys ms ds of
      Nothing -> fail $ "invalid date: " ++ show ys
                        ++ "-" ++ show ms ++ "-" ++ show ds
      Just d -> return d
    mayTime <- optional time
    case mayTime of
      Nothing ->
        let localTime = T.LocalTime day T.midnight
        in return $ T.ZonedTime localTime T.utc
      Just (t, z) -> return $ T.ZonedTime (T.LocalTime day t) z
  <?> "date"

  
-- | Parses an OFX time. Fails if the time is not valid or if there is
-- no time to parse. Fails if there is no time to parse; however, if
-- there is a time but no zone, returns the time and UTC for the zone.
time :: Parser (T.TimeOfDay, T.TimeZone)
time =
  do
    h <- fmap read $ replicateM 2 digit
    m <- fmap read $ replicateM 2 digit
    s <- fmap read $ replicateM 2 digit
    (milli, tz) <- do
      mayDot <- optional (char '.')
      case mayDot of
        Nothing -> return (0, T.utc)
        Just _ -> do
          mil <- fmap ((/ 1000) . read) $ replicateM 3 digit
          mayTz <- optional tzOffset
          case mayTz of
            Nothing -> return (mil, T.utc)
            Just t -> return (mil, t)
    let sec = s + milli
    return (T.TimeOfDay h m sec, tz)
  <?> "time"
                

-- | Parses a time zone offset. Fails if there is no time zone offset
-- to parse.
tzOffset :: Parser T.TimeZone
tzOffset =
  do
    _ <- char '['
    sn <- parseSign
    whole <- many1 digit
    mayDot <- optional (char '.')
    frac <- case mayDot of
      Nothing -> return "0"
      Just _ -> many1 digit
    mayColon <- optional (char ':')
    name <- case mayColon of
      Nothing -> return ""
      Just _ -> many1 P.letter
    _ <- char ']'
    let off = sn $ round ((read (whole ++ "." ++ frac))
                                * (60 :: Double))
    return $ T.TimeZone off False name
  <?> "time zone offset"
  where
    parseSign = do
      mayMinus <- optional (char '-')
      case mayMinus of
        Nothing -> do
          mayPlus <- optional (char '+')
          return $ case mayPlus of
            Nothing -> id
            Just _ -> negate
        Just _ -> return negate

--
-- Manipulating trees of tags
--
-- | Finds child tags with the given name. When a tag is found, that
-- tag is not searched for further children with the same name.
find :: TagName -> Tag -> [Tag]
find n t@(Tag x p)
  | n == x = [t]
  | otherwise = case p of
      Left _ -> []
      Right ts -> concatMap (find n) ts

-- | Finds the first tag (either this tag or any children) that has
-- the given name and that is a data tag (not an aggregate tag.) If no
-- data tag with the given name is found, returns Nothing.
findData :: TagName -> Tag -> Maybe TagData
findData n (Tag tn e) = case e of
  Left d -> if tn == n then Just d else Nothing
  Right ts -> M.getFirst . M.mconcat .  map M.First
              . map (findData n) $ ts


-- | Finds the first tag (either this tag or any children) that has
-- the given name and that is a data tag. Gives an error message if
-- the tag is not found.
required :: TagName -> Tag -> Ex.Exceptional String TagData
required n t = case findData n t of
  Nothing -> Ex.throw $ "required tag missing: " ++ n
  Just r -> return r

--
-- OFX data
-- 

-- | OFX transaction types. These are used in STMTTRN aggregates, see
-- OFX spec section 11.4.2.3.1.1.
data TrnType
  = TCREDIT
  | TDEBIT

  | TINT
  -- ^ Interest earned or paid (which it is depends on sign of amount)

  | TDIV
  -- ^ Dividend

  | TFEE
  | TSRVCHG

  | TDEP
  -- ^ Deposit

  | TATM
  -- ^ ATM debit or credit (which it is depends on sign of amount)

  | TPOS
  -- ^ Point of sale debit or credit (which it is depends on sign of
  -- amount)

  | TXFER
  -- ^ Transfer

  | TCHECK
  | TPAYMENT
  -- ^ Electronic payment

  | TCASH
  -- ^ Cash withdrawal

  | TDIRECTDEP
  -- ^ Direct deposit

  | TDIRECTDEBIT
  -- ^ Merchant initiated debit

  | TREPEATPMT
  -- ^ Repeating payment / standing order

  | TOTHER
  deriving (Eq, Ord, Show)

-- | A single STMTTRN, see OFX spec section 11.4.2.3.1. This is most
-- likely what you are interested in after downloading a statement
-- from a bank.
data Transaction = Transaction
  { txTRNTYPE :: TrnType
    -- ^ Transaction type

  , txDTPOSTED :: T.ZonedTime
    -- ^ Date transaction was posted to account

  , txDTUSER :: Maybe T.ZonedTime
    -- ^ Date user initiated transaction, if known

  , txDTAVAIL :: Maybe T.ZonedTime
    -- ^ Date funds are available

  , txTRNAMT :: String
    -- ^ Amount of transaction. This is left as the string that was
    -- originally in the download. That means the transaction may
    -- include a plus or minus sign (no sign is the same as a plus
    -- sign.) According to section 3.2.9.2, amounts are always signed
    -- from the perspective of the customer.
    --
    -- Typically negative amounts:
    --
    -- * Investment buy amount, investment sell quantity
    -- * Bank statement debit amounts, checks, fees
    -- * Credit card purchases
    -- * Margin balance (unless the institution owes the client money)
    --
    -- Typically positive amounts:
    --
    -- * Investment sell amount, investment buy quantity
    -- * Bank statement credits
    -- * Credit card payments
    -- * Ledger balance (unless the account is overdrawn)
    --
    -- Formats for amounts are described in 3.2.9.1. If there is no
    -- decimal point, there is an implied decimal point at the end of
    -- the value. Trailing and leading spaces \"should\" be
    -- stripped. Positive or minus is indicated with a leading sign; a
    -- plus sign is assumed if there is no sign.
    --
    -- An amount has a maximum of 32 alphanumeric characters,
    -- including digits and punctuation.
    --
    -- The radix point is indicated with either a period or a
    -- comma. Amounts \"should\" not include any digit grouping
    -- characters.

    , txFITID :: String
    -- ^ Transaction ID issued by financial institution. Used to
    -- detect duplicate downloads.

    , txCORRECTFITID :: Maybe String
    -- ^ If present, this indicates the FITID of a previously sent
    -- transaction that is corrected by this record. This transaction
    -- replaces or deletes the transaction that it corrects, based on
    -- the value of CORRECTACTION below.

    , txCORRECTACTION :: Maybe CorrectAction
    -- ^ See 'CorrectAction' and 'txCORRECTFITID'

    , txSRVRTID :: Maybe String
    -- ^ Server assigned transaction ID; used for transactions
    -- initiated by client, such as payment or funds transfer

    , txCHECKNUM :: Maybe String
    -- ^ Check or other reference number

    , txREFNUM :: Maybe String
    -- ^ Reference number that uniquely identifies the
    -- transaction. Can be used in addition to or instead of a
    -- CHECKNUM.

    , txSIC :: Maybe String
    -- ^ Standard Industrial Code

    , txPAYEEID :: Maybe String
    -- ^ Payee identifier if available

    , txPayeeInfo :: Maybe (Either String Payee)
    -- ^ Information on the payee. The OFX spec seems to be saying
    -- that every transaction must have either NAME, wich is \"name of
    -- payee or description of transaction\", or the Payee
    -- aggregate. The former is indicated with a Left, the latter with
    -- a Right.

    , txAccountTo :: Maybe (Either BankAcctTo CCAcctTo)
    -- ^ Information on a transfer. If this transaction wa sa
    -- transfer, this may contain information about the account being
    -- transferred to.

    , txMEMO :: Maybe String
    -- ^ Extra information not in NAME

    , txCurrency :: Maybe (Either Currency OrigCurrency)
    -- ^ Currency option. OFX spec says to choose either CURRENCY or
    -- ORIGCURRENCY.
    } deriving (Show)

data Payee = Payee
  { peNAME :: String
  , peADDR1 :: String
  , peADDR2 :: Maybe String
  , peADDR3 :: Maybe String
  , peCITY :: String
  , peSTATE :: String
  , pePOSTALCODE :: String
  , peCOUNTRY :: Maybe String
  , pePHONE :: String
  } deriving (Eq, Show)

-- | Can be either REPLACE or DELETE.
data CorrectAction
  = REPLACE
  -- ^ Replaces the transaction referenced by the CORRECTFITID

  | DELETE
  -- ^ Deletes the transaction referenced by the CORRECTFITID
  deriving (Eq, Show)

data BankAcctTo = BankAcctTo
  { btBANKID :: String
  -- ^ Routing and transit number

  , btBRANCHID :: Maybe String
  -- ^ Bank identifier for international banks

  , btACCTID :: String
  -- ^ Account number

  , btACCTTYPE :: AcctType
  -- ^ Type of account

  , btACCTKEY :: String
  -- ^ Checksum for international banks
  } deriving Show

data CCAcctTo = CCAcctTo
  { ctACCTID :: String
  -- ^ Account number

  , ctACCTKEY :: String
  -- ^ Checksum for international banks

  } deriving (Eq, Show)

data AcctType
  = ACHECKING
  | ASAVINGS
  | AMONEYMRKT
  | ACREDITLINE
  deriving (Eq, Show, Ord)

-- | Holds all data both for CURRENCY and for ORIGCURRENCY.
data CurrencyData = CurrencyData

  { cdCURRATE :: String
  -- ^ Ratio of CURDEF currency to CURSYM currency, in decimal form

  , cdCURSYM :: String
  -- ^ ISO-4217 3-letter currency identifier
  } deriving (Eq, Show)

data Currency = Currency CurrencyData
  deriving (Eq, Show)

data OrigCurrency = OrigCurrency CurrencyData
  deriving (Eq, Show)

--
-- Helpers to build aggregates
--
trnType :: TagData -> Maybe TrnType
trnType d = case d of
  "CREDIT" -> Just TCREDIT
  "DEBIT" -> Just TDEBIT
  "INT" -> Just TINT
  "DIV" -> Just TDIV
  "FEE" -> Just TFEE
  "SRVCHG" -> Just TSRVCHG
  "DEP" -> Just TDEP
  "ATM" -> Just TATM
  "POS" -> Just TPOS
  "XFER" -> Just TXFER
  "CHECK" -> Just TCHECK
  "PAYMENT" -> Just TPAYMENT
  "CASH" -> Just TCASH
  "DIRECTDEP" -> Just TDIRECTDEP
  "DIRECTDEBIT" -> Just TDIRECTDEBIT
  "REPEATPMT" -> Just TREPEATPMT
  "OTHER" -> Just TOTHER
  _ -> Nothing
