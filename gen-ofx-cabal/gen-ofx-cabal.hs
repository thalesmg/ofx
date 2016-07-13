module Main (main) where

import qualified Cartel

version :: [Word]
version = [0,4,2,0]

properties :: Cartel.Properties
properties = Cartel.Properties
  { Cartel.name = "ofx"
  , Cartel.version = version
  , Cartel.cabalVersion = Just (1,10)
  , Cartel.buildType = Just Cartel.simple
  , Cartel.license = Just Cartel.bsd3
  , Cartel.licenseFile = "LICENSE"
  , Cartel.licenseFiles = []
  , Cartel.copyright = "Copyright (c) 2016 Omari Norman"
  , Cartel.author = "Omari Norman"
  , Cartel.maintainer = "omari@smileystation.com"
  , Cartel.stability = "Experimental"
  , Cartel.homepage = "http://www.github.com/massysett/ofx"
  , Cartel.bugReports = "http://www.github.com/massysett/ofx/issues"
  , Cartel.packageUrl = ""
  , Cartel.synopsis = "Parser for OFX data"
  , Cartel.description =
    [ "A parser for Open Financial Exchange (OFX) financial data. This"
    , "handles OFX version 1.03, which is documented at http://www.ofx.net"
    , "This also handles QFX, which is OFX plus some minor additions made"
    , "by Intuit, the publishers of Quicken."
    , ""
    , "The parser will place all the data into a tree, which you can query"
    , "for whatever data you may need, although you will need to be"
    , "familiar with the OFX spec to do this. There is also a Transaction"
    , "type which you can easily parse from the tree; this will contain"
    , "most, perhaps all, of the data you will be interested in if your OFX"
    , "file is from a bank or credit card account."
    , ""
    , "All the OFX and QFX files I have seen use the format from the 1.0"
    , "series of OFX, which is primarily SGML based. OFX version 2 is XML"
    , "based.  This parser likely will not work on those files."
    ]
  , Cartel.category = "Finance"
  , Cartel.testedWith = []
  , Cartel.dataFiles = []
  , Cartel.dataDir = ""
  , Cartel.extraSourceFiles = []
  , Cartel.extraDocFiles = []
  , Cartel.extraTmpFiles = []
  }

ghcOptions :: Cartel.HasBuildInfo a => a
ghcOptions = Cartel.ghcOptions
  [ "-Wall"
  ]

commonOptions :: Cartel.HasBuildInfo a => [a]
commonOptions
  = ghcOptions
  : Cartel.haskell2010
  : Cartel.hsSourceDirs ["lib"]
  : Cartel.buildDepends libraryDepends
  : []

libraryDepends :: [Cartel.Package]
libraryDepends =
  [ Cartel.closedOpen "base" [4,8] [5]
  , Cartel.atLeast "parsec" [3,1]
  , Cartel.atLeast "pretty" [1,1]
  , Cartel.atLeast "time" [1,4]
  ]

library
  :: [Cartel.NonEmptyString]
  -- ^ List of library modules
  -> [Cartel.LibraryField]
library libModules
  = Cartel.exposedModules libModules
  : commonOptions

github :: Cartel.Section
github = Cartel.githubHead "massysett" "ofx"

-- | Creates an executable with all library modules and library
-- dependencies.
executable
  :: String
  -- ^ Name of executable.  An @.hs@ suffix is appended when looking
  -- for the source file.
  -> [String]
  -- ^ Library modules
  -> Cartel.Section
executable exeName libMods = Cartel.executable exeName $
  [ Cartel.mainIs (exeName ++ ".hs")
  , Cartel.otherModules libMods
  , Cartel.hsSourceDirs ["exe"]
  ] ++ commonOptions

sections
  :: [String]
  -- ^ Library modules
  -> [Cartel.Section]
sections libMods =
  [ github
  , executable "renderOfx" libMods
  , executable "renderTransactions" libMods
  ]

main :: IO ()
main = Cartel.defaultMain $ do
  libModules <- Cartel.modules "../ofx/lib"
  return (properties, library libModules, sections libModules)
