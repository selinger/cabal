{-# LANGUAGE DeriveGeneric #-}
-- runghc -package-env=default Validate.hs validate.yml.zinza .github/workflows/validate.yml
module Main (main) where

import GHC.Generics       (Generic)
import System.Environment (getArgs)
import Zinza
       (Zinza (..), genericFromValueSFP, genericToTypeSFP, genericToValueSFP,
       parseAndCompileTemplateIO)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [src,tgt] -> do
            run <- parseAndCompileTemplateIO src
            -- this shouldn't fail (run-time errors are due bugs in zinza)
            w <- run Z
                { zJobs =
                    [ GhcJob "8.8.1" "--solver-benchmarks" $ defSteps
                        ++ ["solver-benchmarks-tests", "solver-benchmarks-run"]
                    , GhcJob "8.6.5" "" defSteps
                    , GhcJob "8.4.4" "" defSteps
                    , GhcJob "8.2.2" "" defSteps
                    , GhcJob "8.0.2" "" defSteps
                    , GhcJob "7.10.3" "" defSteps
                    , GhcJob "7.8.4" "--lib-only" libSteps
                    ]
                , zMacosJobs =
                    [ mkMacGhcJob "8.8.1" "https://downloads.haskell.org/~ghc/8.8.1/ghc-8.8.1-x86_64-apple-darwin.tar.xz"
                    , mkMacGhcJob "8.6.5" "https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-apple-darwin.tar.xz"
                    ]
                , zMangleVersion = map mangleChar
                }
            writeFile tgt w
        _ -> putStrLn "Usage source.yml.zinza target.yml"

mangleChar :: Char -> Char
mangleChar '.' = '_'
mangleChar c   = c

defSteps :: [String]
defSteps =
    [ "print-config"
    , "print-tool-versions"
    , "make-cabal-install-dev"
    , "build"
    , "lib-tests"
    , "lib-suite"
    , "cli-tests"
    , "cli-suite"
    ]

libSteps :: [String]
libSteps =
    [ "print-config"
    , "print-tool-versions"
    , "build"
    , "lib-tests"
    , "lib-suite"
    ]

data Z = Z
    { zJobs          :: [GhcJob]
    , zMacosJobs     :: [MacGhcJob]
    , zMangleVersion :: String -> String
    }
  deriving (Generic)

data GhcJob = GhcJob
    { gjVersion :: String
    , gjFlags   :: String
    , gjSteps   :: [String]
    }
  deriving (Generic)

data MacGhcJob = MacGhcJob
    { mgjVersion :: String
    , mgjGhcUrl  :: String
    , mgjFlags   :: String
    , mgjSteps   :: [String]
    }
  deriving (Generic)

mkMacGhcJob :: String -> String -> MacGhcJob
mkMacGhcJob v u = MacGhcJob
    { mgjVersion = v
    , mgjGhcUrl  = u
    , mgjFlags   = ""
    , mgjSteps   = defSteps
    }

instance Zinza Z where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Zinza GhcJob where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Zinza MacGhcJob where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP
