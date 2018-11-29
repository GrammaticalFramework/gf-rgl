-- | Reading language config file
module Config (
  LangInfo (..),
  loadLangs, loadLangsFrom, configFile
  ) where

import Data.List (unfoldr)
import System.IO (hPutStrLn,stderr)
import System.Exit (exitFailure)

-- | Path to language config file
configFile :: FilePath
configFile = "languages.csv"

-- | Information about a language
data LangInfo = LangInfo
  { langCode :: String -- ^ 3-letter ISO 639-2/B code
  , langName :: String -- ^ language name
  , langDir :: String -- ^ directory name
  , langFunctor :: Maybe String -- ^ functor (not used)
  , langUnlexer :: Maybe String -- ^ decoding for postprocessing linearizations
  , langPresent :: Bool
  , langAll :: Bool
  , langTry :: Bool
  , langSymbolic :: Bool
  , langCompatibility :: Bool
  , langSynopsis :: Bool -- ^ include in RGL synopsis
  } deriving (Show,Eq)

-- | Load language information from default config file
loadLangs :: IO [LangInfo]
loadLangs = loadLangsFrom configFile

-- | Load language information from specified config file
loadLangsFrom:: FilePath -> IO [LangInfo]
loadLangsFrom configFile = do
  lns <- readFile configFile >>= return . lines
  mapM mkLangInfo (tail lns)
  where
    maybeBit bits n = if length bits >= (n+1) && length (bits !! n) > 0 then Just (bits !! n) else Nothing
    boolBit bits n def = if length bits >= (n+1) && length (bits !! n) > 0 then (if def then bits !! n /= "n" else bits !! n == "y") else def
    mkLangInfo s =
      let bits = separateBy ',' s in
      if length bits < 2
      then die $ "Invalid entry in " ++ configFile ++ ": " ++ s
      else return $ LangInfo
            { langCode = bits !! 0
            , langName = bits !! 1
            , langDir = bits !! 2
            , langFunctor = maybeBit bits 3
            , langUnlexer = maybeBit bits 4
            , langPresent = boolBit bits 5 False
            , langAll = boolBit bits 6 True
            , langTry = boolBit bits 7 True
            , langSymbolic = boolBit bits 8 True
            , langCompatibility = boolBit bits 9 False
            , langSynopsis = boolBit bits 10 False
            }

-- | Separate a string on a character
-- Source: https://stackoverflow.com/a/4978733/98600
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

die :: String -> IO a
die s = do
  hPutStrLn stderr s
  exitFailure
