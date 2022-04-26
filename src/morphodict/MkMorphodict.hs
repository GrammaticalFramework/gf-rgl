module Main where

import PGF

import qualified Data.Map as M
import Data.Char
import Data.List
import Safe
import System.Environment (getArgs)
import Debug.Trace

-- AR 2020-02-28

-- making a word list purely morphological, i.e.
--   - functions are 1-to-1 with lemgrams, i.e.
--     - no sense distinctions
--     - no subcategorizations
--     - no variants
--  - functionname = baseform_category, with exceptions
--     - variant inflection tables: lie_1_V, lie_2_V
--     - words that have non-ident characters: 'bird\'s-eye_A'

-- example:
--   gf -make ../english/DictEng.gf
--   runghc MkMorphodict.hs pgf MorphoDictEng.config DictEngAbs.pgf MorphoDictEng
-- 64923 ->  56599 functions

usage = "runghc MkMorphodict (raw|pgf) <configfile> <datafile> <outfile>"

main = do
  xx <- getArgs
  if length xx /= 4
    then do
      putStrLn "Usage:"
      putStrLn usage
      putStrLn $ "Got instead: " ++ show xx
    else do
      let mode:configfile:datafile:outfile:_ = xx
      config <- readFile configfile >>= return . mkConfig

      rawdata <- case mode of
        "pgf" -> pgfFile2rawData config datafile
        "raw" -> readFile datafile >>= return . map getRawData . filter (not . null) . lines
        _ -> error $ "Expected mode (pgf|raw), got " ++ mode
      rawdata2gf config rawdata outfile


rawdata2gf config rawdata outfile = do

  let env = MDEnv rawdata config
  let (absrules,cncrules) = mkMorphoDict env

  absheader <- readFile (outfile ++ "Abs.header")
  cncheader <- readFile (outfile ++ ".header")

  writeFile (outfile ++ "Abs.gf") absheader
  appendFile (outfile ++ "Abs.gf") $ unlines $ sort absrules
  appendFile (outfile ++ "Abs.gf") "}"

  writeFile (outfile ++ ".gf") cncheader
  appendFile (outfile ++ ".gf") $ unlines $ sort cncrules
  appendFile (outfile ++ ".gf") "}"

-- one way to get raw data from a dictionary pgf
-- another way, more controllod, is to write a wrapper grammar with a function, for each category, to generate a RawData entry
pgfFile2rawData config pgffile = do
  pgf    <- readPGF pgffile

  let cats = nub [c | (c,(_,_,_)) <- M.assocs config]
  let lang:_ = languages pgf

  return [
    (cat, map snd lin) |
        cat  <- cats,
        f    <- functionsByCat pgf (mkCId cat),
        lin  <- tabularLinearizes pgf lang (mkApp f [])
    ]

type Cat  = String
type Fun  = String
type Oper = String
type Config = M.Map Cat (Cat,Oper,([Int],[Int])) -- lin word_Cat = Oper str_i1 str_i2 ... str_in  featj1 ... featjn ;

data MDEnv = MDEnv {
  rawdata :: [RawData],
  config  :: Config
  }

mkConfig :: String -> Config  -- N : N mkN 0 2 4 6 # 9
mkConfig ls = M.fromList [(c,i) | Left (c,i) <- map mkOne (lines ls)]
 where
  mkOne s = case words s of
    "--":_                 -> Right s
    cat:":":tcat:oper:ints -> Left (cat,(tcat,oper,mkArgs ints))
    _ -> Right s
  mkArgs ints = case break (=="#") ints of
    (ss,[])   -> (map read'  ss, [])
    (ss,_:fs) -> (map read' ss, map read' fs)
  read' a = readNote [] a -- Safe.readNote provides better error message

getRawData s = case words s of
  c:cs -> (c,cs)

type RawData  = (String,[String])                              -- old cat name, forms and features
type RawRule  = (([String],Cat), (Oper, ([String],[String])))  -- parts of fun name, new category, oper, arguments
type RuleData = ((Fun,    Cat),  (Oper, ([String],[String])))  -- final fun name, cat, oper, args

mkMorphoDict :: MDEnv -> ([String],[String])  -- fun rules, lin rules
mkMorphoDict env =
  unzip $
  map splitRule $
----  findCompounds $ -- let us not care about compounds for the time being, but include them if they are given
  nameFunctions $
  mergeRules $
  findRules $
  rawdata env
 where
  splitRule ((fun,cat),(oper,(forms,feats))) =
    (unwords ["fun",fun,":",cat,";"], unwords ["lin",fun,"=", unwords (oper : map quote forms ++ feats),";"])

  cats = nub [c | (c,(_,_,_)) <- M.assocs (config env)]

  findRules :: [RawData] -> [RawRule]
  findRules raws = [
    (([lemma],newcat),(oper, appSig sig args)) |
        (oldcat,args) <- raws,
        Just (newcat, oper, sig) <- [M.lookup oldcat (config env)],
        let lemma = args `at` head (fst sig)
   ]

  appSig (ints,feats) args =
    -- If there's wrong number in config file, uncomment the line below to see which number it should be
    -- trace (intercalate "\n" $ map show (zip [0..] args)) $
    ([args `at` i | i <- ints], [args `at` i | i <- feats])

  mergeRules :: [RawRule] -> [RawRule]
  mergeRules = map head . groupBy (\x y -> snd x == snd y) . sortOn snd

  nameFunctions :: [RawRule] -> [RuleData]
  nameFunctions = expandNames . sortOn fst

  expandNames :: [RawRule] -> [RuleData]
  expandNames fls = case fls of
    (f@(w,c),l) : fls2 -> case span ((==f) . fst) fls2 of
      ([],_) -> ((mkFun (w ++ [c]),c),l) : expandNames fls2
      (fls1,fls3) -> renames ((f,l):fls1) ++ expandNames fls3
    _ -> []

  renames :: [RawRule] -> [RuleData]
---  renames fls = [((mkFun (f ++ [show i,c]),c),l) | (i,((f,c),l)) <- zip [1..] fls] -- disambiguate with int
  renames fls = [((mkFun (f ++ fs ++ [c]),c),l) | (i,(((f,c),l),fs)) <- zip [1..] (zip fls (minimize fls))] -- disambiguate with different forms

  minimize :: [RawRule] -> [[String]]
  minimize fls = shrink [ws ++ fs | (_,(_,(_:ws,fs))) <- fls]

  shrink fls = case fls of
    fl@(_:_):_ | all ((==head fl) . head) fls -> shrink (map tail fls)
    fl@(_:_):_ | all ((==last fl) . last) fls -> shrink (map init fls)
    _ -> shrinkMore fls

  shrinkMore fls = case fls of
    _ | length (nub (map init fls)) == length fls -> shrinkMore (map init fls)
    _ | length (nub (map tail fls)) == length fls -> shrinkMore (map tail fls)
    _ -> fls

-- >>> mkFun ["hello", "world", "hello friends", "hello-all"]
-- "hello_world_hello_friends_hello_all"
mkFun :: [String] -> String                          -- if word contains space or hyphen, replace with underscore
mkFun = showCId . mkCId . concat . intersperse "_" . concatMap (words . removeHyphen)
  where
    removeHyphen [] = []
    removeHyphen ['-'] = ['-'] -- If hyphen is the last character, it's usually meaningful, leave it
    removeHyphen ('-':cs) = ' ' : removeHyphen cs
    removeHyphen (c:cs) = c : removeHyphen cs

quote s = "\"" ++ s ++ "\""

{- ---- let us ignore this
  findCompounds :: [RuleData] -> [RuleData]
  findCompounds = getCompounds . sortOn cat_orthrevforms

  cat_orthrevforms (_,(cat,_:forms)) = (cat,[map (!!i) fss | let fss = map reverse forms, i <- [0..minimum (map length fss) - 1]])

  cat_revforms (_,(cat,_:forms)) = (cat,map reverse forms)
  revstem = head . snd . cat_revforms
  wforms (_,(_,_:forms)) = forms

  getCompounds :: [RuleData] -> [RuleData]
  getCompounds fls = case fls of
    fl : fls1 | length (revstem fl) < 2 -> markWith fl [] : getCompounds fls1
    fl : fls2 -> case span (\x -> and [isPrefixOf (reverse w) (reverse w1) | (w,w1) <- zip (wforms fl) (wforms x)]) fls2 of
      ([],_:_) -> markWith fl [] : getCompounds fls2
      (fls1,fls3) -> markWith fl [] : map (markCompound fl) fls1 ++ getCompounds fls3
    _ -> []

  markCompound fl fl1 =
    case and [isPrefixWord (reverse w) (reverse w1) | (w,w1) <- zip (wforms fl) (wforms fl1)] of
      True  -> markWith fl1 [";","--","compound",(fst fl)]
      False -> markWith fl1 [";","--","notcompound",(fst fl)]

  markWith (f,(c,op:ws)) xs = (f,(c,op : map quote ws ++ xs)) ---- TODO only quote string args, not features

  isPrefixWord x xy =
    length suff > 1 &&                                ---- compound first part must be at least two letters long
    any (\c -> elem c "-0123456789aeiouyåäö") suff && ---- must contain a vowel or a digit
    isPrefixOf x xy                                   ---- and of course be a prefix
   where
     suff = drop (length x) xy
-}


