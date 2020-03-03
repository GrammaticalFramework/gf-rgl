module Main where

import PGF

import qualified Data.Map as M
import Data.Char
import Data.List
import System.Environment (getArgs)

-- AR 2020-02-28

-- making a word list purely morphological, i.e.
--   - functions are 1-to-1 with lemgrams, i.e.
--     - no sense distinctions
--     - no subcategorizations
--     - no variants
--  - functionname = baseform_category, with exceptions
--     - variant inflection tables: lie_1_V, lie_2_V
--     - words that have non-ident characters: 'bird\'s-eye_A'
--     - words that start with non-letters: W_'tween_Adv

-- example:
--   gf -make ../english/DictEng.gf
--   runghc MkMorphodict.hs DictEngAbs.pgf MorphoDictEng
-- 64923 ->  56599 functions

usage = "MkMorphodict <pgf> <outfile>"

main = do
  pgfile:outfile:_ <- getArgs
  pgf <- readPGF pgfile
  config <- readFile (outfile ++ ".config") >>= return . mkConfig
  
  let (absrules,cncrules) = mkMorphoDict (MDEnv pgf config (head (languages pgf)))
  
  absheader <- readFile (outfile ++ "Abs.header")
  cncheader <- readFile (outfile ++ ".header")
  
  writeFile (outfile ++ "Abs.gf") absheader
  appendFile (outfile ++ "Abs.gf") $ unlines absrules
  appendFile (outfile ++ "Abs.gf") "}"
  
  writeFile (outfile ++ ".gf") cncheader
  appendFile (outfile ++ ".gf") $ unlines cncrules
  appendFile (outfile ++ ".gf") "}"


type Cat  = CId
type Oper = String
type Config = M.Map Cat (Cat,Oper,[Int])

data MDEnv = MDEnv {
  pgf    :: PGF,
  config :: Config,
  lang   :: Language
  }

mkConfig :: String -> Config
mkConfig ls = M.fromList [(c,i) | Left (c,i) <- map mkOne (lines ls)]
 where
  mkOne s = case words s of
    "--":_                 -> Right s 
    cat:":":tcat:oper:ints -> Left (mkCId cat,(mkCId tcat,oper,map read ints))
    _ -> Right s

mkMorphoDict :: MDEnv -> ([String],[String])
mkMorphoDict env =
  unzip $
  map splitRule $
  findCompounds $
  nameFunctions $
  mergeRules $
  concatMap findRules cats
 where
  splitRule (fun,(cat,lin)) = (unwords ["fun",fun,":",showCId cat,";"], unwords ["lin",fun,"=", unwords lin,";"])

  cats = nub [c | (c,(_,_,_)) <- M.assocs (config env)]

  findRules cat = [
    ([snd (lin !! head ints), showCId c], (c, op : appSig ints (map snd lin))) |  --- head ints is the base form in smart paradigms
      f    <- functionsByCat (pgf env) cat,
      lin  <- tabularLinearizes (pgf env) (lang env) (mkApp f []), -- [[(String, String)]]
      Just (c,op,ints) <- [M.lookup cat (config env)]
   ] 

  appSig ints forms = [forms !! i | i <- ints]

  mergeRules = map head . groupBy (\x y -> snd x == snd y) . sortOn snd

  nameFunctions = expandNames . sortOn fst

  expandNames fls = case fls of
    (f,l):fls2 -> case span ((==f) . fst) fls2 of
      ([],_) -> (mkFun f,l) : expandNames fls2
      (fls1,fls3) -> renames ((f,l):fls1) ++ expandNames fls3
    _ -> []

  renames fls = [(mkFun (init f ++ [show i,last f]),l) | (i,(f,l)) <- zip [1..] fls]

  findCompounds = getCompounds . sortOn cat_orthrevforms

  cat_orthrevforms (_,(cat,_:forms)) = (cat,[map (!!i) fss | let fss = map reverse forms, i <- [0..minimum (map length fss) - 1]])

  cat_revforms (_,(cat,_:forms)) = (cat,map reverse forms)
  revstem = head . snd . cat_revforms
  wforms (_,(_,_:forms)) = forms

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

  markWith (f,(c,op:ws)) xs = (f,(c,op : map quote ws ++ xs))

  isPrefixWord x xy =
    length suff > 1 &&
    any (\c -> elem c "-0123456789aeiouyåäö") suff &&
    isPrefixOf x xy
   where
     suff = drop (length x) xy

mkFun = quoteIf . concat . intersperse "_"
quoteIf s = case s of
  _ | any (\c -> not (isAlphaNum c || elem c "_'")) s -> "'" ++ unSgQuote s ++ "'"
  c:_ | not (isAlpha c) -> "W_" ++ s
  _ -> s
 where
  unSgQuote s = case s of
    '\'':cs -> "\\\'" ++ unSgQuote cs
    c:cs -> c : unSgQuote cs
    _ -> s


quote s = "\"" ++ s ++ "\""

