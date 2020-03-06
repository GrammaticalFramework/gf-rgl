import Data.List
import qualified Data.Map as M

-- AR 2020-03-03
-- generating GF from preprocessed SALDO (of type Lex by John Camilleri)

main = do
  lexicon <- readFile "saldom.hsdump" >>= return . readLex  -- this is the preprocessed file
  let gf = map (mkRules . treatNone) $ mkFuns lexicon
  writeFile "abs.tmp" $ unlines $ map fst gf  -- the generated files need headers
  writeFile "cnc.tmp" $ unlines $ map snd gf  -- use SaldoGF.header for this

-- JC's datatypes, using String for simplicity

type Lex = M.Map String Entry -- key is lemgram ID

type Table = [(String,String)]

data Entry = E
  { ePOS :: String
  , eTable :: Table -- morphological tags to surface form: ("sg def gen" ,"killens")
  } deriving (Show, Read)

readLex :: String -> [(String,Entry)]
readLex = read . drop 8

-- new code by AR

mkRules (fun,cat,lin) = (nunwords ["fun",fun,":",cat,";"],nunwords ["lin",fun,"=",lin,";"])
 where
  -- commenting out functions that still have NONE forms
  nunwords ws = unwords ((if elem "\"NONE\"" (words lin) then ["--n"] else []) ++ ws)

-- converting incomplete paradigms to special mkC constructors, defined in SaldoSwe.header
treatNone (f,cat,lin) = case (cat,drop 1 (words lin)) of
  ("V", "\"NONE\"":"\"NONE\"":v:_) -> (f, "V", unwords ("mkVDep":[v]))
  ("V", i:d:p:a:b:"\"NONE\"":_) -> (f, "V", unwords ("mkVIntr":[i,d,p,a,b]))
  ("A", i:"\"NONE\"":p:c:s:_) -> (f, "A", unwords ("mkAUtr":[i,p,c,s]))
  ("A", i:d:p:"\"NONE\"":"\"NONE\"":_) -> (f, "A", unwords ("mkAComp":[i,d,p]))
  ("N", "\"NONE\"":d:"\"NONE\"":_) -> (f, "PN", unwords ("mkPNDef":[d])) ---
  ("N", i:"\"NONE\"":"\"NONE\"":_) -> (f, "PN", unwords ("mkPNIndef":[i]))
  ("N", i:d:"\"NONE\"":"\"NONE\"":_) -> (f, "N", unwords ("mkNSg":[i,d]))
  ("N", "\"NONE\"":"\"NONE\"":i:d:_) -> (f, "N", unwords ("mkNPl":[i,d]))
  _ -> (f,cat,lin)

--- generating function names for simplicity: the result is fed to ../MkMorphoDict anyway
mkFuns lx = [("w"++show i, cat, lin) | (i,(cat,lin)) <- zip [1000000..] (concatMap (entry2lin . snd) lx)]

entry2lin e =
   [(cat, mkLin cat ws) | ws <- manyTables valuess]
  where
    (cat,forms) = formSpec (ePOS e)
    valuess = [nub [v | (t,v) <- eTable e, t == f] | f <- forms]
    mkLin c ws = unwords $ ["mk"++c] ++ ["\"" ++ w ++ "\"" | w <- ws]

-- looking for the characteristic forms for each POS

formSpec pos = case pos of
  "nn" -> ("N",[
    "sg indef nom",
    "sg def nom",
    "pl indef nom",
    "pl def nom"
    ])
  "av" -> ("A",[
    "pos indef sg u nom",
    "pos indef sg n nom",
    "pos indef pl nom",
    "komp nom",
    "super indef nom"
    ])
  "vb" -> ("V",[
    "inf aktiv",
    "pres ind aktiv",
    "imper",
    "pret ind aktiv",
    "sup aktiv",
    "pret_part indef sg u nom"
    ])
  "ab" -> ("Adv",[
    "invar"
----    "pos"
    ])
  "pp" -> ("Prep",[
    "invar"
    ])
  _ -> ("NONE++pos",["NONE++pos"]) -- ignoring other POS tags, which are rare anyway

-- trying to generate a small number of tables from sets of variant forms; seems to work well enough

manyTables formss = [
  map ((!!i) . pad) formss |
    i <- [0..maximum (map length formss)-1],
    let pad forms = if null forms then repeat "NONE" else forms ++ repeat (head forms)
  ]


