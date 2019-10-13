import qualified Data.Map
import qualified Data.Text.IO
import Data.Char
import Data.List
import System.Directory

-- AR 2019-08-06
-- checking IrregIce wrt Wikipedia:
--
-- > getAllWikt, producing wiktVerbDataIce.txt
-- > correctAllRGL, producing rgl-corrected.tmp, rgl-corrected.gf

-- first do:
-- (gf alltenses/IrregIce.gfo)  > gt -cat=V | l -table | wf -file="irregsIce.tmp"
-- % wget https://en.wiktionary.org/wiki/<each-verb-infinitive> from the 'verbs' list given below

main = correctAllRGL

correctAllRGL = do
  ls <- readFile "irregsIce.tmp" >>= return . lines
  let vs = getGroups ls
  let vis = [(actInf v,v) | v <- vs]
  cvs <- mapM (uncurry rgl2wik) vis
  writeFile "rgl-corrected.tmp" $ unlines $ concat cvs
  let cgf = map (uncurry fixedRGL) (zip (map fst vis) cvs)
  writeFile "rgl-corrected.gf" $ concat $ Data.List.intersperse ", " [f++"_V" | (Just f,_) <- cgf]
  appendFile "rgl-corrected.gf" $ concat $ map (unlines . snd) $ cgf



-- this produces an inspectable file with extracted Wiktionary data, including deviating layouts
getAllWikt = do
  vs <- mapM getWikt verbs
  writeFile "wiktVerbDataIce.txt" $ unlines $ concat $ Data.List.intersperse ["------"] vs

verbs = ["aka","ala","auka","ausa","bera","binda","bi\240ja","bjarga","bl\225sa","brenna","bresta","brj\243ta","b\237ta","b\237\240a","b\250a","detta","deyja","draga","drekka","drepa","drj\250pa","dr\237fa","eiga","falla","fara","fela","finna","fj\250ka","flj\243ta","flj\250ga","fl\225","frj\243sa","f\225","gala","ganga","gefa","geta","geyja","gjalda","gjalla","gj\243sa","grafa","gr\225ta","gr\237pa","gr\243a","halda","hanga","hefja","heita","hlaupa","hla\240a","hlj\243ta","hl\230ja","hnj\243ta","hn\237ga","hrinda","hr\237fa","hr\237na","hr\246kkva","hverfa","hv\237na","h\246ggva","kala","kefja","kj\243sa","klj\250fa","koma","kunna","kve\240a","kv\237\240a","leika","leka","lesa","liggja","lj\250ga","lj\250ka","l\225ta","l\237ta","l\237\240a","l\250ka","l\250ta","mala","mega","meta","muna","munu","m\237ga","nema","nj\243ta","n\250a","reka","renna","rj\250fa","r\225\240a","r\237sa","r\237\240a","r\243a","sitja","sj\225","sj\243\240a","sj\250ga","skafa","skaka","skapa","skella","skera","skj\225lfa","skreppa","skr\237\240a","skulu","sk\237na","sleppa","sl\225","sl\237ta","sl\246kkva","smella","smj\250ga","snerta","sn\237\240a","sn\250a","sofa","spinna","spretta","springa","sp\250a","standa","stela","stinga","strj\250ka","st\237ga","st\246kkva","svelta","sverfa","sverja","sv\237fa","sv\237kja","syngja","s\237ga","s\246kkva","s\250pa","taka","tro\240a","unna","vaxa","vega","vella","velta","vera","verpa","ver\240a","vilja","vinda","vinna","vita","v\237kja","\233ta","\254iggja","\254j\243ta","\254rj\243ta","\254r\237fa","\254urfa","\254verra","\254vo"]

-- not all of these are in Wiktionary
rgl_verbs = ["aka","ala","auka","ausa","bera","binda","bi\240ja","bjarga","bl\225sa","brenna","bresta","brj\243ta","b\237ta","b\237\240a","b\250a","detta","deyja","draga","drekka","drepa","drj\250pa","dr\237fa","eiga","falla","fara","fela","finna","fj\250ka","flj\243ta","flj\250ga","fl\225","frj\243sa","f\225","gala","ganga","gefa","geta","geyja","gjalda","gjalla","gj\243sa","grafa","gr\225ta","gr\237pa","gr\243a","halda","hanga","hefja","heita","hlaupa","hla\240a","hlj\243ta","hl\230ja","hnj\243ta","hn\237ga","hrinda","hr\237fa","hr\237na","hr\246kkva","hverfa","hv\237na","h\246ggva","kala","kefja","kj\243sa","klj\250fa","kl\230ja","koma","kunna","kve\240a","kv\237\240a","leika","leka","lesa","liggja","lj\250ga","lj\250ka","l\225ta","l\237ta","l\237\240a","l\250ka","l\250ta","mala","mega","meta","muna","munu","m\237ga","nema","nj\243ta","n\250a","reka","renna","rj\250fa","r\225\240a","r\237sa","r\237\240a","r\243a","sitja","sj\225","sj\243\240a","sj\250ga","skafa","skaka","skapa","skekja","skella","skera","skj\225lfa","skreppa","skr\237\240a","skulu","sk\237na","sleppa","sl\225","sl\237ta","sl\246kkva","smella","smj\250ga","snerta","sn\237\240a","sn\250a","sofa","spinna","spretta","springa","sp\250a","standa","stela","stinga","strj\250ka","st\237ga","st\246kkva","svelta","sverfa","sverja","sv\237fa","sv\237kja","syngja","s\237ga","s\246kkva","s\250pa","taka","tro\240a","unna","vaxa","vega","vella","velta","vera","verpa","ver\240a","vilja","vinda","vinna","vita","v\237kja","\233ta","\254iggja","\254j\243ta","\254rj\243ta","\254r\237fa","\254urfa","\254verra","\254vo"]

-- Wiktionary table for one verb
getWikt :: FilePath -> IO [String]
getWikt file =
  if elem file verbs
  then putStrLn ("YES: " ++ file) >> readFile file >>= checkPrefixes file . take 75 . dropWhile (not . isStart file) . map untag . getTD . lines
  else putStrLn ("NO: " ++ file) >> return []

-- check that a Wiktionary file has expected layout
--- this is only a partial check
checkPrefixes :: String -> [String] -> IO [String]
checkPrefixes file ls =
  if length ls < 75
  then putStrLn ("too short " ++ file) >> return []
  else case unexpectedWikLines ls of
    [] -> return ls
    us -> putStrLn (unlines (("unexpected layout in " ++ file) : us)) >> return []

-- remove HTML tags
untag s = case s of {'<':cs -> intag cs ; c:cs -> c:untag cs ; _ -> s}
  where intag s = case s of '>':cs -> untag cs ; _:cs -> intag cs

-- consider td cells
getTD = filter (\l -> take 3 l == "<td")

-- start from the Icelandic infinitive form
isStart inf s = words s == ["að",inf]

-- find the word from each td cell; return ZZ if not found
wform wik = last $ "ZZ":(filter (all Data.Char.isAlpha) wik)

-- find the Wiktionary forms for each branch in the RGL table
wforms ws = [(i, (if length ws < j then "XX" else wform (words (ws !! (j-1))))) | (i,j) <- rgl2wiktLines]

-- to correct one RGL table by reference to the Wiktionary data file
rgl2wik :: String -> [String] -> IO [String]
rgl2wik i v = do
  fs <- getWikt i
  let wmap = Data.Map.fromList (wforms fs)
  putStrLn i
  print $ Data.Map.size wmap
  let facit = [unwords (gws ++ correctForm wmap (j,last gws)) | (j,gw) <- zip [1..] v, let gws = words gw]
  return $ status i facit : facit

-- fix one entry, provided it has more than 0 deviations
fixedRGL :: String -> [String] -> (Maybe String,[String])
fixedRGL i (s:rs) =
  if last (words s) == "0"
  then (Nothing,[])
  else (Just i, [
    "lin " ++ i ++ "_V = OI." ++ i ++ "_V ** {",
    "  s = table {"
    ] ++
    fixes rs ++
    [
    "    vf => OI." ++ i ++ "_V.s ! vf",
    "     } ;",
    "  } ;"
    ]
    )

-- produces the branch for a form that is fixed
fixes rs = ["    " ++ vf ++ " => \"" ++ form ++ "\" ; --- " ++ reverse wr !! 2 |
  r <- rs, let wr = words r, head wr == "s", elem "*" wr, notElem "XX" wr, notElem "ZZ" wr,
  let vf = takeWhile (/=')') $ drop 1 $ dropWhile (/='(') r,
  let form = last wr
  ]

-- line in RGL table -> line in Wik data
rgl2wiktLines = [(1,1),(2,38),(3,6),(4,10),(5,14),(6,7),(7,11),(8,15),(9,8),(10,12),(11,16),(12,9),(13,13),(14,17),(15,43),(16,47),(17,51),(18,44),(19,48),(20,52),(21,45),(22,49),(23,53),(24,46),(25,50),(26,54),(27,19),(28,23),(29,27),(30,20),(31,24),(32,28),(33,21),(34,25),(35,29),(36,22),(37,26),(38,30),(39,56),(40,60),(41,64),(42,57),(43,61),(44,65),(45,58),(46,62),(47,66),(48,59),(49,63),(50,67),(51,35),(52,33),(53,72),(54,70),(55,2),(56,39),(81,75),(105,3)]

-- prefixes on each line of valid Wik data, as extracted from the verb "fara"
expectedPrefixes = map unwords [["a\240"],[],[],[],[],["\233g"],["vi\240"],["\233g"],["vi\240"],["\254\250"],["\254i\240"],["\254\250"],["\254i\240"],["hann"],["\254eir"],["hann"],["\254eir"],[],["\233g"],["vi\240"],["\233g"],["vi\240"],["\254\250"],["\254i\240"],["\254\250"],["\254i\240"],["hann"],["\254eir"],["hann"],["\254eir"],[],[],[],[],[],[],[],["a\240"],[],[],[],[],["\233g"],["vi\240"],["\233g"],["vi\240"],["\254\250"],["\254i\240"],["\254\250"],["\254i\240"],["hann"],["\254eir"],["hann"],["\254eir"],[],["\233g"],["vi\240"],["\233g"],["vi\240"],["\254\250"],["\254i\240"],["\254\250"],["\254i\240"],["hann"],["\254eir"],["hann"],["\254eir"],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]]

-- mark Wiktionary data lines not matching the layout, and therefore ignored in the main function
unexpectedWikLines ws = [l ++ " --" ++ p | (l,p) <- zip ws expectedPrefixes, not (Data.List.isPrefixOf p l)]

-- if a form is not found in Wikt, return ?? ; this is not used as a correction
correctForm wmap (j,w) = case Data.Map.lookup j wmap of
  Just v | v == w -> []
  Just v -> ["*",v]
  _ -> ["??"]

--- used on the manually marked Wiktionary list for the verb "fara"; this became the expected layout
wformsIfMarked ws = [(read (last ww) :: Int, (wform ww, i))  |
  (i,w) <- zip [1..] ws, let ww = words w, not (null ww), all Data.Char.isDigit (last ww)]

-- the status is the number of deviations in a verb's table
status :: String -> [String] -> String
status inf ls = unwords $ "STATUS" : inf : [show (deviations ls)]

-- deviations in the intermediate file are marked with *; XX and ZZ are not included as correct forms
deviations :: [String] -> Int
deviations ls = length [l | l <- ls, let r = reverse (words l), r!!1 == "*", notElem (head r) ["XX","ZZ"]]

-- an RGL verb table has length 105
getGroups ls = let (v,vs) = splitAt 105 ls in if null v then [] else v:getGroups vs

actInf v@(i:_) = last (words i)

jumpToIcelandic ls = dropWhile (\l -> not (isPrefixOf "<h2>" l && isPrefixOf "Icelandic" (untag l))) ls

----------------------------------------------------------------------
-- just retrieving, instead of checking existing GF files ------------

-- to be run in wiktionary/, with subdirs nouns/ adjectives/ verbs/

-- to be run in adjectives/
getAllWiktNouns = do
  vs <- readFile "nouns/wikt-nouns.txt" >>= return . lines
  writeFile "n.tmp" ""
  mapM_ (\v -> getWiktNoun "nouns/" v >>= appendFile "n.tmp" . unlines . emitGF) vs

getAllWiktAdjectives = do
  vs <- readFile "adjectives/wikt-adjectives.txt" >>= return . lines
  writeFile "a.tmp" ""
  mapM_ (\v -> getWiktAdjective "adjectives/" v >>= appendFile "a.tmp" . unlines . emitGF) vs

getAllWiktVerbs = do
  vs <- readFile "verbs/wikt-verbs.txt" >>= return . lines
  writeFile "v.tmp" ""
  mapM_ (\v -> getWiktVerb "verbs/" v >>= appendFile "v.tmp" . unlines . emitGF) vs



-- return ([relevant Wikt lines], (fun,cat,lin), message)
getWiktWord :: Int -> (String -> [String] -> ([String],((String,String,String),Message))) -> FilePath -> FilePath -> IO ([String],((String,String,String),Message))
getWiktWord number check dir file = do
  let dirfile = dir++file
  ex <- doesFileExist dirfile
  if not ex
    then return ([],(noGF,MBad (file ++ " does not exist")))
    else do
      s <- readFile dirfile >>= return . map untag . take number . getTD . jumpToIcelandic . lines
      return $ check file s

getWiktNoun = getWiktWord 17 checkNoun
getWiktAdjective = getWiktWord 120 checkAdjective
getWiktVerb = getWiktWord 75 checkVerb

noCheck :: String -> [String] -> ([String],((String,String,String),Message))
noCheck s ss = (ss, (noGF, MMissing s))
noGF = ("--","--","--")

checkNoun noun forms = case length forms of
----  n | n <  24 -> (forms, (noGF, MBad (adj ++ " A: only " ++ show (length forms) ++ " lines")))
  n | n < 17 -> (forms, (noGF, MBad (noun ++ " N: only " ++ show (length forms) ++ " lines")))
  _          -> (forms, checkZZ noun (noun ++ "_N", "N", "mkgN " ++ gender (forms!!0), [forms!!i | i <- [1,5,9,13,3,7,11,15]]))
 where
   gender s = case take 1 s of
     "m" -> "masculine"
     "f" -> "feminine"
     _ -> "neuter" --- "n"


checkAdjective adj forms = case length forms of
  n | n <  24 -> (forms, (noGF, MBad (adj ++ " A: only " ++ show (length forms) ++ " lines")))
  n | n < 120 -> (forms, checkZZ adj (adj ++ "_A", "A", "mkA", [forms!!0, forms!!1]))
  _           -> (forms, checkZZ adj (adj ++ "_A", "A", "mkA", [forms!!0, forms!!1,forms!!48]))

checkVerb verb forms = case length forms of
  n | n <  75 -> (forms, (noGF, MBad (verb ++ " V: only " ++ show (length forms) ++ " lines")))
  _   -> case unexpectedWikLines forms of
    [] -> (forms, checkZZ verb (verb ++ "_V", "V", "mkV", [verb, forms!!5, forms!!18, forms!!74, forms!!1]))
    us -> (forms, (noGF, MBad (verb ++ " V: unexpected lines " ++ show (length us))))

data Message =
    MGood String
  | MBad String
  | MMissing String
   deriving (Show,Eq)

app f xs = unwords $ f : map (quote . wform . words) xs
quote s = "\"" ++ s ++ "\""

checkZZ w (fun,cat,lin,args) =
  if elem "ZZ" (map (wform . words) args)
  then (noGF, MBad (w ++ " " ++ cat ++ ": missing forms in data"))
  else ((fun,cat, app lin args),MGood w)

emitGF (ss,((fun,cat,lin),msg)) = case msg of
  MGood _ -> [unwords ["fun",fun,":",cat,";"],unwords ["lin",fun,"=",lin,";"]]
  _ -> ["-- " ++ show msg]


-- mkN : (x1,_,_,_,_,_,_,x8 : Str) -> Gender -> N = mk8N ; nForms8 a b c d e f g h  ; sgNom,sgAcc,sgDat,sgGen,plNom,plAcc,plDat,plGen
-- mkA : (_,_,_ : Str) -> A = mk3A ; 	mk3A : (_,_,_ : Str) -> A = \mas,fem,com
-- mkV : (_,_,_,_,_ : Str) -> V = \telja,tel,taldi,talinn,talið ->   -- inf,presIndSg1,pastIndSg1, weak past part, sup


