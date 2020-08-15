import System.Directory
import Data.Char
import Data.List

-- counting modules and lines in RGL implementations
-- leaving out Lexicon, Structural, dictionaries, Extra, Extend,... (see below the list of prefixes)

main = do
  mapM_ getCounts allLanguages

getCounts (lan,language) = do
  cs <- mapM (getCount language lan) prefixes
  let (ms,ls) = unzip cs
  putStrLn $ unwords [language, show (sum ms), show (sum ls)]
  
getCount language lan pref = do
  let file = concat ["../src/",language,"/",pref,lan,".gf"]
---  putStrLn file
  ms <- readFileIf file
  case ms of
    Nothing -> return (0,0)
    Just s  -> return (1,length (codeLines (lines s)))

codeLines ls = filter (\l -> not (all isSpace l || isPrefixOf "--" l)) ls

readFileIf name = do
  b <- doesFileExist name
  case b of
    True  -> readFile name >>= return . Just
    False -> return Nothing


prefixes = [
   "Adjective"
  ,"Adverb"
  ,"Cat"
  ,"Common"
  ,"Conjunction"
  ,"Diff"
  ,"Grammar"
  ,"Lang"
--  ,"Lexicon"
  ,"Noun"
  ,"Numeral"
  ,"Phrase"
  ,"Question"
  ,"Relative"
  ,"Sentence"
--  ,"Structural"
  ,"Tense"
  ,"Text"
  ,"Verb"
  ,"Res"
  ,"Morpho"
  ,"Phono"
  ,"Paradigms"
  ]

allLanguages = [
   ("",   "abstract")
  ,("Afr","afrikaans")
  ,("Ara","arabic")
---  ,("Bul","bulgarian") ---- todo convert to utf8
  ,("Cat","catalan")
  ,("Chi","chinese")
  ,("Dan","danish")
  ,("Dut","dutch")
  ,("Eng","english")
  ,("Est","estonian")
  ,("Eus","basque")
  ,("Fin","finnish")
  ,("Fre","french")
  ,("Ger","german")
  ,("Gre","greek")
  ,("Hin","hindi")
  ,("Ice","icelandic")
  ,("Ita","italian")
  ,("Jpn","japanese")
  ,("Lav","latvian")
  ,("Mlt","maltese")
  ,("Mon","mongolian")
  ,("Nep","nepali")
  ,("Nno","nynorsk")
  ,("Nor","norwegian")
  ,("Pes","persian")
  ,("Pnb","punjabi")
  ,("Pol","polish")
  ,("Por","portuguese")
----  ,("Ron","romanian")
  ,("Rus","russian")
  ,("Snd","sindhi")
  ,("Spa","spanish")
  ,("Swe","swedish")
  ,("Tha","thai")
  ,("Urd","urdu")
  ,("Romance","romance")
  ,("Scand","scandinavian")
  ,("Hindustani","hindustani")
  ]