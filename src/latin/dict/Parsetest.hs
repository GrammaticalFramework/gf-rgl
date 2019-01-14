{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Text.Parsec.Error
import Data.Char
import Data.Maybe
import System.IO
import Data.List
import Debug.Trace
data Case = Nom | Gen | Dat | Acc | Abl deriving Show
data Gender = Masc | Fem | Neuter | Common | Unknown
data Cat = N | V | A | Adv | Interj | Prep | Num | Conj | Pron deriving Show;
data VerbKind = Trans | Intrans | Deponens | SemiDeponens | Impersonal deriving Show
data Comparison = Positive | Comparative | Superlative | PosComp | CompSuper | PosCompSuper | Undeclined deriving Show;
data DictEntry = Entry { cat :: Cat , forms :: [String] , other :: OtherInfos , comment :: String } | EmptyEntry deriving Show;
data OtherInfos = Noun { iclass :: Maybe Int , gen :: Gender } | Adjective { iclass :: Maybe Int , comp :: Comparison } | Verb { iclass :: Maybe Int , kind :: VerbKind } | Preposition { c :: Case } | NoInfos deriving Show;

instance Show Gender where
  show Masc = "masculine"
  show Fem = "feminine"
  show Neuter = "neuter"
  show Common = "(feminine | masculine)"
  show Unknown = ""
  
hash :: Stream s m Char => ParsecT s u m Char
hash = char '#'
    
wordForm :: Stream s m Char => ParsecT s u m String
wordForm =
  let
    esse :: Stream s m Char => ParsecT s u m String
    esse =
      string "sum" <|> string "est"
    suffix :: Stream s m Char => ParsecT s u m String
    suffix =
      do
        d <- char '-'
        sfx <- many letter
        spcs <- many space
        return $ d : sfx ++ spcs
    genitive :: Stream s m Char => ParsecT s u m String
    genitive =
      do
        p1 <- string "(gen"
        p2 <- many (noneOf ")")
        p3 <- string ")"
        return $ p1 ++ p2 ++ p3
  in
    do
      w0 <- optionMaybe (char '-' <|> letter)
      ws <- if isJust w0 then many (letter <|> oneOf "()/") else return ""
      s1 <- many space
      s <- option [] $ try $ esse
      s2 <- many space
      g <- option [] $ try genitive
      s3 <- many space
      sfs <- option [] $ try (many suffix)
      return $ maybe ws (:ws) w0 ++ s1 ++ s ++ s2 ++ g ++ s3 ++ concat sfs
      
wordForms :: Stream s m Char => ParsecT s u m [String]
wordForms =
  do
    wf <- wordForm;
    wfs <- many (do
                    _ <- char ','
                    skipMany space
                    wordForm
                )
    return (wf:wfs)

gender :: Stream s m Char => ParsecT s u m Gender
gender =
  do
    skipMany space
    c <- upper
    case c of {
      'N' -> return Neuter ;
      'M' -> return Masc ;
      'F' -> return Fem ;
      'C' -> return Common; -- Feminine and/or masculine
      'X' -> return Unknown;
      _ -> unexpected ("unhandled gender " ++ [c])
      }
      
      
inflection :: Stream s m Char => ParsecT s u m (Maybe Int)
inflection =
  do
    _ <- char '(';
    i <- digit ;
    _ <- string "st" <|> string "nd" <|> string "rd" <|> string "th" ;
    _ <- char ')';
    return $ Just $ digitToInt i
 
verbkind :: Stream s m Char => ParsecT s u m VerbKind
verbkind =
  do
    s <- many upper
    case s of {
      "INTRANS" -> return Intrans;
      "TRANS" -> return Trans;
      "DEP" -> return Deponens;
      "SEMIDEP" -> return SemiDeponens;
      "IMPERS" -> return Impersonal;
      _ -> unexpected s -- Probably just return intrans?
      }

cases :: Stream s m Char => ParsecT s u m Case
cases =
  do
    s <- many upper
    case s of {
      "ACC" -> return Acc;
      "ABL" -> return Abl;
      _ -> unexpected s
      }

rest :: Stream s m Char => ParsecT s u m String
rest =
  do
    c0 <- char '['
    cs <- many anyChar
    return (c0:cs)
    
noun :: Stream s m Char => ParsecT s u m DictEntry
noun =
   do
     wfs <- wordForms
     _ <- char 'N'
     skipMany space
     i <- option Nothing inflection
     g <- gender
     skipMany space
     c <- rest
     return $ Entry N wfs (Noun i g) c
    
verb :: Stream s m Char => ParsecT s u m DictEntry
verb =
  do
    wfs <- wordForms
    _ <- string "V"
    skipMany space
    i <- option Nothing inflection
    skipMany space
    k <- option Intrans verbkind
    skipMany space
    c <- rest
    return $ Entry V wfs (Verb i k) c

adjective :: Stream s m Char => ParsecT s u m DictEntry
adjective =
  let
    dropLast :: Int -> String -> String
    dropLast i s = take (length s - i) s
    fixStuff :: [String] -> ([String],OtherInfos)
    fixStuff wfs@[f1,f2]
      | isSuffixOf "or -or -us" f1 && isSuffixOf "us -a -um" f2 =
        let
          stem1 = dropLast 10 f1
          stem2 = dropLast 9 f2
        in
          ([stem1 ++ "or", stem1 ++ "or", stem1 ++ "us", stem2 ++ "us", stem2 ++ "a", stem2 ++ "um"],
           Adjective (Just 3) CompSuper)
      | f2 == "undeclined" = (wfs, Adjective (Just (-1)) Undeclined)
    fixStuff wfs@[f1,f2,f3]
      | isSuffixOf "us" f1 && isSuffixOf "a" f2 && isSuffixOf "um" f3 = (wfs, Adjective (Just 1) Positive)
      | isSuffixOf "r" f1 && isSuffixOf "ra" f2 && isSuffixOf "rum" f3 = (wfs, Adjective (Just 1) Positive)
      | isSuffixOf "is" f1 && isSuffixOf "is" f2 && isSuffixOf "e" f3 = ([f1,f3], Adjective (Just 3) Positive)
      | isSuffixOf "er" f1 && isSuffixOf "is" f2 && isSuffixOf "e" f3 = (wfs, Adjective (Just 3) Positive)
      | isSuffixOf "os" f1 && isSuffixOf "os" f2 && isSuffixOf "on" f3 = (wfs, Adjective (Just (-1)) Positive) -- Greek stuff
      | isSuffixOf "os" f1 && isSuffixOf "a" f2 && isSuffixOf "on" f3 = (wfs, Adjective (Just (-1)) Positive) -- Green stuff
      | f2 == "(gen.)" = ([f1,f3], Adjective (Just 3) Positive)
        -- acer, acris -e, acrior -or -us, acerrimus -a -um  ADJ
--        abjectus, abjecta -um, abjectior -or -us, abjectissimus -a -um
    fixStuff wfs@[f1,f2,f3,f4]
      | isSuffixOf "us" f1 && isSuffixOf "a -um" f2 && isSuffixOf "or -or -um" f3 && isSuffixOf "us -a -um" f4 =
        let
          stem1 = dropLast 2 f1
          stem2 = dropLast 10 f3
          stem3 = dropLast 9 f4
        in
          ([f1, stem1 ++ "a", stem1 ++ "um",
            stem2 ++ "or", stem2 ++ "or", stem2 ++ "um",
            stem3 ++ "us", stem3 ++ "a", stem3 ++ "um"],
           Adjective (Just 1) PosCompSuper)
      | isSuffixOf "us" f1 && isSuffixOf "a -um" f2 && isSuffixOf "or -or -us" f3 && isSuffixOf "us -a -um" f4 =
        let
          stem1 = dropLast 2 f1
          stem2 = dropLast 10 f3
          stem3 = dropLast 9 f4
        in
          ([f1, stem1 ++ "a", stem1 ++ "um",
            stem2 ++ "or", stem2 ++ "or", stem2 ++ "us",
            stem3 ++ "us", stem3 ++ "a", stem3 ++ "um"],
           Adjective (Just 1) PosCompSuper)
      | isSuffixOf "ns" f1 && isSuffixOf "ntis (gen.)" f2 && isSuffixOf "or -or -us" f3 && isSuffixOf "us -a -um" f4 =
        let
          stem1 = dropLast 11 f2
          stem2 = dropLast 10 f3
          stem3 = dropLast 9 f4
        in
          ([f1, stem1 ++ "ntis",
            stem2 ++ "or", stem2 ++ "or", stem2 ++ "us",
            stem3 ++ "us", stem3 ++ "a", stem3 ++ "um"],
           Adjective (Just 3) PosCompSuper)
      | isSuffixOf "er" f1 && isSuffixOf "is -e" f2 && isSuffixOf "or -or -us" f3 && isSuffixOf "us -a -um" f4 =
          let
            stem1 = dropLast 5 f2
            stem2 = dropLast 10 f3
            stem3 = dropLast 9 f4
          in
            ([f1, stem1 ++ "is", stem1 ++ "e",
              stem2 ++ "or", stem2 ++ "or", stem2 ++ "us",
              stem3 ++ "us", stem3 ++ "a", stem3 ++ "um"],
             Adjective (Just 3) PosCompSuper)
      | isSuffixOf "er" f1 && isSuffixOf "a -um" f2 && isSuffixOf "or -or -us" f3 && isSuffixOf "us -a -um" f4 =
          let
            stem1 = dropLast 5 f2
            stem2 = dropLast 10 f3
            stem3 = dropLast 9 f4
          in
            ([f1, stem1 ++ "a", stem1 ++ "um",
              stem2 ++ "or", stem2 ++ "or", stem2 ++ "us",
              stem3 ++ "us", stem3 ++ "a", stem3 ++ "um"],
             Adjective (Just 1) PosCompSuper)
      | isSuffixOf "is" f1 && isSuffixOf "e" f2 && isSuffixOf "or -or -us" f3 && isSuffixOf "us -a -um" f4 =
          let
            stem2 = dropLast 10 f3
            stem3 = dropLast 9 f4
          in
            ([f1, f2,
              stem2 ++ "or", stem2 ++ "or", stem2 ++ "us",
              stem3 ++ "us", stem3 ++ "a", stem3 ++ "um"],
             Adjective (Just 1) PosCompSuper)
    fixStuff wfs = trace ("ERROR: " ++ show wfs) (wfs,NoInfos)
  in
    do
      wfs <- wordForms
      _ <- string "ADJ"
      -- Set additional infos/normalize foobar
      --      i <- return Nothing
      skipMany space
      c <- rest
      let (nwfs,ninfos) = fixStuff $ map skipSpaces wfs -- (Adjective i)
      return $ Entry A nwfs ninfos c
    
adverb :: Stream s m Char => ParsecT s u m DictEntry
adverb =
  do
    wfs <- wordForms
    _ <- string "ADV"
    skipMany space
    c <- rest
    return $ Entry Adv wfs NoInfos c

interjection :: Stream s m Char => ParsecT s u m DictEntry
interjection =
  do
    wfs <- wordForms
    _ <- string "INTERJ"
    skipMany space
    c <- rest
    return $ Entry Interj wfs NoInfos c

preposition :: Stream s m Char => ParsecT s u m DictEntry
preposition =
  do
    wfs <- wordForms
    _ <- string "PREP"
    skipMany space
    c <- cases
    skipMany space
    cc <- rest
    return $ Entry Prep wfs (Preposition c) cc
    
wrongLine :: Stream s m Char => ParsecT s u m DictEntry
wrongLine =
  do
    line <- many1 $ noneOf "["
    unexpected $ "Unexpected line " ++ line
    
lineParser :: Stream s m Char => ParsecT s u m DictEntry
lineParser =
  do
    _ <- hash
    (try noun <|> try verb <|> try adjective <|> try adverb <|> try interjection <|> try preposition) <|> wrongLine

skipSpaces :: String -> String
skipSpaces = dropWhileEnd isSpace --filter (/= ' ')
  
write :: FilePath -> FilePath -> [(Int,DictEntry)] -> IO ()
write abs conc es =
  let
    join :: String -> [String] -> String
    join g l = foldl (\f s -> f ++ g ++ s ) (head l) (tail l)
    writeEntry :: Handle -> Handle -> [(Int,DictEntry)] -> IO ()
    writeEntry habs hconc [] =
      do
        hPutStrLn habs "}"
        hClose habs
        hPutStrLn hconc "}"
        hClose hconc
    writeEntry habs hconc (e@(i,Entry cat forms _ _):es) =
      do
        let lemma = skipSpaces $ head forms
        hPutStrLn habs $ "  " ++ lemma ++ "_" ++ show i ++ "_" ++ show cat ++ " : " ++ show cat ++ ";";
        case e of {
          (i,Entry N wfs (Noun (Just 1) gender) c) -> hPutStrLn hconc $ "  " ++ lemma ++ "_" ++ show i ++ "_N = mkN \"" ++ head wfs ++ "\"; -- 1st " ++ show gender ++ " " ++ c;
          (i,Entry N wfs (Noun (Just 2) gender) c) -> hPutStrLn hconc $ "  " ++ lemma ++ "_" ++ show i ++ "_N = mkN \"" ++ head wfs ++ "\"; -- 2nd " ++ show gender ++ " " ++ c;
          (i,Entry N wfs (Noun (Just 3) gender) c) -> hPutStrLn hconc $ "  " ++ lemma ++ "_" ++ show i ++ "_N = mkN \"" ++ (join "\" \"" $ map skipSpaces wfs) ++ "\" " ++ show gender ++ "; -- 3rd " ++ show gender ++ " " ++ c;
          (i,Entry A wfs (Adjective (Just 1) comp@Positive) c) -> hPutStrLn hconc $ "  " ++ lemma ++ "_" ++ show i ++ "_N = mkA \"" ++ head wfs ++ "\"; -- 1st " ++ show comp ++ " " ++ c;
          _ -> return () -- print e

          }
        writeEntry habs hconc es
  in
    do
      habs <- openFile abs WriteMode
      hconc <- openFile conc WriteMode
      hPutStrLn habs "abstract DictLatAbs = Cat ** {"
      hPutStrLn habs "\n-- extracted from http://archives.nd.edu/whitaker/dictpage.htm"
      hPutStrLn habs "fun"
      hPutStrLn hconc "concrete DictLat of DictLatAbs = CatLat ** open ParadigmsLat in {"
      hPutStrLn hconc "-- extracted from http://archives.nd.edu/whitaker/dictpage.htm"
      hPutStrLn hconc "lin"
      writeEntry habs hconc es
      
main =
  let
    p :: String -> IO (Maybe DictEntry)
    p l =
      case parse lineParser "dict" l of {
        Left err -> do
            { putStrLn $ drop 1 (showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ errorMessages err) ;
            return Nothing
            } ;
        Right e -> return $ Just e
        }
  in
    do
      ws <- fmap lines (readFile "DICTPAGE.RAW")
      es <- zip [1..] . catMaybes <$> mapM p ws
      write "DictLatAbs.gf" "DictLat.gf" es
