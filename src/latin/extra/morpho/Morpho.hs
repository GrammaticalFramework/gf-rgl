import PGF
import System.Environment
import System.Exit
import System.IO
import Control.Monad
usage =
  do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <grammarFile> <lang> <inputFile>"
    putStrLn $ "  grammarFile: A GF pgf compiled grammar file"
    putStrLn $ "  language: One of the languages in the pgf file"
    putStrLn $ "  inputFile: A text document to be morphologically analyzed"

type MorphoAnalysis = [(String,[(String,String)])]

showMA :: MorphoAnalysis -> IO ()
showMA ((voc,ls):rest) =
  do
    putStrLn $ voc ++ "\t" ++ (show ls)
    showMA rest
showMA [] = return ()

    
analyze :: PGF -> Language -> FilePath -> IO MorphoAnalysis
analyze pgf language input =
  do
    let char = ' ':['a'..'z']++['A'..'Z']
    vocab <- withFile input ReadMode $ \handle -> do
      content <- hGetContents handle
      hPutStrLn stderr content
      return $! words $! concat $! map (\c -> if elem c char then [c] else [' ']) content
    let morphoDict = buildMorpho pgf language
--    let morphoAs = map (\v -> (v,lookupMorpho morphoDict v)) vocab
    
    return $! map (\(v,ls) -> (v, map (\(l,a) -> (show l, a)) ls )) $! map (\v -> (v,lookupMorpho morphoDict v)) vocab
    --return []
    
main =
  do
    args <- getArgs
    if length args < 3 then
        do
          usage
          exitFailure
      else
        do
         let grammarFile = head args
         let language = (head . tail) args
         
         let inFile = (head . tail . tail) args
         pgf <- (readPGF grammarFile)
         let langs = languages pgf
         if not $ elem language (map show langs) then
           do
             putStrLn "Language not in grammar file, got:\n"
             msum $ map (\s -> putStrLn $ "\t" ++ (show s) ++ "\n") langs
             exitFailure
           else
           do
             analysis <- analyze pgf (mkCId language) inFile
             --putStrLn $ show analysis
             showMA analysis
         return ()
