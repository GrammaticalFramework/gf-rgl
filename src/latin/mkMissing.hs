import PGF
import System.Environment
import Data.List
import Data.Maybe
main =
  do
    args <- getArgs -- first one should be pgf file and second one should be the file containing the errors/warnings about missing things
    pgf <- PGF.readPGF (args !! 0) -- "tmp/Lang.pgf"
    ms <- readFile (args !! 1) {- "tmp/MissingLat.tmp" -} >>= return . nub . sort . map (last . words) . lines
    let ts = [maybe ("-- Error: No type found for " ++ m) (\t -> "oper " ++ m ++ " : " ++ PGF.showType [] t ++ " = notYet \"" ++ m ++ "\" ;") $ PGF.functionType pgf (PGF.mkCId m) | m <- ms ]
    putStrLn $ unlines ts -- ["oper " ++ f ++ " : " ++ t ++ " = notYet \"" ++ f ++ "\" ;" | (f,t) <- zip ms ts]
