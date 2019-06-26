import PGF
import System.Environment

main =
  do
    args <- getArgs -- first one should be pgf file and second one should be the file containing the errors/warnings about missing things
    pgf <- PGF.readPGF (args !! 0) -- "tmp/Lang.pgf"
    ms <- readFile (args !! 1) {- "tmp/MissingLat.tmp" -} >>= return . map (last . words) . lines
    let ts = [PGF.showType [] t | m <- ms, Just t <- [PGF.functionType pgf (PGF.mkCId m)]]
    putStrLn $ unlines ["oper " ++ f ++ " : " ++ t ++ " = notYet \"" ++ f ++ "\" ;" | (f,t) <- zip ms ts]
