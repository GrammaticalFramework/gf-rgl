module Main where

import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
import System.Process
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import System.Exit
import Text.Printf

-- To clone a project from one language to another:
--   Clone fromdir todir fromlang tolang
--
-- 1. for each Module in 'fromdir', copy Module(fromlang) to todir/Module(tolang) ; create todir if it doesn't exist
-- 2. in each Module(tolang), replace substrings fromlang by tolang, if proper suffixes of identifiers
-- 3. repeat the above for api/Module(fromlang) to api/Module(tolang)
-- 4. add the language to config file if not present
-- - If the option --comment-body is present, comment out every line in the body
-- - If the option --comment-body is present, comment out every line in the body
--
-- Example: runghc Clone swedish danish Swe Dan

main :: IO ()
main = do
  args <- getArgs
  if length args < 4
    then putStrLn "usage: Clone  (--comment-body|--drop-comments) fromdir todir fromlang tolang"
    else do
      let (options,[fromdir,todir,fromlang,tolang]) = span ((=='-') . head) args
      allfromfiles   <- getDirectoryContents fromdir
      let fromfiles = filter (\f -> isSuffixOf (fromlang ++ ".gf") f || isSuffixOf (fromlang ++ "Abs.gf") f) allfromfiles
      let modules = map (getAbstractName fromlang) fromfiles
      createDirectoryIfMissing True todir
      mapM_ (clone options fromdir todir fromlang tolang) modules

      mapM_ (\md -> clone options "api" "api" fromlang tolang (md,"")) apiModules
      conf <- readFile configFile
      if not (any (isPrefixOf tolang) (lines conf))
      then do
        appendFile configFile (printf "%s,%s\n" tolang todir)
        printf "Language '%s' has been added to %s\n" tolang configFile
      else return ()

configFile :: FilePath
configFile = ".." </> "languages.csv"

apiModules :: [String]
apiModules = ["Try","Symbolic","Syntax","Constructors","Combinators"]

clone :: [String] -> String -> String -> String -> String -> (String, String) -> IO ()
clone options fromdir todir from to (absname,absfx) = do
  s <- readFile (fromdir ++ "/" ++ absname ++ from ++ absfx ++ ".gf")
  writeAndReportFile (todir ++ "/" ++ absname ++ to ++ absfx ++ ".gf") (commentIf options (replaceLang from to s))

getAbstractName :: String -> String -> (String, String)
getAbstractName from file
  | isSuffixOf (from ++ "Abs.gf") file = (take (length file - (length from + 6)) file, "Abs") -- (NewDict, Abs)
  | isSuffixOf (from ++ ".gf") file = (take (length file - (length from + 3)) file, "") -- (NewDict, [])
  | otherwise = error ("Need suffix " ++ (from ++ ".gf") ++ " or " ++ (from ++ "Abs.gf") ++ " therefore cannot clone file name " ++ file)

replaceLang :: String -> String -> String -> String
replaceLang s1 s2 = repl where
  repl s = case s of
    c:cs -> case splitAt lgs s of
      (pre,'A':'b':'s':c:rest) | pre == s1 && elem c " \n\t,:=(){}.-[];" -> s2 ++ "Abs" ++ [c] ++ repl rest
      (pre,c:rest) | pre == s1 && elem c " \n\t,:=(){}.-[];" -> s2 ++ [c] ++ repl rest
      _                      -> c : repl cs
    _ -> s
  lgs = length s1

commentIf :: [String] -> String -> String
commentIf options =
  let commentbody  = if (elem "--comment-body" options) then commentBody else id
      dropcomments = if (elem "--drop-comments" options) then dropComments else id
  in
  unlines . commentbody . dropcomments . lines

commentBody :: [String] -> [String]
commentBody ss = header ++ map comment body ++ ["}"] where
  (header,body) = break (isJment . words) ss
  isJment ws = case ws of
    k:_ | elem k ["flags","lin","lincat","oper","param"] -> True
    _ -> False
  comment l = case l of
    _ | take 2 l == "--" -> l                -- already commented
    _ | all isSpace l    -> l                -- empty line
    _ -> "--" ++ l

dropComments :: [String] -> [String]
dropComments = filter (not . isComment) where
  isComment line = case dropWhile isSpace line of
    '-':'-':'#':_ -> False
    '-':'-':_     -> True
    _             -> False

writeAndReportFile :: FilePath -> String -> IO ()
writeAndReportFile file s = do
  writeFile file s
  putStrLn $ "wrote " ++ file
