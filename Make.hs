{-# LANGUAGE CPP #-}

-- | Main build script for RGL

import Data.List (find,isPrefixOf,isSuffixOf,(\\),unfoldr)
import Data.Maybe (catMaybes)
import System.IO (hPutStrLn,stderr)
import System.IO.Error (catchIOError)
import System.Exit (ExitCode(..),exitFailure)
import System.Environment (getArgs,lookupEnv)
import System.Process (rawSystem)
import System.FilePath ((</>)) -- ,takeFileName,addExtension,dropExtension)
import System.Directory (createDirectoryIfMissing,copyFile,getDirectoryContents,removeDirectoryRecursive,findFile)
#if __GLASGOW_HASKELL__>=800
import System.Directory (getModificationTime,setModificationTime)
#endif
import Control.Monad (when,unless)

main :: IO ()
main = do
  aargs <- getArgs
  case aargs of
    [] -> putStrLn $ "Must specify command, one of: " ++ unwords commands
    a:_ | a `notElem` commands -> putStrLn $ "Unknown command: " ++ a ++ ". Valid commands: " ++ unwords commands
    "build":args -> buildRGL args
    "copy":args -> copyRGL args
    "install":args -> buildRGL args >> copyRGL args
    "clean":_ -> clean
  where
    commands = ["build","copy","install","clean"]

-- | Build grammars into dist
buildRGL :: [String] -> IO ()
buildRGL args = do
  checkArgs args
  let cmds = getCommands args
  let modes = getOptMode args
  info <- mkInfo
  mapM_ (\cmd -> cmdAction cmd modes args info) cmds

-- | Copy everything from dist to install location
copyRGL :: [String] -> IO ()
copyRGL args = do
  let modes = getOptMode args
  info <- mkInfo
  gf_lib_dir <- maybe (die errLocation) return (infoInstallDir info)
  -- let files = getOptModules args
  -- if not (null files)
  -- then do
  --   -- Copy single files
  --   sequence_ [copyOne (flip addExtension "gfo" . dropExtension . takeFileName $ file) (getRGLBuildDir info mode) (gf_lib_dir </> getRGLBuildSubDir mode)|file<-files, mode<-modes]
  -- else do
    -- Copy everything
  copyAll "Prelude" (infoBuildDir info </> "prelude") (gf_lib_dir </> "prelude")
  sequence_ [copyAll (show mode) (getRGLBuildDir info mode) (gf_lib_dir </> getRGLBuildSubDir mode)|mode<-modes]

-- | Error message when install location cannot be determined
errLocation :: String
errLocation = unlines $
  [ "Unable to determine where to install the RGL. Please do one of the following:"
  , " - Pass the " ++ destination_flag ++ "... flag to this script"
  , " - Set the GF_LIB_PATH environment variable"
  , " - Compile & install GF from the gf-core repository (must be in same directory as gf-rgl)"
  ]

-- | Copy single file between directories
copyOne :: String -> FilePath -> FilePath -> IO ()
copyOne file from to = do
  putStrLn $ "Copying [" ++ file ++ "] " ++ to
  createDirectoryIfMissing True to
  copyFileWithModificationTime (from </> file) (to </> file)

-- | Copy all files between directories
copyAll :: String -> FilePath -> FilePath -> IO ()
copyAll msg from to = do
  putStrLn $ "Copying [" ++ msg ++ "] " ++ to
  createDirectoryIfMissing True to
  mapM_ (\file -> when (file /= "." && file /= "..") $ copyFileWithModificationTime (from </> file) (to </> file)) =<< getDirectoryContents from

-- | Copy a file together with its modification time but no other meta data
copyFileWithModificationTime :: FilePath -> FilePath -> IO ()
copyFileWithModificationTime source destination = do
  copyFile source destination
#if __GLASGOW_HASKELL__>=800
  getModificationTime source >>= setModificationTime destination
#endif

-- | Remove dist directory
clean :: IO ()
clean = do
  info <- mkInfo
  removeDirectoryRecursive (infoBuildDir info)

-------------------------------------------------------------------------------
-- Paths and directories

-- | RGL source directory
sourceDir :: FilePath
sourceDir = "src"

-- | Drop source directory prefix
-- TODO use functions from System.FilePath
dropSourceDir :: FilePath -> FilePath
dropSourceDir = drop (length sourceDir + 1)

-- | Information needed in build
data Info = Info
  { infoBuildDir :: FilePath -- ^ where to put built RGL modules (fixed)
  , infoInstallDir :: Maybe FilePath -- ^ install directory (found dynamically)
  , infoGFPath :: FilePath -- ^ path to GF
  , infoVerbose :: Bool
  } deriving (Show)

-- | Build info object from command line args
mkInfo :: IO Info
mkInfo = do
  args <- getArgs
  -- Look for install location in a few different places
  let mflag = getFlag destination_flag args
  mbuilt <- catchIOError (readFile "../gf-core/DATA_DIR" >>= \d -> return (Just (d </> "lib"))) (\e -> return Nothing)
  menvar <- lookupEnv "GF_LIB_PATH"
  let
    inst_dir =
      case catMaybes [mflag,menvar,mbuilt] of
        [] -> Nothing
        p:_ -> Just p
  let verbose = verbose_switch `elem` args || verbose_switch_short `elem` args
  return $ Info
    { infoBuildDir = "dist"
    , infoInstallDir = inst_dir
    , infoGFPath = maybe default_gf id (getFlag gf_flag args)
    , infoVerbose = verbose
    }
  where
    default_gf = "gf"

getRGLBuildDir :: Info -> Mode -> FilePath
getRGLBuildDir info mode = infoBuildDir info </> getRGLBuildSubDir mode

getRGLBuildSubDir :: Mode -> String
getRGLBuildSubDir mode =
  case mode of
    Present   -> "present"
    AllTenses -> "alltenses"

-------------------------------------------------------------------------------
-- Build modes

data Mode = Present | AllTenses
  deriving (Show,Eq)

all_modes :: [String]
all_modes = ["present","alltenses"]

default_modes :: [Mode]
default_modes = [Present,AllTenses]

-- | An RGL build command
data RGLCommand = RGLCommand
  { cmdName   :: String -- ^ name of command
  , cmdIsDef  :: Bool   -- ^ is default?
  , cmdAction :: [Mode] -> [String] -> Info -> IO () -- ^ action
  }

-- | Possible build commands
rglCommands :: [RGLCommand]
rglCommands =
  [ RGLCommand "prelude" True  $ \modes args bi -> do
      let prelude_src_dir = sourceDir       </> "prelude"
          prelude_dst_dir = infoBuildDir bi </> "prelude"
      createDirectoryIfMissing True prelude_dst_dir
      files <- getDirectoryContents prelude_src_dir
      let modules = [prelude_src_dir </> file | file <- files, file /= "." && file /= ".."]
      putStrLn $ "Building [Prelude]"
      when (infoVerbose bi) $ putStrLn (unwords (map dropSourceDir modules))
      run_gfc bi (["--gfo-dir="++prelude_dst_dir] ++ modules)

  , RGLCommand "all"     True  $ gfcp [l,s,c,t,sc]
  , RGLCommand "lang"    False $ gfcp [l,s]
  , RGLCommand "api"     False $ gfcp [t,sc]
  , RGLCommand "compat"  False $ gfcp [c]

  -- Special command, invoked when command ends in .gf
  , RGLCommand "modules"  False $ \modes args bi ->  do
      let modules = getOptModules args
      flip mapM_ modules $ \m -> do
        mex <- findModule m
        case mex of
          Nothing -> die $ "Cannot find module: " ++ m
          Just mfull -> flip mapM_ modes $ \mode -> do
            let dst = getRGLBuildDir bi mode
            putStrLn $ "Building [" ++ show mode ++ "] " ++ dropSourceDir mfull
            run_gfc bi ["--gfo-dir="++dst, mfull]

  ]
  where
    gfcp :: [Mode -> [String] -> (LangInfo -> FilePath,[LangInfo] -> [LangInfo])] -> [Mode] -> [String] -> Info -> IO ()
    gfcp cs modes args bi = parallel_ [gfcp' bi mode args cs | mode <- modes]

    gfcp' :: Info -> Mode -> [String] -> [Mode -> [String] -> (LangInfo -> FilePath,[LangInfo] -> [LangInfo])] -> IO ()
    gfcp' bi mode args cs = do
      langsAll <- loadLangs
      -- f :: LangInfo -> FilePath
      -- as :: [LangInfo] -> [LangInfo]
      -- ss :: [String]
      -- fss :: [[FilePath]]
      let (ss,fss) = unzip [ (summary f langs,map f langs) | c<-cs, let (f,as) = c mode args, let langs = as langsAll]
      gfcn bi mode (unwords (filter (not.null) ss)) (concat fss)

    summary :: (LangInfo -> FilePath) -> [LangInfo] -> String
    summary f langs = unwords (map (dropSourceDir . f) langs)
    -- summary f _ = f (LangInfo "*" "*" Nothing Nothing False False False False)

    l mode args = (lang,optml mode langAll args)
    s mode args = (symbol,optml mode langTry args)
    c mode args = (compat,optml AllTenses langCompatibility args)
    t mode args = (try,optml mode langTry args)
    sc mode args = (symbolic,optml mode langSymbolic args)

    optml :: Mode -> (LangInfo -> Bool) -> [String] -> ([LangInfo] -> [LangInfo])
    optml mode pred args =
      \langsAll ->
        let langsDefault = filter (if mode == Present then langPresent else const True) (filter pred langsAll)
        in  getOptLangs langsAll langsDefault args

-------------------------------------------------------------------------------
-- Getting module paths/names

-- | Search all language dirs for module name
findModule :: String -> IO (Maybe FilePath)
findModule file = do
  langs <- loadLangs
  let langdirs = map langDir langs
  let searchdirs = map ((</>) sourceDir) langdirs
  findFile searchdirs file

lang :: LangInfo -> FilePath
lang l = sourceDir </> langDir l </> ("All" ++ langCode l ++ ".gf")

compat :: LangInfo -> FilePath
compat l = sourceDir </> langDir l </> ("Compatibility" ++ langCode l ++ ".gf")

symbol :: LangInfo -> FilePath
symbol l = sourceDir </> langDir l </> ("Symbol" ++ langCode l ++ ".gf")

try :: LangInfo -> FilePath
try l = sourceDir </> "api" </> ("Try" ++ langCode l ++ ".gf")

syntax :: LangInfo -> FilePath
syntax l = sourceDir </> "api" </> ("Syntax" ++ langCode l ++ ".gf")

symbolic :: LangInfo -> FilePath
symbolic l = sourceDir </> "api" </> ("Symbolic" ++ langCode l ++ ".gf")

-------------------------------------------------------------------------------
-- Argument helpers

-- | Check arguments are valid, failing on error
checkArgs :: [String] -> IO ()
checkArgs args = do
  let args'' = args \\ (getOptModules args)
  let args' = flip filter args'' (\arg -> not
        (  arg `elem` (map cmdName rglCommands)
        || arg `elem` all_modes
        || lang_flag `isPrefixOf` arg
        || gf_flag `isPrefixOf` arg
        || destination_flag `isPrefixOf` arg
        || arg `elem` [verbose_switch, verbose_switch_short]
        ))
  unless (null args') $ die $ "Unrecognised argument: " ++ unwords args'
  return ()

-- | Get commands from args
getCommands :: [String] -> [RGLCommand]
getCommands args =
  let
    cmdModules = head $ filter (\cmd -> cmdName cmd == "modules") rglCommands
    cmds0 =
      [ cmd
      | arg <- args
      , cmd <- rglCommands
      , cmdName cmd == arg
      , cmdName cmd `notElem` all_modes
      ] ++ (if not (null (getOptModules args)) then [cmdModules] else [])
  in if null cmds0
       then [cmd | cmd <- rglCommands, cmdIsDef cmd]
       else cmds0

-- | Get mode from args (may be missing)
getOptMode :: [String] -> [Mode]
getOptMode args =
    if null explicit_modes
    then default_modes
    else explicit_modes
  where
    explicit_modes =
      [Present|have "present"]++
      [AllTenses|have "alltenses"]
    have mode = mode `elem` args

-- | List of languages overriding the default definitions
getOptLangs :: [LangInfo] -> [LangInfo] -> [String] -> [LangInfo]
getOptLangs allLangs defaultLangs args =
  let x = [ ls
          | arg <- args
          , let (f,ls) = splitAt (length lang_flag) arg
          , f == lang_flag
          ]
  in case x of
    -- ('+':ls):_ -> foldr addLang defaultLangs (seps ls)
    ('-':ls):_ -> foldr removeLang defaultLangs (seps ls)
    ls:_ -> findLangs defaultLangs (seps ls)
    _    -> defaultLangs
  where
    seps = words . map (\c -> if c==',' then ' ' else c)
    findLangs langs ls = [lang | lang <- langs, langCode lang `elem` ls]
    removeLang l ls = [lang | lang <- ls, langCode lang /= l]
    -- addLang l ls = if null (findLangs ls [l])
    --                then findLangs allLangs [l]++ls
    --                else ls

-- | Get module names from arguments
getOptModules :: [String] -> [FilePath]
getOptModules = filter (isSuffixOf ".gf")

-- | Flag for specifying languages
-- '=' can optionally be followed by '+' or '-' to alter the default languages
lang_flag :: String
lang_flag = "--langs="

-- | Flag for specifying gf location
gf_flag :: String
gf_flag = "--gf="

-- | Flag for specifying RGL install location
destination_flag :: String
destination_flag = "--dest="

-- | Switch for making verbose
verbose_switch :: String
verbose_switch = "--verbose"

verbose_switch_short :: String
verbose_switch_short = "-v"

-- | Get flag value from list of args
getFlag :: String -> [String] -> Maybe String
getFlag flag args = fmap (drop (length flag)) $ find (isPrefixOf flag) args

-------------------------------------------------------------------------------
-- Languages of the RGL

-- | Path to language config file
configFile :: FilePath
configFile = "languages.csv"

-- | Information about a language
data LangInfo = LangInfo
  { langCode :: String -- ^ 3-letter ISO 639-2/B code
  , langDir :: String -- ^ directory name
  , langFunctor :: Maybe String -- ^ functor (not used)
  , langUnlexer :: Maybe String -- ^ decoding for postprocessing linearizations
  , langPresent :: Bool
  , langAll :: Bool
  , langTry :: Bool
  , langSymbolic :: Bool
  , langCompatibility :: Bool
  } deriving (Show,Eq)

-- | Load language information from config file
loadLangs :: IO [LangInfo]
loadLangs = do
  lns <- readFile configFile >>= return . lines
  mapM mkLangInfo (tail lns)
  where
    maybeBit bits n = if length bits >= (n+1) && length (bits !! n) > 0 then Just (bits !! n) else Nothing
    boolBit bits n def = if length bits >= (n+1) && length (bits !! n) > 0 then (if def then bits !! n /= "n" else bits !! n == "y") else def
    mkLangInfo s =
      let bits = separateBy ',' s in
      if length bits < 2
      then die $ "Invalid entry in " ++ configFile ++ ": " ++ s
      else return $ LangInfo
            { langCode = bits !! 0
            , langDir = bits !! 1
            , langFunctor = maybeBit bits 2
            , langUnlexer = maybeBit bits 3
            , langPresent = boolBit bits 4 False
            , langAll = boolBit bits 5 True
            , langTry = boolBit bits 6 True
            , langSymbolic = boolBit bits 7 True
            , langCompatibility = boolBit bits 8 False
            }

-- | Separate a string on a character
-- Source: https://stackoverflow.com/a/4978733/98600
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

-------------------------------------------------------------------------------
-- Executing GF

gfc :: Info -> [Mode] -> String -> [FilePath] -> IO ()
gfc bi modes summary files =
  parallel_ [gfcn bi mode summary files | mode<-modes]

gfcn :: Info -> Mode -> String -> [FilePath] -> IO ()
gfcn bi mode summary files = do
  let dir = getRGLBuildDir bi mode
      preproc = case mode of
                  Present   -> "--preproc=mkPresent"
                  AllTenses -> ""
  createDirectoryIfMissing True dir
  if length files > 0
  then do
    putStrLn $ "Building [" ++ show mode ++ "]"
    when (infoVerbose bi) (putStrLn summary)
    run_gfc bi (["--no-pmcfg", preproc, "--gfo-dir="++dir] ++ files)
  else
    putStrLn $ "Skipping [" ++ show mode ++ "] (nothing to build)"

-- | Runs the gf executable in compile mode with the given arguments
run_gfc :: Info -> [String] -> IO ()
run_gfc bi args = do
  let
    dir = infoBuildDir bi
    args' = ["--batch", "--quiet", "--gf-lib-path="++dir] ++ filter (not . null) args
    gf = infoGFPath bi
  execute gf args'

-- | Run an arbitrary system command
execute :: String -> [String] -> IO ()
execute command args = do
  let cmdline = command ++ " " ++ unwords (map showArg args)
  e <- rawSystem command args
  case e of
    ExitSuccess   -> return ()
    ExitFailure i -> do
      putStrLn $ "Ran: " ++ cmdline
      die $ command ++ " exited with exit code: " ++ show i
  where
    showArg arg = if ' ' `elem` arg then "'" ++ arg ++ "'" else arg

-- | For parallel RGL module compilation
-- Unfortunately, this has no effect unless compiled with -threaded
parallel_ :: (Foldable t, Monad m) => t (m a) -> m ()
parallel_ ms = sequence_ ms
  -- do c <- newChan
  --    ts <- sequence [ forkIO (m >> writeChan c ()) | m <- ms]
  --    sequence_ [readChan c | _ <- ts]

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

die :: String -> IO a
die s = do
  hPutStrLn stderr s
  exitFailure
