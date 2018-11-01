@echo off
Setlocal EnableDelayedExpansion

REM ---
REM Non-Haskell RGL build script for Windows machines
REM ---

REM Modules to compile for each language
REM TODO read from languages.csv
set langs=Afr Amh Ara Eus Bul Cat Chi Dan Dut Eng Est Fin Fre Grc Gre Heb Hin Ger Ice Ina Ita Jpn Lat Lav Mlt Mon Nep Nor Nno Pes Pol Por Pnb Ron Rus Snd Spa Swe Tha Tur Urd
set modules_langs=All Symbol Compatibility
set modules_api=Try Symbolic

REM Defaults (may be overridden by options)
set gf=gf
set dest=
set verbose=false

REM Check command line options
:Loop
if "%1"=="" goto Continue
  if %1==-v set verbose=true
  if %1==--verbose set verbose=true
  if %1==--gf set gf=%2
  if %1==--dest set dest=%2
shift
goto Loop
:Continue

REM Try to determine install location
if "%dest%"=="" (
  set dest=%GF_LIB_PATH%
)
if "%dest%"=="" (
  REM TODO Look in ../gf-core/DATA=DIR
)
if "%dest%"=="" (
  echo Unable to determine where to install the RGL. Please do one of the following:
  echo  - Pass the --dest=... flag to this script
  echo  - Set the GF_LIB_PATH environment variable
  REM echo  - Compile & install GF from the gf-core repository (must be in same directory as gf-rgl)
  exit /b
)

REM A few more definitions before we get started
set src=src
set dist=dist
set gfc=%gf% --batch --gf-lib-path=%src% --quiet

REM Redirect stderr if not verbose
if %verbose%==false (
  set gfc=!gfc! 2>NUL
)

REM Make directories if not present
if not exist %dist%\prelude mkdir %dist%\prelude
if not exist %dist%\present mkdir %dist%\present
if not exist %dist%\alltenses mkdir %dist%\alltenses

REM Build: prelude
echo Building [prelude]
for /r %src%\prelude %%m in (*.gf) do (
  %gfc% --gfo-dir=%dist%\prelude %%m
)

REM Gather all language modules for building
set modules=
for %%l in (%langs%) do (
  for %%m in (%modules_langs%) do (
    for /r %src% %%m in (*%%m%%l.gf) do (
      set modules=!modules! %%m
    )
  )
  for %%m in (%modules_api%) do (
    for /r %src%\api %%m in (*%%m%%l.gf) do (
      set modules=!modules! %%m
    )
  )
)

REM Build: present
echo Building [present]
for %%m in (%modules%) do (
  %gfc% --no-pmcfg --gfo-dir=%dist%\present --preproc=mkPresent %%m
)

REM Build: alltenses
echo Building [alltenses]
for %%m in (%modules%) do (
  %gfc% --no-pmcfg --gfo-dir=%dist%\alltenses %%m
)

REM Copy
echo Copying to %dest%
copy %dist% %dest%
