#!/bin/bash

# ---
# Non-Haskell RGL build script for Unix-based machines
# ---
set -e

# Get languages from config
langs=$(tail -n +2 languages.csv | awk -F ','  '{ if ($7 != "n") { print $1 } }')
langs_present=$(tail -n +2 languages.csv | awk -F ','  '{ if ($6 == "y") { print $1 } }')
langs_try=$(tail -n +2 languages.csv | awk -F ','  '{ if ($8 != "n") { print $1 } }')
langs_symbolic=$(tail -n +2 languages.csv | awk -F ','  '{ if ($9 != "n") { print $1 } }')
langs_compat=$(tail -n +2 languages.csv | awk -F ','  '{ if ($10 == "y") { print $1 } }')

# Modules to compile for each language
modules_langs="All Symbol Compatibility"
modules_api="Try Symbolic"

# Defaults (may be overridden by options)
gf="gf"
dest=""
verbose="false"

# Check command line options
for arg in "$@"; do
  case $arg in
    --gf=*)
      gf="${arg#*=}"; shift ;;
    --dest=*)
      dest="${arg#*=}"; shift ;;
    --verbose|-v)
      verbose="true"; shift ;;
    *) echo "Unknown option: ${arg}" ; exit 1 ;;
  esac
done

# Try to determine install location
if [ -z "$dest" ]; then
  dest="$GF_LIB_PATH"
fi
if [ -z "$dest" ] && [ -f "../gf-core/DATA_DIR" ]; then
  dest=$(cat ../gf-core/DATA_DIR)
  if [ -n "$dest" ]; then dest="${dest}/lib"; fi
fi
if [ -z "$dest" ]; then
  echo "Unable to determine where to install the RGL. Please do one of the following:"
  echo " - Pass the --dest=... flag to this script"
  echo " - Set the GF_LIB_PATH environment variable"
  echo " - Compile & install GF from the gf-core repository (must be in same directory as gf-rgl)"
  exit 1
fi

# A few more definitions before we get started
src="src"
dist="dist"
gfc="${gf} --batch --quiet --gf-lib-path=${dist}"

# Make directories if not present
mkdir -p "${dist}/prelude"
mkdir -p "${dist}/present"
mkdir -p "${dist}/alltenses"

# Build: prelude
echo "Building [prelude]"
if [ $verbose = true ]; then echo "${src}"/prelude/*.gf; fi
${gfc} --gfo-dir="${dist}"/prelude "${src}"/prelude/*.gf

# Gather all language modules for building
modules_present=
modules_alltenses=
for lang in $langs; do
  for mod in $modules_langs $modules_api; do
    if [ $mod == "Compatibility" ] && [[ "$langs_compat" != *"$lang"* ]]; then continue; fi
    if [ $mod == "Try" ] && [[ "$langs_try" != *"$lang"* ]]; then continue; fi
    if [ $mod == "Symbolic" ] && [[ "$langs_symbolic" != *"$lang"* ]]; then continue; fi
    for file in "${src}"/*/"${mod}${lang}".gf; do
      if [ ! -f "$file" ]; then continue; fi
      if [[ "$langs_present" = *"$lang"* ]]; then modules_present="${modules_present} ${file}"; fi
      modules_alltenses="${modules_alltenses} ${file}"
    done
  done
done

# Build: present
echo "Building [present]"
if [ $verbose = true ]; then echo $modules_present; fi
for module in $modules_present; do
  ${gfc} --no-pmcfg --gfo-dir="${dist}"/present --preproc=mkPresent "${module}"
done

# Build: alltenses
echo "Building [alltenses]"
if [ $verbose = true ]; then echo $modules_alltenses; fi
for module in $modules_alltenses; do
  ${gfc} --no-pmcfg --gfo-dir="${dist}"/alltenses "${module}"
done

# Copy
echo "Copying to ${dest}"
cp -R -p "${dist}"/* "${dest}"
