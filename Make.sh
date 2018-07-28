#!/bin/sh

# Non-Haskell RGL build script for Unix-based machines

modules_langs="All Symbol Compatibility"
modules_api="Try Symbolic"

# Defaults (may be overridden by options)
gf="gf"
dest=""

# Check command line options
for arg in "$@"; do
  case $arg in
    --gf=*)
      gf="${arg#*=}"; shift ;;
    --dest=*)
      dest="${arg#*=}"; shift ;;
    *) echo "Unknown option: ${arg}" ; exit 1 ;;
  esac
done

# Try to determine install location
if [ -z "$dest" ]; then
  dest="$GF_LIB_PATH"
fi
if [ -z "$dest" ] && [ -f "../gf-core/DATA_DIR" ]; then
  dest=`cat ../gf-core/DATA_DIR`
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
gfc="${gf} --batch --gf-lib-path=${src} --quiet "

# Make directories if not present
mkdir -p "${dist}/prelude"
mkdir -p "${dist}/present"
mkdir -p "${dist}/alltenses"

# Build: prelude
echo "Building prelude"
${gfc} --gfo-dir="${dist}"/prelude "${src}"/prelude/*.gf

# Gather all language modules for building
for mod in $modules_langs; do
  for file in "${src}"/*/${mod}???.gf; do
    [[ ! -e $file ]] && continue
    modules="${modules} ${file}"
  done
done
for mod in $modules_api; do
  for file in "${src}"/api/${mod}???.gf; do
    [[ ! -e $file ]] && continue
    modules="${modules} ${file}"
  done
done

# Build: present
echo "Building present"
# ${gfc} -no-pmcfg --gfo-dir="${dist}"/present -preproc=mkPresent "${modules}"
for module in $modules; do
  ${gfc} --no-pmcfg --gfo-dir="${dist}"/present -preproc=mkPresent "${module}"
done

# Build: alltenses
echo "Building alltenses"
# ${gfc} -no-pmcfg --gfo-dir="${dist}"/alltenses "${modules}"
for module in $modules; do
  ${gfc} --no-pmcfg --gfo-dir="${dist}"/alltenses "${module}"
done

# Copy
echo "Copying to ${dest}"
cp -R ${dist}/* ${dest}
