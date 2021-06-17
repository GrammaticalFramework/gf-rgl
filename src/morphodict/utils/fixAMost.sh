#!/bin/bash

cp MorphoDictEng.gf{,.bak}
sed -E 's/mkA "(.*)" "DUMMY" "DUMMY" "(.*)"/mkAMost "\1" "\2"/g' < MorphoDictEng.gf.bak > MorphoDictEng.gf

# If you need to recreate the morphodict, do this.
# ResEng.gf line 162 and ParadigmsEng.gf line 514, fill the inflection table with the word DUMMY instead of nonExist.
# Then recompile RGL and MorphoDictEng.
# Then run MkMorphodict, and it will output a file with "DUMMY" in it.