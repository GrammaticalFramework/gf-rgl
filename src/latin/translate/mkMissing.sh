#!/bin/sh
echo "Create tmp dir"
mkdir tmp/
echo "Remove old file"
echo "resource MissingTranslateLat = {} " > ../MissingTranslateLat.gf
echo "Look for missing functions"
# gf -src -i .. -batch TryLat.gf 2>&1 | grep "Warning: no linearization of" | sort -u > tmp/MissingLat.tmp
gf -src -i .. -batch TranslateLat.gf 2>&1 | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g" | grep -E "constant not found|no linearization of" | sort -u > tmp/MissingLat.tmp
echo "Compile grammar"
gf -src -make -i .. -D tmp Translate.gf &> /dev/null 
echo "Create placeholders for missing functions"
echo "resource MissingTranslateLat = open GrammarLat, Prelude in {" > ../MissingTranslateLat.gf
echo "" >> ../MissingTranslateLat.gf
echo "-- temporary definitions to enable the compilation of RGL Translate" >> ../MissingTranslateLat.gf
runghc ../api/mkMissing.hs >> ../MissingTranslateLat.gf
echo "}" >> ../MissingTranslateLat.gf
echo "Cleanup"
rm -Rf tmp
