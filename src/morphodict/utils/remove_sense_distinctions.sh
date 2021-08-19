#!/bin/bash

USAGE="usage: ./remove_sense_distinctions.sh <concrete syntax file>"

# String manipulation
CONC=$1                      # e.g. MorphoDictFin.gf
BAK="$CONC.bak"              # e.g. MorphoDictFin.gf.bak

NAME=`echo $CONC | cut -f 1 -d '.'` # e.g. MorphoDictFin
ABS="${NAME}Abs.gf"          # e.g. MorphoDictFinAbs.gf
CONC_HEADER="$NAME.header"   # e.g. MorphoDictFin.header

find_duplicates() {
    echo "Putting (temporarily) only homonyms in $CONC"
    echo "cat $CONC_HEADER > $CONC"
    cat $CONC_HEADER > $CONC
    DUPLS=`cut -f 2 -d ' ' /tmp/$CONC  \
         | sort | uniq -c | sort -nr \
         | egrep "^ +1?[2-9][0-9]? [a-zåäö]+_" \
         | tr -d '[0-9][A-ZÅÄÖ]'`
    for d in $DUPLS
    do
        grep "lin $d" $BAK >> $CONC
    done
    echo "}" >> $CONC
}

remove_numbers() {
    echo "cp $CONC{,.bak}"
    cp $CONC{,.bak}
    echo "cat $CONC | sed -E 's/_[0-9]_/_/g' | uniq > /tmp/$CONC"
    cat $CONC | sed -E 's/_[0-9]_/_/g' | uniq > /tmp/$CONC
    echo "Done removing numbers."
}

if [[ $CONC == *"Abs.gf" ]]
  then
    echo $USAGE
  else
    remove_numbers
    find_duplicates
    echo "gf -v=0 -make $CONC"
    gf -v=0 -make $CONC
    echo "$CONC contains now only homonyms. Original file is found in $BAK."
fi
