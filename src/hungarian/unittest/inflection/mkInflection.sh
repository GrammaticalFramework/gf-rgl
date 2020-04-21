#!/usr/bin/env bash

sed "s/PLACEHOLDER/$1_N/g" < inflection_placeholder.gfs > inflection_concrete.gfs
gf --run < inflection_concrete.gfs > $2-new.gftest # don't override the existing ones
rm inflection_concrete.gfs
