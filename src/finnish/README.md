# Finnish

(Doc started by AR 2019-01-03 and is still just a stub)

## Language info

- English name: Finnish
- Autonym: Suomi
- ISO code: Fin

## Authors

Mainly written by Aarne Ranta starting in 2002
With contributions by Inari Listenmaa, ...

## Publications

- Grégoire Détrez and Aarne Ranta, Smart Paradigms, EACL 2012

## Implementation information

### Morphology

This module includes one of the first uses of smart paradigms.
But it was mainly implemented before GF supported regular expression
matching, so that the suffixing functions dp and tk are in heavy use.


## Known issues

* The object case in passives may come out wrong.
* If a possessive suffix is added for a NP that is built out of N2 or N3, the possessive suffix will go after the complements, and not after the head noun as it's supposed to.


## Supplementary files

KOTUS wordlist

stemmed/ directory to minimize the size of inflection tables


## Applications

This grammar has been thoroughly tested in applications.
Examples include
- MOLTO phrasebook
- MathBar
- GF Wide-Coverage translator
- FinWordnet lexicon

Commercial applications:
- Digital Grammars Health (Ambulance and Obstetrics)
- Tillgänglighetsdatabasen
- InLife communication support
- Textual's product information



