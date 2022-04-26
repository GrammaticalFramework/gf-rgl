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
* Adjectives don't have vowel harmony, so `AdjAsCN` has by default back harmony.
* Conjunction of CNs doesn't repeat possessive suffix: you get _minun [[kissa] ja [koira]]ni_ instead of _minun kissani ja koirani_ 'my cat and dog'.
  * If you want it repeated correctly, use ConjNP. Out of the box it repeats "minun", so _minun kissani ja minun koirani_ 'my cat and my dog'.
  * If you want _kissani ja koirani_, use [ProDropPoss](https://github.com/GrammaticalFramework/gf-rgl/blob/master/src/finnish/ExtraFin.gf#L190-L202) for both cat and dog.
    *  `ConjNP and_Conj (BaseNP (DetCN (DetQuant (ProDropPoss i_Pron) NumSg) (UseN cat_N)) (DetCN (DetQuant (ProDropPoss i_Pron) NumSg) (UseN dog_N)))`
  * If you want _minun kissani ja koirani_, use ProDropPoss for only `UseN dog_N`:
    * `ConjNP and_Conj (BaseNP (DetCN (DetQuant (PossPron i_Pron) NumSg) (UseN cat_N)) (DetCN (DetQuant (ProDropPoss i_Pron) NumSg) (UseN dog_N)))`


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



