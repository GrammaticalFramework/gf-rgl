# Hungarian

## Language info

* English name: Hungarian
* Autonym: Magyar
* ISO code: Hun

## Authors

Inari Listenmaa, Julia Jansson, 2020-

With contributions from Erzsébet Galgóczy (initial nominal morphology) and Patrik Jansson (numerals).

## Publications

-

## Implementation information
(2020-04) - Julia Jansson

A basic RGL exists with focus on noun inflection in NounMorphoHun.gf. First the implementation of NounMorphoHun.gf was in paradigms taking one argument (nominative) with different cases for plural, accusative and superessive cases. NounMorphoHun.gf also includes wovels, consonants and double/triple consonants, and functions using wovel harmony for the different stems. Since accusative has shown to be irregular the structure was changed to multi-argument paradigms taking nominative and accusative, from this the paradigms could be simplified much, and yield correct results to a higher degree. For cases when only nominative is entered as an argument there is a function that guesses accusative from nominative, but it is better to use several arguments.

Later, possessive stem inflection was implemented which was quite complicated. Some paradigms take possessive forms as arguments, others have rules were using the accusative was enough. However the irregularity in possessive forms showed that several possessive arguments may be necessary for yielding correct results. Special cases are noted in NounMorphoHun.gf and LexiconHun.gf. For all these to work some more work on wovel harmony and the paradigms need to be done. Furthermore, not all 21 cases are implemented in NounMorphoHun.gf; it is missing the cases Essive, Temporal, Formal and Terminative.

TLDR; a basic noun inflection structure covering most cases and possessive suffixes is implemented. There are special cases that need to be fixed, there is work left on wovel harmony and adding the missing cases.


## Maintainer

Inari Listenmaa ([@inariksit](https://github.com/inariksit))
