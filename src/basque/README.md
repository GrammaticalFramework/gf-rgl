# Basque

## Language info

* English name: Basque
* Autonym: Euskara
* ISO code: Eus

## Authors

* Inari Listenmaa, 2015-
* Francis Tyers, 2015-2016

Thanks to Olatz Perez-De-Viñaspre for providing native help.

## Publications

-

## Implementation information

### Sources
The implementation follows [Standard Basque: a progressive grammar](https://mitpress.mit.edu/books/standard-basque) by Rudolf P.G. de Rijk.

[LexiconEus](LexiconEus.gf) and [Dictionary](https://github.com/GrammaticalFramework/wide-coverage/blob/master/translator/DictionaryEus.gf) have been derived from lexica
in the [Apertium](https://github.com/apertium/apertium-eus) project.

### Testing

Verb morphology has been tested very rudimentarily against Apertium morphological
lexicon, by method described [here](tests#readme). There are known errors in verb forms,
especially for transitive and ditransitive verbs. Some moods and tenses have not been
implemented yet.

## Maintainer

Inari Listenmaa (@inariksit)
