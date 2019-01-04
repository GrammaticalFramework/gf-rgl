# Catalan

The Catalan Resource Grammar is built using the Romance functor at
[romance](../romance/).

## Language information
- English name: Catalan
- Autonym: català
- ISO code: Cat

## Authors

* Jordi Saludes

With contributions from Inari Listenmaa, Aarne Ranta, Francis Tyers and Daniel Vidal Hussey.

## Implementation information

### Morphology

The [BeschCat](BeschCat.gf) module contains the complete inflection tables for
Catalan verbs, divided in paradigms following
[*Els verbs conjugats*](https://www.claret.cat/ca/llibre/ELS-VERBS-CONJUGATS-849136100)
(Xuriguera, 1972).

### Known issues

Clitic pronouns do not combine like they are supposed to. For example:

```
Lang: PredVP (UsePron i_Pron) (ComplSlash (SlashVV want_VV (Slash2V3 give_V3 (UsePron he_Pron))) (UsePron it_Pron))
LangCat: jo ho vull donar li
LangEng: I want to give him it
Lang: PredVP (UsePron i_Pron) (ComplVV want_VV (ComplSlash (Slash3V3 give_V3 (UsePron it_Pron)) (UsePron he_Pron)))
LangCat: jo vull donar ho li
LangEng: I want to give him it
```

Neither of the Catalan versions are correct; it should be *(jo) vull donar-li-ho* or *(jo) li ho vull donar*.

### Supplementary files

[DictionaryCat](https://github.com/GrammaticalFramework/wide-coverage/blob/master/translator/DictionaryCat.gf)
originally derived from [Apertium](https://github.com/apertium/apertium-cat) lexicon.

## History and applications

Some applications include:

* [MOLTO phrasebook](http://www.molto-project.eu/sites/default/files/everyday.pdf)
* GF Mathematical Grammar Library ([paper](http://www.molto-project.eu/sites/default/files/gf-mgl.pdf), [demo](http://cloud.grammaticalframework.org/minibar/minibar.html))
* GF [Wide-Coverage translator](http://cloud.grammaticalframework.org/wc.html)

## Maintainer

Inari Listenmaa ([@inariksit](https://github.com/inariksit))
