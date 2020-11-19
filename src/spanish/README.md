# Spanish

## Language information
* English name: Spanish
* Autonym: español
* ISO code: Spa

## Authors

* Aarne Ranta

With contributions from Inger Andersson, Bruno Cuconato, Inari Listenmaa, and Therese Söderberg.

## Implementation information
The Spanish Resource Grammar is built using the Romance functor at
 [romance](../romance/), and it mostly follows Spanish spoken in Spain.

Romance instantiations differ mostly in their morphological modules
and in the constructions defined in the =Diff*= modules. Some relevant features:

* two copulas, ser and estar
* prepositions that contract with articles are *de* and *a*, called `genitive` and `dative` respectively. When forming new prepositions, e.g. *después de* use `mkPrep "después" genitive` to get correct contractions.
* no inversion in question clauses, except for `QuestIComp` and `QuestIAdv`.

### Known issues

* Verbs with dative subject don't actually have it. Example:

```
Lang> p "I like grammars" | l
I like grammars
yo gusto gramáticas
```

This is not a massive problem per se, the application grammarian just needs to know this is the case. If your application grammar has a function like the following:

```haskell
fun Like : NP -> NP -> Cl ;
```

then you just need to linearise it as follows:

```haskell
lin Like subj obj = mkCl obj like_V2 subj ;
```

(Why don't we have it? Verbs like *gustar* inflect according to the logical *object* of the sentence; this would mean we'd need 2-dimensional inflection tables for verbs, and while that's a cool thing, it's also expensive and not needed for anything else.)

* Contractions with *con* and pronouns: *con* + *mí*/*ti*/… should be *conmigo*/*contigo*/…, but the contraction is not implemented.

* Clitic pronouns and their combinations:

```haskell
-- PredVP (UsePron i_Pron) (ComplSlash (SlashVV want_VV (Slash2V3 give_V3 (UsePron he_Pron))) (UsePron it_Pron))
LangEng: i want to give him it
LangSpa: yo lo quiero dar &+ le -- should be dárselo
-- PredVP (UsePron i_Pron) (ComplVV want_VV (ComplSlash (Slash3V3 give_V3 (UsePron it_Pron)) (UsePron he_Pron)))
LangEng: i want to give him it
LangSpa: yo quiero dar &+ lo le -- should be dárselo
```

* Accents in imperatives and infinitives with clitics

```haskell
--ImpVP (ComplSlash (Slash2V3 give_V3 (UsePron i_Pron)) (UsePron it_Pron))
LangEng: give me it
LangSpa: da &+ lo me
```

Ignoring the clitic combination (should be *dámelo*), the verb form *da* should get an accent *dá* to show the correct syllable stress.

## Data

### Besch & Irreg
The [BeschSpa](BeschSpa.gf) and [IrregSpa](IrregSpa.gf) modules contain the complete inflection tables for all different
Spanish verb types. Based on the Functional Morphology [implementation](http://www.cse.chalmers.se/alumni/markus/FM/download/FM_SPA_1.1.tgz) by Inger Andersson and Therese Söderberg.


### DictSpa
This module contains nouns, adjectives and verbs taken from
 [Open Multilingual Wordnet](http://compling.hss.ntu.edu.sg/omw/) and [DictionarySpa](https://github.com/GrammaticalFramework/wide-coverage/blob/master/translator/DictionarySpa.gf) (mostly from Wiktionary).

#### Multiwords
Some thousands of multiwords are manually checked. As of February 2019, around 4000 are unchecked. Those are marked in [DictSpa](DictSpa.gf) with `-- to-check segmentation`.

#### Morphology
WIP.
* Where there was a non-1-arg paradigm in the old resources, that is retained.
* All verbs that overlap with IrregSpa use the IrregSpa definition.
* Most of the manually checked multiwords are also checked for morphology, but naturally, errors do occur.

## History and applications

Some applications include:

* [MOLTO phrasebook](http://www.molto-project.eu/sites/default/files/everyday.pdf)
* GF Mathematical Grammar Library ([paper](http://www.molto-project.eu/sites/default/files/gf-mgl.pdf), [demo](http://cloud.grammaticalframework.org/minibar/minibar.html))
* GF [Wide-Coverage translator](http://cloud.grammaticalframework.org/wc.html)

## Maintainers
Inari Listenmaa ([@inariksit](https://github.com/inariksit))  
Bruno Cuconato ([@odanoburu](https://github.com/odanoburu))
