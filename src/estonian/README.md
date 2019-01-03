# Estonian

## Language info

* English name: Estonian
* Autonym: eesti keel
* ISO code: Est

## Authors

* Kaarel Kaljurand, 2011-
* Inari Listenmaa, 2011-

## Publications

* Listenmaa, Inari and Kaljurand, Kaarel (2014) **Computational Estonian Grammar in Grammatical Framework**. Proceedings of the SALTMIL Workshop at LREC2014. [Link](http://ixa2.si.ehu.es/~jipsagak/SALTMIL/LREC_2014_Workshop_Proceedings_Saltmil.pdf)

## Implementation information

### Sources

The noun morphology is based on [Eesti käänamissüsteemi seaduspärasused](http://kjk.eki.ee/ee/issues/2012/6/156) (Kaalep, 2012).

We  have  automatically  constructed  a  80k-word  lexicon from all the nouns, adjectives and adverbs in EstWN (v67); the verbs of the EstCG lexicon, which provides information on the verb complement and adjunct cases; and the database of multi-word verbs (Kaalep and Muischnek, 2008).  Morfessor 2.0 was used for compound word splitting of nouns, and Filosoft’s morphology tools were used to generate the base  forms  of  nouns  and  adjectives  (6  forms),  and  verbs (8 forms).

### Testing

Morphology has been tested against Filosoft's morphological analyser.

Additionally, we have ported some of the existing GF ap-
plications (MOLTO Phrasebook (Ranta et al., 2012), ACE-
in-GF (Camilleri et al., 2012)) to Estonian.

## Maintainers

* Kaarel Kaljurand (@kaljurand)
* Inari Listenmaa (@inariksit)
