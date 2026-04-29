# Latvian GF Resource Grammar

There is an ongoing work to develop a wide-coverage reimplementation of the Latvian RGL morphology, based on [Tezaurs.lv](https://tezaurs.lv) dictionary data. Currently, the reimplementation covers noun paradigms and lexical entries, which will be followed by other parts of speech, eventually substituting the legacy Latvian RGL morphology.

The reimplemented morphology and lexicon module consists of the following components:
- `PortedMorphoParadigmsLav.gf`: automatically converted paradigm desrciptions from the [Latvian Morphological Toolkit](https://github.com/LUMII-AILab/Morphology/); please, DO NOT EDIT.
- `PortedMorphoStemchangesLav.gf`: manually ported stemchange functions for the above mentioned paradigms; must be kept aligned with the [source code](https://github.com/LUMII-AILab/Morphology/blob/master/src/main/java/lv/semti/morphology/analyzer/Mijas.java) of the toolkit.
- `PortedTezaursDictLav.gf` and `PortedTezaursDictLavAbs.gf`: automatically exported lexicon data from [Tezaurs.lv](https://tezaurs.lv); please, DO NOT EDIT.
- `PortedMorphoUtilsLav.gf`: utility functions.

For edits and improvements, please, contact us at lauma@ailab.lv!


## Acknowledgements

The work on porting Tezaurs.lv inflectional paradigms to GF and creating a wide-coverage computational GF lexicon for Latvian was funded by the Latvian Council of Science under the grant agreement lzp-2022/1-0443 ([Advancing Latvian Computational Lexical Resources for Natural Language Understanding and Generation](https://wordnet.ailab.lv/project2)).
