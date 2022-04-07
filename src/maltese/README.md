# Maltese

## Language info

- English name: Maltese
- Autonym: Malti
- ISO code: Mlt

## Authors

Mainly written by John J. Camilleri, 2009-2013.
With some contributions by Angelo Zammit, 2012.

## Publications

- Digitizing the grammar and vocabulary of Maltese, John J. Camilleri. In Shifts and Patterns in Maltese. Vol. 19, Studia Typologica. ISBN: 978-3-11-049637-6. 2016.
- A Computational Grammar and Lexicon for Maltese, John J. Camilleri. Chalmers University of Technology. Gothenburg, Sweden. M.Sc. Thesis. 2013.
- Extending the GF Framework towards Coverage of the Maltese Language, Angelo Zammit. University of Malta. Msida, Malta. B.Sc. Dissertation. 2012.

## Implementation information

### Verb morphology

Initial implementation included all enclitic pronouns within verb inflection table, some ~1000 forms. This became uncompilable.
Re-implemetation stores 3 verb stems which are then combined with pronouns using bind.
Choosing the right stem in each case is tricky, and it's unconfirmed whether 3 stems is enough to cover all cases.

## Other resources

Extra test files can be found in the repository:
https://github.com/johnjcamilleri/Maltese-GF-Resource-Grammar

Semi-complete verb inflections for testing purposes:
https://github.com/johnjcamilleri/maltese-verb-inflections

Ġabra fullform lexicon for Maltese, partly bootstrapped by this grammar:
http://mlrs.research.um.edu.mt/resources/gabra/
