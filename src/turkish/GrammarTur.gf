--# -path=.:../abstract:../common:prelude

concrete GrammarTur of Grammar =
  NounTur,
  VerbTur,
  AdjectiveTur,
  AdverbTur,
  NumeralTur,
  SentenceTur,
  QuestionTur,
  RelativeTur,
  ConjunctionTur,
  TextX - [CAdv, AdN],
  StructuralTur,
  PhraseTur,
  IdiomTur,
  TenseX - [CAdv, AdN],
  NamesTur
  ** {

  flags startcat = Phr ;

} ;
