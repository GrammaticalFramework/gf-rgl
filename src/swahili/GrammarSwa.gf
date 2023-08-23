--# -path=.:../abstract:../common:../bantu:prelude

concrete GrammarSwa of Grammar =
  NounSwa,
  VerbSwa,
  AdjectiveSwa,
  AdverbSwa,
  NumeralSwa,
  SentenceSwa,
  QuestionSwa,
  RelativeSwa,
  ConjunctionSwa,
  PhraseSwa,
  TextX,
  StructuralSwa,
  IdiomSwa,
  TenseX,
  NamesSwa
  **  {

flags startcat = Phr ;
} ;
