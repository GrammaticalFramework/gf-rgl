--# -path=.:../abstract:../common:prelude

concrete GrammarRus of Grammar =
  NounRus,
  VerbRus,
  AdjectiveRus,
  AdverbRus,
  NumeralRus,
  SentenceRus,
  QuestionRus,
  RelativeRus,
  ConjunctionRus,
  PhraseRus,
  TextRus,
  StructuralRus,
  IdiomRus,
  TenseRus
  ** { flags  startcat = Phr ; unlexer = text ; lexer = text ; coding=utf8 ;} ;