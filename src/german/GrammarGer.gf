--# -path=.:../abstract:../common:prelude

concrete GrammarGer of Grammar' =
  NounGer, 
  VerbGer, -- to save compile time during development HL 7/22
  AdjectiveGer,
  AdverbGer,
  NumeralGer,
  SentenceGer,
  QuestionGer,
  RelativeGer,
  ConjunctionGer,
  PhraseGer,
  TextX - [Tense,Temp],
  IdiomGer,
  StructuralGer,
  TenseGer
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
