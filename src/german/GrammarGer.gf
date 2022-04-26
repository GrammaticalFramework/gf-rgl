--# -path=.:../abstract:../common:prelude

concrete GrammarGer of Grammar = 
  NounGer, 
  VerbGer, 
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
