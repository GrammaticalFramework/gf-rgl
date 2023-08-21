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
  StructuralGer - [part_Prep,possess_Prep], -- use PartNP, PossNP instead
  TenseGer,
  NamesGer
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
