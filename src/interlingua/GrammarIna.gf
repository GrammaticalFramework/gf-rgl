--# -path=.:../abstract:../common:prelude

concrete GrammarIna of Grammar = 
  NounIna, 
  VerbIna, 
  AdjectiveIna,
  AdverbIna,
  NumeralIna,
  SentenceIna,
  QuestionIna,
  RelativeIna,
  ConjunctionIna,
  PhraseIna,
  TextX,
  StructuralIna,
  IdiomIna,
  TenseX,
  NamesIna
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
