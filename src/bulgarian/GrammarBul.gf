--# -path=.:../abstract:../common:prelude
--# -coding=utf8

concrete GrammarBul of Grammar = 
  NounBul,
  VerbBul,
  AdjectiveBul,
  AdverbBul,
  NumeralBul,
  SentenceBul,
  QuestionBul,
  RelativeBul,
  ConjunctionBul,
  PhraseBul,
  TextBul,
  StructuralBul,
  IdiomBul,
  TenseX - [CAdv,IAdv,AdV,SC]
  ** {
  flags coding=utf8 ;


flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
