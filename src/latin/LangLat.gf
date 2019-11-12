--# -path=.:../abstract:../common:../prelude

concrete LangLat of Lang = 
  GrammarLat,
  ParadigmsLat,
  LexiconLat
--  ConstructionLat
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
