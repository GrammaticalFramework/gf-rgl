--# -path=.:../abstract:../common:../prelude

concrete LangLat of Lang = 
  GrammarLat,
  ParadigmsLat,
  LexiconLat,
  RelativeLat
--  ConstructionLat
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
