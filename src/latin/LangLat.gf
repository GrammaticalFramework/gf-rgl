--# -path=.:../abstract:../common:../prelude:../api

concrete LangLat of Lang = 
  GrammarLat,
  ParadigmsLat,
  LexiconLat
  ,ConstructionLat --# notpresent
  ,DocumentationLat --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
