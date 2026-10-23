--# -path=.:../scandinavian:../abstract:../common:../prelude:../api

concrete LangNor of Lang = 
  GrammarNor,
  LexiconNor
  ,ConstructionNor --# notpresent
  ,DocumentationNor --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
