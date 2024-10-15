--# -path=.:../scandinavian:../abstract:../common:../prelude

concrete LangNor of Lang = 
  GrammarNor,
  LexiconNor
  ,DocumentationNor --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
