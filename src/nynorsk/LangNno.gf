--# -path=.:../scandinavian:../abstract:../common:../prelude:../api

concrete LangNno of Lang =
  GrammarNno,
  LexiconNno
  ,ConstructionNno
  ,DocumentationNno --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
