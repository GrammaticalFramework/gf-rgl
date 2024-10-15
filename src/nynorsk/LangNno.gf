--# -path=.:../scandinavian:../abstract:../common:../prelude

concrete LangNno of Lang =
  GrammarNno,
  LexiconNno
  ,DocumentationNno --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
