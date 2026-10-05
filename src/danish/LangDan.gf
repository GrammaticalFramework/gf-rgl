--# -path=.:../scandinavian:../abstract:../common:../prelude

concrete LangDan of Lang = 
  GrammarDan,
  LexiconDan
  ,ConstructionDan
  ,DocumentationDan --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
