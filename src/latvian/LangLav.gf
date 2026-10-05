--# -path=.:../abstract:../common:../prelude

concrete LangLav of Lang =
  GrammarLav,
  LexiconLav
  ,ConstructionLav
  ,DocumentationLav --# notpresent
  ** {

flags
  startcat = Phr ;
  unlexer = text ;
  lexer = text ;

}
