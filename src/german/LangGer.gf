--# -path=.:../abstract:../common:../api:../prelude

concrete LangGer of Lang =
  GrammarGer,
  LexiconGer
  ,ConstructionGer
  ,DocumentationGer --# notpresent
  ,MarkupGer - [stringMark]
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
