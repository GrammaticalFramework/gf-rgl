--# -path=.:../romance:../abstract:../common:../api

concrete LangIta of Lang = 
  GrammarIta,
  LexiconIta
  ,DocumentationIta --# notpresent
  ,ConstructionIta
  ,MarkupIta - [stringMark]
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
