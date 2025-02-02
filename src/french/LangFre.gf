--# -path=.:../romance:../abstract:../common:../api

concrete LangFre of Lang = 
  GrammarFre,
  LexiconFre
  ,MarkupFre - [stringMark]
  ,DocumentationFre --# notpresent
  ,ConstructionFre
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
