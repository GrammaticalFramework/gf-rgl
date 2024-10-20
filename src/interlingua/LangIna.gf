--# -path=.:../abstract:../common:../prelude

concrete LangIna of Lang = 
  GrammarIna,
  LexiconIna,
  DocumentationIna
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} 

