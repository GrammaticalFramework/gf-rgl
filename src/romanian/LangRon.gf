--# -no-pmcfg 
--# -path=.:../abstract:../common:../prelude

concrete LangRon of Lang = 
  GrammarRon,
  LexiconRon
  , DocumentationRon --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
