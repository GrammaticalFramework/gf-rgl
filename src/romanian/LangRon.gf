--# -no-pmcfg 
--# -path=.:../abstract:../common:../prelude

concrete LangRon of Lang = 
  GrammarRon,
  LexiconRon
  , ConstructionRon --# notpresent
  , DocumentationRon --# notpresent
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
