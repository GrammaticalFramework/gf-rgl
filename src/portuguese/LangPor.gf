--# -path=.:../romance:../abstract:../common:../api:../prelude

concrete LangPor of Lang = 
  GrammarPor,
  LexiconPor
  ,DocumentationPor --# notpresent
  ,ConstructionPor
  ** {

flags startcat = Phr ;

} ;
