--# -path=.:../abstract:../common:../api:../prelude

concrete LangBul of Lang = 
  GrammarBul,
  LexiconBul,
  ConstructionBul
  ,DocumentationBul --# notpresent
  ** {

flags startcat = Phr ;

} ;
