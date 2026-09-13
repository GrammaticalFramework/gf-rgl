--# -path=.:../abstract:../common:../api

concrete LangMkd of Lang = 
  GrammarMkd,
  LexiconMkd
  ,ConstructionMkd
  ,DocumentationMkd --# notpresent

  ** {

flags startcat = Phr ;

} ;
