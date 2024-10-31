--# -path=.:../abstract:../common:../api

concrete LangMkd of Lang = 
  GrammarMkd,
  LexiconMkd
  ,DocumentationMkd --# notpresent

  ** {

flags startcat = Phr ;

} ;
