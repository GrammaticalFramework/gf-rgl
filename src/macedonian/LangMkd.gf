--# -path=.:../abstract:../common:../api

concrete LangMkd of Lang = 
  LexiconMkd
  ,DocumentationMkd --# notpresent

  ** {

flags startcat = Phr ;

} ;
