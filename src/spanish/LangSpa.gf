--# -path=.:../romance:../abstract:../common:../api:../prelude

concrete LangSpa of Lang = 
  GrammarSpa,
  LexiconSpa
  ,DocumentationSpa --# notpresent
  ,ConstructionSpa
  ** {

flags startcat = Phr ;

} ;
