--# -path=.:../abstract
concrete LangSqi of Lang =
  GrammarSqi,
  LexiconSqi
  ,ConstructionSqi
  ,DocumentationSqi --# notpresent
  ** {

flags startcat = Phr ;

}
