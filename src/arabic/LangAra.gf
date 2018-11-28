--# -path=.:../abstract:../common:../api:../prelude

concrete LangAra of Lang = 
  GrammarAra,
  LexiconAra,
  ConstructionAra
  ** {

  flags startcat = Phr ; unlexer = text ; lexer = text ; coding = utf8 ;

}


