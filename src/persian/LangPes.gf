--# -path=.:../abstract:../common

concrete LangPes of Lang = 
  GrammarPes
  ,LexiconPes
  ** {

flags startcat = Phr ; unlexer=unwords ; lexer=words ;

}
