--# -path=.:../abstract:../common:../prelude

-- L.Boizou, 2022 <lboizou@gmail.com>

concrete LangLit of Lang = 
  GrammarLit,
  LexiconLit 
  ** { flags  startcat = Phr ; unlexer = text ; lexer = text; } ;
