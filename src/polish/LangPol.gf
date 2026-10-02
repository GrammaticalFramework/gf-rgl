--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete LangPol of Lang = 
  GrammarPol,
  LexiconPol, 
  ConstructionPol,
  DocumentationPol --# notpresent
  ** { flags  startcat = Phr ; unlexer = text ; lexer = text; } ;
