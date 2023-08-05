--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete GrammarLit of Grammar = 
  NounLit, 
  VerbLit, 
  AdjectiveLit,
  AdverbLit,
  NumeralLit,
  SentenceLit,
  QuestionLit,
  RelativeLit,
  ConjunctionLit,
  PhraseLit,
  TenseX - [Adv,CAdv],
  TextX - [Adv,CAdv],
  StructuralLit,
  IdiomLit
   ** { flags  startcat = Phr ; unlexer = text ; lexer = text ;} ;
