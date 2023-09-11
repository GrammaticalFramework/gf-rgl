--# -path=.:../romance:../abstract:../common:prelude

concrete GrammarPor of Grammar = 
  NounPor, 
  VerbPor, 
  AdjectivePor,
  AdverbPor,
  NumeralPor,
  SentencePor,
  QuestionPor,
  RelativePor,
  ConjunctionPor,
  PhrasePor,
  TextPor - [SC,Temp,Tense,Pol,PPos,PNeg,MU],      -- special punctuation
  IdiomPor,
  StructuralPor,
  TensePor,
  NamesPor

  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
