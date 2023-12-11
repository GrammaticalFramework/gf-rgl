--# -path=.:../romance:../abstract:../common:prelude

concrete GrammarSpa of Grammar = 
  NounSpa, 
  VerbSpa, 
  AdjectiveSpa,
  AdverbSpa,
  NumeralSpa,
  SentenceSpa,
  QuestionSpa,
  RelativeSpa,
  ConjunctionSpa,
  PhraseSpa,
  TextSpa - [SC,Temp,Tense,Pol,PPos,PNeg,MU],      -- special punctuation
  IdiomSpa,
  StructuralSpa,
  TenseSpa,
  NamesSpa

  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
