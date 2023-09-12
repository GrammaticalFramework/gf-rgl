--# -path=.:../romance:../abstract:../common:prelude

concrete GrammarCat of Grammar = 
  NounCat, 
  VerbCat, 
  AdjectiveCat,
  AdverbCat,
  NumeralCat,
  SentenceCat,
  QuestionCat,
  RelativeCat,
  ConjunctionCat,
  PhraseCat,
  TextX - [SC,Temp,Tense,Pol,PPos,PNeg,MU],
  IdiomCat,
  StructuralCat,
  TenseCat,
  NamesCat

  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
