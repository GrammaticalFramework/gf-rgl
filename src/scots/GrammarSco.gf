--# -path=.:../abstract:../common:prelude

concrete GrammarSco of Grammar =
  NounSco,
  VerbSco,
  AdjectiveSco,
  AdverbSco,
  NumeralSco,
  SentenceSco,
  QuestionSco,
  RelativeSco,
  ConjunctionSco,
  PhraseSco,
  TextX - [Pol,PPos,PNeg,SC,CAdv],
  StructuralSco,
  IdiomSco,
  TenseX - [Pol,PPos,PNeg,SC,CAdv],
  NamesSco
  ** open ResSco, Prelude in {

flags startcat = Phr ; unlexer = text ; lexer = text ;

lin
  PPos = {s = [] ; p = CPos} ;
  PNeg = {s = [] ; p = CNeg True} ; -- contracted: don't


} ;
