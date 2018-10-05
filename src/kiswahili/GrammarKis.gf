--# -path=.:../abstract:../common:prelude

concrete GrammarKis of Grammar = 
  NounKis, 
  VerbKis, 
  AdjectiveKis,
  AdverbKis,
  NumeralKis,
  SentenceKis,
  QuestionKis,
  RelativeKis,
  ConjunctionKis,
  PhraseKis,
  TextX - [Pol,PPos,PNeg,Pres],
  StructuralKis,
  IdiomKis,
  TenseX - [Pol,PPos,PNeg,Pres]
  **  {

flags startcat = Phr ; unlexer = text ; lexer = text;

--lin
  --PPos = {s = [] ; p = CPos} ;
 -- PNeg = {s = [] ; p = CNeg True} ; -- contracted: don't
 --PPos  = {s = [] ; b = True} ;
 --  PNeg  = {s = [] ; b = False} ;
 --  TPres = {s = [] ; t = ResKis.Pres} ;
 -- TPast = {s = [] ; t = ResKis.Past };
 --  TFut = {s = [] ; t = ResKis.Fut };
} ;
