--# -path=.:../abstract:../common:prelude

concrete GrammarKam of Grammar = 
  NounKam, 
  VerbKam, 
  AdjectiveKam,
  AdverbKam,
  NumeralKam,
  SentenceKam,
  QuestionKam,
  RelativeKam,
  ConjunctionKam,
  PhraseKam,
  TextX - [Pol,PPos,PNeg,Pres],
  StructuralKam,
  IdiomKam,
  TenseX - [Pol,PPos,PNeg,Pres]
  **  {

flags startcat = Phr ; unlexer = text ; lexer = text;

--lin
  --PPos = {s = [] ; p = CPos} ;
 -- PNeg = {s = [] ; p = CNeg True} ; -- contracted: don't
 --PPos  = {s = [] ; b = True} ;
 --  PNeg  = {s = [] ; b = False} ;
 --  TPres = {s = [] ; t = ResKam.Pres} ;
 -- TPast = {s = [] ; t = ResKam.Past };
 --  TFut = {s = [] ; t = ResKam.Fut };
} ;
