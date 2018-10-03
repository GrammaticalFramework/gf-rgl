--# -path=.:../abstract:../common:prelude

concrete GrammarGus of Grammar = 
  NounGus, 
  VerbGus, 
  AdjectiveGus,
  AdverbGus,
  NumeralGus,
  SentenceGus,
  QuestionGus,
  RelativeGus,
  ConjunctionGus,
  PhraseGus,
  TextX - [Pol,PPos,PNeg,Pres],
  StructuralGus,
  IdiomGus,
  TenseX - [Pol,PPos,PNeg,Pres]
  **  {

flags startcat = Phr ; unlexer = text ; lexer = text;

--lin
  --PPos = {s = [] ; p = CPos} ;
 -- PNeg = {s = [] ; p = CNeg True} ; -- contracted: don't
 --PPos  = {s = [] ; b = True} ;
 --  PNeg  = {s = [] ; b = False} ;
 --  TPres = {s = [] ; t = ResGus.Pres} ;
 -- TPast = {s = [] ; t = ResGus.Past };
 --  TFut = {s = [] ; t = ResGus.Fut };
} ;
