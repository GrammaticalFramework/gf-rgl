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
  TextX - [Adv],
  StructuralKis,
  IdiomKis,
  TenseX - [Adv]
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
