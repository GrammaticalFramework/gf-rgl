concrete GrammarMay of Grammar =
  NounMay,
  VerbMay,
  AdjectiveMay,
  AdverbMay,
  NumeralMay,
  SentenceMay,
  QuestionMay,
  RelativeMay,
  ConjunctionMay,
  PhraseMay,
  TextX,
  StructuralMay,
  IdiomMay,
  TenseX - [AAnter, PNeg]
  ** open ParamX in {

  flags startcat = Phr ;

  lin
    AAnter = {s = "sudah" ; a = ParamX.Anter} ;
    PNeg = {s = "tidak" ; p = ParamX.Neg} ;

} ;
