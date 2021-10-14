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
  TenseX - [AAnter, TFut, TCond]
  ** open ParamX in {

  flags startcat = Phr ;

  lin
    AAnter = {s = "sudah" ; a = ParamX.Anter} ;
    TFut   = {s = "akan" ; t = ParamX.Fut} ;
    TCond  = {s = "akan" ; t = ParamX.Cond} ;

} ;
