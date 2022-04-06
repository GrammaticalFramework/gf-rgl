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
  TextX - [IAdv],
  StructuralMay,
  IdiomMay,
  TenseX - [AAnter, TFut, TCond, IAdv]
  ** open ParamX in {

  flags startcat = Phr ;

  lin
    AAnter = {s = "sudah" ; a = ParamX.Anter} ;
    TFut   = {s = "akan" ; t = ParamX.Fut} ;
    TCond  = {s = "akan" ; t = ParamX.Cond} ;

} ;
