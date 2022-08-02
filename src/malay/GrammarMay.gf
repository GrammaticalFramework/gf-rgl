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
  TextX - [AAnter, TFut, TCond, IAdv],
  StructuralMay,
  IdiomMay,
  TenseX - [AAnter, TFut, TCond, IAdv]
  ** open ParamX in {

  flags startcat = Phr ;

  lin AAnter = {s = "sudah" ; a = ParamX.Anter} ; --# notpresent
    TFut   = {s = "akan" ; t = ParamX.Fut} ; --# notpresent
    TCond  = {s = "akan" ; t = ParamX.Cond} ; --# notpresent

} ;
