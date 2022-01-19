concrete ExtraCatZul of ExtraCatZulAbs = open ResZul,Prelude,ParamX in {

  lincat
    LocN = { s : Str ; empty : Str } ;
    QuantStem = { s : Str ; isPost : Bool } ;
    RelStem = { s : Str } ;
    VAux = { s : Str ; at : AuxType } ;
    ConjN = { s : Str } ;
    IAdj = { s : AForm => Str } ;
    -- INAdv = { s : Str ; postIAdv : Bool } ;

}
