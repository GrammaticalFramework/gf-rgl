concrete CatExtZul of CatExt = open ResZul,Prelude,ParamX in {

  lincat
    Loc = { s : CType => Agr => Polarity => BasicTense => Str } ;
    LocN = { s : Str } ;
    QuantStem = { s : Agr => Str } ;
    RelStem = { s : Str } ;
    VAux = { s : Str ; at : AuxType } ;
    ConjN = { s : Str } ;
    IAdj = { s : AForm => Str } ;
    -- INAdv = { s : Str ; postIAdv : Bool } ;

    Postdet = { s : Agr => Str } ;

}
