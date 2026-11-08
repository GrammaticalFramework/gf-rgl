concrete CommonGla of Common = open (R = ParamX), Prelude in {

  param
    GlaTense = GPres | GPast | GFut | GCond ;
    GlaAnteriority = GSimul | GAnter ;
    GlaPolarity = GPos | GNeg ;

  lincat
    Text = {s : Str} ;
    Phr = {s : Str} ;
    Utt = {s : Str} ;
    Voc = {s : Str} ;
    SC = {s : Str} ;
    Adv = {s : Str} ;
    AdV = {s : Str} ;
    AdA = {s : Str} ;
    AdN = {s : Str} ;
    IAdv = {s : Str} ;
    CAdv = {s,p : Str} ;
    PConj = {s : Str} ;
    Interj = {s : Str} ;

    Temp = {s : Str ; t : GlaTense ; a : GlaAnteriority} ;
    Tense = {s : Str ; t : GlaTense} ;
    Ant = {s : Str ; a : GlaAnteriority} ;
    Pol = {s : Str ; p : GlaPolarity} ;
    MU = {s : Str ; isPre : Bool} ;
}
