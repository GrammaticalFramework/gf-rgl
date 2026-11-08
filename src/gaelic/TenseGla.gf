concrete TenseGla of Tense = CommonGla ** {
  lin
    TTAnt t a = {s = t.s ++ a.s ; t = t.t ; a = a.a} ;
    PPos = {s = [] ; p = GPos} ;
    PNeg = {s = [] ; p = GNeg} ;
    TPres = {s = [] ; t = GPres} ;
    TPast = {s = [] ; t = GPast} ;
    TFut = {s = [] ; t = GFut} ;
    TCond = {s = [] ; t = GCond} ;
    ASimul = {s = [] ; a = GSimul} ;
    AAnter = {s = [] ; a = GAnter} ;
}
