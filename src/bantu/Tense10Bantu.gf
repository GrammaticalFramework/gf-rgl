incomplete concrete TenseBantu of Tense = 
  CatBantu [Tense,Temp], TenseX [Ant,AAnter,ASimul] ** 
    open ResBantu, CommonBantu, Prelude in {

  lin
    TTAnt t a = {s = a.s ++ t.s ; a = a.a ; t = t.t} ;
    TPres = {s = []} ** {t = RPres} ;
    TPast = {s = []} ** {t = RPast} ;   --# notpresent
    TFut  = {s = []} ** {t = RFut} ;    --# notpresent
    TCond = {s = []} ** {t = RCond} ;   --# notpresent

    PPos = {s = [] ; p = RPos} ;
    PNeg = {s = [] ; p = RNeg False} ; 

}
