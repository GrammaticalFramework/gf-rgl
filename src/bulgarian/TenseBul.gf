concrete TenseBul of Tense = CatBul [Tense,Temp], TenseX - [Temp,Tense,TTAnt,TPres,TPast,TFut,TCond,IAdv,AdV,SC] ** open ResBul in {

lin
  TTAnt t a = {s = a.s ++ t.s ; a = a.a ; t = t.t} ;
  TPres = {s = []} ** {t = VPresent} ;
  TPast = {s = []} ** {t = VPastImperfect Indicative} ;  --# notpresent
  TFut  = {s = []} ** {t = VFut Indicative} ;            --# notpresent
  TCond = {s = []} ** {t = VCond Indicative} ;           --# notpresent

}
