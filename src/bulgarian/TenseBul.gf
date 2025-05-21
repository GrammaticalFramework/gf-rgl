concrete TenseBul of Tense = CatBul [Tense,Temp], TenseX - [Temp,Tense,TTAnt,TPres,TPast,TFut,TCond,IAdv,AdV,SC] ** open ResBul in {

lin
  TTAnt t a = {s = a.s ++ t.s ; a = a.a ; t = t.t} ;
  TPres = {s = []} ** {t = VPresent} ;
  TPast = {s = []} ** {t = VPastImperfect} ;  --# notpresent
  TFut  = {s = []} ** {t = VFut} ;            --# notpresent
  TCond = {s = []} ** {t = VCond} ;           --# notpresent

}
