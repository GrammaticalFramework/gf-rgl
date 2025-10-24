concrete TenseMkd of Tense = CatMkd[Tense,Temp], TenseX - [Tense,Temp,TTAnt,TPres,TPast,TFut,TCond] ** open ResMkd in {

lin
  TTAnt t a = {s = a.s ++ t.s ; a = a.a ; t = t.t} ;
  TPres = {s = []} ** {t = VPresent} ;
  TPast = {s = []} ** {t = VPastImperfect} ;  --# notpresent
  TFut  = {s = []} ** {t = VFut} ;            --# notpresent
  TCond = {s = []} ** {t = VCond} ;           --# notpresent

}
