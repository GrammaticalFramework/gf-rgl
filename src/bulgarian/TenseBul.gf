concrete TenseBul of Tense = CatBul [Tense,Temp], TenseX - [Temp,Tense,TPres,TPast,TFut,TCond,IAdv,AdV,SC] ** open ResBul in {

lin
  TPres = {s = []} ** {t = VPresent} ;
  TPast = {s = []} ** {t = VPastImperfect} ;  --# notpresent
  TFut  = {s = []} ** {t = VFut} ;            --# notpresent
  TCond = {s = []} ** {t = VCond} ;           --# notpresent

}
