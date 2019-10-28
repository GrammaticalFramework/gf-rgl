concrete IdiomKam of Idiom = 
  CatKam ** IdiomBantu -[ProgrVP] with  (ResBantu = ResKam) ** open MorphoKam in {
  flags coding=utf8;
  lin
  ProgrVP vp = {s=\\ag,pol,tes,ant=>
                    (polanttense.s!pol!tes!ant!ag).p1 ++ibind ++ vp.progV;
                    compl=\\a => vp.compl!a;
                   progV= vp.progV; imp =\\po,n =>vp.imp!po!n;inf=vp.inf};
oper ibind : Str = Predef.BIND ;
};  

