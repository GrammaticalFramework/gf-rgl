concrete IdiomGus of Idiom = 
  CatGus ** IdiomBantu -[ProgrVP] with  (ResBantu = ResGus) ** open MorphoGus in {
  flags coding=utf8;
  lin
  ProgrVP vp = { s=\\ag,pol,tes,ant =>  case <tes ,ant,pol> of {
        <Past,Simul,Pos> =>(subjclitic.s!ag).p1 + "renge" ++vp.inf ;
        <_,_, Neg> =>(subjclitic.s!ag).p5 ++ ibind++ vp.inf ;
        <_,_,Pos> =>(subjclitic.s!ag).p1 ++ibind++ vp.inf };
        compl=\\a => vp.compl!a;
                   progV= []; imp =\\po,n =>vp.imp!po!n;inf=vp.inf};
                   oper ibind : Str = Predef.BIND ;
} 



