--# -path=.:../common:../abstract

concrete ExtendRon of Extend =
  CatRon ** ExtendFunctor - [PassVPSlash]
  with
    (Grammar = GrammarRon) ** 
  open ResRon in {

lin iFem_Pron = mkPronoun "eu" "mine" "mie" [] [] "meu" "mea" "mei" "mele" Fem Sg P1 ;
    youFem_Pron = mkPronoun "tu" "tine" "ţie" [] "tu" "tău" "ta" "tăi" "tale"  Fem Sg P2 ;
    youPlFem_Pron = mkPronoun "voi" "voi" "vouă" [] "voi" "vostru" "voastră" "voştri" "voastre" Fem Pl P2 ;
    youPolFem_Pron =
       let dvs = mkPronoun "dumneavoastră" "dumneavoastră" "dumneavoastră" [] "dumneavoastră" "dumneavoastră" "dumneavoastră" "dumneavoastră" "dumneavoastră" Fem Pl P2 
       in {s = dvs.s; c1 = dvs.c1; 
           c2 = dvs.c2; a = dvs.a; isPol = True; poss = dvs.poss} ;  

-- KA: derived from PassV2, objects are ignored
lin PassVPSlash vps = insertSimpObj (\\a => vps.s ! PPasse a.g a.n Indef ANomAcc) auxPassive ** {lock_VP = <>};

}
