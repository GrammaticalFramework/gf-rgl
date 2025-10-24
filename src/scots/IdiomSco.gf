concrete IdiomSco of Idiom = IdiomEng - [CleftNP,CleftAdv,ExistNP,ExistIP,ExistNPAdv,ExistIPAdv,ProgrVP] ** open Prelude, ResSco in {

lin CleftNP np rs = mkClause "it" (agrP3 Sg)
      (insertObj (\\_ => rs.s ! np.a)
        (insertObj (\\_ => np.s ! rs.c) (predAux auxBe))) ;

    CleftAdv ad s = mkClause "it" (agrP3 Sg)
      (insertObj (\\_ => conjThat ++ s.s)
        (insertObj (\\_ => ad.s) (predAux auxBe))) ;

    ExistNP np =
      mkClause "there" (agrP3 (fromAgr np.a).n)
        (insertObj (\\_ => np.s ! NPAcc) (predAux auxBe)) ;

    ExistIP ip =
      mkQuestion (ss (ip.s ! npNom))
        (mkClause "there" (agrP3 ip.n) (predAux auxBe)) ;

    ExistNPAdv np adv =
      mkClause "there" (agrP3 (fromAgr np.a).n)
        (insertObj (\\_ => np.s ! NPAcc ++ adv.s) (predAux auxBe)) ;

    ExistIPAdv ip adv =
      mkQuestion (ss (ip.s ! npNom))
        (mkClause "there" (agrP3 ip.n) (insertObj (\\_ => adv.s) (predAux auxBe))) ;

    ProgrVP vp = insertObj (\\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a) (predAux auxBe) ;

}
