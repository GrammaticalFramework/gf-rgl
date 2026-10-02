concrete IdiomCze of Idiom = CatCze ** open Prelude, ResCze in {

lin
    ImpPl1 vp = {s = vp.verb.imppl1 ++ vp.clit ! Ag (Masc Anim) Pl P1 ++ vp.compl ! Ag (Masc Anim) Pl P1} ;
    ImpP3 np vp = {
      s = "nechť" ++ vp.clit ! np.a ++
          (case np.isDrop of {True => np.clit ! Nom ; False => np.s ! Nom}) ++
          verbAgr vp.verb np.a Pos ++ vp.compl ! np.a
      } ;

    ImpersCl vp = let agr = Ag Neutr Sg P3 in
      mkClause [] (vp.clit ! agr) (vp.compl ! agr) vp.verb agr True vp.clitPresent ;

    GenericCl vp = let agr = Ag (Masc Anim) Pl P3 in
      mkClause [] (vp.clit ! agr) (vp.compl ! agr) vp.verb agr True vp.clitPresent ;

    ExistNP np = mkClause [] [] (np.s ! Nom)
      (iii_kupovatVerbForms "existovat") np.a True False ;

    ExistNPAdv np adv =
      let cl = ExistNP np in cl ** {compl = cl.compl ++ adv.s} ;

    ProgrVP vp = vp ;

}
