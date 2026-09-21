concrete IdiomCze of Idiom = CatCze ** open Prelude, ResCze in {

lin
    ImpPl1 vp = {s = vp.verb.imppl1 ++ vp.clit ! Ag (Masc Anim) Pl P1 ++ vp.compl ! Ag (Masc Anim) Pl P1} ;
    ImpP3 np vp = {
      s = "nechť" ++ vp.clit ! np.a ++
          (case np.isDrop of {True => np.clit ! Nom ; False => np.s ! Nom}) ++
          verbAgr vp.verb np.a True ++ vp.compl ! np.a
      } ;

    ImpersCl vp = let agr = Ag Neutr Sg P3 in {
      subj  = [] ;
      clit  = vp.clit ! agr ;
      compl = vp.compl ! agr ;
      verb  = vp.verb ; clitPresent = vp.clitPresent ;
      isDrop = True ;
      a     = agr
      } ;

    GenericCl vp = let agr = Ag (Masc Anim) Pl P3 in {
      subj  = [] ;
      clit  = vp.clit ! agr ;
      compl = vp.compl ! agr ;
      verb  = vp.verb ; clitPresent = vp.clitPresent ;
      isDrop = True ;
      a     = agr
      } ;

    ExistNP np = {
      clitPresent = False ;
      subj, clit = [] ;
      compl = np.s ! Nom ;
      verb = iii_kupovatVerbForms "existovat" ;
      isDrop = True ;
      a = np.a
      } ;

}
