concrete VerbPes of Verb = CatPes ** open ResPes,Prelude in {

  flags coding = utf8;
  flags optimize=all_subs ;

  lin
    UseV  v = predV v   ;
    SlashV2a v = predV v ** {c2 = {s = v.c2.s ; ra = v.c2.ra ; c = VTrans}} ;
    Slash2V3 v np =
      insertObjc (\\_ =>  np.s ! Bare ++ v.c2 ) (predV v ** {c2 = {s = [] ; ra = v.c3 ; c = VTrans}}) ;

    Slash3V3 v np =
      insertObjc (\\_ => v.c3 ++ np.s ! Bare) (predV v ** {c2 = {s = [] ; ra = v.c2 ; c = VTrans}}) ;

    ComplVV v vp = insertVV (infVV v.isAux vp).s (predV v) ;
    ComplVS v s  = insertObj2 (conjThat ++ s.s) (predV v) ;
    ComplVQ v q  = insertObj2 (conjThat ++ q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertObj (\\_ => ap.s ! Bare) (predV v) ; -- check form of adjective
    SlashV2V v vp = insertVV (infVV v.isAux vp).s (predV v) **{c2 = {s = v.c1 ; ra = [] ; c = VTransPost}} ;

    SlashV2S v s  = insertObjc2 (conjThat ++ s.s) (predV v ** {c2 = {s = v.c2.s ;ra = [] ; c = VTransPost}}) ;
    SlashV2Q v q  = insertObjc2 ( q.s ! QIndir) (predV v ** {c2 = {s = v.c2.s ; ra = [] ;c = VTransPost}}) ;
    SlashV2A v ap = insertObjc3 ( ap.s ! Bare) (predV v ** {c2 = {s = [] ; ra = v.c2.ra ;c = VTransPost}}) ; ---- paint it red , check form of adjective

     ComplSlash vp np = insertObjPre (\\_ =>  np.s ! Bare ) vp ;
    SlashVV vv vp =
 --     insertObj (infVV vv.isAux vp).s (predV vv)  **
      insertVV (infVV vv.isAux vp).s (predV vv)  **
        {c2 = vp.c2} ;
    SlashV2VNP vv np vp =
      insertObjPre (\\_ =>  np.s ! Bare  )
--        (insertObjc (infVV vv.isAux vp).s (predVc vv)) **
      (insertVVc (infVV vv.isAux vp).s (predVc vv)) **
          {c2 = vp.c2} ;

    UseComp comp = insertObj comp.s (predAux auxBe) ;

    AdvVP vp adv = insertAdV adv.s vp ;

    AdVVP adv vp = insertAdV adv.s vp ;
    ReflVP v = insertObjPre (\\a =>  reflPron ! a) v ;
    PassV2 v = predV v ; -- need to be fixed
    CompAP ap ={s = \\_ => ap.s ! Bare} ; -- check form of adjective
    CompAdv adv = {s = \\_ => adv.s } ;

    -- see https://sites.la.utexas.edu/persian_online_resources/nouns/noun-in-a-predicative-position/
    CompCN cn = {
      s = \\a => cn.s ! giveNumber a
                      ! case cn.hasAdj of {
                          False => Bare ;
                          True  => Clitic }

      } ;

    CompNP np = {
      s = \\a => np.s ! case np.hasAdj of {
                          False => Bare ;
                          True  => Clitic }
      } ;

}
