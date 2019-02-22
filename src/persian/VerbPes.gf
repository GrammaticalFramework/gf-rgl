concrete VerbPes of Verb = CatPes ** open ResPes,Prelude in {

  flags coding = utf8;
  flags optimize=all_subs ;

  lin
    UseV  v = predV v   ;
    SlashV2a v = predV v ** {c2 = v.c2} ; --{s = v.c2.s ; ra = v.c2.ra ; c = VTrans}} ;
    Slash2V3 v np = {c2 = {s = [] ; ra = v.c3}} **
      insertObj (np.s ! Bare ++ v.c2) (predV v) ; --  **  ; c = VTrans}}) ;

    Slash3V3 v np = {c2 = {s = [] ; ra = v.c2}} **
      insertObj (v.c3 ++ np.s ! Bare) (predV v) ; -- **  ; c = VTrans}}) ;

    ComplSlash vp np = insertObjPre np vp ;

    ComplVV v vp = insertVV (infVV v.isAux vp) (predV v) ;
    ComplVS v s  = embComp (conjThat ++ s.s) (predV v) ;
    ComplVQ v q  = embComp (conjThat ++ q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertComp (\\_ => ap.s ! Bare) (predV v) ; -- check form of adjective
    SlashV2V v vp = insertVV (infVV v.isAux vp) (predV v) **{c2 = {s = v.c1 ; ra = []}} ;

    SlashV2S v s  = v ** embComp (conjThat ++ s.s) (predV v) ;
    SlashV2Q v q  = v ** embComp (q.s ! QIndir) (predV v) ;
    SlashV2A v ap = v ** insertObj (ap.s ! Bare) (predV v) ; ---- paint it red , check form of adjective

    SlashVV vv vps = vps ** insertVV (infVV vv.isAux vps) (predV vv) ;

    SlashV2VNP v2v np vps =
      let vvVP : VPH = insertVV (infVV v2v.isAux vps) (predV v2v) ;
          vvVPS = vvVP ** {c2={s=v2v.c1 ; ra=v2v.c2}} ;
       in insertObjPre np vvVPS ** {c2 = vps.c2} ;


    UseComp comp = insertComp comp.s (predAux auxBe) ;

    AdvVP vp adv = insertAdV adv.s vp ;

    AdVVP adv vp = insertAdV adv.s vp ;
    ReflVP v = insertCompPre reflPron v ;
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
