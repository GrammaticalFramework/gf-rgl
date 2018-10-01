concrete VerbAra of Verb = CatAra ** open Prelude, ResAra in {

  flags optimize=all_subs ;

  lin
    UseV = predV ;

    SlashVV vv vps = vps ** predV vv ; ----IL
    SlashV2a v = predVSlash v ;
    Slash3V3 v np = insertObj np (predVSlash v) ** {c2 = v.c3};

    ComplSlash vp np = insertObj np vp ;

--    Complv3 v np np2 = insertObj np2 (insertObj np (predV v)) ;

{-{s = \\_ => v.c2 ++ np.s ! Acc ++ v.c3 ++ np2.s ! Acc ;
                 a = {pgn = Per3 Masc Sg ; isPron = False} } --FIXME
      (predV v) ;-}

    ComplVV vv vp =  let vvVP = predV vv in --- IL
      vp ** {
        s = \\pgn,vpf => vvVP.s ! pgn ! vpf
                      ++ vv.c2  -- أَنْ‎
                      ++ vp.s ! pgn ! VPImpf Cnj
      } ;

--    ComplVS v s  = insertObj (\\_ => conjThat ++ s.s) (predV v) ;
--    ComplVQ v q  = insertObj (\\_ => q.s ! QIndir) (predV v) ;
--
--    ComplVA  v    ap = insertObj (ap.s) (predV v) ;
--    ComplV2A v np ap =
--      insertObj (\\_ => v.c2 ++ np.s ! Acc ++ ap.s ! np.a) (predV v) ;
--
    UseComp xabar = kaan xabar ;

    AdvVP vp adv = insertStr adv.s vp ;

--    AdVVP adv vp = insertAdV adv.s vp ;
--
--    ReflV2 v = insertObj (\\a => v.c2 ++ reflPron ! a) (predV v) ;
--
    PassV2 v = kaan {s = \\_,_ => v.s ! VPPart} ; ---- IL guessed
--
--    UseVS, UseVQ = \vv -> {s = vv.s ; c2 = [] ; isRefl = vv.isRefl} ; -- no

    CompCN cn = {s = \\agr,c => cn.s ! agr.n ! Indef ! c} ; ----IL
    CompAP ap = {s = \\agr,c => ap.s ! Hum ! agr.g ! agr.n ! Indef ! c} ; --FIXME
    CompNP np = {s = \\_,c => np.s ! c};
    CompAdv a = {s = \\_,_ => a.s} ;
--
--
}
