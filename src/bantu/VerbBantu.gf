incomplete concrete VerbBantu of Verb = 
  CatBantu ** open Prelude, CommonBantu, ResBantu in {
{-
  flags optimize=all_subs ;

  lin
     UseV = predV ;

    SlashV2a v = predV v ** {c2 = v.c2 ; gapInMiddle = False} ;
    Slash2V3 v np = 
      insertObjc (\\_ => v.c2 ++ np.s ! NPAcc) (predV v ** {c2 = v.c3 ; gapInMiddle = False}) ;
    Slash3V3 v np = 
      insertObjc (\\_ => v.c3 ++ np.s ! NPAcc) (predVc v) ; ----

    ComplVV v vp = insertObj (\\a => infVP v.typ vp Simul CPos a) (predVV v) ;  ---- insertExtra?
    ComplVS v s  = insertExtra (conjThat ++ s.s) (predV v) ; 
---    ComplVS v s  = insertObj (variants {\\_ => conjThat ++ s.s; \\_ => s.s}) (predV v) ;
    ComplVQ v q  = insertExtra (q.s ! QIndir) (predV v) ;
   -- ComplVA v ap = insertObj (ap.s (predV v)) ;

    SlashV2V v vp = insertObjc (\\a => v.c3 ++ infVP v.typ vp Simul CPos a) (predVc v) ;
    SlashV2S v s  = insertExtrac (conjThat ++ s.s) (predVc v) ;   ---- insertExtra?
---    SlashV2S v s  = insertObjc (variants {\\_ => conjThat ++ s.s; \\_ => s.s}) (predVc v) ;
    SlashV2Q v q  = insertExtrac (q.s ! QIndir) (predVc v) ;
  --  SlashV2A v ap = insertObjc (\\a => ap.s ! a) (predVc v) ; ----

    ComplSlash vp np = case vp.gapInMiddle of {
      True  => insertObjPre (\\_ => vp.c2 ++ np.s ! NPAcc) vp ;
      False => insertObj    (\\_ => vp.c2 ++ np.s ! NPAcc) vp
      } ;
    SlashVV vv vp = 
      insertObj (\\a => infVP vv.typ vp Simul CPos a) (predVV vv) **
        {c2 = vp.c2 ; gapInMiddle = vp.gapInMiddle} ;
    SlashV2VNP vv np vp = 
      insertObjPre (\\_ => vv.c2 ++ np.s ! NPAcc)
        (insertObjc (\\a => vv.c3 ++ infVP vv.typ vp Simul CPos a) (predVc vv)) **
          {c2 = vp.c2 ; gapInMiddle = vp.gapInMiddle} ;

    UseComp comp = insertObj comp.s (predAux auxBe) ;

    AdvVP vp adv = insertObj (\\_ => adv.s) vp ;
    ExtAdvVP vp adv = insertObj (\\_ => frontComma ++ adv.s ++ finalComma) vp ;
    AdVVP adv vp = insertAdV adv.s vp ;

    AdvVPSlash vp adv = insertObj (\\_ => adv.s) vp ** {c2 = vp.c2 ; gapInMiddle = vp.gapInMiddle} ;
    AdVVPSlash adv vp = insertAdV adv.s vp ** {c2 = vp.c2 ; gapInMiddle = vp.gapInMiddle} ;

    ReflVP v = insertObjPre (\\a => v.c2 ++ reflPron ! a) v ;

    PassV2 v = insertObj (\\_ => v.s ! VPPart ++ v.p) (predAux auxBe) ;

---b    UseVS, UseVQ = \vv -> {s = vv.s ; c2 = [] ; isRefl = vv.isRefl} ; -- no "to"


    --CompAP ap = ap ;
    CompNP np = {s = \\_ => np.s ! NPAcc} ;
    CompAdv a = {s = \\_ => a.s} ;
    CompCN cn = {s = \\a => case (fromAgr a).n of { 
      Sg => artIndef ++ cn.s ! Sg ! Nom ;
      Pl => cn.s ! Pl ! Nom
      }
    } ; 

   -- UseCopula =  auxBe ;  

    VPSlashPrep vp p = vp ** {c2 = p.s!Sg!G1 } ; 
-}
}

