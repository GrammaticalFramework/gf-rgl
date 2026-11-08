concrete VerbSco of Verb = VerbEng-[UseV,SlashV2a,Slash2V3,Slash3V3,
  ComplVV,ComplVS,ComplVQ,ComplVA,SlashVV,SlashV2V,SlashV2VNP,SlashV2S,SlashV2Q,SlashV2A,
  ReflVP,UseComp,UseCopula,PassV2] ** open Prelude, ResSco in {

lin UseV = predV ;

    SlashV2a v = predVc v ** {c2 = v.c2 ; gapInMiddle = False} ;
    Slash2V3 v np =
      insertObjc (\\_ => v.c2 ++ np.s ! NPAcc)
        (predVc v ** {c2 = v.c3 ; gapInMiddle = False}) ;
    Slash3V3 v np = insertObjc (\\_ => v.c3 ++ np.s ! NPAcc) (predVc v) ;

    ComplVV v vp = insertObj (\\a => infVP v.typ vp False Simul CPos a) (predVV v) ;
    ComplVS v s = insertExtra (conjThat ++ s.s) (predV v) ;
    ComplVQ v q = insertExtra (q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertObj ap.s (predV v) ;

    SlashVV vv vp = vp **
      insertObj (\\a => infVP vv.typ vp False Simul CPos a) (predVV vv) ;

    SlashV2V v vp =
      insertObjc (\\a => v.c3 ++ infVP v.typ vp False Simul CPos a) (predVc v) ;
    SlashV2VNP vv np vp = vp **
      insertObjPre (\\_ => vv.c2 ++ np.s ! NPAcc)
        (insertObjc (\\a => vv.c3 ++ infVP vv.typ vp False Simul CPos a) (predVc vv)) ;
    SlashV2S v s = insertExtrac (conjThat ++ s.s) (predVc v) ;
    SlashV2Q v q = insertExtrac (q.s ! QIndir) (predVc v) ;
    SlashV2A v ap = insertObjc (\\a => v.c3 ++ ap.s ! a) (predVc v) ;

    ReflVP v = insertObjPre (\\a => v.c2 ++ reflPron ! a) v ;

    UseComp comp = insertObj comp.s (predAux auxBe) ;
    UseCopula = predAux auxBe ;
    PassV2 v = insertObj (\\_ => v.s ! VPPart ++ v.p) (predAux auxBe) ;

}
