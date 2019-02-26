concrete SentencePes of Sentence = CatPes ** open Prelude, ResPes,Predef in {

  flags optimize=all_subs ;
  coding = utf8;

  lin

    PredVP np vp = mkClause np vp ;

    PredSCVP sc vp = mkSClause ("این" ++ sc.s) defaultAgr vp ;

    ImpVP vp = {
      s = \\pol,n =>
        let agr = Ag (numImp n) P2 ;
         in case vp.defVV of {
              True  => vp.s ! VImp pol (numImp n) ++ vp.ad ++ vp.comp ! agr ++ vp.obj ++ vp.vComp ! agr ! Simul ++ vp.embComp;
              False => vp.ad ++ vp.comp ! agr ++ vp.obj ++ vp.vComp ! agr ! Simul ++ vp.s ! VImp pol (numImp n) ++ vp.embComp }
    } ;

    SlashVP np vp =
      mkSlClause np vp ** {c2 = vp.c2} ;

    AdvSlash slash adv = slash ** {
	  vp = \\t,p,o => adv.s ++ slash.vp ! t ! p ! o  ;
      } ;

    SlashPrep cl prep = {
      subj = [] ; ---- AR 18/9/2017 this can destroy SOV ; Cl should be made discont
      vp = cl.s ;
      c2 = prep ** {ra = []}
      } ;

    SlashVS np vs slash =
      mkSlClause  np
        (embComp (conjThat ++ slash.s) (predV vs))  **
        {c2 = slash.c2} ;

    EmbedS  s  = {s = conjThat ++ s.s} ;
    EmbedQS qs = {s = qs.s ! QIndir} ;
    EmbedVP vp = {s = showVPH Inf defaultAgr vp} ; --- agr


    UseCl temp p cl = let vt = TA temp.t temp.a in {
      s = temp.s ++ p.s ++ cl.s ! vt ! p.p ! ODir
      } ;

    UseQCl temp p qcl  = let vt = TA temp.t temp.a in {
      s = \\q => temp.s ++ p.s ++ qcl.s ! vt ! p.p ! q;
      } ;

    UseRCl temp p rcl = let vt = TA temp.t temp.a in rcl ** {
      s = \\q => temp.s ++ p.s ++ rcl.s ! vt ! p.p ! ODir ! q
      } ;

    UseSlash temp p cls = let vt = TA temp.t temp.a in cls ** {
      s = temp.s ++ p.s ++ cls.subj ++ cls.vp ! vt ! p.p ! ODir
      } ;

    AdvS a s = {s = a.s ++ s.s} ;

    RelS s r = {s = s.s ++ r.s ! agrP3 Sg} ;
    SSubjS s1 sj s2 = { s = s1.s ++ sj.s ++ s2.s};

}
