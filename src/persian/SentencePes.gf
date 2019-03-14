concrete SentencePes of Sentence = CatPes ** open Prelude, ResPes,Predef in {

  flags optimize=all_subs ;
  coding = utf8;

  lin

    PredVP np vp = mkClause np vp ;

    PredSCVP sc vp = mkSClause ("این" ++ sc.s) defaultAgr vp ;

    ImpVP vp = {
      s = \\pol,n =>
        let agr = Ag (numImp n) P2 ;
            vps = vp.prefix ++ vp.s ! VImp pol (numImp n)
         in case vp.vvtype of {
              NoVV => vp.ad ++ vp.comp ! agr ++ vp.obj ++ vp.vComp ! agr ! VVPres ++ vps ++ vp.embComp ;
              _    => vps ++ vp.ad ++ vp.comp ! agr ++ vp.obj ++ vp.vComp ! agr ! VVPres ++ vp.embComp }
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
      mkSlClause np
        (embComp (conjThat ++ slash.s ! Indic) (predV vs))  **
        {c2 = slash.c2} ;

    EmbedS  s  = {s = conjThat ++ s.s ! Indic} ;
    EmbedQS qs = qs ;
    EmbedVP vp = {s = showVPH Inf defaultAgr vp} ; --- agr


    UseCl temp p cl = {
      s = \\vvf => temp.s ++ p.s ++ case vvf of {
            Indic => cl.s ! TA temp.t temp.a ! p.p ! ODir ;
            Subj  => cl.s ! TA Cond   temp.a ! p.p ! ODir }
      } ;

    UseQCl temp p qcl  = let vt = TA temp.t temp.a in {
      s = temp.s ++ p.s ++ qcl.s ! vt ! p.p ;
      } ;

    UseRCl temp p rcl = let vt = TA temp.t temp.a in rcl ** {
      s = \\q => temp.s ++ p.s ++ rcl.s ! vt ! p.p ! ODir ! q
      } ;

    UseSlash temp p cls = cls ** {
      s = \\vvf => temp.s ++ p.s ++ cls.subj ++ case vvf of {
            Indic => cls.vp ! TA temp.t temp.a ! p.p ! ODir ;
            Subj  => cls.vp ! TA Cond   temp.a ! p.p ! ODir }
      } ;

    AdvS a s = {s = \\vvf => a.s ++ s.s ! vvf} ;

    RelS s r = {s = \\vvf => s.s ! vvf ++ r.s ! agrP3 Sg} ;
    SSubjS s1 sj s2 = {s = \\vvf => s1.s ! vvf ++ sj.s ++ s2.s ! sj.compl};

}
