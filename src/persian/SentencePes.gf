concrete SentencePes of Sentence = CatPes ** open Prelude, ResPes,Predef in {

  flags optimize=all_subs ;
  coding = utf8;

  lin

    PredVP np vp = mkClause np vp ;

    PredSCVP sc vp = mkSClause sc.s defaultAgr vp ;

    ImpVP vp = {
      s = \\pol,n =>
        let agr = Ag n P2 ;
            vps = vp.prefix ++ vp.s ! VImp pol n
         in case vp.vvtype of {
              NoVV => vp.ad ++ vp.comp ! agr ! OV ++ vp.obj ++ vp.vComp ! agr ! VVPres ++ vps ++ vp.embComp ;
              _    => vps ++ vp.ad ++ vp.comp ! agr ! OV {-TODO check if legit-} ++ vp.obj ++ vp.vComp ! agr ! VVPres ++ vp.embComp }
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
    EmbedVP vp = {s = infVP vp} ; --- agr


    UseCl temp p cl = {
      s = \\vvf => temp.s ++ p.s ++ case vvf of {
            Indic => cl.s ! Ind temp.t temp.a ! p.p ! ODir ;
            Subj  => cl.s ! Sub        temp.a ! p.p ! ODir }
      } ;

    UseQCl temp p qcl  = let vt = Ind temp.t temp.a in {
      s = temp.s ++ p.s ++ qcl.s ! vt ! p.p ;
      } ;

    UseRCl temp p rcl = let vt = Ind temp.t temp.a in rcl ** {
      s = \\a => temp.s ++ p.s ++ rcl.s ! vt ! p.p ! a
      } ;

    UseSlash temp p cls = cls ** {
      s = \\vvf => temp.s ++ p.s ++ cls.subj ++ case vvf of {
            Indic => cls.vp ! Ind temp.t temp.a ! p.p ! ODir ;
            Subj  => cls.vp ! Sub        temp.a ! p.p ! ODir }
      } ;

    AdvS a s = {s = \\vvf => a.s ++ s.s ! vvf} ;

    RelS s r = {s = \\vvf => s.s ! vvf ++ rs2str Ke (agrP3 Sg) r} ;
    SSubjS s1 sj s2 = {s = \\vvf => s1.s ! vvf ++ sj.s ++ s2.s ! sj.compl};

}
