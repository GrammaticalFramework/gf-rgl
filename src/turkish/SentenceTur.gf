concrete SentenceTur of Sentence = CatTur ** open Prelude, ResTur in {

  lin
    PredVP np vp = {s = \\t,a,p => np.s ! Nom ++ vp.compl ++ vp.s ! Perf ! VFin t a p np.a} ;

    PredSCVP sc vp = {
      s = \\t,a,p => sc.s ++ vp.compl ++ vp.s ! Perf ! VFin t a p (agrP3 Sg)
    } ;

    EmbedVP vp = {s = vp.compl ++ vp.s ! Perf ! VInf Pos} ;

    UseCl temp pol cl = {s = temp.s ++ pol.s ++ cl.s ! temp.t ! temp.a ! pol.p} ;

    UseQCl temp pol cl = {s = temp.s ++ pol.s ++ cl.s ! temp.t ! temp.a ! pol.p} ;

    UseRCl temp pol cl = {s = \\agr => temp.s ++ pol.s ++ cl.s ! temp.t ! temp.a ! pol.p ! agr} ;

    SlashVP np vp = {
      s = \\t,a,p => np.s ! Nom ++ vp.compl ++ mkVerbForms vp ! Perf ! VFin t a p np.a ;
      c = vp.c
    } ;
    AdvSlash cl adv = cl ** {
      s = \\t,a,p => cl.s ! t ! a ! p ++ adv.s
    } ;
    SlashPrep cl prep = cl ** {c = prep} ;
    SlashVS np v ss = {
      s = \\t,a,p => np.s ! Nom ++ ss.s ++ mkVerbForms v ! Perf ! VFin t a p np.a ;
      c = ss.c
    } ;

    EmbedQS q = {s = q.s} ;
    EmbedS s = {s = s.s} ;

    ImpVP vp = {s = \\p,n => vp.compl ++ vp.s ! Perf ! VImp p n
               } ;

    AdvS adv s = {
       s = adv.s ++ s.s
    } ;

    ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;

    SSubjS s1 subj s2 = {s = s1.s ++ "," ++ subj.s ++ s2.s} ;

    AdvImp adv imp = {
      s = \\p,n => adv.s ++ imp.s ! p ! n
    } ;

    UseSlash temp pol cl = {
      s = temp.s ++ pol.s ++ cl.s ! temp.t ! temp.a ! pol.p ;
      c = cl.c
    } ;

}
