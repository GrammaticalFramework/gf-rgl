concrete SentenceTur of Sentence = CatTur ** open Prelude, ResTur in {

  lin
    PredVP np vp = {s = \\t,a,p => np.s ! Nom ++ vp.compl ++ vp.s ! Perf ! VFin t a p np.a} ;

    PredSCVP sc vp = variants {} ;

    -- TODO: Check how correct this is.
    EmbedVP vp = variants {} ; -- {s = (vp.s ! Gerund Sg Acc)} ;

    UseCl temp pol cl = {s = temp.s ++ pol.s ++ cl.s ! temp.t ! temp.a ! pol.p} ;

    UseQCl _ _ = variants {} ;

    UseRCl temp pol cl = {s = \\agr => temp.s ++ pol.s ++ cl.s ! temp.t ! temp.a ! pol.p ! agr} ;

    SlashVP _ _ = variants {} ;
    AdvSlash _ _ = variants {} ;
    SlashPrep _ _ = variants {} ;
    SlashVS v = variants {} ;

    EmbedQS _ = variants {} ;
    EmbedS _ = variants {} ;

    ImpVP vp = {s = \\p,n => vp.compl ++ vp.s ! Perf ! VImp p n
               } ;

    AdvS adv s = {
       s = adv.s ++ s.s
    } ;

    UseSlash _ = variants {} ;

}
