concrete SentenceTur of Sentence = CatTur ** open Prelude, ResTur in {

  lin
    PredVP np vp = {s = \\t,p => np.s ! Nom ++ vp.compl ++ vp.s ! Perf ! VFin t p np.a} ;

    PredSCVP sc vp = variants {} ;

    -- TODO: Check how correct this is.
    EmbedVP vp = variants {} ; -- {s = (vp.s ! Gerund Sg Acc)} ;

    UseCl temp pol cl = {s = temp.s ++ pol.s ++ cl.s ! temp.t ! pol.p} ;

    UseQCl _ _ = variants {} ;

    UseRCl _ _ _ = variants {} ;

    SlashVP _ _ = variants {} ;
    AdvSlash _ _ = variants {} ;
    SlashPrep _ _ = variants {} ;
    SlashVS v = variants {} ;

    EmbedQS _ = variants {} ;
    EmbedS _ = variants {} ;

    ImpVP vp = {s = \\p,n => vp.compl ++ vp.s ! Perf ! VImp p n
               } ;

    AdvS _ _ = variants {} ;
    
    UseSlash _ = variants {} ;

}
