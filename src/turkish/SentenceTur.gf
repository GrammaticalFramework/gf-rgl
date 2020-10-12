concrete SentenceTur of Sentence = CatTur ** open Prelude, ResTur in {

  lin

    PredVP np vp = mkClause (np.s ! Nom) np.a vp ;

    PredSCVP sc vp = variants {} ;

    -- TODO: Check how correct this is.
    EmbedVP vp = {s = (vp.s ! Gerund Sg Acc)} ;

    -- TODO: rudimentary implementation; revise this.
    UseCl temp pol cl = {s = temp.s ++ cl.s ! temp.t; subord=cl.subord} ;

    UseQCl _ _ = variants {} ;

    UseRCl _ _ _ = variants {} ;

    SlashVP _ _ = variants {} ;
    AdvSlash _ _ = variants {} ;
    SlashPrep _ _ = variants {} ;
    SlashVS v = variants {} ;

    EmbedQS _ = variants {} ;
    EmbedS _ = variants {} ;
    
    ImpVP _ = variants {} ;
    
    AdvS _ _ = variants {} ;
    
    UseSlash _ = variants {} ;

}
