concrete VerbTur of Verb = CatTur ** open ResTur in {

  lin
    UseV v = v ;
    SlashV2a v = v ;

    Slash2V3 v = variants {} ;
    Slash3V3 v = variants {} ;
    SlashV2A v = variants {} ;
    SlashV2V v = variants {} ;
    SlashV2S v = variants {} ;
    SlashV2Q v = variants {} ;
    SlashVV v = variants {} ;
    SlashV2VNP = variants {} ;

    ComplSlash vps np = {
      s = \\ vf => vps.c.s ++ np.s ! vps.c.c ++ vps.s ! vf ;
    } ;

    -- TODO: test this and fix.
    ComplVS vs s = {
      s = \\vf => s.subord ++ vs.s ! vf
    } ;

    ComplVA _ _ = variants {} ;
    ComplVV _ _ = variants {} ;
    ComplVQ _ _ = variants {} ;
    
    UseComp _ = variants {} ;
    CompCN _ = variants {} ;
    CompNP _ = variants {} ;
    CompAP _ = variants {} ;
    CompAdv _ = variants {} ;

    ReflVP = variants {} ;
    
    AdvVP = variants {} ;
    AdVVP = variants {} ;

    PassV2 = variants {} ;
}
