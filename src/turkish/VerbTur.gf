concrete VerbTur of Verb = CatTur ** open ResTur in {

  lin
    UseV v = v ;
    SlashV2a v = v ;

    ComplSlash vps np = {
      s = \\ vf => vps.c.s ++ np.s ! vps.c.c ++ vps.s ! vf ;
    } ;

    -- TODO: test this and fix.
    ComplVS vs s = {
      s = \\vf => s.s ! SubordSuffixDik ++ vs.s ! vf
    } ;

}
