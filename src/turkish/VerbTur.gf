concrete VerbTur of Verb = CatTur ** open Prelude, ResTur, SuffixTur, HarmonyTur in {

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
    
    UseComp comp = comp ;
    CompCN _ = variants {} ;

    CompNP ap = lin VP {
      s = table {
            VPres   agr => ap.s ! Nom ++ 
                           case agr of {
                             {n=Sg; p=P3} => [] ;
                             _            => suffixStr ap.h (verbSuffixes ! agr)
                           } ;
            VProg   agr => ap.s ! Nom ;
            VPast   agr => ap.s ! Nom ++
                           suffixStr ap.h (alethicCopulaSuffixes ! agr);
            VFuture agr => ap.s ! Nom ++
                           addSuffix "olacağ" (mkHar I_Har (SCon Hard)) (verbSuffixes ! agr) ;
            VInfinitive => ap.s ! Nom ++ "olmak" ;
            _           => "TODO"
          }
    } ;

    CompAP ap = lin VP {
      s = table {
            VPres   agr => case agr.p of {
                             P3 => ap.s ! Sg ! Nom ;
                             _  => ap.s ! agr.n ! Nom ++ suffixStr ap.h (verbSuffixes ! agr)
                           } ;
            VProg   agr => ap.s ! agr.n ! Nom ;
            VPast   agr => ap.s ! agr.n ! Nom ++
                           suffixStr ap.h (alethicCopulaSuffixes ! agr);
            VFuture agr => ap.s ! agr.n ! Nom ++
                           addSuffix "olacağ" (mkHar I_Har (SCon Hard)) (verbSuffixes ! agr) ;
            VInfinitive => ap.s ! Sg ! Nom ++ "olmak" ;
            _           => "TODO"
          }
    } ;

    CompAdv _ = variants {} ;

    ReflVP = variants {} ;
    
    AdvVP = variants {} ;
    AdVVP = variants {} ;

    PassV2 = variants {} ;
}
