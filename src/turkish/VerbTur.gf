concrete VerbTur of Verb = CatTur ** open Prelude, ResTur, SuffixTur, HarmonyTur in {

  lin
    UseV v = {s = mkVerbForms v; compl = []} ;
    SlashV2a v = v ** {compl = []} ;

    Slash2V3 v = variants {} ;
    Slash3V3 v = variants {} ;
    SlashV2A v = variants {} ;
    SlashV2V v = variants {} ;
    SlashV2S v = variants {} ;
    SlashV2Q v = variants {} ;
    SlashVV v = variants {} ;
    SlashV2VNP = variants {} ;

    ComplSlash vps np = {
      s     = mkVerbForms vps ;
      compl = vps.compl ++ vps.c.s ++ np.s ! vps.c.c ;
    } ;

    -- TODO: test this and fix.
    ComplVS vs s = variants {} ; {- vs ** {
      compl = s.subord
    } ; -}

    ComplVA _ _ = variants {} ;
    ComplVV _ _ = variants {} ;
    ComplVQ _ _ = variants {} ;
    
    UseComp comp = comp ** {compl = []} ;
    CompCN _ = variants {} ;

    CompNP np = {
      s = \\asp,vform =>
          case <asp,vform> of {
            <Perf,VFin Pres Simul p agr>
                            => np.s ! Nom ++
                               case <agr,p> of {
                                 <{n=Sg; p=P3},Pos> => [] ;
                                 <{n=Sg; p=P3},Neg> => BIND ++ suffixStr np.h negativeSuffix ;
                                 <_,           Pos> => BIND ++ suffixStr np.h (verbSuffixes ! agr) ;
                                 <_,           Neg> => BIND ++ suffixStr np.h negativeSuffix +
                                                       (let negHar = mkHar (case np.h.vow of {
                                                                             I_Har  | U_Har  => I_Har ;
                                                                             Ih_Har | Uh_Har => Ih_Har
                                                                            }) SVow
                                                        in suffixStr negHar (verbSuffixes ! agr))
                               } ;
            <Perf,VFin Past Simul p agr>
                            => np.s ! Nom ++ BIND ++
                               case p of {
                                 Pos => [] ;
                                 Neg => suffixStr np.h negativeSuffix
                               } +
                               suffixStr np.h (alethicCopulaSuffixes ! agr) ;
            _               => np.s ! Nom ++
                               mkVerbForms olmak_V ! asp ! vform
          } ;
      compl = []
    } ;

    CompAP ap = {
      s = \\asp,vform =>
          case <asp,vform> of {
            <_,VImp p n>    => ap.s ! n ! Nom ++
                               mkVerbForms olmak_V ! asp ! (VImp p n) ;
            <Perf,VFin Pres Simul p agr> =>
                               ap.s ! agr.n ! Nom ++
                               case <agr,p> of {
                                 <{n=Sg; p=P3},Pos> => [] ;
                                 <{n=Sg; p=P3},Neg> => BIND ++ suffixStr ap.h negativeSuffix ;
                                 <_,           Pos> => BIND ++ suffixStr ap.h (verbSuffixes ! agr) ;
                                 <_,           Neg> => BIND ++ suffixStr ap.h negativeSuffix +
                                                       (let negHar = {vow = case ap.h.vow of {
                                                                             I_Har  | U_Har  => I_Har ;
                                                                             Ih_Har | Uh_Har => Ih_Har
                                                                           } ;
                                                                     con = SVow
                                                                    }
                                                        in suffixStr negHar (verbSuffixes ! agr))
                               } ;
            <Perf,VFin Past Simul p agr> =>
                               ap.s ! agr.n ! Nom ++ BIND ++
                               case p of {
                                 Pos => [] ;
                                 Neg => suffixStr ap.h negativeSuffix
                               } +
                               suffixStr ap.h (alethicCopulaSuffixes ! agr) ;
            <_,VFin t a p agr> =>
                               ap.s ! agr.n ! Nom ++
                               mkVerbForms olmak_V ! asp ! vform ;
            _               => ap.s ! Sg ! Nom ++
                               mkVerbForms olmak_V ! asp ! vform
          } ;
      compl = []
    } ;

    CompAdv _ = variants {} ;

    ReflVP = variants {} ;

    AdvVP vp adv = vp ** {
      compl = vp.compl ++ adv.s ;
    } ;

    ExtAdvVP vp adv = vp ** {
      compl = vp.compl ++ adv.s ;
    } ;

    AdVVP adv vp = vp ** {
      s = \\asp,vf => adv.s ++ vp.s ! asp ! vf ;
    } ;

    AdvVPSlash vp adv = vp ** {
      compl = vp.compl ++ adv.s ;
    } ;

    AdVVPSlash adv vp = vp ** {
      compl = vp.compl ++ adv.s ;
    } ;

    PassV2 v = {
      s = mkVerbForms {
            s = v.stems ! VPass ++ BIND ++ suffixStr v.h infinitiveSuffix ;
            stems = \\_ => v.stems ! VPass ;
            aoristType = v.aoristType ;
            h = v.h ;
          } ;
      compl = []
    } ;

}
