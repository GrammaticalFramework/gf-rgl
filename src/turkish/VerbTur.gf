concrete VerbTur of Verb = CatTur ** open Prelude, ResTur, SuffixTur, HarmonyTur in {

  lin
    UseV v = {s = mkVerbForms v; compl = []} ;
    SlashV2a v = v ** {compl = []} ;

    Slash2V3 v np = v ** {
      compl = np.s ! v.c1.c ++ v.c1.s ;
      c = v.c2
    } ;
    Slash3V3 v np = v ** {
      compl = np.s ! v.c2.c ++ v.c2.s ;
      c = v.c1
    } ;
    SlashV2A v ap = v ** {
      compl = ap.s ! Sg ! Nom ;
      c = v.c
    } ;
    SlashV2V v vp = v ** {
      compl = vp.compl ++ vp.s ! Perf ! VInf Pos ;
      c = v.c
    } ;
    SlashV2S v s = v ** {compl = s.s ; c = v.c} ;
    SlashV2Q v q = v ** {compl = q.s ; c = v.c} ;
    SlashVV v vp = v ** {
      compl = vp.compl ++ mkVerbForms vp ! Perf ! VInf Pos ;
      c = vp.c
    } ;
    SlashV2VNP v np vp = v ** {
      compl = np.s ! v.c.c ++ v.c.s ++ vp.compl ;
      c = vp.c
    } ;

    ComplSlash vps np = {
      s     = mkVerbForms vps ;
      compl = vps.compl ++ np.s ! vps.c.c ++ vps.c.s ;
    } ;

    ComplVS vs s = {s = mkVerbForms vs ; compl = s.s} ;

    ComplVA va ap = {
      s = mkVerbForms va ;
      compl = ap.s ! Sg ! Nom
    } ;
    ComplVV vv vp = {
      s = mkVerbForms vv ;
      compl = vp.compl ++ vp.s ! Perf ! VInf Pos
    } ;
    ComplVQ vq q = {s = mkVerbForms vq ; compl = q.s} ;
    
    UseComp comp = comp ** {compl = []} ;
    CompCN cn = CompNP {
      s = cn.s ! Sg;
      h = cn.h;
      a = agrP3 Sg
    } ;

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
            <_,VImp p n>    => ap.s ! Sg ! Nom ++
                               mkVerbForms olmak_V ! asp ! (VImp p n) ;
            <Perf,VFin Pres Simul p agr> =>
                               ap.s ! Sg ! Nom ++
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
                               ap.s ! Sg ! Nom ++ BIND ++
                               case p of {
                                 Pos => [] ;
                                 Neg => suffixStr ap.h negativeSuffix
                               } +
                               suffixStr ap.h (alethicCopulaSuffixes ! agr) ;
            <_,VFin t a p agr> =>
                               ap.s ! Sg ! Nom ++
                               mkVerbForms olmak_V ! asp ! vform ;
            _               => ap.s ! Sg ! Nom ++
                               mkVerbForms olmak_V ! asp ! vform
          } ;
      compl = []
    } ;

    CompAdv adv = {
      s = \\asp,vform => adv.s ++ mkVerbForms olmak_V ! asp ! vform ;
      compl = []
    } ;

    ReflVP vps = {
      s = mkVerbForms vps ;
      compl = vps.compl ++ "kendini" ++ vps.c.s
    } ;

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

    VPSlashPrep vp prep = {
      s = olmak_V.s;
      stems = olmak_V.stems;
      aoristType = olmak_V.aoristType;
      h = olmak_V.h;
      compl = vp.compl ++ vp.s ! Perf ! VInf Pos;
      c = prep
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
