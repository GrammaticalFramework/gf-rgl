concrete VerbKaz of Verb = CatKaz ** open ResKaz, ParadigmsKaz in {
  lin
    UseV v = v ; SlashV2a v = v ; ComplSlash v np = prefixVerb (complNP v.c2 np) v ;
    AdvVP vp adv = prefixVerb adv.s vp ; ExtAdvVP vp adv = prefixVerb (adv.s ++ ",") vp ;
    AdVVP adv vp = prefixVerb adv.s vp ;
    AdvVPSlash vp adv = lin VPSlash (prefixVerb adv.s vp ** {c2=vp.c2}) ;
    AdVVPSlash adv vp = lin VPSlash (prefixVerb adv.s vp ** {c2=vp.c2}) ;
    Slash2V3 v np = lin VPSlash (prefixVerb (complNP v.c2 np) v ** {c2=v.c3}) ;
    Slash3V3 v np = lin VPSlash (prefixVerb (complNP v.c3 np) v ** {c2=v.c2}) ;
    SlashV2S v s = lin VPSlash (prefixVerb s.s v ** {c2=v.c2}) ;
    SlashV2Q v s = lin VPSlash (prefixVerb s.s v ** {c2=v.c2}) ;
    SlashV2A v ap = lin VPSlash (prefixVerb ap.s v ** {c2=v.c2}) ;
    ComplVS v s = prefixVerb s.s v ; ComplVQ v s = prefixVerb s.s v ;
    ComplVA v ap = prefixVerb ap.s v ; ComplVV vv vp = prefixVerb vp.infinitive vv ;
    SlashVV vv vp = lin VPSlash (prefixVerb vp.infinitive vv ** {c2=vp.c2}) ;
    SlashV2V v vp = lin VPSlash (prefixVerb vp.infinitive v ** {c2=v.c2}) ;
    SlashV2VNP v np vp = lin VPSlash (prefixVerb (complNP v.c2 np ++ vp.infinitive) v ** {c2=v.c3}) ;
    UseComp comp = prefixVerb comp.s (mkV "болу") ; UseCopula = mkV "болу" ;
    CompAP ap = ap ; CompNP np = {s=np.s ! Nom} ; CompAdv adv = adv ; CompCN cn = {s=cn.s ! Nom ! Sg} ;
    VPSlashPrep vp prep = lin VPSlash (vp ** {c2=prep}) ;
    PassV2 vp = prefixVerb "" vp ; ReflVP vp = vp ;
}
