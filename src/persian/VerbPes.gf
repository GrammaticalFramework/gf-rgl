concrete VerbPes of Verb = CatPes ** open ResPes,Prelude in {

  flags coding = utf8;
  flags optimize=all_subs ;

  lin
    UseV = predV ;
    SlashV2a v = predVc v;
    Slash2V3 v np = vs v.c3 **
      complSlash (predVc v ** {c2 = v.c2}) np ;
    Slash3V3 v np = vs v.c2 **
      complSlash (predVc v ** {c2 = v.c3}) np ;
    ComplSlash = complSlash ;

    ComplVV = insertVV ;
    ComplVS v s  = embComp (conjThat ++ s.s ! v.compl) (predV v) ;
    ComplVQ v q  = embComp (conjThat ++ q.s)         (predV v) ;
    ComplVA v ap = let adjStr = appComp v.c2 ap.s in
      case ap.afterPrefix of {
        True  => predV (v ** {prefix = v.prefix ++ adjStr}) ;
        False => insertObj adjStr (predV v) -- check form of adjective
      } ;

    SlashVV vv vps = vps ** ComplVV vv vps ;
    SlashV2S v s  = predVc v ** ComplVS v s ;
    SlashV2Q v q  = predVc v ** ComplVQ v q ;
    SlashV2A v ap = predVc v ** insertObj (appComp v.c2 ap.s) (predV v) ; ---- paint it red , check form of adjective

    -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
    SlashV2V v2v vp = predVc v2v ** {
      agrObj = \\agr => if_then_Str v2v.isAux conjThat []
                     ++ showVPH (case v2v.compl of {
                                   Subj  => VSubj Pos agr ;
                                   Indic => VAor Pos agr })
                                agr -- this will agree with the object added by ComplSlash
                                vp ;
      c2 = v2v.c2 ; -- preposition for the direct object comes from V2V
    } ;

    -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
    SlashV2VNP v2v np vps = predVc v2v ** {
      comp = \\a,wo => if_then_Str v2v.isAux conjThat [] -- that
                 ++ appCompVP v2v.c2 np.s ! wo ;         -- I
                                                         -- ∅ is placed in comp
      vComp = \\_,_ => showVPH (case v2v.compl of {      -- buy
                                  Subj  => VSubj Pos np.a ;
                                  Indic => VAor  Pos np.a })
                               np.a -- agreement fixed to np.a
                               <vps : VPH> ;
      c2 = vps.c2 --  preposition for the direct object comes from VPSlash
    } ;

  -- : VP -> Prep -> VPSlash ;
  VPSlashPrep vp prep = vp ** vs prep ;


    AdvVP vp adv = insertAdv adv.s vp ;
    AdVVP adv vp = insertAdV adv.s vp ;
    ReflVP = insertCompPre reflPron ;
    PassV2 = passV ;

    UseComp comp = insertComp comp.s (predV beVerb) ;
    CompAP ap = {s = \\_ => ap.s ! Bare} ; -- check form of adjective
    CompAdv adv = {s = \\_ => adv.s} ;

    -- see https://sites.la.utexas.edu/persian_online_resources/nouns/noun-in-a-predicative-position/
    -- TODO: extend this to all verbs, when NP is indefinite
    CompCN cn = {
      s = \\a => cn.s ! giveNumber a
                      ! case cn.hasAdj of {
                          False => Bare ;
                          True  => Clitic }
      } ;

    CompNP np = {
      s = \\a => np.s ! case np.takesYeAsComp of {
                          False => Bare ;
                          True  => Clitic }
      } ;

}
