concrete VerbZul of Verb = CatZul ** open ResZul, Prelude in {

  flags optimize=all_subs ;

  lin
    UseV v = v ** {
      oc = [] ;
      comp = [] ;
      hasComp = False ;
      asp = Null ;
      vptype = NoComp ;
      comp_agr = First Sg ; -- this could be anything...
      ap_comp = \\_ => [] ;
      ap_bool = False ;
      aux_root = [] ;
      hasAux = False
    } ;
--
--     SlashV2a v = predVc v ** {c2 = v.c2 ; gapInMiddle = False} ;
--     Slash2V3 v np =
--       insertObjc (\\_ => v.c2 ++ np.s ! NPAcc) (predVc v ** {c2 = v.c3 ; gapInMiddle = False}) ;
--     Slash3V3 v np =
--       insertObjc (\\_ => v.c3 ++ np.s ! NPAcc) (predVc v) ; ----
--
--     ComplVV v vp = insertObj (\\a => infVP v.typ vp False Simul CPos a) (predVV v) ;  ---- insertExtra?
--     ComplVS v s  = insertExtra (conjThat ++ s.s) (predV v) ;
-- ---    ComplVS v s  = insertObj (variants {\\_ => conjThat ++ s.s; \\_ => s.s}) (predV v) ;
--     ComplVQ v q  = insertExtra (q.s ! QIndir) (predV v) ;
--     ComplVA v ap = insertObj (ap.s) (predV v) ;
--
--     SlashV2V v vp = insertObjc (\\a => v.c3 ++ infVP v.typ vp False Simul CPos a) (predVc v) ;
--     SlashV2S v s  = insertExtrac (conjThat ++ s.s) (predVc v) ;   ---- insertExtra?
-- ---    SlashV2S v s  = insertObjc (variants {\\_ => conjThat ++ s.s; \\_ => s.s}) (predVc v) ;
--     SlashV2Q v q  = insertExtrac (q.s ! QIndir) (predVc v) ;
--     SlashV2A v ap = insertObjc (\\a => v.c3 ++ ap.s ! a) (predVc v) ; ----
--
--     ComplSlash vp np =
--       let vp' = case vp.gapInMiddle of {
--                   True  => insertObjPre (\\_ => vp.c2 ++ np.s ! NPAcc) vp ;
--                   False => insertObj    (\\_ => vp.c2 ++ np.s ! NPAcc) vp } ;
--
--           -- IL 24/04/2018
--           -- If the missing argument is not an adverbial, make previous object
--           -- agree with the argument of ComplSlash.
--           -- Example: "you help /me/ like /myself/", not "*you help me like yourself".
--           -- Different order of ReflVP and ComplSlash produces "you /yourself/ help me like me".
--             f = case vp.missingAdv of {
--                   True => id VP ;
--                   False => objAgr np } ;
--       in f vp' ;
--
--     SlashVV vv vp = vp **
--       insertObj (\\a => infVP vv.typ vp False Simul CPos a) (predVV vv) ;
--     SlashV2VNP vv np vp = vp **
--       insertObjPre (\\_ => vv.c2 ++ np.s ! NPAcc)
--       (insertObjc (\\a => vv.c3 ++ infVP vv.typ vp False Simul CPos a) (predVc vv)) ;

    UseComp comp = case comp.comptype of {
      CopDescr => {
        s = [] ;
        perfSuff = [] ;
        oc = [] ;
        comp = [] ;
        hasComp = True ;
        r = comp.r ; -- adjectives don't typically start on vowels
        syl = SylMult ;
        asp = comp.asp ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ; -- this could be anything...
        ap_comp = comp.s ;
        ap_bool = comp.ap_bool ;
        aux_root = [] ;
        hasAux = False
      } ;
      CopIdent => {
        s = [] ;
        perfSuff = [] ;
        oc = [] ;
        comp = comp.s!A1 ; -- doesn't matter
        hasComp = True ;
        r = comp.r ;
        syl = SylMult ;
        asp = comp.asp ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ;
        ap_comp = \\_ => [] ;
        ap_bool = False ;
        aux_root = [] ;
        hasAux = False
      } ;
      AdvComp => {
        s = [] ;
        perfSuff = [] ;
        oc = [] ;
        comp = comp.s!A1 ;
        hasComp = True ;
        r = comp.r ; -- probably works...
        syl = SylMult ;
        asp = comp.asp ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ;
        ap_comp = \\_ => [] ;
        ap_bool = False ;
        aux_root = [] ;
        hasAux = False
      } ;
      -- the default tries to treat the comp as a NP type
      _ => {
        s = [] ;
        perfSuff = [] ;
        oc = [] ;
        comp = comp.s!A1 ; -- doesn't matter
        hasComp = True ;
        r = comp.r ;
        syl = SylMult ;
        asp = comp.asp ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ;
        ap_comp = \\_ => [] ;
        ap_bool = False ;
        aux_root = [] ;
        hasAux = False
      }
    } ;

--     AdvVP vp adv = insertObj (\\_ => adv.s) vp ;
--     ExtAdvVP vp adv = insertObj (\\_ => frontComma ++ adv.s ++ finalComma) vp ;
--     AdVVP adv vp = insertAdV adv.s vp ;
--
--     AdvVPSlash vp adv = vp ** insertObj (\\_ => adv.s) vp ;
--     AdVVPSlash adv vp = vp ** insertAdV adv.s vp ;
--
--     ReflVP v = insertObjPre (\\a => v.c2 ++ reflPron ! a) v ;
--
--     PassV2 v = insertObj (\\_ => v.s ! VPPart ++ v.p) (predAux auxBe) ;
--
-- ---b    UseVS, UseVQ = \vv -> {s = vv.s ; c2 = [] ; isRefl = vv.isRefl} ; -- no "to"

    CompAP ap = {
      s = ap.s ;
      r = RC ;
      agr = First Sg ; -- this could be anything...
      asp = Null ;
      comptype = CopDescr ;
      ap_bool = ap.b ;
    } ;

    CompNP np = {
      s = \\_ => np.nom ! Full ++ np.desc ;
      r = nominit!np.agr ;
      agr = np.agr ;
      asp = Null ;
      comptype = CopIdent ;
      ap_bool = False
    } ;

    CompAdv adv = {
      s = \\_ => case adv.reqLocS of {
        True => "s" ++BIND ;
        False => []
      } ++ adv.s ;
      r = RC ; -- probably works...
      agr = First Sg ; -- this could be anything...
      asp = adv.asp ;
      comptype = AdvComp ;
      ap_bool = False ;
    } ;

    -- CompCN cn = {s = \\a => case (fromAgr a).n of {
    --   Sg => artIndef ++ cn.s ! Sg ! Nom ;
    --   Pl => cn.s ! Pl ! Nom
    --   }
    -- } ;

--     UseCopula = predAux auxBe ;
--
--     VPSlashPrep vp p = vp ** {c2 = p.s ; gapInMiddle = False; missingAdv = True } ;

}
