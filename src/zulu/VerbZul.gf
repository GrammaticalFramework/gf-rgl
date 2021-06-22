concrete VerbZul of Verb = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin
    UseV v = v ** {
      oc = [] ;
      comp = \\_ => [] ;
      iadv = [] ;
      advs = [] ;
      hasComp = False ;
      asp = Null ;
      asp_pref = \\_ => [] ;
      vptype = NoComp ;
      comp_agr = First Sg ; -- this could be anything...
      ap_comp = \\_ => [] ;
      aux_root = [] ;
      hasAux = False
    } ;

--     ComplVV v vp = insertObj (\\a => infVP v.typ vp False Simul CPos a) (predVV v) ;  ---- insertExtra?

    ComplVS vs s = vs ** {
      -- s = vs.s ;
      oc = [] ;
      comp = \\_ => s.subjs ;
      iadv = [] ;
      advs = [] ;
      hasComp = True ;
      -- r = vs.r ;
      -- syl = vs.syl ;
      asp = Null ;
      asp_pref = \\_ => [] ;
      vptype = VSCompl ;
      comp_agr = First Sg ; -- this could be anything...
      ap_comp = \\_ => [] ;
      aux_root = [] ;
      hasAux = False
    } ;

-- ---    ComplVS v s  = insertObj (variants {\\_ => conjThat ++ s.s; \\_ => s.s}) (predV v) ;
--     ComplVQ v q  = insertExtra (q.s ! QIndir) (predV v) ;

    -- ComplVA va ap = va ** {
    --   -- s = va.s ;
    --   oc = [] ;
    --   comp = \\_ => [] ;
    --   iadv = [] ;
    --   advs = [] ;
    --   hasComp = True ;
    --   -- r = va.r ;
    --   -- syl = va.syl ;
    --   asp = Null ;
    --   asp_pref = \\_ => [] ;
    --   vptype = VACompl ;
    --   comp_agr = First Sg ; -- this could be anything...
    --   ap_comp = ap.s ;
    --   ap_bool = ap.b ;
    --   aux_root = [] ;
    --   hasAux = False
    -- } ;


    SlashV2a v = v ** {
      oc = [] ;
      comp = \\_ => [] ;
      iadv = [] ;
      advs = [] ;
      hasComp = False ;
      asp = Null ;
      asp_pref = \\_ => [] ;
      vptype = VNPCompl ;
      comp_agr = First Sg ; -- this could be anything...
      ap_comp = \\_ => [] ;
      aux_root = [] ;
      hasAux = False ;
      missing_np1 = True
    } ;
--     Slash2V3 v np =
--       insertObjc (\\_ => v.c2 ++ np.s ! NPAcc) (predVc v ** {c2 = v.c3 ; gapInMiddle = False}) ;
--     Slash3V3 v np =
--       insertObjc (\\_ => v.c3 ++ np.s ! NPAcc) (predVc v) ; ----
--     SlashV2V v vp = insertObjc (\\a => v.c3 ++ infVP v.typ vp False Simul CPos a) (predVc v) ;
--     SlashV2S v s  = insertExtrac (conjThat ++ s.s) (predVc v) ;   ---- insertExtra?
-- ---    SlashV2S v s  = insertObjc (variants {\\_ => conjThat ++ s.s; \\_ => s.s}) (predVc v) ;
--     SlashV2Q v q  = insertExtrac (q.s ! QIndir) (predVc v) ;
--     SlashV2A v ap = insertObjc (\\a => v.c3 ++ ap.s ! a) (predVc v) ; ----

    -- ComplSlash vp np = {
    --   s = vp.s ;
    --   perfSuff = vp.perfSuff ;
    --   oc = case np.proDrop of {
    --     True => objConc np.agr v2.r v2.syl ;
    --     False => np.empty
    --   comp = case v2.voice of {
    --     Active => vp.comp ++ np.s ! Full ++ np.desc ;
    --     Passive => vp.comp ++ (cop_pref np.agr) ++BIND++ np.s ! Full ++ np.desc
    --   } ;
    --   hasComp = True ;
    --   r = vp.r ;
    --   syl = vp.syl ;
    --   asp = vp.asp ;
    --   asp_pref = vp.asp_pref ;
    --   vptype = VNPCompl ;
    --   comp_agr = np.agr ;
    --   ap_comp = vp.ap_comp ;
    --   ap_bool = vp.ap_bool ;
    --   aux_root = vp.aux_root ;
    --   hasAux = vp.hasAux
    -- } ;


--     SlashVV vv vp = vp **
--       insertObj (\\a => infVP vv.typ vp False Simul CPos a) (predVV vv) ;
--     SlashV2VNP vv np vp = vp **
--       insertObjPre (\\_ => vv.c2 ++ np.s ! NPAcc)
--       (insertObjc (\\a => vv.c3 ++ infVP vv.typ vp False Simul CPos a) (predVc vv)) ;

    UseComp comp = case comp.comptype of {
      CopDescr => {
        s = \\_ => [] ;
        oc = [] ;
        comp = \\_ => [] ; -- doesn't matter
        iadv = [] ;
        advs = [] ;
        hasComp = True ;
        r = comp.r ; -- adjectives don't typically start on vowels
        syl = SylMult ;
        asp = comp.asp ;
        asp_pref = comp.asp_pref ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ; -- this could be anything...
        ap_comp = comp.s ;
        aux_root = [] ;
        hasAux = False
      } ;
      CopIdent => {
        s = \\_ => [] ;
        oc = [] ;
        comp = \\_ => comp.s!AF1 ; -- doesn't matter
        iadv = [] ;
        advs = [] ;
        hasComp = True ;
        r = comp.r ;
        syl = SylMult ;
        asp = comp.asp ;
        asp_pref = comp.asp_pref ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ;
        ap_comp = \\_ => [] ;
        aux_root = [] ;
        hasAux = False
      } ;
      AdvComp => {
        s = \\_ => [] ;
        oc = [] ;
        comp = \\_ => [] ;
        iadv = [] ;
        advs = comp.s!AF1 ;
        hasComp = True ;
        r = comp.r ; -- probably works...
        syl = SylMult ;
        asp = comp.asp ;
        asp_pref = comp.asp_pref ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ;
        ap_comp = \\_ => [] ;
        aux_root = [] ;
        hasAux = False
      } ;
      -- the default tries to treat the comp as a NP type
      _ => {
        s = \\_ => [] ;
        oc = [] ;
        comp = \\_ => comp.s!AF1 ; -- doesn't matter
        iadv = [] ;
        advs = [] ;
        hasComp = True ;
        r = comp.r ;
        syl = SylMult ;
        asp = comp.asp ;
        asp_pref = comp.asp_pref ;
        vptype = comp.comptype ;
        comp_agr = comp.agr ;
        ap_comp = \\_ => [] ;
        aux_root = [] ;
        hasAux = False
      }
    } ;

    AdvVP vp adv = {
      s = vp.s ;
      oc = vp.oc ;
      comp = vp.comp ;
      iadv = vp.iadv ;
      advs = vp.advs ++ adv.s ;
      hasComp = vp.hasComp ;
      r = vp.r ;
      syl = vp.syl ;
      asp = vp.asp ;
      asp_pref = vp.asp_pref ;
      vptype = vp.vptype ;
      comp_agr = vp.comp_agr ;
      ap_comp = vp.ap_comp ;
      aux_root = vp.aux_root ;
      hasAux = vp.hasAux
    } ;

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
      asp_pref = \\_ => [] ;
      comptype = CopDescr
    } ;

    CompNP np = {
      s = \\_ => np.predet_pre ++ np.s ! Full ++ np.mod ++ np.predet_post ;
      r = initNP np.isPron np.agr ;
      agr = np.agr ;
      asp = Null ;
      asp_pref = \\_ => [] ;
      comptype = CopIdent
    } ;

    CompAdv adv = {
      s = \\_ => case adv.reqLocS of {
        True => "s" ++BIND ;
        False => []
      } ++ adv.s ;
      r = RC ; -- probably works...
      agr = First Sg ; -- this could be anything...
      asp = Null ;
      asp_pref = \\_ => [] ; -- TODO: check
      comptype = AdvComp
    } ;

    -- CompCN cn = {s = \\a => case (fromAgr a).n of {
    --   Sg => artIndef ++ cn.s ! Sg ! Nom ;
    --   Pl => cn.s ! Pl ! Nom
    --   }
    -- } ;

--     UseCopula = predAux auxBe ;
--
--     VPSlashPrep vp p = vp ** {c2 = p.s ; gapInMiddle = False; missingAdv = True } ;

  -- oper
  --   insert_np : VPSlash -> NP -> VP = \vp,np -> {
  --     s = vp.s ;
  --     perfSuff = vp.perfSuff ;
  --     oc = case np.proDrop of {
  --       True => objConc np.agr v2.r v2.syl ;
  --       False => np.empty
  --     } ;
  --     comp = case np.proDrop of {
  --       True => vp.comp ;
  --       False => vp.comp ++ np.s ! Full ++ np.desc
  --     } ;
  --     hasComp = True ;
  --     r = vp.r ;
  --     syl = vp.syl ;
  --     asp = vp.asp ;
  --     asp_pref = vp.asp_pref ;
  --     vptype = VNPCompl ;
  --     comp_agr = np.agr ; -- this could be anything...
  --     ap_comp = vp.ap_comp ;
  --     ap_bool = vp.ap_bool ;
  --     aux_root = vp.aux_root ;
  --     hasAux = vp.hasAux
  --   } ;

}
