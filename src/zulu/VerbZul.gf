concrete VerbZul of Verb = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin
    UseV v = {
      s = table {
        MainCl => \\a,p,t,l => let
          vform = VFIndic MainCl p t ;
          tp = tensePref vform v.r v.syl ; -- [] / zo- / zuku-
          r = v.s!(rform (VFIndic MainCl p t) l) -- hamba
          -- rest of verb prefix built later (eg no "ya" with certain question words)
        in tp ++ r ;
        RelCl => \\a,p,t,l => let
          vform = VFIndic RelCl p t ;
          rc = relConc vform a v.r ; -- o-
          tp = tensePref vform v.r v.syl ; -- [] / zo- / zuku-
          r = v.s!(rform vform l) ; -- hamba
          suf = case l of {
            True => relSuf vform ;
            False => []
          } ;
        in rc ++ tp ++ r ++ suf
      } ;
      iadv, advs, comp = [] ;
      -- ap_comp = \\_ => [] ;
      hasComp = False ;
      r = v.r ;
      syl = v.syl ;
      vptype = NoComp
    } ;

--     ComplVV v vp = insertObj (\\a => infVP v.typ vp False Simul CPos a) (predVV v) ;  ---- insertExtra?

    -- ComplVS vs s = vs ** {
    --   -- s = vs.s ;
    --   oc = [] ;
    --   comp = s.subjs ;
    --   iadv = [] ;
    --   advs = [] ;
    --   hasComp = True ;
    --   -- r = vs.r ;
    --   -- syl = vs.syl ;
    --   asp = Null ;
    --   asp_pref = \\_ => [] ;
    --   vptype = VSCompl ;
    --   comp_agr = First Sg ; -- this could be anything...
    --   ap_comp = \\_ => [] ;
    --   aux_root = [] ;
    --   hasAux = False
    -- } ;

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


    -- SlashV2a v = v ** {
    --   oc = [] ;
    --   comp = [] ;
    --   -- iadv = [] ;
    --   -- advs = [] ;
    --   hasComp = False ;
    --   asp = Null ;
    --   asp_pref = \\_ => [] ;
    --   vptype = VNPCompl ;
    --   comp_agr = First Sg ; -- this could be anything...
    --   ap_comp = \\_ => [] ;
    --   aux_root = [] ;
    --   hasAux = False -- ;
    --   -- missing_np1 = True
    -- } ;
--     Slash2V3 v np =
--       insertObjc (\\_ => v.c2 ++ np.s ! NPAcc) (predVc v ** {c2 = v.c3 ; gapInMiddle = False}) ;
--     Slash3V3 v np =
--       insertObjc (\\_ => v.c3 ++ np.s ! NPAcc) (predVc v) ; ----
--     SlashV2V v vp = insertObjc (\\a => v.c3 ++ infVP v.typ vp False Simul CPos a) (predVc v) ;
--     SlashV2S v s  = insertExtrac (conjThat ++ s.s) (predVc v) ;   ---- insertExtra?
-- ---    SlashV2S v s  = insertObjc (variants {\\_ => conjThat ++ s.s; \\_ => s.s}) (predVc v) ;
--     SlashV2Q v q  = insertExtrac (q.s ! QIndir) (predVc v) ;
--     SlashV2A v ap = insertObjc (\\a => v.c3 ++ ap.s ! a) (predVc v) ; ----

    -- TODO: this simply adds the new np to the end of vp.comp; to be expanded beyond V2 using missing_np1
    -- ComplSlash vp np = {
    --   oc = case np.proDrop of {
    --     True => objConc np.agr vp.r vp.syl ;
    --     False => np.empty
    --   } ;
    --   comp = np.s!Full ;
    --   iadv = [] ;
    --   advs = [] ;
    --   hasComp = case np.proDrop of {
    --     True => False ;
    --     False => True
    --   } ;
    --   s = vp.s ;
    --   r = vp.r ;
    --   syl = vp.syl ;
    --   asp = vp.asp ;
    --   asp_pref = vp.asp_pref ;
    --   vptype = VNPCompl ;
    --   comp_agr = np.agr ;
    --   ap_comp = vp.ap_comp ;
    --   aux_root = vp.aux_root ;
    --   hasAux = vp.hasAux
    -- } ;

    -- UseComp comp = case comp.comptype of {
    --   CopDescr => {
    --     s = \\_ => [] ;
    --     oc = [] ;
    --     comp = [] ; -- doesn't matter
    --     iadv = [] ;
    --     advs = [] ;
    --     hasComp = True ;
    --     r = comp.r ; -- adjectives don't typically start on vowels
    --     syl = SylMult ;
    --     asp = comp.asp ;
    --     asp_pref = comp.asp_pref ;
    --     vptype = comp.comptype ;
    --     comp_agr = comp.agr ; -- this could be anything...
    --     ap_comp = comp.s ;
    --     aux_root = [] ;
    --     hasAux = False
    --   } ;
    --   CopIdent => {
    --     s = \\_ => [] ;
    --     oc = [] ;
    --     comp = comp.s!AF1 ; -- doesn't matter
    --     iadv = [] ;
    --     advs = [] ;
    --     hasComp = True ;
    --     r = comp.r ;
    --     syl = SylMult ;
    --     asp = comp.asp ;
    --     asp_pref = comp.asp_pref ;
    --     vptype = comp.comptype ;
    --     comp_agr = comp.agr ;
    --     ap_comp = \\_ => [] ;
    --     aux_root = [] ;
    --     hasAux = False
    --   } ;
    --   AdvComp => {
    --     s = \\_ => [] ;
    --     oc = [] ;
    --     comp = [] ;
    --     iadv = [] ;
    --     advs = comp.s!AF1 ;
    --     hasComp = True ;
    --     r = comp.r ; -- probably works...
    --     syl = SylMult ;
    --     asp = comp.asp ;
    --     asp_pref = comp.asp_pref ;
    --     vptype = comp.comptype ;
    --     comp_agr = comp.agr ;
    --     ap_comp = \\_ => [] ;
    --     aux_root = [] ;
    --     hasAux = False
    --   } ;
    --   -- the default tries to treat the comp as a NP type
    --   _ => {
    --     s = \\_ => [] ;
    --     oc = [] ;
    --     comp = comp.s!AF1 ; -- doesn't matter
    --     iadv = [] ;
    --     advs = [] ;
    --     hasComp = True ;
    --     r = comp.r ;
    --     syl = SylMult ;
    --     asp = comp.asp ;
    --     asp_pref = comp.asp_pref ;
    --     vptype = comp.comptype ;
    --     comp_agr = comp.agr ;
    --     ap_comp = \\_ => [] ;
    --     aux_root = [] ;
    --     hasAux = False
    --   }
    -- } ;

    AdvVP vp adv = vp ** { advs = vp.advs ++ adv.s ; hasComp = True } ;
    -- {
    --   s = vp.s ;
    --   oc = vp.oc ;
    --   comp = vp.comp ;
    --   iadv = vp.iadv ;
    --   advs = vp.advs ++ adv.s ;
    --   hasComp = vp.hasComp ;
    --   r = vp.r ;
    --   syl = vp.syl ;
    --   asp = vp.asp ;
    --   asp_pref = vp.asp_pref ;
    --   vptype = vp.vptype ;
    --   comp_agr = vp.comp_agr ;
    --   ap_comp = vp.ap_comp ;
    --   aux_root = vp.aux_root ;
    --   hasAux = vp.hasAux
    -- } ;

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

    -- CompAP ap = {
    --   s = ap.s ;
    --   r = RC ;
    --   agr = First Sg ; -- this could be anything...
    --   asp = Null ;
    --   asp_pref = \\_ => [] ;
    --   comptype = CopDescr
    -- } ;
    --
    -- CompNP np = {
    --   s = \\nform => np.s!nform ; -- TODO: refactor
    --   r = initNP np.isPron np.agr ;
    --   agr = np.agr ;
    --   asp = Null ;
    --   asp_pref = \\_ => [] ;
    --   comptype = CopIdent
    -- } ;
    --
    -- CompAdv adv = {
    --   s = \\_ => case adv.reqLocS of {
    --     True => "s" ++BIND ;
    --     False => []
    --   } ++ adv.s ;
    --   r = RC ; -- probably works...
    --   agr = First Sg ; -- this could be anything...
    --   asp = Null ;
    --   asp_pref = \\_ => [] ; -- TODO: check
    --   comptype = AdvComp
    -- } ;

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

  oper
    v_prefix : RInit -> Bool -> Agr -> Polarity -> BasicTense -> Str = \r,c,a,p,t ->
    let
      vow = case <r,p,t> of {
        <RC,Pos,PresTense> => False ;
        <_,Pos,PresTense> => True ;
        <_,_,_> => False
      } ;
      lfya = case <c,p,t> of {
        <False,Pos,PresTense> => "ya" ++BIND ;
        <_,_,_> => []
      } ;
      vform_main = VFIndic MainCl p t
    in
      (negPref vform_main)
      -- ++ (exclSePref vform_main)
      ++ (subjConc vform_main a vow)
      -- ++ (negPref2 vform_main)
      ++ lfya
      -- ++ (tensePref vform_main)
      ;
}
