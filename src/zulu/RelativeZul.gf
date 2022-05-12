concrete RelativeZul of Relative = CatZul ** open ResZul,Prelude,ParamX in {

  flags optimize=all_subs ;

  lin

--     RelCl cl = {
--       s = \\t,a,p,_ => "such" ++ "that" ++ cl.s ! t ! a ! p ! oDir ;
--       c = npNom
--       } ;

    RelVP rp vp = case vp.vptype of {
      CopIdent => rcl_with_id_cop_predicate rp vp ;
      CopAssoc => rcl_with_ass_cop_predicate rp vp ;
      CopEq => rcl_with_eq_cop_predicate rp vp ;
      CopDescr => rcl_with_descr_predicate rp vp ;
      _ => rcl_with_verb_predicate rp vp
    } ;

-- -- Pied piping: "that we are looking at". Pied piping and empty
-- -- relative are defined in $ExtraZul.gf$ ("at which we are looking",
-- -- "we are looking at").
--
--     RelSlash rp slash = {
--       s = \\t,a,p,ag =>
--         rp.s ! RC (fromAgr ag).g NPAcc ++ slash.s ! t ! a ! p ! oDir ++ slash.c2 ;
--       c = NPAcc
--       } ;
--
--     FunRP p np rp = {
--       s = \\c => np.s ! NPAcc ++ p.s ++ rp.s ! RPrep (fromAgr np.a).g ;
--       a = RAg np.a
--       } ;

    IdRP = { s = [] } ;

    oper
      rcl_with_verb_predicate : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
        s = \\a,p,t =>
          let
            vform = VFIndic RelCl p t ;
            vow = case <vp.r,p,t> of {
              <RC,Pos,PresTense> => False ;
              <_,Pos,PresTense> => True ;
              <RC,_,PastTense> => False ;
              <_,_,PastTense> => True ;
              <_,_,_> => False
            } ;
            reqLF = case vp.hasComp of {
              True => False ;
              False => True
            } ;
            relsuf = case vp.hasComp of {
              True => [] ;
              False => relSuf vform
            } ;
            rcform = RelC ; -- case vform_main of {
            --   VFIndic Part Pos PastTense _ => RelCA ;
            --   VFIndic _ _ _ _ => RelC ;
            --   VFPot _ _ _ => RelC ;
            --   VFSubj _ => RelC
            -- } ;
          in
            -- naively only took out the subject
            rp.s
            -- ++ (negPref vform_main)
            -- -- ++ (exclSePref vform_main)
            -- ++ relConc!a!rcform ++BIND
            -- -- ++ (negPref2 vform_main)
            -- -- ++ (exclKaPref vform)
            -- ++ (tensePref vform)
            -- ++ vp.oc
            -- ++ vp.s!(rform vform_main reqLF)
            ++ vp.s!RelCl!a!p!t!reqLF
            -- ++ relsuf
            ++ vp.iadv
            ++ vp.comp
            ++ vp.advs
      } ;

      rcl_with_id_cop_predicate : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
        s = \\a,p,t =>
          let
            vform_main = VFIndic RelCl p t ;
            pcp = relConcLookup!a!vp.r ++BIND ;
            -- cp = id_cop_pref vp.comp_agr ;
            cb = vp.comp ;
          in
            -- naively removed subject
            rp.s ++
            -- pcp ++
            vp.s!RelCl!a!p!t!False ++
            vp.iadv ++
            vp.advs
      } ;

      rcl_with_ass_cop_predicate : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
        s = \\a,p,t =>
          let
            vform_main = VFIndic RelCl p t ;
            pcp = relConcLookup!a!vp.r ++BIND;
            -- cp = (assoc_cop_pref vp.comp_agr) ;
            -- cb = (withPref ! vp.r) ++ BIND ++ vp.comp ;
            -- asp = case vp.asp of {
            --   Prog => progPref vform_main ;
            --   _ => []
            -- } ;
          in
            -- naively removed subject
            rp.s ++
            -- pcp ++
            vp.s!RelCl!a!p!t!False ++
            vp.iadv ++
            vp.advs
      } ;

      rcl_with_eq_cop_predicate : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
        s = \\a,p,t =>
          let
            vform_main = VFIndic RelCl p t ;
            pcp = relConcLookup!a!vp.r ++BIND;
            -- cb = (eqPref ! vp.r) ++ BIND ++ vp.comp ;
          in
            -- naively removed subject
            rp.s ++
            pcp ++
            vp.s!RelCl!a!p!t!False ++
            vp.iadv ++
            vp.advs
      } ;

      rcl_with_descr_predicate : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
      s = \\a,p,t =>
        let
          vform_main = VFIndic RelCl p t ;
          pcp = pre_cop_pref vform_main a ;
          -- adjf = aformN a ;
          -- adjpref =  relAdjAgrLookup!p!a ++BIND ;
          -- comp = vp.ap_comp!adjf ++ vp.comp
        in
          rp.s ++
          -- adjpref ++
          vp.s!RelCl!a!p!t!False
          ++ vp.iadv ++ vp.advs
        } ;

}
