concrete RelativeExtZul of RelativeExt = CatZul ** open ResZul,Prelude,ParamX in {

  flags optimize=all_subs ;

  lin

    RelVPShort rp vp = case vp.vptype of {
      CopIdent => rcl_with_id_cop_predicate_short rp vp ;
      CopAssoc => rcl_with_ass_cop_predicate_short rp vp ;
      CopEq => rcl_with_eq_cop_predicate_short rp vp ;
      CopDescr => rcl_with_descr_predicate_short rp vp ;
      _ => rcl_with_verb_predicate_short rp vp
    } ;

    oper

      rcl_with_verb_predicate_short : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
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
            rcform = RelC ;
          in
            -- naively only took out the subject
            rp.s
            ++ vp.s!RelCl!a!p!t!False
            ++ vp.iadv
            ++ vp.comp
            ++ vp.advs
      } ;

      rcl_with_ass_cop_predicate_short : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
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
            "*" ++
            rp.s ++
            -- pcp ++
            vp.s!RelCl!a!p!t!False ++
            vp.iadv ++
            vp.advs
      } ;

      rcl_with_id_cop_predicate_short : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
        s = \\a,p,t =>
          let
            vform_main = VFIndic RelCl p t ;
            pcp = relConcLookup!a!vp.r ++BIND ;
            -- cp = id_cop_pref vp.comp_agr ;
            cb = vp.comp ;
          in
            -- naively removed subject
            "*" ++
            rp.s ++
            -- pcp ++
            vp.s!RelCl!a!p!t!False ++
            vp.iadv ++
            vp.advs
      } ;

      rcl_with_ass_cop_predicate_short : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
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
            "*" ++
            rp.s ++
            -- pcp ++
            vp.s!RelCl!a!p!t!False ++
            vp.iadv ++
            vp.advs
      } ;

      rcl_with_eq_cop_predicate_short : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
        s = \\a,p,t =>
          let
            vform_main = VFIndic RelCl p t ;
            pcp = relConcLookup!a!vp.r ++BIND;
            -- cb = (eqPref ! vp.r) ++ BIND ++ vp.comp ;
          in
            -- naively removed subject
            "*" ++
            rp.s ++
            pcp ++
            vp.s!RelCl!a!p!t!False ++
            vp.iadv ++
            vp.advs
      } ;

      rcl_with_descr_predicate_short : RP -> VP -> { s : Agr => Polarity => BasicTense => Str } = \rp,vp -> {
      s = \\a,p,t =>
        let
          vform_main = VFIndic RelCl p t ;
          pcp = pre_cop_pref vform_main a ;
          -- adjf = aformN a ;
          -- adjpref =  relAdjAgrLookup!p!a ++BIND ;
          -- comp = vp.ap_comp!adjf ++ vp.comp
        in
          "*" ++
          rp.s ++
          -- adjpref ++
          vp.s!RelCl!a!p!t!False
          ++ vp.iadv ++ vp.advs
        } ;

}
