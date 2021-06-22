concrete SentenceZul of Sentence = CatZul ** open Prelude,ResZul,ParamX in {

  flags optimize=all_subs ;

  lin

    PredVP np vp = case vp.vptype of {
      CopIdent => cl_with_id_cop_predicate np vp ;
      CopAssoc => cl_with_ass_cop_predicate np vp ;
      CopDescr => cl_with_descr_cop_predicate np vp ;
      CopEq => cl_with_eq_cop_predicate np vp ;
      -- VACompl => cl_with_ap_comp_predicate np vp ;
      AdvComp => cl_with_adv_comp_predicate np vp ;
      _ => cl_with_verb_predicate np vp
    } ;

--     PredSCVP sc vp = mkClause sc.s (agrP3 Sg) vp ;

    ImpVP vp = let
      np = {
        empty,predet_pre,predet_post,mod = [] ;
        s = table {Full|Reduced|Poss|Loc => []} ;
        -- loc = [] ;
        -- desc = [] ;
        det = [] ;
        agr = Second Sg ;
        proDrop = True ;
        isPron = True ;
        reqLocS = True ;
        qdef = Article Def
      } ;
      impTense = Absolute PresTense
    in case vp.vptype of {
      VNPCompl => {
        s = table {
          Pos => vp.s!R_a ++ vp.comp!Pos ++ vp.advs ;
          Neg => "unga" ++ vp.s!R_i ++ vp.comp!Neg ++ vp.advs
        }
      } ;

      CopIdent => {s = \\pol => (cl_with_id_cop_predicate np vp).s!pol!impTense!Princ } ;
      CopAssoc => {s = \\pol => (cl_with_ass_cop_predicate np vp).s!pol!impTense!Princ } ;
      CopDescr => {s = \\pol => (cl_with_descr_cop_predicate np vp).s!pol!impTense!Princ } ;
      CopEq => {s = \\pol => (cl_with_eq_cop_predicate np vp).s!pol!impTense!Princ } ;
      -- VACompl => {s = \\pol => (cl_with_ap_comp_predicate np vp).s!pol!impTense!Princ } ;
      AdvComp => {s = \\pol => (cl_with_adv_comp_predicate np vp).s!pol!impTense!Princ } ;
      _ => {s = \\pol => (cl_with_verb_predicate np vp).s!pol!impTense!Princ }
    } ;

--     SlashVP np vp =
--       mkClause (np.s ! npNom) np.a vp ** {c2 = vp.c2} ;
--
--     AdvSlash slash adv = {
--       s  = \\t,a,b,o => slash.s ! t ! a ! b ! o ++ adv.s ;
--       c2 = slash.c2
--     } ;
--
--     SlashPrep cl prep = cl ** {c2 = prep.s} ;
--
--     SlashVS np vs slash =
--       mkClause (np.s ! npNom) np.a
--         (insertObj (\\_ => conjThat ++ slash.s) (predV vs))  **
--         {c2 = slash.c2} ;
--
--     EmbedS  s  = {s = conjThat ++ s.s} ;
--     EmbedQS qs = {s = qs.s ! QIndir} ;
--     EmbedVP vp = {s = infVP VVInf vp False Simul CPos (agrP3 Sg)} ;

    UseCl t p cl = {
      s = \\dm => t.s ++ p.s ++ cl.s ! p.p ! t.t ! dm  ;
      subjs = t.s ++ p.s ++ cl.subjcl ! p.p ! t.t ;
      pots = \\dm => t.s ++ p.s ++ cl.potcl ! p.p ! dm
    } ;
    UseQCl t p cl = {
      s = t.s ++ p.s ++ cl.s ! p.p ! t.t ! Princ ;
      -- potqs = t.s ++ p.s ++ cl.potqcl ! p.p ! Princ ;
      qword_pre = cl.qword_pre ;
      qword_post = cl.qword_post
    } ;
    UseRCl temp pol rcl = {
      s = \\a => temp.s ++ pol.s ++ rcl.s!pol.p!temp.t!a ;
    } ;
--     UseSlash t p cl = {
--       s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! ctr p.p  ! oDir ;
--       c2 = cl.c2
--     } ;
--
--     AdvS a s = {s = a.s ++ s.s} ;
--     ExtAdvS a s = {s = a.s ++ frontComma ++ s.s} ;
--
--     SSubjS a s b = {s = a.s ++ frontComma ++ s.s ++ b.s} ;
--
--     RelS s r = {s = s.s ++ frontComma ++ r.s ! agrP3 Sg} ;
--
--   oper
--     ctr : CPolarity -> CPolarity = \x -> x ;
-- ---    ctr = contrNeg True ;  -- contracted negations

  oper

    -- NOTE: removing everything related to auxiliaries and compound tenses

    subjNP : NP -> Str = \np ->
    np.predet_pre ++
    case <np.qdef,np.isPron> of {
      <Article d,_> => np.s ! Full ++ np.mod ;
      <Demonstrative d,False> => dem_pron!d!np.agr ++ np.s ! Reduced ++ np.mod ;
      <Demonstrative d,True> => dem_pron!d!np.agr ++ np.s ! Full ++ np.mod 
    }
    ++ np.predet_post ;

    cl_with_verb_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      -- advs = vp.advs ;
      s = \\p,t,dm =>
        let
          subj = subjNP np ;
          aux_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b1
          } ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = case t of {
            Absolute bt => VFIndic dm p bt vp.asp ;
            Relative b1 b2 => VFIndic Part p b2 vp.asp
          } ;
          aux = case t of {
            Absolute bt => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => []
            } ;
            Relative _ _ => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr
            }
          } ;
          vow = case <vp.r,p,main_tense> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          -- subjconc = case t of {
          --   Absolute bt => subjConc vform_main np.agr vow ;
          --   Relative PerfTense _ => subjConcLookup!np.agr!ResZul.SCBe ;
          --   Relative _ _ => subjConc vform_main np.agr vow
          -- } ;
          lfya = case <vp.hasComp,p,t> of {
            <False,Pos,Absolute PresTense> => "ya" ++BIND ;
            <_,_,_> => []
          } ;
          reqLF = case vp.hasComp of {
            True => False ;
            False => True
          } ;
        in
          subj
          ++ (verb_prefix vp p t dm np.agr)
          ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main reqLF vp.perfSuff vp.suff)
          ++ vp.s!(rform vform_main reqLF)
          ++ vp.iadv
          ++ vp.comp!p
          ++ vp.advs ;
      subjcl = \\p,t =>
        let
          subj = subjNP np ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_main = VFSubj p ;
          vow = case <vp.r,p,main_tense> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          }
        in
          subj
          -- ++ aux
          ++ (subjConc vform_main np.agr vow)
          ++ (negPref2 vform_main)
          ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main False vp.perfSuff vp.suff)
          ++ vp.s!(rform vform_main False)
          ++ vp.iadv
          ++ vp.comp!p
          ++ vp.advs ;
      potcl = \\p,dm =>
        let
          subj = subjNP np ;
          vform_main = VFPot dm p vp.asp ;
        in
          subj
          ++ (subjConc vform_main np.agr False)
          ++ (potPref vform_main)
          ++ (negPref2 vform_main)
          ++ vp.asp_pref!vform_main
          ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main False vp.perfSuff vp.suff)
          ++ vp.s!(rform vform_main False)
          ++ vp.iadv
          ++ vp.comp!p
          ++ vp.advs
    } ;

    verb_prefix : VP -> Polarity -> ZTense -> DMood -> Agr -> Str = \vp,p,t,dm,agr -> case t of {
      Absolute bt => let
        aux = case vp.hasAux of {
          True => (subjConcLookup!agr!SC) ++BIND++ vp.aux_root ;
          False => []
        } ;
        vow = case <vp.r,p,bt> of {
          <RC,Pos,PresTense> => False ;
          <RC,Pos,PerfTense> => False ;
          <_,Pos,PresTense> => True ;
          <_,Pos,PerfTense> => True ;
          <_,_,_> => False
        } ;
        lfya = case <vp.hasComp,p,t> of {
          <False,Pos,Absolute PresTense> => "ya" ++BIND ;
          <_,_,_> => []
        } ;
        vform_main = VFIndic dm p bt vp.asp
      in
       aux
       ++ (negPref vform_main agr)
       ++ (exclSePref vform_main)
       ++ (subjConc vform_main agr vow)
       ++ (negPref2 vform_main)
       ++ lfya
       ++ vp.asp_pref!vform_main
       ++ (tensePref vform_main) ;
      Relative b1 b2 => case <b1,vp.hasAux> of {
        <PerfTense,False> => let
          vform_aux = VFIndic dm p b1 vp.asp ;
          vform_main = VFIndic Part p b2 vp.asp ;
          aux_sc = aux_be vform_aux agr
        in case agr of {
          -- ( Second Sg |
          --   Third C1_2 Sg |
          --   Third C3_4 _ |
          --   Third C5_6 Pl |
          --   Third C9_6 _ |
          --   Third C9_10 Sg) =>
          --     vp.asp_pref!vform_main -- almost definitely wrong; to block infinite application of aspect-changing functions on VP
          --     ++ aux_sc
          --     ++ (negPrefNga vform_main) ;
          (Third _ _ | First _ | Second _) =>
            vp.asp_pref!vform_main ++ -- almost definitely wrong; to block infinite application of aspect-changing functions on VP
            aux_sc
            ++ (negPrefNga vform_main)
        } ;
        <_,True> => let
          aux = (subjConcLookup!agr!SC) ++BIND++ vp.aux_root ; -- TODO: no tense yet!
          vow = case <vp.r,p,b2> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          vform_main = VFIndic Part p b2 vp.asp
        in
          aux
          ++ (negPref vform_main agr)
          ++ (exclSePref vform_main)
          ++ (subjConc vform_main agr vow)
          ++ (negPref2 vform_main)
          ++ vp.asp_pref!vform_main
          ++ (tensePref vform_main) ;
        <_,False> => let
          vform_aux = VFIndic dm p b1 vp.asp ;
          aux = aux_be vform_aux agr ;
          vow = case <vp.r,p,b2> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          vform_main = VFIndic Part p b2 vp.asp
        in
          aux
          ++ (negPref vform_main agr)
          ++ (exclSePref vform_main)
          ++ (subjConc vform_main agr vow)
          ++ (negPref2 vform_main)
          ++ vp.asp_pref!vform_main
          ++ (tensePref vform_main)
      }
    } ;

    -- become green
    -- cl_with_ap_comp_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
    --   -- advs = vp.advs ;
    --   s = \\p,t,dm =>
    --     let
    --       subj = case np.proDrop of {
    --         True => np.empty ++ np.desc ;
    --         False => np.s ! Full ++ np.desc
    --       } ;
    --       aux_tense = case t of {
    --         Absolute bt => bt ;
    --         Relative b1 b2 => b1
    --       } ;
    --       main_tense = case t of {
    --         Absolute bt => bt ;
    --         Relative b1 b2 => b2
    --       } ;
    --       vform_aux = VFIndic dm p aux_tense vp.asp ;
    --       vform_main = case t of {
    --         Absolute bt => VFIndic dm p bt vp.asp ;
    --         Relative b1 b2 => VFIndic Part p b2 vp.asp
    --       } ;
    --       aux = case t of {
    --         Absolute bt => case vp.hasAux of {
    --           True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
    --           False => aux_be vform_aux np.agr
    --         } ;
    --         Relative _ _ => case vp.hasAux of {
    --           True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
    --           False => aux_be vform_aux np.agr -- relSubjConc aux_tense np.agr --
    --         }
    --       } ;
    --       vow = case <vp.r,p,main_tense> of {
    --         <RC,Pos,PresTense> => False ;
    --         <RC,Pos,PerfTense> => False ;
    --         <_,Pos,PresTense> => True ;
    --         <_,Pos,PerfTense> => True ;
    --         <_,_,_> => False
    --       } ;
    --       lfya = [] ;
    --       reqLF = False ;
    --       adjf = case vp.ap_bool of {
    --         True => (aformN np.agr) ;
    --         -- True => AF2 ;
    --         False => AF1
    --       } ;
    --       comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf -- ++ vp.comp
    --     in
    --       subj
    --       ++ aux
    --       ++ (negPref vform_main np.agr)
    --       ++ (exclSePref vform_main)
    --       ++ (subjConc vform_main np.agr vow)
    --       ++ (negPref2 vform_main)
    --       ++ lfya
    --       ++ vp.asp_pref!vform_main
    --       -- ++ (exclKaPref vform_main)
    --       ++ (tensePref vform_main)
    --       -- ++ vp.s ++ BIND
    --       -- ++ (vtermSuff vform_main False vp.perfSuff vp.suff)
    --       ++ vp.s ! (rform vform_main False)
    --       ++ vp.iadv
    --       ++ comp
    --       ++ vp.comp!p
    --       ++ vp.advs ;
    --   subjcl = \\p,t =>
    --     let
    --       subj = case np.proDrop of {
    --         True => np.empty ++ np.desc ;
    --         False => np.s ! Full ++ np.desc
    --       } ;
    --       main_tense = case t of {
    --         Absolute bt => bt ;
    --         Relative b1 b2 => b2
    --       } ;
    --       vform_main = VFSubj p ;
    --       vow = case <vp.r,p,main_tense> of {
    --         <RC,Pos,PresTense> => False ;
    --         <RC,Pos,PerfTense> => False ;
    --         <_,Pos,PresTense> => True ;
    --         <_,Pos,PerfTense> => True ;
    --         <_,_,_> => False
    --       } ;
    --       adjf = case vp.ap_bool of {
    --         True => (aformN np.agr) ;
    --         -- True => AF2 ;
    --         False => AF1
    --       } ;
    --       comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf -- ++ vp.comp
    --     in
    --       subj
    --       -- ++ aux
    --       ++ (subjConc vform_main np.agr vow)
    --       ++ (negPref2 vform_main)
    --       ++ vp.asp_pref!vform_main
    --       -- ++ vp.s ++ BIND
    --       -- ++ (vtermSuff vform_main False vp.perfSuff vp.suff)
    --       ++ vp.s ! (rform vform_main False)
    --       ++ vp.iadv
    --       ++ comp
    --       ++ vp.comp!p
    --       ++ vp.advs ;
    --   potcl = \\p,dm =>
    --     let
    --       subj = case np.proDrop of {
    --         True => np.empty ++ np.desc ;
    --         False => np.s ! Full ++ np.desc
    --       } ;
    --       vform_main = VFPot dm p vp.asp ;
    --       adjf = case vp.ap_bool of {
    --         True => (aformN np.agr) ;
    --         -- True => AF2 ;
    --         False => AF1
    --       } ;
    --       comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf -- ++ vp.comp
    --     in
    --       subj
    --       ++ (subjConc vform_main np.agr False)
    --       ++ (potPref vform_main)
    --       ++ (negPref2 vform_main)
    --       ++ vp.asp_pref!vform_main
    --       -- ++ vp.s ++ BIND
    --       -- ++ (vtermSuff vform_main False vp.perfSuff vp.suff)
    --       ++ vp.s!(rform vform_main False)
    --       ++ vp.iadv
    --       ++ comp
    --
    --       ++ vp.advs
    -- } ;

    cl_with_id_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      -- advs = vp.advs ;
      s = \\p,t,dm =>
        let
          aux_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b1
          } ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = case t of {
            Absolute bt => VFIndic dm p bt vp.asp ;
            Relative b1 b2 => VFIndic Part p b2 vp.asp
          } ;
          aux = case t of {
            Absolute bt => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr
            } ;
            Relative _ _ => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr -- relSubjConc aux_tense np.agr --
            }
          } ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = id_cop_pref vp.comp_agr ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
          cb = vp.comp!p
        in
          subj ++
          aux ++
          pcp ++
          cp ++ BIND ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = id_cop_pref vp.comp_agr ;
          cb = vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          cp ++ BIND ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = id_cop_pref vp.comp_agr ;
          cb = vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          cp ++ BIND ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs
    } ;

    -- TODO: aspect
    cl_with_ass_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      -- advs = vp.advs ;
      s = \\p,t,dm =>
        let
          aux_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b1
          } ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = case t of {
            Absolute bt => VFIndic dm p bt vp.asp ;
            Relative b1 b2 => VFIndic Part p b2 vp.asp
          } ;
          vform_cop = vform_aux ; -- this might depend on vp.hasAux
          aux = case t of {
            Absolute bt => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr
            } ;
            Relative _ _ => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False "e") -- relSubjConc aux_tense np.agr --
            }
          } ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_cop np.agr ;
          cp = (assoc_cop_pref vp.comp_agr)!p ;
          cb = (withPref ! vp.r) ++ BIND ++ vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          aux ++
          pcp ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = (assoc_cop_pref vp.comp_agr)!p ;
          cb = (withPref ! vp.r) ++ BIND ++ vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = (assoc_cop_pref vp.comp_agr)!p ;
          cb = (withPref ! vp.r) ++ BIND ++ vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs
    } ;

    -- TODO: aspect
    cl_with_eq_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      -- advs = vp.advs ;
      s = \\p,t,dm =>
        let
          aux_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b1
          } ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = case t of {
            Absolute bt => VFIndic dm p bt vp.asp ;
            Relative b1 b2 => VFIndic Part p b2 vp.asp
          } ;
          aux = case t of {
            Absolute bt => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr
            } ;
            Relative _ _ => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr -- relSubjConc aux_tense np.agr --
            }
          } ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          -- cp = id_cop_pref vp.comp_agr ;
          cb = (eqPref ! vp.r) ++ BIND ++ vp.comp!p ;
        in
          subj ++
          aux ++
          pcp ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          -- cp = id_cop_pref vp.comp_agr ;
          cb = (eqPref ! vp.r) ++ BIND ++ vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          -- cp = id_cop_pref vp.comp_agr ;
          cb = (eqPref ! vp.r) ++ BIND ++ vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          vp.asp_pref!vform_main ++
          cb
          ++ vp.iadv ++ vp.advs
    } ;

    -- is green
    cl_with_descr_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      -- advs = vp.advs ;
      s = \\p,t,dm =>
        let
          aux_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b1
          } ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = case t of {
            Absolute bt => VFIndic dm p bt vp.asp ;
            Relative b1 b2 => VFIndic Part p b2 vp.asp
          } ;
          aux = case t of {
            Absolute bt => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr
            } ;
            Relative _ _ => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr -- relSubjConc aux_tense np.agr --
            }
          } ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          adjf = aformN np.agr ;
          adjpref =  adjPrefLookup!np.agr ++BIND ;
          -- asp = case vp.asp of {
          --   Prog => "se" ++BIND ; -- p339
          --   _ => []
          -- } ;
          comp = vp.ap_comp!adjf ++ vp.comp!p
        in
          subj ++
          aux ++
          pcp ++
          adjpref ++
          vp.asp_pref!vform_main ++
          comp
          ++ vp.iadv ++ vp.advs ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          adjf = aformN np.agr ;
          comp =  adjPrefLookup!np.agr ++BIND++ vp.ap_comp!adjf ++ vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          vp.asp_pref!vform_main ++
          comp
          ++ vp.iadv ++ vp.advs ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = subjNP np ;
          pcp = pre_cop_pref vform_main np.agr ;
          adjf = aformN np.agr ;
          comp =  adjPrefLookup!np.agr ++BIND++ vp.ap_comp!adjf ++ vp.comp!p ;
          -- asp = case vp.asp of {
          --   Prog => progPref vform_main ;
          --   _ => []
          -- } ;
        in
          subj ++
          pcp ++
          vp.asp_pref!vform_main ++
          comp
          ++ vp.iadv ++ vp.advs
    } ;

    cl_with_adv_comp_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      -- advs = vp.advs ;
      s = \\p,t,dm =>
        let
          subj = subjNP np ;
          aux_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b1
          } ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = case t of {
            Absolute bt => VFIndic dm p bt vp.asp ;
            Relative b1 b2 => VFIndic Part p b2 vp.asp
          } ;
          aux = case t of {
            Absolute bt => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr
            } ;
            Relative _ _ => case vp.hasAux of {
              True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
              False => aux_be vform_aux np.agr -- relSubjConc aux_tense np.agr --
            }
          } ;
          vow = case <vp.r,p,main_tense> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          lfya = case <vp.hasComp,p,main_tense> of {
            -- <False,Pos,Absolute PresTense> => "ya" ++BIND ;
            <False,Pos,PresTense> => "ya" ++BIND ;
            <_,_,_> => []
          } ;
          reqLF = case vp.hasComp of {
            True => False ;
            False => True
          }
        in
          subj
          ++ aux
          ++ (negPref vform_main np.agr)
          ++ (exclSePref vform_main)
          ++ (subjConc vform_main np.agr vow)
          ++ (negPref2 vform_main)
          ++ lfya
          ++ vp.asp_pref!vform_main
          -- ++ (exclKaPref vform_main)
          ++ (tensePref vform_main)
          -- ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main reqLF vp.perfSuff)
          ++ vp.comp!p
          ++ vp.iadv ++ vp.advs ;
      subjcl = \\p,t =>
        let
          subj = subjNP np ;
          main_tense = case t of {
            Absolute bt => bt ;
            Relative b1 b2 => b2
          } ;
          vform_main = VFSubj p ;
          vow = case <vp.r,p,main_tense> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          }
        in
          subj
          ++ (subjConc vform_main np.agr vow)
          ++ (negPref2 vform_main)
          -- ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main False vp.perfSuff)
          ++ vp.asp_pref!vform_main
          ++ vp.comp!p
          ++ vp.iadv ++ vp.advs ;
      potcl = \\p,dm =>
        let
          subj = subjNP np ;
          vform_main = VFPot dm p vp.asp ;
        in
          subj
          ++ (subjConc vform_main np.agr False)
          ++ (potPref vform_main)
          ++ (negPref2 vform_main)
          -- ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main False vp.perfSuff)
          ++ vp.asp_pref!vform_main
          ++ vp.comp!p
          ++ vp.iadv ++ vp.advs
    } ;

}
