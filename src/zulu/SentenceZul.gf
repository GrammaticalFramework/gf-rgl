concrete SentenceZul of Sentence = CatZul ** open Prelude,ResZul,ParamX in {

  flags optimize=all_subs ;

  lin

    PredVP np vp = case vp.vptype of {
      CopIdent => cl_with_id_cop_predicate np vp ;
      CopAssoc => cl_with_ass_cop_predicate np vp ;
      CopDescr => cl_with_descr_cop_predicate np vp ;
      CopEq => cl_with_eq_cop_predicate np vp ;
      APComp => cl_with_ap_comp_predicate np vp ;
      AdvComp => cl_with_adv_comp_predicate np vp ;
      _ => cl_with_verb_predicate np vp
    } ;

--     PredSCVP sc vp = mkClause sc.s (agrP3 Sg) vp ;

    ImpVP vp = let
      np = {
        empty = [] ;
        nom = table {Full|Reduced => []} ;
        loc = [] ;
        desc = [] ;
        agr = Second Sg ;
        isPron = True ;
        reqLocS = True
      } ;
      impTense = PresTense
    in case vp.vptype of {
      NPComp => {
        s = table {
          Pos => vp.s ++BIND++ "a" ++ vp.comp ;
          Neg => "unga" ++ vp.s ++ "i" ++ vp.comp
        }
      } ;
      -- CopIdent => {s = \\pol => (cl_with_id_cop_predicate np vp).s!pol!(Absolute PresTense)!Princ } ;
      -- CopAssoc => {s = \\pol => (cl_with_ass_cop_predicate np vp).s!pol!(Absolute PresTense)!Princ } ;
      -- CopDescr => {s = \\pol => (cl_with_descr_cop_predicate np vp).s!pol!(Absolute PresTense)!Princ } ;
      -- CopEq => {s = \\pol => (cl_with_eq_cop_predicate np vp).s!pol!(Absolute PresTense)!Princ } ;
      -- APComp => {s = \\pol => (cl_with_ap_comp_predicate np vp).s!pol!(Absolute PresTense)!Princ } ;
      -- AdvComp => {s = \\pol => (cl_with_adv_comp_predicate np vp).s!pol!(Absolute PresTense)!Princ } ;
      -- _ => {s = \\pol => (cl_with_verb_predicate np vp).s!pol!(Absolute PresTense)!Princ }

      CopIdent => {s = \\pol => (cl_with_id_cop_predicate np vp).s!pol!impTense!Princ } ;
      CopAssoc => {s = \\pol => (cl_with_ass_cop_predicate np vp).s!pol!impTense!Princ } ;
      CopDescr => {s = \\pol => (cl_with_descr_cop_predicate np vp).s!pol!impTense!Princ } ;
      CopEq => {s = \\pol => (cl_with_eq_cop_predicate np vp).s!pol!impTense!Princ } ;
      APComp => {s = \\pol => (cl_with_ap_comp_predicate np vp).s!pol!impTense!Princ } ;
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
      s = \\dm => t.s ++ p.s ++ cl.s ! p.p ! t.t ! dm ;
      subjs = t.s ++ p.s ++ cl.subjcl ! p.p ! t.t ;
      pots = \\dm => t.s ++ p.s ++ cl.potcl ! p.p ! dm
    } ;
    UseQCl t p cl = {
      s = t.s ++ p.s ++ cl.s ! p.p ! t.t ! Princ ;
      potqs = t.s ++ p.s ++ cl.potqcl ! p.p ! Princ ;
      qword = cl.qword
    } ;
    UseRCl temp pol rcl = {
      s = \\a => rcl.s!pol.p!temp.t!a ;
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

    cl_with_verb_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      s = \\p,t,dm =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          -- aux_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b1
          -- } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          -- vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = VFIndic dm p t vp.asp ;
          -- aux = case t of {
          --   Absolute bt => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => []
          --   } ;
          --   Relative _ _ => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          --   }
          -- } ;
          vow = case <vp.r,p,t> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          lfya = case <vp.hasComp,p,t> of {
            <False,Pos,PresTense> => "ya" ++BIND ;
            <_,_,_> => []
          } ;
          reqLF = case vp.hasComp of {
            True => False ;
            False => True
          }
        in
          subj
          -- ++ aux
          ++ (negPref vform_main np.agr)
          ++ (exclSePref vform_main)
          ++ (subjConc vform_main np.agr vow)
          ++ (negPref2 vform_main)
          ++ lfya
          ++ (progPref vform_main)
          ++ (exclKaPref vform_main)
          ++ (tensePref vform_main)
          ++ vp.oc
          ++ vp.s ++ BIND
          ++ (vtermSuff vform_main reqLF vp.perfSuff)
          ++ vp.comp ;
      subjcl = \\p,t =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          vform_main = VFSubj p ;
          vow = case <vp.r,p,t> of {
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
          ++ vp.oc
          ++ vp.s ++ BIND
          ++ (vtermSuff vform_main False vp.perfSuff)
          ++ vp.comp ;
      potcl = \\p,dm =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          vform_main = VFPot dm p vp.asp ;
        in
          subj
          ++ (subjConc vform_main np.agr False)
          ++ (potPref vform_main)
          ++ (negPref2 vform_main)
          ++ vp.oc
          ++ vp.s ++ BIND
          ++ (vtermSuff vform_main False vp.perfSuff)
          ++ vp.comp
    } ;

    cl_with_ap_comp_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      s = \\p,t,dm =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          -- aux_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b1
          -- } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          -- vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = VFIndic dm p t vp.asp ;
          -- aux = case t of {
          --   Absolute bt => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => []
          --   } ;
          --   Relative _ _ => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          --   }
          -- } ;
          vow = case <vp.r,p,t> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          lfya = [] ;
          reqLF = False ;
          adjf = case vp.ap_bool of {
            True => (aformN np.agr) ;
            -- True => AF2 ;
            False => AF1
          } ;
          comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf
        in
          subj
          -- ++ aux
          ++ (negPref vform_main np.agr)
          ++ (exclSePref vform_main)
          ++ (subjConc vform_main np.agr vow)
          ++ (negPref2 vform_main)
          ++ lfya
          ++ (progPref vform_main)
          ++ (exclKaPref vform_main)
          ++ (tensePref vform_main)
          ++ vp.s ++ BIND
          ++ (vtermSuff vform_main False vp.perfSuff)
          ++ comp ;
      subjcl = \\p,t =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          vform_main = VFSubj p ;
          vow = case <vp.r,p,t> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          adjf = case vp.ap_bool of {
            True => (aformN np.agr) ;
            -- True => AF2 ;
            False => AF1
          } ;
          comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf
        in
          subj
          ++ (subjConc vform_main np.agr vow)
          ++ (negPref2 vform_main)
          ++ vp.s ++ BIND
          ++ (vtermSuff vform_main False vp.perfSuff)
          ++ comp ;
      potcl = \\p,dm =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          vform_main = VFPot dm p vp.asp ;
          adjf = case vp.ap_bool of {
            True => (aformN np.agr) ;
            -- True => AF2 ;
            False => AF1
          } ;
          comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf
        in
          subj
          ++ (subjConc vform_main np.agr False)
          ++ (potPref vform_main)
          ++ (negPref2 vform_main)
          ++ vp.s ++ BIND
          ++ (vtermSuff vform_main False vp.perfSuff)
          ++ comp
    } ;

    cl_with_id_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      s = \\p,t,dm =>
        let
          -- aux_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b1
          -- } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          -- vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = VFIndic dm p t vp.asp ;
          -- aux = case t of {
          --   Absolute bt => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => []
          --   } ;
          --   Relative _ _ => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          --   }
          -- } ;
          subj = case np.isPron of {
            True => np.empty ; -- () ngiyiphoyisa (I am a policeman) p359
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = vp.comp
        in
          subj ++
          -- aux ++
          pcp ++
          cp ++ BIND ++
          cb ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = vp.comp
        in
          subj ++
          pcp ++
          cp ++ BIND ++
          cb ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = vp.comp
        in
          subj ++
          pcp ++
          cp ++ BIND ++
          cb
    } ;

    cl_with_ass_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      s = \\p,t,dm =>
        let
          -- aux_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b1
          -- } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          -- vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = VFIndic dm p t vp.asp ;
          -- aux = case t of {
          --   Absolute bt => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => []
          --   } ;
          --   Relative _ _ => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          --   }
          -- } ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = (advPref ! vp.r) ++ BIND ++ vp.comp
        in
          subj ++
          -- aux ++
          pcp ++
          cb ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = (advPref ! vp.r) ++ BIND ++ vp.comp
        in
          subj ++
          pcp ++
          cb ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = (advPref ! vp.r) ++ BIND ++ vp.comp
        in
          subj ++
          pcp ++
          cb
    } ;

    cl_with_eq_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      s = \\p,t,dm =>
        let
          -- aux_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b1
          -- } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          -- vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = VFIndic dm p t vp.asp ;
          -- aux = case t of {
          --   Absolute bt => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => []
          --   } ;
          --   Relative _ _ => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          --   }
          -- } ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = (eqPref ! vp.r) ++ BIND ++ vp.comp
        in
          subj ++
          -- aux ++
          pcp ++
          cb ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = (eqPref ! vp.r) ++ BIND ++ vp.comp
        in
          subj ++
          pcp ++
          cb ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref vp.comp_agr ;
          cb = (eqPref ! vp.r) ++ BIND ++ vp.comp
        in
          subj ++
          pcp ++
          cb
    } ;

    cl_with_descr_cop_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      s = \\p,t,dm =>
        let
          -- aux_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b1
          -- } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          -- vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = VFIndic dm p t vp.asp ;
          -- aux = case t of {
          --   Absolute bt => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => []
          --   } ;
          --   Relative _ _ => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          --   }
          -- } ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          adjf = case vp.ap_bool of {
            True => (aformN np.agr) ;
            --  True => AF2 ;
            False => AF1
          } ;
          comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf
        in
          subj ++
          -- aux ++
          pcp ++
          comp ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          adjf = case vp.ap_bool of {
            True => (aformN np.agr) ;
            -- True => AF2 ;
            False => AF1
          } ;
          comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf
        in
          subj ++
          pcp ++
          comp ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p vp.asp ;
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          pcp = pre_cop_pref vform_main np.agr ;
          adjf = case vp.ap_bool of {
            True => (aformN np.agr) ;
            -- True => AF2 ;
            False => AF1
          } ;
          comp =  adjPrefLookup!np.agr!adjf ++BIND++ vp.ap_comp!adjf
        in
          subj ++
          pcp ++
          comp
    } ;

    cl_with_adv_comp_predicate : NP -> VP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np,vp -> {
      s = \\p,t,dm =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          -- aux_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b1
          -- } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          -- vform_aux = VFIndic dm p aux_tense vp.asp ;
          vform_main = VFIndic dm p t vp.asp ;
          -- aux = case t of {
          --   Absolute bt => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => []
          --   } ;
          --   Relative _ _ => case vp.hasAux of {
          --     True => (subjConcLookup!np.agr!SC) ++BIND++ vp.aux_root ;
          --     False => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          --   }
          -- } ;
          vow = case <vp.r,p,t> of {
            <RC,Pos,PresTense> => False ;
            <RC,Pos,PerfTense> => False ;
            <_,Pos,PresTense> => True ;
            <_,Pos,PerfTense> => True ;
            <_,_,_> => False
          } ;
          lfya = case <vp.hasComp,p,t> of {
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
          -- ++ aux
          ++ (negPref vform_main np.agr)
          ++ (exclSePref vform_main)
          ++ (subjConc vform_main np.agr vow)
          ++ (negPref2 vform_main)
          ++ lfya
          ++ (progPref vform_main)
          ++ (exclKaPref vform_main)
          ++ (tensePref vform_main)
          -- ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main reqLF vp.perfSuff)
          ++ vp.comp ;
      subjcl = \\p,t =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          -- main_tense = case t of {
          --   Absolute bt => bt ;
          --   Relative b1 b2 => b2
          -- } ;
          vform_main = VFSubj p ;
          vow = case <vp.r,p,t> of {
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
          ++ vp.comp ;
      potcl = \\p,dm =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          vform_main = VFPot dm p vp.asp ;
        in
          subj
          ++ (subjConc vform_main np.agr False)
          ++ (potPref vform_main)
          ++ (negPref2 vform_main)
          -- ++ vp.oc
          -- ++ vp.s ++ BIND
          -- ++ (vtermSuff vform_main False vp.perfSuff)
          ++ vp.comp
    } ;

}
