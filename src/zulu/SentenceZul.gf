concrete SentenceZul of Sentence = CatZul ** open Prelude,ResZul,ParamX in {

  flags optimize=all_subs ;

  lin

    PredVP np vp = case vp.vptype of {
      CopIdent => comp_pred np vp ;
      CopAssoc => comp_pred np vp ;
      CopDescr => comp_pred np vp ;
      CopLoc   => comp_pred np vp ;
      CopEq => cl_with_eq_cop_predicate np vp ;
      -- VACompl => cl_with_ap_comp_predicate np vp ;
      AdvComp => cl_with_adv_comp_predicate np vp ;
      (NoComp | VNPCompl) =>
      let
        longform_suffix = case vp.hasComp of {
          True => False ;
          False => True
        }
      in {
        s = \\p,t => np.s!NFull ++ (verb_prefix vp p t np.agr) ++ vp.s!MainCl!np.agr!p!t!longform_suffix ++ vp.comp ++ vp.iadv ++ vp.advs ;
      } ;
      _ => cl_with_verb_predicate np vp
    } ;

--     PredSCVP sc vp = mkClause sc.s (agrP3 Sg) vp ;

    ImpVP vp = let
      np = {
        empty = [] ;
        s = table {NFull|NReduced|NPoss|NLoc => []} ;
        -- loc = [] ;
        -- desc = [] ;
        -- det = [] ;
        agr = Second Sg ;
        i = nominit!(Second Sg) ;
        proDrop = True ;
        isPron = True ;
        heavy = False
        -- reqLocS = True ;
        -- qdef = Article Spec
      } ;
      impTense = PresTense
    in case vp.vptype of {
      VNPCompl => {
        s = table {
          Pos => vp.s!MainCl!np.agr!Pos!impTense!False ++ vp.comp ++ vp.iadv ++ vp.advs ;
          Neg => "unga" ++ vp.s!MainCl!np.agr!Neg!impTense!False ++ vp.comp ++ vp.iadv ++ vp.advs
        }
      } ;

      CopIdent => {s = \\pol => (comp_pred np vp).s!pol!impTense } ;
      CopAssoc => {s = \\pol => (comp_pred np vp).s!pol!impTense } ;
      CopDescr => {s = \\pol => (comp_pred np vp).s!pol!impTense } ;
      CopEq => {s = \\pol => (cl_with_eq_cop_predicate np vp).s!pol!impTense } ;
      -- VACompl => {s = \\pol => (cl_with_ap_comp_predicate np vp).s!pol!impTense!Princ } ;
      AdvComp => {s = \\pol => (cl_with_adv_comp_predicate np vp).s!pol!impTense } ;
      _ => {s = \\pol => (imp_verb_predicate np vp).s!pol!impTense }
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
      s = t.s ++ p.s ++ cl.s ! p.p ! t.t
    } ;
    UseQCl t p cl = {
      s = t.s ++ p.s ++ cl.s ! p.p ! t.t  ;
      -- potqs = t.s ++ p.s ++ cl.potqcl ! p.p ! Princ ;
      qword_pre = cl.qword_pre ;
      qword_post = cl.qword_post
    } ;
    UseRCl temp pol rcl = {
      s = \\a => temp.s ++ pol.s ++ rcl.s!a!pol.p!temp.t ;
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

    comp_pred : NP -> VP -> { s : Polarity => BasicTense => Str } = \np,vp -> {
      s = \\p,t =>
        let
          subj = np.s!NFull
        in
          subj ++
          vp.s!MainCl!np.agr!p!t!False
          ++ vp.comp ++ vp.iadv ++ vp.advs
    } ;

    imp_verb_predicate : NP -> VP -> { s : Polarity => BasicTense => Str } = \np,vp -> {
      s = \\p,t =>
        let
          subj = np.s!NFull ;
          vform_main = VFIndic MainCl p t ;
        in
          impPref p
          ++ vp.s!MainCl!np.agr!p!t!False
          ++ vp.iadv
          ++ vp.comp
          ++ vp.advs
    } ;

    -- imp_verb_prefix : VP -> Polarity -> BasicTense -> Agr -> Str = \vp,p,t,agr ->
    --   let
    --     -- vow = case <vp.hasComp,vp.r,p,t> of {
    --     vow = case <vp.hasComp,p,t,vp.r> of {
    --       <False,Pos,PresTense,RC> => False ; -- force the compiler to understand the table
    --       <False,Pos,PresTense,_> => False ; -- long form ya
    --
    --       <_,_,PresTense,RC> => False ;
    --       <_,_,PresTense,_> => True ;
    --       -- <_,Pos,PerfTense,RC> => False ;
    --       -- <_,Pos,PerfTense,_> => True ;
    --       <_,_,PastTense,RC> => False ;
    --       <_,_,PastTense,_> => True ;
    --       <_,_,RemPastTense,RC> => False ;
    --       <_,_,RemPastTense,_> => True ;
    --       <_,_,_,_> => False
    --     } ;
    --     vform = VFIndic MainCl p t
    --   in
    --       (negPref vform)
    --    -- ++ (exclSePref vform_main)
    --    ++ (subjConc vform agr vow)
    --    -- ++ (negPref2 vform_main)
    --    -- ++ (tensePref vform)
    -- ;

    cl_with_verb_predicate : NP -> VP -> { s : Polarity => BasicTense => Str } = \np,vp -> {
      s = \\p,t =>
        let
          subj = np.s!NFull ;
          vform_main = VFIndic MainCl p t ;
          longform_suffix = case vp.hasComp of {
            True => False ;
            False => True
          }
        in
          subj
          ++ (verb_prefix vp p t np.agr)
          ++ vp.s!MainCl!np.agr!p!t!longform_suffix
          ++ vp.iadv
          ++ vp.comp
          ++ vp.advs
    } ;

    verb_prefix : VP -> Polarity -> BasicTense -> Agr -> Str = \vp,p,t,agr ->
      let
        lfya = case <vp.hasComp,p,t,vp.r> of {
          <False,Pos,PresTense,RC> => "ya" ++BIND ;
          <False,Pos,PresTense,_> => "y" ++BIND ;
          <_,_,_> => []
        } ;
        -- vow = case <vp.hasComp,vp.r,p,t> of {
        vow = case <vp.hasComp,p,t,vp.r> of {
          <False,Pos,PresTense,RC> => False ; -- force the compiler to understand the table
          <False,Pos,PresTense,_> => False ; -- long form ya

          <_,_,PresTense,RC> => False ;
          <_,_,PresTense,_> => True ;
          -- <_,Pos,PerfTense,RC> => False ;
          -- <_,Pos,PerfTense,_> => True ;
          <_,_,PastTense,RC> => False ;
          <_,_,PastTense,_> => True ;
          <_,_,RemPastTense,RC> => False ;
          <_,_,RemPastTense,_> => True ;
          <_,_,_,_> => False
        } ;
        vform = VFIndic MainCl p t
      in
          (negPref vform)
       -- ++ (exclSePref vform_main)
       ++ (subjConc vform agr vow)
       -- ++ (negPref2 vform_main)
       ++ lfya
       -- ++ (tensePref vform)
    ;

    -- TODO: aspect
    cl_with_eq_cop_predicate : NP -> VP -> { s : Polarity => BasicTense => Str } = \np,vp -> {
      s = \\p,t =>
        let
          vform_main = VFIndic MainCl p t ;
          subj = np.s!NFull ;
          pcp = pre_cop_pref vform_main np.agr ;
          -- cb = (eqPref ! vp.r) ++ BIND ++ vp.comp ;
        in
          subj ++
          -- pcp ++
          vp.s!MainCl!np.agr!p!t!False
          ++ vp.comp ++ vp.iadv ++ vp.advs
    } ;

    cl_with_adv_comp_predicate : NP -> VP -> { s : Polarity => BasicTense => Str } = \np,vp -> {
      s = \\p,t =>
        let
          subj = np.s!NFull ;
          vform_main = VFIndic MainCl p t ;
          vow = case <vp.r,p,t> of {
            <RC,Pos,PresTense> => False ;
            <_,Pos,PresTense> => True ;
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
          subj ++
          -- ++ (negPref vform_main)
          -- -- ++ (exclSePref vform_main)
          -- ++ (subjConc vform_main np.agr vow)
          -- -- ++ (negPref2 vform_main)
          -- ++ lfya
          -- -- ++ (tensePref vform_main)
          -- ++ vp.comp
          vp.s!MainCl!np.agr!p!t!False
          ++ vp.comp ++ vp.iadv ++ vp.advs
    } ;

}
