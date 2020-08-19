concrete ExtraZul of ExtraZulAbs =
  GrammarZul[NP,VP,CN,V,Temp,Pol,S,Cl,Adv,Pron,QCl,QS,A,RS],
  ExtraCatZul
  ** open ResZul,Prelude,ParamX in {

  lin
    PotQS p qcl = {
      s = qcl.potqcl!p.p!Princ ;
      qword = qcl.qword
    } ;

    AssocCop np = {
      s = [] ;
      perfSuff = [] ;
      oc = [] ;
      comp = case np.isPron of {
        True => np.nom!Full ;
        False => np.nom!Reduced ++ np.desc
      } ;
      hasComp = True ;
      r = case np.isPron of {
        True => RC ;
        False => nominit!np.agr
      } ;
      syl = SylMult ;
      asp = Null ;
      asp_pref = \\_ => [] ;
      vptype = CopAssoc ;
      comp_agr = np.agr ;
      ap_comp = \\_ => [] ;
      ap_bool = False ;
      aux_root = [] ;
      hasAux = False
    } ;

    EqCop np = {
      s = [] ;
      perfSuff = [] ;
      oc = [] ;
      comp = case np.isPron of {
        True => np.nom!Full ;
        False => np.nom!Reduced ++ np.desc
      } ;
      hasComp = True ;
      r = case np.isPron of {
        True => RC ;
        False => nominit!np.agr
      } ;
      syl = SylMult ;
      asp = Null ;
      asp_pref = \\_ => [] ;
      vptype = CopEq ;
      comp_agr = np.agr ;
      ap_comp = \\_ => [] ;
      ap_bool = False ;
      aux_root = [] ;
      hasAux = False
    } ;

    -- NOTE: this is only here to play nice with Xhosa; commented out now.
    -- DescrNP cn np = {
    --   nom = \\n,cpf => cn.nom!n!cpf ;
    --   loc = \\n => cn.loc!n ;
    --   desc = \\n => cn.desc!n ++ np.nom!Full ++ np.desc ;
    --   c = cn.c
    -- } ;

    PossPronZul cn pron =
    let
      stem = case pron.agr of {
        First Sg => "mi" ;
        First Pl => "thu" ;
        Second Sg => "kho" ;
        Second Pl => "nu" ;
        Third _ Sg => "khe" ;
        Third _ Pl => "bo"
      } ;
      proninit = case pron.agr of {
        First Sg => RC ;
        First Pl => RI ;
        Second Sg => RC ;
        Second Pl => RI ;
        Third _ Sg => RC ;
        Third _ Pl => RC
      } ;
    in {
      nom = \\n,cpf => cn.nom!n!cpf ++ poss_concord!cn.c!n!proninit ++BIND++ stem ;
      loc = \\n => cn.loc!n ++ poss_concord!cn.c!n!proninit ++BIND++ stem ;
      desc = \\n => cn.desc!n ;
      c = cn.c
    } ;

    InstrNPAdv np =
    let
      pref = case np.isPron of {
        True => "nga" ;
        False => instrument_pref np.agr
        }
    in {
      s = pref ++ np.nom!Reduced ++ np.desc ;
      asp = Null ;
      reqLocS = False
    } ;

    InstrAdvNPAdv adv np =
    let
      pref = case np.isPron of {
        True => "nga" ;
        False => instrument_pref np.agr
        }
    in {
      s = adv.s ++ pref ++ np.nom!Reduced ++ np.desc ;
      asp = adv.asp ;
      reqLocS = False
    } ;

    LocNPAdv np = {
      s = np.loc ++ np.desc ;
      asp = Null ;
      reqLocS = True
    } ;

    LocAdvNPAdv adv np = {
      s = adv.s ++ np.loc ++ np.desc ;
      asp = adv.asp ;
      reqLocS = False
    } ;

    -- NOTE: this seems to be a specific construction. Not yet found in Poulos+Msimang
    KwaNPAdv adv np =
      let
        c = case np.agr of {
          (First _ | Second _) => C1_2 ; -- people class as default
          Third c _ => c
        } ;
        n = case np.agr of {
          (First Sg | Second Sg | Third _ Sg) => Sg ;
          (First Pl | Second Pl | Third _ Pl) => Pl
        }
      in
      {
        s = adv.s ++ poss_concord!C15!Sg!(nominit!np.agr) ++BIND++ np.nom!Reduced ++ np.desc ;
        asp = adv.asp ;
        reqLocS = False
      } ;

    -- locative ku
    KuNPAdv np = {
      s = case np.isPron of {
        True => "ki" ;
        False => case (nominit!np.agr) of {
          RI  => "ki" ;
          RO  => "ko" ;
          RA  => "kw" ;
          _   => "ku"
        }
      }
      ++BIND++ np.nom!Reduced ++ np.desc ;
      asp = Null ;
      reqLocS = False
    } ;

    KuAdvNPAdv adv np = {
      s = adv.s ++
        case np.isPron of {
          True => "ki" ;
          False => case (nominit!np.agr) of {
            RI  => "ki" ;
            RO  => "ko" ;
            RA  => "kw" ;
            _   => "ku"
          }
        }
      ++BIND++ np.nom!Reduced ++ np.desc ;
      asp = Null ;
      reqLocS = False
    } ;

    NaNPAdv np = {
      s = advPref ! (nominit!np.agr) ++ BIND ++ np.nom!Reduced ++ np.desc ;
      asp = Null ;
      reqLocS = False
    } ;

    RelAdv adv = {
      s = \\a => relConc!a ++BIND++ adv.s
    } ;

    -- ProgVP vp = {
    --   s = vp.s ;
    --   perfSuff = vp.perfSuff ;
    --   oc = vp.oc ;
    --   comp = vp.comp ;
    --   hasComp = vp.hasComp ;
    --   r = vp.r ;
    --   syl = vp.syl ;
    --   asp = Prog ;
    --   vptype = vp.vptype ;
    --   comp_agr = vp.comp_agr ;
    --   ap_comp = vp.ap_comp ;
    --   ap_bool = vp.ap_bool ;
    --   aux_root = vp.aux_root ;
    --   hasAux = vp.hasAux
    -- } ;

    QuantRS quant = {
      s = \\a => relConc!a ++BIND++ quantConc!a ++BIND++ quant.s
    } ;

    RelRS rel = {
      s = \\a => relConc!a ++BIND++ rel.s
    } ;

    QuantCN quant cn =
    let
      cn_agr = Third cn.c quant.n
    in
    {
      empty = [] ;
      nom = \\p => quantConc!cn_agr ++BIND++ quant.s ++ cn.nom ! quant.n ! p ;
      loc = quantConc!cn_agr ++BIND++ quant.s ++ cn.loc ! quant.n ;
      desc = cn.desc ! quant.n ;
      agr = cn_agr ;
      isPron = False ;
      reqLocS = False
    } ;

    NumAdjCN a cn = {
      nom = cn.nom ;
      loc = cn.loc ;
      desc = \\num =>
        let
          agr = Third cn.c num ;
        in
          "na" ++BIND++ a.s!AF2 ++ cn.desc ! num ;
      c = cn.c
    } ;

    only_QuantStem = { s = "dwa" ; n = Sg } ;
    all_QuantStem = { s = "nke" ; n = Pl } ;
    painful_RelStem = { s = "buhlungu" } ;

    -- TPerfPast = { s = [] ; t = Relative PerfTense PastTense } ;
    -- TPresPres = { s = [] ; t = PresTense } ;
    -- TPastPres = { s = [] ; t = Relative PastTense PresTense } ;
    -- TPastPerf = { s = [] ; t = Relative PastTense PerfTense } ;

    PredNP np = cl_with_np_predicate np ;

    IAdvQCl np iadv = qcl_np_iadv np iadv ;

    ComplVAux vaux vp = {
        s = vp.s ;
        perfSuff = vp.perfSuff ;
        oc = vp.oc ;
        comp = vp.comp ;
        hasComp = vp.hasComp ;
        r = vp.r ;
        syl = vp.syl ;
        asp = vp.asp ;
        asp_pref = vp.asp_pref ;
        vptype = vp.vptype ;
        comp_agr = vp.comp_agr ;
        ap_comp = vp.ap_comp ;
        ap_bool = vp.ap_bool ;
        aux_root = vaux.s ;
        hasAux = True
    } ;

    -- UseLocNP np = {
    --   s = [] ;
    --   perfSuff = [] ;
    --   oc = [] ;
    --   comp = "s" ++BIND++ np.loc ++ np.desc ;
    --   hasComp = True ;
    --   r = nominit!np.agr ;
    --   syl = SylMult ;
    --   asp = Null ;
    --   vptype = CopIdent ;
    --   comp_agr = np.agr ;
    --   ap_comp = \\_ => [] ;
    --   ap_bool = False ;
    --   aux_root = [] ;
    --   hasAux = False
    -- } ;

    ConjNAdv conj s = {
      s = conj.s ++ s.s!Part ;
      asp = Null ;
      reqLocS = False
    } ;

    where_ConjN = { s = "lapho" } ;

    it3_Pron = { s = "wo" ; agr = Third C3_4 Sg ; empty = [] } ;
    they4_Pron = { s = "yo" ; agr = Third C3_4 Pl ; empty = [] } ;
    it5_Pron = { s = "lo" ; agr = Third C5_6 Sg ; empty = [] } ;
    they6_Pron = { s = "wo" ; agr = Third C5_6 Pl ; empty = [] } ;
    it7_Pron = { s = "so" ; agr = Third C7_8 Sg ; empty = [] } ;
    they8_Pron = { s = "zo" ; agr = Third C7_8 Pl ; empty = [] } ;
    it9_Pron = { s = "yo" ; agr = Third C9_10 Sg ; empty = [] } ;
    they10_Pron = { s = "zo" ; agr = Third C9_10 Pl ; empty = [] } ;
    it11_Pron = { s = "lo" ; agr = Third C11_10 Sg ; empty = [] } ;
    it14_Pron = { s = "bo" ; agr = Third C14 Sg ; empty = [] } ;
    it15_Pron = { s = "ko" ; agr = Third C15 Sg ; empty = [] } ;
    it17_Pron = { s = "ko" ; agr = Third C17 Sg ; empty = [] } ;

    at_which_IAdv np = { s = "nga" ++BIND++ atwhichPhiPref!np.agr ++BIND++ "phi" ++ np.nom!Full } ;

    how_many_IAdj = regAdj "ngaki" ;

    IAdjIAdv np iadj = {
      s = np.loc ++ np.desc ++ adjConcLookup!np.agr!(aformN np.agr) ++BIND++ iadj.s!(aformN np.agr)
    } ;

  oper
    qcl_np_iadv : NP -> IAdv -> {s : Polarity => ZTense => DMood => Str ; potqcl : Polarity => DMood => Str ; qword : Str} = \np,iadv -> {
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
          -- vform_aux = VFIndic dm p aux_tense Null ;
          vform_main = VFIndic dm p t Null ;
          -- aux = case t of {
          --   Absolute bt => [] ;
          --   Relative _ _ => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          -- } ;
        in
          subj ++
          -- aux ++
          (subjConc vform_main np.agr False) ++
          iadv.s ;
      potqcl = \\p,dm =>
        let
          subj = case np.isPron of {
            True => np.empty ;
            False => np.nom ! Full ++ np.desc
          } ;
          vform_main = VFPot dm p Null ;
        in
          subj ++
          -- aux ++
          (subjConc vform_main np.agr False) ++
          (potPref vform_main) ++
          iadv.s ;
      qword = []
    } ;

    cl_with_np_predicate : NP -> { s : Polarity => ZTense => DMood => Str ; subjcl : Polarity => ZTense => Str ; potcl : Polarity => DMood => Str } = \np -> {
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
          -- vform_aux = VFIndic dm p aux_tense Null ;
          vform_main = VFIndic dm p t Null ;
          -- aux = case t of {
          --   Absolute bt => [] ;
          --   Relative _ _ => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
          -- } ;
          --pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref np.agr ;
          cb = np.nom ! Full ++ np.desc
        in
          -- aux ++
          --pcp ++
          cp ++ BIND ++
          cb ;
      subjcl = \\p,t =>
        let
          vform_main = VFSubj p ;
          --pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref np.agr ;
          cb = np.nom ! Full ++ np.desc
        in
          --pcp ++
          cp ++ BIND ++
          cb ;
      potcl = \\p,dm =>
        let
          vform_main = VFPot dm p ;
          --pcp = pre_cop_pref vform_main np.agr ;
          cp = cop_pref np.agr ;
          cb = np.nom ! Full ++ np.desc
        in
          --pcp ++
          cp ++ BIND ++
          cb
    } ;

}
