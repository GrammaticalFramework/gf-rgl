concrete ExtraExtZul of ExtraExt =
  CatZul [NP,VP,CN,V,Temp,S,Cl,Adv,Pron,QCl,QS,A,RS,IAdv,IComp,Pol,Det,Quant,N,PN],
  CatExtZul
  ** open ResZul,Prelude,ParamX in {

  lin
    -- use with caution
    ProDrop pron = {
      s = table {
        NFull => case pron.proDrop of {
          True => "*" ++ pron.s!NFull ;
          False => pron.empty
        } ;
        nform => "*" ++ pron.s!nform
      } ;
      agr = pron.agr ;
      empty = pron.empty ;
      proDrop = True
    } ;

  lin
    -- PotQS pol qcl = {
    --   s = pol.s ++ qcl.potqcl!pol.p!Princ ;
    --   qword_pre = qcl.qword_pre ;
    --   qword_post = qcl.qword_post
    -- } ;

    -- SubjunctS s = { s = s.subjs } ;

    -- AssocCop np = {
    --   s = \\_ => [] ;
    --   oc = [] ;
    --   comp = np.s!Reduced ;
    --   iadv = [] ;
    --   advs = [] ;
    --   hasComp = True ;
    --   r = case np.proDrop of {
    --     True => RC ;
    --     False => initNP np.isPron np.agr
    --   } ;
    --   syl = SylMult ;
    --   asp = Null ;
    --   asp_pref = \\_ => [] ;
    --   vptype = CopAssoc ;
    --   comp_agr = np.agr ;
    --   ap_comp = \\_ => [] ;
    --   aux_root = [] ;
    --   hasAux = False
    -- } ;
    --
    -- EqCop np = {
    --   s = \\_ => [] ;
    --   oc = [] ;
    --   comp = np.s!Full ;
    --   iadv = [] ;
    --   advs = [] ;
    --   hasComp = True ;
    --   r = case np.isPron of {
    --     True => RC ;
    --     False => initNP np.isPron np.agr
    --   } ;
    --   syl = SylMult ;
    --   asp = Null ;
    --   asp_pref = \\_ => [] ;
    --   vptype = CopEq ;
    --   comp_agr = np.agr ;
    --   ap_comp = \\_ => [] ;
    --   aux_root = [] ;
    --   hasAux = False
    -- } ;

    -- UsePNPl pn = let
    --   agr = Third pn.c Pl
    -- in {
    --   empty,predet_pre,predet_post = pn.empty ;
    --   s = pn.s!Pl ;
    --   mod = pn.empty ;
    --   dem = pn.empty ;
    --   agr = agr ;
    --   i = nominit!agr ;
    --   proDrop = False ;
    --   isPron = False ;
    --   -- reqLocS = True ;
    --   qdef = Article Spec ;
    -- } ;

    -- PNAsCN pn = pn ** { mod = \\_ => [] } ;

    -- DemPron quant pron = let
    -- d = case quant.qdef of {
    --   Article _ => Dem1 ;
    --   Demonstrative d => d
    -- }
    -- in {
    --   empty,predet_pre,dem,predet_post = pron.empty ;
    --   -- dem = case quant.qdef of {
    --   --   Article _ => dem_pron!Dem1!pron.agr ;
    --   --   Demonstrative d => dem_pron!d!pron.agr
    --   -- } ;
    --   -- s = \\nform => quant.s ++ pron.s!nform ;
    --   s = table {
    --     Full => quant.s ++ dem_pron!d!pron.agr ++ pron.empty ;
    --     Reduced => quant.s ++ dem_pron!d!pron.agr ++ pron.empty ;
    --     Poss => quant.s ++ dem_pron!d!pron.agr ++ pron.empty ;
    --     Loc => quant.s ++ dem_pron!d!pron.agr ++ pron.empty
    --   } ;
    --   mod = pron.empty ;
    --   agr = pron.agr ;
    --   i = RC ;
    --   proDrop = False ;
    --   isPron = True ;
    --   -- reqLocS = True ;
    --   qdef = case quant.qdef of {
    --     Article _ => Demonstrative Dem1 ;
    --     Demonstrative d => Demonstrative d
    --   }
    -- } ;

    -- EmphCN cn = {
    --   s = \\num,nform => pron_stem!(Third cn.c num) ++BIND++ "na" ++ cn.s!num!nform ;
    --   mod = cn.mod ;
    --   c = cn.c ;
    --   empty = cn.empty
    -- } ;
    --
    -- ContrastCN cn = {
    --   s = cn.s ;
    --   mod = \\num => pron_stem!(Third cn.c num) ++BIND++ "na" ++ cn.mod!num ;
    --   c = cn.c ;
    --   empty = cn.empty
    -- } ;

    -- ApposCNCN cn1 cn2 = {
    --   s = cn1.s ;
    --   mod = \\n => cn1.mod!n ++ cn2.s!n!Full ++ cn2.mod!n ;
    --   c = cn1.c ;
    --   empty = cn1.empty ++ cn2.empty
    -- } ;

    -- ApposNPN np n = let
    --   num = case np.agr of {
    --     First n => n ;
    --     Second n => n ;
    --     Third c n => n
    --   } ;
    -- in {
    --   empty = np.empty ;
    --   s = np.s;
    --   mod = np.mod ++ np.predet_post ++ n.s!num!Full ;
    --   dem = np.dem ;
    --   predet_pre = np.predet_pre ;
    --   predet_post = np.empty ;
    --   agr = Third n.c num ;
    --   i = np.i ;
    --   proDrop = np.proDrop ;
    --   isPron = np.isPron ;
    --   -- reqLocS = np.reqLocS ;
    --   qdef = np.qdef ;
    -- } ;

    PossLocNP locn np = {
      empty = np.empty ;
      s = \\n,nform => locn.s ;
      mod = \\num => poss_concord!(C17)!Sg!np.i ++BIND++ (poss_NP np) ;
      c = C17 ;
      emph = False
    } ;

    InstrNPAdv np =
    let
      pref = instrPref!(initNP np.isPron np.agr)
    in {
      s = pref ++BIND++ (np.s!NReduced) ;
      -- asp = Null ;
      reqLocS = False
    } ;

    InstrAdvNPAdv adv np =
    let
      pref = instrPref!(initNP np.isPron np.agr)
    in {
      s = adv.s ++ pref ++BIND++ (np.s!NReduced) ;
      -- asp = adv.asp ;
      reqLocS = False
    } ;

    LocNPAdv np = {
      s = np.s!NLoc ;
      -- asp = Null ;
      reqLocS = case np.isPron of {
        False => True ;
        True => False -- ki-
      } ;
    } ;

    LocAdvNPAdv adv np = {
      s = adv.s ++ (np.s!NLoc) ;
      -- asp = adv.asp ;
      reqLocS = False
    } ;

    -- locative kwa
    KwaNPAdv np = {
      -- s = "kwa" ++BIND++ (np.s!Reduced) ;
      s = (poss_concord_agr!(Third C17 Sg)!np.i) ++BIND++ (np.s!NReduced) ;
      -- asp = Null ;
      reqLocS = False
    } ;

    -- -- NOTE: this seems to be a specific construction. Not yet found in Poulos+Msimang
    -- KwaAdvNPAdv adv np =
    --   let
    --     c = case np.agr of {
    --       (First _ | Second _) => C1_2 ; -- people class as default
    --       Third c _ => c
    --     } ;
    --     n = case np.agr of {
    --       (First Sg | Second Sg | Third _ Sg) => Sg ;
    --       (First Pl | Second Pl | Third _ Pl) => Pl
    --     }
    --   in
    --   {
    --     s = adv.s ++ poss_concord!C15!Sg!(initNP np.isPron np.agr) ++BIND++ np.s!Reduced ++ np.mod ++ np.predet_pre ++ np.predet_post ;
    --     -- asp = adv.asp ;
    --     reqLocS = False
    --   } ;

    -- locative ku
    KuNPAdv np = {
      s = case np.isPron of {
        True => "ki" ;
        False => case (initNP np.isPron np.agr) of {
          -- RI  => "ki" ;
          RO  => "ko" ;
          RA  => "kw" ;
          _   => "ku"
        }
      }
      ++BIND++ (np.s!NReduced) ;
      -- asp = Null ;
      reqLocS = False
    } ;

    KuAdvNPAdv adv np = {
      s = adv.s ++
        case np.proDrop of {
          True => "ki" ;
          False => case (initNP np.isPron np.agr) of {
            RI  => "ki" ;
            RO  => "ko" ;
            RA  => "kw" ;
            _   => "ku"
          }
        }
      ++BIND++ (np.s!NReduced) ;
      -- asp = Null ;
      reqLocS = False
    } ;

    NaNPAdv np = {
      s = withPref ! (initNP np.isPron np.agr) ++BIND++ (np.s!NReduced) ;
      -- asp = Null ;
      reqLocS = False
    } ;

    RelAdv adv = {
      s = \\a => relConcLookup!a!RC ++BIND++ adv.s
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

    -- QuantRS quant = {
    --   s = \\a => relConcLookup!a!RC ++BIND++ quantConc!a ++BIND++ quant.s
    -- } ;
    --
    -- RelRS rel = {
    --   s = \\a => relConcLookup!a!RC ++BIND++ rel.s
    -- } ;

    -- QuantCN quant cn = {
    --   empty = cn.empty ;
    --   s = \\num,nform =>
    --     let
    --       agr = Third cn.c num
    --     in
    --     case quant.isPost of {
    --       True => cn.s ! num ! nform ++ quantConc!agr ++BIND++ quant.s ;
    --       False => quantConc!agr ++BIND++ quant.s ++ cn.s ! num ! nform
    --     } ;
    --   c = cn.c
    -- } ;
    -- let
    --   cn_agr = Third cn.c quant.n
    -- in
    -- {
    --   empty = cn.empty ;
    --   s = \\p => case quant.isPost of {
    --     True => cn.s ! quant.n ! p ++ quantConc!cn_agr ++BIND++ quant.s ;
    --     False => quantConc!cn_agr ++BIND++ quant.s ++ cn.s ! quant.n ! p
    --   } ;
    --   loc = quantConc!cn_agr ++BIND++ quant.s ++ cn.loc ! quant.n ;
    --   desc = cn.desc ! quant.n ;
    --   det = cn.empty ;
    --   poss = poss_concord!cn.c!quant.n!(initNP False cn_agr) ++ cn.s ! quant.n ! Reduced ;
    --   agr = cn_agr ;
    --   proDrop = False ;
    --   isPron = False ;
    --   reqLocS = False ;
    --   qdef = Article Def
    -- } ;

    NumAdjCN cn a = {
      s = \\num,nform => cn.s!num!nform ++ "na" ++BIND++ a.s!AF2 ;
      -- loc = cn.loc ;
      -- desc = \\num =>
      --   let
      --     agr = Third cn.c num ;
      --   in
      --     cn.desc ! num ++ "na" ++BIND++ a.s!AF2 ;
      c = cn.c ;
      empty = cn.empty ++ a.empty
    } ;

    only_QuantStem = {
      s = table {
        Third C1_2 Sg => "yedwa" ;
        Third C1_2 Pl => "bodwa" ;
        Third C1a_2a Sg => "yedwa" ;
        Third C1a_2a Pl => "bodwa" ;
        Third C3_4 Sg  => "wodwa" ;
        Third C3_4 Pl => "yodwa" ;
        Third C5_6 Sg => "lodwa" ;
        Third C5_6 Pl => "odwa" ;
        Third C7_8 Sg => "sodwa" ;
        Third C7_8 Pl => "zodwa" ;
        Third C9_10 Sg => "yodwa" ;
        Third C9_10 Pl => "zodwa" ;
        Third C11_10 Sg => "lodwa" ;
        Third C11_10 Pl => "zodwa" ;
        Third C9_6 Sg => "yodwa" ;
        Third C9_6 Pl => "odwa" ;
        Third C14 _ => "bodwa" ;
        Third C15 _ => "kodwa" ;
        Third C17 _ => "kodwa" ;
        First Sg => "ngedwa" ;
        First Pl => "sodwa" ;
        Second Sg  => "wedwa" ;
        Second Pl => "nodwa"
      }
    } ;
    all_QuantStem = {
      s = table {
        Third C1_2 Sg => "wonke" ;
        Third C1_2 Pl => "bonke" ;
        Third C1a_2a Sg => "wonke" ;
        Third C1a_2a Pl => "bonke" ;
        Third C3_4 Sg  => "wonke" ;
        Third C3_4 Pl => "yonke" ;
        Third C5_6 Sg => "lonke" ;
        Third C5_6 Pl => "onke" ;
        Third C7_8 Sg => "sonke" ;
        Third C7_8 Pl => "zonke" ;
        Third C9_10 Sg => "yonke" ;
        Third C9_10 Pl => "zonke" ;
        Third C11_10 Sg => "lonke" ;
        Third C11_10 Pl => "zonke" ;
        Third C9_6 Sg => "yonke" ;
        Third C9_6 Pl => "onke" ;
        Third C14 _ => "bonke" ;
        Third C15 _ => "konke" ;
        Third C17 _ => "konke" ;
        First Sg => "ngenke" ;
        First Pl => "sonke" ;
        Second Sg  => "wenke" ;
        Second Pl => "nonke"
      }
    } ;
    -- all_pre_Predet = { s = "nke" ; isPost = False } ;
    painful_RelStem = { s = "buhlungu" } ;

    -- TPerfPast = { s = [] ; t = Relative PerfTense PastTense } ;
    -- TPresPres = { s = [] ; t = PresTense } ;
    -- TPastPres = { s = [] ; t = Relative PastTense PresTense } ;
    -- TPastPerf = { s = [] ; t = Relative PastTense PerfTense } ;

    PredNP np = cl_with_np_predicate np ;

    -- IAdvQS np iadv = {
    --   s = case np.proDrop of {
    --     True => np.empty ;
    --     False => np.s ! Full ++ np.desc
    --     } ;
    --   qword_pre = case iadv.postIAdv of {
    --     False => let
    --               vform = VFIndic Princ Pos PresTense Null
    --             in
    --               (subjConc vform np.agr False) ++ iadv.s ;
    --     True => []
    --   } ;
    --   qword_post = case iadv.postIAdv of {
    --     True => let
    --               vform = VFIndic Princ Pos PresTense Null
    --             in
    --               (subjConc vform np.agr False) ++ iadv.s ;
    --     False => []
    --   } ;
    -- } ;

    AdvQCl adv qcl = {
      s = \\p,t,m => qcl.s!p!t!m ++ adv.s ;
      potqcl = \\p,m => qcl.potqcl!p!m ++ adv.s ;
      qword_pre = qcl.qword_pre ;
      qword_post = qcl.qword_post
    } ;

    ComplVAux vaux vp = {
        s = vp.s ;
        perfSuff = vp.perfSuff ;
        suff = vp.suff ;
        oc = vp.oc ;
        comp = vp.comp ;
        iadv = vp.iadv ;
        advs = vp.advs ;
        hasComp = vp.hasComp ;
        r = vp.r ;
        syl = vp.syl ;
        asp = vp.asp ;
        asp_pref = vp.asp_pref ;
        vptype = vp.vptype ;
        comp_agr = vp.comp_agr ;
        ap_comp = vp.ap_comp ;
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
      s = conj.s ++ s.s;
      -- asp = Null ;
      reqLocS = False
    } ;

    where_ConjN = { s = "lapho" } ;

    -- IAdvVP vp iadv = {
    --   s = vp.s ;
    --   -- perfSuff = vp.perfSuff ;
    --   -- suff = vp.suff ;
    --   -- oc = vp.oc ;
    --   iadv = vp.iadv ++ iadv.s ;
    --   comp = vp.comp ;
    --   advs = vp.advs ;
    --   hasComp = True ;
    --   r = vp.r ;
    --   -- syl = vp.syl ;
    --   -- asp = vp.asp ;
    --   -- asp_pref = vp.asp_pref ;
    --   vptype = vp.vptype -- ;
    --   -- comp_agr = vp.comp_agr ;
    --   -- ap_comp = vp.ap_comp ;
    --   -- aux_root = vp.aux_root ;
    --   -- hasAux = vp.hasAux
    -- } ;

    it3_Pron = mkPron (Third C3_4 Sg) ;
    they4_Pron = mkPron (Third C3_4 Pl) ;
    it5_Pron = mkPron (Third C5_6 Sg) ;
    they6_Pron = mkPron (Third C5_6 Pl) ;
    it7_Pron = mkPron (Third C7_8 Sg) ;
    they8_Pron = mkPron (Third C7_8 Pl) ;
    it9_Pron = mkPron (Third C9_10 Sg) ;
    they10_Pron = mkPron (Third C9_10 Pl) ;
    it11_Pron = mkPron (Third C11_10 Sg) ;
    it14_Pron = mkPron (Third C14 Sg) ;
    it15_Pron = mkPron (Third C15 Sg) ;
    it17_Pron = mkPron (Third C17 Sg) ;

    yonder_Quant = { s = [] ; dist = Dem3 } ;

    at_which_IAdv np = {
      s = "nga" ++BIND++ atwhichPhiPref!np.agr ++BIND++ "phi" ++ (np.s!NFull) ;
      postIAdv = False
    } ;

    what_IAdv = {s = BIND++"ni" ; postIAdv = True } ;

    how_many_IAdj = regAdj "ngaki" ;

    -- IAdjIAdv np iadj = {
    --   s = (np.s!Loc) ++ adjConcLookup!np.agr ++BIND++ iadj.s!(aformN np.agr) ;
    --   postIAdv = False
    -- } ;

    how_IComp = { s = "njani" ; postIComp = False } ; -- -njani
    where_IComp = { s = "phi" ; postIComp = True } ; -- -phi
    how_much_IComp = { s = "ngakanani" ; postIComp = False } ; -- -ngakanani

    how2_IAdv = {s = "anjani" ; postIAdv = False } ;
    how8much2_IAdv = {s = "angakanani" ; postIAdv = False } ;

    phakathi_LocN = { s = "phakathi" ; empty = [] } ;
    phansi_LocN = { s = "phansi" ; empty = [] } ;
    phesheya_LocN = { s = "phesheya" ; empty = [] } ;
    phandle_LocN = { s = "phandle" ; empty = [] } ;
    phambili_LocN = { s = "phambili" ; empty = [] } ;
    phambi_LocN = { s = "phambi" ; empty = [] } ;
    phakade_LocN = { s = "phakade" ; empty = [] } ;
    phezulu_LocN = { s = "phezulu" ; empty = [] } ;

    lapha_Loc = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t ;
          pcp = ap_cop_pref vform a RelType ; -- u-
          cop_base = "lapha"
        in
          case vform of {
            VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
            VFIndic _ _ _ => pcp ++ cop_base
          } ;
        RelCl => \\a,p,t => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop vform a RC) ; -- o- / onge-
          pcp = ap_cop_pref vform a RelType ; -- [] / zoba
          cop_base = "lapha" -- lapha
        in
        case vform of {
          VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
          VFIndic _ _ _ => rcp ++ pcp ++ cop_base
        }
      }
    } ;

    khona_Loc = {
      s = \\c,a,p,t => kho_cop (VFIndic c p t) a ;
    } ;

    kakhulu_Adv = { s = "kakhulu" ; reqLocS = False } ;

    AdvQS adv qs = { s = adv.s ++ qs.s ; qword_pre = [] ; qword_post = [] } ;

    -- Deverb15 v =
    -- let
    --   agr = Third C15 Sg ;
    -- in
    -- {
    --   s = \\_ => table {
    --     Full => case v.r of {
    --       RC => "uku"++BIND++(v.s!R_a) ;
    --       (RA|RE) => "ukw"++BIND++(v.s!R_a) ;
    --       _ => "uk"++BIND++(v.s!R_a)
    --     } ;
    --     Reduced => case v.r of {
    --       RC => "ku"++BIND++(v.s!R_a) ;
    --       (RA|RE) => "kw"++BIND++(v.s!R_a) ;
    --       _ => "k"++BIND++(v.s!R_a)
    --     } ;
    --     Poss => case v.r of {
    --       RC => "ku"++BIND++(v.s!R_a) ;
    --       (RA|RE) => "kw"++BIND++(v.s!R_a) ;
    --       _ => "k"++BIND++(v.s!R_a)
    --     } ;
    --     Loc => case v.r of {
    --       RC => "eku"++BIND++(v.s!R_e)++BIND++"ni" ;
    --       (RA|RE) => "ekw"++BIND++(v.s!R_e)++BIND++"ni" ;
    --       _ => "ek"++BIND++(v.s!R_e)++BIND++"ni"
    --     }
    --   } ;
    --   c = C15 ;
    --   empty = []
    -- } ;

  oper
    -- qcl_np_iadv : NP -> IAdv -> {s : Polarity => ZTense => DMood => Str ; potqcl : Polarity => DMood => Str ; qword_pre : Str ; qword_post : Str } = \np,iadv -> {
    --   s = \\p,t,dm =>
    --     let
    --       subj = case np.proDrop of {
    --         True => np.empty ;
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
    --       vform_aux = VFIndic dm p aux_tense Null ;
    --       vform_main = VFIndic dm p main_tense Null ;
    --       aux = case t of {
    --         Absolute bt => [] ;
    --         Relative _ _ => relSubjConc aux_tense np.agr -- (subjConcLookup!np.agr!SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False)
    --       } ;
    --     in
    --       subj ++
    --       aux ++
    --       (subjConc vform_main np.agr False) ++
    --       iadv.s ;
    --   potqcl = \\p,dm =>
    --     let
    --       subj = case np.proDrop of {
    --         True => np.empty ;
    --         False => np.s ! Full ++ np.desc
    --       } ;
    --       vform_main = VFPot dm p Null ;
    --     in
    --       subj ++
    --       -- aux ++
    --       (subjConc vform_main np.agr False) ++
    --       (potPref vform_main) ++
    --       iadv.s ;
    --   qword_pre = [] ;
    --   qword_post = []
    -- } ;

    cl_with_np_predicate : NP -> { s : Polarity => BasicTense => Str } = \np -> {
      -- advs = [] ;
      s = \\p,t =>
        let
          vform_main = VFIndic MainCl p t ;
          --pcp = pre_cop_pref vform_main np.agr ;
          cp = id_cop_pref np.agr ;
          cb = np.s!NFull
        in
          cp ++BIND++
          cb
    } ;

}
