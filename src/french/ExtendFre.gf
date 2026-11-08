--# -path=alltenses:../common:../abstract

concrete ExtendFre of Extend =
  CatFre ** ExtendFunctor -
   [
----   iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, youPolFem_Pron, youPolPl_Pron, youPolPlFem_Pron,
   GenRP,
   ExistCN, ExistMassCN, ExistPluralCN, RNP, ReflRNP, ReflPron,
   PassVPSlash, PassAgentVPSlash, PastPartAP, PastPartAgentAP, ApposNP, CompoundN,
   BaseVPS, ConsVPS, PredVPS, MkVPS, ConjVPS, RelVPS, ExistsNP,
   BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
   ProgrVPSlash, ReflPoss, CompoundAP, GerundCN, GerundNP, GerundAdv,
   ByVP, InOrderToVP, PositAdVAdj, TPastSimple
   ]                   -- put the names of your own definitions here
  with
    (Grammar = GrammarFre) **
  open
    GrammarFre,
    ResFre,
    MorphoFre,
    PhonoFre,
    Coordination,
    Prelude,
    ParadigmsFre,
    (P = ParamX) in {
    -- put your own definitions here

lincat
  RNP = {s : Agr => Case => Str} ;

lin
    GenRP nu cn = {
      s = \\_b,_aagr,_c => "dont" ++ num ++ artDef False g n Nom ++ cn.s ! n ;
      a = aagr g n ;
      hasAgr = True
      } where {
        g = cn.g ;
        n = nu.n ;
        num = if_then_Str nu.isNum (nu.s ! g) []
      } ;

   ExistCN cn =
      let
         pos = ExistNP (DetCN (DetQuant IndefArt NumSg) cn) ;
         neg = ExistNP (DetCN (DetQuant de_Quant NumSg) cn) ;
      in posNegClause pos neg PNeg.p ;
   ExistMassCN cn =
      let
         pos = ExistNP (MassNP cn) ;
         neg = ExistNP (DetCN (DetQuant de_Quant NumSg) cn) ;
      in posNegClause pos neg PNeg.p ;
   ExistPluralCN cn =
      let
         pos = ExistNP (DetCN (DetQuant IndefArt NumPl) cn) ;
         neg = ExistNP (DetCN (DetQuant de_Quant NumPl) cn) ;
      in posNegClause pos neg PNeg.p ;

oper
  de_Quant : Quant = IndefArt ** {s = \\_,_,_,_ => elisDe} ;

lin PassVPSlash vps = passVPSlash vps [] ;
    PassAgentVPSlash vps np = passVPSlash 
      vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

    PastPartAP vps = pastPartAP vps [] ;
    PastPartAgentAP vps np = pastPartAP vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

    ReflRNP v rnp =      -- VPSlash -> RNP -> VP ; -- love my family and myself
      case v.c2.isDir of {
        True  => insertRefl v ;
        False => insertComplement
                   (\\a => let agr = verbAgr a in v.c2.s ++ rnp.s ! agr ! v.c2.c) v
      } ;

    ReflPron = {         -- RNP ; -- myself
      s = \\agr,c => reflPron agr.n agr.p c
    } ;

    ReflPoss num cn = {
      s = \\agr,c => possCase cn.g num.n c ++
                       possDet agr num.n cn.g ++ cn.s ! num.n
      } ;

    AdvRNP np prep rnp = {
      s = \\agr,c => (np.s ! c).ton ++ prep.s ++ rnp.s ! agr ! prep.c
      } ;

    AdvRVP vp prep rnp =
      insertComplement (\\a => prep.s ++ rnp.s ! verbAgr a ! prep.c) vp ;

    AdvRAP ap prep rnp = ap ** {
      s = \\af => ap.s ! af ++ prep.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! prep.c ;
      isPre = False
      } ;

    ReflA2RNP a rnp = a ** {
      s = \\af => a.s ! af ++ a.c2.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! a.c2.c ;
      isPre = False
      } ;

    PossPronRNP pron num cn rnp = heavyNP {
      s = \\c => possCase cn.g num.n c ++ pron.poss ! num.n ! cn.g ++
                  cn.s ! num.n ++ rnp.s ! pron.a ! (CPrep P_de) ;
      a = agrP3 cn.g num.n ;
      hasClit = False ;
      isNeg = False
      } ;

oper
    possDet : Agr -> Number -> Gender -> Str ;
    possDet = \agr,n,g ->
      case <agr.n,agr.p> of {
        <Sg,P1> => i_Pron.poss ! n ! g ;
        <Sg,P2> => youSg_Pron.poss ! n ! g ;
        <Sg,P3> => he_Pron.poss ! n ! g ;
        <Pl,P1> => we_Pron.poss ! n ! g ;
        <Pl,P2> => youPl_Pron.poss ! n ! g ;
        <Pl,P3> => they_Pron.poss ! n ! g
        } ;

oper
    passVPSlash : VPSlash -> Str -> VP = \vps, agent -> 
      let auxvp = predV auxPassive 
      in
      vps ** {
         s = auxvp.s ;
         agr = auxvp.agr ;
         comp  = \\a => (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ vps.comp ! a ++ agent ;
        } ;

    pastPartAP : VPSlash -> Str -> AP ;
    pastPartAP vps agent = lin AP {
      s = \\af => vps.s.s ! VPart (aform2gender af) (aform2number af) ++ vps.comp ! (aform2aagr af ** {p = P3}) ++ agent ;
      isPre = False ;
      copTyp = serCopula
      } ;

lin ApposNP np1 np2 = np1 ** {    -- guessed by KA
      s = \\c => np1.s ! c ** {ton  =(np1.s ! c).ton  ++ "," ++ (np2.s ! Nom).ton;
                               comp =(np1.s ! c).comp ++ "," ++ (np2.s ! Nom).comp
                              } ;
    } ;

lin
    PresPartAP vp = {
      s = \\af => gerVP vp RPos (aform2aagr af ** {p = P3}) ;
      isPre = False ;
      copTyp = serCopula
      } ;

    ProgrVPSlash vps = GrammarFre.ProgrVP (lin VP vps) ** {c2 = vps.c2} ;

    CompoundAP noun adj = {
      s = \\af => adj.s ! af ++ "de" ++ noun.s ! (aform2number af) ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    GerundNP vp = let agr = Ag Masc Sg P3 in heavyNP {
      s = \\_ => infVP vp RPos agr ;
      a = agr
      } ;

    GerundCN vp = {
      s = \\n => infVP vp RPos (Ag Masc n P3) ;
      g = Masc
      } ;

    GerundAdv vp = {s = "en" ++ gerVP vp RPos (Ag Masc Sg P3)} ;
    ByVP vp = {s = "en" ++ gerVP vp RPos (Ag Masc Sg P3)} ;
    InOrderToVP vp = {s = "afin de" ++ infVP vp RPos (Ag Masc Sg P3)} ;
    PositAdVAdj a = {s = a.s ! AA} ;

  lincat
    VPI = {s : Agr => Str} ;
    [VPI] = {s1,s2 : Agr => Str} ;
    [Comp] = {s1,s2 : Agr => Str ; cop : CopulaType} ;
    [Imp] = {s1,s2 : RPolarity => P.ImpForm => Gender => Str} ;

  lin
    MkVPI vp = {s = \\a => infVP vp RPos a} ;
    BaseVPI = twoTable Agr ;
    ConsVPI = consrTable Agr comma ;
    ConjVPI = conjunctDistrTable Agr ;
    ComplVPIVV vv vpi =
      insertComplement (\\a => prepCase vv.c2.c ++ vpi.s ! a) (predV vv) ;

    BaseComp x y = twoTable Agr x y ** {cop = x.cop} ;
    ConsComp xs x = consrTable Agr comma xs x ** xs ;
    ConjComp conj cs = conjunctDistrTable Agr conj cs ** {cop = cs.cop} ;

    BaseImp = twoTable3 RPolarity P.ImpForm Gender ;
    ConsImp = consrTable3 RPolarity P.ImpForm Gender comma ;
    ConjImp = conjunctDistrTable3 RPolarity P.ImpForm Gender ;

    TPastSimple = {s = []} ** {t = RPasse} ; --# notpresent

lin CompoundN a b = lin N {
      s = \\n => b.s ! n ++
                 case b.relType of {
                   NRelPrep p => prepCase (CPrep p) ++ a.s ! Sg ;  -- tasa de suicidio
                   NRelNoPrep => a.s ! n               -- connessione internet = internet connection
                 } ;
      g = b.g ;
      relType = b.relType
      } ;

lin UseDAP = \dap ->
      let
        g = Masc ;
        n = dap.n
      in heavyNPpol dap.isNeg {
        s = dap.spn ;
        a = agrP3 g n ;
        hasClit = False
        } ;
    UseDAPMasc = \dap ->
      let
        g = Masc ;
        n = dap.n
      in heavyNPpol dap.isNeg {
        s = dap.sp ! g ;
        a = agrP3 g n ;
        hasClit = False
        } ;
    UseDAPFem dap =
      let
        g = Fem ;
        n = dap.n
      in heavyNPpol dap.isNeg {
           s = dap.sp ! g ;
           a = agrP3 g n ;
           hasClit = False
           } ;

  lincat
    VPS = {s : Mood => Agr => Bool => Str} ;
    [VPS] = {s1,s2 : Mood => Agr => Bool => Str} ;

  lin
    BaseVPS x y = twoTable3 Mood Agr Bool x y ;
    ConsVPS = consrTable3 Mood Agr Bool comma ;

  lin
    PredVPS np vpi = {
      s = \\m => (np.s ! Nom).comp ++ vpi.s ! m ! np.a ! np.isNeg
      } ;
    MkVPS tm p vp = {
      s = \\m,agr,isNeg =>
        tm.s ++ p.s ++
        (mkClausePol (orB isNeg vp.isNeg) [] False False agr vp).s
          ! DDir ! tm.t ! tm.a ! p.p ! m
      } ;
    ConjVPS = conjunctDistrTable3 Mood Agr Bool ;
    
    RelVPS rp vpi = {
      s = \\m, agr => rp.s ! False ! complAgr agr ! Nom ++ vpi
                      .s ! m ! (Ag rp.a.g rp.a.n P3) ! False ;
      c = Nom
      } ;

    ExistsNP np =
      mkClause "il" True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV (mkV "exister"))) ;

}
