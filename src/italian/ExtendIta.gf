--# -path=alltenses:../common:../abstract:../romance
concrete ExtendIta of Extend = CatIta ** ExtendRomanceFunctor  -
   [
   EmptyRelSlash,
   BaseVPI, ConsVPI, ConjVPI, MkVPI, ComplVPIVV,
   BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
   RNP, ReflRNP, ReflPron, ReflPoss, PredetRNP,
   AdvRNP, AdvRVP, AdvRAP, PossPronRNP,
   PresPartAP, GerundNP, GerundCN,
   CompoundAP, InOrderToVP, CompVP,
   GenRP,
   PassVPSlash, PassAgentVPSlash,
   ExistsNP

   ]
  -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarIta), (Syntax = SyntaxIta), (ResRomance = ResIta) **
  open
  GrammarIta,
  ResIta,
  MorphoIta,
  Coordination,
  Prelude,
  ParadigmsIta,
  IrregIta,
  (P = ParamX) in {
    -- put your own definitions here

lin
    EmptyRelSlash = RelSlash IdRP ;

  lincat
    RNP = {s : Agr => Case => Str ; isPron : Bool} ;
    VPI = {s : Agr => Str} ;
    [VPI] = {s1,s2 : Agr => Str} ;

  lin
    MkVPI vp = {s = \\a => infVP vp RPos a} ;
    BaseVPI = twoTable Agr ;
    ConsVPI = consrTable Agr comma ;
    ConjVPI = conjunctDistrTable Agr ;
    ComplVPIVV vv vpi =
      insertComplement (\\a => prepCase vv.c2.c ++ vpi.s ! a) (predV vv) ;

  lincat [Comp] = {s1,s2 : Agr => Str ; cop : CopulaType} ;

  lin
    BaseComp x y = twoTable Agr x y ** {cop = x.cop} ;
    ConsComp xs x = consrTable Agr comma xs x ** xs ;
    ConjComp conj cs = conjunctDistrTable Agr conj cs ** {cop = cs.cop} ;

  lincat [Imp] = {s1,s2 : RPolarity => P.ImpForm => Gender => Str} ;

  lin
    BaseImp = twoTable3 RPolarity P.ImpForm Gender ;
    ConsImp = consrTable3 RPolarity P.ImpForm Gender comma ;
    ConjImp = conjunctDistrTable3 RPolarity P.ImpForm Gender ;

    -- The simple form is idiomatic in the relative and interrogative
    -- contexts where a progressive VPSlash is used, and preserves the gap.
    ProgrVPSlash vps = vps ;

    ReflRNP v rnp = case <rnp.isPron,v.c2.isDir> of {
      <True,True> => insertRefl v ;
      _ => insertComplement
             (\\a => let agr = verbAgr a in
                    v.c2.s ++ rnp.s ! agr ! v.c2.c) v
      } ;

    ReflPron = {
      s = \\agr,c => reflPron agr.n agr.p c ;
      isPron = True
      } ;

    ReflPoss num cn = {
      s = \\agr,c => possCase cn.g num.n c ++
                       possDet agr num.n cn.g ++ cn.s ! num.n ;
      isPron = False
      } ;

    PredetRNP pred rnp = {
      s = \\agr,c => pred.s ! complAgr agr ! c ++ rnp.s ! agr ! pred.c ;
      isPron = False
      } ;

    AdvRNP np prep rnp = {
      s = \\agr,c => (np.s ! c).ton ++ prep.s ++ rnp.s ! agr ! prep.c ;
      isPron = False
      } ;

    AdvRVP vp prep rnp =
      insertComplement (\\a => prep.s ++ rnp.s ! verbAgr a ! prep.c) vp ;

    AdvRAP ap prep rnp = ap ** {
      s = \\af => ap.s ! af ++ prep.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! prep.c ;
      isPre = False
      } ;

    PossPronRNP pron num cn rnp = heavyNP {
      s = \\c => possCase cn.g num.n c ++ pron.poss ! num.n ! cn.g ++
                  cn.s ! num.n ++ rnp.s ! pron.a ! ResIta.genitive ;
      a = agrP3 cn.g num.n ;
      hasClit = False ;
      isNeg = False
      } ;

    PresPartAP vp = {
      s = \\af =>
        let agr = aform2aagr af ** {p = P3} ;
        in vp.s.s ! VPresPart ++ vp.comp ! agr ++ vp.ext ! RPos ;
      isPre = False ;
      copTyp = serCopula
      } ;

    GerundNP vp = let agr = Ag Masc Sg P3 in heavyNP {
      s = \\_ => infVP vp RPos agr ;
      a = agr
      } ;

    GerundCN vp = {
      s = \\n => infVP vp RPos (Ag Masc n P3) ;
      g = Masc
      } ;

    CompoundAP noun adj = {
      s = \\af => adj.s ! af ++ "da" ++ noun.s ! (aform2number af) ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    InOrderToVP vp = {s = "per" ++ infStr vp} ;

    CompVP ant pol vp = {
      s = \\agr => ant.s ++ pol.s ++ infVP vp pol.p agr ;
      cop = serCopula
      } ;

    GenRP nu cn = {
      s = \\_b,_aagr,_c => "di cui" ++ num ++ cn.s ! n ;
      a = aagr g n ;
      hasAgr = True
      } where {
        g = cn.g ;
        n = nu.n ;
        num = if_then_Str nu.isNum (nu.s ! g) []
      } ;




lin PassVPSlash vps = passVPSlash vps [] ;
    PassAgentVPSlash vps np = passVPSlash 
      vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

oper
    possDet : Agr -> Number -> Gender -> Str = \agr,n,g ->
      case <agr.n,agr.p> of {
        <Sg,P1> => i_Pron.poss ! n ! g ;
        <Sg,P2> => youSg_Pron.poss ! n ! g ;
        <Sg,P3> => he_Pron.poss ! n ! g ;
        <Pl,P1> => we_Pron.poss ! n ! g ;
        <Pl,P2> => youPl_Pron.poss ! n ! g ;
        <Pl,P3> => they_Pron.poss ! n ! g
        } ;

    passVPSlash : VPSlash -> Str -> VP = \vps, agent -> 
      let auxvp = predV auxPassive 
      in
      vps ** {
         s = auxvp.s ;
         agr = auxvp.agr ;
         comp  = \\a => vps.comp ! a ++ (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ agent ;
        } ;


lin
    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV esistere_V)) ;



}
