--# -path=alltenses:../common:../abstract

concrete ExtendFre of Extend =
  CatFre ** ExtendFunctor -
   [
----   iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, youPolFem_Pron, youPolPl_Pron, youPolPlFem_Pron,
   GenRP,
   ExistCN, ExistMassCN, ExistPluralCN, RNP, ReflRNP,
   PassVPSlash, PassAgentVPSlash, PastPartAP, PastPartAgentAP, ApposNP, CompoundN,
   BaseVPS, ConsVPS, PredVPS, MkVPS, ConjVPS, RelVPS, ExistsNP
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
    ParadigmsFre in {
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

lin CompoundN a b = lin N {
      s = \\n => b.s ! n ++
                 case b.relType of {
                   NRelPrep p => prepCase (CPrep p) ;  -- tasa de suicidio
                   NRelNoPrep => []                    -- connessione internet = internet connection
                 } ++
                 a.s ! Sg ;
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
