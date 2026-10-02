--# -path=.:../common:../abstract

concrete ExtendPol of Extend =
  CatPol ** ExtendFunctor - [
    iFem_Pron, youFem_Pron, theyFem_Pron, ProDrop, PassVPSlash, ExistsNP,
    VPS, BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS,
    VPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
    PresPartAP, PastPartAP, PastPartAgentAP, PassAgentVPSlash,
    ProgrVPSlash, CompoundN, CompoundAP, GerundCN, GerundNP,
    GerundAdv, ByVP, InOrderToVP, ApposNP, PositAdVAdj,
    ReflRNP, ReflPron, ReflPoss, PredetRNP, AdvRNP, AdvRVP,
    AdvRAP, ReflA2RNP, PossPronRNP, ReflPossPron
  ]
  with
    (Grammar = GrammarPol) **
  open PronounMorphoPol, ResPol, VerbMorphoPol, Prelude in {

lincat
  VPS = {s : GenNum => Person => Str} ;
  [VPS] = {s1,s2 : GenNum => Person => Str} ;
  VPI = {s : GenNum => Str} ;
  [VPI] = {s1,s2 : GenNum => Str} ;

-- ExtendFunctor defaults ExistsNP to ExistNP, which gives "jest macierz".
-- Polish distinguishes the two: "there is" is jest/są, but "there exists" is
-- istnieć, which is the form mathematical prose uses.
oper
  istniec_V : Verb = mkMonoVerb "istnieć" conj52 Imperfective ;

lin ExistsNP np = {
    s = \\pol,anter,tense =>
      (indicative_form istniec_V False pol) ! <tense, anter, np.gn, np.p> ++ np.nom
  } ;

lin iFem_Pron = pronJa FemSg ;
lin youFem_Pron = pronTy FemSg ;
lin theyFem_Pron = pronOneFem ;

lin ProDrop p = {
    nom = [] ;
    voc = p.voc ;
    dep = p.dep ;
    sp = p.sp ;
    p  = p.p ;
    gn = p.gn ;
 } ;

lin
  UseDAP = dap2np Neut ;
  UseDAPMasc = dap2np (Masc Personal) ;
  UseDAPFem = dap2np Fem ;

lin
  MkVPS temp pol vp = {
    s = \\gn,p => temp.s ++ pol.s ++ vp.prefix ++
      (indicative_form vp.verb vp.imienne pol.p ! <temp.t,temp.a,gn,p>) ++
      vp.sufix ! pol.p ! gn
    } ;

  BaseVPS x y = {s1=x.s; s2=y.s} ;
  ConsVPS x xs = {
    s1 = \\gn,p => x.s ! gn ! p ++ "," ++ xs.s1 ! gn ! p;
    s2 = xs.s2
    } ;
  ConjVPS conj xs = {
    s = \\gn,p => conj.s1 ++ xs.s1 ! gn ! p ++ conj.s2 ++ xs.s2 ! gn ! p
    } ;
  PredVPS np vps = {s = np.nom ++ vps.s ! np.gn ! np.p} ;

  MkVPI vp = {
    s = \\gn => vp.prefix ++ infinitive_form vp.verb vp.imienne Pos gn ++
      vp.sufix ! Pos ! gn
    } ;
  BaseVPI x y = {s1=x.s; s2=y.s} ;
  ConsVPI x xs = {
    s1 = \\gn => x.s ! gn ++ "," ++ xs.s1 ! gn;
    s2 = xs.s2
    } ;
  ConjVPI conj xs = {
    s = \\gn => conj.s1 ++ xs.s1 ! gn ++ conj.s2 ++ xs.s2 ! gn
    } ;
  ComplVPIVV vv vpi = setSufix (defVP vv) (\\_,gn => vpi.s ! gn) ;

  PresPartAP vp = {
    s = \\af => case af of {
      AF gn _ => (mkAtable (table2record vp.verb.apart)) ! af ++
                 vp.sufix ! Pos ! gn
      };
    adv = vp.verb.si ! VInfM ++ vp.sufix ! Pos ! MascPersSg;
    isPost = False
    } ;

  PastPartAP vps = {
    s = \\af => case af of {
      AF gn _ => (mkAtable (table2record vps.verb.ppartp)) ! af ++
                 vps.sufix ! Pos ! gn ++ vps.postfix ! Pos ! gn
      };
    adv = vps.verb.si ! VInfM;
    isPost = False
    } ;

  PastPartAgentAP vps np = {
    s = \\af => case af of {
      AF gn _ => (mkAtable (table2record vps.verb.ppartp)) ! af ++
                 vps.sufix ! Pos ! gn ++ vps.postfix ! Pos ! gn ++
                 "przez" ++ np.dep ! AccPrep
      };
    adv = vps.verb.si ! VInfM ++ "przez" ++ np.dep ! AccPrep;
    isPost = False
    } ;

  PassAgentVPSlash vps np = setSufix (setImienne vps True)
    (\\p,gn => vps.sufix ! p ! gn ++ vps.postfix ! p ! gn ++
               "przez" ++ np.dep ! AccPrep) ;

  ProgrVPSlash vps = vps ;

  CompoundN modifier head = {
    s = \\sf => case sf of {
      SF n c => head.s ! sf ++ modifier.s ! SF Sg Gen
      };
    g = head.g
    } ;

  CompoundAP noun adj = {
    s = \\af => noun.s ! SF Sg Instr ++ (mkAtable adj.pos) ! af;
    adv = noun.s ! SF Sg Instr ++ adj.advpos;
    isPost = False
    } ;

  GerundCN vp = {
    s = \\n,c => vp.prefix ++ vp.verb.ger ! SF n c ++
                  vp.sufix ! Pos ! NeutSg;
    g = Neut
    } ;
  GerundNP vp = {
    nom = vp.prefix ++ vp.verb.ger ! SF Sg Nom ++
              vp.sufix ! Pos ! NeutSg;
    voc = vp.prefix ++ vp.verb.ger ! SF Sg VocP ++
              vp.sufix ! Pos ! NeutSg;
    dep = \\cc => vp.prefix ++ vp.verb.ger ! SF Sg (extract_case ! cc) ++
                 vp.sufix ! Pos ! NeutSg;
    gn = NeutSg; p = P3
    } ;
  GerundAdv vp = {
    s = "podczas" ++ vp.prefix ++ vp.verb.ger ! SF Sg Gen ++
        vp.sufix ! Pos ! NeutSg
    } ;
  ByVP vp = {
    s = "przez" ++ vp.prefix ++ vp.verb.ger ! SF Sg Acc ++
        vp.sufix ! Pos ! NeutSg
    } ;
  InOrderToVP vp = {
    s = "aby" ++ vp.prefix ++ infinitive_form vp.verb vp.imienne Pos NeutSg ++
        vp.sufix ! Pos ! NeutSg
    } ;

  ApposNP a b = {
    nom = a.nom ++ "," ++ b.nom;
    voc = a.voc ++ "," ++ b.voc;
    dep = \\c => a.dep ! c ++ "," ++ b.dep ! c;
    gn = a.gn; p = a.p
    } ;
  PositAdVAdj a = {s = a.advpos} ;

  ReflPron = lin RNP (lin NP (pronRefl NeutSg)) ;
  ReflPoss num cn = lin RNP (DetCN (DetQuant (PossPron (pronRefl NeutSg)) num) cn) ;
  PredetRNP pred rnp = lin RNP (PredetNP pred (lin NP rnp)) ;
  AdvRNP np prep rnp = lin RNP (AdvNP np (PrepNP prep (lin NP rnp))) ;
  AdvRVP vp prep rnp = AdvVP vp (PrepNP prep rnp) ;
  AdvRAP ap prep rnp = AdvAP ap (PrepNP prep rnp) ;
  ReflRNP vps rnp = ComplSlash vps (lin NP rnp) ;
  ReflA2RNP a rnp = ComplA2 a (lin NP rnp) ;
  PossPronRNP pron num cn rnp =
    DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP rnp)) ;
  ReflPossPron = PossPron (pronRefl NeutSg) ;

oper
  dap2np : Gender -> DAP -> NP ;
  dap2np g dap = lin NP {
    nom = dap.sp ! Nom  ! g;
    voc = dap.sp ! VocP ! g;
    dep = \\cc => let c = extract_case ! cc
                  in dap.sp ! c ! g;
    gn = accom_gennum ! <dap.a, g, dap.n>;
    p = P3        
  };

-- KA: PassVPSlash is derived from PassV2. Objects might be ignored
lin PassVPSlash vps = setImienne vps True; 

}
