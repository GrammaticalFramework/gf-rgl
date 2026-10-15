--# -path=.:../common:../abstract

concrete ExtendRon of Extend =
  CatRon ** ExtendFunctor - [
    PassVPSlash, PassAgentVPSlash, PastPartAP, PastPartAgentAP, PresPartAP,
    CompoundN, CompoundAP, GerundCN, GerundNP, GerundAdv, ByVP, InOrderToVP,
    ApposNP, PositAdVAdj, ProgrVPSlash,
    RNP, ReflRNP, ReflPron, ReflPoss, PredetRNP, AdvRNP, AdvRVP, AdvRAP,
    ReflA2RNP, PossPronRNP,
    VPS, BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS,
    VPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
    BaseComp, ConsComp, ConjComp,
    BaseImp, ConsImp, ConjImp,
    UseDAP, UseDAPFem, UseDAPMasc]
  with
    (Grammar = GrammarRon) ** 
  open ResRon, Prelude, Coordination, GrammarRon in {

lincat
  RNP = {s : Agr => NCase => Str ; isPron : Bool} ;
  VPS = {s : Mood => Agr => Str} ;
  [VPS] = {s1,s2 : Mood => Agr => Str} ;
  VPI = {s : Agr => Str} ;
  [VPI] = {s1,s2 : Agr => Str} ;
  [Comp] = {s1,s2 : Agr => Str} ;
  [Imp] = {s1,s2 : Polarity => ImpForm => Gender => Str} ;

lin iFem_Pron = mkPronoun "eu" "mine" "mie" [] [] "meu" "mea" "mei" "mele" Fem Sg P1 ;
    youFem_Pron = mkPronoun "tu" "tine" "ţie" [] "tu" "tău" "ta" "tăi" "tale"  Fem Sg P2 ;
    youPlFem_Pron = mkPronoun "voi" "voi" "vouă" [] "voi" "vostru" "voastră" "voştri" "voastre" Fem Pl P2 ;
    youPolFem_Pron =
       let dvs = mkPronoun "dumneavoastră" "dumneavoastră" "dumneavoastră" [] "dumneavoastră" "dumneavoastră" "dumneavoastră" "dumneavoastră" "dumneavoastră" Fem Pl P2 
       in {s = dvs.s; c1 = dvs.c1; 
           c2 = dvs.c2; a = dvs.a; isPol = True; poss = dvs.poss} ;  

-- KA: derived from PassV2, objects are ignored
lin PassVPSlash vps = insertSimpObj (\\a => vps.s ! PPasse a.g a.n Indef ANomAcc) auxPassive ** {lock_VP = <>};

    PassAgentVPSlash vps np =
      insertSimpObj (\\a => vps.s ! PPasse a.g a.n Indef ANomAcc ++
                            vps.comp ! a ++ "de" ++ (np.s ! No).comp)
                    auxPassive ** {lock_VP = <>} ;

    PastPartAP vps = {
      s = \\af => case af of {
        AF g n sp c => vps.s ! PPasse g n sp c ++ vps.comp ! agrP3 g n ;
        AA => vps.s ! PPasse Masc Sg Indef ANomAcc ++ vps.comp ! agrP3 Masc Sg
        } ;
      isPre = False
      } ;

    PastPartAgentAP vps np = {
      s = \\af => case af of {
        AF g n sp c => vps.s ! PPasse g n sp c ++ vps.comp ! agrP3 g n ++ "de" ++ (np.s ! No).comp ;
        AA => vps.s ! PPasse Masc Sg Indef ANomAcc ++ vps.comp ! agrP3 Masc Sg ++ "de" ++ (np.s ! No).comp
        } ;
      isPre = False
      } ;

    PresPartAP vp = {
      s = \\af => case af of {
        AF g n _ _ => vp.s ! Ger ++ vp.comp ! agrP3 g n ++ vp.ext ! Pos ;
        AA => vp.s ! Ger ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! Pos
        } ;
      isPre = False
      } ;

    ProgrVPSlash vps = vps ;

    CompoundN n1 n2 = {
      s = \\n,sp,c => n2.s ! n ! sp ! c ++ "de" ++ n1.s ! Sg ! Indef ! ANomAcc ;
      g = n2.g ; a = n2.a
      } ;

    CompoundAP n a = {
      s = \\af => a.s ! af ++ "de" ++ n.s ! Sg ! Indef ! ANomAcc ;
      isPre = False
      } ;

    GerundCN vp = {
      s = \\_,_,_ => "faptul de a" ++ vp.s ! Inf ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! Pos ;
      g = NNeut ; a = Inanimate ; isComp = True ; needsRefForm = False
      } ;

    GerundNP vp = heavyNP {
      s = \\_ => "faptul de a" ++ vp.s ! Inf ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! Pos ;
      a = agrP3 Masc Sg ; hasClit = HasRef False ;
      ss = "faptul de a" ++ vp.s ! Inf ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! Pos
      } ;

    GerundAdv vp = {s = vp.s ! Ger ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! Pos} ;
    ByVP vp = {s = "prin" ++ vp.s ! Ger ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! Pos} ;
    InOrderToVP vp = {s = "pentru a" ++ vp.s ! Inf ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! Pos} ;
    PositAdVAdj a = {s = a.s ! AA} ;

    ApposNP np1 np2 = heavyNP {
      s = \\c => (np1.s ! c).comp ++ SOFT_BIND ++ "," ++ (np2.s ! No).comp ;
      a = np1.a ; hasClit = np1.nForm ;
      ss = np1.indForm ++ SOFT_BIND ++ "," ++ np2.indForm
      } ;

    ReflPron = {
      s = \\agr,c => reflPron agr.n agr.p (convCase c) ++ reflPronHard agr.g agr.n agr.p ;
      isPron = True
      } ;

    ReflPoss num cn = {
      s = \\agr,c => cn.s ! num.n ! Def ! convCase c ++ possDet agr num.n (agrGender cn.g num.n) (convCase c) ;
      isPron = False
      } ;

    PredetRNP pred rnp = {
      s = \\agr,c => pred.s ! aagr agr.g agr.n ! convCase c ++ rnp.s ! agr ! pred.c ;
      isPron = False
      } ;

    AdvRNP np prep rnp = {
      s = \\agr,c => (np.s ! c).comp ++ prep.s ++ rnp.s ! agr ! prep.c ;
      isPron = False
      } ;

    AdvRVP vp prep rnp = insertSimpObj (\\a => prep.s ++ rnp.s ! a ! prep.c) vp ;
    AdvRAP ap prep rnp = ap ** {
      s = \\af => case af of {
        AF g n _ _ => ap.s ! af ++ prep.s ++ rnp.s ! agrP3 g n ! prep.c ;
        AA => ap.s ! af ++ prep.s ++ rnp.s ! agrP3 Masc Sg ! prep.c
        } ; isPre = False
      } ;
    ReflA2RNP ap rnp = ap ** {
      s = \\af => case af of {
        AF g n _ _ => ap.s ! af ++ ap.c2.s ++ rnp.s ! agrP3 g n ! ap.c2.c ;
        AA => ap.s ! af ++ ap.c2.s ++ rnp.s ! agrP3 Masc Sg ! ap.c2.c
        } ; isPre = False
      } ;
    ReflRNP vps rnp = insertSimpObjPre (\\a => vps.c2.s ++ rnp.s ! a ! vps.c2.c) vps ;

    PossPronRNP pron num cn rnp = heavyNP {
      s = \\c => cn.s ! num.n ! Def ! convCase c ++ pron.poss ! num.n ! (agrGender cn.g num.n) ! convCase c ++
                  "de" ++ rnp.s ! pron.a ! No ;
      a = agrP3 (agrGender cn.g num.n) num.n ; hasClit = HasRef False ;
      ss = cn.s ! num.n ! Def ! ANomAcc ++ pron.poss ! num.n ! (agrGender cn.g num.n) ! ANomAcc ++
           "de" ++ rnp.s ! pron.a ! No
      } ;

    UseDAP dap = heavyNP {
      s = dap.sp ! Masc ; a = agrP3 Masc dap.n ; hasClit = HasRef False ;
      ss = dap.sp ! Masc ! No
      } ;
    UseDAPMasc dap = heavyNP {
      s = dap.sp ! Masc ; a = agrP3 Masc dap.n ; hasClit = HasRef False ;
      ss = dap.sp ! Masc ! No
      } ;
    UseDAPFem dap = heavyNP {
      s = dap.sp ! Fem ; a = agrP3 Fem dap.n ; hasClit = HasRef False ;
      ss = dap.sp ! Fem ! No
      } ;

    BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPS x xs = {s1 = \\m,a => x.s ! m ! a ++ comma ++ xs.s1 ! m ! a ; s2 = xs.s2} ;
    MkVPS t p vp = {s = \\m,a => t.s ++ p.s ++ (mkClause "" False a vp).s ! DDir ! t.t ! t.a ! p.p ! m} ;
    ConjVPS conj xs = {s = \\m,a => conj.s1 ++ xs.s1 ! m ! a ++ conj.s2 ++ xs.s2 ! m ! a} ;
    PredVPS np vps = {s = \\m => (np.s ! No).comp ++ vps.s ! m ! np.a} ;

    BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPI x xs = {s1 = \\a => x.s ! a ++ comma ++ xs.s1 ! a ; s2 = xs.s2} ;
    MkVPI vp = {s = \\a => "a" ++ vp.s ! Inf ++ vp.comp ! a ++ vp.ext ! Pos} ;
    ConjVPI conj xs = {s = \\a => conj.s1 ++ xs.s1 ! a ++ conj.s2 ++ xs.s2 ! a} ;
    ComplVPIVV vv vpi = insertSimpObj vpi.s (useVerb vv) ;

    BaseComp x y = {s1 = x.s ; s2 = y.s} ;
    ConsComp x xs = {s1 = \\a => x.s ! a ++ comma ++ xs.s1 ! a ; s2 = xs.s2} ;
    ConjComp conj xs = {s = \\a => conj.s1 ++ xs.s1 ! a ++ conj.s2 ++ xs.s2 ! a} ;

    BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = {s1 = \\p,f,g => x.s ! p ! f ! g ++ comma ++ xs.s1 ! p ! f ! g ; s2 = xs.s2} ;
    ConjImp conj xs = {s = \\p,f,g => conj.s1 ++ xs.s1 ! p ! f ! g ++ conj.s2 ++ xs.s2 ! p ! f ! g} ;

oper
  possDet : Agr -> Number -> Gender -> ACase -> Str = \agr,n,g,c ->
    case <agr.n,agr.p> of {
      <Sg,P1> => i_Pron.poss ! n ! g ! c ; <Sg,P2> => youSg_Pron.poss ! n ! g ! c ;
      <Sg,P3> => he_Pron.poss ! n ! g ! c ; <Pl,P1> => we_Pron.poss ! n ! g ! c ;
      <Pl,P2> => youPl_Pron.poss ! n ! g ! c ; <Pl,P3> => they_Pron.poss ! n ! g ! c
      } ;

}
