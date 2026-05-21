--# -path=.:../common:../abstract

concrete ExtendHun of Extend = CatHun
  -- ** ExtendFunctor - []
  -- with (Grammar=GrammarHun)
  ** open Prelude, ResHun, NounHun, VerbHun, AdjectiveHun in {

lincat
    VPS = {s : Person => Number => Str} ;
    VPI = SS ;
    VPS2 = {s : ObjDef => Person => Number => Str ; c2 : Case} ;
    [VPS2] = {s1,s2 : ObjDef => Person => Number => Str ; c2 : Case} ;
    VPI2 = {s : ObjDef => Str ; c2 : Case} ;
    [VPI2] = {s1,s2 : ObjDef => Str ; c2 : Case} ;
    RNP = NounPhrase ;
    RNPList = {s1,s2 : Possessor => Case => Str ; agr : Person*Number ; g : Gender ; postmod : Str} ;

lin
    TPastSimple = {s = []} ** {t = Past} ;   --# notpresent

    CompoundN n1 n2 =
      n2 ** {s = \\nc => n1.s ! SgNom ++ BIND ++ n2.s ! nc} ;

    GenModNP num np cn =
      let det : Determiner = DetQuant DefArt num ;
          pron : Pronoun = pronTable ! np.agr ;
       in emptyNP ** cn ** det ** {
            s = \\_,c =>
              np.s ! NoPoss ! Nom
              ++ np.postmod
              ++ caseFromPossStem cn (DetQuant (PossPron pron) num) c
              ++ cn.compl ! det.n ! c ;
            agr = <P3,det.n> ;
            objdef = Def ;
            } ;

    UseDAP = DetNP ;
    UseDAPMasc,
    UseDAPFem = \dap -> DetNP dap ** {g = Human} ;

    MkVPS t pol vp = {
      s = \\p,n =>
        t.s ++ pol.s ++ if_then_Pol pol.p [] "nem" ++
        case t.t of {
          Past => vp.s ! VPast p n ;
          Fut  => futureAux Indef p n ++ vp.s ! VInf ;
          _    => vp.s ! VPres p n
        } ++ vp.obj ++ vp.adv
      } ;

    PredVPS np vps = {
      s = linNP np ++ vps.s ! np.agr.p1 ! np.agr.p2
      } ;

    MkVPI vp = {s = infVP vp} ;

    MkVPS2 t pol vps = {
      s = \\od,p,n =>
        t.s ++ pol.s ++ if_then_Pol pol.p [] "nem" ++
        case t.t of {
          Past => vps.s ! od ! VPast p n ;
          Fut  => futureAux od p n ++ vps.s ! od ! VInf ;
          _    => vps.s ! od ! VPres p n
        } ++ vps.adv ;
      c2 = vps.c2
      } ;

    ComplVPS2 vps np = {
      s = \\p,n => vps.s ! np.objdef ! p ! n ++ np.s ! NoPoss ! vps.c2 ++ np.postmod
      } ;

    ReflVPS2 vps rnp = {
      s = \\p,n => vps.s ! rnp.objdef ! p ! n ++ rnp.s ! NoPoss ! vps.c2 ++ rnp.postmod
      } ;

    BaseVPS2 x y = {
      s1 = x.s ;
      s2 = y.s ;
      c2 = y.c2
      } ;

    ConsVPS2 x xs = {
      s1 = \\od,p,n => x.s ! od ! p ! n ++ bindComma ++ xs.s1 ! od ! p ! n ;
      s2 = xs.s2 ;
      c2 = xs.c2
      } ;

    ConjVPS2 conj xs = {
      s = \\od,p,n => xs.s1 ! od ! p ! n ++ conj.s2 ++ xs.s2 ! od ! p ! n ;
      c2 = xs.c2
      } ;

    MkVPI2 vps = {
      s = \\_ => infVPSlash vps ;
      c2 = vps.c2
      } ;

    ComplVPI2 vpi np = {
      s = vpi.s ! np.objdef ++ np.s ! NoPoss ! vpi.c2 ++ np.postmod
      } ;

    BaseVPI2 x y = {
      s1 = x.s ;
      s2 = y.s ;
      c2 = y.c2
      } ;

    ConsVPI2 x xs = {
      s1 = \\od => x.s ! od ++ bindComma ++ xs.s1 ! od ;
      s2 = xs.s2 ;
      c2 = xs.c2
      } ;

    ConjVPI2 conj xs = {
      s = \\od => xs.s1 ! od ++ conj.s2 ++ xs.s2 ! od ;
      c2 = xs.c2
      } ;

    PresPartAP vp = emptyAP ** {
      s = \\_,_ => vp.obj ++ vp.adv ++ vp.s ! VPresPart
      } ;

    EmbedPresPart vp = {s = infVP vp} ;

    PastPartAP vps = emptyAP ** {
      s = \\_,_ => vps.adv ++ vps.s ! Indef ! VAdvPart
      } ;

    PastPartAgentAP vps np = emptyAP ** {
      s = \\_,_ =>
        vps.adv ++ vps.s ! Indef ! VAdvPart
        ++ applyAdp (caseAdp Nom "által") np
      } ;

    PassVPSlash vps = passiveVP vps [] ;

    PassAgentVPSlash vps np =
      passiveVP vps (applyAdp (caseAdp Nom "által") np) ;

    ProgrVPSlash vps = vps ;

    ComplBareVS vs s = useV vs ** {
      adv = s.s
      } ;

    ReflRNP vps rnp = insertObj vps rnp ;

    ReflPron = indeclNP "magát" ** {objdef = Def ; g = Human} ;

    ReflPoss num cn = DetCN (DetQuant DefArt num) cn ;

    PredetRNP predet rnp = rnp ** {
      s = \\p,c => predet.s ++ rnp.s ! p ! c
      } ;

    ReflA2RNP a2 rnp =
      let ap : AP = ComplA2 a2 rnp in ap ;

    PossPronRNP pron num cn rnp =
      let det : Determiner = DetQuant (PossPron pron) num
       in emptyNP ** cn ** det ** {
            s = \\_,c =>
              rnp.s ! NoPoss ! Nom
              ++ caseFromPossStem cn det c
              ++ cn.compl ! det.n ! c ;
            agr = <P3,det.n> ;
            objdef = Def ;
            g = NonHuman
          } ;

    Base_rr_RNP x y = {
      s1 = x.s ;
      s2 = y.s ;
      agr = y.agr ;
      g = y.g ;
      postmod = []
      } ;

    Base_nr_RNP x y = {
      s1 = x.s ;
      s2 = y.s ;
      agr = y.agr ;
      g = y.g ;
      postmod = []
      } ;

    Base_rn_RNP x y = {
      s1 = x.s ;
      s2 = y.s ;
      agr = y.agr ;
      g = y.g ;
      postmod = []
      } ;

    ConjRNP conj xs = emptyNP ** {
      s = \\p,c => xs.s1 ! p ! c ++ conj.s2 ++ xs.s2 ! p ! c ;
      agr = <P3,conj.n> ;
      objdef = Def ;
      g = xs.g ;
      postmod = [] ;
      empty = []
      } ;

    GerundCN vp =
      let gn : Str = infVP vp
       in {
        s = \\_ => gn ;
        h = H_a ;
        g = NonHuman ;
        compl = \\_,_ => [] ;
        postmod = []
        } ;

    GerundNP vp = MassNP (GerundCN vp) ;

    GerundAdv vp = {
      s = infVP vp ;
      isPre = False
      } ;

    WithoutVP vp = {
      s = infVP vp ++ "nélkül" ;
      isPre = False
      } ;

    ByVP vp = {
      s = infVP vp ++ "által" ;
      isPre = False
      } ;

    InOrderToVP vp = {
      s = infVP vp ++ "céljából" ;
      isPre = False
      } ;

    iFem_Pron = pronTable ! <P1,Sg> ** {g = Human} ;
    theyFem_Pron = pronTable ! <P3,Pl> ** {g = Human} ;
    theyNeutr_Pron = pronTable ! <P3,Pl> ;
    weFem_Pron = pronTable ! <P1,Pl> ** {g = Human} ;
    youFem_Pron = pronTable ! <P2,Sg> ** {g = Human} ;
    youPlFem_Pron = pronTable ! <P2,Pl> ** {g = Human} ;
    youPolFem_Pron = pronTable ! <P2,Sg> ** {g = Human} ;
    youPolPlFem_Pron = pronTable ! <P2,Pl> ** {g = Human} ;
    youPolPl_Pron = pronTable ! <P2,Pl> ** {g = Human} ;

oper
    passiveVP : VPSlash -> Str -> VP = \vps,agent ->
      lin VP (useV (copula ** {
        s = \\vf => case vf of {
          VPres P3 _ => [] ;
          _          => copula.s ! vf
          }
        }) ** {
          adv = vps.adv ++ vps.s ! Indef ! VAdvPart ++ agent
        }) ;

} ;
