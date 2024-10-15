--# -path=.:../scandinavian:../abstract:../common:prelude
concrete ExtendSwe of Extend = CatSwe **
  ExtendFunctor -
  [
    GenNP, GenModNP, ComplBareVS, CompBareCN,
    ApposNP, DetNPMasc, DetNPFem,
    StrandRelSlash, EmptyRelSlash, StrandQuestSlash,
    PassVPSlash, PassAgentVPSlash, UttVPShort, ByVP, InOrderToVP,
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS, RelVPS,
    MkVPS2, ConjVPS2, ComplVPS2, ReflVPS2, MkVPI2, ConjVPI2, ComplVPI2,
    ICompAP,ProDrop,EmbedSSlash,
    AdAdV, PositAdVAdj, GerundCN, GerundNP, GerundAdv, PresPartAP, PastPartAP, PastPartAgentAP,
    RNP, RNPList, ReflRNP, ReflPron, ReflPoss, PredetRNP, ConjRNP,
    Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP, ReflPossPron,
    CompoundN, CompoundAP, AdvIsNP,
    UttAccNP,
    A2VPSlash, N2VPSlash,
    CardCNCard,
    GenRP
  ]
  with (Grammar = GrammarSwe)
    **
 open CommonScand, ResSwe, ParamX, VerbSwe, Prelude, DiffSwe, StructuralSwe, MorphoSwe,
      NounSwe, Coordination, AdjectiveSwe, SentenceSwe, AdverbSwe, RelativeSwe, (P = ParadigmsSwe),
      (M = MakeStructuralSwe)
in {

  flags coding=utf8 ;

  lin
    GenNP np = {
      s,sp = \\n,_,_,g  => np.s ! NPPoss (gennum (ngen2gen g) n) Nom ;
      det = DDef Indef
      } ;

    GenModNP num np cn = DetCN (DetQuant (GenNP (lin NP np)) num) cn ;

    ComplBareVS v s  = insertObj (\\_ => s.s ! Sub) (predV v) ;

    CompBareCN cn = {s = \\a => case a.n of {
      Sg => cn.s ! Sg ! DIndef ! Nom ;
      Pl => cn.s ! Pl ! DIndef ! Nom
      }
    } ;

    StrandRelSlash rp slash  = {
      s = \\t,a,p,ag,_ =>
          rp.s ! ag.g ! ag.n ! RNom ++ slash.s ! t ! a ! p ! Sub ++ slash.n3 ! ag ++ slash.c2.s ;
      c = NPAcc
      } ;
    EmptyRelSlash slash = {
      s = \\t,a,p,ag,_ =>
          slash.s ! t ! a ! p ! Sub ++ slash.n3 ! ag ++ slash.c2.s ;
      c = NPAcc
      } ;

    StrandQuestSlash ip slash = {
      s = \\t,a,p =>
            let
              cls = slash.s ! t ! a ! p ;
              who = ip.s ! accusative ;
	      agr = agrP3 ip.g ip.n ;
            in table {
              QDir   => who ++ cls ! Inv ++ slash.n3 ! agr ++ slash.c2.s ;
              QIndir => who ++ cls ! Sub ++ slash.n3 ! agr ++ slash.c2.s
              }
      } ;

  lin
    PassVPSlash vps =
      insertObj (\\a => vps.c2.s ++ vps.n3 ! a) (passiveVP vps) ;
    PassAgentVPSlash vps np =
      insertObjPost (\\a => vps.c2.s ++ vps.n3 ! a) (insertObj (\\_ => (PrepNP by8agent_Prep np).s) (passiveVP vps)) ;
    ProgrVPSlash vp = 
      insertObj (\\a => "att" ++ infVP vp a) (predV (P.partV I.hålla_V "på")) **
        { n3 = vp.n3 ;
          c2 = vp.c2
        } ;


    N2VPSlash n2 =
      let vp : CatSwe.VP = UseComp (CompCN (UseN2 n2)) ;
          dummyVPS : VPSlash = SlashV2a (P.mkV2 "dummy") ;
      in dummyVPS **  -- has necessary fields for VPSlash
               vp **  -- has all the right fields except for c2
              {c2 = n2.c2} ; -- has the right c2



    A2VPSlash a2 =
      let vp : CatSwe.VP = UseComp (CompAP (UseA2 a2)) ;
          dummyVPS : VPSlash = SlashV2a (P.mkV2 "dummy") ;
      in dummyVPS **  -- has necessary fields for VPSlash
               vp **  -- has all the right fields except for c2
              {c2 = a2.c2} ; -- has the right c2


  lin UttVPShort vp = {s = infVP vp (agrP3 Utr Sg)} ;

  lincat
    VPI   = {s : VPIForm => Agr => Str} ;
    [VPI] = {s1,s2 : VPIForm => Agr => Str} ;

  lin
    BaseVPI = twoTable2 VPIForm Agr ;
    ConsVPI = consrTable2 VPIForm Agr comma ;

    MkVPI vp = {
      s = \\v,a => infVP vp a ---- no sup
      } ;
    ConjVPI = conjunctDistrTable2 VPIForm Agr ;
    ComplVPIVV vv vpi = insertObj (\\a => vv.c2.s ++ vpi.s ! VPIInf ! a) (predV vv) ;

  lincat
    VPS   = {s : Order => Agr => {verb, compl : Str}} ;
    [VPS] = {s : Order => Agr => {s1, s2, s3 : Str}} ; -- älskar, (jag) dig, (och) är lycklig

  lin
      BaseVPS v w = {
      s = \\ord, agr =>
        let
	  vs = v.s ! ord ! agr ;
	  ws = w.s ! ord ! agr ;
	in {
	  s1 = vs.verb ;
	  s2 = vs.compl ;
	  s3 = ws.verb ++ ws.compl
	  }
       } ;
	  
    ConsVPS v vv = {
      s = \\ord, agr =>
        let
	  vs = v.s ! ord ! agr ;
	  vvs = vv.s ! ord ! agr ;
	in {
	  s1 = vs.verb ;
	  s2 = vs.compl ++ comma ++ vvs.s1 ++ vvs.s2 ;
	  s3 = vvs.s3
	  }
      } ;

    ConjVPS conj vv = {
      s = \\ord, agr =>
         let
           vvs = vv.s ! ord ! agr
         in {
	   verb = vvs.s1 ;
	   compl = conj.s1 ++ vvs.s2 ++ conj.s2 ++ vvs.s3
	   }
      } ;

    PredVPS np vps =
      let
        subj = np.s ! nominative ;
        agr  = np.a ;
      in {
        s = \\o =>
          let verb = vps.s ! o ! agr
          in case o of {
            Main => subj ++ verb.verb ++ verb.compl ;
            Inv  => verb.verb ++ subj ++ verb.compl ;   -- älskar jag henne och sover
            Sub  => subj ++ verb.verb ++ verb.compl     --- not quite correct in ConjVPS
            }
        } ;

    RelVPS rp vps = {
      s = \\ag,rcase =>
        let agr = case rp.a of {  -- RP's agr may override in the regular RelativeScand, is this true with VPS too?
                    RNoAg => ag ;
                    RAg g n p => {g = g ; n = n ; p = p}
                  } ;
	    verb = vps.s ! Sub ! agr
        in
	    rp.s ! ag.g ! ag.n ! rcase ++ verb.verb ++ verb.compl ;
      c = NPNom
      } ;

    MkVPS t p vp = {
      s = \\o,a =>
            let
              verb  = vp.s ! Act ! VPFinite t.t t.a ;
	      neg   = verb.a1 ! p.p ! a ;
              compl = vp.n2 ! a ++ vp.a2 ++ vp.ext ;
	      pron  = vp.n1 ! a ;
	      verbf = t.s ++ p.s ++ verb.fin
            in 
            case o of { 
              Main => {verb = verbf ; compl = neg.p1 ++ verb.inf ++ pron ++ neg.p2 ++ compl} ;
              Inv  => {verb = verbf ; compl = neg.p1 ++ verb.inf ++ pron ++ neg.p2 ++ compl} ;
              Sub  => {verb = neg.p1 ++ neg.p2 ++ verbf ; compl = verb.inf ++ pron ++ compl}
              }
      } ;

  lincat
    VPS2   = {s : Order => Agr => Str ; c2 : {s : Str; hasPrep : Prelude.Bool}} ;
    [VPS2] = {s1,s2 : Order => Agr => Str ; c2 : {s : Str; hasPrep : Prelude.Bool}} ;

  lin
    BaseVPS2 x y = twoTable2 Order Agr x y ** {c2 = y.c2} ;
    ConsVPS2 x xs = consrTable2 Order Agr comma x xs ** {c2 = xs.c2};


    MkVPS2 t p vp = {
      s = \\o,a =>
            let
              verb  = vp.s ! Act ! VPFinite t.t t.a ;
	      neg   = verb.a1 ! p.p ! a ;
              compl = vp.n2 ! a ++ vp.a2 ++ vp.ext ;
	      pron  = vp.n1 ! a
            in t.s ++ p.s ++ case o of {
              Main => verb.fin ++ neg.p1 ++ verb.inf ++ pron ++ neg.p2 ++ compl ;
              Inv  => verb.fin ++ neg.p1 ++ verb.inf ++ pron ++ neg.p2 ++ compl ; ----
              Sub  => neg.p1 ++ neg.p2 ++ verb.fin ++ verb.inf ++ pron ++ compl
              } ;
       c2 = vp.c2
      } ;

    ComplVPS2 vps2 np = {
        s = \\o,a => {verb = vps2.s !o ! a ; compl = vps2.c2.s ++ np.s ! NPAcc}
        } ;
    ReflVPS2 vps2 rnp = {
        s = \\o,a => {verb = vps2.s ! o ! a ; compl = vps2.c2.s ++ rnp.s ! a}
        } ;

    ConjVPS2 c xs = conjunctDistrTable2 Order Agr c xs ** {c2 = xs.c2} ;

  lincat
    VPI2   = {s : VPIForm => Agr => Str ; c2 : {s : Str; hasPrep : Prelude.Bool}} ;
    [VPI2] = {s1,s2 : VPIForm => Agr => Str ; c2 : {s : Str; hasPrep : Prelude.Bool}} ;

  lin
    BaseVPI2 x y = twoTable2 VPIForm Agr x y ** {c2 = y.c2} ;
    ConsVPI2 x xs = consrTable2 VPIForm Agr comma x xs ** {c2 = xs.c2} ;

    MkVPI2 vp = {
      s = \\v,a => infVP vp a  ; ---- no sup
      c2 = vp.c2
      } ;
    ConjVPI2 c xs = conjunctDistrTable2 VPIForm Agr c xs ** {c2 = xs.c2} ;

    ComplVPI2 vpi2 np = {
        s = \\t,a => vpi2.s ! t ! a ++ vpi2.c2.s ++  np.s ! NPAcc
        } ;

lincat [Comp] = {s1,s2 : Agr => Str} ;
lin BaseComp x y = twoTable Agr x y ;
    ConsComp xs x = consrTable Agr comma xs x ;
    ConjComp conj ss = conjunctDistrTable Agr conj ss ;

lincat ListImp = {s1,s2 : Polarity => Number => Str} ;
lin BaseImp = twoTable2 Polarity Number ;
    ConsImp = consrTable2 Polarity Number comma ;
    ConjImp conj ss = conjunctDistrTable2 Polarity Number conj ss ;

-----------

    ICompAP ap = {s = \\a => hur_IAdv.s ++ ap.s ! a} ;

    ProDrop pro = pro ** {s = \\_ => []} ;

  lincat
    RNP     = {s : Agr => Str ; isPron : Bool} ;   ---- inherent Agr needed: han färgar sitt hår vitt. But also depends on subject
    RNPList = {s1,s2 : Agr => Str} ;

  lin
    ReflRNP vps rnp =
      insertObjPost (\\a => vps.n3 ! a)
        (insertObjPron (andB rnp.isPron (notB vps.c2.hasPrep)) (\\a => vps.c2.s ++ rnp.s ! a)
          vps) ;

    ReflPron = {s = \\a => reflPron a ; isPron = True} ; ---- agr ??
    ReflPoss num cn = {
      s = \\a => possPron a.n a.p num.n (ngen2gen cn.g) ++ num.s ! cn.g ++ cn.s ! num.n ! DDef Indef ! Nom ;
      isPron = False
      } ;
    PredetRNP predet rnp = {
      s = \\a => predet.s ! Utr ! Pl ++ predet.p ++ rnp.s ! a ;  ---- agr needed here as well
----      s = \\a => predet.s ! np.a.g ! np.a.n ++ predet.p ++ np.s ! a ;
----      a = case pred.a of {PAg n => agrP3 np.a.g n ; _ => np.a} ;
      isPron = False
      } ;

    AdvRNP np prep rnp = {s = \\a => np.s ! NPAcc ++ prep.s ++ rnp.s ! a; isPron = False} ;
    AdvRVP vp prep rnp = insertObjPost (\\a => prep.s ++ rnp.s ! a) vp ;
    AdvRAP ap prep rnp = {
      s = \\a => let agr = case a of {
                              Strong (GSg g) => agrP3 g Sg ;
                              Strong GPl => agrP3 Utr Pl ;
                              Weak n => agrP3 Utr n
                            }
                  in ap.s ! a ++ prep.s ++ rnp.s ! agr ;
      isPre = ap.isPre
      } ;

    ReflA2RNP a rnp = {
      s = \\ap => let agr = case ap of {
                              Strong (GSg g) => agrP3 g Sg ;
                              Strong GPl => agrP3 Utr Pl ;
                              Weak n => agrP3 Utr n
                            }
                  in a.s ! AF (APosit ap) Nom ++ a.c2.s ++ rnp.s ! agr ;
      isPre = False
      } ;

    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = \\_ => rnp.s ! pron.a; a = pron.a; isPron=False})) ;

    ConjRNP conj rpns = conjunctDistrTable Agr conj rpns ** {isPron = False} ;

    Base_rr_RNP x y = twoTable Agr x y ;
    Base_nr_RNP x y = twoTable Agr {s = \\a => x.s ! NPAcc} y ;
    Base_rn_RNP x y = twoTable Agr x {s = \\a => y.s ! NPAcc} ;
    Cons_rr_RNP x xs = consrTable Agr comma x xs ;
    Cons_nr_RNP x xs = consrTable Agr comma {s = \\a => x.s ! NPAcc} xs ;

    ReflPossPron = M.mkQuant "sin" "sitt" "sina" ;

  lin
    ApposNP np1 np2 = {s = \\nform => np1.s ! nform ++ comma ++ np2.s ! nform; a = np1.a; isPron = False} ;

    DetNPMasc, DetNPFem = \det ->
      let
        g = utrum ; ----
        m = True ;  ---- is this needed for other than Art?
      in {
        s = \\c => det.sp ! m ! g ;    ---- case of det!
        a = agrP3 (ngen2gen g) det.n ;
        isPron = False
      } ;

    CompoundN n1 n2 = {
      s  = \\n,s,c => n1.co ++ BIND ++ n2.s ! n ! s ! c ;
      co = n1.co ++ BIND ++ n2.co ;
      g  = n2.g
    } ;

    CompoundAP noun adj = {
      s = \\ap => noun.co ++ BIND ++ adj.s ! AF (APosit ap) Nom ;
      isPre = True
    } ;

  lin
    AdAdV = cc2 ;

    PositAdVAdj a = {s = a.s ! AAdv} ;

    PresPartAP vp = {
      s = \\af => case vp.isSimple of {
                    True  => partVPPlus     vp (PartPres Sg Indef Nom) (aformpos2agr af) Pos ;
                    False => partVPPlusPost vp (PartPres Sg Indef Nom) (aformpos2agr af) Pos
                  } ;
      isPre = vp.isSimple
    } ;

    PastPartAP vp = {
      s = \\af => let vp' = vp**{n2 : Agr => Str =\\a => vp.n2 ! a ++ vp.n3 ! a}
                  in case vp.isSimple of {
                       True  => partVPPlus     vp' (PartPret af Nom) (aformpos2agr af) Pos ;
                       False => partVPPlusPost vp' (PartPret af Nom) (aformpos2agr af) Pos
                     } ;
      isPre = vp.isSimple
    } ;

    PastPartAgentAP vp np = {
      s = \\af => let vp' = vp**{n2 : Agr => Str =\\a => vp.n2 ! a ++ vp.n3 ! a}
                  in partVPPlusPost vp' (PartPret af Nom) (aformpos2agr af) Pos ++ "av" ++ np.s ! accusative ;
      isPre = False
    } ;

    GerundCN vp = {  -- infinitive: att dricka öl, att vara glad
      s = \\_,_,_ => "att" ++ infVP vp {g = Utr ; n = Sg ; p = P3} ;
      g = Neutr ;
      isMod = False
    } ;

    GerundNP vp = {  -- infinitive: att dricka öl, att vara glad
      s = \\_ => "att" ++ infVP vp {g = Utr ; n = Sg ; p = P3} ;
      a = {g = Neutr ; n = Sg ; p = P3} ;
      isPron = False
    } ;

    GerundAdv vp = {
      s = partVPPlusPost vp (PartPres Sg Indef (Nom|Gen)) {g = Utr ; n = Sg ; p = P3} Pos -- sovande(s) i sängen
    } ;

    ByVP vp = {  -- infinitive: att dricka öl, att vara glad
      s = "genom att" ++ infVP vp {g = Utr ; n = Sg ; p = P3}
    } ;

    InOrderToVP vp = {  -- infinitive: att dricka öl, att vara glad
      s = "för att" ++ infVP vp {g = Utr ; n = Sg ; p = P3}
    } ;

    AdvIsNP adv np = PredVP {s = \\_ => adv.s ; a = np.a ; isPron = False} (UseComp (CompNP np)) ;

    EmbedSSlash ss = {s = "det" ++ ss.s ! Main ++ ss.c2.s ++ ss.n3 ! agrUSgP3} ;

    UttAccNP np = {s = np.s ! NPAcc} ;

lin UseDAP dap =
      let
        g = neutrum ; ----
        m = True ;  ---- is this needed for other than Art?
      in {
        s = table {
               NPPoss _ _ => dap.sp ! m ! g ++ BIND ++ "s" ;
               _          => dap.sp ! m ! g
            } ;
        a = agrP3 (ngen2gen g) dap.n ;
        isPron = False
      } ;

lin UseDAPMasc, UseDAPFem = \dap ->
      let
        g = utrum ; ----
        m = True ;  ---- is this needed for other than Art?
      in {
        s = table {
               NPPoss _ _ => dap.sp ! m ! g ++ BIND ++ "s" ;
               _          => dap.sp ! m ! g
            } ;
        a = agrP3 (ngen2gen g) dap.n ;
        isPron = False
      } ;

lin CardCNCard card cn =
  {s = \\g => card.s ! cn.g ++ cn.s ! card.n ! DIndef ! Nom ; n = Pl} ;

  GenRP num cn = {
      s = \\g_,n,c => "vars" ++ cn.s ! num.n ! DDef Indef ! Nom ; --- c ?
      a = RAg cn.g num.n P3
      } ;

}
