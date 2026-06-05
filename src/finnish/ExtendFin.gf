--# -path=.:../common:../abstract

concrete ExtendFin of Extend =
  CatFin ** ExtendFunctor - [
    VPI2,VPS2,MkVPS,MkVPS2,ConjVPS2,ComplVPS2, ConsVPS, BaseVPS, ListVPS, VPS, ConjVPS,PredVPS,
    MkVPI,BaseVPI,ConsVPI,ConjVPI,ComplVPIVV,
    MkVPI2,ConjVPI2,ComplVPI2,ComplVPIVV
    ,ListComp, BaseComp, ConsComp, ConjComp
    ,ListImp, BaseImp, ConsImp, ConjImp

    ,ExistCN, ExistMassCN, ICompAP, ByVP
    ,CompoundN, CompoundAP, GenNP, GenIP, GenRP, AdvIsNP, EmbedSSlash
    ,PassVPSlash, PassAgentVPSlash
    ,CardCNCard
    ,UttAccNP
    ,AdjAsCN, AdjAsNP, GerundCN, GerundNP, GerundAdv
    ,ApposNP
    ,PresPartAP, PastPartAP, PastPartAgentAP
    ,PositAdVAdj, ProgrVPSlash
    ,RNP,RNPList,ReflPron,ReflPoss,ReflVPSlash,ReflVPS2,ReflA2
    ,PredetRNP,AdvRNP,AdvRVP,AdvRAP,PossPronRNP
    ,Base_rr_RNP,Base_nr_RNP,Base_rn_RNP,Cons_rr_RNP,Cons_nr_RNP,ConjRNP
    ]
  with
    (Grammar = GrammarFin) **

  open
    GrammarFin,
    ResFin,
    StemFin,
    (S=StemFin),
    (P=PhraseFin),
    IdiomFin,
    Coordination,
    Prelude,
    MorphoFin,
    ParadigmsFin in {

lin
   ExistCN cn =
      let
         pos = ExistNP (DetCN (DetQuant IndefArt NumSg) cn) ;
         neg = ExistNP (partCN cn) ;
      in posNegClause pos neg ;
   ExistMassCN cn = ExistNP (partCN cn) ;

oper
    partCN : CN -> GrammarFin.NP ;
    partCN cn =
      let
        acn = DetCN (DetQuant IndefArt NumSg) cn
      in acn ** {
        s = table {
          NPCase Nom | NPAcc => acn.s ! NPCase ResFin.Part ;
          c => acn.s ! c
          }
	} ;


  lincat
      VPS = {
      s   : Agr  => Str ;
      sc  : SubjCase ;  --- can be different for diff parts
      h   : Harmony   --- can be different for diff parts
      } ;

    [VPS] = {
      s1,s2 : Agr  => Str ;
      sc    : SubjCase ;   --- take the first: minä osaan kutoa ja täytyy virkata
      h     : Harmony    --- take the first: osaanko minä kutoa ja käyn koulua
      } ;

    VPI   = {s : VVType => Agr => Str ; sc : SubjCase } ;     -- Agr needed for possessive suffix:
    [VPI] = {s1,s2 : VVType => Agr => Str ; sc : SubjCase } ; -- e.g. toivon nukkuva+ni

  lin
    MkVPS t p vp0 = let vp = vp2old_vp vp0 in
     { --  Temp -> Pol -> VP -> VPS ;
      s = \\a =>
        let
	  agrfin = case vp.sc of {
                     SCNom => <a,True> ;
                     _ => <agrP3 Sg,False>      -- minun täytyy, minulla on
                     } ;
	  vps = vp.s ! VIFin t.t ! t.a ! p.p ! agrfin.p1
        in
        t.s ++ p.s ++
        vps.fin ++ vps.inf ++
        vp.s2 ! agrfin.p2 ! p.p ! a ++
        vp.adv ! p.p ++
        vp.ext ;
      sc = vp.sc ;
      h = vp.h
      } ;
      
    BaseVPS x y = twoTable Agr x y ** {sc = x.sc ; h = x.h} ;
    ConsVPS x y = consrTable Agr comma x y ** {sc = x.sc ; h = x.h} ;

    ConjVPS conj ss = conjunctDistrTable Agr conj ss ** {
      sc = ss.sc ; h = ss.h
      } ;

    PredVPS np vps = { -- NP -> VPS -> S ;
      s = subjForm np vps.sc Pos ++ vps.s ! np.a
      } ;


    BaseVPI x y = {
      s1 = x.s ;
      s2 = y.s ;
      sc = x.sc
      } ;

    ConsVPI x xs = {
      s1 = \\vt,agr => x.s ! vt ! agr ++ comma ;
      s2 = \\vt,agr => xs.s1 ! vt ! agr ++ xs.s2 ! vt ! agr ;
      sc = x.sc
      } ;

    ConjVPI conj xs = {
      s = \\vt,agr => conj.s1 ++ xs.s1 ! vt ! agr ++ conj.s2 ++ xs.s2 ! vt ! agr ;
      sc = xs.sc
      } ;

    MkVPI vp = mkVPI vp ;
    ComplVPIVV vv vpi =
      S.insertObj (\\_,_,a => vpi.s ! vv.vi ! a)
                  (S.predV (vv ** {sc = case vpi.sc of {
                                         SCNom => vv.sc ;  -- minun täytyy pestä auto
                                         c     => c }})   -- minulla täytyy olla auto
                  ) ;


lincat [Comp] = {s1,s2 : Agr => Str} ;
lin BaseComp = twoTable Agr ;
    ConsComp = consrTable Agr comma ;
    ConjComp conj xs = conjunctDistrTable Agr conj xs ;

lincat [Imp] = {s1,s2 : Polarity => Agr => Str} ;
lin BaseImp = twoTable2 Polarity Agr ;
    ConsImp = consrTable2 Polarity Agr comma ;
    ConjImp conj xs = conjunctDistrTable2 Polarity Agr conj xs ;

-------- two-place verb conjunction

  lincat
    -- Polarity needed to pick the right object case
    VPS2   = {s : Agr => Str ; c2 : Compl ; p : Polarity ; sc : SubjCase ; h : Harmony} ;
    [VPS2] = {s1,s2 : Agr => Str ; c2 : Compl ; p : Polarity ; sc : SubjCase ; h : Harmony} ;
    -- A version with
    VPI2   = {s : VVType => Agr => Str ; c2 : Compl ; sc : SubjCase } ;
    [VPI2] = {s1,s2 : VVType => Agr => Str ; c2 : Compl ; sc : SubjCase } ;

  lin
    -- : Temp -> Pol -> VPSlash -> VPS2 ;  -- has loved
    MkVPS2 t p vpsl = mkVPS t p (lin VP vpsl) ** {c2 = vpsl.c2 ; p = p.p} ;

    -- : VPSlash -> VPI2 ;                 -- to love
    MkVPI2 vpsl = mkVPI (lin VP vpsl) ** {c2 = vpsl.c2} ;

    BaseVPS2 x y = twoTable Agr x y ** {c2 = y.c2 ; p = xs.p } ; ---- just remembering the compl. case of the latter verb
    ConsVPS2 x xs = consrTable Agr comma x xs ** {c2 = xs.c2 ; p = xs.p } ;

    BaseVPI2 x y = twoTable2 VVType Agr x y ** {c2 = y.c2} ; ---- just remembering the compl. case of the latter verb
    ConsVPI2 x xs = consrTable2 VVType Agr comma x xs ** {c2 = xs.c2} ;


    ConjVPS2 c xs = conjunctDistrTable Agr c xs ** {c2 = xs.c2 ; p = xs.p ; sc = xs.sc ; h = xs.h} ;
    ConjVPI2 c xs = conjunctDistrTable2 VVType Agr c xs ** {c2 = xs.c2 ; p = xs.p ; sc = xs.sc } ;


                            -- appCompl : Bool -> Polarity -> Compl -> ResFin.NP -> Str
    ComplVPS2 v np = v ** { s = \\agr => v.s ! agr ++ appCompl True v.p v.c2 np } ;

                                              -- TODO: Version with variable polarity?
    ComplVPI2 v np = v ** { s = \\vt,a => v.s ! vt ! a ++ appCompl True Pos v.c2 np };

oper
    mkVPS : Temp -> Pol -> VP -> VPS = \tem,pol,vp -> lin VPS {
      s = \\agr => (UseCl tem pol (S.mkClause (\_ -> []) agr vp)).s ;
      h = vp.s.h ; sc = vp.s.sc
      } ;

    mkVPI : VP -> VPI = \vp -> lin VPI {
      s = \\vt,agr => S.infVP vp.s.sc Pos agr vp (vvtype2infform vt) ;
      sc = vp.s.sc } ;

lin
    CompoundN n1 n2 =
    let ukkos_ = (S.snoun2nounBind n1).s ! NCompound in {
      s  = \\c => ukkos_ ++ BIND ++ n2.s ! c ;
      h  = n2.h
      } ;

    CompoundAP n a =
    let prefix = (S.snoun2nounBind n).s ! NCompound in {
      s = \\_,af => prefix ++ BIND ++ sAdjFull2nforms Posit a ! af ;
      p = [] ;
      hasPrefix = True
      } ;

---- copied from VerbFin.CompAP, should be shared
    ICompAP ap = {
      s = \\agr =>
          let
            n = complNumAgr agr ;
            c = case n of {
              Sg => Nom ;  -- minä olen iso ; te olette iso
              Pl => ResFin.Part   -- me olemme isoja ; te olette isoja
              }            --- definiteness of NP ?
          in "kuinka" ++ ap.s ! False ! (NCase n c)
      } ;

  lin
    GenNP np = {
      s1,sp = \\_,_ => np.s ! NPCase Gen ;
      s2 = case np.isPron of { -- "isän auto", "hänen autonsa"
             True => table {Front => BIND ++ possSuffixFront np.a ;
                            Back  => BIND ++ possSuffix np.a } ;
             False => \\_ => []
             } ;
      isNum  = False ;
      isPoss = np.isPron ; --- also gives "sen autonsa"
      isDef  = True ; --- "Jussin kolme autoa ovat" ; thus "...on" is missing
      isNeg = False
     } ;

    GenIP ip = {s = \\_,_ => ip.s ! NPCase Gen} ;

    GenRP num cn = {
      s = \\n,c =>
        let k = npform2case num.n c
         in relPron ! n ! Gen ++ linCN (NCase num.n k) cn ;
---      a = RNoAg
      a = RAg (agrP3 num.n)
      } ;


    ByVP vp = lin Adv {s = S.infVP vp.s.sc Pos (Ag Sg P3) vp Inf3Adess} ; ---- Agr ?

    AdvIsNP adv np = S.mkClause (\_ -> adv.s) np.a (UseComp (CompNP np)) ;

    -- : SSlash -> SC
    EmbedSSlash ss =
      let it_NP : NP = UsePron it_Pron ;
          thatWhich : NP = it_NP ** {
            s = \\nc => it_NP.s ! NPSep ++ case nc of {
                  NPCase c => mikaInt ! Sg ! c ;
                  NPAcc    => mikaInt ! Sg ! Gen ;
                  NPSep    => mikaInt ! Sg ! Nom }
           } ;
       in {s = appCompl True Pos ss.c2 thatWhich ++ ss.s} ;

  PassVPSlash vp = S.passVP vp vp.c2 ;

  PassAgentVPSlash vp np = {
      s = {s = vp.s.s ; h = vp.s.h ; p = vp.s.p ; sc = npform2subjcase vp.c2.c} ;
      s2 = \\b,p,a => np.s ! NPSep ++ vp.s2 ! b ! p ! a ;
      adv = vp.adv ;
      ext = vp.ext ;
      vptyp = vp.vptyp ;
      } ;

  UseDAP, UseDAPFem, UseDAPMasc = \dap ->
      let
        n : ParadigmsFin.Number = case dap.isNum of {
          True => Sg ;
          _ => dap.n
          } ;
      in {
        s = \\c => let k = npform2case n c in
                 dap.sp ! k ; -- det.s2 is possessive suffix
        a = agrP3 (case dap.isDef of {
            False => Sg ;  -- autoja menee; kolme autoa menee
            _ => dap.n
            }) ;
        isPron = False ; isNeg = dap.isNeg
      } ;
      
lin CardCNCard card cn = {
  s = \\n,c =>
    let k = case <card.n, c> of {<Pl,Nom> => Part ; _ => c}
    in card.s ! n ! c ++ cn.s ! NCase Sg k ;
  n = Pl
  } ;

lin UttAccNP np = {s = P.addNegation np.isNeg ++ np.s ! NPAcc} ;
lin AdjAsCN ap = {s = ap.s ! True ; postmod = \\_ => ap.p ; h = Back} ; ---- Harmony just a guess
lin AdjAsNP ap = MassNP (AdjAsCN ap) ;
lin ApposNP np1 np2 = np1 ** {s = \\npf => np1.s ! npf ++ np2.s ! NPSep} ;
lin PresPartAP vp = {
      s = \\_,nf => preCompVP vp (PresPartAct (AN nf)) ;
      p = [] ;
      hasPrefix = False
      } ;
lin PastPartAP vps = {
      s = \\_,nf => preCompVP (lin VP vps) (PastPartAct (AN nf)) ;
      p = vps.c2.s.p1 ;
      hasPrefix = False
      } ;
    PastPartAgentAP vps np = {
      s = \\_,nf => np.s ! NPCase Gen ++ preCompVP (lin VP vps) (AgentPart (AN nf)) ;
      p = vps.c2.s.p1 ;
      hasPrefix = False
      } ;

lin GerundCN vp = {
      s = \\nf => let infForm : InfForm =
                        case nf of {
                          NCase _ Part => Inf4Part ;
                          _ => Inf4Nom
                        }
                  in infVP vp.s.sc Pos (agrP3 (numN nf)) vp infForm ;
      postmod = \\_ => [] ;
      h = Back
      } ;

    GerundNP vp =
      MassNP {
        s = \\nf => let infForm : InfForm =
                        case nf of {
                          NCase _ Part => Inf4Part ;
                          _ => Inf4Nom
                        }
                    in infVP vp.s.sc Pos (agrP3 (numN nf)) vp infForm ;
        postmod = \\_ => [] ;
        h = Back
        } ;

    GerundAdv vp = {
      s = infVP vp.s.sc Pos (agrP3 Sg) vp Inf3Adess
      } ;

    PositAdVAdj a = {s = a.s ! Posit ! sAAdv} ;

lin ProgrVPSlash vp =
      let
        inf = (sverb2verbSep vp.s).s ! Inf Inf3Iness ;
        on  = predV olla
      in vp ** {
        s = on.s ;
        s2 = \\b,p,a => inf ++ vp.s2 ! b ! p ! a ;
        } ;

lincat
  RNP = {s : Agr => NPForm => Str ; isPron : Bool ; isNeg : Bool} ;
  RNPList = {s1,s2 : Agr => NPForm => Str ; isPron : Bool ; isNeg : Bool} ;

lin ReflPron = {
      s = \\agr,npf => (reflPron agr).s ! npf ;
      isPron = True ;
      isNeg = False
      } ;

    ReflPoss num cn = {
      s = \\agr, npf =>
        let
          quant : Quant = lin Quant {  -- possessive pronoun with suffix only
            s2 : Harmony => Str = \\harm => possSuffixGen harm agr ;
	        s1,sp = \\_,_ => [] ; isNum,isNeg = False ; isPoss,isDef = True
	      }
        in (DetCN (DetQuant quant num) cn).s ! npf ;
     isPron = False ;
     isNeg = False
     } ;

    ReflVPS2 vps rnp = {
      s = \\agr => vps.s ! agr ++ appCompl True vps.p vps.c2 (rnp2np agr rnp) ;
      sc = vps.sc ;
      h = vps.h
      } ;

    PredetRNP pred rnp = {
      s = \\agr,npf => pred.s ! complNumAgr agr ! npf ++ rnp.s ! agr ! npf ;
      isPron = False ;
      isNeg = rnp.isNeg
      } ;

    AdvRNP np prep rnp = {
      s = \\agr,npf => np.s ! npf ++ appCompl True Pos prep (rnp2np agr rnp) ;
      isPron = False ;
      isNeg = orB np.isNeg rnp.isNeg
      } ;

    AdvRVP vp prep rnp =
      insertObj (\\_,b,agr => appCompl True b prep (rnp2np agr rnp)) vp ;

    AdvRAP ap prep rnp = ap ** {
      s = \\isMod,af => ap.s ! isMod ! af ++ appCompl True Pos prep (rnp2np (agrP3 Sg) rnp) ;
      hasPrefix = False
      } ;

    PossPronRNP p num cn rnp =
      let np = DetCN (DetQuant (PossPron p) num) cn in
      np ** {
        s = \\npf => np.s ! npf ++ rnp.s ! p.a ! NPCase Gen ;
        isNeg = orB np.isNeg rnp.isNeg
        } ;

    Base_rr_RNP x y = {
      s1 = x.s ;
      s2 = y.s ;
      isPron = False ;
      isNeg = orB x.isNeg y.isNeg
      } ;

    Base_nr_RNP x y = {
      s1 = \\_,npf => x.s ! npf ;
      s2 = y.s ;
      isPron = False ;
      isNeg = orB x.isNeg y.isNeg
      } ;

    Base_rn_RNP x y = {
      s1 = x.s ;
      s2 = \\_,npf => y.s ! npf ;
      isPron = False ;
      isNeg = orB x.isNeg y.isNeg
      } ;

    Cons_rr_RNP x xs = {
      s1 = \\agr,npf => x.s ! agr ! npf ++ comma ;
      s2 = \\agr,npf => xs.s1 ! agr ! npf ++ xs.s2 ! agr ! npf ;
      isPron = False ;
      isNeg = orB x.isNeg xs.isNeg
      } ;

    Cons_nr_RNP x xs = {
      s1 = \\_,npf => x.s ! npf ++ comma ;
      s2 = \\agr,npf => xs.s1 ! agr ! npf ++ xs.s2 ! agr ! npf ;
      isPron = False ;
      isNeg = orB x.isNeg xs.isNeg
      } ;

    ConjRNP conj xs = {
      s = \\agr,npf => conj.s1 ++ xs.s1 ! agr ! npf ++ conj.s2 ++ xs.s2 ! agr ! npf ;
      isPron = xs.isPron ;
      isNeg = xs.isNeg
      } ;

oper
  -- ruohoa syövä, Ranskassa valmistettu
  preCompVP : S.VP -> VForm -> Str = \vp, vform ->
    vp.s2 ! True ! Pos ! agrP3 Sg ++
    vp.adv ! Pos ++
    vp.s.s ! vform ++
    vp.ext ;

  rnp2np agr rnp = {
    s = \\npf => rnp.s ! agr ! npf ;
    a = agr ;
    isPron = rnp.isPron ;
    isNeg = rnp.isNeg
    } ;

}
