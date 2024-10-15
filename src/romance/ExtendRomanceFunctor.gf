incomplete concrete ExtendRomanceFunctor of Extend =
  Cat ** open Grammar, ResRomance in {

  lincat
    RNP = {s : Agr => Case => Str} ;

  ---- these come from ExtraRomance: how to avoid the repetition?
  ---- can't seem to be able to use two functors
  lincat
    VPS = {s : Mood => Agr => Bool => Str} ;
    [VPS] = {s1,s2 : Mood => Agr => Bool => Str} ;

  lin
    BaseVPS x y = twoTable3 Mood Agr Bool x y ;
    ConsVPS = consrTable3 Mood Agr Bool comma ;

    BaseVPI = variants {} ;
    ConsVPI = variants {} ;
    BaseVPS2 = variants {} ;
    ConsVPS2 = variants {} ;
    BaseVPI2 = variants {} ;
    ConsVPI2 = variants {} ;

    --GenNP = variants {} ;
    GenNP np =
      let denp = (np.s ! ResRomance.genitive).ton in {
        s = \\_,_,_,_ => [] ;
        sp = \\_,_,_ => denp ;
        spn= \\_ => denp ;
        s2 = denp ;
        isNeg = False ;
      } ;

    GenIP ip = {s = \\_,_,c => ip.s ! c} ;
    GenRP = variants {} ;     -- Num -> CN -> RP ; -- whose car

    GenModNP num np cn = DetCN (DetQuant (GenNP (lin NP np)) num) cn ;
    GenModIP num ip cn = IdetCN (IdetQuant (GenIP (lin IP ip)) num) cn ;

    CompBareCN cn = {
      s = \\agr => cn.s ! agr.n ;
      cop = serCopula
      } ;

    StrandQuestSlash = QuestSlash ; -- whom does John live with ; DEFAULT with whom does John live
    StrandRelSlash = RelSlash ; -- that he lives in ; DEFAULT in which he lives

    EmptyRelSlash = variants {} ;
    -- --TODO debug this definition
    -- EmptyRelSlash cls = {
    --   s = \\agr,t,a,p,m => cls.s ! agr ! DDir ! t ! a ! p ! m ++ cls.c2.s ;
    --   c = Nom
    --   } ;
    SubjRelNP np rs = heavyNPpol np.isNeg {
      s = \\c => (np.s ! c).ton ++ rs.s ! Conjunct ! np.a ;
      a = np.a ;
      hasClit = False
      } ;
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

    SubjunctRelCN cn rs = let g = cn.g in {
      s = \\n => cn.s ! n ++ rs.s ! Conjunct ! agrP3 g n ; --- mood
      g = g
      } ;


    MkVPI vp = variants {} ;     -- Temp -> Pol -> VP -> VPI ; -- to sleep / hasn't slept
    ConjVPI = variants {} ;     -- Conj -> [VPI] -> VPI ; -- has walked and won't sleep
    ComplVPIVV = variants {} ;     -- VV -> VPI -> VP ; -- want to sleep and to walk
    MkVPS2 = variants {} ;     --     : Temp -> Pol -> VPSlash -> VPS2 ;  -- has loved
    ConjVPS2 = variants {} ;     --   : Conj -> [VPS2] -> VPS2 ;          -- has loved and now hates
    ComplVPS2 = variants {} ;     --  : VPS2 -> NP -> VPS ;               -- has loved and now hates that person
    MkVPI2 = variants {} ;     --     : Ant  -> Pol -> VPSlash -> VPI2 ;  -- to have loved
    ConjVPI2 = variants {} ;     --   : Conj -> [VPI2] -> VPI2 ;          -- to love and have hated
    ComplVPI2 = variants {} ;     --  : VPI2 -> NP -> VPI ;               -- to love and hate that person

  lin
    ProDrop p = p ** {
      s = table {
        Nom => let pn = p.s ! Nom
          in {c1 = pn.c1 ; c2 = pn.c2 ; comp = [] ; ton = pn.ton} ;
        c => p.s ! c
        } ;
      isNeg = False
      } ;

    ICompAP = variants {} ;     -- AP -> IComp ; -- "how old"
    IAdvAdv = variants {} ;     -- Adv -> IAdv ; -- "how often"

    CompIQuant iq = {s = \\aa => iq.s ! aa.n ! aa.g ! Nom ; cop = serCopula} ;

    PrepCN prep cn = {s = prep.s ++ prepCase prep.c ++ cn.s ! Sg} ;

    FocusObj = variants {} ;     -- NP -> SSlash -> Utt ; -- her I love
    FocusAdv = variants {} ;     -- Adv -> S -> Utt ; -- today I will sleep
    FocusAdV = variants {} ;     -- AdV -> S -> Utt ; -- never will I sleep
      FocusAP = variants {} ;     -- AP -> NP -> Utt ; -- green was the tree

  lin
    EmbedPresPart = variants {} ;     -- VP -> SC ; -- looking at Mary (is fun)

    PresPartAP vp = {
      s = \\af => gerVP vp RPos (aform2aagr af ** {p = P3}) ;
      isPre = False ;
      copTyp = serCopula
      } ;

    PassVPSlash vps = passVPSlash vps [] ;
    PassAgentVPSlash vps np = passVPSlash vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

    PastPartAP vps = pastPartAP vps [] ;
    PastPartAgentAP vps np = pastPartAP vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

    ExistsNP = variants {} ;

    NominalizeVPSlashNP = variants {} ;     -- VPSlash -> NP -> NP ;

    ProgrVPSlash = variants {} ;            -- VPSlash -> VPSlash ;

    ExistCN cn = ExistNP (DetCN (DetQuant IndefArt NumSg) cn) ;
    ExistMassCN cn = ExistNP (MassNP cn) ;
    ExistPluralCN cn = ExistNP (DetCN (DetQuant IndefArt NumPl) cn) ;
    AdvIsNP adv np = mkClause adv.s False False np.a (UseComp_estar (CompNP np)) ;
    AdvIsNPAP adv np ap = -- <aquí:Adv> está <documentada:AP> <la examinación:NP>
      let emptyN : N = lin N {s = \\_ => [] ; relType = NRelNoPrep ; g = np.a.g} ; -- To match the gender of the N
          indef : Quant = IndefArt ** {s = \\b,n,g,c => []} ;
          det : Det = case np.a.n of {Sg => DetQuant indef NumSg ; Pl => DetQuant indef NumPl} ;
          apAsNP : NP = DetCN det (AdjCN ap (UseN emptyN)) ; -- NP where the string comes only from AP
       in AdvIsNP adv (ApposNP apAsNP np) ;

  lin
    ComplBareVS = ComplVS ;     -- VS -> S -> VP ; -- say she runs ; DEFAULT say that she runs
    SlashBareV2S = SlashV2S ;     -- V2S -> S -> VPSlash ; -- answer (to him) it is good ; DEFAULT answer that it is good
    ComplDirectVS vs utt = AdvVP (UseV <lin V vs : V>) (lin Adv {s = ":" ++ quoted utt.s}) ; -- DEFAULT complement added as Adv in quotes
    ComplDirectVQ vq utt = AdvVP (UseV <lin V vq : V>) (lin Adv {s = ":" ++ quoted utt.s}) ; -- DEFAULT complement added as Adv in quotes
    FrontComplDirectVS = variants {} ; -- NP -> VS -> Utt -> Cl ;      -- "I am here", she said
    FrontComplDirectVQ  = variants {} ; -- NP -> VQ -> Utt -> Cl ;      -- "where", she asked
    PredAPVP ap vp = ImpersCl (UseComp (CompAP (SentAP ap (EmbedVP vp)))) ; -- DEFAULT it is (good to walk)

    AdjAsCN ap = {
      s = \\n => ap.s ! genNum2Aform Masc n ;
      g = Masc
      } ;

    AdjAsNP ap = heavyNP {
      s = \\_c => ap.s ! genNum2Aform Masc Sg ;
      a = Ag Masc Sg P3
      } ;

  lin
    ReflRNP v rnp =      -- VPSlash -> RNP -> VP ; -- love my family and myself
      case v.c2.isDir of {
        True  => insertRefl v ;
        False => insertComplement
                   (\\a => let agr = verbAgr a in v.c2.s ++ rnp.s ! agr ! v.c2.c) v
      } ;

    ReflPron = {         -- RNP ; -- myself
      s = \\agr,c => reflPron agr.n agr.p c
    } ;

    ReflPoss = variants {} ;     -- Num -> CN -> RNP ; -- my car(s)
    PredetRNP = variants {} ;     -- Predet -> RNP -> RNP ; -- all my brothers
    ConjRNP = variants {} ;     -- Conj -> RNPList -> RNP ; -- my family, John and myself
    Base_rr_RNP = variants {} ;     -- RNP -> RNP -> RNPList ; -- my family, myself
    Base_nr_RNP = variants {} ;     -- NP -> RNP -> RNPList ; -- John, myself
    Base_rn_RNP = variants {} ;     -- RNP -> NP -> RNPList ; -- myself, John
    Cons_rr_RNP = variants {} ;     -- RNP -> RNPList -> RNPList ; -- my family, myself, John
    Cons_nr_RNP = variants {} ;     -- NP -> RNPList -> RNPList ; -- John, my family, myself
    ComplGenVV = variants {} ;     -- VV -> Ant -> Pol -> VP -> VP ; -- want not to have slept
    ComplSlashPartLast = ComplSlash ;

    CompoundN a b = lin N {
      s = \\n => b.s ! n ++
                 case b.relType of {
                   NRelPrep p => prepCase (CPrep p) ;  -- tasa de suicidio
                   NRelNoPrep => []                    -- connessione internet = internet connection
                 } ++
                 a.s ! Sg ;
      g = b.g ;
      relType = b.relType
      } ;
    CompoundAP = variants {} ;     -- N -> A -> AP ; -- language independent / language-independent

  lin
    GerundNP vp = let
      neutrAgr = Ag Masc Sg P3
      in heavyNP {
        s = \\_ => gerVP vp RPos neutrAgr ;
        a = neutrAgr
      } ;

    GerundCN vp = {
      s = \\n => gerVP vp RPos {g = Masc ; n = n ; p = P3} ;
      g = Masc
      } ;

    GerundAdv vp = {
      s = gerundStr vp
      } ;

  lin
    PurposeVP vp = {
      s = infVP vp RPos (Ag Masc Sg P3)
      } ;

    WithoutVP = variants {} ;     -- VP -> Adv ; -- without publishing the document

    ByVP vp = {
      s = gerundStr vp
      } ;

    InOrderToVP = variants {} ;     -- VP -> Adv ; -- (in order) to publish the document

    PredIAdvVP iadv vp = {s = \\_,_,_,_ => iadv.s ++ infStr vp} ;

    ApposNP np1 np2 = np1 ** {
      s = \\c => {
        c1 = (np1.s ! c).c1  ++ (np2.s ! c).c1 ;
        c2 = (np1.s ! c).c2 ++ (np2.s ! c).c2 ;
        comp = (np1.s ! c).comp ++ (np2.s ! c).comp ;
        ton = (np1.s ! c).ton ++ (np2.s ! c).ton
        } ;
      } ;

    AdAdV aa av = {
      s = aa.s ++ av.s
      } ;
    UttAdV av = av ;
    PositAdVAdj a = {
      s = a.s ! AA
      } ;

  lin
    CompS = variants {} ;     -- S -> Comp ; -- (the fact is) that she sleeps
    CompQS = variants {} ;     -- QS -> Comp ; -- (the question is) who sleeps

    --TODO: actually use ant
    CompVP ant p vp = {
        s = \\agr => ant.s ++ p.s ++ "de" ++ infVP vp p.p agr ;
        cop = serCopula
      } ;

  lin
    DetNPMasc = DetNP ;
    DetNPFem det =
      let
        g = Fem ;
        n = det.n
      in heavyNPpol det.isNeg {
           s = det.sp ! g ;
           a = agrP3 g n ;
           hasClit = False
           } ;


    UseDAP = \dap ->
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

  lin
    iFem_Pron = i_Pron ; -- DEFAULT I (masc)
    youFem_Pron = youSg_Pron ; -- DEFAULT you (masc)
    weFem_Pron = we_Pron ;  -- DEFAULT we (masc)
    youPlFem_Pron = youPl_Pron ;  -- DEFAULT you plural (masc)
    theyFem_Pron = they_Pron ;  -- DEFAULT they (masc)
    youPolFem_Pron = youPol_Pron ;  -- DEFAULT you polite (masc)
    youPolPl_Pron = youPl_Pron ;  -- DEFAULT you plural (masc)
    youPolPlFem_Pron = youPl_Pron ;  -- DEFAULT you plural (masc)

  lin
    UseComp_estar comp = insertComplement comp.s (predV stare_V) ;
    UncontractedNeg = {s = [] ; p = RNeg False} ;
    UttAccNP = UttNP ; -- him (accusative) ; DEFAULT he
    UttDatNP np = UttAccNP (lin NP np) ; -- him(dative) ; DEFAULT he
    UttAccIP = UttIP ; -- whom (accusative) ; DEFAULT who
    UttDatIP ip = UttAccIP (lin IP ip) ; -- whom (dative) ; DEFAULT who
    UttVPShort = UttVP ;

    TPastSimple = {s = []} ** {t = RPasse} ;   --# notpresent

  oper
    quoted : Str -> Str = \s -> "\"" ++ s ++ "\"" ; ---- TODO bind ; move to Prelude?

  oper
    gerundStr : VP -> Str ;
    gerundStr vp = gerVP vp RPos (Ag Masc Sg P3) ;

    infStr : VP -> Str ;
    infStr vp = infVP vp RPos (Ag Masc Sg P3) ;

    pastPartAP : VPSlash -> Str -> AP ;
    pastPartAP vps agent = lin AP {
      s = \\af => vps.s.s ! VPart (aform2gender af) (aform2number af) ++ vps.comp ! (aform2aagr af ** {p = P3}) ++ agent ;
      isPre = False ;
      copTyp = serCopula
      } ;

    passVPSlash : VPSlash -> Str -> VP ;
    passVPSlash vps agent = let
      auxvp = predV auxPassive
      in
      vps ** {
        s = auxvp.s ;
        agr = auxvp.agr ;
        comp  = \\a => (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ vps.comp ! a ++ agent ;
      } ;

} ;
