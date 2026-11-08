concrete ExtendFao of Extend = CatFao ** 
	open Prelude, ParadigmsFao, ResFao, (P = ParamX) in {

lincat
  VPS = {s : Gender => PersNum => Str} ;
  [VPS] = {s1,s2 : Gender => PersNum => Str} ;
  VPI = {s : Str} ;
  [VPI] = {s1,s2 : Str} ;
  VPS2 = {s : Gender => PersNum => Str ; c2 : Compl ; sc : Str} ;
  [VPS2] = {s1,s2 : Gender => PersNum => Str ; c2 : Compl ; sc : Str} ;
  VPI2 = {s : Str ; c2 : Compl ; sc : Str} ;
  [VPI2] = {s1,s2 : Str ; c2 : Compl ; sc : Str} ;
  [Comp] = {s1,s2 : Gender => Number => Str} ;
  [Imp] = {s1,s2 : Polarity => Number => Str} ;
  RNP = {s : Case => Str ; g : Gender ; n : Number ; p : Person} ;
  RNPList = {s1,s2 : Case => Str ; g : Gender ; n : Number ; p : Person} ;
  X = {s : Str} ;

lin
  UttAdV adv = {s = adv.s} ;

  iFem_Pron  = mkPron "eg" "meg" "mær" "mín" Masc Sg P1 ;
  youFem_Pron = mkPron "tú" "teg" "tær" "tín" Masc Sg P2 ;
  weFem_Pron = mkPron "vit" "okkum" "okkum" "okkara" Masc Pl P1 ;
  youPlFem_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  theyFem_Pron = mkPron "tær" "tær" "teimum" "teirra" Masc Pl P3 ;
  youPolFem_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  youPolPl_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  youPolPlFem_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;

lin
    GenIP ip = {s = ip.s} ;
    GenRP num cn = {s = cn.s ! Def ! num.n ! Gen} ;
    GenModNP num np cn = mkNP (np.s ! Gen ++ cn.s ! Def ! num.n ! Nom) cn.g num.n P3 ;
    GenModIP num ip cn = {s = ip.s ++ cn.s ! Def ! num.n ! Nom ; n = num.n} ;
    CompBareCN cn = {s = \\_,n => cn.s ! Indef ! n ! Nom} ;

    StrandQuestSlash ip cls = {
      s = \\t,pol => ip.s ++ cls.s ! t ! pol ;
      anterior = \\t,pol => ip.s ++ cls.s ! t ! pol ;
      future = \\pol => ip.s ++ cls.s ! Pres ! pol ;
      conditional = \\pol => ip.s ++ cls.s ! Past ! pol
    } ;
    StrandRelSlash rp cls = {s = \\t,pol,_,_ => rp.s ++ cls.s ! t ! pol} ;
    EmptyRelSlash cls = {s = \\t,pol,_,_ => cls.s ! t ! pol} ;

    MkVPS temp pol vp =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = \\g,p => temp.s ++ pol.s ++ vp.Indicative ! tense ! pol.p ! g ! p
      } ;
    ConjVPS conj xs = {s = \\g,p => xs.s1 ! g ! p ++ conj.s ++ xs.s2 ! g ! p} ;
    PredVPS np vps = {s = np.s ! Nom ++ vps.s ! np.g ! persNum np.n np.p} ;
    SQuestVPS np vps = {s = np.s ! Nom ++ vps.s ! np.g ! persNum np.n np.p} ;
    QuestVPS ip vps = {s = ip.s ++ vps.s ! Masc ! persNum ip.n P3} ;
    RelVPS rp vps = {s = \\g,p => rp.s ++ vps.s ! g ! p} ;

    BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPS x xs = {s1 = \\g,p => x.s ! g ! p ++ "," ++ xs.s1 ! g ! p ; s2 = xs.s2} ;

    ExistS temp pol np =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = temp.s ++ "tað" ++ copula ! tense ! persNum np.n P3 ++ negStr pol.p ++ np.s ! Nom
      } ;
    ExistNPQS temp pol np =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = temp.s ++ "er tað" ++ np.s ! Nom
      } ;
    ExistIPQS temp pol ip =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = temp.s ++ ip.s ++ copula ! tense ! persNum ip.n P3 ++ negStr pol.p
      } ;

    MkVPI vp = {s = vp.Nonfinite} ;
    ConjVPI conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
    ComplVPIVV vv vpi = {
      Converb = vv.Converb ++ vv.particle ++ vpi.s ;
      Imperative = \\n => vv.imperative ! n ++ vv.particle ++ vpi.s ;
      Indicative = \\t,pol,g,p => vv.Indicative ! t ! p ++ vv.particle ++ negStr pol ++ vpi.s ;
      Finite = vv.Indicative ;
      Remainder = \\pol,_,_ => negStr pol ++ vv.particle ++ vpi.s ;
      Nonfinite = vv.Nonfinite ++ vv.particle ++ vpi.s ;
      Participle = \\t => vv.Participle ! t ++ vv.particle ++ vpi.s
    } ;
    BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPI x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;

    MkVPS2 temp pol vps =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = \\g,p => temp.s ++ pol.s ++ vps.Indicative ! tense ! p ++ vps.particle ++ negStr pol.p ++ vps.sc ;
        c2 = vps.c2 ;
        sc = vps.sc
      } ;
    ConjVPS2 conj xs = {s = \\g,p => xs.s1 ! g ! p ++ conj.s ++ xs.s2 ! g ! p ; c2 = xs.c2 ; sc = xs.sc} ;
    ComplVPS2 vps np = {s = \\g,p => vps.s ! g ! p ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc} ;
    ReflVPS2 vps rnp = {s = \\g,p => vps.s ! g ! p ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc} ;
    BaseVPS2 x y = {s1 = x.s ; s2 = y.s ; c2 = y.c2 ; sc = y.sc} ;
    ConsVPS2 x xs = {s1 = \\g,p => x.s ! g ! p ++ "," ++ xs.s1 ! g ! p ; s2 = xs.s2 ; c2 = xs.c2 ; sc = xs.sc} ;

    MkVPI2 vps = {s = vps.Nonfinite ++ vps.particle ; c2 = vps.c2 ; sc = vps.sc} ;
    ConjVPI2 conj xs = {s = xs.s1 ++ conj.s ++ xs.s2 ; c2 = xs.c2 ; sc = xs.sc} ;
    ComplVPI2 vpi np = {s = vpi.s ++ vpi.c2.s ++ np.s ! vpi.c2.c ++ vpi.sc} ;
    BaseVPI2 x y = {s1 = x.s ; s2 = y.s ; c2 = y.c2 ; sc = y.sc} ;
    ConsVPI2 x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2 ; c2 = xs.c2 ; sc = xs.sc} ;

    ConjComp conj xs = {s = \\g,n => xs.s1 ! g ! n ++ conj.s ++ xs.s2 ! g ! n} ;
    BaseComp x y = {s1 = x.s ; s2 = y.s} ;
    ConsComp x xs = {s1 = \\g,n => x.s ! g ! n ++ "," ++ xs.s1 ! g ! n ; s2 = xs.s2} ;
    ConjImp conj xs = {s = \\pol,n => xs.s1 ! pol ! n ++ conj.s ++ xs.s2 ! pol ! n} ;
    BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = {s1 = \\pol,n => x.s ! pol ! n ++ "," ++ xs.s1 ! pol ! n ; s2 = xs.s2} ;

    ProDrop p = p ** {s = \\_ => []} ;
    ICompAP ap = {s = ap.s ! Strong ! Neuter ! Sg ! Nom} ;
    IAdvAdv adv = {s = adv.s} ;
    CompIQuant iq = {s = iq.s} ;
    PrepCN prep cn = {s = prep.s ++ cn.s ! Indef ! Sg ! prep.c} ;
    FocusObj np ss = {s = np.s ! Acc ++ ss.s} ;
    FocusAdv adv s = {s = adv.s ++ s.s} ;
    FocusAdV adv s = {s = adv.s ++ s.s} ;
    PresPartAP vp = {s = \\_,_,_,_ => vp.Participle ! Pres} ;
    EmbedPresPart vp = {s = vp.Participle ! Pres} ;
    PastPartAP vps = {s = \\_,_,_,_ => vps.Participle ! Past ++ vps.particle ++ vps.sc} ;
    PastPartAgentAP vps np = {s = \\_,_,_,_ => vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat} ;
    PassVPSlash vps = {
      Converb = "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Imperative = \\n => case n of {Sg => "ver" ; Pl => "verið"} ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Indicative = \\t,pol,_,p => copula ! t ! p ++ negStr pol ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Finite = copula ;
      Remainder = \\pol,_,_ => negStr pol ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Nonfinite = "vera" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Participle = \\_ => "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc
    } ;
    PassAgentVPSlash vps np = {
      Converb = "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Imperative = \\n => case n of {Sg => "ver" ; Pl => "verið"} ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Indicative = \\t,pol,_,p => copula ! t ! p ++ negStr pol ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Finite = copula ;
      Remainder = \\pol,_,_ => negStr pol ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Nonfinite = "vera" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Participle = \\_ => "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat
    } ;
    ProgrVPSlash vps = vps ** {
      Nonfinite = "vera við at" ++ vps.Nonfinite ;
      Participle = \\_ => "verið við at" ++ vps.Nonfinite
    } ;
    ExistsNP np = {
      Converb = "tað finst" ++ np.s ! Nom ;
      Indicative = \\t,pol => "tað" ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ;
      Interrogative = \\t,pol => copula ! t ! persNum np.n P3 ++ "tað" ++ negStr pol ++ np.s ! Nom ;
      Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ "verið" ++ np.s ! Nom ;
      AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ "verið" ++ np.s ! Nom ;
      Nonfinite = "vera" ++ np.s ! Nom ;
      Participle = \\_ => "verið" ++ np.s ! Nom
    } ;
    AdvIsNP adv np = {
      Converb = adv.s ++ copula ! Pres ! persNum np.n P3 ++ np.s ! Nom ;
      Indicative = \\t,pol => adv.s ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ;
      Interrogative = \\t,pol => copula ! t ! persNum np.n P3 ++ adv.s ++ negStr pol ++ np.s ! Nom ;
      Future = \\pol => adv.s ++ futureAux ! persNum np.n P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      FutureInterrogative = \\pol => adv.s ++ futureAux ! persNum np.n P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      Conditional = \\pol => adv.s ++ conditionalAux ! persNum np.n P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      ConditionalInterrogative = \\pol => adv.s ++ conditionalAux ! persNum np.n P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      Anterior = \\t,pol => adv.s ++ perfectAux ! t ! persNum np.n P3 ++ negStr pol ++ "verið" ++ np.s ! Nom ;
      AnteriorInterrogative = \\t,pol => adv.s ++ perfectAux ! t ! persNum np.n P3 ++ negStr pol ++ "verið" ++ np.s ! Nom ;
      Nonfinite = adv.s ++ "vera" ++ np.s ! Nom ;
      Participle = \\_ => adv.s ++ "verið" ++ np.s ! Nom
    } ;

    ComplBareVS vs s = {
      Converb = vs.Converb ++ vs.particle ++ s.s ;
      Imperative = \\n => vs.imperative ! n ++ vs.particle ++ s.s ;
      Indicative = \\t,pol,g,p => vs.Indicative ! t ! p ++ vs.particle ++ negStr pol ++ s.s ;
      Finite = vs.Indicative ;
      Remainder = \\pol,_,_ => negStr pol ++ vs.particle ++ s.s ;
      Nonfinite = vs.Nonfinite ++ vs.particle ++ s.s ;
      Participle = \\t => vs.Participle ! t ++ vs.particle ++ s.s
    } ;
    SlashBareV2S v s = v ** {c2 = v.c2 ; sc = s.s} ;
    ComplDirectVS vs utt = {
      Converb = vs.Converb ++ vs.particle ++ utt.s ;
      Imperative = \\n => vs.imperative ! n ++ vs.particle ++ utt.s ;
      Indicative = \\t,pol,g,p => vs.Indicative ! t ! p ++ vs.particle ++ negStr pol ++ utt.s ;
      Finite = vs.Indicative ;
      Remainder = \\pol,_,_ => negStr pol ++ vs.particle ++ utt.s ;
      Nonfinite = vs.Nonfinite ++ vs.particle ++ utt.s ;
      Participle = \\t => vs.Participle ! t ++ vs.particle ++ utt.s
    } ;
    ComplDirectVQ vq utt = {
      Converb = vq.Converb ++ vq.particle ++ utt.s ;
      Imperative = \\n => vq.imperative ! n ++ vq.particle ++ utt.s ;
      Indicative = \\t,pol,g,p => vq.Indicative ! t ! p ++ vq.particle ++ negStr pol ++ utt.s ;
      Finite = vq.Indicative ;
      Remainder = \\pol,_,_ => negStr pol ++ vq.particle ++ utt.s ;
      Nonfinite = vq.Nonfinite ++ vq.particle ++ utt.s ;
      Participle = \\t => vq.Participle ! t ++ vq.particle ++ utt.s
    } ;
    FrontComplDirectVS np vs utt = {
      Converb = utt.s ++ np.s ! Nom ++ vs.Converb ;
      Indicative = \\t,pol => utt.s ++ np.s ! Nom ++ vs.Indicative ! t ! persNum np.n np.p ++ negStr pol ;
      Interrogative = \\t,pol => utt.s ++ vs.Indicative ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ;
      Future = \\pol => utt.s ++ np.s ! Nom ++ futureAux ! persNum np.n np.p ++ negStr pol ++ vs.Nonfinite ;
      FutureInterrogative = \\pol => utt.s ++ futureAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ vs.Nonfinite ;
      Conditional = \\pol => utt.s ++ np.s ! Nom ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ vs.Nonfinite ;
      ConditionalInterrogative = \\pol => utt.s ++ conditionalAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ vs.Nonfinite ;
      Anterior = \\t,pol => utt.s ++ np.s ! Nom ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ vs.Converb ;
      AnteriorInterrogative = \\t,pol => utt.s ++ perfectAux ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ vs.Converb ;
      Nonfinite = utt.s ++ np.s ! Nom ++ vs.Nonfinite ;
      Participle = \\t => utt.s ++ np.s ! Nom ++ vs.Participle ! t
    } ;
    FrontComplDirectVQ np vq utt = {
      Converb = utt.s ++ np.s ! Nom ++ vq.Converb ;
      Indicative = \\t,pol => utt.s ++ np.s ! Nom ++ vq.Indicative ! t ! persNum np.n np.p ++ negStr pol ;
      Interrogative = \\t,pol => utt.s ++ vq.Indicative ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ;
      Future = \\pol => utt.s ++ np.s ! Nom ++ futureAux ! persNum np.n np.p ++ negStr pol ++ vq.Nonfinite ;
      FutureInterrogative = \\pol => utt.s ++ futureAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ vq.Nonfinite ;
      Conditional = \\pol => utt.s ++ np.s ! Nom ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ vq.Nonfinite ;
      ConditionalInterrogative = \\pol => utt.s ++ conditionalAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ vq.Nonfinite ;
      Anterior = \\t,pol => utt.s ++ np.s ! Nom ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ vq.Converb ;
      AnteriorInterrogative = \\t,pol => utt.s ++ perfectAux ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ vq.Converb ;
      Nonfinite = utt.s ++ np.s ! Nom ++ vq.Nonfinite ;
      Participle = \\t => utt.s ++ np.s ! Nom ++ vq.Participle ! t
    } ;
    PredAPVP ap vp = {
      Converb = "tað er" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Indicative = \\t,pol => "tað" ++ copula ! t ! PSg P3 ++ negStr pol ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Interrogative = \\t,pol => copula ! t ! PSg P3 ++ "tað" ++ negStr pol ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ "verið" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ "verið" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Nonfinite = "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Participle = \\_ => "verið" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ++ vp.Nonfinite
    } ;
    AdjAsCN ap = mkCN (ap.s ! Strong ! Masc ! Sg ! Nom) Masc ;
    AdjAsNP ap = mkNP (ap.s ! Strong ! Masc ! Sg ! Nom) Masc Sg P3 ;
    PredIAdvVP iadv vp = {
      s = \\t,pol => iadv.s ++ vp.Nonfinite ;
      anterior = \\t,pol => iadv.s ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ vp.Converb ;
      future = \\pol => iadv.s ++ futureAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ;
      conditional = \\pol => iadv.s ++ conditionalAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite
    } ;
    EmbedSSlash ss = {s = ss.s} ;

    ReflPron = mkNP "seg" Masc Sg P3 ;
    ReflPoss num cn = {
      s = \\c => reflPoss cn.g num.n c ++ cn.p ! num.n ! c ;
      g = cn.g ; n = num.n ; p = P3
    } ;
    PredetRNP pred rnp = rnp ** {s = \\c => pred.s ++ rnp.s ! c} ;
    AdvRNP np prep rnp = rnp ** {s = \\c => rnp.s ! c ++ prep.s ++ np.s ! prep.c} ;
    AdvRVP vp prep rnp = vp ** {
      Converb = vp.Converb ++ prep.s ++ rnp.s ! prep.c ;
      Imperative = \\n => vp.Imperative ! n ++ prep.s ++ rnp.s ! prep.c ;
      Indicative = \\t,pol,g,p => vp.Indicative ! t ! pol ! g ! p ++ prep.s ++ rnp.s ! prep.c ;
      Remainder = \\pol,g,p => vp.Remainder ! pol ! g ! p ++ prep.s ++ rnp.s ! prep.c ;
      Nonfinite = vp.Nonfinite ++ prep.s ++ rnp.s ! prep.c ;
      Participle = \\t => vp.Participle ! t ++ prep.s ++ rnp.s ! prep.c
    } ;
    AdvRAP ap prep rnp = {s = \\d,g,n,c => ap.s ! d ! g ! n ! c ++ prep.s ++ rnp.s ! prep.c} ;
    PossPronRNP pron num cn rnp = mkNP (pron.poss ! cn.g ! num.n ! Nom ++ cn.p ! num.n ! Nom ++ rnp.s ! Gen) cn.g num.n P3 ;
    ConjRNP conj xs = mkNP (xs.s1 ! Nom ++ conj.s ++ xs.s2 ! Nom) xs.g Pl P3 ;
    Base_rr_RNP x y = {s1 = x.s ; s2 = y.s ; g = x.g ; n = Pl ; p = P3} ;
    Base_nr_RNP x y = {s1 = x.s ; s2 = y.s ; g = x.g ; n = Pl ; p = P3} ;
    Base_rn_RNP x y = {s1 = x.s ; s2 = y.s ; g = x.g ; n = Pl ; p = P3} ;
    Cons_rr_RNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c ; s2 = xs.s2 ; g = xs.g ; n = Pl ; p = P3} ;
    Cons_nr_RNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c ; s2 = xs.s2 ; g = xs.g ; n = Pl ; p = P3} ;
    ReflPossPron = {s = \\_,g,n,c => reflPoss g n c ; sp = Indef ; d = Weak} ;
    CompoundN n1 n2 = mkCN (n1.s ! Indef ! Sg ! Nom ++ BIND ++ n2.s ! Indef ! Sg ! Nom) n2.g ;
    CompoundAP n a = {s = \\d,g,num,c => n.s ! Indef ! Sg ! Nom ++ BIND ++ a.s ! d ! g ! num ! c} ;
    GerundCN vp = mkCN vp.Nonfinite Neuter ;
    GerundNP vp = mkNP vp.Nonfinite Neuter Sg P3 ;
    GerundAdv vp = {s = vp.Nonfinite} ;
    WithoutVP vp = {s = "uttan at" ++ vp.Nonfinite} ;
    ByVP vp = {s = "við at" ++ vp.Nonfinite} ;
    ApposNP np app = np ** {s = \\c => np.s ! c ++ "," ++ app.s ! Nom} ;
    AdAdV ada adv = {s = ada.s ++ adv.s} ;
    PositAdVAdj a = {s = a.s ! Strong ! Neuter ! Sg ! Nom} ;
    CompS s = {s = \\_,_ => s.s} ;
    CompQS qs = {s = \\_,_ => qs.s} ;
    UttVPShort vp = {s = vp.Nonfinite} ;
    ComplSlashPartLast vps np = {
      Converb = vps.Converb ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Imperative = \\n => vps.imperative ! n ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Indicative = \\t,pol,g,p => vps.Indicative ! t ! p ++ negStr pol ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Finite = vps.Indicative ;
      Remainder = \\pol,_,_ => negStr pol ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Nonfinite = vps.Nonfinite ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Participle = \\t => vps.Participle ! t ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc
    } ;
    UseComp_estar comp = {
      Converb = "verið" ++ comp.s ! Masc ! Sg ;
      Imperative = \\n => case n of {Sg => "ver" ; Pl => "verið"} ++ comp.s ! Masc ! n ;
      Indicative = \\t,pol,g,p => copula ! t ! p ++ negStr pol ++ comp.s ! g ! persNumNumber p ;
      Finite = copula ;
      Remainder = \\pol,g,p => negStr pol ++ comp.s ! g ! persNumNumber p ;
      Nonfinite = "vera" ++ comp.s ! Masc ! Sg ;
      Participle = \\_ => "verið" ++ comp.s ! Masc ! Sg
    } ;
    UseComp_ser comp = {
      Converb = "verið" ++ comp.s ! Masc ! Sg ;
      Imperative = \\n => case n of {Sg => "ver" ; Pl => "verið"} ++ comp.s ! Masc ! n ;
      Indicative = \\t,pol,g,p => copula ! t ! p ++ negStr pol ++ comp.s ! g ! persNumNumber p ;
      Finite = copula ;
      Remainder = \\pol,g,p => negStr pol ++ comp.s ! g ! persNumNumber p ;
      Nonfinite = "vera" ++ comp.s ! Masc ! Sg ;
      Participle = \\_ => "verið" ++ comp.s ! Masc ! Sg
    } ;
    SubjRelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.g ! persNum np.n np.p} ;
    theyNeutr_Pron = {
      s = table {Nom => "tey" ; Acc => "tey" ; Dat => "teimum" ; Gen => "teirra"} ;
      poss = \\_,_,_ => "teirra" ;
      g = Neuter ; n = Pl ; p = P3
    } ;
    UttAccNP np = {s = np.s ! Acc} ;
    UttDatNP np = {s = np.s ! Dat} ;
    UttAccIP ip = {s = ip.s} ;
    UttDatIP ip = {s = ip.s} ;
    UseDAP dap = mkNP (dap.s ! Neuter ! Nom) Neuter dap.n P3 ;
    UseDAPMasc dap = mkNP (dap.s ! Masc ! Nom) Masc dap.n P3 ;
    UseDAPFem dap = mkNP (dap.s ! Fem ! Nom) Fem dap.n P3 ;
    CardCNCard card cn = {s = \\_,c => card.s ! cn.g ! c ++ cn.s ! Indef ! card.n ! c ; n = Pl} ;
    SubjunctRelCN cn rs = cn ** {
      s = \\sp,n,c => cn.s ! sp ! n ! c ++ rs.s ! cn.g ! persNum n P3 ;
      p = \\n,c => cn.p ! n ! c ++ rs.s ! cn.g ! persNum n P3
    } ;

  TPastSimple = {s = [] ; t = P.Past} ;

}
