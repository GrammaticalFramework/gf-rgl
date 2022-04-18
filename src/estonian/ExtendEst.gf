--# -path=.:../common:../abstract

concrete ExtendEst of Extend =
  CatEst ** ExtendFunctor -
  [
    -- Extensions of VP
    VPS, ListVPS, VPI, ListVPI, VPS2, ListVPS2, VPI2, ListVPI2,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS, QuestVPS, SQuestVPS, RelVPS,
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    MkVPS2, BaseVPS2, ConsVPS2, ConjVPS2, ComplVPS2, ReflVPS2,
    MkVPI2, BaseVPI2, ConsVPI2, ConjVPI2, ComplVPI2,

    -- Reflexives
    RNP, RNPList, Base_nr_RNP, Base_rn_RNP, Base_rr_RNP, ConjRNP, Cons_nr_RNP, Cons_rr_RNP, PredetRNP, ReflRNP, ReflPoss, ReflPron,

    -- Rest in alphabetical order
    AdAdV, AdjAsCN, AdjAsNP, ApposNP, AdvIsNP, A2VPSlash, ByVP,
    CardCNCard, CompBareCN, CompIQuant, CompQS, CompS, CompVP,
      ComplBareVS, ComplGenVV, ComplSlashPartLast, ComplVPSVV, CompoundAP, CompoundN,
    EmbedPresPart, EmbedSSlash, EmptyRelSlash, ExistsNP, ExistCN, ExistMassCN, ExistPluralCN,
    FocusAP, FocusAdV, FocusAdv, FocusObj, FrontComplDirectVQ, FrontComplDirectVS,
    GenIP, GenModIP, GenModNP, GenNP, GenRP, GerundAdv, GerundCN, GerundNP,
    IAdvAdv, ICompAP, InOrderToVP, N2VPSlash, NominalizeVPSlashNP,
    PassAgentVPSlash, PassVPSlash, PastPartAP, PastPartAgentAP, PositAdVAdj,
      PredAPVP, PredIAdvVP, PredVPSVV, PresPartAP, PrepCN, ProDrop, ProgrVPSlash, PurposeVP,
    SlashBareV2S, UttAccIP, UttAccNP, UttAdV, UttDatIP, UttDatNP, UttVPShort, WithoutVP


   ]
  with
    (Grammar = GrammarEst) **

  open
    GrammarEst,
    ResEst,
    (R=ResEst),
    (X=ExtraEst),
    IdiomEst,
    Coordination,
    Prelude,
    MorphoEst,
    LexiconEst,
    ParadigmsEst in {

---------------------------------
-- VPS, VPI, VPS2 + list versions
  lincat
    VPS = X.VPS ;
    [VPS] = X.ListVPS ;
    VPI = X.VPI ;
    [VPI] = X.ListVPI ;
    VPS2 = X.VPS ** {c2 : Compl} ;
    [VPS2] = X.ListVPS ** {c2 : Compl} ;
    VPI2 = X.VPI ** {c2 : Compl} ;
    [VPI2] = X.ListVPI ** {c2 : Compl}  ;

  lin
    MkVPS = X.MkVPS ;
    BaseVPS = X.BaseVPS ;
    ConsVPS = X.ConsVPS ;
    ConjVPS = X.ConjVPS ;

    PredVPS = X.PredVPS ;
    -- QuestVPS
    -- SQuestVPS
    -- RelVPS

    MkVPI = X.MkVPI ;
    BaseVPI = X.BaseVPI ;
    ConsVPI = X.ConsVPI ;
    ConjVPI = X.ConjVPI ;
    ComplVPIVV = X.ComplVPIVV ;

    MkVPS2 t p vps = MkVPS t p vps ** {c2 = vps.c2} ;
--    BaseVPS2, ConsVPS2, ConjVPS2,

    ComplVPS2 v np = lin VPS (v ** {
      -- TODO: param to record whether it's pos or neg, so we get right form of np
      s = \\agr => v.s ! agr ++ appCompl True Pos v.c2 np ;
      }) ;

--    ReflVPS2 v rnp =
-- MkVPI2, BaseVPI2, ConsVPI2, ConjVPI2, ComplVPI2,

---------------------------------
-- RNP + all related funs

  lincat
    RNP     = {s : Agr => NPForm => Str} ;
    RNPList = {s1,s2 : Agr => NPForm => Str} ;

  oper
    rnp2np : Agr -> RNP -> NPhrase = \agr,rnp -> emptyNP ** {
      a = agr ;
      s = rnp.s ! agr ;
      isPron = False ; -- ??
      } ;

  lin
    -- : VPSlash -> RNP -> VP ;   -- support my family and myself
    ReflRNP vps rnp = insertObj (\\b,p,a => appCompl True Pos vps.c2 (rnp2np a rnp)) vps ;

    -- : RNP
    ReflPron = {s = \\agr,npf => (reflPron agr).s ! npf} ;

    -- : Num -> CN -> RNP ;      -- my car(s)
    ReflPoss num cn = {
      s = \\a,npf => possPron ! a ++ num.s ! Sg ! Nom ++
        case npf of {
          NPCase c =>  cn.s ! NCase num.n c ;
          NPAcc => cn.s ! NCase num.n Gen } ;
      } ;

    PredetRNP predet rnp = {
      s = \\a,c => case a of {
        Ag n p => predet.s ! n ! c ++ rnp.s ! a ! c ;
        AgPol => predet.s ! Pl ! c ++ rnp.s ! a ! c }
      } ;

    ConjRNP conj rpns = conjunctDistrTable2 Agr NPForm conj rpns ;

    Base_rr_RNP x y = twoTable2 Agr NPForm x y ;
    Base_nr_RNP x y = twoTable2 Agr NPForm {s = \\a => x.s} y ;
    Base_rn_RNP x y = twoTable2 Agr NPForm x {s = \\a => y.s} ;
    Cons_rr_RNP x xs = consrTable2 Agr NPForm comma x xs ;
    Cons_nr_RNP x xs = consrTable2 Agr NPForm comma {s = \\a => x.s} xs ;

{-
    -- : Pron -> Num -> CN -> RNP -> NP ; -- his abandonment of his wife and children
    PossPronRNP pron num cn rnp =

    -- : NP -> Prep -> RNP -> RNP ;   -- a dispute with his wife
    AdvRAP adv rp =

    -- : VP -> Prep -> RNP -> VP ;    -- lectured about her travels
    AdvRNP adv rp =

    -- : AP -> Prep -> RNP -> AP ;    -- adamant in his refusal
    AdvRVP adv rp =
 -}

  oper
    possPron : Agr => Str = table {
      Ag Sg P1 => "minu" ;
      Ag Sg P2 => "sinu" ;
      Ag Sg P3 => "tema" ;
      Ag Pl P1 => "meie" ;
      Ag Pl P2 => "teie" ;
      Ag Pl P3 => "nende" ;
      AgPol => "teie"
      } ;


---------------------------------
-- A - B
  lin


    AdAdV ad adv = AdAdv ad adv ;

     -- : AP -> CN ;   -- a green one ; en grön (Swe)
    AdjAsCN ap = emptyCN ** {s = ap.s ! True} ; -- True = attributive ; False = predicative

    -- : AP -> NP
    AdjAsNP ap = MassNP (AdjAsCN ap) ;

    -- : NP -> NP -> NP
    ApposNP np1 np2 = np2 ** {
      s = \\nf => np1.s ! nf ++ np2.s ! nf ; -- comma or not?
      } ;

    -- : Adv -> NP -> Cl ;  -- here is the tree / here are the trees
    AdvIsNP adv np = mkClause (\_ -> adv.s) (agrP3 Sg) (UseComp (CompNP np)) ;

    -- : A2 -> VPSlash
    A2VPSlash a2 = UseComp (CompAP (UseA2 a2)) ** {c2 = a2.c2} ;

    -- : VP -> Adv ;
    ByVP = GerundAdv ;

---------------------------------
-- C

  lin

   -- : VS -> S -> VP ;
   ComplBareVS  v s = insertExtrapos s.s (predV v) ;

   -- : N -> N -> N ;      -- control system / controls system / control-system
   CompoundN noun cn = cn ** {
     s = \\nf => noun.s ! NCase Sg Gen ++ BIND ++ cn.s ! nf
     } ;

   -- : N -> A  -> AP ;     -- language independent / language-independent
   CompoundAP n a = PositA (a ** {s = \\d,af => n.s ! NCase Sg Nom ++ BIND ++ a.s ! d ! af}) ;

   -- : VS -> Utt -> VP ;      -- say: "today"
   ComplDirectVS vs utt = insertExtrapos (BIND ++ ":" ++ utt.s) (predV vs) ;

   -- : VQ -> Utt -> VP ;      -- ask: "when"
   ComplDirectVQ vq utt = insertExtrapos (BIND ++ ":" ++ utt.s) (predV vq) ;

   -- : S -> Comp ;                   -- (the fact is) that she sleeps
   CompS s = {s = \\_ => "et" ++ s.s} ;

   -- : QS -> Comp ;                  -- (the question is) who sleeps
   CompQS qs = {s = \\_ => qs.s } ;

   -- : Ant -> Pol -> VP -> Comp ;    -- (she is) to go
   CompVP ant pol vp = {s = \\a => infVPAnt ant.a (NPCase Nom) pol.p a vp InfDa } ;


  -- ComplGenVV v a p vp = insertObj (\\agr => a.s ++ p.s ++ infVP v.typ vp a.a p.p agr)
  --                                 (predVV v) ;

  -- ComplSlashPartLast vps np = {} ;  --- AR 7/3/2013

---------------------------------
-- E - F

  lin

  -- : VP -> SC ;   -- looking at Mary (is fun) / filmide vaatamine (on tore) / ___ga abielus olemine,
    EmbedPresPart vp = {s = infVP (NPCase Gen) Pos (agrP3 Sg) vp InfMine } ;

    EmbedSSlash s = {s = s.s ++ s.c2.s} ;

    -- : ClSlash -> RCl ;   -- he lives in
    EmptyRelSlash cls = {
      s = \\t,a,p,_ => cls.s ! t ! a ! p ++ cls.c2.s ;
      c = NPCase Nom
      } ;

    -- : CN -> Cl ;  -- there is a car / there is no car ; there is beer / there is no beer ; there are
    -- TODO: these all use the literal "exist" verb. Does Estonian have a construction for "there is"?
    ExistCN, ExistMassCN = \cn -> ExistsNP (MassNP cn) ;
    ExistPluralCN cn = ExistsNP (DetCN (DetQuant IndefArt NumPl) cn) ;

    -- : NP -> Cl ;  -- there exists a number / there exist numbers
    ExistsNP = IdiomEst.ExistNP ;

    --  : AP  -> NP      -> Utt ;   -- green was the tree
    FocusAP ap np =
      let pred : VP = UseComp (CompNP np) ;
          subj : NP = AdjAsNP ap ;
          cl : Cl = PredVP subj pred ;
       in UttS (UseCl (TTAnt TPres ASimul) PPos cl) ; -- use AdvIsNP for similar construction but that returns a Cl instead

    -- : Ad[vV] -> S -> Utt -- today I will sleep
    FocusAdV, FocusAdv = \adv,s -> cc2 adv s ;

    --  : NP  -> SSlash  -> Utt ;   -- her I love
    FocusObj np sslash = {s = appCompl True Pos sslash.c2 np ++ sslash.s} ;


    -- : NP -> VS -> Utt -> Cl ;      -- "I am here", she said
    FrontComplDirectVS np vs utt =
      let cl : Cl = PredVP np (UseV vs) ;
       in cl ** {s = \\t,a,p,o => utt.s ++ bindComma ++ cl.s ! t ! a ! p ! o} ;

    -- : NP -> VQ -> Utt -> Cl ;      -- "where", she asked
    FrontComplDirectVQ np vq utt =
      let cl : Cl = PredVP np (UseV vq) ;
       in cl ** {s = \\t,a,p,o => utt.s ++ bindComma ++ cl.s ! t ! a ! p ! o} ;


---------------------------------
-- G

  lin
    -- : NP -> Quant ;       -- this man's
    GenNP np = {
      s,sp = \\_,_ => linNP (NPCase Gen) np ;
      isNum  = False ;
      isDef  = True ;
      isNeg = False
      } ;

    -- : IP -> IQuant ;      -- whose
    GenIP ip = {s = \\_,_ => linIP (NPCase Gen) ip} ;

    -- : Num -> CN -> RP ;   -- whose car
    GenRP num cn = {
      s = \\n,c => let k = npform2case num.n c
                    in relPron ! NCase n Gen ++ cn.s ! NCase num.n k ;
      a = RNoAg
      } ;

-- In case the first two are not available, the following applications should in any case be.

    -- : Num -> NP -> CN -> NP ; -- this man's car(s)
    GenModNP num np cn = DetCN (DetQuant (GenNP (lin NP np)) num) cn ;

    -- : Num -> IP -> CN -> IP ; -- whose car(s)
    GenModIP num ip cn = IdetCN (IdetQuant (GenIP (lin IP ip)) num) cn ;

    -- : VP -> Adv
    GerundAdv vp = {s = infVPdefault vp InfDes} ;

    -- : VP -> CN    -- publishing of the document (can get a determiner)
    GerundCN vp = emptyCN ** {s = \\nf => infVPdefault vp InfMine} ;

    -- : VP -> NP    -- publishing the document (by nature definite)
    GerundNP vp = MassNP (GerundCN vp) ;

---------------------------------
-- I - N

  lin

    -- : AP -> IComp ;   -- "how old"
    ICompAP ap = icompAP "kui" ap ;

    -- : Adv -> IAdv ;   -- "how often"
    IAdvAdv adv = { s = "kui" ++ adv.s } ;

    -- : VP -> Adv -- et raamatut paremini näha
    InOrderToVP vp = {s = "et" ++ infVPdefault vp InfDa} ;

    -- : N2 -> VPSlash
    N2VPSlash n2 = UseComp (CompCN (UseN2 n2)) ** {c2 = n2.c2} ;

    -- : VPSlash -> NP -> NP ; publishing of the document
    -- NominalizeVPSlashNP vpslash np = {} ;


---------------------------------
-- P

  lin

    -- : VPSlash -> NP -> VP ;  -- be begged by her to go
    PassAgentVPSlash vps np = let vp : VP = PassVPSlash vps in vp ** {
      adv = vp.adv ++ appCompl True Pos by8agent_Prep np ;
      } ;


    -- : VPSlash -> VP ; -- be forced to sleep
    PassVPSlash vps = vps ** {
      s = \\vf => case vf of {
                    VIFin t => vps.s ! VIPass t ;
                    x => vps.s ! x } ;
      sc = compl2subjcase vps.c2
      } ;

    -- : VPSlash -> AP ;    -- täna leitud
    PastPartAP vp = {
      s = \\_,_ => vp2adv vp True (VIPass Past) ;
      infl = Invariable
      } ;

    -- : VP -> AP ;   -- (the man) looking at Mary / filme vaatav (mees)
    PresPartAP vp = {
      s = \\_,_ => vp2adv vp True VIPresPart ;
      infl = Invariable
      } ;

    -- : VPSlash -> NP -> AP    -- hobisukeldujate poolt leitud (süvaveepomm)
    PastPartAgentAP vp np = {
      s = \\_,_ => appCompl True Pos by8agent_Prep np ++ vp2adv vp True (VIPass Past) ;
      infl = Invariable
      } ;

    PositAdVAdj = PositAdvAdj ;

    -- : AP -> VP -> Cl ;   -- it is good to walk / on hea kõndida
    PredAPVP ap vp =
      let heaOllaVP : VP = insertObj (\\_,_,_ => ap.s ! True ! NCase Sg Nom) vp ; -- puts AP into the s2 field
          heaOllaComp : Comp = CompVP ASimul PPos heaOllaVP ; -- chooses InfDa, fixes word order
          heaOlla : VP = UseComp heaOllaComp -- looks silly, but I want to reuse the abstract syntax funs :-P
       in existClause noSubj (agrP3 Sg) heaOlla ;

    -- : IAdv -> VP -> QCl ; -- how to walk?
    PredIAdvVP iadv vp = {s = \\t,a,p => iadv.s ++ infVPdefault vp InfMa} ;

    PrepCN prep cn = PrepNP prep (MassNP cn) ;

    ProDrop pron = pron ** {s = \\_ => []} ;

    ProgrVPSlash vps = ProgrVP vps ** vps ;

    PurposeVP = InOrderToVP ; --- is there a difference?

  oper
    -- calling infVP with the "default arguments": NPCase Nom, Pos, agrP3 Sg
    infVPdefault : VP -> InfForms -> Str = infVP (NPCase Nom) Pos (agrP3 Sg) ;

    vp2adv : R.VP -> Bool -> VIForm -> Str = \vp,sentIsPos,vif ->
      let vpforms : {fin,inf : Str} = case vif of {
            VIInf if => applyInfFormsVP {stem=if ; suf="a"} vp ; --- this oper shouldn't be used if you want to use an InfForm but just trying to be robust here
            _        => mkVPForms vp.v ! vif ! Simul ! Pos ! agrP3 Sg} ;
       in  vp.s2 ! sentIsPos ! Pos ! agrP3 Sg  -- raamatut
        ++ vp.adv                           -- paremini
        ++ vp.p                             -- ära
        ++ vpforms.fin -- tunda/tundes/tundmata/...
        ++ vpforms.inf -- TODO is this necessary???
        ++ vp.ext ;

---------------------------------
-- S - W

  lin

    -- SlashBareV2S v s = insertExtrapos s.s (predV v) ** v ;

    UseDAP,
      UseDAPFem,
      UseDAPMasc = DetNP ;

    UttAccIP ip = {s = linIP NPAcc ip} ;
    UttAccNP np = {s = linNP NPAcc np} ;
    UttAdV adv = adv ;
    UttDatIP ip = {s = linIP (NPCase Part) ip} ; -- is partitive a reasonable translation?
    UttDatNP np = {s = linNP (NPCase Part) np} ;

    -- : VP -> Utt ;  -- There's no "short form", so just using InfDa instead of InfMa
    UttVPShort vp = {s = infVPdefault vp InfDa} ;

    -- : VP -> Adv ;  -- ilma raamatut nägemata
    WithoutVP vp = {s = "ilma" ++ infVPdefault vp InfMata} ;


}
