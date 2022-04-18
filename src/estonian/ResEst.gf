--1 Estonian auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work. To define everything that is needed to
-- implement $Test$, it moreover contains regular lexical
-- patterns needed for $Lex$.

resource ResEst = ParamX ** open Prelude in {

  flags optimize=all ; coding=utf8;


--2 Parameters for $Noun$

-- This is the $Case$ as needed for both nouns and $NP$s.

  param
    Case = Nom | Gen | Part | Transl
         | Illat | Iness | Elat | Allat | Adess | Ablat
         ;
    NForm = NCase Number Case ;

  oper
    -- Reduce the Case parameter: many cases use the Genitive stem and just add suffix to it
    CasePlus : Type = {
      c : Case ;  -- e.g. Gen
      suf : Str   -- e.g. "ga" for comitative
      } ;

      Nominative  = {c = Nom ;   suf = []} ;
      Genitive    = {c = Gen ;   suf = []} ;
      Partitive   = {c = Part ;   suf = []} ;
      Illative    = {c = Illat ;  suf = []} ;
      Inessive    = {c = Iness ;  suf = []} ;
      Elative     = {c = Elat ;   suf = []} ;
      Allative    = {c = Allat ;  suf = []} ;
      Adessive    = {c = Adess ;  suf = []} ;
      Ablative    = {c = Ablat ;  suf = []} ;
      Translative = {c = Transl ; suf = []} ;
      Terminative = {c = Gen ;    suf = BIND ++ "ni"} ;
      Essive      = {c = Gen ;    suf = BIND ++ "na"} ;
      Abessive    = {c = Gen ;    suf = BIND ++ "ta"} ;
      Comitative  = {c = Gen ;    suf = BIND ++ "ga"} ;

  param
-- Agreement of $NP$ has number*person and the polite second ("te olette valmis").
    Agr = Ag Number Person | AgPol ;

  oper
    complNumAgr : Agr -> Number = \a -> case a of {
      Ag n _ => n ;
      AgPol  => Sg
      } ;
    verbAgr : Agr -> {n : Number ; p : Person} = \a -> case a of {
      Ag n p => {n = n  ; p = p} ;
      AgPol  => {n = Pl ; p = P2}
      } ;

  oper
    IPhrase : Type = {
      s : NPForm => Str ; -- the noun phrase + premodifiers
      postmod : Str ;     -- adverb, RS, etc. other postmods
      n : Number
    } ;

    NPhrase : Type = {
      s : NPForm => Str ; -- the noun phrase + premodifiers
      postmod : Str ;     -- adverb, RS, etc. other postmods
      a : Agr ;
      isPron : Bool
      } ;

    emptyNP : NPhrase = {
      s = \\_ => [] ;
      postmod = [] ;
      a = agrP3 Sg ;
      isPron = False
    } ;

    emptyIP : IPhrase = {
      s = \\_ => [] ;
      postmod = [] ;
      n = Sg ;
    } ;

    linNP : NPForm -> NPhrase -> Str = \npf,np -> np.s ! npf ++ np.postmod ;
    linIP : NPForm -> IPhrase -> Str = \npf,ip -> ip.s ! npf ++ ip.postmod ;
--
--2 Adjectives
--
-- The major division is between the comparison degrees. A degree fixed,
-- an adjective is like common nouns, except for the adverbial form.

param
  AForm = AN NForm | AAdv ;

  Infl = Regular | Participle | Invariable ;

oper
  Adjective : Type = {s : Degree => AForm => Str} ;

  APhrase : Type = {s : Bool => NForm => Str ; infl : Infl} ;

--2 Noun phrases
--
-- Two forms of *virtual accusative* are needed for nouns in singular,
-- the nominative and the genitive one ("loen raamatu"/"loe raamat").
-- For nouns in plural, only a nominative accusative exists in positive clauses.
-- Pronouns use the partitive as their accusative form ("mind", "sind"), in both
-- positive and negative, indicative and imperative clauses.

param
  NPForm = NPCase Case | NPAcc ;

oper
  NPFormPlus : Type = {
    npf : NPForm ;  -- e.g. NPCase Gen
    suf : Str       -- e.g. "ga" for comitative
    } ;

  casep2npformp : CasePlus -> NPFormPlus = \cp -> cp ** {npf = NPCase cp.c} ;
  case2npformp  : NPForm -> NPFormPlus   = \npf-> {npf = npf ; suf = []} ;

  npform2case : Number -> NPForm -> Case = \n,f ->

--  type signature: workaround for gfc bug 9/11/2007
    case <<f,n> : NPForm * Number> of {
      <NPCase c,_> => c ;
      <NPAcc,Sg>   => Gen ;-- appCompl does the job
      <NPAcc,Pl>   => Nom
    } ;

--2 For $Verb$

-- A special form is needed for the negated plural imperative.

param
  VForm =
     Inf InfStem
   | Presn Number Person
   | Impf Number Person
   | Condit Number Person
   | ConditPass --loetagu
   | Imper Number
   | ImperP3
   | ImperP1Pl
   | ImperPass
   | PassPresn Bool
   | PassImpf Bool
   | Quotative Voice
   | PresPart Voice
   | PastPart Voice
   ;

  Voice = Act | Pass ;

  InfStem =
     InfD -- luge+da/des, but can be irregular: tulla, tulles
   | InfM -- lugema/mas/mast/maks/mata/mine
   ;
oper
  InfForms : Type = {stem : InfStem ; suf : Str} ;

  InfDa, InfDes, InfMa, InfMas, InfMast, InfMata, InfMaks, InfMine : InfForms ;
  InfDa  = {stem = InfD ; suf = "a"} ;    -- lugeda
  InfDes = {stem = InfD ; suf = "es"} ;   -- lugedes
  InfMa = {stem = InfM ; suf = "a"} ;     -- lugema
  InfMas = {stem = InfM ; suf = "as"} ;   -- lugemas
  InfMast = {stem = InfM ; suf = "ast"} ; -- lugemast
  InfMata = {stem = InfM ; suf = "ata"} ; -- lugemata
  InfMaks = {stem = InfM ; suf = "aks"} ; -- lugemaks
  InfMine = {stem = InfM ; suf = "ine"} ; -- lugemine

  applyInfFormsVP : InfForms -> VP -> {fin,inf : Str} = \if,vp ->
    let vpforms : VPForms = mkVPForms vp.v ;
        stemOnly : {fin,inf : Str} = vpforms ! VIInf if.stem ! Simul ! Pos ! agrP3 Sg ;
     in stemOnly ** {fin = glue stemOnly.fin if.suf} ; -- Despite the name, the infinite form is in the "fin" field, "inf" contains participle

  applyInfFormsV  : InfForms -> (VForm => Str)  -> Str = \if,vf ->
    glue (vf  ! Inf   if.stem) if.suf ;

param
  SType = SDecl | SQuest | SInv ;

--2 For $Relative$

    RAgr = RNoAg | RAg Agr ;

--2 For $Numeral$

    CardOrd = NCard NForm | NOrd NForm ;

--2 Transformations between parameter types

  oper
    agrP3 : Number -> Agr = \n ->
      Ag n P3 ;

    conjAgr : Agr -> Agr -> Agr = \a,b -> case <a,b> of {
      <Ag n p, Ag m q> => Ag (conjNumber n m) (conjPerson p q) ;
      <Ag n p, AgPol>  => Ag Pl (conjPerson p P2) ;
      <AgPol,  Ag n p> => Ag Pl (conjPerson p P2) ;
      _ => b
      } ;

---

  Compl : Type = {s : Str ; c : NPFormPlus ; isPre : Bool} ;

  npfplus2compl : NPFormPlus -> Compl = \npf -> {s = [] ; c = npf ; isPre = False} ;

  appCompl : Bool -> Polarity -> Compl -> NPhrase -> Str = \isFin,b,co,np ->
    let
      c = case co.c.npf of {
        NPAcc => case b of {
          Neg => NPCase Part ; -- ma ei näe raamatut/sind
          Pos => case isFin of {
               True => NPAcc ; -- ma näen raamatu/sind
               _ => case np.isPron of {
                  False => NPCase Nom ;  --tuleb see raamat lugeda
                  _ => NPAcc             --tuleb sind näha --TODO I: is this correct?
                  }
               }
          } ;
        _        => co.c.npf
        } ;
      nps = np.s ! c ++ co.c.suf ; -- complement's NPFormPlus may include suffix for the cases based on Gen stem, e.g. comitative "ga"
    in
    preOrPost co.isPre co.s nps ++ np.postmod ;

  -- Used for passive; c2 of V2/VPSlash becomes sc of VP
  compl2subjcase : Compl -> NPForm = \compl ->
    case compl.c.npf of {
      NPCase Gen => NPCase Nom ;  -- valisin koera -> koer valitakse
      _          => compl.c.npf   -- rääkisin koerale -> koerale räägitakse
    } ;
-- For $Verb$.

  Verb : Type = {
    s : VForm => Str ;
    p : Str  -- particle verbs
    } ;

  Verb1 : Type = Verb ** {sc : NPForm} ; --subject case, i.e. "ma näen kassi"/"mul on kass"
  Verb2 : Type = Verb1 ** {c2 : Compl} ;
  Verb3 : Type = Verb2 ** {c3 : Compl} ;

  linV2, linV : Verb -> Str = \v -> applyInfFormsV InfMa v.s ++ v.p ;

param
  VIForm =
     VIFin  Tense
   | VIInf  InfStem
   | VIPass Tense
   | VIPresPart
   | VIImper
   ;

oper
  VP : Type = {
    v : Verb ;
    s2  : Bool => Polarity => Agr => Str ; -- raamat/raamatu/raamatut
    adv : Str ;
    p : Str ; --uninflecting component in multi-word verbs
    ext : Str ;
    sc  : NPForm ;
    } ;

  passiveVerb : Verb -> Verb = \verb -> verb ** {
    s = table {
      Presn _ _  => verb.s ! PassPresn True ;
      Impf  _ _  => verb.s ! PassImpf True ;   --# notpresent
      Condit _ _ => verb.s ! ConditPass ;  --# notpresent
      ImperP3    => verb.s ! ImperPass ;
      Imper Sg   => verb.s ! PassPresn False ; -- weird hack, because the Imper Sg field is used for negative form; if VP undergoes PassV*, then its negation should also be in passive.
      PresPart _ => verb.s ! PresPart Pass ;
      PastPart _ => verb.s ! PastPart Pass ;
      x => verb.s ! x }
    } ;

  -- NB. only chooses passive verb forms, to get subject case need compl2subjcase, used in PassV2
  passiveVP : VP -> VP = \vp -> vp ** {v = passiveVerb vp.v} ;

  VPForms : Type = VIForm => Anteriority => Polarity => Agr => {fin, inf : Str} ;

  mkVPForms : Verb -> VPForms = \verb -> \\vi,ant,b,agr0 =>
    let
      agr = verbAgr agr0 ;
      verbs = verb.s ;
      part  : Str = case vi of {
        VIPass _ => verbs ! PastPart Pass ;
        _      => verbs ! PastPart Act
      } ;

      einegole : Str * Str * Str = case <vi,agr.n> of {
        <VIFin Pres>  => <"ei", verbs ! Imper Sg,     "ole"> ;
        <VIFin Fut>   => <"ei", verbs ! Imper Sg,     "ole"> ;
        <VIFin Cond>  => <"ei", verbs ! Condit Sg P3, "oleks"> ;
        <VIFin Past>  => <"ei", part,                 "olnud"> ;
        <VIImper, Sg> => <"ära", verbs ! Imper Sg,   "ole"> ;
        <VIImper, Pl> => <"ärge", verbs ! Imper Pl,  "olge"> ;
        <VIPass Pres> => <"ei", verbs ! PassPresn False,  "ole"> ;
        <VIPass Fut>  => <"ei", verbs ! PassPresn False,  "ole"> ; --# notpresent
        <VIPass Cond> => <"ei", verbs ! ConditPass,  "oleks"> ; --# notpresent
        <VIPass Past> => <"ei", verbs ! PassImpf False,  "olnud"> ; --# notpresent
        <VIPresPart>  => <"ei", verbs ! PresPart Act, "olev"> ; --# notpresent
        <VIInf i>     => <"ei", verbs ! Inf i, verbOlema.s ! Inf i>

      } ;

      ei  : Str = einegole.p1 ;
      neg : Str = einegole.p2 ;
      ole : Str = einegole.p3 ;

      olema : VForm => Str = verbOlema.s ;

      vf : Str -> Str -> {fin, inf : Str} = \x,y -> {fin = x ; inf = y} ;

      mkvf : VForm -> {fin, inf : Str} = \p -> case <ant,b> of {
        <Simul,Pos> => vf (verbs ! p) [] ;
        <Anter,Pos> => vf (olema ! p) part ;
        <Simul,Neg> => vf (ei ++ neg) [] ;
        <Anter,Neg> => vf (ei ++ ole) part
      } ;

      passPol = case b of {Pos => True ; Neg => False} ;

   in case vi of {
        VIFin Past  => mkvf (Impf agr.n agr.p) ; --# notpresent
        VIFin Cond  => mkvf (Condit agr.n agr.p) ; --# notpresent
        VIFin Fut   => mkvf (Presn agr.n agr.p) ; --# notpresent
        VIFin Pres  => mkvf (Presn agr.n agr.p) ;
        VIImper     => mkvf (Imper agr.n) ;
        VIPass Pres => mkvf (PassPresn passPol) ;
        VIPass Past => mkvf (PassImpf passPol) ;  --# notpresent
        VIPass Cond => mkvf (ConditPass) ; --# notpresent
        VIPass Fut  => mkvf (PassPresn passPol) ;  --# notpresent
        VIPresPart  => mkvf (PresPart Act) ;  --# notpresent
        VIInf i    => mkvf (Inf i)
        } ;

  predV : Verb1 -> VP = \verb -> {
    v = verb ; -- ignoring the subject case of Verb, it is stored in VP.sc later
    s2 = \\_,_,_ => [] ;
    adv = [] ;
    ext = [] ; --relative clause
    p = verb.p ; --particle verbs
    sc = verb.sc
    } ;

  insertObj : (Bool => Polarity => Agr => Str) -> VP -> VP = \obj,vp ->
    vp ** { s2 = \\fin,b,a => vp.s2 ! fin ! b ! a ++ obj ! fin ! b ! a } ;

  insertObjPre : (Bool => Polarity => Agr => Str) -> VP -> VP = \obj,vp ->
    vp ** { s2 = \\fin,b,a => obj ! fin ! b ! a ++ vp.s2 ! fin ! b ! a  } ;

  insertAdv : Str -> VP -> VP = \adv,vp ->
    vp ** { adv = vp.adv ++ adv } ;

  insertExtrapos : Str -> VP -> VP = \obj,vp ->
    vp ** { ext = vp.ext ++ obj } ;

-- For $Sentence$.

  Clause : Type = {
    s : Tense => Anteriority => Polarity => SType => Str
    } ;

  ClausePlus : Type = {
    s : Tense => Anteriority => Polarity => {subj,fin,inf,compl,adv,p,ext : Str}
    } ;

  -- The Finnish version of SQuest featured a word order change and
  -- the question particle "ko". The Estonian version just prefixes the
  -- declarative sentence with the yes/no-queryword "kas".
  -- SQuest: "kas" + SDecl
  -- It would be also correct to use the Finnish structure, just without the ko-particle.
  -- Inari: added a third SType, SInv.
  -- Not sure if SInv is needed, but keeping it for possible future use.
  -- There's need for an inverted word order with auxiliary verbs; infVP handles that. ComplVV calls infVP, which inverts the word order for the complement VP, and puts it into the resulting VP's `compl' field.
  -- SInv made by mkClause would be for cases where you just need to construct an inverted word order, and then call it from some other place; application grammar (TODO: api oper for SType) or ExtraEst.
  mkClause : (Polarity -> Str) -> Agr -> VP -> Clause = \sub,agr,vp ->
   { s = \\t,a,b =>
      let
        c = (mkClausePlus sub agr vp).s ! t ! a ! b ;
        --                 saan              sinust     aru    0
        --       ma        olen     täna     sinust     aru    saanud
        declCl = c.subj ++ c.fin ++ c.adv ++ c.compl ++ c.p ++ c.inf ++ c.ext ;
        --                                [sind näha]  0      tahtnud
        --      täna     olen     ma        sinust     aru    saanud
        invCl = c.adv ++ c.fin ++ c.subj ++ c.compl ++ c.p ++ c.inf ++ c.ext
      in
         table {
           SDecl  => declCl ;
           SQuest => "kas" ++ declCl ;
           SInv   => invCl
         }
      } ;

  existClause : (Polarity -> Str) -> Agr -> VP -> Clause = \sub,agr,vp ->
   { s = \\t,a,b =>
      let
        c = (mkClausePlus sub agr vp).s ! t ! a ! b ;
        --       (mis)     on       olnud    olemas (lammas)
        declCl = c.subj ++ c.fin ++ c.inf ++ c.compl ;
      in
         table {
           SQuest => "kas" ++ declCl ;
           _      => declCl
         }
    } ;

  mkClausePlus : (Polarity -> Str) -> Agr -> VP -> ClausePlus =
    \sub,agr,vp -> {
      s = \\t,a,b =>
        let
          agrfin = case vp.sc of {
                    NPCase Nom => <agr,True> ;
                    _ => <agrP3 Sg,False>      -- minule meeldib, minul on
                    } ;
          verb  = mkVPForms vp.v ! VIFin t ! a ! b ! agrfin.p1 ;
        in {subj = sub b ;
            fin  = verb.fin ;
            inf  = verb.inf ;
            compl = vp.s2 ! agrfin.p2 ! b ! agr ;
            p = vp.p ;
            adv  = vp.adv ;
            ext  = vp.ext ;
            }
     } ;


  insertKinClausePlus : Predef.Ints 1 -> ClausePlus -> ClausePlus = \p,cl -> {
    s = \\t,a,b =>
      let
         c = cl.s ! t ! a ! b
      in
      case p of {
         0 => {subj = c.subj ++ gi ; fin = c.fin ; inf = c.inf ;  -- Jussikin nukkuu
               compl = c.compl ; p = c.p ; adv = c.adv ; ext = c.ext ; h = c.h} ;
         1 => {subj = c.subj ; fin = c.fin ++ gi ; inf = c.inf ;  -- Jussi nukkuukin
               compl = c.compl ; p = c.p ; adv = c.adv ; ext = c.ext ; h = c.h}
         }
    } ;

  insertObjClausePlus : Predef.Ints 1 -> Bool -> (Polarity => Str) -> ClausePlus -> ClausePlus =
   \p,ifKin,obj,cl -> {
    s = \\t,a,b =>
      let
         c = cl.s ! t ! a ! b ;
         co = obj ! b ++ if_then_Str ifKin (kin b) [] ;
      in case p of {
         0 => {subj = c.subj ; fin = c.fin ; inf = c.inf ;
               compl = co ; p = c.p ; adv = c.compl ++ c.adv ; ext = c.ext ; h = c.h} ; -- Jussi juo maitoakin
         1 => {subj = c.subj ; fin = c.fin ; inf = c.inf ;
               compl = c.compl ; p = c.p ; adv = co ; ext = c.adv ++ c.ext ; h = c.h}   -- Jussi nukkuu nytkin
         }
     } ;

  kin : Polarity -> Str  =
    \p -> case p of {Pos => "gi" ; Neg => "gi"} ;

  --allomorph "ki", depends only on phonetic rules "üks+ki", "ühe+gi"
  --waiting for post construction in GF :P
  gi : Str = "gi" ;

-- This is used for subjects of passives: therefore isFin in False.

  subjForm : NPhrase -> NPForm -> Polarity -> Str = \np,sc,b ->
    appCompl False b {s = [] ; c = case2npformp sc ; isPre = True} np ;

  infVP : NPForm -> Polarity -> Agr -> VP -> InfForms -> Str = infVPAnt Simul ;

  infVPAnt : Anteriority -> NPForm -> Polarity -> Agr -> VP -> InfForms -> Str =
    \ant,sc,pol,agr,vp,vi ->
        let
          complCase = case sc of {     -- choosing case for the complement. sometimes this function is called so that sc is the VP's subject case, but other times it's some other form.
            NPCase Nom => True ;
            _ => False
            } ;
          verbStem = mkVPForms vp.v ! VIInf vi.stem ! ant ! Pos ! agr ; -- no "ei"
          verb = verbStem ** {fin = glue verbStem.fin vi.suf} ;
          compl = vp.s2 ! complCase ! pol ! agr ;                             -- but compl. case propagated
          adv = vp.adv
        in
        -- inverted word order; e.g.
      --sinust   kunagi aru     saada       tahtnud     rel. clause
        compl ++ adv ++ vp.p ++ verb.inf ++ verb.fin ++ vp.ext ;
        --TODO adv placement?
        --TODO inf ++ fin or fin ++ inf? does it ever become a case here?

-- The definitions below were moved here from $MorphoEst$ so that
-- auxiliary of predication can be defined.

  verbOlema : Verb =
    let olema = mkVerb
      "olema" "olla" "olen" "ollakse"
      "olge" "oli" "olnud" "oldud"
     in {s = table {
      Presn _ P3 => "on" ;
      v => olema.s ! v
      } ;
      p = []
    } ;

  verbMinema : Verb =
    let minema = mkVerb
      "minema" "minna" "läheb" "minnakse"
      "minge" "läks" "läinud" "mindud"
    in {s = table {
      Impf Sg P1 => "läksin" ;
      Impf Sg P2 => "läksid" ;
      Impf Pl P1 => "läksime" ;
      Impf Pl P2 => "läksite" ;
      Impf Pl P3 => "läksid" ;
      Imper Sg => "mine" ;
      v => minema.s ! v
      } ;
      p = []
    } ;


--3 Verbs

  --Auxiliary for internal use
  mkVerb : (x1,_,_,_,_,_,_,x8 : Str) -> Verb =
    \tulema,tulla,tuleb,tullakse,tulge,tuli,tulnud,tuldud ->
    vforms2verb (vForms8 tulema tulla tuleb tullakse tulge tuli tulnud tuldud) ;

    VForms : Type = Predef.Ints 7 => Str ;

    vForms8 : (x1,_,_,_,_,_,_,x8 : Str) -> VForms =
      \tulema,tulla,tuleb,tullakse,tulge,tuli,tulnud,tuldud ->
      table {
        0 => tulema ;
        1 => tulla ;
        2 => tuleb ;
        3 => tullakse ;
        4 => tulge ;
        5 => tuli ;
        6 => tulnud ;
        7 => tuldud
      } ;

    vforms2verb : VForms -> Verb = \vh ->
    let
      tulema = vh ! 0 ;
      tulla = vh ! 1 ;
      tuleb = vh ! 2 ;
      tullakse = vh ! 3 ; --juuakse; loetakse
      tulge = vh ! 4 ;  --necessary for tulla, surra (otherwise *tulege, *surege)
      tuli = vh ! 5 ; --necessary for jooma-juua-jõi
      tulnud = vh ! 6 ;
      tuldud = vh ! 7 ; --necessary for t/d in tuldi; loeti

      tull_ = init tulla ; --juu(a); saad(a); tull(a);
      tulles = tull_ + "es" ; --juues; saades; tulles;

      tule_ = init tuleb ;

      lask_ = Predef.tk 2 tulema ;
      laulev = case (last lask_) of { --sooma~soov ; laulma~laulev
          ("a"|"e"|"i"|"o"|"u"|"õ"|"ä"|"ö"|"ü") => lask_ + "v" ;
          _ => lask_ + "ev" } ; --consonant stem in -ma, add e

      --imperfect stem
      kaisi_ = case (Predef.dp 3 tuli) of {
          "sis"    => lask_ + "i" ; --tõusin, tõusis
          _ + "i"  => tuli ;        --jõin, jõi
          _        => lask_ + "si"  --käisin, käis; muutsin, muutis
         };

      tuld_ = Predef.tk 2 tuldud ; --d/t choice for tuldi etc.
      tulgu = (init tulge) + "u" ;
    in
    {s = table {
      Inf InfD    => tull_ ;
      Inf InfM    => init tulema ;
      Presn Sg P1 => tule_ + "n" ;
      Presn Sg P2 => tule_ + "d" ;
      Presn Sg P3 => tuleb ;
      Presn Pl P1 => tule_ + "me" ;
      Presn Pl P2 => tule_ + "te" ;
      Presn Pl P3 => tule_ + "vad" ;
      Impf Sg P1  => kaisi_ + "n" ;   --# notpresent
      Impf Sg P2  => kaisi_ + "d" ;  --# notpresent
      Impf Sg P3  => tuli ;  --# notpresent
      Impf Pl P1  => kaisi_ + "me" ;  --# notpresent
      Impf Pl P2  => kaisi_ + "te" ;  --# notpresent
      Impf Pl P3  => kaisi_ + "d" ;  --# notpresent
      Condit Sg P1 => tule_ + "ksin" ;  --# notpresent
      Condit Sg P2 => tule_ + "ksid" ;  --# notpresent
      Condit Sg P3 => tule_ + "ks";  --# notpresent
      Condit Pl P1 => tule_ + "ksime" ;  --# notpresent
      Condit Pl P2 => tule_ + "ksite" ;  --# notpresent
      Condit Pl P3 => tule_ + "ksid" ;  --# notpresent
      ConditPass   => tuld_ + "aks" ; --# notpresent
      Imper Sg  => tule_ ; -- tule / ära tule
      Imper Pl  => tulge ; -- tulge / ärge tulge
      ImperP3   => tulgu ; -- tulgu (ta/nad)
      ImperP1Pl => tulge + "m" ; -- tulgem
      ImperPass => tuld_ + "agu" ; --tuldagu
      PassPresn True  => tullakse ;
      PassPresn False => tuld_ + "a" ; --da or ta
      PassImpf  True  => tuld_ + "i" ; --di or ti
      PassImpf  False => tuldud ;
      Quotative Act  => lask_ + "vat" ;
      Quotative Pass => tuld_ + "avat" ; --d or t
      PresPart Act  => laulev ;
      PresPart Pass => tuld_ + "av" ; --d or t
      PastPart Act  => tulnud ;
      PastPart Pass => tuldud
      } ;
    sc = NPCase Nom ;
    p = []
    } ;

  -- For regular verbs, paradigm from 4 base forms
  -- Analoogiaseosed pöördsõna paradigmas
  -- http://www.eki.ee/books/ekk09/index.php?p=3&p1=5&id=227
  regVForms : (x1,_,_,x4 : Str) -> VForms = \vestlema,vestelda,vestleb,vesteldakse ->
    let
      vestle_ = Predef.tk 2 vestlema ;
      vesteld_ = init vestelda ;
      vestel_ = init vesteld_ ;
      lase_ = init vestleb ;
      jaet_ = Predef.tk 4 vesteldakse ;
      g = case (last vesteld_) of { --doesn't work for anda~andke
        "t" => "k" ;
        _   => "g"
      } ;
      toit_ = case (last vestle_) of {
        ("t"|"d") => vesteld_ ; --toit(ma)   -> toitke;
         _        => vestel_    --vestle(ma) -> vestelge
      } ;
      laski_ = case (last vestle_) of {
        ("a"|"e"|"i"|"o"|"u"|"õ"|"ä"|"ö"|"ü")
            => vestle_ ;      --vestle(ma) -> vestles
         _  => vestle_ + "i"  --lask(ma)   -> laskis
      } ;
    in
      vForms8
        vestlema
        vestelda
        vestleb
        vesteldakse
        (toit_ + g + "e") --da: käskiva kõneviisi ainsuse 3. pööre ja mitmus;
        (laski_ + "s") --ma: kindla kõneviisi lihtmineviku pöörded;
        (toit_ + "nud") --da: isikulise tegumoe mineviku kesksõna
        (jaet_ + "ud"); --takse: ülejäänud umbisikulise tgm vormid


  regVerb : (_,_,_,_ : Str) -> Verb = \kinkima,kinkida,kingib,kingitakse ->
    vforms2verb (regVForms kinkima kinkida kingib kingitakse) ;


  noun2adj : Noun -> Adj = noun2adjComp True ;

  -- TODO: AAdv is current just Sg Ablat, which seems OK in most cases, although
  -- ilus -> ilusti | ilusalt?
  -- hea -> hästi
  -- parem -> paremini
  -- parim -> kõige paremini | parimalt?
  noun2adjComp : Bool -> Noun -> Adj = \isPos,tuore ->
    let
      tuoreesti  = Predef.tk 1 (tuore.s ! NCase Sg Gen) + "sti" ;
      tuoreemmin = Predef.tk 2 (tuore.s ! NCase Sg Gen) + "in"
    in {s = table {
         AN f => tuore.s ! f ;
         -- AAdv => if_then_Str isPos tuoreesti tuoreemmin
         AAdv => tuore.s ! NCase Sg Ablat
         } ;
       } ;

  Noun : Type = {s : NForm => Str} ;

  CNoun : Type = Noun ** {postmod : Str} ;

  emptyCN : CNoun = {
    s = \\nf => [] ;
    postmod = []
  } ;

  linCN : NForm -> CNoun -> Str = \nf,cn -> cn.s ! nf ++ cn.postmod ;

-- To form an adjective, it is usually enough to give a noun declension: the
-- adverbial form is regular.

  Adj : Type = {s : AForm => Str} ;

-- Helper functions to form Comps.
  compAP = icompAP [] ;

  icompAP : Str -> {s : Bool => NForm => Str} -> {s : Agr => Str} = \kui,ap ->
    { s = \\agr =>
       let n = complNumAgr agr ;
        in kui ++ ap.s ! False ! NCase n Nom } ;

  compCN : Noun -> {s : Agr => Str} = \cn ->
    { s = \\agr =>
       let n = complNumAgr agr ;
        in cn.s ! NCase n Nom } ;


-- Reflexive pronoun.
--- Possessive could be shared with the more general $NounFin.DetCN$.

  reflPron : Agr -> NPhrase = \agr ->
    let
      ise = nForms2N (nForms6 "ise" "enda" "ennast" "endasse" "endi" "endid") ;
      n = case agr of {
        AgPol => Sg ;
        Ag n _ => n } ;
     in emptyNP ** {
      s = table {
        NPAcc => "ennast" ;
        NPCase c => fixPlNom "endid" ise.s ! NCase n c
        } ;
      a = agr ;
      isPron = False -- no special acc form
      } ;

  -- Using nForms6 as a shortcut works pretty nicely, but plural nominative is often wrong.
  -- This is used at least 3 times :-D
  fixPlNom : Str -> (NForm => Str) -> (NForm => Str) = \mis,n ->
    table { NCase Pl Nom => mis ;
            x            => n ! x } ;

  NForms : Type = Predef.Ints 5 => Str ;

  nForms6 : (x1,_,_,_,_,x6 : Str) -> NForms =
      \jogi,joe,joge,joesse, -- sg nom, gen, part, ill
       jogede,jogesid -> table { -- pl gen, part,
      0 => jogi ;
      1 => joe ;
      2 => joge ;
      3 => joesse ;
      4 => jogede ;
      5 => jogesid
      } ;

  n2nforms : Noun -> NForms = \ukko -> table {
    0 => ukko.s ! NCase Sg Nom ;
    1 => ukko.s ! NCase Sg Gen ;
    2 => ukko.s ! NCase Sg Part ;
    3 => ukko.s ! NCase Sg Illat ;
    4 => ukko.s ! NCase Pl Gen ;
    5 => ukko.s ! NCase Pl Part
  } ;

    -- Converts 6 given strings (Nom, Gen, Part, Illat, Gen, Part) into Noun
    -- http://www.eki.ee/books/ekk09/index.php?p=3&p1=5&id=226
  nForms2N : NForms -> Noun = \f ->
      let
        jogi = f ! 0 ;
        joe = f ! 1 ;
        joge = f ! 2 ;
        joesse = f ! 3 ;
        jogede = f ! 4 ;
        jogesid = f ! 5 ;
      in
    {s = table {
      NCase Sg Nom    => jogi ;
      NCase Sg Gen    => joe ;
      NCase Sg Part   => joge ;
      NCase Sg Transl => joe + "ks" ;
      NCase Sg Iness  => joe + "s" ;
      NCase Sg Elat   => joe + "st" ;
      NCase Sg Illat  => joesse ;
      NCase Sg Adess  => joe + "l" ;
      NCase Sg Ablat  => joe + "lt" ;
      NCase Sg Allat  => joe + "le" ;

      NCase Pl Nom    => joe + "d" ;
      NCase Pl Gen    => jogede ;
      NCase Pl Part   => jogesid ;
      NCase Pl Transl => jogede + "ks" ;
      NCase Pl Iness  => jogede + "s" ;
      NCase Pl Elat   => jogede + "st" ;
      NCase Pl Illat  => jogede + "sse" ;
      NCase Pl Adess  => jogede + "l" ;
      NCase Pl Ablat  => jogede + "lt" ;
      NCase Pl Allat  => jogede + "le"

      }
    } ;

oper
  -- Technically, we could also add a postmod field for RP,
  -- because multiple applications of FunRP add multiple complements.
  -- But I will only add it if I see a real-world sentence that uses multiple applications of FunRP.
  RelPron : Type = {s : Number => NPForm => Str ; a : RAgr} ;
  rp2np : Number -> RelPron -> NPhrase = \n,rp -> emptyNP ** {
    s = rp.s ! n ;
    a = agrP3 Sg ;  -- does not matter (--- at least in Slash)
    isPron = False  -- has no special accusative
    } ;

  etta_Conj : Str = "et" ;

  Determiner : Type = {
    s : Case => Str ;       -- minun kolme
    sp : Case => Str ;       -- se   (substantival form)
    n : Number ;             -- Pl   (agreement feature for verb)
    isNum : Bool ;           -- True (a numeral is present)
    isDef : Bool             -- True (verb agrees in Pl, Nom is not Part) --I: actually, can we get rid of this?
    } ;

  IDeterminer : Type = {s : Case => Str ; n : Number ; isNum : Bool} ;

    heavyDet : PDet -> Determiner = \d -> d ** {sp = d.s} ;
    PDet : Type = {
      s : Case => Str ;
      n : Number ;
      isNum : Bool ;
      isDef : Bool
      } ;

    heavyQuant : PQuant -> PQuant ** {sp : Number => Case => Str} = \d ->
      d ** {sp = d.s} ;
    PQuant : Type =
      {s : Number => Case => Str ; isDef : Bool} ;

}
