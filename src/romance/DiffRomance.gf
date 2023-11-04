--1 Differences between Romance languages

interface DiffRomance = open CommonRomance, Prelude in {
  flags coding=utf8 ;

-- The first eight constants show the essential differences
-- between French, Italian, and Romance syntaxes (as regards the
-- resource API). The other constants are either derivatively
-- dependent, or have as values strings, which are language-dependent
-- anyway.


--2 Constants whose definitions fundamentally depend on language

-- Prepositions that fuse with the article
-- (Fre, Spa "de", "a"; Ita also "con", "da", "in", "su).

  param Prepos ;

-- Which types of verbs exist, in terms of auxiliaries.
-- (Fre, Ita "avoir", "être", and refl; Spa only "haber" and refl).

  param VType ;

-- Derivatively, if/when the participle agrees to the subject.
-- (Fre "elle est partie", Ita "lei è partita", Spa not)

  oper partAgr   : VType -> Bool ;

-- Whether participle agrees to foregoing clitic.
-- (Fre "je l'ai vue", Spa "yo la he visto")

  oper vpAgrClit : Agr -> VPAgr ;

-- Whether a preposition is repeated in conjunction
-- (Fre "la somme de 3 et de 4", Ita "la somma di 3 e 4").

  oper conjunctCase : Case -> Case ;

-- How infinitives and clitics are placed relative to each other
-- (Fre "la voir", Ita "vederla"). The $Bool$ is used for indicating
-- if there are any clitics.

  oper clitInf : Bool -> Str -> Str -> Str ;

-- To render pronominal arguments as clitics and/or ordinary complements.
-- Returns $True$ if there are any clitics.

  oper pronArg : Number -> Person -> CAgr -> CAgr -> Str * Str * Bool ;

-- To render imperatives (with their clitics etc).

  oper mkImperative : Bool -> Person -> VP -> RPolarity => Gender => Number => Str ;

-- To render the copula (ser/estar in Spa,Cat,Por)

  oper CopulaType : PType ;
  oper selectCopula : CopulaType -> Verb ;
  oper serCopula : CopulaType ;
  oper estarCopula : CopulaType ;

-- Adjective and AForm, two things:
-- 1) Whether AForm has different attributive forms:
--    e.g. un bon amic (Cat), una gran parte (Spa) vs. predicative bo/bona, grande
  oper AForm : PType ;
  oper Adj : Type = {s : AForm => Str} ;
  oper mkOrd : Adj -> {s,s2 : AAgr => Str} = \x -> {s = \\ag => x.s ! aagr2aform ag; s2 = \\_ => []} ;

  oper aform2aagr : AForm -> AAgr ;
  oper aform2gender : AForm -> Gender = \af -> (aform2aagr af).g ;
  oper aform2number : AForm -> Number = \af -> (aform2aagr af).n ;

  oper aagr2aform : AAgr -> AForm = \a -> genNum2Aform a.g a.n ;
  oper genNum2Aform : Gender -> Number -> AForm ;
  oper genNumPos2Aform : Gender -> Number -> Bool -> AForm ; -- depends on language, whether attributive is different from predicative

-- 2) Whether comparatives and superlatives inflect in only number, or also in gender:
--    e.g. el/la mejor, but le meilleur, la meilleure
  oper ComparAgr : PType = Number ; -- except Fre, where it's Number and Gender
  oper af2compar : AForm -> ComparAgr = aform2number ; -- except Fre
  oper aagr2compar : AAgr -> ComparAgr = \a -> a.n ; -- except Fre


-- To decide if adverbial questions are inverted

  oper iAdvQuestionInv : Direct = DInv ; -- except Fre

  oper iCompQuestionInv : Direct = DInv ; -- for Cat,Por,Spa where otherInv will be DDir

  oper otherInv : Direct = DInv ; -- except Cat, Por, Spa

--2 Constants that must derivatively depend on language

  param NPForm = Ton Case | Aton Case | Poss {g : Gender ; n : Number} ; --- AAgr

  oper dative   : Case ;
  oper genitive : Case ;

  vRefl   : VType -> VType ;
  isVRefl : VType -> Bool ;


--2 Strings

  prepCase  : Case -> Str ;

  partitive : Gender -> Case -> Str ;

  artDef    : Bool -> Gender -> Number -> Case -> Str ;
  artIndef  : Bool -> Gender -> Number -> Case -> Str ; -- True = used as NP, False = used as Det

-- This is the definite article in Italian, $prepCase c$ in French and Spanish.

  possCase  : Gender -> Number -> Case -> Str ;

  auxVerb   : VType -> (VF => Str) ;
  negation  : RPolarity => (Str * Str) ;
  copula    : Verb ;

  conjThan  : Str ;
  conjThat  : Str ;

  subjIf    : Str ;

  piuComp   : Str ; -- to form comparative and superlative: plus cher, más grande, …

  relPron   : Bool => AAgr => Case => Str ;
  pronSuch  : AAgr => Str ;

  partQIndir : Str ; -- ce, ciò

  reflPron : Number -> Person -> Case -> Str ;
--  argPron  : Gender -> Number -> Person -> Case -> Str ;

  auxPassive : Verb ;


--2 Constants needed in type signatures above

param
  Case = Nom | Acc | CPrep Prepos ;

  NRelType = NRelPrep Prepos | NRelNoPrep ; -- How do build a compound noun

oper
  Verb = {s : VF => Str ; vtyp : VType ; p : Str} ;

  VPAgrType : Type              = Str * Bool ;                                                  ---- originally VPAgr, expensive
  getVPAgr  : Verb -> VPAgrType = \v -> <verbDefaultPart v, partAgr v.vtyp> ;          -- str may be used
  vpAgrSubj : Verb -> VPAgrType = \v -> <verbDefaultPart v, True> ;                  -- str not used but subject instead    ---- VPAgrSubj
  vpAgrClits : Verb -> AAgr -> VPAgrType = \v,a -> <v.s ! (VPart a.g a.n), False> ;    -- str used from clitic             ---- vpAgrClit
  verbDefaultPart : Verb -> Str = \v -> v.s ! (VPart Masc Sg) ;


  VP : Type = {
    s      : Verb ;
    agr    : VPAgrType ;                -- dit/dite dep. on verb, subj, and clitic
    neg    : RPolarity => (Str * Str) ; -- ne-pas
    clit1  : Str ;                      -- le/se
    clit2  : Str ;                      -- lui
    clit3  : Clit3 ;                    -- y en
    isNeg  : Bool ;                     -- includes a negative element, such as "rien"
    comp   : Agr => Str ;               -- content(e) ; à ma mère ; hier
    ext    : RPolarity => Str ;         -- que je dors / que je dorme
    } ;

  Clit3 : Type = {s : Str ; imp : Str ; hasClit : Bool} ;   --- imp encodes special imperative clitic, hasClit whether there is a clitic

  addClit3 : Bool -> Str -> Str -> Clit3 -> Clit3 = \hasClit,s,imp,clit -> {
    s = clit.s ++ s ; imp = clit.imp ++ imp ; hasClit = hasClit  ---- in Fre, imp is "moi" for "me"
    } ;

  imperClit : Agr -> Str -> Str -> Str = \a,c1,c2 -> c1 ++ c2 ;

-- The pronoun to be repeated in $VPS$ coordination, and also in Fre inverted questions.
-- Empty in other languages than Fre (as a kind of prodrop).

  subjPron : Agr -> Str ;

-- Whether subject negative in direct position causes negation to the sentence: "personne ne dort" vs. "nessuno dorme".

  polNegDirSubj : RPolarity ;

--2 Workarounds, to be eliminated

-- This should be provided by $pronArg$ above, but causes trouble in compilation.
-- (AR 16/8/2008)

  oper infForm : Number -> Person -> CAgr -> CAgr -> Bool ;

-- AR 21/2/2013
-- inverted clause order, only deviant in Fre where also the intervening -t- has to be taken to account

  invertedClause :
    VType -> (RTense * Anteriority * Number * Person) -> Bool -> (Str * Str) -> Str -> (clit,fin,inf,compl,subj,ext : Str) -> Str =
    \_,_,_,neg,_,clit,fin,inf,compl,subj,ext -> neg.p1 ++ clit ++ fin ++ neg.p2 ++ inf ++ compl ++ subj ++ ext ;

  verbHyphen : Verb -> Str = \v -> [] ; -- in Fre, - or -t-

  contractInf : Bool -> Bool -> Bool = \_,_ -> False ; -- only True in Ita, by orB

  chooseTA : RTense -> Anteriority
    -> (VF => Str) -> (VF => Str)
    -> Number -> Person -> Mood -> Str -> Str * Str ;
  chooseTA t a verb vaux n p m part = case <t,a> of {
    <RPast,Simul>  => <verb ! VFin (VImperf m) n p, []> ; --# notpresent
    <RPast,Anter>  => <vaux ! VFin (VImperf m) n p, part> ; --# notpresent
    <RFut,Simul>   => <verb ! VFin VFut n p, []> ; --# notpresent
    <RFut,Anter>   => <vaux ! VFin VFut n p, part> ; --# notpresent
    <RCond,Simul>  => <verb ! VFin VCondit n p, []> ; --# notpresent
    <RCond,Anter>  => <vaux ! VFin VCondit n p, part> ; --# notpresent
    <RPasse,Simul> => <verb ! VFin VPasse n p, []> ; --# notpresent
    <RPasse,Anter> => <vaux ! VFin VPasse n p, part> ; --# notpresent
    <RPres,Anter>  => <vaux ! VFin (VPres m) n p, part> ; --# notpresent
    <RPres,Simul>  => <verb ! VFin (VPres m) n p, []>
    } ;

  oper
    essere_V : Verb ;
    stare_V : Verb ;
    stare_V = essere_V ;

  param HasArt ;
  
  oper
    superlCanBePost : Bool ;

} ;
