--# -path=.:../prelude:../abstract:../common

concrete CatCgg of Cat = CommonX -[Adv,IAdv, AdA]**
  open (Res=ResCgg), Prelude, (Px=ParamX), Predef in {

lincat
  
  
  Imp = {s : Res. ImpPol=> Str} ;
  QS = {s : Str} ;

  -- Note: SS is a shorthand for {s:Str}, defined in Prelude.gf
  -- You must change some of the lincats (e.g., for NP, Det and Pron) so that everything works

  S = SS ;                        -- declarative sentence                e.g. "she lived here"
  Cl  = Res.Clause ;               -- declarative clause, with all tenses e.g. "she looks at this"
  --Questions
  QCl = Res.Clause ** {posibleSubAgr: Res.Agreement =>Str} ; 
  IComp = 
    {
      s : Str; 
      --other:Str; Has been deleted but note that there are several words for asking questions
      n : Res.INumber; 
      requiresSubjPrefix: Bool;
      requiresIPPrefix: Bool;
      usesAux : Bool;
      endOfSentence : Bool
    } ; 
  IP   = {s :Str ; n : Res.INumber; isVerbSuffix: Bool; requiresIPPrefix: Bool; aux:Str; endOfSentence:Bool}; -- other holds the Idet without a prefix
  IAdv = {s : Str ; requiresSubjPrefix: Bool; endOfSentence:Bool};
  IDet = {s : Str ; n : Res.Number; requiresSubjPrefix: Bool};
  IQuant   = {s : Res.Number =>Str ; requiresSubjPrefix: Bool};
  RS = {s :Res.RForm => Str} ; -- relative                            e.g. "in which she lived"
  V,VS, VQ, VA = Res.Verb ;   --change to {verb : Str ; comp = []}               -- one-place verb                      e.g. "sleep"  
  V2,V2Q, V2S = Res.Verb2;
  V2A,V3 = Res.Verb3;    -- three-place verb                    e.g. "show"
  
  VP = Res.VerbPhrase ;           -- verb phrase                         e.g. "is very warm"
  
  N = Res.Noun ;                  -- common noun                         e.g. "house"
  CN = Res.Noun ;                 -- common noun (without determiner)    e.g. "red house"
  NP = Res.NounPhrase;            -- noun phrase (subject or object)     e.g. "the red house"
  Pron = Res.Pronoun ;            -- personal pronoun                    e.g. "she"
  Det = Res.Determiner ;          -- determiner phrase                   e.g. "those seven"
  Quant = {s : Res.Pronoun; s2 :Res.Agreement => Str; doesAgree : Bool; isPron: Bool} ;                    -- quantifier ('nucleus' of Det)       e.g. "this/these"
  Num = Res.Numer ;               -- number determining element          e.g. "seven"
  AP = {s :Res.Agreement=> Str ; position : Res.Position; isProper : Bool; isPrep: Bool};--Res.AdjectivalPhrase;
  A  = Res.Adjective;
  Comp = Res.Comp;    -- complement of copula, such as AP  e.g. "very warm"

  Adv = Res.Adverb; --Verb Phrase modifying adverb
  VPSlash = Res.VPSlash;
  PN = Res.ProperNoun; -- ProperNoun : Type = {s: Str ; a:Agreement ; isPlace : Bool};
  Conj = Res.Conjunction; -- Conjunction: Type = {s : AgrConj =>Str ;s2 : Str ; n : Number} ; -- conjunction e.g. "and"
  -- see Structural for explanation of this structure
  Predet = {s : Str ; s2 : Str; isMWE : Bool; isInflected : Bool}; -- predeterminer (prefixed Quant)      e.g. "all"
  RP = {s : Res.RCase => Res.Agreement => Str ; rObjVariant2: Res.Agreement => Str} ;
  RCl ={ 
      s : Str ; --subject
      --subAgr:Res.Agreement;
      rp: Res.RCase => Res.Agreement => Str; -- could delete this
      --rObjVariant2: Res.Agreement => Str;
      agr : Res.AgrExist;
      pres :Str;
      perf :Str; 
      root : Str;
      --morphs : Res.VFormMini => Res.VerbMorphPos =>Str;
      isPresBlank :Bool;
      isPerfBlank : Bool;
      compl : Str; -- after verb: complement, adverbs
      isCompApStem : Bool;
      whichRel: Res.RForm
      } ;
  --VPSlash ={s:Str; morphs: VMorphs};  --VPSlash ; -- verb phrase missing complement    e.g. "give to John"
  --ClSlash;-- clause missing NP (S/NP in GPSG)    e.g. "she looks at"
  ClSlash = {   
        s : Str ; --subject
        subjAgr : Res.Agreement;
        root : Str;
        pres: Str;
        perf:Str;
        --morphs : Res.VFormMini => Res.VerbMorphPos =>Str; --; compl : Str -- after verb: complement, adverbs                              
        isPresBlank : Bool;
        isPerfBlank : Bool;
        ap:Str;
        isRegular:Bool;
        adv:Str;
        adV:Str;
        complType: Res.ComplType;
        } ;
  Numeral = {s : Res.CardOrd=>Res.Agreement=> Str ; g : Res.Gender;  n: Res.Number} ;
  Digits  = {s : Res.CardOrd => Res.Agreement=>Str ; n : Res.Number ; tail : Px.DTail} ;
  Ord  = {s :Res.Agreement=>Str; position:Res.Position} ;
  Card = {s :Res.Agreement=>Str; n : Res.Number} ;
  
  DAP = Res.Determiner ;
  N2  = Res.Noun ** {c2 : Res.Agreement =>Str};    -- relational noun                     e.g. "son"   
  Prep = Res.Preposition;  -- preposition, or just case           e.g. "in"
  N3   = N2 ** {c3 : Res.Agreement =>Str};
  VV = Res.Verb ** {inf:Str; whenUsed: Res.VVMood}; --inf is the other verb
  AdA = {s:Str; position:Res.Position};
linref
  
  Cl =\cl -> cl.s ++ Res.mkSubjClitic cl.subjAgr ++  cl.root ++ BIND ++ cl.pres ++ cl.compl;
  QCl =\qcl -> qcl.s ++ qcl.posibleSubAgr ! (Res.mkAgreement Res.MU_BA Res.P3 Res.Sg) ++ qcl.root ++ BIND ++ qcl.pres;
  VP =\vp -> vp.adv ++ vp.s ++ BIND ++ vp.pres ++ vp.comp ++vp.comp2 ++ vp.ap;
  VPSlash =\vpslash -> vpslash.s ++ BIND ++ vpslash.pres;


--1 Cat: the Category System

-- The category system is central to the library in the sense
-- that the other modules ($Adjective$, $Adverb$, $Noun$, $Verb$ etc)
-- communicate through it. This means that a e.g. a function using
-- $NP$s in $Verb$ need not know how $NP$s are constructed in $Noun$:
-- it is enough that both $Verb$ and $Noun$ use the same type $NP$,
-- which is given here in $Cat$.
-- 
-- Some categories are inherited from [``Common`` Common.html].
-- The reason they are defined there is that they have the same
-- implementation in all languages in the resource (typically,
-- just a string). These categories are
-- $AdA, AdN, AdV, Adv, Ant, CAdv, IAdv, PConj, Phr$, 
-- $Pol, SC, Tense, Text, Utt, Voc, Interj$.
--
-- Moreover, the list categories $ListAdv, ListAP, ListNP, ListS$
-- are defined on $Conjunction$ and only used locally there.

{-
abstract Cat = Common ** {

  cat

--2 Sentences and clauses

-- Constructed in [Sentence Sentence.html], and also in
-- [Idiom Idiom.html].

    S ;     -- declarative sentence                e.g. "she lived here"
    QS ;    -- question                            e.g. "where did she live"
    RS ;    -- relative                            e.g. "in which she lived"
    Cl ;    -- declarative clause, with all tenses e.g. "she looks at this"
    ClSlash;-- clause missing NP (S/NP in GPSG)    e.g. "she looks at"
    SSlash ;-- sentence missing NP                 e.g. "she has looked at"
    Imp ;   -- imperative                          e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in [Question Question.html].

    QCl ;   -- question clause, with all tenses    e.g. "why does she walk"
    IP ;    -- interrogative pronoun               e.g. "who"
    IComp ; -- interrogative complement of copula  e.g. "where"
    IDet ;  -- interrogative determiner            e.g. "how many"
    IQuant; -- interrogative quantifier            e.g. "which"

--2 Relative clauses and pronouns

-- Constructed in [Relative Relative.html].

    RCl ;   -- relative clause, with all tenses    e.g. "in which she lives"
    RP ;    -- relative pronoun                    e.g. "in which"

--2 Verb phrases

-- Constructed in [Verb Verb.html].

    VP ;      -- verb phrase                       e.g. "is very warm"
    Comp ;    -- complement of copula, such as AP  e.g. "very warm"
    VPSlash ; -- verb phrase missing complement    e.g. "give to John"

--2 Adjectival phrases

-- Constructed in [Adjective Adjective.html].

    AP ;    -- adjectival phrase                   e.g. "very warm"

--2 Nouns and noun phrases

-- Constructed in [Noun Noun.html]. 
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in [Structural Structural.html].
-- The determiner structure is
-- ``` Predet (QuantSg | QuantPl Num) Ord
-- as defined in [Noun Noun.html].

    CN ;     -- common noun (without determiner)    e.g. "red house"
    NP ;     -- noun phrase (subject or object)     e.g. "the red house"
    Pron ;   -- personal pronoun                    e.g. "she"
    Det ;    -- determiner phrase                   e.g. "those seven"
    Predet ; -- predeterminer (prefixed Quant)      e.g. "all"
    Quant ;  -- quantifier ('nucleus' of Det)       e.g. "this/these"
    Num ;    -- number determining element          e.g. "seven"
    Card ;   -- cardinal number                     e.g. "seven"
    ACard ;  -- adjective like cardinal             e.g. "few", "many"
    Ord ;    -- ordinal number (used in Det)        e.g. "seventh"
    DAP ;    -- determiner with adjective           e.g. "three small"

--2 Numerals

-- Constructed in [Numeral Numeral.html].

    Numeral ; -- cardinal or ordinal in words       e.g. "five/fifth"
    Digits ;  -- cardinal or ordinal in digits      e.g. "1,000/1,000th"

--2 Structural words

-- Constructed in [Structural Structural.html].

    Conj ;  -- conjunction                         e.g. "and"
---b    DConj ; -- distributed conjunction             e.g. "both - and"
    Subj ;  -- subjunction                         e.g. "if"
    Prep ;  -- preposition, or just case           e.g. "in"

--2 Words of open classes

-- These are constructed in [Lexicon Lexicon.html] and in 
-- additional lexicon modules.

    V ;     -- one-place verb                      e.g. "sleep" 
    V2 ;    -- two-place verb                      e.g. "love"
    V3 ;    -- three-place verb                    e.g. "show"
    VV ;    -- verb-phrase-complement verb         e.g. "want"
    VS ;    -- sentence-complement verb            e.g. "claim"
    VQ ;    -- question-complement verb            e.g. "wonder"
    VA ;    -- adjective-complement verb           e.g. "look"
    V2V ;   -- verb with NP and V complement       e.g. "cause"
    V2S ;   -- verb with NP and S complement       e.g. "tell"
    V2Q ;   -- verb with NP and Q complement       e.g. "ask"
    V2A ;   -- verb with NP and AP complement      e.g. "paint"

    A ;     -- one-place adjective                 e.g. "warm"
    A2 ;    -- two-place adjective                 e.g. "divisible"

    N ;     -- common noun                         e.g. "house"
    N2 ;    -- relational noun                     e.g. "son"
    N3 ;    -- three-place relational noun         e.g. "connection"
    PN ;    -- proper name                         e.g. "Paris"

-- DEPRECATED: QuantSg, QuantPl
---    QuantSg ;-- quantifier ('nucleus' of sing. Det) e.g. "every"
---    QuantPl ;-- quantifier ('nucleus' of plur. Det) e.g. "many"

-}

}
