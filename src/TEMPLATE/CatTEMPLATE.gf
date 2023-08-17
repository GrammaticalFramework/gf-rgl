concrete CatTEMPLATE of Cat = CommonX ** open ResTEMPLATE, Coordination, Prelude in {

  flags optimize=all_subs ;

  lincat

--2 Sentences and clauses
-- Constructed in SentenceTEMPLATE, and also in IdiomTEMPLATE

    S  = SS ;
    QS = SS ;
    RS = SS ;
    -- relative sentence. Tense and polarity fixed,
    -- but agreement may depend on the CN/NP it modifies.

    Cl = ResTEMPLATE.LinCl ;
    ClSlash = SS ;
    SSlash  = SS ; -- sentence missing NP; e.g. "she has looked at"
    Imp     = SS ; -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionTEMPLATE.

    QCl = SS ;
    IComp = SS ;                -- interrogative complement of copula  e.g. "where"
    IDet = SS ;  -- interrogative determiner            e.g. "how many"
    IQuant = SS ;     -- interrogative quantifier            e.g. "which"
    IP = SS ;    -- interrogative pronoun               e.g. "who"

--2 Subord clauses and pronouns

    RCl = SS ;
    RP = SS ;

--2 Verb phrases

-- Constructed in VerbTEMPLATE.

    VP = ResTEMPLATE.LinVP ;
    VPSlash = SS ;
    Comp = SS ;


--2 Adjectival phrases

-- Constructed in AdjectiveTEMPLATE.

    AP = SS ;


--2 Nouns and noun phrases

-- Constructed in NounTEMPLATE.
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in StructuralTEMPLATE.

    CN = ResTEMPLATE.LinCN ;
    NP = ResTEMPLATE.LinNP ;
    Pron = SS ; -- NB. Pronouns need enough info to become NP or Quant.
    Det = ResTEMPLATE.LinDet ; -- s : Str , n : Number
    Predet = SS ;
    Quant = ResTEMPLATE.LinQuant ; -- s : Number => Str
    Num = ResTEMPLATE.LinDet ;
    Ord = SS ;
    DAP = SS ;


--2 Numerals

-- Constructed in NumeralTEMPLATE.

    Card = ResTEMPLATE.LinNumeral ;
    Numeral = ResTEMPLATE.LinNumeral ;
    Digits = ResTEMPLATE.LinNumeral ;



--2 Structural words

-- Constructed in StructuralTEMPLATE.
    Conj = Coordination.ConjunctionDistr ;
    Subj = SS ;
    Prep = SS ;



--2 Words of open classes

-- These are constructed in LexiconTEMPLATE and in
-- additional lexicon modules.

    -- TODO: eventually different lincats
    VS,    -- sentence-complement verb            e.g. "claim"
    VQ,    -- question-complement verb            e.g. "wonder"
    VA,    -- adjective-complement verb           e.g. "look"
    V = ResTEMPLATE.LinV ;

    VV     -- verb-phrase-complement verb         e.g. "want"
      = SS ;

    V2A,   -- verb with NP and AP complement      e.g. "paint"
    V2V,   -- verb with NP and V complement       e.g. "cause"
    V2S,   -- Reverb with NP and S complement       e.g. "tell"
    V2Q,   -- verb with NP and Q complement       e.g. "ask"
    V2 = SS ;
    V3 = SS ;

    A = SS ;
    A2  = SS ;

    N = ResTEMPLATE.LinN ;
    N2 = ResTEMPLATE.LinN ;
    N3 = ResTEMPLATE.LinN ;
    PN = SS ;

}
