concrete CatGla of Cat = CommonX ** open ResGla, Coordination, Prelude in {

  flags optimize=all_subs ;

  lincat

--2 Sentences and clauses
-- Constructed in SentenceGla, and also in IdiomGla
    S  = SS ;
    QS = SS ;
    RS = SS ;
    -- relative sentence. Tense and polarity fixed,
    -- but agreement may depend on the CN/NP it modifies.

    Cl = ResGla.LinCl ;
    ClSlash = SS ;
    SSlash  = SS ; -- sentence missing NP; e.g. "she has looked at"
    Imp     = SS ; -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionGla.
    QCl = SS ;
    IComp = SS ;   -- interrogative complement of copula  e.g. "where"
    IDet = SS ;    -- interrogative determiner            e.g. "how many"
    IQuant = SS ;  -- interrogative quantifier            e.g. "which"
    IP = SS ;      -- interrogative pronoun               e.g. "who"

--2 Subord clauses and pronouns

    RCl = SS ;
    RP = SS ;

--2 Verb phrases

-- Constructed in VerbGla.
    VP = ResGla.LinVP ;
    VPSlash = SS ;
    Comp = SS ;

--2 Adjectival phrases

-- Constructed in AdjectiveGla.
    AP = SS ;

--2 Nouns and noun phrases

-- Constructed in NounGla.
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in StructuralGla.

    CN = ResGla.LinCN ;
    NP = ResGla.LinNP ;
    Pron = SS ; -- NB. Pronouns need enough info to become NP or Quant.
    Det = ResGla.LinDet ; -- s : Str , n : Number
    Predet = SS ;
    Quant = ResGla.LinQuant ; -- s : Number => Str
    Num = ResGla.LinNum ;
    Card = ResGla.LinNum ;
    ACard = SS ;
    Ord = SS ;
    DAP = SS ;


--2 Numerals

-- Constructed in NumeralGla.

    Numeral = ResGla.LinNumeral ;
    Digits = ResGla.LinNumeral ;

--2 Structural words

-- Constructed in StructuralGla.
    Conj = Coordination.ConjunctionDistr ** {
        n : Number -- The number of the NP that results from
                   -- coordinating a list of NPs with that Conj.
        } ;        -- "[Ann and Bob] are children" → and_Conj.n = Pl
    Subj = SS ;
    Prep = ResGla.LinPrep ;



--2 Words of open classes

-- These are constructed in LexiconGla and in
-- additional lexicon modules.

    -- TODO: eventually different lincats
    VS,    -- sentence-complement verb            e.g. "claim"
    VQ,    -- question-complement verb            e.g. "wonder"
    VA,    -- adjective-complement verb           e.g. "look"
    V = ResGla.LinV ;

    VV     -- verb-phrase-complement verb         e.g. "want"
      = SS ;

    V2A,   -- verb with NP and AP complement      e.g. "paint"
    V2V,   -- verb with NP and V complement       e.g. "cause"
    V2S,   -- verb with NP and S complement       e.g. "tell"
    V2Q,   -- verb with NP and Q complement       e.g. "ask"
    V2 = SS ;
    V3 = SS ;

    A = SS ;
    A2  = SS ;

    N = ResGla.LinN ;
    N2 = ResGla.LinN ;
    N3 = ResGla.LinN ;
    PN = SS ;

    -- From the Names module, not in the official API as of 2023-08
    GN = SS ; -- Given name,    e.g. "George"
    SN = SS ; -- Second name,   e.g. "Washington"
    LN = SS ; -- Location name, e.g. "Sweden"

  linref
    Cl = linCl ;

}
