concrete CatKor of Cat = CommonX ** open ResKor, Prelude in {

  flags optimize=all_subs ;

  lincat

--2 Sentences and clauses
-- Constructed in SentenceKor, and also in IdiomKor

    S  = ResKor.Sentence ;
    QS = {s : Style => Str}; -- Questions not implemented yet
    RS = ResKor.Sentence ;
    -- relative sentence. Tense and polarity fixed,
    -- but agreement may depend on the CN/NP it modifies.

    Cl = ResKor.ClSlash ;
    ClSlash = ResKor.ClSlash ;
    SSlash  = ResKor.Sentence ; -- sentence missing NP; e.g. "she has looked at"
    Imp     = {s : Number => Polarity => Str} ; -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionKor.

    QCl = ResKor.QClause ;
    IComp = SS ;                -- interrogative complement of copula  e.g. "where"
    IDet = ResKor.Determiner ;  -- interrogative determiner            e.g. "how many"
    IQuant = ResKor.Quant ;     -- interrogative quantifier            e.g. "which"
    IP = ResKor.NounPhrase ;    -- interrogative pronoun               e.g. "who"

--2 Subord clauses and pronouns

    RCl = ResKor.RClause ;
    RP = SS ;

--2 Verb phrases

-- Constructed in VerbKor.

    VP = ResKor.VerbPhrase ;
    VPSlash = ResKor.VPSlash ;
    Comp = ResKor.Complement ;


--2 Adjectival phrases

-- Constructed in AdjectiveKor.

    AP = ResKor.AdjPhrase ;


--2 Nouns and noun phrases

-- Constructed in NounKor.
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in StructuralKor.
-- The determiner structure is
-- ``` Predet (QuantSg | QuantPl Num) Ord
-- as defined in NounKor.

    CN = ResKor.CNoun ;
    NP = ResKor.NounPhrase ;
    Pron = ResKor.Pronoun ; --Pronouns need enough info to turn it into NP or Quant.
    Det = ResKor.Determiner ;
    Predet = {s : Phono => Str ; p : Phono} ;
    Quant = ResKor.Quant ;
    Num = ResKor.Num ;
    Ord = ResKor.Adjective ** { -- Ord can come from AP and become AP again
      n : Number -- Ord can come from Num, which has inherent number
      } ;
    DAP = ResKor.Determiner ;


--2 Numerals

-- Constructed in NumeralKor.

    Card = ResKor.Num ;
    Numeral = ResKor.Numeral ;
    Digits = {s : CardOrd => Str ; n : Number} ;



--2 Structural words

-- Constructed in StructuralKor.
    Conj = ResKor.Conj ;
    Subj = SS ;
    Prep = ResKor.Postposition ;



--2 Words of open classes

-- These are constructed in LexiconKor and in
-- additional lexicon modules.

    VS,    -- sentence-complement verb            e.g. "claim"
    -- TODO: eventually different lincats
    VQ,    -- question-complement verb            e.g. "wonder"
    VA,    -- adjective-complement verb           e.g. "look"
    V = ResKor.Verb ;

    VV,    -- verb-phrase-complement verb         e.g. "want"
    V2A,   -- verb with NP and AP complement      e.g. "paint"
    V2V,   -- verb with NP and V complement       e.g. "cause"
    V2S,   -- verb with NP and S complement       e.g. "tell"
    V2Q,   -- verb with NP and Q complement       e.g. "ask"
    V2 = ResKor.Verb2 ;
    V3 = ResKor.Verb3 ;

    A = ResKor.Adjective ;
    A2  = ResKor.Adjective2 ;

    N = ResKor.Noun ;
    N2 = ResKor.Noun2 ;
    N3 = ResKor.Noun3 ;
    PN = ResKor.NounPhrase ;

linref
  V, V2, V3 = linVerb ;
  VP = linVP linVF ;
  AP = linAP ;
}
