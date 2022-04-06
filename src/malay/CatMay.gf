concrete CatMay of Cat = CommonX - [IAdv] ** open ResMay, Prelude in {

  flags optimize=all_subs ;

  lincat

--2 Sentences and clauses
-- Constructed in SentenceMay, and also in IdiomMay

    S  = ResMay.Sentence ;
    QS = SS ;
    RS = ResMay.RS ;
    -- relative sentence. Tense and polarity fixed,
    -- but agreement may depend on the CN/NP it modifies.

    Cl = ResMay.Clause ;
    ClSlash = ResMay.ClSlash ;
    SSlash  = ResMay.Sentence ; -- sentence missing NP; e.g. "she has looked at"
    Imp     = {s : Number => Polarity => Str} ; -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionMay.

    QCl = ResMay.Clause ;
    IComp = {s: Str} ;                -- interrogative complement of copula  e.g. "where"
    IDet = ResMay.Determiner ;  -- interrogative determiner            e.g. "how many"
    IQuant = ResMay.IQuant ;     -- interrogative quantifier            e.g. "which"
    IP = ResMay.IPhrase ;    -- interrogative pronoun               e.g. "who"
    IAdv = ResMay.IAdv ;

--2 Subord clauses and pronouns

    RCl = ResMay.RClause ;
    RP = SS ;

--2 Verb phrases

-- Constructed in VerbMay.

    VP = ResMay.VerbPhrase ;
    VPSlash = ResMay.VPSlash ;
    Comp = ResMay.VerbPhrase ;


--2 Adjectival phrases

-- Constructed in AdjectiveMay.

    AP = ResMay.AdjPhrase ;


--2 Nouns and noun phrases

-- Constructed in NounMay.
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in StructuralMay.
-- The determiner structure is
-- ``` Predet (QuantSg | QuantPl Num) Ord
-- as defined in NounMay.

    CN = ResMay.CNoun ;
    NP = ResMay.NounPhrase ;
    Pron = ResMay.Pronoun ; --Pronouns need enough info to turn it into NP or Quant.
    Det = ResMay.Determiner ;
    Predet = SS ;
    Quant = ResMay.Quant ;
    Num = ResMay.Num ;
    Ord = {
      s : Str ; -- AForm => Str ; -- Ord can came from AP and become AP again
      -- n : Number -- Ord can come from Num, which has inherent number
      } ;
    DAP = ResMay.Determiner ;


--2 Numerals

-- Constructed in NumeralMay.

    Card = ResMay.CardNum ;
    Numeral = ResMay.CardOrdNum ;
    Digits = ResMay.DigNum ;



--2 Structural words

-- Constructed in StructuralMay.
    Conj = {s2 : Str ; s1 : Str ; n : Number } ;
    Subj = SS ;
    Prep = ResMay.Preposition ;



--2 Words of open classes

-- These are constructed in LexiconMay and in
-- additional lexicon modules.

    -- TODO: eventually different lincats
    VS,    -- sentence-complement verb            e.g. "claim"
    VQ,    -- question-complement verb            e.g. "wonder"
    VA,    -- adjective-complement verb           e.g. "look"
    V = ResMay.Verb ;

    VV     -- verb-phrase-complement verb         e.g. "want"
      = SS ;

    V2A,   -- verb with NP and AP complement      e.g. "paint"
    V2V,   -- verb with NP and V complement       e.g. "cause"
    V2S,   -- Reverb with NP and S complement       e.g. "tell"
    V2Q,   -- verb with NP and Q complement       e.g. "ask"
    V2 = ResMay.Verb2 ;
    V3 = ResMay.Verb3 ;

    A = ResMay.Adjective ;
    A2  = ResMay.Adjective2 ;

    N = ResMay.Noun ;
    N2 = ResMay.Noun2 ;
    N3 = ResMay.Noun3 ;
    PN = ResMay.PNoun ;
}
