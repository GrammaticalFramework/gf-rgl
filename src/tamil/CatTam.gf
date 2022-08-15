concrete CatTam of Cat = CommonX - [IAdv] ** open ResTam, Prelude in {

--  flags optimize=all_subs ;

  lincat

--2 Sentences and clauses
-- Constructed in SentenceTam, and also in IdiomTam

--    S  = ResTam.Sentence ;
--    QS = SS ;
--    RS = ResTam.RS ;
--    -- relative sentence. Tense and polarity fixed,
--    -- but agreement may depend on the CN/NP it modifies.

--    Cl = ResTam.Clause ;
--    ClSlash = ResTam.ClSlash ;
--    SSlash  = ResTam.Sentence ; -- sentence missing NP; e.g. "she has looked at"
--    Imp     = {s : Number => Polarity => Str} ; -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionTam.

--    QCl = ResTam.Clause ;
--    IComp = {s: Str} ;                -- interrogative complement of copula  e.g. "where"
--    IDet = ResTam.Determiner ;  -- interrogative determiner            e.g. "how many"
--    IQuant = ResTam.IQuant ;     -- interrogative quantifier            e.g. "which"
--    IP = ResTam.IPhrase ;    -- interrogative pronoun               e.g. "who"
--    IAdv = ResTam.IAdv ;

--2 Subord clauses and pronouns

--    RCl = ResTam.RClause ;
--    RP = SS ;

--2 Verb phrases

-- Constructed in VerbTam.

--    VP = ResTam.VerbPhrase ;
--    VPSlash = ResTam.VPSlash ;
--    Comp = ResTam.VerbPhrase ;


--2 Adjectival phrases

-- Constructed in AdjectiveTam.

--    AP = ResTam.AdjPhrase ;


--2 Nouns and noun phrases

-- Constructed in NounTam.
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in StructuralTam.
-- The determiner structure is
-- ``` Predet (QuantSg | QuantPl Num) Ord
-- as defined in NounTam.

    CN = ResTam.CNoun ;
    NP = ResTam.NounPhrase ;
--    Pron = ResTam.Pronoun ; --Pronouns need enough info to turn it into NP or Quant.
    Det = ResTam.Determiner ;
--    Predet = SS ;
--    Quant = ResTam.Quant ;
    Num = ResTam.Num ;
--    Ord = {
--      s : Str ; -- AForm => Str ; -- Ord can came from AP and become AP again
--      -- n : Number -- Ord can come from Num, which has inherent number
--      } ;
--    DAP = ResTam.Determiner ;


--2 Numerals

-- Constructed in NumeralTam.

--    Card = ResTam.CardNum ;
--    Numeral = ResTam.CardOrdNum ;
--    Digits = ResTam.DigNum ;



--2 Structural words

-- Constructed in StructuralTam.
--    Conj = {s2 : Str ; s1 : Str ; n : Number } ;
--    Subj = SS ;
--    Prep = ResTam.Preposition ;



--2 Words of open classes

-- These are constructed in LexiconTam and in
-- additional lexicon modules.

--    -- TODO: eventually different lincats
--    VS,    -- sentence-complement verb            e.g. "claim"
--    VQ,    -- question-complement verb            e.g. "wonder"
--    VA,    -- adjective-complement verb           e.g. "look"
--    V = ResTam.Verb ;

--    VV     -- verb-phrase-complement verb         e.g. "want"
--      = SS ;

--    V2A,   -- verb with NP and AP complement      e.g. "paint"
--    V2V,   -- verb with NP and V complement       e.g. "cause"
--    V2S,   -- Reverb with NP and S complement       e.g. "tell"
--    V2Q,   -- verb with NP and Q complement       e.g. "ask"
--    V2 = ResTam.Verb2 ;
--    V3 = ResTam.Verb3 ;

--    A = ResTam.Adjective ;
--    A2  = ResTam.Adjective2 ;

    N = ResTam.Noun ;
--    N2 = ResTam.Noun2 ;
--    N3 = ResTam.Noun3 ;
--    PN = ResTam.PNoun ;

--    linref
--        CN = \cn -> cn.s ! NF Sg Bare ++ cn.heavyMod;
--        ClSlash = \cl -> cl.subj ++ cl.pred ! Root ! Pos ++ cl.c2.s;
--        RCl = \cl -> cl.subj ++ cl.pred ! P1 ! Pos;
--        Cl = \cl -> cl.subj ++ cl.pred ! Active ! Pos;
--        Det = linDet ;
--}

--   Determiner : Type = Quant ** {
--     pr : Str ; -- prefix for numbers
--     n : NumType ; -- number as in 5 (noun in singular), Sg or Pl
--     } ;
}
