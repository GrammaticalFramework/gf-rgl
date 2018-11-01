concrete CatSom of Cat = CommonX - [Adv] ** open ResSom, Prelude in {

  flags optimize=all_subs ;

  lincat

--2 Sentences and clauses
-- Constructed in SentenceSom, and also in IdiomSom

    S  = ResSom.Sentence ;
    QS = ResSom.Sentence ;
    RS = { s : Agreement => Str } ;
    -- relative sentence. Tense and polarity fixed,
    -- but agreement may depend on the CN/NP it modifies.

    Cl = ResSom.Clause ;
    ClSlash = ResSom.ClSlash ;
    SSlash  = ResSom.Sentence ; -- sentence missing NP; e.g. "she has looked at"
    Imp     = { s : Str } ;   -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionSom.

    QCl = ResSom.Clause ;
    IP = ResSom.NounPhrase ;
    IComp = { s : Str } ; -- interrogative complement of copula  e.g. "where"
    IDet = ResSom.Determiner ;  -- interrogative determiner            e.g. "how many"
    IQuant = ResSom.Quant ; -- interrogative quantifier            e.g. "which"




--2 Relative clauses and pronouns

-- Constructed in RelativeSom.

    RCl = ResSom.RClause ;
    RP = { s : Str } ;



--2 Verb phrases

-- Constructed in VerbSom.

    VP = ResSom.VerbPhrase ;
    VPSlash = ResSom.VPSlash ;
    Comp = ResSom.Complement ;


--2 Adjectival phrases

-- Constructed in AdjectiveSom.

    AP = ResSom.AdjPhrase ;


--2 Nouns and noun phrases

-- Constructed in NounSom.
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in StructuralSom.
-- The determiner structure is
-- ``` Predet (QuantSg | QuantPl Num) Ord
-- as defined in NounSom.

    CN = ResSom.CNoun ;
    NP = ResSom.NounPhrase ;
    Pron = ResSom.Pronoun ; --Pronouns need enough info to turn it into NP or Quant.
    Det = ResSom.Determiner ;
    Predet = {s : Str} ;
    Quant = ResSom.Quant ;
    Num = { s : Str ; n : Number ; isNum : Bool } ;
    Card, Ord = { s : Str ; n : Number } ;
    DAP = ResSom.Determiner ;


--2 Numerals

-- Constructed in NumeralSom.

    Numeral = { s : Str ; n : Number } ;
    Digits = { s : CardOrd => Str ; n : Number } ;



--2 Structural words

-- Constructed in StructuralSom.
    Conj = { s1,s2 : Str ; n : Number } ;
    Subj = { s : Str ; isPre : Bool } ; --ba+dut vs. dut+en
    Prep = ResSom.Prep ;



--2 Words of open classes

-- These are constructed in LexiconSom and in
-- additional lexicon modules.

    V,
    V2,
    V3,
    VV,    -- verb-phrase-complement verb         e.g. "want"
    VS,    -- sentence-complement verb            e.g. "claim"
    VQ,    -- question-complement verb            e.g. "wonder"
    VA,    -- adjective-complement verb           e.g. "look"
    V2V,   -- verb with NP and V complement       e.g. "cause"
    V2S,   -- verb with NP and S complement       e.g. "tell"
    V2Q,   -- verb with NP and Q complement       e.g. "ask"
    V2A = ResSom.Verb ;   -- verb with NP and AP complement      e.g. "paint"

    A = ResSom.Adjective ;
    A2  = ResSom.Adjective2 ;

    N = ResSom.Noun ;
    N2 = ResSom.Noun2 ;
    N3 = ResSom.Noun3 ;
    PN = ResSom.PNoun ;

    Adv = ResSom.Adverb ;

linref
    -- Cl = linCl ;
    VP = linVP ;
    CN = linCN ;
}
