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
    Imp     = SS ;              -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionSom.

    QCl = ResSom.QClause ;
    IP = ResSom.NounPhrase ;
    IComp = SS ;              -- interrogative complement of copula  e.g. "where"
    IDet = ResSom.Determiner ;  -- interrogative determiner            e.g. "how many"
    IQuant = ResSom.Quant ; -- interrogative quantifier            e.g. "which"

--2 Relative clauses and pronouns

-- Constructed in RelativeSom.

    RCl = ResSom.RClause ;
    RP = SS ;

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
    Num = ResSom.Num ;
    Ord = {s : Str ; n : Number} ;
    DAP = ResSom.Determiner ;


--2 Numerals

-- Constructed in NumeralSom.

    Card = BaseNum ;
    Numeral = ResSom.Numeral ;
    Digits = {s : CardOrd => Str ; n : Number} ;



--2 Structural words

-- Constructed in StructuralSom.
    Conj = { s1,s2 : Str ; n : Number } ;
    Subj = { s : Str ; isPre : Bool } ; --ba+dut vs. dut+en
    Prep = ResSom.Prep ** {c2 : Preposition ; berri, sii, dhex : Str} ;



--2 Words of open classes

-- These are constructed in LexiconSom and in
-- additional lexicon modules.

    V,
    -- TODO: eventually proper lincats
    VV,    -- verb-phrase-complement verb         e.g. "want"
    VS,    -- sentence-complement verb            e.g. "claim"
    VQ,    -- question-complement verb            e.g. "wonder"
    VA,    -- adjective-complement verb           e.g. "look"
    V2V,   -- verb with NP and V complement       e.g. "cause"
    V2S,   -- verb with NP and S complement       e.g. "tell"
    V2Q,   -- verb with NP and Q complement       e.g. "ask"
    V2A = ResSom.Verb ;   -- verb with NP and AP complement      e.g. "paint"

    V2 = ResSom.Verb2 ;
    V3 = ResSom.Verb3 ;

    A = ResSom.Adjective ;
    A2  = ResSom.Adjective2 ;

    N = ResSom.Noun ;
    N2 = ResSom.Noun2 ;
    N3 = ResSom.Noun3 ;
    PN = ResSom.PNoun ;

    Adv = ResSom.Adverb ; -- Preposition of an adverbial can merge with obligatory complements of the verb.

linref
    -- Cl = linCl ;
    VP = linVP ;
    CN = linCN ;
    Prep = \prep -> prep.s ! Pl3 ++ prep.sii ++ prep.dhex ;
}
