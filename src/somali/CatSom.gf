concrete CatSom of Cat = CommonX - [Adv,IAdv] ** open ResSom, Prelude in {

  flags optimize=all_subs ;

  lincat

--2 Sentences and clauses
-- Constructed in SentenceSom, and also in IdiomSom

    S  = ResSom.Sentence ;
    QS = SS ;
    RS = {s : State => GenNum => Case => Str} ;
    -- relative sentence. Tense and polarity fixed,
    -- but agreement may depend on the CN/NP it modifies.

    Cl = ResSom.ClSlash ;
    ClSlash = ResSom.ClSlash ;
    SSlash  = ResSom.Sentence ; -- sentence missing NP; e.g. "she has looked at"
    Imp     = {s : Number => Polarity => Str} ; -- imperative             e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in QuestionSom.

    QCl = ResSom.QClause ;
    IComp = SS ;                -- interrogative complement of copula  e.g. "where"
    IDet = ResSom.Determiner ;  -- interrogative determiner            e.g. "how many"
    IQuant = ResSom.Quant ; -- interrogative quantifier            e.g. "which"
    IP = ResSom.NounPhrase ** {contractSTM : Bool} ;  -- like NP but may contract with STM
    IAdv = ResSom.IAdv ;

--2 Subord clauses and pronouns

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
    Predet = {s : Str ; da : DefArticle ; isPoss : Bool} ;
    Quant = ResSom.Quant ;
    Num = ResSom.Num ;
    Ord = {
      s : AForm => Str ; -- Ord can came from AP and become AP again
      n : Number -- Ord can come from Num, which has inherent number
      } ;
    DAP = ResSom.Determiner ;


--2 Numerals

-- Constructed in NumeralSom.

    Card = BaseNum ;
    Numeral = ResSom.Numeral ;
    Digits = {s : CardOrd => Str ; n : Number} ;



--2 Structural words

-- Constructed in StructuralSom.
    Conj = {s2 : State => Str ; s1 : Str ; n : Number } ;
    Subj = SS ;
    Prep = ResSom.Prep ;



--2 Words of open classes

-- These are constructed in LexiconSom and in
-- additional lexicon modules.

    VS,    -- sentence-complement verb            e.g. "claim"
    -- TODO: eventually different lincats
    VQ,    -- question-complement verb            e.g. "wonder"
    VA,    -- adjective-complement verb           e.g. "look"
    V = ResSom.Verb ;

    VV = ResSom.VV ;    -- verb-phrase-complement verb         e.g. "want"

    V2A,   -- verb with NP and AP complement      e.g. "paint"
    V2V,   -- verb with NP and V complement       e.g. "cause"
    V2S,   -- verb with NP and S complement       e.g. "tell"
    V2Q,   -- verb with NP and Q complement       e.g. "ask"
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
    VP = infVP ;
    CN = linCN ;
    Prep = \prep -> prep.s ! P3_Prep ++ prep.sii ++ prep.dhex ++ prep.hoostiisa ! Sg3 Masc ;
}
