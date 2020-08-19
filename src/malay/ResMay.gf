resource ResMay = ParamMay ** open Prelude, Predef, ParamMay in {

--------------------------------------------------------------------------------
-- Nouns
oper

  Noun : Type = {
    s : NForm => Str
    } ;
  Noun2 : Type = Noun ; -- TODO eventually more parameters?
  Noun3 : Type = Noun ;

  CNoun : Type = Noun ** {
    } ;

  PNoun : Type = Noun ;

  mkNoun : Str -> Noun = \anjing -> {
    s = table {
      NF Sg p => anjing + ParamMay.poss2str p ;
      NF Pl p => duplicate anjing + ParamMay.poss2str p
      }
    } ;

  useN : Noun -> CNoun = \n -> n ;

---------------------------------------------
-- NP

  NounPhrase = {
    s : Str ;
    p : Person -- for relative clauses
    } ;

--------------------------------------------------------------------------------
-- Pronouns

  Pronoun : Type = NounPhrase ** {
    -- poss : { -- for PossPron : Pron -> Quant
    --   } ;
    sp : NForm => Str ;
    } ;


--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  BaseQuant : Type = {
    s : Str ;
    isPoss : Bool
    } ;

  Determiner : Type = BaseQuant ** {
    sp : NForm => Str ;
    n : Number ;
--    numtype : NumType ; -- number as in "5" or "Sg/Pl", often makes a difference in lots of things
    } ;

  Quant : Type = BaseQuant ** {
    sp : NForm => Str ;
    } ;

  Num : Type = {
    s : DForm => Str ; -- independent or attribute
    n : Number
    } ;

  baseNum : Num = {
    s = \\_ => [] ;
    n = Sg ;
    numtype = NoNum
    } ;

  Numeral : Type = Num ** {
    ord : Str
    } ;

  baseQuant : BaseQuant = {
    s = [] ;
    isPoss = False ;
    } ;

  mkQuant : Str -> Quant = \str -> baseQuant ** {
    s = str ;
    sp = \\_ => str
    } ;

  --------------------------------------------------------------------------------
-- Prepositions

  Preposition : Type = {
    s : Str ;           -- dengan
    obj : Person => Str -- dengan+nya -- needed in relative clauses to refer to the object
    } ;

  mkPrep : Str -> Preposition = \dengan -> {
    s = dengan ;
    obj = \\p => dengan + poss2str (Poss p)
    } ;

  emptyPrep : Preposition = {
    s = [] ;
    obj = \\_ => []
    } ;

--------------------------------------------------------------------------------
-- Adjectives

  Adjective : Type = Verb ; -- TODO check if meaningful
  Adjective2 : Type = Adjective ;

  mkAdj : Str -> Adjective = \str -> {s = \\_ => str} ;

  AdjPhrase : Type = Adjective ; -- ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

  Verb : Type = {
    s : VForm => Str
    } ;
  Verb2 : Type = Verb ** {
    c2 : Preposition ;
    passive : Str
    } ;

  Verb3 : Type = Verb2 ** {
    c3 : Preposition
    } ;

--  VV : Type = Verb ** {vvtype : VVForm} ;

  mkVerb : Str -> Prefix -> Verb = \str,p -> {
    s = table {
      Root => str ;
      Active => prefix p str
      }
    } ;

  mkVerb2 : Verb -> Preposition -> Verb2 = \v,pr -> v ** {
    c2 = pr ;
    passive = "di" + v.s ! Root -- TODO check
    } ;

  mkVerb3 : Verb -> (p,q : Preposition) -> Verb3 = \v,p,q ->
    mkVerb2 v p ** {c3 = q} ;

  copula : Verb = {s = \\_ => "ada"} ; -- TODO
------------------
-- Adv

  Adverb : Type = SS ;

------------------
-- VP

  VerbPhrase : Type = {
    s : VForm => Polarity => Str ; -- tidak or bukan
    } ;

  VPSlash : Type = VerbPhrase ** {
    c2 : Preposition ;
    } ;

  useV : Verb -> VerbPhrase = \v -> v ** {
    s = \\vf,pol => verbneg pol ++ v.s ! vf
    } ;

  useComp : Str -> VerbPhrase = \s -> {
    s = \\vf,pol => nounneg pol ++ s ;
    } ;

  verbneg : Polarity -> Str = \pol -> case pol of {
    Neg => "tidak" ; -- or "tak"?
    Pos => []
    } ;

  nounneg : Polarity -> Str = \pol -> case pol of {
    Neg => "bukan" ;
    Pos => []
    } ;
--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {
    subj : Str ;
    pred : VForm => Polarity => Str -- Cl may become relative clause, need to keep open VForm
    } ;

  RClause : Type = {
    subj : Str ;
    pred : Person => Polarity => Str
    } ;

  RS : Type = {s : Person => Str} ;

  ClSlash : Type = Clause ** {c2 : Preposition} ;

  Sentence : Type = {s : Str} ;

  predVP : NounPhrase -> VerbPhrase -> Clause = \np,vp -> {
    subj = np.s ;
    pred = vp.s
    } ;

  predVPSlash : NounPhrase -> VPSlash -> ClSlash = \np,vps ->
    predVP np <vps : VerbPhrase> ** {c2 = vps.c2} ;

--------------------------------------------------------------------------------
-- linrefs

}
