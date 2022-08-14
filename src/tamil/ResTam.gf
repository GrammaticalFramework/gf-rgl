resource ResTam = ParamTam ** open Prelude, Predef in {

--------------------------------------------------------------------------------
-- Nouns
  oper
    Noun : Type = {s: ParamX.Number => ParamTam.Case  => Str} ;
--  Noun2 : Type = Noun ** {c2 : Preposition} ;
--  Noun3 : Type = Noun2 ** {c3 : Preposition} ;

    CNoun : Type = Noun ;
--      heavyMod : Str ; -- heavy stuff like relative clauses after determiner

--  PNoun : Type = Noun ;

    mkN : (_,_ : Str)  -> Noun =  \x,y -> {
      s = table {
	Sg => table{
	  Nom => x ;
	  Acc => x + "ai" ;
	  Dat => x + "ukku" ;
	  Soc => x + "otu" ;
	  Gen => x + "utaiya" ;
	  Instr => x + "al" ;
	  Loc => x + "itam" ;
	  Abl => x + "itamiruntu"
	  } ;
	Pl => table{
	  Nom => y + "kal" ;
	  Acc => y + "kal" + "ai" ;
	  Dat => y + "kal" + "ukku" ;
	  Soc => y + "kal" + "otu" ;
	  Gen => y + "kal" + "utaiya" ;
	  Instr => y + "kal" + "al" ;
	  Loc => y + "kal" + "itam" ;
	  Abl => y + "kal" + "itamiruntu"
	  }
	} ;
      } ;

  useN : Noun -> CNoun = \n -> n ** {
    heavyMod = []
    } ;

---------------------------------------------
-- Pronoun

--  Pronoun : Type = {
--    s : Str ;
--    p : Person ; -- for relative clauses
--    empty : Str ; -- need to avoid GF being silly. See https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#metavariables-or-those-question-marks-that-appear-when-parsing
--    } ;

--  mkPron : Str -> Person -> Pronoun = \str,p -> {
--    s = str ;
--    p = p ;
--    empty = []
--    } ;
---------------------------------------------
-- NP

    NounPhrase : Type = {
    s : Str ; 
--    empty : Str ; -- need to avoid GF being silly. See https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#metavariables-or-those-question-marks-that-appear-when-parsing
    } ;

--  IPhrase : Type = NounPhrase ** {
--    sp : NForm => Str ; -- standalone berapa banyak kucing
--  } ;

--  emptyNP : NounPhrase = {
--    s = \\_ => [] ;
--    a = NotPron ;
--    empty = []
--    } ;

  mkNounPhrase : Str -> NounPhrase = \str -> {
    s = str ;
--    a = NotPron ;
--    empty = []
    } ;

--  mkIP : Str -> IPhrase = \str -> {
--    s = \\_ => str ;
--    a = NotPron ;
--    empty = [] ;
--    sp = \\_ => str ;
--  } ;


--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  Quant : Type = {
    s : Str ; -- quantifier in a context, eg. 'berapa (kucing)' (Tamil: I (Nemo) am uncommenting only this part)
--    sp : NForm => Str ; -- a standalone, eg. '(kucing) berapa banyak'
--    poss : Possession ;
    } ;

--  IQuant : Type = Quant ** {
--    isPre : Bool ;
--  } ;

--  linDet : Determiner -> Str = \det -> det.pr ++ det.s ;

  Determiner : Type = Quant ** {
    pr : Str ; -- prefix for numbers
    n : NumType ; -- number as in 5 (noun in singular), Sg or Pl
    count: Str ;
    } ;
  
  CardNum : Type = {
    s : Str ;
    } ;

  Num : Type = CardNum ** {
    n : NumType
    } ; -- (Tamil: Necessary for DetQuant : Quant -> Num -> Det ;)

  baseNum : Num = {
    s = [] ;
   n = NoNum Sg
    } ;

--  CardOrdNum : Type = CardNum ** {
--    ord : Str
--    } ;

--  DigNum : Type = {
--    s : CardOrd => Str ;
--    } ;

  baseQuant : Quant = {
    s = [] ;
    sp = \\_ => [] ;
--    poss = Bare ;
    } ;

--      -- \\vf,pol, =>
--      -- let
--      --   verb   : Str    = joinVP vp tense ant pol agr ;
--      --   obj    : Str    = vp.s2 ! agr ;
--      -- in case ord of {
--      --   ODir   => subj ++ verb ++ obj ;  -- Ġanni jiekol ħut
--      --   OQuest => verb ++ obj ++ subj    -- jiekol ħut Ġanni ?
--      -- }

  mkQuant : Str -> Quant = \str -> baseQuant ** {
    s = str ; -- (Tamil: Extra arguments leftover from Malay)
    sp = \\_ => str
    } ;
  

  mkDet : Str -> Str -> Number -> Determiner = \cnt, str, num -> mkQuant str ** {
    pr = "" ;
    n = NoNum num ;
    count = "" ;
  } ;


--  mkIdet : Str -> Str -> Str -> Number -> Bool -> Determiner = \cnt, str, standalone, num, isPre -> mkDet cnt str num ** {
--    pr = case isPre of {True => str ; False => [] } ;
--    -- if isPre is True, then: "berapa kucing"
--    s = case isPre of { False => str ; True => [] };
--    count = cnt ;
--    sp = \\_ => standalone ;
--  } ;


--  --   s = \\p,a => vp.topic ++ np ++ vp.prePart ++ useVerb vp.verb ! p ! a ++ vp.compl ++ compl ;
--  -- np = vp.topic ++ np ;
--  -- vp = insertObj (ss compl) vp ;

--------------------------------------------------------------------------------
-- Prepositions

--  Preposition : Type = {
--    s : Str ;             -- dengan
--    obj : Person => Str ; -- dengan+nya -- needed in relative clauses to refer to the object
--    prepType : PrepType ; -- TODO rename, the name is confusing
--    } ;

--  mkPrep : Str -> Preposition = \dengan -> {
--    s = dengan ;
--    obj = \\p => dengan + poss2str (Poss p) ;
--    prepType = OtherPrep ;
--    } ;

--  -- direct object: "hits him" -> "memukul+nya"
--  dirPrep : Preposition = {
--    s = [] ;
--    obj = table {
--      P1 => BIND ++ "ku" ;
--      P2 => BIND ++ "mu" ;
--      P3 => BIND ++ "nya" } ;
--    prepType = DirObj ;
--    } ;

--  -- truly empty
--  emptyPrep : Preposition = {
--    s = [] ;
--    obj = \\_ => [] ;
--    prepType = EmptyPrep ;
--    } ;

--  datPrep : Preposition = mkPrep "kepada" ;

--  applyPrep : Preposition -> NounPhrase -> Str = \prep,np ->
--    case <np.a, prep.prepType> of {
--      <IsPron p,OtherPrep> => prep.obj ! p ++ np.empty ;
--      _                    => prep.s ++ np.s ! Bare
--    } ;

--------------------------------------------------------------------------------
-- Adjectives

--  Adjective : Type = SS ;
--  Adjective2 : Type = Adjective ;

--  mkAdj : Str -> Adjective = \str -> {s = str} ;

--  AdjPhrase : Type = Adjective ; -- ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

  Verb : Type = {s: Tense  => VForm => Str} ;

  mkVerb : (s : Str) -> Verb = \x -> {
      s = table {
	Past => table {
	  VF P1 Sg  => x + "nt" + "en" ;
	  VF P2 Sg => x + "nt" + "ay" ;
	  VFP3 Sg Hon => x + "nt" + "ar" ;
	  VFP3 Sg Masc => x + "nt" + "an" ;
	  VFP3 Sg Fem => x + "nt" + "al" ;
	  VFP3 Sg Neu => x + "nt" + "atu" ;
	  VF P1 Pl => x + "nt" + "om" ;
	  VF P2 Pl => x + "nt" + "irkal" ;
	  VFP3 Pl Hum => x + "nt" + "arkal" ;
	  VFP3 Pl Neu => x + "nt" + "ana" ;

	  VF _ _ => x ;
	  VFP3 _ _ => x 
	  } ;
	Pres => table{
	  VF P1 Sg  => x + "kir" + "en" ;
	  VF P2 Sg => x + "kir" + "ay" ;
	  VFP3 Sg Hon => x + "kir" + "ar" ;
	  VFP3 Sg Masc => x + "kir" + "an" ;
	  VFP3 Sg Fem => x + "kir" + "al" ;
	  VFP3 Sg Neu => x + "kir" + "atu" ;
	  VF P1 Pl => x + "kir" + "om" ;
	  VF P2 Pl => x + "kir" + "irkal" ;
	  VFP3 Pl Hum => x + "kir" + "arkal" ;
	  VFP3 Pl Neu => x + "kir" + "ana" ;

	  VF _ _ => x ;
	  VFP3 _ _ => x 
	  } ;
	Fut => table{
	  VF P1 Sg  => x + "v" + "en" ;
	  VF P2 Sg => x + "v" + "ay" ;
	  VFP3 Sg Hon => x + "v" + "ar" ;
	  VFP3 Sg Masc => x + "v" + "an" ;
	  VFP3 Sg Fem => x + "v" + "al" ;
	  VFP3 Sg Neu => x + "v" + "atu" ;
	  VF P1 Pl => x + "v" + "om" ;
	  VF P2 Pl => x + "v" + "irkal" ;
	  VFP3 Pl Hum => x + "v" + "arkal" ;
	  VFP3 Pl Neu => x + "v" + "ana" ;

	  VF _ _ => x ;
	  VFP3 _ _ => x
	  } ;
        Cond => table{
	  VF P1 Sg  => x + "v" + "en" ;
	  VF P2 Sg => x + "v" + "ay" ;
	  VFP3 Sg Hon => x + "v" + "ar" ;
	  VFP3 Sg Masc => x + "v" + "an" ;
	  VFP3 Sg Fem => x + "v" + "al" ;
	  VFP3 Sg Neu => x + "v" + "atu" ;
	  VF P1 Pl => x + "v" + "om" ;
	  VF P2 Pl => x + "v" + "irkal" ;
	  VFP3 Pl Hum => x + "v" + "arkal" ;
	  VFP3 Pl Neu => x + "v" + "ana" ;

	  VF _ _ => x ;
	  VFP3 _ _ => x 
	  }
	} ;
    } ; -- N.B. Cond is just a placeholder, it uses the Future Tamil case

--  mkVerb2 : Verb -> Preposition -> Verb2 = \v,pr -> v ** {
--    c2 = pr ;
--    passive = "di" ++ BIND ++ v.s ! Root
--    } ;

--  mkVerb3 : Verb -> (p,q : Preposition) -> Verb3 = \v,p,q ->
--    mkVerb2 v p ** {c3 = q} ;

--  mkVerb4 : Verb -> Preposition -> Str -> Verb4 = \v,pr,str -> v ** {
--    s = \\_ => v.s ! Active ++ str;
--    c2 = pr ;
--    passive = "di" ++ BIND ++ v.s ! Root ++ str
--    } ;

--  copula : Verb = {s = \\_ => "ada"} ; -- TODO
------------------
-- Adv

--  Adverb : Type = {
--    s : Str;
--  } ;

--  IAdv : Type = Adverb ** {
--    isPre : Bool ;
--    vf : VForm ;
--  } ;

------------------
-- VP

--  VerbPhrase : Type = {
--    s : VForm => Polarity => Str ; -- tidak or bukan
--    } ;

--  VPSlash : Type = VerbPhrase ** {
--    c2 : Preposition ;
--    adjCompl : Str ;
--    } ;

--  useV : Verb -> VerbPhrase = \v -> v ** {
--    s = \\vf,pol => verbneg pol ++ v.s ! vf
--    } ;

--  useComp : Str -> VerbPhrase = \s -> {
--    s = \\vf,pol => nounneg pol ++ s ;
--    } ;

--  linVP : VerbPhrase -> Str = \vp -> vp.s ! Active ! Pos;

-- https://www.reddit.com/r/indonesian/comments/gsizsv/when_to_use_tidak_bukan_jangan_belum/

--  verbneg : Polarity -> Str = \pol -> case pol of {
--    Neg => "tidak" ; -- or "tak"?
--    Pos => []
--    } ;

--  nounneg : Polarity -> Str = \pol -> case pol of {
--    Neg => "bukan" ;
--    Pos => []
--    } ;

--  impneg : Polarity -> Str = \pol -> case pol of {
--    Neg => "jangan" ;
--    Pos => []
--  } ;
--------------------------------------------------------------------------------
-- Cl, S

--  Clause : Type = {
--    subj : Str ;
--    pred : VForm => Polarity => Str -- Cl may become relative clause, need to keep open VForm
--    } ;

--  RClause : Type = {
--    subj : Str ;
--    pred : Person => Polarity => Str
--    } ;

--  RS : Type = {s : Person => Str} ;

--  ClSlash : Type = Clause ** {c2 : Preposition} ;

--  Sentence : Type = {s : Str} ;

--  predVP : NounPhrase -> VerbPhrase -> Clause = \np,vp -> {
--    subj = np.s ! Bare ;
--    pred = vp.s
--    } ;

--  predVPSlash : NounPhrase -> VPSlash -> ClSlash = \np,vps ->
--    predVP np <vps : VerbPhrase> ** {c2 = vps.c2} ;


--  -- mkClause : Str -> NounPhrase -> VPSlash -> Clause = \str,np,vp -> {
--  --   subj = str ++ np.s ! Bare;
--  --   pred = vp.s
--  -- } ;


--  -- mkClause : Str -> IPhrase -> VerbPhrase -> Clause = \str,ip,vp -> {
--  --   subj = ip.s ! Bare ;
--  --   pred = vp.s ;
--  -- } ;


--  -- baseQuant : Quant = {
--  --   s = [] ;
--  --   sp = \\_ => [] ;
--  --   poss = Bare ;
--  --   } ;

--  --     -- \\vf,pol, =>
--  --     -- let
--  --     --   verb   : Str    = joinVP vp tense ant pol agr ;
--  --     --   obj    : Str    = vp.s2 ! agr ;
--  --     -- in case ord of {
--  --     --   ODir   => subj ++ verb ++ obj ;  -- Ġanni jiekol ħut
--  --     --   OQuest => verb ++ obj ++ subj    -- jiekol ħut Ġanni ?
--  --     -- }

--  -- mkQuant : Str -> Quant = \str -> baseQuant ** {
--  --   s = str ;
--  --   sp = \\_ => str
--  --   } ;

--------------------------------------------------------------------------------
-- linrefs

--}
}
