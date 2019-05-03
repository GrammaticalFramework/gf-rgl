--# -path=.:../prelude:../abstract:../common

concrete NounCgg of Noun = CatCgg **
  open ResCgg, Prelude in {

lin 
  
  --UsePN   : PN -> NP ;          -- John
  UsePN pn = {s = \\ _ =>  pn.s; agr = pn.a}; -- John
  
  UsePron pron = { s = pron.s; agr = pron.agr};  --: Pron -> NP ;            -- he
  --UsePron pron = pron; -- the result of use pron is a NounPhrase
  --MassNP     : CN -> NP ;            -- (beer)
  MassNP cn = {s = \\_ =>cn.s ! Complete ! Pl; agr = AgP3 Pl cn.gender};   --: CN -> NP ; -- milk
  --DetCN det cn = mkDeterminer det cn; --Should be nemed mkDetCN
  DetCN  det cn =  mkDetCN det cn;       -- the man
    {-
    case det.pos of{ 
                     PreDeterminer =>{s = det.s ++ cn.s!det.ntype!det.num} ;
                     PostDeterminer=>{s = cn.s!det.ntype!det.num ++ mkNCParticles!SubjM!AgP3 det.num cn.nc + det.s}
                   }; --use a function because code is becoming long
    -}
  UseN noun = noun ;

  --Noun = {s : NounType=>Number => Str ; nc : NClass} ;
  --AdjCN ap cn = {s=\\ntype, num=>cn.s!ntype!num ++ ap.s!AgP3 num cn.nc; nc=cn.nc};
    AdjCN ap cn = 
      case <ap.isPre, ap.isProper, > of {
          <True, True> => { 
                    s = \\ ns, num =>ap.s ++ cn.s ! ns ! num ; 
                    gender = cn.gender 
                  };
          <False, False> => case ap.isPrep of {
                     False  =>  { 
                            s = \\ ns, num => cn.s ! ns ! num ++ mkAdjPronIVClitic (AgP3 num cn.gender) 
                                 ++ ap.post ; 
                              gender = cn.gender   
                          };
                     True  =>  { 
                            s = \\ ns, num => (cn.s ! ns ! num) ++ 
                                mkGenPrepNoIVClitic (AgP3 num cn.gender) ++ ap.post ; 
                            gender = cn.gender 
                          }
                  };
          <True, False> => { 
                    s = \\ ns, num => mkAdjPronIVClitic (AgP3 num cn.gender) 
                               ++ ap.s ++ (cn.s ! ns ! num) ; 
                    gender = cn.gender 
                  };
          <False, True> => { 
                     s = \\ ns, num => (cn.s ! ns ! num) ++ ap.post ; 
                     gender = cn.gender
                  }                   

    };        -- big house


    {-
      A predeterminer is any word that modifies a noun Phrase.
      These Predeterminers are found in Structural
    -}
    --PredetNP : Predet -> NP -> NP ; -- only the man 
    PredetNP predet np = let a = np.agr;
                             nomS = np.s ! Nom; --It does not matter which. Just pick out one.
                             accS = np.s ! Acc;
                         in
                          case <predet.isMWE, predet.isInflected> of {
                              <False, True>  => {s = \\_ =>nomS ++ mkPredetPref a ++ Predef.BIND ++ predet.s ; agr = a};
                              <True, True >  => {s = \\_ =>nomS ++ mkPredetPref a ++ Predef.BIND ++ predet.s  ++
                                                 mkPredetPref a ++ Predef.BIND ++ predet.s2; agr = a};
                              <False,False>  => {s = \\_ =>nomS ++ predet.s ; agr = a};
                              <True,False>   => {s = \\_ =>nomS ++ predet.s ++ predet.s2; agr = a} -- never seen this case              
                          };

      --AdvNP   : NP -> Adv -> NP ;    -- Paris today
      AdvNP np adv = {s= table{ Nom => np.s!Nom ++ adv.s; Acc => np.s !Acc ++ adv.s}; agr = np.agr };


      {-What the hell does this mean?-}
      --ExtAdvNP: NP -> Adv -> NP ;    -- boys, such as ..
--    Determiner: Type = {s:Str; ntype:NounType; num:Number; pos:Position}; -- type for Determier necessary for catCgg.gf
{-
  Need sample quatitifiers in Runynakore. Immediate, Far, Distant. so DetQuant is commented out
-}
--DetQuant quant num = {s = quant.s ++ num.s ; ntype = Complete ; num = num.n ; pos = PostDeterminer } ;

  --NumSg = {s=[]; n=Sg};
  --NumPl = {s=[]; n=Pl};
  -- NumCard card = {...};

  --IndefArt = {s=[]};
  --DefArt = {s=[]} ;

-- AdvCN   : CN -> Adv -> CN ;   -- house on the hill
   --AdvCN cn adv ={s=\\ntype,num =>cn.s!ntype!num ++ adv.s!(AgP3 num cn.nc); nc=cn.nc};
{-
--1 Noun: Nouns, noun phrases, and determiners

abstract Noun = Cat ** {


--2 Noun phrases

-- The three main types of noun phrases are
-- - common nouns with determiners
-- - proper names
-- - pronouns
--
--
  fun
    DetCN   : Det -> CN -> NP ;   -- the man
    UsePN   : PN -> NP ;          -- John
    UsePron : Pron -> NP ;        -- he

-- Pronouns are defined in the module [``Structural`` Structural.html].

-- A noun phrase already formed can be modified by a $Predet$erminer.

    PredetNP : Predet -> NP -> NP ; -- only the man 

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause

    PPartNP : NP -> V2  -> NP ;    -- the man seen
    AdvNP   : NP -> Adv -> NP ;    -- Paris today
    ExtAdvNP: NP -> Adv -> NP ;    -- boys, such as ..
    RelNP   : NP -> RS  -> NP ;    -- Paris, which is here

-- Determiners can form noun phrases directly.

    DetNP   : Det -> NP ;  -- these five


--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

    DetQuant    : Quant -> Num ->        Det ;  -- these five
    DetQuantOrd : Quant -> Num -> Ord -> Det ;  -- these five best

-- Whether the resulting determiner is singular or plural depends on the
-- cardinal.

-- All parts of the determiner can be empty, except $Quant$, which is
-- the "kernel" of a determiner. It is, however, the $Num$ that determines
-- the inherent number.

    NumSg   : Num ;  -- [no numeral, but marked as singular]
    NumPl   : Num ;  -- [no numeral, but marked as plural]
    NumCard : Card -> Num ; -- one/five [explicit numeral]

-- $Card$ consists of either digits or numeral words.

  data
    NumDigits  : Digits  -> Card ;  -- 51
    NumNumeral : Numeral -> Card ;  -- fifty-one

-- The construction of numerals is defined in [Numeral Numeral.html].

-- A $Card$ can  be modified by certain adverbs.

  fun
    AdNum : AdN -> Card -> Card ;   -- almost 51

-- An $Ord$ consists of either digits or numeral words.
-- Also superlative forms of adjectives behave syntactically like ordinals.

    OrdDigits  : Digits  -> Ord ;  -- 51st
    OrdNumeral : Numeral -> Ord ;  -- fifty-first
    OrdSuperl  : A       -> Ord ;  -- warmest

-- One can combine a numeral and a superlative.

    OrdNumeralSuperl : Numeral -> A -> Ord ; -- third largest

-- Definite and indefinite noun phrases are sometimes realized as
-- neatly distinct words (Spanish "un, unos ; el, los") but also without
-- any particular word (Finnish; Swedish definites).

    IndefArt   : Quant ;  -- a/an
    DefArt     : Quant ;  -- the

-- Nouns can be used without an article as mass nouns. The resource does
-- not distinguish mass nouns from other common nouns, which can result
-- in semantically odd expressions.

    MassNP     : CN -> NP ;            -- (beer)

-- Pronouns have possessive forms. Genitives of other kinds
-- of noun phrases are not given here, since they are not possible
-- in e.g. Romance languages. They can be found in $Extra$ modules.

    PossPron : Pron -> Quant ;    -- my (house)

-- Other determiners are defined in [Structural Structural.html].



--2 Common nouns

-- Simple nouns can be used as nouns outright.

    UseN : N -> CN ;              -- house

-- Relational nouns take one or two arguments.

    ComplN2 : N2 -> NP -> CN ;    -- mother of the king
    ComplN3 : N3 -> NP -> N2 ;    -- distance from this city (to Paris)

-- Relational nouns can also be used without their arguments.
-- The semantics is typically derivative of the relational meaning.

    UseN2   : N2 -> CN ;          -- mother
    Use2N3  : N3 -> N2 ;          -- distance (from this city)
    Use3N3  : N3 -> N2 ;          -- distance (to Paris)

-- Nouns can be modified by adjectives, relative clauses, and adverbs
-- (the last rule will give rise to many 'PP attachment' ambiguities
-- when used in connection with verb phrases).

    
    AdjCN   : AP -> CN  -> CN ;   -- big house
    RelCN   : CN -> RS  -> CN ;   -- house that John bought
    AdvCN   : CN -> Adv -> CN ;   -- house on the hill

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in [Verb Verb.html].

    SentCN  : CN -> SC  -> CN ;   -- question where she sleeps

--2 Apposition

-- This is certainly overgenerating.

    ApposCN : CN -> NP -> CN ;    -- city Paris (, numbers x and y)

--2 Possessive and partitive constructs

-- (New 13/3/2013 AR; Structural.possess_Prep and part_Prep should be deprecated in favour of these.)

    PossNP  : CN -> NP -> CN ;     -- house of Paris, house of mine
    PartNP  : CN -> NP -> CN ;     -- glass of wine

-- This is different from the partitive, as shown by many languages.

    CountNP : Det -> NP -> NP ;    -- three of them, some of the boys

--3 Conjoinable determiners and ones with adjectives

    AdjDAP : DAP -> AP -> DAP ;    -- the large (one)
    DetDAP : Det -> DAP ;          -- this (or that) 

-}

}
