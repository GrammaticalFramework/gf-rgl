--# -path=.:../prelude:../abstract:../common

concrete NounCgg of Noun = CatCgg **
  open ResCgg, Prelude, Predef in {

lin 
  
  --UsePN   : PN -> NP ;          -- John
  UsePN pn = {s = \\ _ =>  pn.s; agr = pn.a; nounCat = PropNoun}; -- John
  
  {- need use of a pre -}
  UsePron pron = 
    let default3PAgr = (AgP3 Sg KI_BI)
    in case <pron.agr> of {
            <(AgrYes a)> => {s = pron.s;  agr = a; nounCat = ComNoun};  --: Pron -> NP ;            -- he
            <_>        =>   {s = pron.third !default3PAgr; agr = default3PAgr; nounCat = ComNoun}
       };
  --UsePron pron = pron; -- the result of use pron is a NounPhrase
  --MassNP     : CN -> NP ;            -- (beer)
  MassNP cn = {s = \\_ =>cn.s ! Sg ! Complete; agr = AgP3 Sg cn.gender; nounCat = ComNoun};   --: CN -> NP ; -- milk
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
  --AdjCN   : AP -> CN  -> CN ;  -- big house
    AdjCN ap cn = 
      case <ap.position, ap.isProper, > of {
          <Pre, True> => { 
                    s = \\ num, ns =>ap.s ! AgP3 num cn.gender ++ cn.s ! num ! ns ; 
                    gender = cn.gender; nounCat = cn.nounCat
                  };
          <Post, False> => case ap.isPrep of {
                     False  =>  { 
                            s = \\ num, ns => cn.s  ! num ! ns ++ mkAdjPronIVClitic (AgP3 num cn.gender) 
                                 ++ ap.s ! AgP3 Sg KI_BI; 
                              gender = cn.gender; nounCat = cn.nounCat 
                          };
                     True  =>  { 
                            s = \\ num, ns => (cn.s  ! num ! ns) ++ 
                                mkGenPrepNoIVClitic (AgP3 num cn.gender) ++ ap.s ! AgP3 Sg KI_BI; 
                            gender = cn.gender; nounCat = cn.nounCat
                          }
                  };
          <Pre, False> => { 
                    s = \\ num, ns => mkAdjPronIVClitic (AgP3 num cn.gender) 
                               ++ ap.s ! AgP3 num cn.gender ++ (cn.s ! num ! ns) ; 
                    gender = cn.gender; nounCat = cn.nounCat
                  };
          <Post, True> => { 
                     s = \\ num, ns => (cn.s  ! num ! ns) ++ ap.s ! AgP3 num cn.gender; 
                     gender = cn.gender; nounCat = cn.nounCat
                  }                   

    };        -- big house

    --RelCN   : CN -> RS  -> CN ;   -- house that John bought
    RelCN cn rs = {s=\\n,ns => cn.s !n ! ns ++ rs.s! (RF RObj); gender = cn.gender; nounCat = cn.nounCat};

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
                              <False, True>  => {s = \\_ =>nomS ++ mkPredetPref a ++ predet.s ; agr = a; nounCat = np.nounCat};
                              <True, True >  => {s = \\_ =>nomS ++ mkPredetPref a ++ predet.s  ++
                                                 mkPredetPref a  ++ predet.s2; agr = a; nounCat = np.nounCat};
                              <False,False>  => {s = \\_ =>nomS ++ predet.s ; agr = a; nounCat = np.nounCat};
                              <True,False>   => {s = \\_ =>nomS ++ predet.s ++ predet.s2; agr = a; nounCat = np.nounCat} -- never seen this case              
                          };
      
      --AdvNP   : NP -> Adv -> NP ;    -- Paris today
      AdvNP np adv = {s= \\c => np.s ! c ++ adv.s; agr = np.agr; nounCat = np.nounCat };
      --PPartNP : NP -> V2  -> NP ;    -- the man seen use the Passive form of the verb see. abantu abarebirwe
      PPartNP np v2 = 
        {s= \\c => np.s!c ++ mkSubjClitic np.agr ++ v2.s ++ BIND ++ mkVerbMorphs!VFPastPart!RestOfVerb; agr = np.agr; nounCat = np.nounCat};

      {-What the hell does this mean?-}
      ExtAdvNP np adv = {s= \\c => np.s ! c  ++ embedInCommas adv.s; agr = np.agr; nounCat = np.nounCat}; -- how do I do the adverbial clause?
  --    Determiner: Type = {s:Str; ntype:NounType; num:Number; pos:Position}; -- type for Determier necessary for catCgg.gf
     RelNP np rs ={s = \\c => np.s ! c ++ rs.s! (RF RSubj); agr =np.agr; nounCat = np.nounCat};   
        -- The determiner has a fine-grained structure, in which a 'nucleus'
  -- quantifier and an optional numeral can be discerned.
     --DetQuant    : Quant -> Num -> Det ;  -- these five
     DetQuant  quant num = case quant.isPron of {
                                True => {s=[]; s2 = quant.s2; ntype = Incomplete; num = num.n; pos=Pre; doesAgree = quant.doesAgree};
                                False => {s= quant.s.s ! Nom; s2 =\\_ =>[]; ntype = Complete; num = num.n; pos=Pre; doesAgree = quant.doesAgree} --
          };
                                
     --DetQuantOrd : Quant -> Num -> Ord -> Det ;  -- these five best
     DetQuantOrd quant num ord = {
      s =[]; 
      s2 =\\agr => mkThis!agr ++ quant.s2 ! agr ++ ord.s!agr; 
      ntype = Complete;
      num = num.n;
      pos = Pre;
      doesAgree = True
    };

    NumSg = {s=\\_=>[]; n=Sg}; --Num
    NumPl = {s=\\_=>[]; n=Pl}; --Num
    --NumCard : Card -> Num ; -- one/five [explicit numeral]
    NumCard card = {s =\\agr =>card.s ! agr; n=card.n };
    --Quant = {s : Res.Pronoun; s2 :Res.Agreement => Str; doesAgree : Bool; isPron: Bool} ;
    IndefArt = {s={s=\\_=>[]; third = \\_,_=>[];agr = AgrNo }; s2 = \\_=>[]; doesAgree = False; isPron=False};
    DefArt = {s={s =\\_=>[]; third = \\_,_=>[]; agr = AgrNo }; s2 = \\_=>[]; doesAgree = False; isPron = False}; -- noun with initial vowel

    --NumDigits  : Digits  -> Card ;  -- 51
    NumDigits dig  = {s = dig.s!NCard ; n=dig.n};
    --NumNumeral : Numeral -> Card ;  -- fifty-one
    NumNumeral numeral = {s=numeral.s!NCard; n=numeral.n};
    --OrdDigits  : Digits  -> Ord ;  -- 51st
    OrdDigits dig ={s=dig.s!NOrd ; position = Post};

    --OrdNumeral : Numeral -> Ord ;  -- fifty-first
    OrdNumeral numeral ={s=numeral.s!NOrd; position = Post};
    --OrdSuperl  : A       -> Ord ;  -- warmest
    --Adjective : Type = {s : Str ; position : Position; isProper : Bool; isPrep: Bool};
    OrdSuperl a = {s= \\c =>  (mkAdjPronIVClitic  c) ++ BIND ++ "kukirayo" ++  "obu" ++ BIND ++ a.s; position= a.position} ;--{s= \\c =>  "okukirayo" ++ (mkAdjPronIVClitic  c) ++ a.s ++  ++ BIND ++ "ona"; position = a.position};
  -- One can combine a numeral and a superlative.
  --OrdNumeralSuperl : Numeral -> A -> Ord ; -- third largest
    OrdNumeralSuperl numeral a = {s= \\c =>  numeral.s !NOrd !c ++ "omu" ++ "kukirayo" ++ "obu" ++ BIND ++ a.s; position = a.position};
  -- AdvCN   : CN -> Adv -> CN ;   -- house on the hill
     AdvCN cn adv ={s=\\ntype,num =>cn.s!ntype!num ++ adv.s; gender=cn.gender; nounCat = cn.nounCat};
  -- Pronouns have possessive forms. Genitives of other kinds
  -- of noun phrases are not given here, since they are not possible
  -- in e.g. Romance languages. They can be found in $Extra$ modules.

    --PossPron : Pron -> Quant ;    -- my (house)
    PossPron pron = {s =pron; s2 =\\_=> []; doesAgree = True; isPron = True};

    --3 Conjoinable determiners and ones with adjectives

    --AdjDAP : DAP -> AP -> DAP ;    -- the large (one)
    
    AdjDAP dap ap = 
                { s = dap.s ++ ap.s! AgP3 Sg KI_BI ; -- does no harm since they are all same strings
                  s2 = dap.s2; 
                  ntype = dap.ntype ; 
                  num = dap.num ; 
                  pos = dap.pos; 
                  doesAgree= True 
                };
    
    --DetDAP : Det -> DAP ;          -- this (or that)
    DetDAP det =det;

    --AdNum : AdN -> Card -> Card ;   -- almost 51
    AdNum  adn card = {s = \\a => adn.s ++ card.s ! a; n = card.n};
    --ComplN2 : N2->NP -> CN
    
    ComplN2 n2 np =
      { s = \\n, ns => n2.s ! n ! ns ++ n2.c2 ! mkAgreement n2.gender P1 n ++ np.s !Acc; 
      gender=n2.gender; nounCat = n2.nounCat};
    --ComplN3 : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
    
    ComplN3 n3 np = 
        {s = \\n, ns => n3.s ! n ! ns ++ n3.c2 ! mkAgreement n3.gender P1 n ++ np.s !Acc;
           c2 = n3.c3; 
        gender=n3.gender; nounCat = n3.nounCat}; -- we choose n3 because it is important when using the na conjunction
   
  --2 Apposition

-- This is certainly overgenerating.

  --ApposCN : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np ={s = \\n, ns => cn.s! n!ns ++ np.s !Nom; gender = cn.gender; nounCat = cn.nounCat};
  -- This is different from the partitive, as shown by many languages.

  --CountNP : Det -> NP -> NP ;    -- three of them, some of the boys
  CountNP det np = case det.doesAgree of {
                        True  => {s=\\c=> np.s!c ++ "emye ahari" ++ det.s2 ! np.agr; agr = np.agr; nounCat = np.nounCat};
                        False => {s=\\c=> np.s!c ++ det.s; agr = np.agr; nounCat = np.nounCat} 
                      };

--Determiners can form noun phrases directly.
  --DetNP   : Det -> NP ;  -- these five
  DetNP det = case det.doesAgree of {
                        True  => {s=\\_=> det.s2 ! AgP3 Sg KI_BI; agr = AgP3 Sg KI_BI; nounCat = ComNoun};
                        False => {s=\\c=> det.s; agr = AgP3 Sg KI_BI; nounCat = ComNoun} 
                      };
-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in [Verb Verb.html].
--SentCN  : CN -> SC  -> CN
SentCN cn sc = {s = \\ n, ns => cn.s!n!ns  ++ sc.s; gender = cn.gender; nounCat = cn.nounCat}; 


-- Relational nouns can also be used without their arguments.
-- The semantics is typically derivative of the relational meaning.

    --UseN2   : N2 -> CN ;          -- mother
    UseN2 n2  = {s = n2.s; gender = n2.gender; nounCat = n2.nounCat};
    --Use2N3  : N3 -> N2 ;          -- distance (from this city)
    Use2N3 n3 = {s = n3.s; gender = n3.gender; nounCat = n3.nounCat; c2 = n3.c2};
    --Use3N3  : N3 -> N2 ;          -- distance (to Paris)
    Use3N3 n3 = {s = n3.s; gender = n3.gender; nounCat = n3.nounCat; c2 = n3.c3};

    -- (New 13/3/2013 AR; Structural.possess_Prep and part_Prep should be deprecated in favour of these.)

    --PossNP  : CN -> NP -> CN ;     -- house of Paris, house of mine
    PossNP  cn np ={s =\\n,ns => cn.s! n ! ns ++ mkGenPrepNoIVClitic np.agr ++ np.s ! Nom; gender = cn.gender; nounCat = cn.nounCat};
    --PartNP  : CN -> NP -> CN ;     -- glass of wine
    PartNP cn np ={s =\\n,ns => cn.s! n ! ns ++ mkGenPrepNoIVClitic np.agr ++ np.s ! Nom; gender = cn.gender; nounCat = cn.nounCat};
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
