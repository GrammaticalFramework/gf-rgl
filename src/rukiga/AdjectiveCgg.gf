--# -path=.:../prelude:../abstract:../common

concrete AdjectiveCgg of Adjective = CatCgg **
  open ResCgg, Prelude, ParamX in {

lin

    PositA a = {s=\\_=> a.s; position1= a.position1; isProper = a.isProper; isPrep = a.isPrep};

    -- The superlative use is covered in $Ord$.

    --AdjOrd  : Ord -> AP ;       -- warmest
    AdjOrd ord = {s= \\agr => ord.s!agr ; position1= ord.position1; isProper = False; isPrep = False};
    -- UseComparA : A  -> AP ;     -- warmer
    UseComparA a ={s =\\_ => a.s ++ BIND ++ "ho" ++ "kukira"; position1= a.position1; isProper = a.isProper; isPrep = a.isPrep};

    -- An adjectival phrase can be modified by an *adadjective*, such as "very".
    {-NOTE: AdA is an adjective modifying adverb-}
    --AdAP    : AdA -> AP -> AP ; -- very warm
    
    AdAP ada ap = case ada.position1 of {
                        Pre  => {s = \\agr => ada.s ++ ap.s!agr ; position1= ap.position1; isProper = ap.isProper; isPrep = ap.isPrep};
                        Post => {s = \\agr => ap.s ! agr ++ ada.s; position1= ap.position1; isProper = ap.isProper; isPrep = ap.isPrep}
                        
                    };
    {-                      
abstract Adjective = Cat ** {

  fun

-- The principal ways of forming an adjectival phrase are
-- positive, comparative, relational, reflexive-relational, and
-- elliptic-relational.

    PositA  : A  -> AP ;        -- warm
    ComparA : A  -> NP -> AP ;  -- warmer than I
    ComplA2 : A2 -> NP -> AP ;  -- married to her
    ReflA2  : A2 -> AP ;        -- married to itself
    UseA2   : A2 -> AP ;        -- married
    UseComparA : A  -> AP ;     -- warmer
    CAdvAP  : CAdv -> AP -> NP -> AP ; -- as cool as John

-- The superlative use is covered in $Ord$.

    AdjOrd  : Ord -> AP ;       -- warmest

-- Sentence and question complements defined for all adjectival
-- phrases, although the semantics is only clear for some adjectives.
 
    SentAP  : AP -> SC -> AP ;  -- good that she is here

-- An adjectival phrase can be modified by an *adadjective*, such as "very".

    AdAP    : AdA -> AP -> AP ; -- very warm

-- It can also be postmodified by an adverb, typically a prepositional phrase.

    AdvAP   : AP -> Adv -> AP ; -- warm by nature

-- The formation of adverbs from adjectives (e.g. "quickly") is covered
-- in [Adverb Adverb.html]; the same concerns adadjectives (e.g. "extremely").

-}

}