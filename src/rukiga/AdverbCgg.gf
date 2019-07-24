--# -path=.:../prelude:../abstract:../common

concrete AdverbCgg of Adverb = CatCgg **
  open ResCgg, Prelude in {

lin
  --PrepNP      : Prep -> NP -> Adv -- Verb Phrase modifyingadverb such as everywhere

  --adverb of place pg 118 part (c)
  -- some prepositions can only operate with CN but not PN
  -- how can we distinguish NPs i.e if they are CN or PN?
  -- because aha is used for CN while aha-ri is used for PN Omubazi
  -- nigukora ahari John
  -- The nounPhrase must carry information about its derivation
  
  {-Assumed that a PrepNP is always Acc-}
  PrepNP prep np = {s = prep.s ++ np.s ! Acc;  agr = AgrNo}; -- aha meza 
  
  --PositAdvAdj : A -> Adv ;                 -- warmly
  --"Impossible to implement because each is lexically different word."
  {-
  PositAdvAdj a = case <a.isProper, a.position1> of {
                        <True, True>   =>{ s= a}
                        <False, False> =>
                        <True, False>  =>
                        <_,_>          =>
    };
  -}

{-
abstract Adverb = Cat ** {

  fun

-- The two main ways of forming adverbs are from adjectives and by
-- prepositions from noun phrases.

    PositAdvAdj : A -> Adv ;                 -- warmly
    PrepNP      : Prep -> NP -> Adv ;        -- in the house

-- Comparative adverbs have a noun phrase or a sentence as object of
-- comparison.

    ComparAdvAdj  : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs

-- Adverbs can be modified by 'adadjectives', just like adjectives.

    AdAdv  : AdA -> Adv -> Adv ;             -- very quickly

-- Like adverbs, adadjectives can be produced by adjectives.

    PositAdAAdj : A -> AdA ;                 -- extremely

-- Subordinate clauses can function as adverbs.

    SubjS  : Subj -> S -> Adv ;              -- when she sleeps

-- Comparison adverbs also work as numeral adverbs.

    AdnCAdv : CAdv -> AdN ;                  -- less (than five)

-}

}
