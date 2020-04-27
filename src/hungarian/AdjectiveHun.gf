concrete AdjectiveHun of Adjective = CatHun ** open ResHun, Prelude in {

  flags optimize=all_subs ;

  lin

  -- : A  -> AP ;
  PositA a = emptyAP ** {
    s = \\n,c =>
      let adj : Noun = (a ** {s = a.s ! Posit}) in
      caseFromStem glue adj c n ;
    } ;

  -- : A  -> NP -> AP ;
  ComparA a np = UseComparA a ** {
    compl = \\n => applyAdp (caseAdp Ade) np ;
    -- compl = applyAdp (prepos Nom "mint") np ;
    } ;

  -- : A2 -> NP -> AP ;  -- married to her
  ComplA2 a2 np = let ap : AP = PositA a2 in ap ** {
    s = case a2.isPost of {False => ap.s ; _ => \\_,_ => []} ;
    compl = \\n => applyAdp a2.c2 np
                ++ case a2.isPost of {
                     True => ap.s ! n ! Nom ;
                     False => [] } ;
    } ;

  -- : A2 -> AP ;        -- married to itself
  -- ReflA2 a2 = a2 ** { } ;

  -- : A2 -> AP ;        -- married
  UseA2 = PositA ;

  -- : A  -> AP ;     -- warmer
  UseComparA a = emptyAP ** {
    s = \\n,c =>
    let adj : Noun = (a ** {s = a.s ! Compar}) in
    caseFromStem glue adj c n ;
    } ;

  -- : CAdv -> AP -> NP -> AP ; -- as cool as John
  CAdvAP adv ap np = ap ** {
    s = \\n,c => adv.s ++ ap.s ! n ! c ;
    compl = \\n => ap.compl ! n ++ adv.p ++ applyAdp (caseAdp Nom) np ;
    } ;

-- The superlative use is covered in $Ord$.

  -- : Ord -> AP ;       -- warmest
  AdjOrd ord = emptyAP ** ord ;

-- Sentence and question complements defined for all adjectival
-- phrases, although the semantics is only clear for some adjectives.

  -- : AP -> SC -> AP ;  -- good that she is here
  -- SentAP ap sc = ap ** {} ;

-- An adjectival phrase can be modified by an *adadjective*, such as "very".

  -- : AdA -> AP -> AP ;
  AdAP ada ap = ap ** {
    s = \\n,c => ada.s ++ ap.s ! n ! c ;
    } ;


-- It can also be postmodified by an adverb, typically a prepositional phrase.

  -- : AP -> Adv -> AP ; -- warm by nature
  AdvAP ap adv = ap ** {
    s = \\n,c => ap.s ! n ! c ++ adv.s ;
    } ;

}
