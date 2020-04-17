concrete AdjectiveHun of Adjective = CatHun ** open ResHun, Prelude in {

  flags optimize=all_subs ;

  lin

  -- : A  -> AP ;
  PositA a = emptyAP ** {
    s = a.s ! Posit
    } ;

  -- : A  -> NP -> AP ;
  ComparA a np = emptyAP ** {
    s = a.s ! Compar ;
    compar = np.s ! Ade ;
    } ;

  -- : A2 -> NP -> AP ;  -- married to her
  ComplA2 a2 np = emptyAP ** {
    s = a2.s ! Posit ;
    compar = np.s ! a2.c2.c ++ a2.c2.s
    } ;

  -- : A2 -> AP ;        -- married to itself
  -- ReflA2 a2 = a2 ** { } ;

  -- : A2 -> AP ;        -- married
  UseA2 = PositA ;

  -- : A  -> AP ;     -- warmer
  UseComparA a = emptyAP ** {
    s = a.s ! Compar ;
    } ;

  -- : CAdv -> AP -> NP -> AP ; -- as cool as John
  CAdvAP adv ap np = ap ** {
    s = \\n => adv.s ++ ap.s ! n ;
    compar = ap.compar ++ adv.p ++ np.s ! Nom
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
    s = \\af => ada.s ++ ap.s ! af ;
    } ;


-- It can also be postmodified by an adverb, typically a prepositional phrase.

  -- : AP -> Adv -> AP ; -- warm by nature
  AdvAP ap adv = ap ** {
    s = \\af => ap.s ! af ++ adv.s ;
    } ;

}
