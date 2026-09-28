concrete AdjectiveGla of Adjective = CatGla ** open ResGla, Prelude in {

  flags optimize=all_subs ;

  lin

  -- : AP -> Adv -> AP ; -- warm by nature
  AdvAP  ap adv = ap ** {
    s   = \\af => ap.s ! af ++ adv.s ;
    voc = \\g => ap.voc ! g ++ adv.s
  } ;

  -- : A  -> AP ;
  PositA a = a ;

  -- : A  -> NP -> AP ;
  ComparA a np = addAP a ("na" ++ linNP np) ;

  -- : A2 -> NP -> AP ;  -- married to her
  ComplA2 a2 np = addAP a2 (prepNP a2.c2 np) ;

  -- : A2 -> AP ;        -- married to itself
  ReflA2 a2 = a2 ;

  -- : A2 -> AP ;        -- married
  UseA2 a2 = a2 ;

  -- : A  -> AP ;     -- warmer
  UseComparA a = {s = \\_ => a.compar ; voc = \\_ => a.compar} ;


  -- : CAdv -> AP -> NP -> AP ; -- as cool as John
  CAdvAP adv ap np = addAP ap (adv.p ++ linNP np) ;

-- The superlative use is covered in $Ord$.

  -- : Ord -> AP ;       -- warmest
  -- AdjOrd ord = ord ** {
  --   compar = []
  --   } ;
  -- AdjOrd : Ord -> AP  =
  -- AdjOrd ord = ord ;

-- Sentence and question complements defined for all adjectival
-- phrases, although the semantics is only clear for some adjectives.

  -- : AP -> SC -> AP ;  -- good that she is here
  SentAP ap sc = addAP ap sc.s ;

-- An adjectival phrase can be modified by an *adadjective*, such as "very".

  -- : AdA -> AP -> AP ;
  AdAP ada ap = {
    s = \\af => ada.s ++ ap.s ! af ;
    voc = \\g => ada.s ++ ap.voc ! g
    } ;


-- It can also be postmodified by an adverb, typically a prepositional phrase.

oper
  addAP : LinAP -> Str -> LinAP = \ap,x -> {
    s = \\af => ap.s ! af ++ x ;
    voc = \\g => ap.voc ! g ++ x
    } ;

}
