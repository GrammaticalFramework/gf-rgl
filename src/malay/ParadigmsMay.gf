resource ParadigmsMay = open CatMay, ResMay, ParamMay, NounMay, Prelude in {

oper

--2 Parameters
--
-- To abstract over number, valency and (some) case names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResSom$.

  noPrep : Prep = mkPrep "" ;

--2 Nouns

  mkN : overload {
    mkN : (noun : Str) -> N ; -- Predictable nouns
  } ;

  mkPN : overload {
    mkPN : Str -> PN ; -- Proper nouns
  } ;

--2 Adjectives

  mkA : overload {
    mkA : (adj : Str) -> A ;
  } ;

  mkA2 : overload {
    mkA2 : (adj : Str) -> Prep -> A2 ;
  } ;

--2 Verbs

  -- Verbs
  mkV : overload {
    mkV : (root : Str) -> V ; -- Verb that takes meng as a active prefix
    mkV : (root : Str) -> Prefix -> V  -- Root and prefix
  } ;


  mkV2 : overload {
    mkV2 : (root : Str) -> V2 ; -- The prefix is meng and no preposition
    mkV2 : V -> Prep  -> V2 ;   -- V and Prep
    } ;

  mkV3 : overload {
    mkV3 : V -> V3 ; -- No prepositions
    mkV3 : V -> Prep -> Prep -> V3 ; -- Prepositions for direct and indirect objects given
    } ;

  mkVV : overload {
    mkVV : Str -> VV ;
   } ;

  mkVS : overload {
    mkVS : V -> VS ;
  } ;

  mkVA : overload {
    mkVA : V -> VA ;
  } ;

  mkV2V : overload {
    mkV2V : Str -> V2V ;
    mkV2V : V -> Prep -> Prep -> V2V ;
  } ;

  mkV2S : overload {
    mkV2S : Str -> V2S ;
    mkV2S : V -> Prep -> V2S ;
  } ;

  -- mkVQ : Str -> VQ
  --   = \s -> lin VQ (regV s) ;

  mkV2A : overload {
    mkV2A : Str -> V2A ;
  } ;
  --
  -- mkV2A : Str -> V2A
  --   = \s -> lin V2A (regV s ** {c2 = noPrep}) ;
  mkV2Q : overload {
    mkV2Q : Str -> V2Q ;
    mkV2Q : V -> Prep -> V2Q ;
  } ;

  -----

--2 Structural categories

  -- mkPrep = overload {
  --   } ;

  -- mkConj : (_,_ : Str) -> Number -> Conj = \s1,s2,num ->
  --   lin Conj { s = s1 ; s2 = s2 } ;

  -- mkSubj : Str -> Bool -> Subj = \s,b ->
  --   lin Subj { } ;

  mkAdv : Str -> Adv = \s -> lin Adv {s = s} ;

  mkAdV : Str -> AdV = \s -> lin AdV {s = s} ;

  mkAdA : Str -> AdA = \s -> lin AdA {s = s} ;


--.
-------------------------------------------------------------------------------
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  mkN = overload {
    mkN : Str -> N = \s -> lin N (mkNoun s) ;
    mkN : Str -> Animacy -> N = \s,a -> lin N (mkNoun s) ;
    } ;


  mkN2 = overload {
    mkN2 : Str -> N2 = \s -> lin N2 (mkNoun s ** {c2 = dirPrep}) ;
    mkN2 : N   -> N2 = \n -> lin N2 (n ** {c2 = dirPrep}) ;
   } ;

  mkN3 = overload {
    mkN3 : Str -> N3 = \s -> lin N3 (mkNoun s ** {c2,c3 = dirPrep}) ;
    mkN3 : N   -> N3 = \n -> lin N3 (n ** {c2,c3 = dirPrep}) ;
    mkN3 : N   -> Prep -> Prep -> N3 = \n,c2,c3 -> lin N3 (n ** {c2,c3 = dirPrep}) ;
   } ;

  mkPN = overload {
    mkPN : Str -> PN = \s -> lin PN {s = \\_ => s} ;
    } ;

  mkA = overload {
    mkA : (adj : Str) -> A = \s -> lin A (mkAdj s) ;
    } ;

  mkA2 = overload {
    mkA2 : (adj : Str) -> A2 = \s -> lin A2 ((mkAdj s) ** {c2 = emptyPrep}) ;
    mkA2 : A -> A2 = \a -> lin A2 (a ** {c2 = emptyPrep}) ;
    mkA2 : A -> Prep -> A2 = \a,p -> lin A2 (a ** {c2 = p}) ;
    mkA2 : (adj : Str) -> Prep -> A2 = \a,p -> lin A2 ((mkAdj a) ** {c2 = p}) ;
    } ;

  mkV = overload {
    mkV : Str           -> V = \v   -> lin V (regVerb v Ber) ;
    mkV : Str -> Prefix -> V = \v,p -> lin V (regVerb v p) ;
    } ;

  prefixV : V -> V = \v -> v ** {
    s = table {
      Root => v.s ! Active ;
      x => v.s ! x -- TODO: how does it work with passives?
      }
    } ;

  mkV2 = overload {
    mkV2 : Str       -> V2 = \v2  -> lin V2 (mkVerb2 (regVerb v2 Meng) dirPrep) ;
    mkV2 : V -> Prep -> V2 = \v,p -> lin V2 (mkVerb2 v p)
    } ;

  mkV3 = overload {
    mkV3 : V -> V3 = \v ->
      lin V3 (mkVerb3 v dirPrep dirPrep) ;
    mkV3 : V -> (p,q : Prep) -> V3 = \v,p,q ->
      lin V3 (mkVerb3 v p q)
    } ;

  mkV4 = overload {
    mkV4 : Str       -> Str -> V2 = \v2,str  ->
      lin V2 (mkVerb4 (regVerb v2 Meng) dirPrep str) ;
    mkV4 : V -> Prep -> Str -> V2 = \v,p,str -> lin V2 (mkVerb4 v p str)
    } ;

  mkVV = overload {
    mkVV : Str -> VV = \vv -> lin VV (ss vv)
    } ;

  mkVS = overload {
    mkVS : V -> VS = \v -> lin VS (v)
    } ;

  mkVA = overload {
    mkVA : V -> VA = \v -> lin VA (v)
    } ;

  mkV2V = overload {
    mkV2V : Str -> V2V = \v -> lin V2V (mkVerb2 (regVerb v Meng) dirPrep) ;
    mkV2V : V -> Prep -> Prep -> V2V = \v,p1,p2 -> lin V2V (mkVerb3 v p1 p2)
  } ;

  mkV2S = overload {
    mkV2S : Str -> V2S = \v -> lin V2S (mkVerb2 (regVerb v Meng) dirPrep) ;
    mkV2S : V -> Prep -> V2S = \v,p -> lin V22 (mkVerb2 v p)
  } ;

--   lin like_V2 = let like' : V2 = mkV2 "suka" in like' ** {
--   s = table {Passive => "disukai" ; _ => "suka"} ;
-- } ;
--------------------------------------------------------------------------------

}
