resource ParadigmsMay = open CatMay, ResMay, ParamMay, NounMay, Prelude in {

oper

--2 Parameters
--
-- To abstract over number, valency and (some) case names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResSom$.


--2 Nouns

  mkN : overload {
    mkN : (noun : Str) -> N ; -- Predictable nouns
  } ;

--2 Adjectives

  mkA : overload {
    mkA : (adj : Str) -> A ;
  } ;

  -- mkA2 : Str -> Prep -> A2 ;

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

  -- mkV3 : overload {
  --   } ;

  -- mkVV : overload {
  --  } ;

  --
  -- mkVA : Str -> VA
  --   = \s -> lin VA (regV s) ;
  -- mkVQ : Str -> VQ
  --   = \s -> lin VQ (regV s) ;
  -- mkVS : Str -> VS
  --   = \s -> lin VS (regV s) ;
  --
  -- mkV2A : Str -> V2A
  --   = \s -> lin V2A (regV s ** {c2 = noPrep}) ;
  -- mkV2V : Str -> V2V
  --   = \s -> lin V2V (regV s ** {c2 = noPrep}) ;
  -- mkV2Q : Str -> V2Q
  --   = \s -> lin V2Q (regV s ** {c2 = noPrep}) ;

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
    } ;


  mkN2 = overload {
    mkN2 : Str -> N2 = \s -> lin N2 (mkNoun s) ;
    mkN2 : N   -> N2 = \n -> lin N2 n ;
   } ;

  -- mkPN = overload {
  --   } ;

  mkA = overload {
    mkA : (adj : Str) -> A = \s -> lin A (mkAdj s) ;
    } ;

  mkV = overload {
    mkV : Str           -> V = \v   -> lin V (mkVerb v Meng) ;
    mkV : Str -> Prefix -> V = \v,p -> lin V (mkVerb v p)
  } ;

  mkV2 = overload {
    mkV2 : Str       -> V2 = \v2  -> lin V2 ((mkVerb v2 Meng) ** {c2 = emptyPrep}) ;
    mkV2 : V -> Prep -> V2 = \v,p -> lin V2 (v ** {c2 = p})
    } ;
  --
  -- mkV3 = overload {
  --   } ;
  --
  -- mkVV = overload {
  --  } ;

--------------------------------------------------------------------------------

}
