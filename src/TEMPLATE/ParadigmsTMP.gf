resource ParadigmsTMP = open CatTMP, ResTMP, NounTMP, Prelude in {

oper

--2 Parameters
--
-- To abstract over number, valency and (some) case names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResSom$.

  Prep : Type ;
  noPrep : Prep ;

  -- Add more overload instances if needed for all categories!

--2 Nouns

  mkN : overload {
    mkN : Str -> N ; -- Predictable nouns
  } ;

  mkPN : overload {
    mkPN : Str -> PN ; -- Proper nouns
  } ;

--2 Adjectives

  mkA : overload {
    mkA : Str -> A ; -- Predictable adjective
  } ;

  mkA2 : overload {
    mkA2 : Str -> A2 ; -- Predictable A2, no preposition
    mkA2 : A -> Prep -> A2 ; -- A2 made from A and Prep
  } ;

--2 Verbs

  -- Verbs
  mkV : overload {
    mkV : Str -> V ; -- Predictable verb
  } ;


  mkV2 : overload {
    mkV2 : Str -> V2 ;  -- Predictable transitive verb
    mkV2 : V -> Prep  -> V2 ;   -- V2 made from V and Prep
    } ;

  mkV3 : overload {
    mkV3 : V -> V3 ; -- No prepositions
    mkV3 : V -> Prep -> Prep -> V3 ; -- Prepositions for direct and indirect objects given
    } ;

  mkVV : overload {
    mkVV : V -> VV ;
   } ;

  mkVA : overload {
    mkVA : V -> VA ;
   } ;

  mkVQ : overload {
    mkVQ : V -> VQ ;
   } ;

  mkVS : overload {
    mkV : V -> VS ;
  } ;

  -- Etc. do the same for other V subcats (V2A, V2V, V2S, …)


  -----

--2 Structural categories

  -- If prepositions take case, add that as argument to mkPrep
  mkPrep : overload {
    mkPrep : Str -> Prep ;
    } ;

  mkConj : overload {
    mkConj : (and : Str) -> Conj ; -- (coffee) and (tea)
    mkConj : (either : Str) -> (or : Str) -> Conj ; -- either (coffee) or (tea)
    } ;

  mkSubj : overload {
    mkSubj : Str -> Subj ;
    } ;

  mkAdv : overload {
    mkAdv : Str -> Adv ;
    } ;

  mkAdV : overload {
    mkAdV : Str -> AdV ;
    } ;

  mkAdA : overload {
    mkAdA : Str -> AdA ;
    } ;


--.
-------------------------------------------------------------------------------
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  Prep = CatTMP.Prep ;
  noPrep = mkPrep [] ;

  -- Add more overload instances if needed for all categories!

  -- For explanation of `lin N`, see
  -- https://inariksit.github.io/gf/2018/05/25/subtyping-gf.html#lock-fields

  mkN = overload {
    mkN : Str -> N = \s -> lin N (ResTMP.mkNoun s) ;
    -- TODO: more overload instances
  } ;

{-
  mkPN = overload {
    mkPN : Str -> PN = …
  } ;

--2 Adjectives

  mkA = overload {
    mkA : Str -> A = \s -> …
  } ;

  mkA2 = overload {
    mkA2 : Str -> A2 = \s -> …
    mkA2 : A -> Prep -> A2 = \s -> …
  } ;

--2 Verbs
-}
  -- Verbs
  mkV = overload {
    mkV : Str -> V = \s -> lin V (mkVerb s) ;
  } ;

{-

  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> …
    mkV2 : V -> Prep -> V2 = \s -> …
    } ;

  mkV3 = overload {
    mkV3 : V -> V3 = \s -> …
    mkV3 : V -> Prep -> Prep -> V3 = \s -> …
    } ;

  mkVV = overload {
    mkVV : V -> VV = \s -> …
   } ;

  mkVA = overload {
    mkVA : V -> VA = \s -> …
   } ;

  mkVQ = overload {
    mkVQ : V -> VQ = \s -> …
   } ;


  mkVS = overload {
    mkV : V -> VS = \s -> …
  } ;

  -- Etc. do the same for other V subcats (V2A, V2V, V2S, …)


  -----
-}

  -- If prepositions take case, add that as argument to mkPrep
  mkPrep = overload {
    mkPrep : Str -> Prep = \s -> lin Prep {s = s} ;
    } ;
{-
  mkConj = overload {
    mkConj : (and : Str) -> Conj = \s -> …
    mkConj : (either : Str) -> (or : Str) -> Conj = \s -> …
    } ;

  mkSubj = overload {
    mkSubj : Str -> Subj = \s -> …
    } ;

  mkAdv = overload {
    mkAdv : Str -> Adv = \s -> …
    } ;

  mkAdV = overload {
    mkAdV : Str -> AdV = \s -> …
    } ;

  mkAdA = overload {
    mkAdA : Str -> AdA = \s -> …
    } ;

-}
--------------------------------------------------------------------------------

}
