--# -path=.:../abstract:../../prelude:../common

resource ParadigmsGus = open 
  (Predef=Predef), 
  Prelude, 
  MorphoGus,
   ResGus,
  CatGus
  in {

oper
  Gender : Type ; 
   omo_aba  : Gender ;--%
  omo_eme   : Gender ;
  e_ci      : Gender ;
  eri_ama   : Gender ;
  ege_ebi   : Gender ;
  oro_ci    : Gender ;
  aka_ebi   : Gender ; --%
  abo_ama   : Gender ;
  oko_ama   : Gender ;
  aa        : Gender ;

-- To abstract over number names, we define the following.

  Number : Type ; 

  singular : Number ; 
  plural   : Number ;

-- To abstract over case names, we define the following.

  Case : Type ; --%

  nominative : Case ; --%
  locative   : Case ; --%

  npNumber : NP -> Number ; -- exctract the number of a noun phrase


--2 Nouns

-- Nouns are constructed by the function $mkN$, which takes a varying
-- number of arguments.

  mkN : overload {
    mkN : (flash : Str) -> Gender -> N ;  --regular plural 
    mkN : (man,men : Str) ->Gender -> N ; -- irregular plural
    mkN : Str  -> N -> N -- e.g. baby + boom compound noun
  } ;

--3 Relational nouns 
 mkN2 : overload {
      mkN2 : N -> Prep -> N2 ;
      mkN2 : N -> Str -> N2 ;
      mkN2 : N -> N2 ; 
      mkN2 : N -> (Gender => Number => Str)-> N2 ;
    } ;
 
oper dfltGender : Gender = G1 ; 
       dfltNumber : Number = Sg ;
 
-- Three-place relational nouns ("the connection from x to y") need two prepositions.

  mkN3 : N -> Prep -> Prep -> N3 ; -- e.g. connection from x to y



--3 Proper names and noun phrases
--
-- Proper names, with a regular genitive, are formed from strings.

  mkPN : overload {

    mkPN : Str ->Gender -> PN ;

-- Sometimes a common noun can be reused as a proper name, e.g. "Bank"

    mkPN : N -> PN --%
  } ;

--3 Determiners and quantifiers

  mkOrd : Str -> Ord ; --%

--2 Adjectives

  mkA : overload {
    mkA : (happy : Str) -> A ; 
    --mkA : (happy : Str) -> A ;-- regular adj, 
    mkA : (fat,fatter : Str) -> A ; -- irreg. 
    } ;


--3 Two-place adjectives

  mkA2 : overload {
    mkA2 : A -> Prep -> A2 ; -- absent from 
    mkA2 : A -> Str -> A2 ; -- absent from --%
    mkA2 : Str -> Prep -> A2 ; -- absent from --%
    mkA2 : Str -> Str -> A2 -- absent from --%

  } ;


--2 Adverbs

-- Adverbs are not inflected. Most lexical ones have position
-- after the verb. Some can be preverbal (e.g. "always").

  mkAdv : Str -> Adv ; -- e.g. today
  mkAdV : Str -> AdV ; -- e.g. always

-- Adverbs modifying adjectives and sentences can also be formed.

  mkAdA : Str -> AdA ; -- e.g. quite

-- Adverbs modifying numerals

  mkAdN : Str -> AdN ; -- e.g. approximately

--2 Prepositions

  mkPrep : overload {
   mkPrep : Str -> Prep  ;
  -- mkPrep : Str -> Str -> Prep ;
    mkPrep : (Gender => Number => Str) -> Prep ;
  } ;
  
  noPrep : Prep ;  -- no preposition

-- (These two functions are synonyms.)

--2 Conjunctions
--

  mkConj : overload {
    mkConj : Str -> Conj ;                  -- and (plural agreement) --%
    mkConj : Str -> Number -> Conj ;        -- or (agrement number given as argument) --%
    mkConj : Str -> Str -> Conj ;           -- both ... and (plural) --%
    mkConj : Str -> Str -> Number -> Conj ; -- either ... or (agrement number given as argument) --%
  } ;

--2 Verbs
--

-- Verbs are constructed by the function $mkV$, which takes a varying
-- number of arguments.

  mkV : overload {
    mkV : (cry : Str) -> V ; -- regular, incl. cry-cries, kiss-kisses etc
    mkV : Str -> V -> V ;  -- fix compound, e.g. under+take
  };




--3 Two-place verbs
--
-- Two-place verbs need a preposition, except the special case with direct object.
-- (transitive verbs). Notice that a particle comes from the $V$.-}

  mkV2 : overload {
    mkV2  : Str -> V2 ;       -- kill --%
    mkV2  : V -> V2 ;         -- transitive, e.g. hit
    mkV2  : V -> Prep -> V2 ; -- with preposiiton, e.g. believe in
    mkV2  : V -> Str -> V2 ;  -- believe in --%
    mkV2  : Str -> Prep -> V2 ; -- believe in --%
    mkV2  : Str -> Str -> V2  -- believe in --%
  };

--3 Three-place verbs
--
-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 : overload {
    mkV3  : V -> V3 ;                   -- ditransitive, e.g. give,_,_
    mkV3  : V -> Prep -> Prep -> V3 ;   -- two prepositions, e.g. speak, with, about
    mkV3  : V -> Prep -> V3 ;           -- give,_,to --%
    mkV3  : V -> Str -> V3 ;            -- give,_,to --%
    mkV3  : Str -> Str -> V3 ;          -- give,_,to --%
    mkV3  : Str -> V3 ;                 -- give,_,_ --%
  };


--2 Other categories

mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; --%
mkInterj : Str -> Interj
  = \s -> lin Interj (ss s) ;

--.
--2 Definitions of paradigms
--
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  Gender =  MorphoGus.Gender ; 
  Number =  MorphoGus.Number ;
  Case   =  MorphoGus.NPCase ;
   omo_aba   = G1 ;
  omo_eme   = G2 ;
  e_ci      = G3 ;
  eri_ama   = G4 ;
  ege_ebi   = G5 ;
  oro_ci    = G6 ;
  aka_ebi   = G7 ; 
  abo_ama   = G8 ;
  oko_ama   = G9 ;
  aa        = G10 ;
  singular = Sg ;
  plural = Pl ;
  nominative = npNom ;
  locative = npLoc ;

  npNumber np = (agrFeatures np.a).n ;



  regN = MorphoGus.regN ; 
  iregN = MorphoGus.iregN ;

 
  compoundN s n = lin N {s = \\x,y => s ++ n.s ! x ! y ; g=n.g} ;

  mkPN = overload {
   mkPN : Str -> Gender -> PN = regPN;
   mkPN : N -> PN = nounPN
  } ;


mkN2 = overload {
      mkN2 : N -> Prep -> N2 = prepN2 ;
      mkN2 : N -> Str -> N2 = \n,s -> prepN2 n (mkPrep s);
      mkN2 : N -> N2         = \n -> prepN2 n (mkPrep  mkPrepof ) ;
      mkN2 : N -> (Number =>Gender =>  Str)-> N2= \n,s -> prepN2 n (mkPrep mkPrepof) ;
    } ;
 
  prepN2 = \n,p -> lin N2 (n ** {c2 = p}) ; 
  regN2 = \n -> (prepN2 n (mkPrep mkPrepof )) ; 
  mkN3 = \n,p,q -> lin N3 (n ** {c2 = p ; c3 = q}) ;  
  mkPrepof : Number => Gender => Str = 
    table Number {  Sg => table {
                    G1| G2  => "bwo" ;
                    G3 => "ya";
                    G4 => "ria";
                    G5 => "kia"; --
                    G6 => "rwa";
                    G7 => "ka";
                    G8 => "bwa";
                    G9 => "kwa";
                    G10 => "a"
                   }; 
                                 
                   Pl => table { G1 => "ba" ;
                    G2  => "ya" ;
                    G3|G6  => "cia";
                    G4  |G8|G9|G10 => "a";
                    G5  => "bi"; --
                    G7 => "bia"} } ;
                       
 


  cnN2 : CN -> Prep -> N2 ;
  cnN3 : CN -> Prep -> Prep -> N3 ;

-- This is obsolete.
  cnN2 = \n,p -> lin N2 (n ** {c2 = p}) ;
  cnN3 = \n,p,q -> lin N3 (n ** {c2 = p ; c3 = q}) ;

  
    regPN n g = lin PN {s = table {Loc => ""  ; _ => n} ; g = g} ;
    
   
  nounPN n = lin PN {s = n.s ! singular ; g = n.g} ;

    mkOrd : Str -> Ord = \x -> lin Ord { s =\\g => x};

 
  prepA2 a p = lin A2 (a ** {c2 = p.s!Sg!G1}) ;

  mkAdv x = lin Adv (ss x) ;
  mkAdV x = lin AdV (ss x) ;
  mkAdA x = lin AdA (ss x) ;
  mkAdN x = lin AdN (ss x) ;

  mkPrep = overload {
    mkPrep : Str -> Prep = \str -> lin Prep {s = \\n,g => str } ;
    mkPrep : (Number => Gender =>  Str) -> Prep = \t ->lin Prep {s = t} ;
  } ;

   noPrep = mkPrep []  ;
  {-} mkPrep : Str -> Str -> Prep = \p,q -> lin Prep 
        {s = table{Sg => table{G1 => p; _=> "" }; 
              Pl => table{G1 => q; _=> ""}}} ; 
 prepV2 : V -> Prep -> V2 ;
  prepV2 = \v,p -> lin V2 (v ** {c2 = p.s!Sg!G1}) ;
  dirV2 : V -> V2 = \v -> prepV2 v noPrep ;

  prepPrepV3 v p q = lin V3 (v ** {c2 = p ; c3 = q}) ; 
   dirV3 v p = prepPrepV3 v noPrep p ;
  dirdirV3 v = dirV3 v noPrep ;

 
 mkA2V : A -> Prep -> Prep -> A2V;
      A2S, A2V : Type = A2 ;
      mkA2V v p q = mkA2 v p ** {s3 = q.p2 ; c3 = q.p1 ; lock_A2V = <>} ;
 

      mkAV  v  = v ** { lock_AV = <>} ;
      mkAV  : A ->  AV ;
      AS, AV : Type = A ;

      mkAS  : A -> AS ; 
      mkAS  v = v ** {lock_AS = <>} ;

      mkVS  : V -> VS ;
      mkVS  v = v ** { lock_VS = <>} ;

      mkVQ  : V -> VQ ;
      mkVQ  v = v ** {lock_VQ = <>} ;

 
     -- mkVV : V -> VV ;
    --  mkVV  v = v ** { lock_VV = <>} ;

      mkVA  : V -> VA ;
      mkVA  v = v ** {lock_VA = <>} ;

       mkV2V : V -> Prep -> Prep -> V2V ;
         mkV2V v p q = prepPrepV3 v p q ** {lock_V2V = <>} ;

         mkV2S : V -> Prep -> V2S ; 
         mkV2S v p = prepV2 v p ** { lock_V2S = <>} ;

         mkV2Q : V -> Prep -> V2Q ;
         mkV2Q v p = prepV2 v p ** {lock_V2Q = <>} ;
          
         mkV2A : V -> Prep -> Prep -> V2A ;
         mkV2A v p q = prepPrepV3 v p q ** {lock_V2A = <>} ;


        mkV0  : V -> V0 ;
        V0 : Type ;
        V0 : Type = V;
        mkV0  v = v ** {lock_V0 = <>} ;
-}
-- pre-overload API and overload definitions

 -- regN : Str ->Gender -> N ;
  --iregN : (man,men : Str) ->Gender -> N ;
  compoundN : Str  -> N -> N ;

  mkN = overload {
    mkN : Str ->Gender -> N =  \n, g -> lin N (regN n g );
    mkN : (man,men : Str) ->Gender -> N = \s,p,g -> lin N ( iregN s p g) ;
        } ;
--mkN : Str ->  N -> N = compoundN taken from mkN can be added later if need be
-- Relational nouns ("daughter of x") need a preposition. 

  prepN2 : N -> Prep -> N2 ;

-- The most common preposition is "of", and the following is a
-- shortcut for regular relational nouns with "of".

  regN2 : N -> N2 ;
  


 -- iregA : (free,freely : Str) -> A ;
  regA : Str -> A = \s -> lin A (MorphoGus.regA s) ;
 -- pregA : Str -> A = \s -> lin A (MorphoGus.pregA s) ;
   iregA : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoGus.iregA a b);
  mkA = overload {
    mkA : Str -> A = \a -> lin A (regA a);-- |lin A (pregA a) ;
    mkA : (fat,fatter : Str) -> A =\a,b -> lin A (iregA a b);
    } ;

  prepA2 : A -> Prep -> A2 ;

  mkA2 = overload {
    mkA2 : A -> Prep -> A2   = prepA2 ;
    mkA2 : A -> Str -> A2    = \a,p -> prepA2 a (mkPrep p) ;
    mkA2 : Str -> Prep -> A2 = \a,p -> prepA2 (regA a) p;
    mkA2 : Str -> Str -> A2  = \a,p -> prepA2 (regA a) (mkPrep p);
  } ;

  {-}
  regV=MorphoGus.regV ;

  mkV = overload {
    mkV :  Str -> V =\v ->lin V(regV v) ;
   mkV : Str -> V -> V = prefixV
  };

 
  prefixV : Str -> V -> V = \p,v -> lin V { s = \\b,vform => p + v.s! b ! vform } ;
  mkV2 = overload {
    mkV2  : V -> V2 = dirV2 ;
    mkV2  : Str -> V2 = \s -> dirV2 (regV s) ;
    mkV2  : V -> Prep -> V2 = prepV2; 
    mkV2  : V -> Str -> V2 = \v,p -> prepV2 v (mkPrep p) ;
    mkV2  : Str -> Prep -> V2 = \v,p -> prepV2 (regV v) p ;
    mkV2  : Str -> Str -> V2 = \v,p -> prepV2 (regV v) (mkPrep p)
  }; 

  prepPrepV3 : V -> Prep -> Prep -> V3 ;
  dirV3 : V -> Prep -> V3 ;
  dirdirV3 : V -> V3 ;

  mkV3 = overload {
    mkV3 : V -> Prep -> Prep -> V3 = prepPrepV3 ;
    mkV3 : V -> Prep -> V3 = dirV3 ;
    mkV3 : V -> Str -> V3 = \v,s -> dirV3 v (mkPrep s);
    mkV3 : Str -> Str -> V3 = \v,s -> dirV3 (regV v) (mkPrep s);
    mkV3 : V -> V3 = dirdirV3 ;
    mkV3 : Str -> V3 = \v -> dirdirV3 (regV v) ;
  } ;
-}
  mkConj = overload {
    mkConj : Str -> Conj = \y -> mk2Conj [] y plural ;
    mkConj : Str -> Number -> Conj = \y,n -> mk2Conj [] y n ;
    mkConj : Str -> Str -> Conj = \x,y -> mk2Conj x y plural ;
    mkConj : Str -> Str -> Number -> Conj = mk2Conj ;
  } ;

  mk2Conj : Str -> Str -> Number -> Conj = \x,y,n -> 
    lin Conj (sd2 x y ** {n = n}) ;

  regPN    : Str ->Gender -> PN ;          
   nounPN : N -> PN ;



} 
