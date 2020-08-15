--# -path=.:../abstract:../../prelude:../common

interface ParadigmsBantu = DiffBantu ** open CommonBantu in {

flags 
  optimize=all ;
  coding=utf8 ;


oper
  Cgender : PType ; 


  Number : Type ; 
  singular : Number ; 
  plural   : Number ;



  NPCase : Type ; 

  nominative : NPCase ; 
  locative   : NPCase ; 
  npNumber : NP -> Number ; -- exctract the number of a noun phrase
  

--2 Nouns

  mkN : overload {
     mkN : (flash : Str) -> Cgender -> N ;  --regular plural 
     mkN :(man,men : N) -> Cgender -> N ;
     mkN : V -> N ; --nouns from verbs
     mkN : (man,men : Str) ->Cgender -> N };-- irregular plural
   
 

--3 Relational nouns 
 mkN2 : overload {
      mkN2 : N -> Prep -> N2 ;
      mkN2 : N -> Str -> N2 ;
      mkN2 : N -> N2 ; 
      mkN2 : N -> (Cgender => Number => Str)-> N2 ;
    } ;
 
oper dfltGender : Cgender = G1 ; --default gender
       dfltNumber : Number = Sg ; --default number
 
-- Three-place relational nouns ("the connection from x to y") need two prepositions.

  mkN3 : N -> Prep -> Prep -> N3 ; -- e.g. connection from x to y



--3 Proper names and noun phrases
--
-- Proper names, with a regular genitive, are formed from strings.

  mkPN : overload {

    mkPN : Str ->Cgender -> PN ;

-- Sometimes a common noun can be reused as a proper name, e.g. "Bank"

    mkPN : N -> PN --%
  } ;

--3 Determiners 

  mkOrd : Str -> Ord ; --%

--2 Adjectives

  mkA : overload {
    mkA : (happy : Str) -> A ; 
    mkA : (fat,fatter : Str) -> A ; -- irreg. 
    } ;


--3 Two-place adjectives

  mkA2 : overload {
    mkA2 : A -> Prep -> A2 ;  
    mkA2 : A -> Str -> A2 ; 
    mkA2 : Str -> Prep -> A2 ; 
    mkA2 : Str -> Str -> A2 
  } ;


--2 Adverbs

-- Adverbs are not inflected. Most lexical ones have position
-- after the verb. Some can be preverbal (e.g. "always").

  mkAdv : Str -> Adv ; -- e.g. umuthi
  mkAdV : Str -> AdV ; -- e.g. lila

-- Adverbs modifying adjectives and sentences can also be formed.

  mkAdA : Str -> AdA ; -- e.g. pi

-- Adverbs modifying numerals

  mkAdN : Str -> AdN ; -- e.g. approximately

--2 Prepositions

  mkPrep : overload {
   mkPrep : Str ->Bool-> Prep  ;
   mkPrep : (Cgender => Number => Str) ->Bool -> Prep ;
  } ;
  
  noPrep : Prep ;  -- no preposition

--2 Conjunctions
--

  mkConj : overload {
    mkConj : Str -> Conj ;                  -- and (plural agreement) --%
    mkConj : Str -> Number -> Conj ;        -- or (agrement number given as argument) --%
    mkConj : Str -> Str -> Conj ;           -- both ... and (plural) --%
    mkConj : Str -> Str -> Number -> Conj ;  } ;

--2 Verbs
--

-- Verbs are constructed by the function $mkV$, which takes a varying
-- number of arguments.

  mkV : overload {
    mkV : (cry : Str) -> V ; -- regular, incl. cry-cries, kiss-kisses etc
    mkV : Str -> V -> V ;  -- fix compound, e.g. under+take
  };




-- Two-place verbs
--
-- Two-place verbs need a preposition, except the special case with direct object.
-- (transitive verbs). Notice that a particle comes from the $V$.-}


mkV2 : overload {
      mkV2 : V-> Prep -> V2 ;
      mkV2 : V -> Str -> V2 ;
      mkV2 : V -> V2 ; 
          } ;
--3 Three-place verbs
--
-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 : overload {
    mkV3  : V -> V3 ;                   -- 
    mkV3  : V -> Prep -> Prep -> V3 ;   
    mkV3  : V -> Prep -> V3 ;           
          
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

   singular = Sg ;
  plural = Pl ;


  npNumber np = (nounAgr np.a).n ;
 
  mkPN = overload {
   mkPN : Str -> Cgender -> PN = regPN;
   mkPN : N -> PN = nounPN
  } ;


mkN2 = overload {
      mkN2 : N -> Prep -> N2 = prepN2 ;
      mkN2 : N -> Str -> N2 = \n,s -> prepN2 n (mkPrep s False);
      mkN2 : N -> N2         = \n -> prepN2 n (mkPrep  mkPrepof False) ;
      mkN2 : N -> (Number =>Cgender =>  Str)-> Bool-> N2= \n,s,bool -> prepN2 n (mkPrep mkPrepof bool) ;
    } ;
 
  prepN2 = \n,p -> lin N2 (n ** {c2 = p}) ; 
  regN2 = \n -> (prepN2 n (mkPrep mkPrepof False )) ; 
  mkN3 = \n,p,q -> lin N3 (n ** {c2 = p ; c3 = q}) ; 
  nNumber : Number => Case => Number= table Number {
  Sg =>\\c => Sg; Pl=>\\c => Pl};
  dfGender: Cgender=> Cgender = table Cgender {G1 =>G1; _=>G1};
  fGender = G1 ;
  
                       
 

--3 Relational common noun phrases


  cnN2 : CN -> Prep -> N2 ;
  cnN3 : CN -> Prep -> Prep -> N3 ;


  cnN2 = \n,p -> lin N2 (n ** {c2 = p}) ;
  cnN3 = \n,p,q -> lin N3 (n ** {c2 = p ; c3 = q}) ;

  
    regPN n g = lin PN {s =\\c=> n  ; g = g} ;
    
   
  nounPN n = lin PN {s = n.s ! singular ; g = n.g} ;

    mkOrd : Str -> Ord = \x -> lin Ord { s =\\g => x};

 
  prepA2 a p = lin A2 (a ** {c2 = p.s!Sg!G1}) ;

  mkAdv  s =  lin Adv { s= \\_ => s };
  mkAdV x = lin AdV (ss x) ;
  mkAdA x = lin AdA (ss x) ;
  mkAdN x = lin AdN (ss x) ;

  mkPrep = overload {
    mkPrep : Str ->Bool-> Prep = \str,bool -> 
    lin Prep {s = \\n,g => str ;isFused = bool } ;
    mkPrep : (Number => Cgender =>  Str) ->Bool-> Prep = \t,bool ->
    lin Prep {s = t ;isFused = bool} ;};

   noPrep = mkPrep [] False ;
  

  mkN = overload {
    mkN : Str ->Cgender -> N =\n, g -> lin N (regN n g );
    mkN : (man,men : N)-> Cgender -> N =compoundN; 
     mkN : V -> N = \v -> lin N (verb2snoun v G1) ;    
    mkN : (man,men : Str) ->Cgender -> N = \s,p,g -> lin N ( iregN s p g) ;
            } ;
 compoundN : N -> N ->Cgender-> N = \mundu,muume,g -> {
   s = \\n,c => mundu.s! n! c ++ muume.s!n! Nom ;   
  g = g ;
   lock_N = <>
   } ;

   
 
  prepN2 : N -> Prep -> N2 ;

-- The most common preposition is "of", and the following is a
-- shortcut for regular relational nouns with "of".

  regN2 : N -> N2 ;
  


{-}

  regA : Str -> A = \s -> lin A (MorphoKam.regA s) ;
   cregA : Str -> A = \s -> lin A (MorphoKam.cregA s) ;
   sregA : Str -> A = \s -> lin A (MorphoKam.sregA s) ;
   iregA : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoKam.iregA a b);
   regAdj : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoKam.regAdj a b); 
  
  mkA = overload {
    mkA : Str -> A = \a -> lin A (regA a |cregA a );
    mkA : (fat,fatter : Str) -> A =\a,b -> lin A (iregA a b| regAdj a b );
    } ; -}

  prepA2 : A -> Prep -> A2 ;

  mkA2 = overload {
    mkA2 : A -> Prep -> A2   = prepA2 ;
    mkA2 : A -> Str -> A2    = \a,p -> prepA2 a (mkPrep p False) ;
    mkA2 : Str -> Prep -> A2 = \a,p -> prepA2 (regA a) p;
    mkA2 : Str -> Str -> A2  = \a,p -> prepA2 (regA a) (mkPrep p False);
  } ;

 -- mkV = overload {
   -- mkV : (cry : Str) -> V=\v-> lin V (regV v ) ; -- regular, incl. cry-cries, kiss-kisses etc
    --mkV : Str -> V -> V=\v -> lin V (regV v ) ;  -- fix compound, e.g. under+take
 -- }; 
  --iregV=MorphoKam.iregV ;
 -- regV=MorphoKam.regV ; 
 {-
mkV2 = overload {
        mkV2  : Str -> V2 = \s -> dirV2 (regV s |iregV s) ;   
        mkV2  : V -> V2 = dirV2 ;
        mkV2  : V -> Prep -> V2 = prepV2  ;
  };
-}
mkV3 = overload {
    mkV3 : V -> V3 = dirdirV3 ;
    mkV3 : V -> Prep -> Prep -> V3 = prepPrepV3 ;
    mkV3 : V -> Prep -> V3 = dirV3 ;
      } ;

prepV2 : V -> Prep -> V2 ;
dirV2 : V -> V2 ;
prepV2 v p = lin V2 (v**{c2=p} ) ;
dirV2 v = prepV2 v noPrep ;
prepPrepV3 v p t = lin V3 (v ** { c2 =  p ; c3 = t }) ;
dirV3      v   t = lin V3 (v ** { c2 = t ; c3 = noPrep }) ;
dirdirV3   v     = lin V3 (v  ** { c2 = noPrep ; c3 = noPrep }) ;

 
 prepPrepV3 : V -> Prep -> Prep -> V3 ;
  dirV3 : V -> Prep -> V3 ;
  dirdirV3 : V -> V3 ;

mkVV : V -> VV ;
     mkVV  v = v ** { lock_VV = <>} ;
mkVV : V -> VV ;
   

      mkVA  : V -> VA ;
      mkVA  v = v ** {lock_VA = <>} ;
 
  mkConj = overload {
    mkConj : Str -> Conj = \y -> mk2Conj [] y plural ;
    mkConj : Str -> Number -> Conj = \y,n -> mk2Conj [] y n ;
    mkConj : Str -> Str -> Conj = \x,y -> mk2Conj x y plural ;
    mkConj : Str -> Str -> Number -> Conj = mk2Conj ;
  } ;

  mk2Conj : Str -> Str -> Number -> Conj = \x,y,n -> 
    lin Conj (sd2 x y ** {n = n}) ;



  
  regPN    : Str ->Cgender -> PN ;          
 

  nounPN : N -> PN ;



} 
