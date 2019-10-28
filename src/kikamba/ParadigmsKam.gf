--# -path=.:../abstract:../../prelude:../common

resource ParadigmsKam = open 
  (Predef=Predef), 
  Prelude, 
  MorphoKam,
   --ResKam
  CatKam
  in {

oper
  Cgender : Type ; 
  mu_a   : Cgender ; --class gender 
  mu_mi  : Cgender ;
  i_ma   : Cgender ;
  ki_i   : Cgender ;
  ka_tu  : Cgender ;
  va_ku  : Cgender ;
  n_n    : Cgender ; 
  u_ma   : Cgender ;
  u_n    : Cgender ;
  ku_ma  : Cgender ;

  Number : Type ; 
  singular : Number ; 
  plural   : Number ;



  Case : Type ; 

  nominative : Case ; 
  locative   : Case ; 
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
 
oper dfltGender : Cgender = G1 ; 
       dfltNumber : Number = Sg ;
 
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




--3 Two-place verbs
--
-- Two-place verbs need a preposition, except the special case with direct object.
-- (transitive verbs). Notice that a particle comes from the $V$.-}

 -- mkV2 : overload {
 --   mkV2  : V -> V2 ;         -- transitive, e.g. hit
 --   mkV2  : V -> Prep -> V2 ; -- with preposiiton, e.g. believe in
 --       };

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
    mkV3  : V -> V3 ;                   -- ditransitive, e.g. give,_,_
    mkV3  : V -> Prep -> Prep -> V3 ;   -- two prepositions, e.g. speak, with, about
    mkV3  : V -> Prep -> V3 ;           -- give,_,to --%
           -- give,_,_ --%
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

  Cgender =  MorphoKam.Cgender ; 
  Number =  MorphoKam.Number ;
  Case =  MorphoKam.NPCase ;
  mu_a   = G1 ;-- class gender 1/2
  mu_mi  = G2 ; -- class gender 3/4
  i_ma   = G3 ; -- class gender 5/6
  ki_i   = G4 ; -- class gender 7/8
  ka_tu  = G5 ; -- class gender 12/13
  va_ku  = G6 ; -- class gender 16/17
  n_n    = G7 ; -- class gender 9/10
  u_ma   = G8;  -- class gender 11/6
  u_n    =G9 ; -- class gender 11/10
  ku_ma  = G10 ; -- class gender 15/6
  singular = Sg ;
  plural = Pl ;
  nominative = npNom ;
  locative = npLoc ;

  npNumber np = (nounAgr np.a).n ;
 

  regN = MorphoKam.regN ; 
  iregN = MorphoKam.iregN ;

 
 

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
  mkPrepof : Number => Cgender => Str = 
    table Number { Sg => table {  
                                 G3| G7 => "ya" ; 
                                 G4 => "kya" ; 
                                 G5 => "ka" ; 
                                 G6 => "va" ;
                                  G10 => "kwa";
                                 _ => "wa" } ; 
                                 
                   Pl => table { G1|G3| G8 | G10 => "ma" ; 
                                 G4| G7 |G9 => "sya" ; 
                                 G2 => "ya" ; 
                                 G5 => "twa" ; 
                                 G6 => "kwa"} } ;
  
                       
 

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

 -- regN : Str ->Cgender -> N ;
  --iregN : (man,men : Str) ->Cgender -> N ;

  --compoundN : Str  -> N -> N ;

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

    verb2snoun : Verb ->  Cgender -> Noun = \v,g->    
    let wp = "mu" + init(v.s ! VGen) +"i" ;
        wpl = "a" + init(v.s ! VGen) +"i" in 
    iregN wp wpl g ;
 
  prepN2 : N -> Prep -> N2 ;

-- The most common preposition is "of", and the following is a
-- shortcut for regular relational nouns with "of".

  regN2 : N -> N2 ;
  




  regA : Str -> A = \s -> lin A (MorphoKam.regA s) ;
   cregA : Str -> A = \s -> lin A (MorphoKam.cregA s) ;
   sregA : Str -> A = \s -> lin A (MorphoKam.sregA s) ;
   iregA : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoKam.iregA a b);
   regAdj : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoKam.regAdj a b);
  
  mkA = overload {
    mkA : Str -> A = \a -> lin A (regA a |cregA a |sregA a);
    mkA : (fat,fatter : Str) -> A =\a,b -> lin A (iregA a b| regAdj a b );
    } ;

  prepA2 : A -> Prep -> A2 ;

  mkA2 = overload {
    mkA2 : A -> Prep -> A2   = prepA2 ;
    mkA2 : A -> Str -> A2    = \a,p -> prepA2 a (mkPrep p False) ;
    mkA2 : Str -> Prep -> A2 = \a,p -> prepA2 (regA a) p;
    mkA2 : Str -> Str -> A2  = \a,p -> prepA2 (regA a) (mkPrep p False);
  } ;

  mkV = overload {
    mkV : (cry : Str) -> V=\v-> lin V (regV v ) ; -- regular, incl. cry-cries, kiss-kisses etc
    --mkV : Str -> V -> V=\v -> lin V (regV v ) ;  -- fix compound, e.g. under+take
  }; 
  iregV=MorphoKam.iregV ;
  regV=MorphoKam.regV ;
mkV2 = overload {
        mkV2  : Str -> V2 = \s -> dirV2 (regV s |iregV s) ;   
        mkV2  : V -> V2 = dirV2 ;
        mkV2  : V -> Prep -> V2 = prepV2  ;
  };

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
    --  mkVV  v = v ** {c2 = Prep ; lock_VV = <>} ;

-- mkVV : V -> VV ;
     -- mkVV  v = v ** {c2 = Prep ; lock_VV = <>} ;

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
