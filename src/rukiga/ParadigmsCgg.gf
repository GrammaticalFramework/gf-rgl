--# -path=.:../prelude:../abstract:../common

--1 Rukiga Lexical Paradigms
--
-- David Bamutura 2017--2020
-- based on the English Paradigms file.
-- This is an API for the user of the resource grammar 
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
-- 
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$. 
--
-- The structure of functions for each word class $C$ is the following:
-- first we give a handful of patterns that aim to cover all
-- regular cases. Then we give a worst-case function $mkC$, which serves as an
-- escape to construct the most irregular words of type $C$.

resource ParadigmsCgg =
  open (Predef=Predef), ResCgg, CatCgg, Prelude in {

oper

  mkN : overload {
    mkN : (fish : Str) -> NClass -> N ;
    mkN : (man,men : Str) -> NClass -> N ;
    } ;

  mkV : overload {
    mkV : (cry : Str) -> V ;
    mkV : (cry, pres, perf :Str) -> V;
    } ;


  mkN = overload {
    mkN : (fish : Str) -> Gender -> N
      = \fish,nclass -> lin N (smartNoun fish nclass) ;
    mkN : (man,men : Str) -> Gender -> N
      = \man,men,nclass -> lin N (mkNoun man men nclass) ;
    };

  mkV = overload {
    mkV  : Str -> Verb  
    = \root -> lin V (smartVerb root);  --{s =root; pres =[]; perf = []; morphs= mkVerbMorphs; isRegular = True}; --only those verbs whose conjugation involves change of last letter and are done in the same way in both runyankore and rukiga
    mkV  : Str -> Str ->Str -> Verb 
    = \root, restPres, restPerf ->lin V (mkVerb root restPres restPerf);
    mkV  : Str -> Str ->Str ->Str -> Bool -> Verb 
    = \root, restPres, restPerf, p, bool ->lin V (mkVerbV2X root restPres restPerf p bool);
  };
  
  mkV2 = overload {
    mkV2 : Str -> V2 = \root ->dirV2 (smartVerb root); --**  {comp =[] ; isCompN2 = False};
    mkV2 : Str -> Str ->Str -> V2 = \root, s1, s2 ->dirV2 (mkVerb root s1 s2); --**  {comp =[] ; isCompN2 = False};
  };
  mkV3 = overload {
    mkV3 : Str -> Verb3 = \root ->mkV2 root ** {comp2 =[]};
    mkV3 : Str -> Str ->Str -> Verb3 = \root ,s1,s2 ->mkV2 root s1 s2 ** {comp2 =[]};
    --mkV3 : Str -> Str ->Str ->Str ->Verb3 =\root, s1, s2, prep
  };

  
  --3 Relational nouns 

  -- mkN2 : overload {
  --   --mkN2 : Str -> N2 ; -- reg. noun, prep. "of" --% 
  --   --mkN2 : N -> N2 ; -- e.g. wife of (default prep. to)
  --   --mkN2 : N -> Str -> N2 ; -- access to --%
  --   mkN2 : N -> Prep -> N2 ; -- e.g. access to
  --   --mkN2 : Str -> Str -> N2 ; -- access to (regular noun) --%
  -- } ;

  mkN2 = overload {
    mkN2 : N -> Prep -> N2 = prepN2 ;
    --mkN2 : N -> Str -> N2 = \n,s -> prepN2 n (mkPrep s);
    --mkN2 : Str -> Str -> N2 = \n,s -> prepN2 (regN n) (mkPrep s);
    --mkN2 : N -> N2         = \n -> prepN2 n (mkPrep "of") ;
    --mkN2 : Str -> N2       = \s -> prepN2 (regN s) (mkPrep "of") 
  } ;
  
  -- mkN2  : N -> Prep -> N2 = \n,p -> case p.isGenPrep of{
  --                                         False => lin N2 (n ** {c2 =\\_=> p.s}) ; 
  --                                         True  => lin N2 (n ** {c2 = mkGenPrepWithIV }) --avoiding lock_C fields
  --                                       };
  prepN2  : N -> Prep -> N2 = \n,p -> case p.isGenPrep of{
                                          False => lin N2 (n ** {c2 =\\_=> p.s}) ; 
                                          True  => lin N2 (n ** {c2 = mkGenPrepWithIV }) --avoiding lock_C fields
                                        };
  -- Three-place relational nouns ("the connection from x to y") need two prepositions.

  mkN3 : N -> Prep -> Prep -> N3 ; -- e.g. connection from x to y
  mkN3 = \n,p,q -> case <p.isGenPrep,q.isGenPrep> of{
                        <False,False>  => lin N3 ( lin N2 (n ** {c2 =\\_=> p.s}) ** {c3 =\\_=> q.s}); --method of avoiding lock_C fields
                        <True, False>  => n ** {c2 = mkGenPrepWithIV ; c3 =\\_=> q.s; lock_N2 = <>;lock_N3 = <>} ;
                        <False,True>   => n ** {c2 =\\_=> p.s ; c3 = mkGenPrepWithIV; lock_N2 = <>;lock_N3 = <>} ;
                        <True,True>    => n ** {c2 = mkGenPrepWithIV; c3 = mkGenPrepWithIV; lock_N2 = <>; lock_N3 = <>}
                                        };
  
   mkVS  : V -> VS ; -- sentence-compl e.g. say (that S)
   mkVS  v = lin VS v ;
   mkVQ  : V -> VQ ; -- e.g. wonder (QS)
   mkVQ  v = lin VQ v ;
  
   mkVA  : V -> VA ; -- e.g. become (AP)  
   mkVA  v = lin VA v ;


   mkV2S : V -> Prep -> V2S ; -- e.g. tell (NP) (that S)
   mkV2S v p = lin V2S (prepV2 v p) ;
   --mkPrep : Str -> Str ->Bool -> Preposition ; -- e.g. "in front of"
   mkPrep : Str -> Str  ->Bool -> Prep; 
   mkPrep first other bool = lin Prep {
     s = first ;
     other = other;
     isGenPrep = bool
   };
   prepV2 : V -> Prep -> V2 ;
   prepV2 v p = lin V2 {s = v.s ; 
                        pres = v.pres ; 
                        perf = v.perf ;
                        isPresBlank = v.isPresBlank ; 
                        isPerfBlank = v.isPerfBlank; 
                        isRegular = v.isRegular; 
                        p = v.p ;
                        isRefl = v.isRefl; 
                        comp = p.s;
                        isCompN2 = p.isGenPrep}; --; isRefl = v.isRefl} ;
  dirV2 : V -> V2 = \v -> prepV2 v noPrep ;
  noPrep = mkPrep [] [] False;
  --2 Prepositions
  --
  -- A preposition as used for rection in the lexicon, as well as to
  -- build $PP$s in the resource API, just requires a string.

  -- mkPrep : Str -> Str ->Bool -> Prep ; -- e.g. "in front of"
  -- noPrep : Prep; -- no preposition
  -- noPrep = mkPrep [] [] False;
  
  --mkVQ  : V -> VQ ; -- e.g. wonder (QS)
  mkV2Q : V -> Prep -> V2Q ; -- e.g. ask (NP) (QS)
  mkV2Q v p = lin V2Q (prepV2 v p) ;

   --V2V verbs
  mkV2V = overload {
    -- mkV2V : Str -> V2V = \s -> lin V2V (dirV2 (mkV s) ** {c3 = [] ; typ = VVAux}) ;
    mkV2V : V -> V2V = \v -> lin V2V (dirV2 v ** {c3 = [] ; typ = VVAux}) ;
    mkV2V : V -> Prep -> Prep -> V2V = \v,p,t -> lin V2V (prepV2 v p ** {c3 = t.s ; typ = VVAux}) ;
  } ;

  -- Verbs with a particle.
-- The particle, such as in "switch on", is given as a string.

  partV  : V -> Str -> V ; -- with particle, e.g. switch + on
  partV v p = lin V {s = v.s ; 
                      pres = v.pres; 
                      perf = v.perf;
                      isPresBlank = v.isPresBlank;
                      isPerfBlank = v.isPerfBlank;
                      isRegular = v.isRegular;
                      p = p ; 
                      isRefl = v.isRefl
                    } ;
  partV2  : V2 -> Str -> V2 ; -- with particle, e.g. switch + on
  partV2 v p = lin V2 {s = v.s ; 
                      pres = v.pres; 
                      perf = v.perf;
                      isPresBlank = v.isPresBlank;
                      isPerfBlank = v.isPerfBlank;
                      isRegular = v.isRegular;
                      p = p ; 
                      isRefl = v.isRefl;
                      comp = []; isCompN2 = False} ;
  
  -- mkVA  : V -> VA ; -- e.g. become (AP)
  -- mkV2A : overload {
  --   mkV2A : V -> V2A ; -- e.g. paint (NP) (AP)
  --   mkV2A : V -> Prep -> V2A ; -- backwards compatibility
  --   mkV2A : V -> Prep -> Prep -> V2A ; -- e.g. strike (NP) as (AP)
  --   } ;
  -- mkVQ  : V -> VQ ; -- e.g. wonder (QS)
  -- mkV2Q : V -> Prep -> V2Q ; -- e.g. ask (NP) (QS)

  -- mkAS  : A -> AS ; --%
  -- mkA2S : A -> Prep -> A2S ; --%
  -- mkAV  : A -> AV ; --%
  -- mkA2V : A -> Prep -> A2V ; --%

-- Notice: Categories $V0, AS, A2S, AV, A2V$ are just $A$.
-- $V0$ is just $V$; the second argument is treated as adverb.

  V0 : Type ; --%
  AS, A2S, AV, A2V : Type ; --%

--2 Other categories

mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; --%
mkInterj : Str -> Interj
  = \s -> lin Interj (ss s) ;

--.
--2 Definitions of paradigms
--
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.


-- Rearrange this document in future so that a paradigms file is
-- as should be i.e with an abstract part and a a part with 
-- definitions
  mkOrd : Str -> Ord = \s -> lin Ord { s =  \\_=>s; position = Post};
  V0 : Type = V ;
  AS, A2S, AV : Type = A ;
  A2V : Type = A2 ;
  mkV0 : V -> V;
  mkV0  v = v ;
  mkA2 : Str -> Position -> Bool -> Bool ->Bool-> A2V = \a2, pos, isProper, isPrep,isNeg -> 
    lin A2V ((mkAdjective a2 pos isProper isPrep isNeg) ** {c2 = ""; isPre = True});
  --mkA2V : A -> A2V;
  --mkA2V a = lin A2V (a * {c2 = ""; isPre = True});
  mkA2V : Str -> Position -> Bool -> Bool ->Bool-> A2 =\a2, pos, isProper, isPrep,isNeg -> lin A2 ((mkAdjective a2 pos isProper isPrep isNeg) ** {c2 = ""; isPre = True});

  
  -- Adverbs modifying numerals

  mkAdN : Str -> AdN ; -- e.g. approximately
  mkCAdv : Str -> CAdv ;
  mkAdN x = lin AdN (ss x) ;
  mkCAdv x = lin CAdv (ss x ** {p = []}) ;

  mkConj : overload {
    mkConj : Str -> Conj ;                  -- and (plural agreement) --%
    mkConj : Str -> Number -> Conj ;        -- or (agrement number given as argument) --%
    mkConj : Str -> Str -> Conj ;           -- both ... and (plural) --%
    mkConj : Str -> Str -> Number -> Conj ; -- either ... or (agrement number given as argument) --%
  } ;

  mkConj = overload {
    mkConj : Str -> Conj = \y -> mk2Conj [] y Pl ; -- when you have simply and
    mkConj : Str -> Number -> Conj = \y,n -> mk2Conj [] y n ;
    mkConj : Str -> Str -> Conj = \x,y -> mk2Conj x y Pl ; -- when you have both ... and ...
    mkConj : Str -> Str -> Number -> Conj = mk2Conj ;
  } ;

  mk2Conj : Str -> Str -> Number -> Conj = \x,y,n -> 
    lin Conj {s = \\_=>x; s2 = y; n = n};

  --2 Adverbs

  -- Adverbs are not inflected. Most lexical ones have position
  -- after the verb. Some can be preverbal (e.g. "always").

  mkAdv : Str -> AgrExist -> Adv ; -- e.g. today
  
  --mkAdV : Str -> AdV ; -- e.g. always

  -- Adverbs modifying adjectives and sentences can also be formed.

  mkAdA  : Str -> Position -> AdA ; -- e.g. quite
  --mkCAdv : Str -> Str -> Str -> CAdv ;   -- more than/no more than

  mkAdv x agrEx = lin Adv {s = x ; agr = agrEx } ;
  mkAdA x pos = lin AdA {s = x ; position = pos } ; -- e.g. quite
}
