--# -path=.:../prelude:../abstract:../common

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

  mkN2 : overload {
    --mkN2 : Str -> N2 ; -- reg. noun, prep. "of" --% 
    --mkN2 : N -> N2 ; -- e.g. wife of (default prep. to)
    --mkN2 : N -> Str -> N2 ; -- access to --%
    mkN2 : N -> Prep -> N2 ; -- e.g. access to
    --mkN2 : Str -> Str -> N2 ; -- access to (regular noun) --%
  } ;

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
   mkPrep : Str -> Str  ->Bool -> Preposition; 
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

  {-
 


  mkV2V : overload {
    mkV2V : Str -> V2V ;
    mkV2V : V -> V2V ;
    mkV2V : V -> Prep -> Prep -> V2V ;  -- e.g. want (noPrep NP) (to VP)
    } ;

  -}
  --mkV = overload {
    --mkV : (cry : Str) -> V
      --= \cry -> lin V (mkVerb cry) ; -- what does it mean to create a lin on the fly
    --};


{- Note: The following is copied from the file swahili/ParadigmsSwa.gf

--1 Swahili Lexical Paradigms

--2 Parameters 
--
-- To abstract over gender names, we define the following identifiers.

oper
  Animacy : Type ; 

  animate   : Animacy ;
  inanimate : Animacy ;

-- To abstract over number names, we define the following.

  Number : Type ; 

  singular : Number ;
  plural   : Number ;

-- To abstract over case names, we define the following.

  Case : Type ;

  nominative : Case ;
  locative   : Case ;

-- To abstract over nounclass names, we define the following.
  
  Gender : Type ;

   m_wa : Gender ;
   m_mi : Gender ;
   ji_ma : Gender ;
   e_ma : Gender ;
   ma_ma : Gender ;
   ki_vi : Gender ;
   e_e : Gender ;
   u_u : Gender ;
   u_ma : Gender ;
   u_e : Gender ;

 

--2 Nouns

-- Worst case: give all four forms and the semantic gender.

  mkN  : (mtu,watu : Str) -> Gender -> Animacy -> N ;

-- The regular function captures the variants for nouns depending on Gender and Number

  regN : Str -> Gender -> Animacy -> N ;

-- In practice the worst case is just: give singular and plural nominative.


  mk2N : (mtu , watu : Str) -> Gender -> Animacy -> N ;
  mk2N x y g anim = mkNounIrreg x y g anim ** {lock_N = <>};

  mkN2 : N -> Prep -> N2 ;
  mkN2  : N -> Prep -> N2 = \n,p -> n ** {c2 = p.s ; lock_N2 = <>} ;

  mkPrep : Str -> Prep ;
--  mkPrep p = {s = p ; c = CPrep PNul ; isDir = False ; lock_Prep = <>} ;
  mkPrep p = {s = p ; lock_Prep = <>} ;


--3 Relational nouns 
-- 
-- Relational nouns ("fille de x") need a case and a preposition. 

-- All nouns created by the previous functions are marked as
-- $nonhuman$. If you want a $human$ noun, wrap it with the following
-- function:

--  genderN : Gender -> N -> N ; 

-- For regular adjectives, the adverbial form is derived. This holds
-- even for cases with the variation "happy - happily".

   regA : Str -> A ;

-- If comparison is formed by "kuliko", as usual in Swahili,
-- the following pattern is used:

 compADeg : A -> A ;

--2 Definitions of paradigms
--
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.
--.

  Animacy = ResSwa.Animacy ; 
  Number = ResSwa.Number ;
  Case = ResSwa.Case ;
  Gender = ResSwa.Gender ;
  animate = AN ; 
  inanimate = IN ;
  singular = Sg ;
  plural = Pl ;
  nominative = Nom ;
  locative = Loc ;
  m_wa = g1_2 ;
  m_mi = g3_4 ;
  ji_ma = g5_6 ;
  e_ma = g5a_6 ;
  ma_ma = g6 ; 
  ki_vi = g7_8 ;
  e_e = g9_10 ; 
  u_u = g11 ; 
  u_ma = g11_6 ; 
  u_e = g11_10 ;
  VForm = ResSwa.VForm ;

--  regN x g anim = mkNomReg x g anim ** {lock_N = <>} ;

    regN = \x,g,anim ->
      mkNomReg x g anim ** {lock_N = <>} ;

--  mkN x y g anim = mkNounIrreg x y g anim ** {lock_N = <>} ;
  mkN = \x,y,g,anim -> 
    mkNounIrreg x y g anim ** {lock_N = <>} ;
 
-- Adjectives

   regA a = compADeg { 
         s = \\_ => (mkAdjective a).s ;
         lock_A = <>} ;

  compADeg a = 
    {
       s = table {
          Posit => a.s ! Posit ;
           _ => \\f => a.s ! Posit ! f ++ "kuliko" 
       } ; 
     lock_A = <>} ;
 
-- Verbs
    regV : Str -> V ;
    regV = \enda -> mkV enda ** {s1 = [] ; lock_V = <>} ;

{--
	mkV2 = overload {
	    mkV2 : Str -> V2 = \s -> dirV2 (regV s) ;
	    mkV2 : V -> V2 = dirV2 ;  
	    mkV2 : V -> Prep -> V2 = mmkV2
	  } ;

   mmkV2 : V -> Prep -> V2 ;
   mmkV2 v p = v ** {c2 = p ; lock_V2 = <>} ;
   dirV2 : V -> V2 = \v -> mmkV2 v "na" ;
--}

--2 Adverbs

-- Adverbs are not inflected. Most lexical ones have position
-- after the verb. 

  mkAdv : Str -> Adv ;
  mkAdv x = ss x ** {lock_Adv = <>} ;

-}

}
