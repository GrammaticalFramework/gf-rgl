--# -path=.:../abstract:../../prelude:../common
--
----1 Sndu Lexical Paradigms

resource ParadigmsSnd = open 
  Predef, 
  Prelude, 
  MorphoSnd,
  CatSnd
  in {

--2 Parameters 

oper
  masculine : Gender ;
  feminine : Gender ;
  singular : Number;
  plural : Number;
  
  singular = Sg ; plural = Pl;

  masculine = Masc ; feminine = Fem ; --i


--2 Nouns


  mkN2 : N -> Prep -> Str -> N2;
  mkN2 = \n,p,c -> n ** {lock_N2 = <> ; c2 = p.s ; c3 = c } ; 
  
  mkN3 : N -> Prep -> Str -> Str-> N3 ;
  mkN3 = \n,p,q,r -> n ** {lock_N3 = <> ; c2 = p.s ; c3 = q ; c4 = r} ;
  
-- Compound Nouns  

    mkCmpdNoun : Str -> N -> N
     = \s,noun -> {s =\\n,c => s ++ noun.s ! n ! c ; g = noun.g ; lock_N = <>};
 

-- Proper names
mkPN = overload {
  mkPN : Str -> PN =
     \s -> let n = mkN11 s in {s = n.s ! Sg ; g = n.g ; lock_PN = <>} ;
  mkPN : Str -> Gender -> PN =
     \s,g -> let n = mkN11 s in {s = n.s ! Sg ; g = g ; lock_PN = <>} ;   
  };   
-- Personal Pronouns     
  personalPN : Str -> Str -> Str -> Str -> Str -> Number -> Gender -> PPerson -> Pron =
     \s1,s2,s3,s4,sp,nn,g,p -> let n = mkPron s1 s2 s3 s4 in {s = n.s ; a = toAgr nn p g ; ps = sp ; lock_Pron = <>};
-- Demonstration Pronouns     
  demoPN : Str -> Str -> Str -> Quant =
    \s1,s2,s3 -> let n = makeDemonPronForm s1 s2 s3 in {s = n.s ; a = defaultAgr ; lock_Quant = <>};
	
-- Determiner    
  mkDet : Str -> Str -> Str -> Str -> Number -> Det =
    \s1,s2,s3,s4,nb -> let dt = makeDet s1 s2 s3 s4 nb in {s = dt.s ; n = nb ; lock_Det = <>};
	
-- Intergative pronouns    
  mkIP : (x1,x2,x3,x4:Str) -> Number -> Gender -> IP =
   \s1,s2,s3,s4,n,g -> let p = mkIntPronForm s1 s2 s3 s4 in { s = p.s ; n = n ; g = g ;  lock_IP = <>}; 

-- AdN
  mkAdN : Str -> AdN = \s -> ss s ;
  
--2 Adjectives

  mkA = overload {
    mkA : Str-> A 
      = \s -> mkAdj1 s ** {lock_A = <>} ;
	mkA : Str -> Str -> A2
	  = \a,c -> let n = mkAdj1 a in {s = n.s; c2 = c} ** {lock_A2 = <>} ;
    } ;

--2 Verbs

  mkV : Str -> V 
      = \s -> mkVerb s ** {lock_V = <>} ;

  mkV2 = overload {
    mkV2 : Str -> V2 
      = \s -> mkVerb s **  {c2 = {s = [] ; c = VTrans} ; lock_V2 = <>} ;
    mkV2 : V -> V2 
      = \v -> v ** {c2 = {s = [] ; c = VTrans} ; lock_V2 = <>} ;
    mkV2 : V -> Str -> V2 
      = \v,p -> v ** {c2 = {s = p ; c = VTrans} ; lock_V2 = <>} ;
    } ;
  
  mkV3 : V -> Str -> Str -> V3;
    mkV3 v p q = v ** { c2 = p ; c3 = q ; lock_V3 = <>} ;
  mkV2V : V -> Str -> Str -> Bool -> V2V ;
    mkV2V v s1 s2 b = v ** {isAux = b ; c1 = s1 ; c2 = s2 ; lock_V2V = <>} ;
  
-- compund verbs
   compoundV = overload {
   compoundV : Str -> V -> V = \s,v -> {s = \\vf => s ++ v.s ! vf ; lock_V = <>} ;     
   compoundV : Str -> V2 -> V = \s,v -> {s = \\vf => s ++ v.s ! vf ; lock_V = <>} ;
   };
 

----2 Adverbs
  mkAdv : Str -> Adv = \str -> {s =\\ _ =>  str ; lock_Adv = <>};

----2 Prepositions

  mkPrep : Str -> Prep ;
    mkPrep str = makePrep str ** {lock_Prep = <>};
    
--3 Determiners and quantifiers

--  mkQuant : overload {
    mkQuant1 : Pron -> Quant ;
--    mkQuant : (no_sg, no_pl, none_sg, non_pl : Str) -> Quant ;
--  } ;
  mkIQuant : Str -> Str -> Str -> Str -> IQuant ;
  mkIQuant s1 s2 s3 s4 = let n = makeIQuant s1 s2 s3 s4 in {s = n.s; lock_IQuant = <>} ;
  
--  mkQuant = overload {
    mkQuant1 : Pron -> Quant = \p -> {s = \\_,_,c => p.s!c ;a = p.a ; lock_Quant = <>};
--    mkQuant : (no_sg, no_pl, none_sg, non_pl : Str) -> Quant = mkQuantifier;
--  } ;

--2 Conjunctions
  mkConj : overload {
    mkConj : Str -> Conj ;                  -- and (plural agreement)
    mkConj : Str -> Number -> Conj ;        -- or (agrement number given as argument)
    mkConj : Str -> Str -> Conj ;           -- both ... and (plural)
    mkConj : Str -> Str -> Number -> Conj ; -- either ... or (agrement number given as argument)
  } ;
 mkConj = overload {
    mkConj : Str -> Conj = \y -> mk2Conj [] y plural ;
    mkConj : Str -> Number -> Conj = \y,n -> mk2Conj [] y n ;
    mkConj : Str -> Str -> Conj = \x,y -> mk2Conj x y plural ;
    mkConj : Str -> Str -> Number -> Conj = mk2Conj ;
  } ;

  mk2Conj : Str -> Str -> Number -> Conj = \x,y,n -> 
    lin Conj (sd2 x y ** {n = n}) ;  

--  mkV0  : V -> V0 ;
--  mkVS  : V -> VS ;
--  mkV2S : V -> Prep -> V2S ;
  mkVV  : V -> VV = \v ->  lin VV (v ** {isAux = False});
    

--  mkV2V : V -> Prep -> Prep -> V2V ;
--  mkVA  : V -> VA ;
--  mkV2A : V -> Prep -> V2A ;
--  mkVQ  : V -> VQ ;
--  mkV2Q : V -> Prep -> V2Q ;
--
--  mkAS  : A -> AS ;
--  mkA2S : A -> Prep -> A2S ;
--  mkAV  : A -> AV ;
--  mkA2V : A -> Prep -> A2V ;
--  mkA2V a p = a ** {c2 = p.s } ; 
--
---- Notice: Categories $V0, AS, A2S, AV, A2V$ are just $A$.
---- $V0$ is just $V$; the second argument is treated as adverb.
--
--  V0 : Type ;
--  AS, A2S, AV, A2V : Type ;
--
----.
----2 Definitions of paradigms
----
---- The definitions should not bother the user of the API. So they are
---- hidden from the document.
--
--  Gender = MorphoHin.Gender ; 
--  Number = MorphoHin.Number ;
--  Case = MorphoHin.Case ;
--  human = Masc ; 
--  nonhuman = Neutr ;
--  masculine = Masc ;
--  feminine = Fem ;
--  singular = Sg ;
--  plural = Pl ;
--  nominative = Nom ;
--  genitive = Gen ;

}
