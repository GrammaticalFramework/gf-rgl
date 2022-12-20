--# -path=.:../prelude:../common:../abstract
--# -coding=utf8

-- A Polish verb Resource Morphology 
--
-- Adam Slaski, 2009 <adam.slaski@gmail.com>
--
resource ParadigmsVerbLit = ResLit ** open Prelude, CatLit, (Predef=Predef), (Adj=ParadigmsAdjectiveLit), MorphoLit in {

     flags  coding=utf8; 

-- 2 Conjugation classes

-- According to "Czasownik polski. Odmiana. Slownik." by Zygmunt Saloni 2001 
-- there are 106 schemes of verb inflection in Polish. I implement only 
-- these, which are necessery. Numeration as in the book mentioned above.

-- opers for the building of the whole paradigm of a verb
-- in all tenses

oper

-- 3 Verb types definition   

--  mkV : Str ->  ConjCl -> Str ->  ConjCl -> Verb; 
  mkV : Str ->  Str -> Str -> Verb; 
  mkV = mkVerb;	   
  
--  mkV1 : Str ->  ConjCl -> Str ->  ConjCl -> Verb; 
  mkV1 : Str ->  Str -> Str -> Verb; 
  mkV1 infForm presForm pastForm = mkIntrVerb (mkVerb infForm presForm pastForm);	   

  
-- reflexive verbs

{-  
  oper mkReflVerb : Verb -> Verb = 
	 \v -> 
	 {si = v.si;
	  sp = v.sp;
	  refl = "się";
	  asp = v.asp;
	  ppartp =  v.ppartp;
	  pparti =  v.pparti
	 };
 -}
-- intransitive verbs

{-
  oper mkItVerb : Verb -> Verb = 
	 \v -> 
	 {si = v.si;
	  sp = v.sp;
	  refl = v.refl;
	  asp = v.asp;
	  ppartp = record2table { msnom, msacc, msgen, msins, msdat, msloc, mpnom, mpacc, mpgen, mpins, mpdat, mploc, fsnom, fsacc, fsgen, fsins, fsdat, fsloc, fpnom, fpacc, fpgen, fpins, fpdat, fploc, nnom, ngen = "["++v.si!VInfM ++ [": the participle form does not exist]"]};
	  pparti = record2table { msnom, msacc, msgen, msins, msdat, msloc, mpnom, mpacc, mpgen, mpins, mpdat, mploc, fsnom, fsacc, fsgen, fsins, fsdat, fsloc, fpnom, fpacc, fpgen, fpins, fpdat, fploc, nnom, ngen = "["++v.si!VInfM ++ [": the participle form does not exist]"]}
	 };
-}

-- Utilité ???
  oper mkIntrVerb : Verb -> Verb = 
	 \v -> 
	 {
	  forms = v.forms;
	  refl = v.refl;
	  asp = v.asp;
	  passPastPart = v.passPastPart;
	  actPastPart = v.actPastPart;
	  actPastFreqPart = v.actPastFreqPart;
	  actPresPart = v.actPresPart;
	  actFutPart = v.actFutPart;
--	  ppart = record2table { msnom, msacc, msgen, msins, msdat, msloc, mpnom, mpacc, mpgen, mpins, mpdat, mploc, fsnom, fsacc, fsgen, fsins, fsdat, fsloc, fpnom, fpacc, fpgen, fpins, fpdat, fploc, nnom, ngen = "["++v.forms!Unfronted!VInf ++ [": the participle form does not exist]"]};
	 };


-- monoaspective verbs

{-
  oper mkMonoVerb : Str -> ConjCl -> Aspect -> Verb = 
	 \s, c, a -> let tmp = (c s) in 
	 {si = tmp.s;
	 sp = tmp.s; 
	 refl = "";
	 asp = a;
	 ppartp = tmp.p;
	 pparti = tmp.p
	 };
-}

-- normal verbs
  

-- Comlicated verbs
-- Comlicated verbs like 'mieć nadzieję' ('to hope'). Sometimes happens that English verb
-- can't be translated directly into one Polish word, so I introduced this (little bit
-- unnatural) construction.

{-
  oper mkComplicatedVerb : Verb -> Str -> Verb = 
	 \v,s -> 
	 {si = \\form => v.si !form ++ s;
	 sp = \\form => v.sp !form ++ s;
	 refl = v.refl; asp = v.asp;
	 ppartp = record2table { msnom, msacc, msgen, msins, msdat, msloc, mpnom, mpacc, mpgen, mpins, mpdat, mploc, fsnom, fsacc, fsgen, fsins, fsdat, fsloc, fpnom, fpacc, fpgen, fpins, fpdat, fploc, nnom, ngen = "["++v.si!VInfM ++s++ [": the participle form does not exist]"]};
	 pparti = record2table { msnom, msacc, msgen, msins, msdat, msloc, mpnom, mpacc, mpgen, mpins, mpdat, mploc, fsnom, fsacc, fsgen, fsins, fsdat, fsloc, fpnom, fpacc, fpgen, fpins, fpdat, fploc, nnom, ngen = "["++v.si!VInfM ++s++ [": the participle form does not exist]"]}
	 };
-}
  
-- Two-place verbs   
-- Two-place verbs, and the special case with a direct object. Note that
-- a particle can be included in a $V$.
  
  mkV2 : Verb -> Str -> Case -> V2;  
  mkV2 v p cas = v ** { cplCase = mkCompl p cas; lock_V2 = <> }; 
  
  -- verb Prep1 Case1 Prep2 Case2
  mkV3 : Verb -> Str -> Case -> Str -> Case -> V3; 
  mkV3 v s1 c1 s2 c2 = v ** { cplCase = mkCompl s1 c1; cplCase2 = mkCompl s2 c2; lock_V3 = <> };  
  
  dirV2 : Verb -> V2; -- a typical case ie. "kochać", "pisać"
  dirV2 v = mkV2 v [] Acc;

  dirV3 : Verb -> V3; -- a typical case ie. "zabrać", "dać"
  dirV3 v = mkV3 v [] Acc [] Dat; 


}
