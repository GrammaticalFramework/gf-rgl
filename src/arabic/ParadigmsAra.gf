--# -path=.:../abstract:../../prelude:../common

--1 Arabic Lexical Paradigms
--
-- Ali El Dada 2005--2006
--
-- This is an API to the user of the resource grammar
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
--
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$.
--
-- The main difference with $MorphoAra.gf$ is that the types
-- referred to are compiled resource grammar types. We have moreover
-- had the design principle of always having existing forms, rather
-- than stems, as string arguments of the paradigms.
--
-- The structure of functions for each word class $C$ is the following:
-- first we give a handful of patterns that aim to cover all
-- regular cases. Then we give a worst-case function $mkC$, which serves as an
-- escape to construct the most irregular words of type $C$.
--
-- The following modules are presupposed:

resource ParadigmsAra = open
  Predef,
  Prelude,
  MorphoAra,
  OrthoAra,
  (ResAra=ResAra),
  (A=AdjectiveAra),
  CatAra
  in {

  flags optimize = noexpand;  coding=utf8 ;

  oper

  Case : Type ;
  nom : Case ;
  acc : Case ;
  gen : Case ;

-- Prepositions are used in many-argument functions for rection.
  Preposition : Type ;
  noPrep : Preposition ;
  casePrep : Case -> Preposition ;
   --- TODO: continue, add all over the grammar

  Gender : Type ;
  masc : Gender ;
  fem : Gender ;

  Number : Type ;
  sg : Number ;
  pl : Number ;

  Species : Type ;
  hum : Species ;
  nohum : Species ;

  Vowel : Type ;
  va : Vowel ;
  vi : Vowel ;
  vu : Vowel ;

--2 Nouns

-- Overloaded operator for main cases


 mkN : overload {
   mkN : (sg : Str) -> N ; -- non-human regular nouns
   mkN : Species -> N -> N ;
   mkN : (sg,pl : Str) -> Gender -> Species -> N ;
   mkN : NTable -> Gender -> Species -> N ;  -- loan words, irregular
   mkN : (root,sgPatt,brokenPlPatt : Str) -> Gender -> Species -> N ; -- broken plural
   mkN : N -> (attr : Str) -> N ; -- Compound noun with invariant attribute
   mkN : N -> N -> N ;            -- Compound noun with singular genitive attribute, but inflects in state.
   mkN : Number -> N -> N -> N ;  -- Compound noun with genitive attribute, but inflects in state. Attribute's number specified by 1st arg.
   mkN : N -> A -> N ;            -- Force adjective modifier into the noun. Adjective inflects in state, case and number.
   mkN : Number -> N -> A -> N ;            -- Force adjective modifier into the noun. Adjective inflects in state and case. Adjective's number specified by 1st arg.
   mkN : N -> AP -> N  ;          -- Force AP modifier into the noun. AP inflects in state, case and number.
---   mkN : (root,sgPatt : Str) -> Gender -> Species -> N                -- sound feminine plural
---    = sdfN ;
   } ;

  dualN : N -> N ; -- Force the plural of the N into dual (e.g. "twins")

--This is used for loan words or anything that has untreated irregularities
--in the interdigitization process of its words
  mkFullN : NTable -> Gender -> Species -> N ;

--Takes a root string, a singular pattern string, a broken plural
--pattern string, a gender, and species. Gives a noun.
  brkN : Str -> Str -> Str -> Gender -> Species -> N ;

--Takes a root string, a singular pattern string, a gender,
--and species. Gives a noun whose plural is sound feminine.
  sdfN : Str -> Str -> Gender -> Species -> N ;

--takes a root string, a singular pattern string, a gender,
--and species. Gives a noun whose plural is sound masculine
  sdmN : Str -> Str -> Gender -> Species -> N ;




--3 Proper names

  mkPN = overload {
    mkPN : Str -> PN        -- Fem Hum if ends with ة, otherwise Masc Hum
     = smartPN ;
    mkPN : N -> PN
     = \n -> lin PN (n ** {s = \\c => n.s ! Sg ! Const ! c ++ n.s2 ! Sg ! Def ! c }) ; -- no idea /IL
    mkPN : Str -> Gender -> Species -> PN
     = mkFullPN ;
    } ;

  mkFullPN : Str -> Gender -> Species -> PN ;



--3 Relational nouns

  mkN2 : overload {
    mkN2 : N -> Preposition -> N2 ; -- ready-made preposition
    mkN2 : N -> Str -> N2 ; -- preposition given as a string
    mkN2 : N -> N2 ; -- no preposition
    mkN2 : Str -> N2 ; -- no preposition, predictable inflection
  } ;

  mkN3 : overload {
    mkN3 : N -> Preposition -> Preposition -> N3 ; -- ready-made prepositions
    mkN3 : N -> Str -> Str -> N3 ;  -- prepositions given as strings
  } ;


--2 Adjectives

-- Overloaded operator for main cases

 mkA = overload {
   mkA : (root,sg : Str) -> A             -- adjective with sound plural; takes root string and sg. pattern string
    = \r,p -> lin A (sndA r p);
   mkA : (root : Str) -> A                -- adjective with positive form aFCal
    = \r -> lin A (clrA r);
   mkA : (root,sg,pl : Str) -> A          -- adjective with broken plural
    = \r,s,p -> lin A (brkA r s p) ;
   mkA : A -> Str -> A = \a,s -> a ** {   -- add non-inflecting component after adjective
    s = table {af => a.s ! af ++ s}
    } ;
   mkA : Str -> A -> A = \s,a -> a ** {   -- add non-inflecting component before adjective
    s = table {af => s ++ a.s ! af}
    }

   } ;

  idaafaA : N -> A -> A ; -- first argument will be in constructus but inflect in case, adjective in genitive, but inflect in gender, number and definiteness. e.g. غَيْرُ طَيِّبٍ

  degrA : (posit,compar,plur : Str) -> A ;

  irregFemA : (masc : A) -> (fem : A) -> A ; -- adjective with irregular feminine. Takes two adjectives (masc. "regular" and fem. "regular") and puts them together.

--Takes a root string and a pattern string
  sndA : (root,patt : Str) -> Adj ;

--Takes a root string only
  clrA : (root : Str) -> Adj ;  -- forms adjectives of type aFCal

  nisbaA : Str -> Adj ; -- forms relative adjectives by adding the suffix ِيّ

--3 Two-place adjectives
--
-- Two-place adjectives need a preposition for their second argument.

  mkA2 : overload {
    mkA2 : A -> Preposition -> A2 ;
    mkA2 : A -> Str -> A2
  } ;

--2 Adverbs

-- Adverbs are not inflected. Most lexical ones have position
-- after the verb. Some can be preverbal.

  mkAdv : Str -> Adv ;
  mkAdV : Str -> AdV ;

-- Adverbs modifying adjectives and sentences can also be formed.

  mkAdA : Str -> AdA ;

  mkInterj : Str -> Interj ;

  mkSubj : overload {
    mkSubj : Str -> Subj ;        -- Default order Subord (=noun first and in accusative)
    mkSubj : Str -> Order -> Subj -- Specify word order
  } ;

--2 Prepositions
--
-- A preposition as used for rection in the lexicon, as well as to
-- build $PP$s in the resource API. Requires a string and a case.

  mkPrep : overload {
    mkPrep : Str -> Prep ;
    mkPrep : Str -> Case -> Prep
  } ;  -- preposition in the sense of RGL abstract syntax

--2 Conjunctions
  mkConj : overload {
    mkConj : Str -> Conj ; -- and
    mkConj : Str -> Str -> Conj ; -- either … or
  } ;

--2 Verbs

-- Overloaded operations

  mkV : overload {
    mkV : (imperfect : Str) -> V ; -- The verb in Per3 Sg Masc imperfect tense gives the most information
    mkV : (root : Str) -> (perf,impf : Vowel) -> V ; -- verb form I ; vowel = a|i|u
    mkV : (root : Str) -> VerbForm -> V ;  -- FormI .. FormX (no VII, IX) ; default vowels a u for I
    mkV : V -> (particle : Str) -> V -- V with a non-inflecting particle/phrasal verb
    } ;

  -- regV : Str -> V ;

  reflV : V -> V ; -- نَفْس in the proper case and with possessive suffix, e.g.  نَفْسَكِ

  v1 : Str -> Vowel -> Vowel -> V ; -- Verb Form I : fa`ala, fa`ila, fa`ula

  v2 : Str -> V ; -- Verb Form II : fa``ala

  v3 : Str -> V ; -- Verb Form III : faa`ala

  v4 : Str -> V ; -- Verb Form IV : 'af`ala

  v5 : Str -> V ; -- Verb Form V : tafa``ala

  v6 : Str -> V ; -- Verb Form VI : tafaa`ala

  v7 : Str -> V ; -- Verb Form VII : infa`ala

  v8 : Str -> V ; -- Verb Form VIII ifta`ala

  v10 : Str -> V ; -- Verb Form X 'istaf`ala

--3 Two-place verbs

-- Two-place verbs need a preposition, except the special case with direct object.
-- (transitive verbs). Notice that a particle comes from the $V$.

  mkV2 : overload {
    mkV2 : V -> V2 ; -- No preposition
    mkV2 : V -> Str -> V2 ; -- Preposition as string, default case genitive
    mkV2 : V -> Preposition -> V2 ; -- Ready-made preposition
    mkV2 : Str -> V2 ; -- Predictable verb conjugation, no preposition
  } ;

  dirV2 : V -> V2 ;

--3 Three-place verbs

-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 : overload {
    mkV3 : V -> Preposition -> Preposition -> V3 ; -- speak, with, about
    mkV3 : V -> (to : Str)  -> (about:Str) -> V3  -- like above, but with strings as arguments (default complement case genitive)
  } ;
  dirV3 : overload {
    dirV3 : V -> Preposition -> V3 ; -- give,_,to
    dirV3 : V -> (to : Str)  -> V3   -- like above, but with string as argument (default complement case genitive)
  } ;
  dirdirV3 : V -> V3 ;               -- give,_,_

--3 Other complement patterns
--
-- Verbs and adjectives can take complements such as sentences,
-- questions, verb phrases, and adjectives.

  mkV0  : V -> V0 ;
  mkVS : overload {
    mkVS : V -> VS ;
    mkVS : V -> Str -> VS
    } ;
  mkV2S : V -> Str -> V2S ;
  mkVV = overload {
    mkVV : V -> VV = regVV ;
    mkVV : V -> Str -> VV = c2VV ;
    mkVV : V -> Preposition -> VV = prepVV ;
    mkVV : V -> Preposition -> Preposition -> VV = prep2VV
    } ;
  mkV2V : overload {
    mkV2V : V -> Str -> Str -> V2V ;
    mkV2V : V -> Preposition -> Preposition -> V2V ;
    mkV2V : VV -> Preposition -> V2V
  } ;
  mkVA  : V -> VA ;
  mkV2A : V -> Str -> V2A ;
  mkVQ  : V -> VQ ;
  mkV2Q : V -> Str -> V2Q ;

  mkAS  : A -> AS ;
  mkA2S : A -> Str -> A2S ;
  mkAV  : A -> AV ;
  mkA2V : A -> Str -> A2V ;

-- Notice: categories $AS, A2S, AV, A2V$ are just $A$,
-- and the second argument is given
-- as an adverb. Likewise
-- $V0$ is just $V$.

  V0 : Type ;
  AS, A2S, AV, A2V : Type ;


--.
--2 Definitions of paradigms

-- The definitions should not bother the user of the API. So they are
-- hidden from the document.
  Case = ResAra.Case ;
  nom = ResAra.Nom ;
  acc = ResAra.Acc ;
  gen = ResAra.Gen ;

-- Prepositions are used in many-argument functions for rection.

  Preposition = ResAra.Preposition ;
  noPrep = {s=[]; c=nom; binds=False} ;
  casePrep c = {s=[]; c=c; binds=False} ;

  Gender = ResAra.Gender ;
  masc = ResAra.Masc ;
  fem = ResAra.Fem ;

  Number = ResAra.Number ;
  sg = ResAra.Sg ;
  pl = ResAra.Pl ;

  Species = ResAra.Species ;
  hum = ResAra.Hum ;
  nohum = ResAra.NoHum ;

  Vowel = ResAra.Vowel ;
  va = ResAra.a ;
  vu = ResAra.u ;
  vi = ResAra.i ;

  mkPrep = overload {
    mkPrep : Str -> Prep = \s ->
      lin Prep (mkPreposition s) ;
    mkPrep : Str -> Case -> Prep = \s,c ->
      lin Prep (mkPreposition s c)
    } ;


  mkV2 = overload {
    mkV2 : V -> V2 = dirV2 ;
    mkV2 : V -> Str -> V2 = \v,p -> prepV2 v (mkPreposition p);
    mkV2 : V -> Preposition -> V2 = prepV2 ;
    mkV2 : Str -> V2 = strV2;
  } ;

  prepV2 : V -> Preposition -> V2 = \v,p -> v ** {s = v.s ; c2 = p ; lock_V2 = <>} ;
  strV2 : Str -> V2 = \str -> dirV2 (mkV str) ;

 mkN = overload {
   mkN : (sg : Str) -> N                                     -- non-human regular nouns
     = smartN ;
   mkN : Species -> N -> N
     = \p,n -> n ** {h = p} ;
   mkN : (sg,pl : Str) -> Gender -> Species -> N
     = \sg,pl -> mkFullN (reg sg pl) ;
   mkN : NTable -> Gender -> Species -> N                             -- loan words, irregular
     = mkFullN ;
   mkN : (root,sgPatt,brokenPlPatt : Str) -> Gender -> Species -> N   -- broken plural
     = brkN ;
   mkN : N -> (attr : Str) -> N                                       -- Compound nouns with noninflecting attribute
     = \n,attr -> n ** {s2 = \\num,s,c => n.s2 ! num ! s ! c ++ attr} ;
   mkN : N -> N -> N                                                  -- Compound nouns where attribute inflects in state and case but not number
     = attrN Sg ;
   mkN : Number -> N -> N -> N                                        -- Compound nouns where attribute inflects in state, case and number
     = attrN ;
   mkN : N -> A -> N
     = mkAN ;
   mkN : Number -> N -> A -> N
     = \num,n,a -> mkAPNumN n (A.PositA a) num ;
   mkN : N -> AP -> N
     = mkAPN
   } ;

  attrN : Number -> N -> N -> N = \num,n1,n2 -> n1 ** {
    s  = \\n,_,c => n1.s ! n ! Const ! c ;
    s2 = \\n,s,_ => n1.s2 ! num ! s ! Gen -- attribute doesn't change
                 ++ n2.s  ! num ! s ! Gen
                 ++ n2.s2 ! num ! s ! Gen} ;

  mkAN : N -> A -> N = \n,a -> mkAPN n (A.PositA a) ;

  mkAPN : N -> AP -> N = \n,ap -> n ** {
    s2 = \\num,s,c => n.s2 ! num ! s ! c
                   ++ ap.s ! n.h ! n.g ! num ! s ! c
    } ;

  mkAPNumN : N -> AP -> Number -> N = \n,ap,forceNum -> n ** {
    s2 = \\num,s,c => n.s2 ! num ! s ! c
                   ++ ap.s ! Hum ! n.g ! forceNum ! s ! c
    } ; -- Hum because we want to override the smartness in number agreement

  dualN : N -> N = \n -> n ** {isDual=True} ;

  proDrop : NP -> NP ; -- Force a NP to lose its string, only contributing with its agreement.

  mkPron : (_,_,_ : Str) -> PerGenNum -> Pron ;

  mkV = overload {
    mkV : (imperfect : Str) -> V
      = regV ;
    mkV : (root : Str) -> (perf,impf : Vowel) -> V  -- verb form I ; vowel = a|i|u
      = v1 ;
    mkV : (root : Str) -> VerbForm -> V             -- FormI .. FormX (no VII, IX) ; default vowels a u for I
      = formV ;
    mkV : V -> (particle : Str) -> V = \v,p ->
      v ** { s = \\vf => v.s ! vf ++ p } ;
    } ;

  regV : Str -> V = \wo ->
    let rau : Str * Vowel * Vowel =
    case wo of {
      "يَ" + fc + "ُ" + l => <fc+l, a, u> ;
      "يَ" + fc + "ِ" + l => <fc+l, a, i> ;
      "يَ" + fc + "َ" + l => <fc+l, a, a> ;
      f@? + "َ" + c@? + "ِ" + l  => <f+c+l, i, a> ;
      _ => Predef.error ("regV not applicable to" ++ wo)
    }
    in v1 rau.p1 rau.p2 rau.p3 ;

  v1 = \rootStr,vPerf,vImpf ->
    let { raw = v1' rootStr vPerf vImpf } in
    lin V { s = \\vf =>rectifyHmz (raw.s ! vf) } ;

  v1' : Str ->  Vowel -> Vowel -> Verb =
    \rootStr,vPerf,vImpf ->
    let root = mkRoot3 rootStr
     in case rootStr of {
          _          + "ّ"   => v1geminate rootStr vPerf vImpf ;
          ? + #hamza + #weak => v1doubleweak root ;
          ? + ?      + #weak => case vPerf of {
                                  i => v1defective_i root vImpf ;
                                  _ => v1defective_a root vImpf } ;
          ? + #weak + ?      => v1hollow root vImpf ;
          _                  => v1sound root vPerf vImpf } ;

  v2 =
    \rootStr ->
    let {
      root = mkRoot3 rootStr
    } in {
      s =
        case root.l of {
          #weak => (v2defective root).s;
          _       => (v2sound root).s
        };
      lock_V = <>
    };

  v3 =
    \rootStr ->
    let {
      tbc = mkRoot3 rootStr ;
    } in {
      s = (v3sound tbc).s  ;
      lock_V = <>
    };

  v4 =
    \rootStr ->
    let root : Root3 = mkRoot3 rootStr ;
        verb : Verb  = case rootStr of {
          ? + #hamza + #weak => v4doubleweak root ;
          ? + #weak + ?      => v4hollow root ;
          _          + #weak => v4defective root  ;
          _                  => v4sound root } ;
    in lin V verb ;


  v5 =
    \rootStr ->
    let { raw = v5' rootStr } in
    { s = \\vf =>
        case rootStr of {
          _ + #hamza + _ => rectifyHmz(raw.s ! vf);
          _ => raw.s ! vf
        };
      lock_V = <>
    };

  v5' : Str -> V =
    \rootStr ->
    let {
      nfs = mkRoot3 rootStr ;
    } in {
      s = (v5sound nfs).s  ; lock_V = <>
    };

  v6 =
    \rootStr ->
    let {
      fqm = mkRoot3 rootStr ;
    } in {
      s = (v6sound fqm).s  ;
      lock_V = <>
    };

  v7 =
    \rootStr ->
    let {
      fcl = mkRoot3 rootStr ;
      v7fun = v7geminate ; -- TODO add rest
      } in lin V (v7fun fcl) ;

  v8 =
    \rootStr ->
    let {
      rbT = mkRoot3 rootStr ;
      v8fun = case rbT.f of {
                ("و"|"ي"|"ّ") => v8assimilated ;
                _ =>
                  case rbT.c of {
                        #weak => v8hollow ;
                        _     => v8sound }}
      } in lin V (v8fun rbT) ;

  v10 =
    \rootStr ->
    let {
      rbT = mkRoot3 rootStr ;
      v10fun : Root3 -> Verb = case rootStr of {
                ? + #weak + ? => v10hollow ;
                ? + ? + #weak => v10defective ;
                _             => v10sound }
      } in lin V (v10fun rbT) ;

  reflV v = lin V (ResAra.reflV v) ;

  mkFullN nsc gen spec = lin N
    { s = nsc; --NTable
      s2 = emptyNTable;
      g = gen;
      h = spec;
      isDual = False
    };

  brkN' : Str -> Str -> Str -> Gender -> Species -> N =
    \root,sg,pl,gen,spec ->
    let { kitAb = case root of {
            ? + ? + "ي" => mkDefectiveAlifMaqsura (mkPat sg) (mkRoot3 root) ;
            _           => mkWord sg root };
          kutub = mkWord pl root
    } in mkFullN (reg kitAb kutub) gen spec;

  brkN root sg pl gen spec =
    let { raw = brkN' root sg pl gen spec} in raw **
    { s = \\n,d,c =>
        case root of {
          _ + #hamza + _ => rectifyHmz(raw.s ! n ! d ! c);
          _ => raw.s ! n ! d ! c
        }
    };

  sdfN =
    \root,sg,gen,spec -> let {
      kalimaStr = case root of {
        x@? + y@? + "ي" => mkDefectiveAlifMaqsura (mkPat sg) (mkRoot3 root) ;
        _               => mkWord sg root } ;
      kalimaRaw : NTable = case gen of {
            Fem  =>    sndf kalimaStr ;
            Masc => sgMsndf kalimaStr } ; -- TODO this isn't actually the case of gender, add an argument
      kalima : NTable = \\n,d,c =>
         rectifyHmz (kalimaRaw ! n ! d ! c)
    } in mkFullN kalima gen spec;

  sdmN =
    \root,sg,gen,spec ->
    let { mucallim = mkWord sg root;
    } in mkFullN (sndm mucallim) gen spec;

  mkFullPN = \str,gen,species ->
    { s = \\c => str + indecl!c ;
      g = gen;
      h = species;
      lock_PN = <>
    };

  mkN2 = overload {
    mkN2 : N -> Preposition -> N2 = prepN2 ;
    mkN2 : N -> Str -> N2 = \n,s -> prepN2 n (mkPreposition s);
    mkN2 : N -> N2 = \n -> lin N2 (n ** {c2 = noPrep}) ;
    mkN2 : Str -> N2 = \str -> lin N2 (smartN str ** {c2 = noPrep})
  } ;

  prepN2 : N -> Preposition -> N2 = \n,p -> lin N2 (n ** {c2 = p}) ;

  mkN3 = overload {
    mkN3 : N -> Preposition -> Preposition -> N3 = \n,p,q ->
      lin N3 (n ** {c2 = p ; c3 = q}) ;
    mkN3 : N -> Str -> Str -> N3 = \n,p,q ->
      lin N3 (n ** {c2 = mkPreposition p ; c3 = mkPreposition q}) ;
  } ;

  mkConj = overload {
    mkConj : Str -> Conj = \s -> lin Conj {s1 = [] ; s2 = s ; n = Sg} ;
    mkConj : Str -> Str -> Conj = \s1,s2 -> lin Conj {s1 = s1 ; s2 = s2 ; n = Sg} ;
    mkConj : Str -> Number -> Conj = \s,n -> lin Conj {s1 = [] ; s2 = s ; n = n} ;
    mkConj : Str -> Str -> Number -> Conj = \s1,s2,n -> lin Conj {s1 = s1 ; s2 = s2 ; n = n}
    } ;

  mkPron : (_,_,_ : Str) -> PerGenNum -> Pron = \ana,nI,I,pgn ->
    lin Pron (ResAra.mkPron ana nI I pgn) ;

  proDrop : NP -> NP = \np -> lin NP (ResAra.proDrop np) ;


  mkQuant7 : (_,_,_,_,_,_,_ : Str) -> State -> Quant =
    \hava,havihi,havAn,havayn,hAtAn,hAtayn,hA'ulA,det -> lin Quant (baseQuant **
    { s = \\n,s,g,c =>
        case <s,g,c,n> of {
          <_,Masc,_,Sg>  => hava;
          <_,Fem,_,Sg>   => havihi;
          <_,Masc,Nom,Dl>=> havAn;
          <_,Masc,_,Dl>  => havayn;
          <_,Fem,Nom,Dl> => hAtAn;
          <_,Fem,_,Dl>   => hAtayn;
          <Hum,_,_,Pl>   => hA'ulA;
          _              => havihi
        };
      d = det
    });

  mkQuant3 : (_,_,_ : Str) -> State -> Quant =
    \dalika,tilka,ula'ika,det -> lin Quant (baseQuant **
    { s = \\n,s,g,c =>
        case <s,g,c,n> of {
          <_,Masc,_,Sg>  => dalika;
          <_,Fem,_,Sg>   => tilka;
          <Hum,_,_,_>   => ula'ika;
          _              => tilka
        };
      d = det
    });

  brkA : (root,sg,pl : Str) -> Adj = \root,sg,pl ->
    let jadId  = mkWord sg root ;
        jadIda = jadId + "َة" ;
        judud  = mkWord pl root ;
        akbar  = mkWord "أَفعَل" root ;
        mascTbl = reg jadId judud ;
        femTbl = reg jadIda judud ;
     in { s = table {
             APosit Masc n d c => rectifyHmz (mascTbl ! n ! d ! c) ;
             APosit Fem  n d c => rectifyHmz (femTbl ! n ! d ! c) ;
             AComp d c         => rectifyHmz (indeclN akbar ! d ! c) }
        } ;

  degrA : (posit,compar,plur : Str) -> A
    = \posit,compar,plur -> lin A {s = clr posit compar plur} ;

  idaafaA : N -> A -> A = \ghayr,tayyib -> tayyib ** {
    s = table {
      APosit g n d c => ghayr.s ! n ! Const ! c ++ tayyib.s ! APosit g n d c ;
      AComp d c => ghayr.s ! Sg ! Const ! c ++ tayyib.s ! AComp d c }
    } ;

  sndA root pat =
    let  raw = sndA' root pat in {
      s = \\af =>
        case root of {
          _ + #hamza + _ => rectifyHmz(raw.s ! af);
        _ => raw.s ! af
        }
    };

  sndA' : Str -> Str -> Adj =
    \root,pat ->
    let { kabIr = mkWord pat root;
          akbar = mkWord "أَفعَل" root
    } in {
      s = table {
        APosit g n d c  => positAdj kabIr ! g ! n ! d ! c ;
        AComp d c => indeclN akbar ! d ! c
        }
    };

  irregFemA : (masc : A) -> (fem : A) -> A = \m,f -> m ** {
    s = table {
      APosit Masc n d c => m.s ! APosit Masc n d c ;
      APosit Fem  n d c => f.s ! APosit Masc n d c ; -- The fem. adjective is built as if the irregular fem. forms were Masc. This is on purpose.
      x => m.s ! x }
    } ;

  nisbaA : Str -> Adj = \Haal ->
    let Haaliyy = Haal + "ِيّ" in {
      s = table {
        APosit g n d c  => positAdj Haaliyy ! g ! n ! d ! c ;
        AComp d c => "أَكْثَر" ++ indeclN Haaliyy ! d ! c
        }
    } ;

  clrA root =
    let { eaHmar = mkWord "أَفعَل" root;
          HamrA' = mkWord "فَعلاء" root;
          Humr   = mkWord "فُعل" root
    } in {
      s = clr eaHmar HamrA' Humr;
    };

  mkA2 = overload {
    mkA2 : A -> Preposition -> A2 = prepA2 ;
    mkA2 : A -> Str -> A2 = \a,p -> prepA2 a (mkPreposition p)
    } ;

  prepA2 : A -> Preposition -> A2 = \a,p -> lin A2 (a ** {c2 = p}) ;

  mkAdv x = lin Adv (ss x) ;
  mkAdV x = lin AdV (ss x) ;
  mkAdA x = lin AdA (ss x) ;
  mkInterj x = lin Interj (ss x) ;

  mkSubj = overload {
    mkSubj : Str -> Subj = \s -> lin Subj {s = s ; o = Subord} ;
    mkSubj : Str -> Order -> Subj = \s,o -> lin Subj {s = s ; o = o} ;
  } ;

  dirV2 v = prepV2 v (casePrep acc) ;

  mkV3 = overload {
    mkV3 : V -> Preposition -> Preposition -> V3 = \v,p,q ->
      lin V3 (prepV3 v p q) ;
    mkV3 : V -> Str -> Str -> V3 = \v,p,q ->
      lin V3 (v ** {s = v.s ; c2 = mkPreposition p ; c3 = mkPreposition q})
    } ;

  prepV3 : V -> Preposition -> Preposition -> Verb3 = \v,p,q ->
    v ** {s = v.s ; c2 = p ; c3 = q} ;

  dirV3 = overload {
    dirV3 : V -> Preposition -> V3 = \v,p -> mkV3 v (casePrep acc) p ;
    dirV3 : V -> Str -> V3 = \v,s -> mkV3 v (casePrep acc) (mkPreposition s)
    } ;

  dirdirV3 v = dirV3 v (casePrep acc) ;

  mkVS = overload {
    mkVS : V -> VS = \v -> lin VS (v ** {o = Subord; s2 = []}) ;
    mkVS : V -> Str -> VS =  \v,s -> lin VS (v ** {o = Subord; s2 = s}) 
    } ;
  mkVQ v = lin VQ v ;

  regVV : V -> VV = \v -> lin VV v ** {c2 = mkPreposition "أَنْ" ; sc = noPrep} ;
  c2VV : V -> Str -> VV = \v,prep -> regVV v ** {c2 = mkPreposition prep ; sc = noPrep} ;
  prepVV : V -> Preposition -> VV = \v,prep -> regVV v ** {c2=prep; sc=noPrep} ;
  prep2VV : V -> (_,_ : Preposition) -> VV = \v,p1,p2 -> regVV v ** {c2=p1; sc=p2} ;
  V0 : Type = V ;
----  V2S, V2V, V2Q, V2A : Type = V2 ;
  AS, A2S, AV : Type = A ;
  A2V : Type = A2 ;

  mkV0  v = v ;
  mkV2S v p = lin V2S (prepV2 v (mkPreposition p)) ;
  mkV2V = overload {
    mkV2V : V -> Str -> Str -> V2V = \v,p,q ->
      lin V2V (prepV3 v (mkPreposition p) (mkPreposition q) ** {sc = noPrep}) ;
    mkV2V : V -> Preposition -> Preposition -> V2V = \v,p,q ->
      lin V2V (prepV3 v p q ** {sc = noPrep}) ;
    mkV2V : VV -> Preposition -> V2V = \vv,p ->
      lin V2V (vv ** {c2 = p ; c3 = vv.c2}) ;
  } ;
  mkVA  v   = v ** {lock_VA = <>} ;
  mkV2A v p = lin V2A (prepV2 v (mkPreposition p));
  mkV2Q v p = lin V2Q (prepV2 v (mkPreposition p));

  mkAS,
  mkAV = \a -> a ;
  mkA2S,
  mkA2V = \a,p -> prepA2 a (mkPreposition p) ;



smartN : Str -> N = \s -> case s of {
  _ + "ة" => mkFullN (sndf s) Fem NoHum ;
  _ + "ة" + #vow => mkFullN (sndf s) Fem NoHum ;
  _ => mkFullN (sndm s) Masc NoHum
  } ;

smartPN : Str -> PN = \s -> case last s of {
  "ة" => mkFullPN s Fem Hum ;
  _ => mkFullPN s Masc Hum
  } ;

formV : (root : Str) -> VerbForm -> V = \s,f -> case f of {
   FormI   => v1 s a u ;
   FormII  => v2 s ;
   FormIII => v3 s ;
   FormIV  => v4 s ;
   FormV   => v5 s ;
   FormVI  => v6 s ;
   FormVII  => v7 s ;
   FormVIII => v8 s ;
   FormX   => v10 s
   } ;

param VerbForm =
  FormI | FormII |  FormIII |  FormIV |  FormV |  FormVI | FormVII | FormVIII | FormX ;

} ;
