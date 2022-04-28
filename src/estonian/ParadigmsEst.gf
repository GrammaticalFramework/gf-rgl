--1 Estonian Lexical Paradigms
--
-- Based on the Finnish Lexical Paradigms by Aarne Ranta 2003--2008
--
-- This is an API to the user of the resource grammar
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
--
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API and $Structural.gf$.
--
-- The main difference with $MorphoEst.gf$ is that the types
-- referred to are compiled resource grammar types. We have moreover
-- had the design principle of always having existing forms, rather
-- than stems, as string arguments of the paradigms.
--
-- The structure of functions for each word class $C$ is the following:
-- there is a polymorphic constructor $mkC$, which takes one or
-- a few arguments. In Estonian, one argument is enough in 90% of
-- cases in average.
--
-- @author Inari Listenmaa
-- @author Kaarel Kaljurand
-- @version 2013-10-21

resource ParadigmsEst = open
  (Predef=Predef),
  Prelude,
  MorphoEst,
  (ResEst=ResEst),
  HjkEst,
  CatEst
  in {

  flags optimize=noexpand ; coding=utf8;

--2 Parameters
--
-- To abstract over gender, number, and (some) case names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResEst$.

oper
  Number   : Type ;

  singular : Number ;
  plural   : Number ;

  Case        : Type ;
  nominative  : Case ; -- e.g. "karp"
  genitive    : Case ; -- e.g. "karbi"
  partitive   : Case ; -- e.g. "karpi"
  illative    : Case ; -- e.g. "karbisse/karpi"
  inessive    : Case ; -- e.g. "karbis"
  elative     : Case ; -- e.g. "karbist"
  allative    : Case ; -- e.g. "karbile"
  adessive    : Case ; -- e.g. "karbil"
  ablative    : Case ; -- e.g. "karbilt"
  translative : Case ; -- e.g. "karbiks"
  terminative : Case ; -- e.g. "karbini"
  essive      : Case ; -- e.g. "karbina"
  abessive    : Case ; -- e.g. "karbita"
  comitative  : Case ; -- e.g. "karbiga"

  InfForm : Type ;

  infDa : InfForm ; -- e.g. "lugeda"
  infDes : InfForm ; -- e.g. "lugedes"
  infMa : InfForm ; -- e.g. "lugema"
  infMas : InfForm ; -- e.g. "lugemas"
  infMaks : InfForm ; -- e.g. "lugemaks"
  infMast : InfForm ;  -- e.g. "lugemast"
  infMata : InfForm ; -- e.g. "lugemata"
  infMine : InfForm ; -- e.g. "lugemine"

-- The following type is used for defining *rection*, i.e. complements
-- of many-place verbs and adjective. A complement can be defined by
-- just a case, or a pre/postposition and a case.

  prePrep     : Case -> Str -> Prep ;  -- preposition, e.g. abessive "ilma"
  postPrep    : Case -> Str -> Prep ;  -- postposition, e.g. genitive "taga"
  postGenPrep :         Str -> Prep ;  -- genitive postposition, e.g. "taga"
  casePrep    : Case ->        Prep ;  -- just case, e.g. adessive


--2 Conjunctions, adverbs


  mkAdv : Str -> Adv ;
  mkAdV : Str -> AdV ;
  mkAdN : Str -> AdN ;
  mkAdA : Str -> AdA ;

  mkConj : overload {
    mkConj : Str -> Conj ; -- just one word, default number Sg: e.g. "ja"
    mkConj : Str -> Number -> Conj ; --just one word + number: e.g. "ja" Pl
    mkConj : Str -> Str -> Conj ; --two words, default number: e.g. "nii" "kui"
    mkConj : Str -> Str -> Number -> Conj ; --two words + number: e.g. "nii" "kui" Pl
  } ;

  mkPConj : Str -> PConj ;

--2 Nouns

oper

-- The regular noun heuristic takes just one form (singular
-- nominative) and analyses it to pick the correct paradigm.
-- If the 1-argument paradigm does not give the correct result,
-- one can try and give 2, 3, 4, or 6 forms.

  mkN : overload {
    mkN : (ema : Str) -> N ;  -- predictable nouns, covers 90%
    mkN : (tukk,tuku : Str) -> N ; -- sg nom,gen: unpredictable stem vowel
    mkN : (tukk,tuku,tukku : Str) -> N ; -- sg nom,gen,part
    mkN : (pank,panga,panka,panku : Str) -> N ; -- sg nom,gen,part, pl.part

    mkN : (oun,ouna,ouna,ounasse,ounte,ounu : Str) -> N ; -- worst case, 6 forms
--    mkN : (pika : Str) -> (juna  : N) -> N ; -- compound with invariable prefix
--    mkN : (oma : N)    -> (tunto : N) -> N ; -- compound with inflecting prefix
  } ;

-- Nouns used as functions need a case, of which the default is
-- the genitive.

  mkN2 : overload {
    mkN2 : N -> N2 ;        -- relational noun with genitive
    mkN2 : N -> Prep -> N2  -- relational noun another prep.
    } ;

  mkN3  : N -> Prep -> Prep -> N3 ; -- relation with two complements

-- Proper names can be formed by using declensions for nouns.
-- The plural forms are filtered away by the compiler.

  mkPN : overload {
    mkPN : Str -> PN ;  -- predictable noun made into name
    mkPN : N -> PN      -- any noun made into name
    } ;

--2 Adjectives

-- Non-comparison one-place adjectives are just like nouns.
-- The regular adjectives are based on $regN$ in the positive.
-- Comparison adjectives have three forms.
-- The comparative and the superlative
-- are always inflected in the same way, so the nominative of them is actually
-- enough (TODO: confirm).

  mkA : overload {
    mkA : Str -> A ;  -- regular noun made into adjective
    mkA : N -> A ;    -- any noun made into adjective
    mkA : N -> (infl : Infl) -> A ; -- noun made into adjective, agreement type specified
    mkA : N -> (parem, parim : Str) -> A ; -- deviating comparison forms
  } ;

-- Two-place adjectives need a case for the second argument.

  mkA2 : A -> Prep -> A2  -- e.g. "vihane" (postGenPrep "peale")
    = \a,p -> lin A2 (a ** {c2 = p}) ;

  invA : Str -> A ; -- invariable adjectives, such as genitive attributes ; no agreement to head, no comparison forms.

--2 Verbs
--
-- The grammar does not cover the quotative mood and some nominal
-- forms. One way to see the coverage is to linearize a verb to
-- a table.
-- The worst case needs eight forms, as shown in the following.

  mkV : overload {
    mkV : (lugema : Str) -> V ;     -- predictable verbs, covers 90 %
    mkV : (lugema,lugeda : Str) -> V ; -- ma infinitive, da infinitive
    mkV : (lugema,lugeda,loeb : Str) -> V ; -- ma, da, present sg 3
    mkV : (lugema,lugeda,loeb,loetakse : Str) -> V ; --ma, da, pres sg 3, pres passive
    mkV : (tegema,teha,teeb,tehakse,tehke,tegi,teinud,tehtud : Str) -> V ; -- worst-case verb, 8 forms
    mkV : (saama : V) -> (aru : Str) -> V ; -- multi-word verbs
  } ;

-- All the patterns above have $nominative$ as subject case.
-- If another case is wanted, use the following.

  caseV : Case -> V -> V ;  -- deviating subj. case, e.g. allative "meeldima"

-- The verbs "be" and "go" are special.

  vOlema : V ; -- the verb "be"
  vMinema : V ; -- the verb "go"


--3 Two-place verbs
--
-- Two-place verbs need an object case, and can have a pre- or postposition.
-- The default is direct (accusative) object. There is also a special case
-- with case only. The string-only argument case yields a regular verb with
-- accusative object.

  mkV2 : overload {
    mkV2 : Str -> V2 ;  -- predictable direct transitive
    mkV2 : V -> V2 ;    -- direct transitive
    mkV2 : V -> Case -> V2 ; -- complement just case
    mkV2 : V -> Prep -> V2 ; -- complement pre/postposition
    } ;


--3 Three-place verbs
--
-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3     : overload {
     mkV3 : V -> Prep -> Prep -> V3 ;  -- e.g. rääkima, allative, elative
     mkV3 : V                 -> V3 ;
     mkV3 : Str               -> V3 ;  -- string, default cases accusative + allative
  } ;
  dirV3    : V -> Case -> V3 ;          -- liigutama, (accusative), illative
  dirdirV3 : V         -> V3 ;          -- andma, (accusative), (allative)


--3 Other complement patterns
--
-- Verbs and adjectives can take complements such as sentences,
-- questions, verb phrases, and adjectives.

  mkV0  : V -> V0 ; --%
  mkVS  : overload {
    mkVS : V -> VS ;
    mkVS : Str -> VS ;
  } ;
  mkV2S : overload {
     mkV2S : V -> Prep -> V2S ; -- e.g. "ütlema" allative
     mkV2S : Str -> V2S ; --default (mkV foo) allative
  } ;
  mkVV  : overload {
     mkVV : V -> VV ;  -- e.g. "hakkama"
     mkVV : Str -> VV ;
  } ;
  mkVVf : V -> InfForm -> VV ; -- e.g. "hakkama" infMa
  mkV2V : overload {
    mkV2V : V -> Prep -> V2V ;  -- e.g. "käskima" adessive
    mkV2V : Str -> V2V ;  -- e.g. "käskima" adessive
  } ;
  mkV2Vf : V -> Prep -> InfForm -> V2V ; -- e.g. "keelama" partitive infMast

  mkVA  : overload {
     mkVA : V -> Prep -> VA ; -- e.g. "muutuma" translative
     mkVA : Str       -> VA ; -- string, default case translative
  } ;

  mkV2A : overload {
     mkV2A : V -> Prep -> Prep -> V2A ; -- e.g. "värvima" genitive translative
     mkV2A : Str               -> V2A ; -- string, default cases genitive and translative
  } ;

  mkVQ  : overload {
     mkVQ : V -> VQ ;
     mkVQ : Str -> VQ ;
  } ;
  mkV2Q : V -> Prep -> V2Q ; -- e.g. "küsima" ablative

  mkAS  : A -> AS ; --%
  mkA2S : A -> Prep -> A2S ; --%
  mkAV  : A -> AV ; --%
  mkA2V : A -> Prep -> A2V ; --%

-- Notice: categories $AS, A2S, AV, A2V$ are just $A$,
-- and the second argument is given
-- as an adverb. Likewise
-- $V0$ is just $V$.

  V0 : Type ; --%
  AS, A2S, AV, A2V : Type ; --%

--.
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  Case = MorphoEst.CasePlus ;
  Number = MorphoEst.Number ;

  singular = Sg ;
  plural = Pl ;

  nominative  = Nominative ;
  genitive    = Genitive ;
  partitive   = Partitive ;
  illative    = Illative ;
  inessive    = Inessive ;
  elative     = Elative ;
  allative    = Allative ;
  adessive    = Adessive ;
  ablative    = Ablative ;
  translative = Translative ;
  terminative = Terminative ;
  essive      = Essive ;
  abessive    = Abessive ;
  comitative  = Comitative ;

  -- IL 2022-04: after introducing stem+suffixes, 4 other cases have just genitive stems.
  -- isActuallyGenitive is needed for those mkN2 and mkN3 instances that take a Prep as an argument,
  -- and actual Gen gets isPre=True, and those with genitive stem+suffix should get False.
  -- This is confusing and error-prone, consider restructuring/renaming things later.
  isActuallyGenitive : MorphoEst.CasePlus -> Bool = \c -> case c of {
    {c = MorphoEst.Gen ; suf = ""} => True ;
    _ => False
  } ;

  -- combination of stem + suffix, e.g. infDes = {stem = InfD ; suf = "es"} ;
  InfForm = ResEst.InfForms ;
  infDa = InfDa ; infMa = InfMa ; infMast = InfMast ;
  infDes = InfDes ; infMas = InfMas ; infMaks = InfMaks ; infMata = InfMata ; infMine = InfMine ;

  mkPrep : (isPre : Bool) -> Case -> Str ->  Prep = \isPre,c,p -> lin Prep {
    c = casep2npformp c ;
    s = p ;
    isPre = isPre
    } ;
  prePrep  : Case -> Str -> Prep = mkPrep True ;
  postPrep : Case -> Str -> Prep = mkPrep False ;
  postGenPrep : Str -> Prep = postPrep genitive ;

  -- The Prep's isPre field is used in a special (hacky) way in mkN3 and mkN2.
  -- Used to be able to match whether the Prep's case is Gen, but now several
  -- Preps use the genitive stem, so we need to check if it's actually genitive.
  casePrep : Case -> Prep = \c -> mkPrep (isActuallyGenitive c) c [] ;

  -- NPAcc is different, it's not formed from a Case(Plus)
  accPrep : Prep = lin Prep {
    c = case2npformp NPAcc ;
    s = [] ;
    isPre = True
    } ;

  mkAdv : Str -> Adv = \str -> lin Adv (ss str) ;
  mkAdV : Str -> AdV = \str -> lin AdV (ss str) ;
  mkAdN : Str -> AdN = \str -> lin AdN (ss str) ;
  mkAdA : Str -> AdA = \str -> lin AdA (ss str) ;

  mkConj = overload {
    mkConj : Str -> Conj = \ja -> lin Conj ((sd2 "" ja) ** {n = Sg}) ;
    mkConj : Str -> Number -> Conj = \ja,num -> lin Conj ((sd2 "" ja) ** {n = num}) ;
    mkConj : Str -> Str -> Conj = \nii,kui -> lin Conj ((sd2 nii kui) ** {n = Sg}) ;
    mkConj : Str -> Str -> Number -> Conj = \nii,kui,num -> lin Conj ((sd2 nii kui) ** {n = num}) ;
  } ;

  mkPConj s = lin PConj (ss s) ;

  mkN = overload {
    mkN : (nisu : Str) -> N = mk1N ;
    mkN : (link,lingi : Str) -> N = mk2N ;
    mkN : (tukk,tuku,tukku : Str) -> N = mk3N ;
    mkN : (paat,paadi,paati,paate : Str) -> N = mk4N ;
    mkN : (oun,ouna,ouna,ounasse,ounte,ounu : Str) -> N = mk6N ;

    mkN : (sora : Str) -> (tie : N) -> N = mkStrN ;
    mkN : (oma,tunto : N) -> N = mkNN ;
  } ;

  -- Adjective forms (incl. comp and sup) are derived from noun forms
  mk1A : Str -> A = \suur ->
    let aforms = aForms2A (nforms2aforms (hjk_type suur))
     in lin A (aforms ** {infl = Regular}) ;

  mkNA : N -> A = \suur ->
    let aforms = aForms2A (nforms2aforms (n2nforms suur)) ;
     in lin A (aforms ** {infl = Regular}) ;

  mk1N : (link : Str) -> N = \s -> lin N (nForms2N (hjk_type s)) ;

  -- mk2N, mk3N, mk4N make sure that the user specified forms end up in the paradigm,
  -- even though the rest is wrong
  mk2N : (link,lingi : Str) -> N = \link,lingi ->
    let nfs : NForms = (nForms2 link lingi) ;
        nfs_fixed : NForms = table {
                0 => link ;
                1 => lingi ;
                2 => nfs ! 2 ;
                3 => nfs ! 3 ;
                4 => nfs ! 4 ;
                5 => nfs ! 5
        } ;
    in lin N (nForms2N nfs_fixed) ;


  mk3N : (tukk,tuku,tukku : Str) -> N = \tukk,tuku,tukku ->
    let nfs : NForms = (nForms3 tukk tuku tukku) ;
        nfs_fixed : NForms = table {
                0 => tukk ;
                1 => tuku ;
                2 => tukku ;
                3 => nfs ! 3 ;
                4 => nfs ! 4 ;
                5 => nfs ! 5
        } ;
    in lin N (nForms2N nfs_fixed) ;


  mk4N : (paat,paadi,paati,paate : Str) -> N = \paat,paadi,paati,paate ->
    let nfs : NForms = (nForms4 paat paadi paati paate) ;
        nfs_fixed : NForms = table {
                0 => paat ;
                1 => paadi ;
                2 => paati ;
                3 => nfs ! 3 ;
                4 => nfs ! 4 ;
                5 => paate
        } ;
    in lin N (nForms2N nfs_fixed) ;


  mk6N : (oun,ouna,ouna,ounasse,ounte,ounu : Str) -> N =
      \a,b,c,d,e,f -> lin N (nForms2N (nForms6 a b c d e f)) ;

  mkStrN : Str -> N -> N = \sora,tie -> tie ** {
    s = \\c => sora + tie.s ! c
    } ;
  mkNN : N -> N -> N = \oma,tunto -> tunto ** {
    s = \\c => oma.s ! c + tunto.s ! c ;
    } ; ---- TODO: oma in possessive suffix forms


  -- This rule uses the additional information that can be derived from the
  -- singular genitive:
  --   - stem vowel (the sg gen always ends with a vowel)
  --   - e-deletion (laager/laagri vs paber/paberi)
  --   - adjectives with different genitive ending compared to nouns
  --     vahe/vaheda -> vahedat
  --   - type VII (tõuge)
  nForms2 : (_,_ : Str) -> NForms = \link,lingi ->
    let
      i = last lingi ;
      reegl = init lingi ;
    in
      case <link,lingi> of {
        -- e-deletion
        <_ + #c + "el", _ + #c + "li"> => hjk_type_IVb_audit1 link reegl ;
        <_ + #c + "er", _ + #c + "ri"> => hjk_type_IVb_audit1 link reegl ;
        <_ + #c + "el", _ + #c + "eli"> => hjk_type_IVb_audit link i ;
        <_ + #c + "er", _ + #c + "eri"> => hjk_type_IVb_audit link i ;

        -- This applies only to adjectives.
        -- If genitive just adds 'da' to the nominative, then construct
        -- the paradigm using IVa_aasta, giving it the genitive as the argument.
        -- We assume here that the nominative is overridden by the calling rule.
        -- Example: vahe, vaheda, vahedaT, vahedaSSE, vahedaTE, vahedaID
        <_ + "e", _ + "eda"> => hjk_type_IVa_aasta lingi ;

        -- More specific VII rules (which work reliably)
        -- These cannot be easily integrated into 'stronger'.
        <_ + "e", _ + #c + "me"> => hjk_type_VII_touge2 link lingi ;
        <_ + "se", _ + "ske"> => hjk_type_VII_touge2 link lingi ;
        <_ + "re", _ + "rde"> => hjk_type_VII_touge2 link lingi ;
        <_ + #v + "e", _ + "de"> => hjk_type_VII_touge2 link lingi ;--riie:riide


        -- This is not allowed in GF (not linear)
        --<stronger + "e", stronger + "e"> => hjk_type2 link i ;
        --<_ + "e", stronger + "e"> => hjk_type_VII_touge2 link lingi ;

        -- General VII rule
        -- If both forms end with 'e' then we check if the stronger
        -- form of nominative equals the given genitive. In this case
        -- there is reason to believe that type VII applies.
        -- We additionally require that both forms are different.
        -- TODO: this is not always ortographically visible: makse -> `makse
        <_ + "e", _ + "e"> =>
            let
                stronger = stronger_noun (init link) ;
                noChange = pbool2bool (Predef.eqStr link lingi) ;
                equal = pbool2bool (Predef.eqStr stronger reegl)
            in case <noChange, equal> of {
                <False, True> => hjk_type_VII_touge2 link lingi ;
                            _ => hjk_type2 link i
            } ;


        -- Some commented out experiments follow
        --improved total count a little, but introduced new errors
        --not recommended, not stable and productive word class
        --<_ + "i", _ + "e">  => dMeri link lingi ;

        --introduced a couple of errors, "aine" recognized as "kõne"
        --<_ + "ne", _ + "ne">  => hjk_type_III_ratsu link ;

        -- Selecting the correct vowel for IVa_audit.
        -- visin/visina, pidžin/pidžini
        -- TODO: we could cover more cases here, e.g. tudeng/tudengi
        <_ + #c + "in", _ + #c + "in" + #v> =>
            case (syl_type link) of {
                S2 => hjk_type_IVb_audit link i ;
                 _ => hjk_type2 link i
            } ;

        -- catch all calls hjk_type with the correct stem vowel
        _ => hjk_type2 link i
      } ;

  nForms3 : (_,_,_ : Str) -> NForms = \tukk,tuku,tukku ->
    let u = last tuku ;
    in  case <tukk,tuku,tukku> of {

      -- koi/koi/koid
      <_ + #v + #v, _ + #v + #v, _ + #v + #v + "d"> => hjk_type_I_koi tukk ;

      -- ema/ema/ema
      <_ + #v, _ + #v, _ + #v> => hjk_type_II_ema tukk ;

      --distinguish between hammas and maakas
      <_+"as",_+"a",_+"ast"> => dHammas tukk tuku ;
      <_+"es",_+"e",_+"est"> => dHammas tukk tuku ;
      <_+"us",_+"u",_+"ust"> => dHammas tukk tuku ;
      <_+"as",_,_+"at"> => hjk_type_IVb_maakas tukk ;

      <_ + "ik", _ + "iku", _ + "ikku"> => hjk_type_VI_imelik tukk ; --imelik:_:imelikku caught here

      <_ + "ud", _ + "u", _ + "ut"> => nForms2 tukk tuku ;  -- -nud/-tud participles are not like 'voolik'

      -- cases handled reliabl(ish) by 1- and 2-arg opers
      <_ + ("nd"|"el"|"er"), _, _> => nForms2 tukk tuku ;

      -- Type VI (sg gen and sg part end with a vowel)
      -- Note that we use the sg part as the argument for the constructor
      -- because it's more informative than sg nom, compare:
      -- link/lingi/linki
      -- kabinet/kabineti/kabinetti
      -- TODO: check that the genitive is actually weaker
      -- TODO: distinguish between the subtypes of VI
      -- TODO: do this also in nForms2
      <_ + #c, _ + #v, _ + #v> => hjk_type_VI_link2 (init tukku) u ;

      -- voolik/vooliku/voolikut
      <_ + #c, _ + #v, _ + #v + "t"> => hjk_type_IVb_audit tukk u ;

      _ => nForms2 tukk tuku
    } ;

  nForms4 : (_,_,_,_ : Str) -> NForms = \paat,paadi,paati,paate ->
    case <paat,paadi,paati,paate> of {
     -- distinguish between joonis and segadus
      <_ +("ne"|"s"),  _+"se", _+"st", _+"seid"> => hjk_type_Va_otsene paat ;
      <_ +("ne"|"s"),  _+"se", _+"st", _+"si"> => hjk_type_Vb_oluline paat ;

      <_ +"ne", _+"se", _+"set", _+"seid"> => nForms3 paat paadi paati ; -- -ne adjectives ('algne') are not like 'tõuge'

      --distinguish between kõne and aine
      <_ +"e", _+"e", _+"et", _+"sid"> => hjk_type_III_ratsu paat ;
      <_ +"e", _+"e", _+"et", _+"eid"> => hjk_type_VII_touge2 paat paadi ;

      _  => nForms3 paat paadi paati
      } ;

{-
  --Version that uses pl gen instead of pl part
  nForms4 : (_,_,_,_ : Str) -> NForms = \paat,paadi,paati,paatide ->
    case <paat,paadi,paati,paatide> of {
     -- pl gen can't distinguish between joonis and segadus
    --  <_ +("ne"|"s"),  _+"se", _+"st", _+"seid"> => hjk_type_Va_otsene paat ;
    --  <_ +("ne"|"s"),  _+"se", _+"st", _+"si"> => hjk_type_Vb_oluline paat ;

      --pl gen can distinguish between kõne and aine
      --plus side that any noun that is formed with 4-arg,
      --the user given forms are inserted to the paradigm,
      --and more forms are created from pl gen, none from pl part
      <_ +"e", _+"e", _+"et", _+"de"> => hjk_type_III_ratsu paat ;
      <_ +"e", _+"e", _+"et", _+"te"> => hjk_type_VII_touge2 paat paadi ;

      _  => nForms3 paat paadi paati
      } ;
-}

  mkN2 = overload {
    mkN2 : N -> N2 = \n -> mmkN2 n (casePrep genitive) ;
    mkN2 : N -> Prep -> N2 = mmkN2
    } ;

  mmkN2 : N -> Prep -> N2 = \n,c -> lin N2 (n ** {
    c2 = c ;
    isPre = mkIsPre c ;
    postmod = []
    }) ;

  mkN3 = \n,c,e -> lin N3 (n ** {
    c2 = c ; c3 = e ;
    isPre = mkIsPre c  ; -- matka Londonist Pariisi
    isPre2 = mkIsPre e ;          -- Suomen voitto Ruotsista
    }) ;

  mkIsPre : Prep -> Bool = \p -> case p.c.npf of {
    NPCase Gen => notB p.isPre ;  -- Jussin veli (prep is <Gen,"",True>, isPre becomes False)
    _ => True                     -- syyte Jussia vastaan, puhe Jussin puolesta
    } ;

  mkPN = overload {
    mkPN : Str -> PN = mkPN_1 ;
    mkPN : N -> PN = \s -> lin PN {s = \\c => s.s ! NCase Sg c} ;
    } ;

  mkPN_1 : Str -> PN = \s -> lin PN {s = \\c => (mk1N s).s ! NCase Sg c} ;

-- adjectives

  mkA = overload {
    mkA : Str -> A  = mkA_1 ;
    mkA : N -> A = \n -> noun2adjDeg n ** {infl = Regular} ;
    mkA : N -> (parem,parim : Str) -> A = regAdjective ;
    mkA : N -> (infl : Infl) -> A = \n,infl -> noun2adjDeg n ** {infl = infl} ;
    -- TODO: temporary usage of regAdjective1
    mkA : N -> (valmim,valmeim : Str) -> (infl : Infl) -> A =
		\n,c,s,infl -> (regAdjective1 n c s) ** {infl = infl} ;
  } ;

  invA balti = lin A {s = \\_,_ => balti ; infl = Invariable} ;

  mkA_1 : Str -> A = \x -> noun2adjDeg (mk1N x) ** {infl = Regular } ;

-- auxiliaries
  mkAdjective : (_,_,_ : Adj) -> A = \hea,parem,parim -> lin A ({
    s = table {
      Posit  => hea.s ;
      Compar => parem.s ;
      Superl => parim.s
      } ;
     infl = Regular ;
    }) ;

  -- Adjectives whose comparison forms are explicitly given.
  -- The inflection of these forms with the audit-rule always works.
  regAdjective : Noun -> Str -> Str -> A = \posit,compar,superl ->
    mkAdjective
      (noun2adj posit)
      (noun2adjComp False (nForms2N (hjk_type_IVb_audit compar "a")))
      (noun2adjComp False (nForms2N (hjk_type_IVb_audit superl "a"))) ;

  -- TODO: this is a temporary hack that converts A ~> Adjective.
  -- The caller needs this otherwise ** fails.
  -- This should be cleaned up but I don't know how (K).
  regAdjective1 : Noun -> Str -> Str -> Adjective = regAdjective ;

  -- Adjectives whose comparison forms can be derived from the sg gen.
  -- In case of comparative this fails only for 70 adjectives.
  -- Superlative is more complex, and does not always exist,
  -- e.g. lai -> laiem -> laiim? / laieim?
  -- See also: http://www.eki.ee/books/ekk09/index.php?p=3&p1=4&id=208
  -- Rather use "kõige" + Comp instead of the superlative.
  noun2adjDeg : Noun -> A = \kaunis ->
    let
      kauni = (kaunis.s ! NCase Sg Gen) ;
      -- Convert the final 'i' to 'e' for the superlative
      kaune : Str = case kauni of { kaun@(_) + "i" => kaun + "e" ; _ => kauni }
    in
    regAdjective kaunis (kauni + "m") (kaune + "im") ;


-- verbs

  mkV = overload {
    mkV : (lugema : Str) -> V = mk1V ;
    mkV : (lugema,lugeda : Str) -> V = mk2V ;
    mkV : (lugema,lugeda,loeb : Str) -> V = mk3V ;
    mkV : (lugema,lugeda,loeb,loetakse : Str) -> V = mk4V ;
    mkV : (tegema,teha,teeb,tehakse,tehke,tegi,teinud,tehtud : Str) -> V = mk8V ;
    mkV : (aru : Str) -> (saama : V) -> V = mkPV ; -- particle verbs
  } ;

  vforms2v : ResEst.VForms -> CatEst.V = \vfs -> lin V (vforms2verb vfs ** {sc = NPCase Nom}) ;
  mk1V : Str -> V = \s -> vforms2v (vForms1 s) ;
  mk2V : (_,_ : Str) -> V = \x,y -> vforms2v (vForms2 x y) ;
  mk3V : (_,_,_ : Str) -> V = \x,y,z -> vforms2v (vForms3 x y z) ;
  mk4V : (x1,_,_,x4 : Str) -> V = \a,b,c,d -> vforms2v (vForms4 a b c d) ;
  mk8V : (x1,_,_,_,_,_,_,x8 : Str) -> V = \a,b,c,d,e,f,g,h -> vforms2v (vForms8 a b c d e f g h) ;
  mkPV : (aru : Str) -> (saama : V) -> V = \aru,saama -> saama ** {p=aru} ;


  -- This used to be the last case: _ => Predef.error (["expected infinitive, found"] ++ ottaa)
  -- regexp example: ("" | ?) + ("a" | "e" | "i") + _ + "aa" =>
  vForms1 : Str -> VForms = \lugema ->
    let
      luge = Predef.tk 2 lugema ;
      loe = weaker luge ;
    in
    case lugema of {
      -- TS 49
      -- Small class of CVVma
      ? + ("ä"|"õ"|"i") + "ima" =>
        cKaima lugema ;  --käima,viima,võima
      ? + ("aa"|"ee"|"ää") + "ma" =>
        cSaama lugema ;  -- saama,jääma,keema
      ? + ("oo"|"öö"|"üü") + "ma" =>
        cJooma lugema ;  --jooma,looma,lööma,müüma,pooma,sööma,tooma

      -- TS 53
      _ + #c + #v + "elema" =>
        cTegelema lugema ; --not aelema

      -- TS 54
      -- Small class, just list all members
      ("tule"|"sure"|"pane") + "ma" =>
        cTulema lugema ;

      -- TS 55-57
      -- Consonant gradation
      -- Regular (55-56)'leppima' and irregular (57) 'lugema'
      -- For reliable results regarding consonant gradation, use mk3V
      _ + "ndima" =>
        cLeppima lugema ;
      _ + #lmnr + ("k"|"p"|"t"|"b") + ("ima"|"uma") =>
        cLeppima lugema ;
      _ + ("sk"|"ps"|"ks"|"ts"|"pl") + ("ima") => --|"uma") =>
        cLeppima lugema ;
      _ + ("hk"|"hm"|"hn"|"hr"|"ht") + ("ima") => --most *hCuma are TS 51 (muutuma)
        cLeppima lugema ;
      _ + #c + "ssima" => --weaker *ss = *ss; should be weaker Css = Cs
        cLugema lugema ;
      _ + ("pp"|"kk"|"tt"|"ss"|"ff"|"nn"|"mm"|"ll"|"rr") + ("ima"|"uma") =>
        cLeppima lugema ;

      -- TS 59 (petma, tapma)
      -- Use mk4V for TS 60 (jätma, võtma)
      ? + #v + ("tma"|"pma") =>
        cPetma lugema (luge + "etakse") ;
      -- TS 58 for rest that end tma (muutma,kartma,...)
      _ + "tma" =>
        cMuutma lugema ;

     -- TS 61 (laulma,kuulma,naerma,möönma)
     -- Default vowel e for lma, a for (r|n)ma.
     -- Other vowel with mk3V.
      _ + "lma" =>
        cKuulma lugema (loe + "eb") ;
      _ + ("r"|"n") + "ma" =>
        cKuulma lugema (loe + "ab") ;

      -- TS 63 (andma,hoidma)
      -- Other vowel than a (tundma~tunneb) with mk3V
      _ + "dma" =>
        cAndma lugema (loe + "ab") ;

      -- TS 62, 64 (tõusma,mõskma), default vowel e
      -- 62 alt form (jooksma,joosta) with mk2V
      -- Other vowel than e with mk3V
      _ + #c + "ma" =>
        cLaskma lugema (loe + "eb") ;

      -- TS 65 (pesema)
      #c + #v + "sema" =>
        cPesema lugema ;

      -- TS 66 (nägema)
      -- Small class, just list all members
      ("nägema"|"tegema") =>
        cNagema lugema ;

      -- TS 67-68 with mk2V
      -- no 100% way to distinguish from 50-52 that end in ama

      -- TS 69
      (?|"") + (?|"") + ? + "tlema" => --vestlema,mõtlema,ütlema; not õnnitlema
        cOmblema lugema ;
      _ + "tlema" =>
        cElama lugema ;
      _ + #c + "lema" =>
        cOmblema lugema ;

      -- TS 50-52
      -- Default case
      _ =>
        cElama lugema
    } ;

  vForms2 : (_,_ : Str) -> VForms = \petma,petta ->
    -- Arguments: ma infinitive, da infinitive
    -- Use this for the following cases:
    -- * 62 alt form (Csma, sta)
    -- * 50-52 (elama) recognized as 69 (õmblema)
    -- * 66 (nägema~näha)
    -- * 54 (tulema~tulla)
    -- * 67-68 (hüppama~hüpata)
    case <petma,petta> of {
      <_ + "ksma", _ + "sta"> => cJooksma petma ; --62 alt forms
      <_,          _ + "ata"> => cHyppama petma ; --67-68
      <_,          _ + "ha"> => cNagema petma ; --66
      <_,          _ + ("rra"|"lla"|"nna")> => cTulema petma ; --54
      <_ + #c + "lema",
       _ + #c + "leda"> => cElama petma ; --50-52 (õnnitlema) recognized as 69 (mõtlema)
       _ => vForms1 petma
      } ;

  vForms3 : (_,_,_ : Str) -> VForms = \taguma,taguda,taob ->
    -- Arguments: ma infinitive, da infinitive, b
    -- Use this for the following cases:
    -- * Irregular gradation (taguma~taob)
    -- * Non-detectable gradation (sattuma~satub ; pettuma~pettub)
    -- * Non-default vowel in b for TS 58-64 (laulma~laulab)
    case <taguma,taguda,taob> of {

      --to be sure about vowel in b
      <_ + "dma", _ + "da", _> => cAndma taguma taob ;
      <_, _ + #vv + #lmnr + "da", _> => cKuulma taguma taob ;
      <_, _ + #c + "ta", _> => cLaskma taguma taob ;

      --irregular gradation
      <_, _, (""|#c) + #c + #v + #v + "b"> => cLugema taguma ; --57

      --to be sure about consonant gradation
      <_ + #c + "lema", _, _> => vForms2 taguma taguda ; --catch "-Clema" first
      <_ + #v + "ma", _+"da", _> => cSattumaPettuma taguma taob ;

      <_,_,_> => vForms2 taguma taguda
    } ;

  vForms4 : (x1,_,_,x4 : Str) -> VForms = \jatma,jatta,jatab,jaetakse ->
    -- 4 forms needed to get full paradigm for regular verbs
    -- (source: http://www.eki.ee/books/ekk09/index.php?p=3&p1=5&id=227)
    -- regVForms in MorphoEst handles majority of these.
    -- Filter out known irregularities and give rest to regVForms.
    -- Not trying to match TS 49 ; can't separate käima (49) from täima (50), or detect compounds like taaslooma.
    case <jatma,jatta,jatab,jaetakse> of {
      <_,          _+("kka"|"ppa"|"tta"),
       _,          _+"takse"> => cPetma jatma jaetakse ;
      <_ + "dma",  _,
       _,          _+"takse"> => cAndma jatma jatab ;
      <_ + ("ts"|"ks"|"sk") + "ma", _,_,_> => cLaskma jatma jatab ;
      <_, _ + ("lla"|"nna"|"rra"), _, _> => cTulema jatma ;
      <_, _ + "ha", _, _> => cNagema jatma ;
      <_ + #v + "sema", _ + "sta", _, _> => cPesema jatma ;
      <_,_,_,_> => regVForms jatma jatta jatab jaetakse
    } ;

  caseV c v = v ** {sc = NPCase c.c} ;

  vOlema = lin V (verbOlema ** {sc = NPCase Nom}) ;
  vMinema = lin V (verbMinema ** {sc = NPCase Nom}) ;

  mk2V2 : V -> Prep -> V2 = \v,c -> lin V2 (v ** {c2 = c}) ;
  caseV2 : V -> Case -> V2 = \v,c -> mk2V2 v (casePrep c) ;
  dirV2 v = mk2V2 v accPrep ;


  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> dirV2 (mk1V s) ;
    mkV2 : V -> V2 = dirV2 ;
    mkV2 : V -> Case -> V2 = caseV2 ;
    mkV2 : V -> Prep -> V2 = mk2V2 ;
    } ;

  mk2V2 : V -> Prep -> V2 ;
  caseV2 : V -> Case -> V2 ;
  dirV2 : V -> V2 ;

  mkV3 = overload {
    mkV3 : V -> Prep -> Prep -> V3 = \v,p,q -> lin V3 (v ** {c2 = p ; c3 = q}) ;
    mkV3 : V  -> V3 = \v -> lin V3 (v ** {c2 = accPrep ; c3 = casePrep allative}) ;
    mkV3 : Str -> V3 = \str ->
      let v : V = mkV str
      in lin V3 (v ** {c2 = accPrep ; c3 = casePrep allative})
    } ;
  dirV3 v p = mkV3 v accPrep (casePrep p) ;
  dirdirV3 v = dirV3 v allative ;

  mkVS = overload {
    mkVS : V -> VS   = \v -> lin VS v ;
    mkVS : Str -> VS = \str -> let v : V = mkV str in lin VS v ;
  } ;
  mkVV = overload {
    mkVV : V -> VV   = \v -> mkVVf v infDa ;
    mkVV : Str -> VV = \str -> mkVVf (mkV str) infDa ;
  } ;
  mkVVf  v f = lin VV (v ** {vi = f}) ;
  mkVQ = overload {
    mkVQ : V   -> VQ = \v -> lin VQ v ;
    mkVQ : Str -> VQ = \str -> let v : V = mkV str in lin VQ v ;
  } ;

  V0 : Type = V ;
  AS, A2S, AV : Type = A ;
  A2V : Type = A2 ;

  mkV0 v = v ;
  mkV2S = overload {
    mkV2S : V -> Prep -> V2S   = \v,p -> lin V2S (mk2V2 v p) ;
    mkV2S : Str -> V2S = \str ->
      let v : V = mkV str
      in lin V2S (mk2V2 v (casePrep allative))
  } ;

  mkV2V = overload {
    mkV2V : V -> Prep -> V2V = \v,p -> mkV2Vf v p infMa ;
    mkV2V : V         -> V2V = \v   -> mkV2Vf v (casePrep genitive) infMa ;
    mkV2V : Str       -> V2V = \str -> mkV2Vf (mkV str) (casePrep genitive) infMa ;
  } ;
  mkV2Vf v p f = lin V2V (mk2V2 v p ** {vi = f}) ;

  mkVA = overload {
    mkVA : V -> Prep -> VA = \v,p -> lin VA (v ** {c2 = p}) ;
    mkVA : V -> VA = \v -> lin VA (v ** {c2 = casePrep genitive}) ;
    mkVA : Str -> VA = \str -> let v : V = mkV str in
      lin VA (v ** {c2 = casePrep genitive}) ;
  } ;

  mkV2A = overload {
    mkV2A : V -> Prep -> Prep -> V2A = \v,p,q ->
      lin V2A (v ** {c2 = p ; c3 = q}) ;
    mkV2A : V -> V2A = \v ->
      lin V2A (v ** {c2 = casePrep genitive ; c3 = casePrep translative}) ;
    mkV2A : Str -> V2A = \str -> let v : V = mkV str in
      lin V2A (v ** {c2 = casePrep genitive ; c3 = casePrep translative}) ;
  } ;

  mkV2Q v p = lin V2Q (mk2V2 v p) ;

  mkAS  a = a ;
  mkA2S a p = mkA2 a p ;
  mkAV  a = a ;
  mkA2V a p = mkA2 a p ;

} ;
