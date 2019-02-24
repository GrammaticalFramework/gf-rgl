--# -path=.:../../prelude
--
----1 A Simple Persian Resource Morphology
----
----  Shafqat Virk, Aarne Ranta,2010
----
---- This resource morphology contains definitions needed in the resource
---- syntax. To build a lexicon, it is better to use $ParadigmsPes$, which
---- gives a higher-level access to this module.
--
resource MorphoPes = ParamX ** open Prelude,Predef in {

  flags optimize=all ;
   coding = utf8;

---- Orthography

oper
  -- Zero-width non-joiner, used for certain morphemes
  -- See https://en.wikipedia.org/wiki/Persian_alphabet#Word_boundaries
  ZWNJ : Str = "‌" ;
  zwnj : Str -> Str -> Str = \s1,s2 -> s1 + ZWNJ + s2 ;

---- Nouns
param
  Animacy = Animate | Inanimate ;
  Mod = Bare | Ezafe | Clitic | Poss ;
  Agr = Ag Number Person ;

------------------------------------------
-- Agreement transformations
-----------------------------------------
oper
  toAgr : Number -> Person -> Agr = \n,p -> Ag n p ;

  fromAgr : Agr -> {n : Number ; p : Person } = \agr -> case agr of {
    Ag n p => {n = n ; p = p}
  } ;

  conjAgr : Agr -> Agr -> Agr = \a0,b0 ->
    let a = fromAgr a0 ; b = fromAgr b0
     in toAgr (conjNumber a.n b.n) b.p ;

  giveNumber : Agr -> Number = \a -> case a of {
     Ag n _ => n
  } ;

  defaultAgr : Agr = agrP3 Sg ;
  agrP3 : Number -> Agr = \n -> Ag n P3 ;
  agrP1 : Number -> Agr = \n -> Ag n P1 ;

-------------------------
-- Ezafe construction
------------------------
oper


mkPossStem : Str -> Str = \str ->

  case str of {
_+ "اه" => str ;
_+ "او" => str ;
_+ "وه" => str ;
_+ ("ا"|"و") => str + "ی" ;
_+ "ه" => zwnj str "ا" ;
_ => str } ;



  mkEzafe : Str -> Str = \str ->
  --let kasre = "ِ" in -- TODO: Eventually use this
    let kasre = "" in
    case str of {
      st + "اه" => str + kasre ;
      st + "وه" => str + kasre ;
      st + "ه"  => zwnj str "ی" ;-- alt. st + "ۀ" ;
      st + "او" => str + kasre ;
      st + "وو" => str + kasre ;
      st + "و"  => str + "ی" ;
      st + "ا"  => str + "ی" ;
      _ => str + kasre
    };

  mkEnclic : Str -> Str ;
  mkEnclic str = case str of {
      st + ("ا"|"و") => zwnj str "یی" ; -- ی after a long vowel to help pronunciation
      st + "اه"      => str + "ی" ; -- here ه is a consonant, so single ی
      st + ("ی"|"ه") => zwnj str "ای" ; -- after ی or ه as a vowel, add an alif to help pronunciation
      _               =>      str + "ی" -- any other case: just a single ی
    } ;

    Noun = {s : Number => Mod => Str ; animacy : Animacy} ;
    mkN : (x1,x2 : Str) -> Animacy -> Noun = \sg,pl,ani -> {
      s = table {
           Sg => table {Bare => sg ;
                        Ezafe => mkEzafe sg ;
                        Clitic => mkEnclic sg ;
                        Poss => mkPossStem sg } ;
           Pl => table {Bare => pl ;
                        Ezafe => mkEzafe pl ;
                        Clitic => mkEnclic pl ;
                        Poss => mkPossStem pl }
      } ;
     animacy = ani
    } ;

-- masculine nouns end with alif, choTi_hay, ain Translitration: (a, h, e)
-- Arabic nouns ends with h. also taken as Masc

---------------------
--Determiners
--------------------
Determiner : Type = {s : Str ; n :Number ; isNum : Bool ; mod : Mod} ;

 makeDet : Str -> Number -> Bool -> Determiner = \str,n,b -> {
   s = str;
   isNum = b;
   mod = Bare ;
   n = n
 };

 makeQuant : Str -> Str  -> {s : Number => Str ; a : Agr; mod : Mod } = \sg,pl -> {
   s = table {Sg => sg ; Pl => pl} ;
   mod = Bare ;
   a = agrP3 Sg
 };
---------------------------
-- Adjectives
--------------------------
 Adjective : Type = {s : Mod => Str ; adv : Str} ;

 mkAdj : Str -> Str -> Adjective = \adj,adv -> {
   s = table { Bare => adj;
               Ezafe => mkEzafe adj ;
               Clitic => mkEnclic adj ;
               Poss => mkPossStem adj
             } ;
   adv = adv
   };

------------------------------------------------------------------
-- Verbs
------------------------------------------------------------------
param
  VerbForm = VF Polarity VTense Agr
            | Vvform Agr
            | Imp Polarity Number
            | Inf | Root1 | Root2 ;
  VTense   = VFPres PrAspect
            | VFPast PstAspect
            | VFFut FtAspect
            | VFInfrPast InfrAspect;
  PrAspect  = PrPerf | PrImperf ;
  PstAspect = PstPerf | PstImperf | PstAorist ;
  FtAspect  = FtAorist ; -- just keep FtAorist
  InfrAspect = InfrPerf | InfrImperf ;

oper
  Verb = {s : VerbForm => Str} ;

  mkVerb : (x1,x2 : Str) -> Verb = \inf,root2 ->
    let root1 = tk 1 inf ;
        impRoot = impRoot root2
     in { s = table {
       Root1    => root1 ;
       Root2    => root2 ;
       Inf      => inf ;
       Imp Pos Sg => addBh impRoot ;
       Imp Pos Pl => addBh impRoot + "ید" ;
       Imp Neg Sg => "ن" + impRoot ;
       Imp Neg Pl => "ن" +  impRoot + "ید" ;
       Vvform ag  => mkvVform root2 ag ;
       VF p t ag  => mkCmnVF root1 root2 p t ag }
    } ;

  -- Verbs that end in یدن, ادن or ودن
  -- Also some verbs that don't: دانستن with stem دان
  mkVerb1 : (_: Str) -> Verb = \inf -> mkVerb inf (tk 3 inf) ;

  -- Most verbs that end in C+تن or C+دن
  mkVerb2 : (_: Str) -> Verb = \inf -> mkVerb inf (tk 2 inf) ;

  mkCmnVF : Str -> Str -> Polarity -> VTense -> Agr -> Str = \root1,root2,pol,t,ag ->
    let khordh = root1 + "ه";
        nkhordh = addN khordh ;
        mekhor = zwnj "می" root2 ;
        nmekhor =  zwnj "نمی" root2 ;
        mekhord = zwnj "می" root1 ;
        nmekhord = zwnj "نمی" root1 ;
        mekhordh = zwnj "می" khordh ;
        nmekhordh = zwnj "نمی" khordh ;
        khah = "خواه" ;
        nkhah = "نخواه" ;
        -- mekhah = zwnj "می" khah ;
        -- nmekhah = zwnj "نمی" khah ;
        bvdh = "بوده" ;
        impfSuff : Str -> Str = imperfectSuffix ag ;
        impfSuffD : Str -> Str = imperfectSuffixD ag ;
        perfSuff : Str -> Str = perfectSuffix ag ;
        pluperfSuff : Str -> Str = pluperfectSuffix ag
     in case <pol,t> of {
        <Pos,VFPres PrImperf> => impfSuffD mekhor ;
        <Pos,VFPres PrPerf>   => perfSuff khordh ;

        <Pos,VFPast PstPerf>   => pluperfSuff khordh ;
        <Pos,VFPast PstImperf> => impfSuff mekhord ;
        <Pos,VFPast PstAorist> => impfSuff root1 ;

        <Pos,VFFut FtAorist> => impfSuffD khah ++ root1;

        <Pos,VFInfrPast InfrPerf>   => khordh ++ perfSuff bvdh ;
        <Pos,VFInfrPast InfrImperf> => perfSuff khordh ;

     -- negatives
        <Neg,VFPres PrImperf> => impfSuffD nmekhor ;
        <Neg,VFPres PrPerf> => perfSuff nkhordh ;

        <Neg,VFPast PstPerf> => pluperfSuff nkhordh ;
        <Neg,VFPast PstImperf> => impfSuff nmekhord ;
        <Neg,VFPast PstAorist> => impfSuff (addN root1) ;

        <Neg,VFFut FtAorist> => impfSuffD nkhah ++ root1 ;

        <Neg,VFInfrPast InfrPerf>   => nkhordh ++ perfSuff bvdh ;
        <Neg,VFInfrPast InfrImperf> => perfSuff nmekhordh

        -- <Pos,VFFut FtImperf> => perfSuffD mekhah ++ addBh (perfSuffD root2) ;
        -- <Neg,VFFut FtImperf> => perfSuffD nmekhah ++ addBh (perfSuffD root2) ;
      } ;

  mkvVform : Str -> Agr -> Str = \root2,ag ->
    addBh (imperfectSuffixD ag root2) ;

  impRoot : Str -> Str = \root -> case root of {
    st + "ی" => st ;
    _        => root
    };

-------------------
-- making negatives
-------------------
  addN : Str -> Str ;
  addN str =
    case str of {
      "ا" + st => "نی" + str ;
      "آ" + st => "نیا" + st ;
       _        => "ن" + str } ;

  addBh : Str -> Str ;
  addBh str =
    case str of {
      "ا" + st => "بی" + str ;
      "آ" + st => "بیا" + st ;
      _        => "ب" + str
         };

  -- TODO: is this needed anywhere? what does it do? /IL
  addBh2 : Str -> Str ; -- should use drop instead but it gives linking error
  addBh2 str1 =
    case str1 of {
      "می" + str =>
         case str of {
           "ا" + st => Prelude.glue "بی" str ;
           "آ" + st => Prelude.glue "بیا" st ;
            _       => Prelude.glue "ب" str
                      };
      _          => "" -- ????
    };

-------------------
-- Common suffixes
-------------------
  imperfectSuffix : Agr -> Str -> Str = \ag,s -> s +
    case ag of {
      Ag Sg P1 => "م" ;
      Ag Sg P2 => "ی" ;
      Ag Sg P3 => [] ;
      Ag Pl P1 => "یم" ;
      Ag Pl P2 => "ید" ;
      Ag Pl P3 => "ند" } ;

  imperfectSuffixD : Agr -> Str -> Str = \ag,s ->
    case ag of {
      Ag Sg P3 => s + "د" ;
      _ => imperfectSuffix ag s } ;

  perfectSuffix : Agr -> Str -> Str = \ag,s ->
    case ag of {
      Ag Sg P1 => zwnj s "ام" ;
      Ag Sg P2 => zwnj s "ای" ;
      Ag Sg P3 => s ++ "است" ; -- no ZWNJ
      Ag Pl P1 => zwnj s "ایم" ;
      Ag Pl P2 => zwnj s "اید" ;
      Ag Pl P3 => zwnj s "اند" } ;

  pluperfectSuffix : Agr -> Str -> Str = \ag,s -> s ++
    case ag of { -- not suffix, just using consistent naming scheme :-P /IL
      Ag Sg P1 => "بودم" ;
      Ag Sg P2 => "بودی" ;
      Ag Sg P3 => "بود" ;
      Ag Pl P1 => "بودیم" ;
      Ag Pl P2 => "بودید" ;
      Ag Pl P3 => "بودند" } ;

----------------------------------
-- Irregular verbs
----------------------------------

  haveVerb : Verb = {s = table {
    Root1 => "داشت" ;
    Root2 => "دار" ;
    Inf   => "داشتن" ;
    Imp Pos Sg => "بدار" ;
    Imp Pos Pl => "بدارید" ;
    Imp Neg Sg => "ندار" ;
    Imp Neg Pl => "ندارید" ;
    Vvform agr => mkvVform "دار" agr ;
    VF pol tense agr => case <pol,tense> of {
         <Pos,VFPres PrImperf> => imperfectSuffixD agr "دار"  ;
         <Neg,VFPres PrImperf> => imperfectSuffixD agr (addN "دار") ;
         _ =>  mkCmnVF "داشت" "دار" pol tense agr
         }
    }
  } ;

  -- TODO: merge with auxBe in ResPes
  beVerb : Verb = { s = table {
    Vvform agr => imperfectSuffixD agr "باش" ;
    Imp Pos Sg => "باش" ;
    Imp Pos Pl => "باشید" ;
    Imp Neg Sg => "نباش" ;
    Imp Neg Pl => "نباشید" ;
    Inf => "بودن" ;
    Root1 => "بود" ;
    Root2 => "باش" ;
    VF pol tense agr =>
      let impfSuff = imperfectSuffix agr ;
          perfSuff = perfectSuffix agr
      in case <pol,tense,agr> of {
       <Pos,VFPres PrImperf,Ag Sg P3> => "است" ;
       <Neg,VFPres PrImperf,Ag Sg P3> => "نیست" ;
       <Pos,VFPres PrImperf>          => impfSuff "هست" ;
       <Neg,VFPres PrImperf>          => impfSuff "نیست" ;
       <Pos,VFPres PrPerf>            => perfSuff "بوده" ;
       <Neg,VFPres PrPerf>            => perfSuff "نبوده" ;
       <Pos,VFPast PstImperf>         => impfSuff "بود" ;
       <Neg,VFPast PstImperf>         => impfSuff "نبود" ;
       _                              => mkCmnVF "بود" "باش" pol tense agr
       }
    }
  } ;
}
