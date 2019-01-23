resource MorphoAra = PatternsAra ** open Prelude, OrthoAra, Predef in  {

flags optimize = all ;
  coding=utf8 ;

param

  Vowel   = u | a | i ;

oper
  -- Choose patterns based on vowels

  fvc : Vowel => Pattern =  --used in assimilated (wqf -> qif, wqc -> qac..) and hollow (qwl -> qul, xwf->xaf..)
    table {u => fuc ; i => fic ; a => fac} ;

  fVc : Vowel => Pattern =
    table {u => fUc ; i => fIc ; a => fAc} ;

  fvcc : Vowel => Pattern =
    table {u => fucc ; i => ficc ; a => facc} ;

  fcv : Vowel => Pattern =
    table {u => fcu ; i => fci ; a => fca} ;

  fcvl : Vowel => Pattern =
    table {u => fcul ; i => fcil ; a => fcal} ;

  facvl : Vowel => Pattern =
    table {u => facul ; i => facil ; a => facal} ;


param

  Number  = Sg | Dl | Pl;
  Gender  = Masc | Fem ;
  Case    = Nom | Acc | Gen
           | Bare -- 1st person poss. suff. overrides case
           | Dat ; -- Hack to make the preposition لِ contract
  Species = NoHum | Hum ;
  State   = Def | Indef | Const
          | Poss ; -- ة turns into ت
                     -- sound masculine plural drops ن
                     -- case vowel retained
  Mood    = Ind | Cnj | Jus ;
  Voice   = Act | Pas ;
  Order   = Verbal | Nominal
          | VOS      -- Used for relative clauses with resumptive pronouns
          | Subord ; -- Nominal word order but subject in accusative

oper
  ---------------
  -- Prepositions

  Preposition : Type = {s : Str ; c : Case ; binds : Bool} ;

  mkPreposition = overload {
    mkPreposition : Str -> Case -> Preposition = \s,c -> {s=s; c=c; binds=False} ;
    mkPreposition : Str -> Preposition = \s -> {s=s; c=Gen; binds=False} ;
    } ;

  mkPrefix = overload {
    mkPrefix : Str -> Preposition = \s -> {s=s; c=Gen; binds=True} ;
    mkPrefix : Str -> Case -> Preposition = \s,c -> {s=s; c=c; binds=True}
    } ;

  noPrep : Preposition = mkPreposition [] Nom ;
  liPrep : Preposition = mkPrefix (
    pre { #pronSuffAndOther => "لِ" ;
          #pronSuff         => "لَ" ;
          _                 => "لِ"
        }) Dat ;
  biPrep : Preposition = mkPrefix "بِ" ;
  accPrep : Preposition = mkPreposition [] Acc ; -- default object case in VP
  genPrep : Preposition = mkPreposition [] Gen ; -- default object case in N2

  pronSuff : pattern Str = #("كَ"|"كِ"|"كُمَا"|"كُمْ"|"كُنَّ"|"هُ"|"ها"|"هُمَا"|"هُمْ"|"هُنَّ") ;
  pronSuffAndOther : pattern Str = #( "كَم" ) ; -- TODO list words that begin like pron.suff. but aren't

  --------
  -- Verbs

  Verb : Type = {s : VForm => Str} ;
  Verb2 : Type = Verb ** {c2 : Preposition} ;
  Verb3 : Type = Verb2 ** {c3 : Preposition} ;

param

  VForm = VPerf Voice PerGenNum
        | VImpf Mood Voice PerGenNum
        | VImp Gender Number
        | VPPart -- TODO: add gender and number (or check if easy to use BIND)
        | Masdar ; -- verbal noun

  PerGenNum = Per3 Gender Number
            | Per2 Gender Number
            | Per1 SgPl;

  SgPl = Sing | Plur;

oper
  --------
  -- Nouns

  Noun : Type = {
    s,s2 : NTable ;
    g : Gender ;
    h : Species ;
    isDual : Bool -- whether it takes dual instead of plural: eyes, twins, ...
    } ;

  NTable = Number => State => Case => Str;
  emptyNTable : NTable = \\n,s,c => [] ;

  Noun2 : Type = Noun ** {c2 : Preposition} ;
  Noun3 : Type = Noun2 ** {c3 : Preposition} ;

  -------------
  -- Adjectives

  Adj  : Type = {s : AForm => Str} ;
  Adj2 : Type = Adj ** {c2 : Preposition} ;

param
  AForm = APosit Gender Number State Case
        | AComp State Case ;


-----------------------------------------------------------------------------
-- General morphology with roots, patterns, and making words:

oper

  Pattern : Type = {h, m1, m2, t : Str};
  Root    : Type = {f : Str};
  Root2   : Type = Root ** {c : Str} ;
  Root3   : Type = Root2 ** {l : Str} ;

  mkRoot3 : Str -> Root3 = \fcl -> case fcl of {
      f@? + c@? + l => {f = f ; c = c ; l = l} ;
      _ => error ("mkRoot3: too short root" ++ fcl)
      } ;

    --for roots with 2 consonants (works also for assimilated strs, like fc~,
    --because the function discards anything after the first two characters
  mkRoot2 : Str -> Root2 = \fcl ->
      case fcl of {
       f@? + c@? + _ => { f = f ; c = c } ;
       _ => error ("mkRoot2: too short root" ++ fcl)
      };

  mkPat : Str -> Pattern = \pat ->
      case pat of {
        w + "ف" + x + "ع" + y + "ل" + z
          => { h = w ; m1 = x; m2 = y; t = z} ;
        w + "ف" + x + ("ع"|"ل") + y
          => { h = w ; m1 = x; m2 = ""; t = y}
      } ;

  --opers to interdigitize (make words out of roots and patterns:
  --regular case, 3 non-weak consonants
  mkStrong : Pattern -> Root3 -> Str = \p,fcl ->
    p.h + fcl.f + p.m1 + fcl.c + p.m2 + fcl.l + p.t;

  mkDefective : Pattern -> Root3 -> Str = \p,fcl ->
    p.h + fcl.f + p.m1 + fcl.c + p.t;

  mkDefectiveAlifMaqsura : Pattern -> Root3 -> Str = \p,fcl ->
    p.h + fcl.f + p.m1 + fcl.c + p.t + "َى" ;

  mkHollow : Pattern -> Root3 -> Str = \p,fcl ->
    p.h + fcl.f + p.m1 + fcl.l + p.t;

  mkAssimilated : Pattern -> Root3 -> Str = \p,fcl ->
    p.h + fcl.c + p.m1 + fcl.l + p.t;

  -- takes a weak pattern and a triliteral root and makes
  -- a word, deducing which root consonant is weak
  mkWeak : Pattern -> Root3 -> Str = \pat,fcl ->
    case <fcl.f,fcl.c,fcl.l> of {
     <_,_,#weak|"ّ"> => mkDefective   pat fcl;
     <_,#weak,_>     => mkHollow      pat fcl;
     <#weak,_,_>     => mkAssimilated pat fcl
    };

  mkBilit : Pattern -> Root2 -> Str = \p,fcl ->
    p.h + fcl.f + p.m1 + fcl.c + p.t;

  --takes a pattern string and root string and makes a word
  mkWord : Str -> Str -> Str = \pS, rS ->
    let pat = mkPat pS in
    case pS of {
      w + "ف" + x + "ع" + y + "ل" + z =>
        case rS of { -- TODO: reconsider shadda, maybe handling it should be moved 100% to ParadigmsAra /IL 2019-01-01
          x@? + y@? + "ّ" => mkStrong pat (mkRoot3 (x+y+y)) ; -- In principle, shadda shouldn't be in the root when dealing with strong inflection, but if someone puts one, this should fix it. /IL
          _               => mkStrong pat (mkRoot3 rS) } ;
      w + "ف" + x + "ع" + y =>
        case rS of {
             x + "ّ" => mkBilit pat (mkRoot2 x) ; -- fc~
             x@? + y@? + ("و"|"ي")
                     => mkDefective pat (mkRoot3 rS) ;
             x@? + ("و"|"ي") + z@?
                     => mkHollow pat (mkRoot3 rS) ;
            ("و"|"ي") + y@? + z@?
                    => mkAssimilated pat (mkRoot3 rS) ;
            ? + ? + _ => mkBilit pat (mkRoot2 rS) ; --2=>
            _=> error rS ---- AR error "expected 3--6"
        }
    };

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Nominal morphology

  caseTbl : Case => Str =
    table {
      Bare => [] ;
      Nom  => "ُ";
      Acc  => "َ";
      _Gen  => "ِ" -- dat is the same as gen, except in definite before لِ
    };

  -- indeclinable nominal word (mamnuu3 mina S-Sarf)
  indeclN : Str -> State => Case => Str =
    \aHmar -> \\s,c => defArt s c aHmar + indecl!c;

  -- takes 2 words, singular and broken plural, and gives the
  -- complete noun inflection table
  reg : Str -> Str -> NTable =
    \kitAb,kutub ->
    table {
      Sg => sing kitAb ;
      Dl => dual kitAb ;
      Pl => brkPl kutub
    };

  --takes the sound noun in singular and gives the
  --complete noun inflection table of sound feminine plural
  sndf : Str -> NTable =
    \lawHa ->
    table {
      Sg => sing lawHa ;
      Dl => dual lawHa ;
      Pl => plurF lawHa
    };

  --takes the sound noun in singular and gives the
  --complete noun inflection table of sound feminine plural
  sgMsndf : Str -> NTable  =
    \lawHa ->
    table {
      Sg => sing lawHa ;
      Dl => dual lawHa ;
      Pl => singMplurF lawHa
    };

  --takes the sound noun in singular and gives the
  --complete inflection table of sound masculine plural nominals
  sndm : Str -> NTable =
    \muzAric ->
    table {
      Sg => sing muzAric ;
      Dl => dual muzAric ;
      Pl => plurM muzAric
    };


  -- takes a singular or broken plural word and tests the ending to
  -- determine the declension and gives the corresponding inf table
  brkPl : Str -> State => Case => Str = \word ->
    \\s,c => defArt s c (case word of {
      lemma + "ِي"  => fixShd lemma (dec2sg ! s ! c) ; -- 2nd declension
      _ + ("ا"|"ى") => fixShd word  (dec3sg ! s ! c) ;
      lemma + (#hamza|#hamzaseat)
                    => word + dec1sgNoDoubleAlif ! s ! c ;
      lemma + "ة"   => case s of {
                          Poss => lemma + "ت" + dec1sg ! s ! c ;
                          _    => word        + dec1sgNoDoubleAlif ! s ! c
                        } ;
       _             => fixShd word (dec1sg ! s ! c)
    }) ;

  sing : Str -> State => Case => Str = \word ->
    \\s,c => case word of {
      -- This only applies for singular indefinite
      x + y@? + #hamza => defArt s c (
         case <s,c> of { -- if hamza was last, it's now in the body
            <Indef,Acc> =>
                case y of {
                  #vstar => word + dec1sgNoDoubleAlif ! Indef ! Acc ;
                  _ => let an  : Str = dec1sg ! Indef ! Acc ;
                           hmz : Str = bHmz x an ;
                        in x + y + hmz + an } ;
            _ => word + dec1sg ! s ! c }) ;
       -- The rest is identical with singulars and broken plurals
      _ => brkPl word ! s ! c
    } ;

  -- takes a singular word and tests the ending to
  -- determine the declension and gives the corresponding dual inf table
  dual : Str -> State => Case => Str = \caSaA ->
    \\s,c => defArt s c (case caSaA of {
      lemma + ("ا"|"ى") => lemma + "ي" + dl ! s ! c ;
      lemma + "ة"       => lemma + "ت" + dl ! s ! c ;
      _                 => fixShd caSaA (dl ! s ! c)
    });

  -- takes a singular word and gives the corresponding sound
  --plural feminine table
  plurF : Str -> State => Case => Str =
    \kalima ->
    \\s,c => defArt s c (mkAt kalima) + f_pl ! s ! c ;

  -- takes a singular masculine word and gives the corresponding
  -- sound plural feminine table
  singMplurF : Str -> State => Case => Str =
    \ijra' ->
    \\s,c => defArt s c (mkAtMasc ijra') + f_pl ! s ! c ;

  -- takes a singular word and gives the corresponding sound
  --plural masculine table. FIXME: consider declension 3
  plurM : Str -> State => Case => Str =
    \mucallim ->
    \\s,c => defArt s c mucallim + m_pl ! s ! c ;

  -- to add the Al prefix for Definite words
  Al : State => Str =
    table {
      Def => "ال" ;
      _   => ""
    };

  defArt : State -> Case -> Str -> Str = \st,c,stem -> -- IL -- to be checked
    let al = "ال" in
    case <st,c> of {
      <Def,Dat> => "ل" + stem ; -- only happens before the preposition لِ
      <Def> =>
        case stem of {
          s@#sun + x  => fixShd (al + s) ("ّ" + x) ;
          x           => al + x } ;
      _   => stem
    };

  --declension 1 (strong ending) of the singular or broken plural words
  dec1sg : State => Case => Str =
    table {
      Indef =>
        table {
          Bare => [];
          Nom => "ٌ";
          Acc => "اً";
          _Gen => "ٍ"
        };
      _ => caseTbl --think of ?axU, ?axA, (the five nouns)

    };

  -- if a word ends in ء or ة, don't add alif for indef acc.
  dec1sgNoDoubleAlif : State => Case => Str = \\s,c =>
    case <s,c> of {
      <Indef,Acc> => "ً" ;
      _           => dec1sg ! s ! c
    };

  --indeclinables (mamnuu3 mina S-Sarf)
  indecl : Case => Str =
    table {
      (Gen|Dat) => "َ" ;
      x         => caseTbl ! x
    };


  --declension 2 (ends in yaa')
  dec2sg : State => Case => Str = \\s,c =>
    case <s,c> of {
      <Indef,Acc> => "ِياً" ;
      <Indef>     => "ٍ" ;
      <_,    Acc> => "ِيَ" ;
      _           => "ِي"
    };


  --declension 3 (ending in alif)
  dec3sg : State => Case => Str = \\s,c =>
    case <s,c> of {
      <Indef,Bare> => [] ;
      <Indef>      => "ً" ;
      _            => []
    };

  --dual suffixes
  dl : State => Case => Str =
    table {
      Const =>
        table {
          Nom => "َا";
          _   => "َيْ"
        };
      Poss =>
        table {
          Nom => "َا" ; -- wrong for 1st person poss. suff
          Bare => "َيَّ" ; -- this covers 1st person for genitive and accusative
          _   => "َيْ"
        };
      _ =>
        table {
          Nom  => "َانِ";
          Bare => "َيْن";
          _    => "َيْنِ"
        }
    };

  --sound masculine plural suffixes
  m_pl : State => Case => Str =
    table {
      (Const|Poss) =>
        table {
          Nom => "ُو";
          _   => "ِي"
        };
      _ =>
        table {
          Bare => "ِين";
          Nom => "ُونَ";
          _   => "ِينَ"
        }
    };

  --sound feminine plural suffixes
  f_pl : State => Case => Str =
    table {
      Indef =>
        table {
          Bare => [];
          Nom => "ٌ";
          _   => "ٍ"
        };
      _ =>
        table {
          Bare => [];
          Nom => "ُ";
          _   => "ِ"
        }
    };

  -- TODO: this isn't actually because of gender, it "just happens".
  -- Refactor the whole sdfN and make variant with a parameter
  -- whether to insert a و or ه or something else /IL
  mkAtMasc : Str -> Str = \x ->
    case x of {
      y + "ة"  => y + "ات";
      y + "ى"  => y + "يَات"; -- TODO check does this happen?
      _        => x + "ات"
    };

  mkAt : Str -> Str = \bayDo ->
    case bayDo of {
      bayD + "ة"  => bayD + "ات";
      bayD + "اء" => bayD + "اوات";
      bayD + "ى"  => bayD + "يَات";
      _           => bayDo + "ات"
    };

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Adjective morphology

--takes the adjective lemma and gives the Posit table
  positAdj : Str -> Gender => NTable  =
    \kabIr ->
    let kabIra = kabIr + "َة" in
    table {
      Masc => sndm kabIr;
      Fem  => sndf kabIra
    };

  clr : Str -> Str -> Str -> AForm => Str =
    \aHmar,HamrA',Humr ->
    table {
      APosit Masc n d c => case n of {
        Sg => indeclN aHmar ! d ! c ;
        Dl => dual aHmar ! d ! c ;
        Pl => brkPl Humr ! d ! c
        };
      APosit Fem n d c => case n of {
        Sg => indeclN HamrA'  ! d ! c;
        Dl => dual ((tk 2 HamrA') + "و") ! d ! c;
        Pl => brkPl Humr ! d ! c
        };
      AComp d c => indeclN aHmar ! d ! c
    };


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Verbal morphology


  -- To share code between different paradigms
  SoundForms : Type = Predef.Ints 6  => Str ;

  -- Defective needs max 13 forms, hollow and geminate verbs need 12 forms.
  -- NB. the numbers don't always refer to the same forms!
  -- The verb(Def|Hollow|Geminate) constructors pick the right forms.
  DefForms : Type = Predef.Ints 12 => Str ;

  toSoundForms : (x1,_,_,_,_,_,x7 : Str) -> SoundForms =
    \a,b,c,d,e,f,g ->
    table {
      0 => a ; 1 => b ; 2 => c ; 3 => d ; 4 => e ; 5 => f ; 6 => g
    } ;

  toDefForms = overload {
    toDefForms : (x1,_,_,_,_,_,_,_,_,_,_,_,x13 : Str) -> DefForms =
      \a,b,c,d,e,f,g,h,i,j,k,l,m ->
          table {
            0 => a ; 1 => b ; 2 => c ; 3 => d ; 4 => e ;
            5 => f ; 6 => g ; 7 => h ; 8 => i ;
            9 => j ; 10 => k ; 11 => l ; 12 => m
          } ;
    toDefForms : (x1,_,_,_,_,_,_,_,_,_,_,x12 : Str) -> DefForms =
      \a,b,c,d,e,f,g,h,i,j,k,l ->
          table {
            0 => a ; 1 => b ; 2 => c ; 3 => d ; 4 => e ;
            5 => f ; 6 => g ; 7 => h ; 8 => i ;
            9 => j ; 10 => k ; 11 => l ; 12 => "never used"
          }
   } ;



  ------------------------------------------------------------
  -- Macro for sound verbs
  -- PerfAct, PerfPas, ImpfAct, ImpfPas, Imp, PPart, Masdar

  verb : (x1,_,_,_,_,_,x7 : Str) -> Verb =
    \katab,kutib,aktub,uktab,euktub,maktUb,katb -> {
      s = \\vf => rectifyHmz (case vf of { -- TODO: make one oper that calls rectifyHmz for all verbs
        VPerf Act pgn     => katab + suffixPerf ! pgn ;
        VPerf Pas pgn     => kutib + suffixPerf ! pgn ;
        VImpf Ind Act pgn => prefixImpf!pgn + aktub + suffixImpfInd !pgn;
        VImpf Ind Pas pgn => prefixImpf!pgn + uktab + suffixImpfInd !pgn;
        VImpf m Act pgn => prefixImpf!pgn + aktub + suffixImpfCJ m ! pgn;
        VImpf m Pas pgn => prefixImpf !pgn + uktab + suffixImpfCJ m !pgn;
        VImp  g n  => euktub + suffixImpfCJ Jus ! (Per2 g n);
        VPPart     => maktUb ;
        Masdar     => katb
        })
    } ;

  verb' : SoundForms -> Verb = \vforms ->
   let katab = vforms ! 0 ; -- VPerf Act
       kutib = vforms ! 1 ; -- VPerf Pas
       aktub = vforms ! 2 ; -- VImpf _ Act
       uktab = vforms ! 3 ; -- VImpf _ Pas
       euktub = vforms ! 4 ; -- VImp
       maktUb = vforms ! 5 ; -- VPPart
       katb   = vforms ! 6 ; -- Masdar
    in verb katab kutib aktub uktab euktub maktUb katb ;

  ------------------------------------------------------------
  -- Macro for hollow verbs

  verbHollow : DefForms -> Verb = \vforms -> { s = table {
    VPerf     v pgn =>                    patPerf ! v ! pgn + suffixPerf ! pgn ;
    VImpf Ind v pgn => prefixImpf ! pgn + patImpf ! v ! pgn + suffixImpfInd ! pgn ;
    VImpf Cnj v pgn => prefixImpf ! pgn + patImpf ! v ! pgn + suffixImpfCJ Cnj ! pgn ;
    VImpf Jus v pgn => prefixImpf ! pgn + patJus  ! v ! pgn + suffixImpfCJ Jus ! pgn ;
    VImp        g n =>                    patImp  ! g ! n   + suffixImpfCJ Jus ! Per2 g n ;
    VPPart          => ppart ;
    Masdar          => masdar }
  } where {

    xAf = vforms ! 0 ; -- VPerf Act _
    xif = vforms ! 1 ; -- VPerf Act (Per3 Fem Pl)
    xIf = vforms ! 2 ; -- VPerf Pas _
    xuf = vforms ! 3 ; -- VPerf Pas (Per3 Fem Pl)
    axAf = vforms ! 4 ; -- VImpf Act _
    axaf = vforms ! 5 ; -- VImpf Act (Per2/Per3 Fem Pl)
    uxAf = vforms ! 6 ; -- VImpf Pas _
    uxaf = vforms ! 7 ; -- VImpf Pas (Per2/Per3 Fem Pl)
    impSg = vforms ! 8 ; -- VImp (Sg Masc / Pl Fem)
    impPl = vforms ! 9 ; -- VImp (Pl Masc / Sg Fem)
    ppart = vforms ! 10 ; -- VPPart
    masdar = vforms ! 11 ; -- verbal noun

    patPerf : Voice => PerGenNum => Str = table {
      Act => table {
        Per3 Fem Pl => xif ;
        Per3 _   _  => xAf ;
        _           => xif } ;
      Pas => table {
        Per3 Fem Pl => xuf ;
        Per3 _   _  => xIf ;
        _           => xuf }
      } ;

    --this is the pattern of imperfect  hollow (ind & conj) and geminate verbs (all)
    patImpf : Voice => PerGenNum => Str = table {
      Act => table {
        (Per3 Fem Pl|Per2 Fem Pl) => axaf ;
        _                         => axAf } ;
      Pas => table {
        (Per3 Fem Pl|Per2 Fem Pl) => uxaf ;
        _                         => uxAf }
      } ;

    patJus : Voice => PerGenNum => Str = table {
      Act => table {
        (Per3 _ Sg|Per3 Fem Pl|Per2 Fem Pl|Per2 Masc Sg|Per1 _)
          => axaf ;
        _ => axAf } ;
      Pas => table {
        (Per3 _ Sg|Per3 Fem Pl|Per2 Fem Pl|Per2 Masc Sg|Per1 _)
          => uxaf ;
        _ => uxAf }
      } ;

    patImp : Gender => Number => Str = table {
      Masc => table {Sg => impSg ; _ => impPl} ;
      Fem  => table {Pl => impSg ; _ => impPl}
      }
  } ;

  ------------------------------------------------------------
  -- Macro for geminate verbs: same behaviour as hollow verbs,
  -- except for jussive and imperative. /IL
  verbGeminate : DefForms -> Verb = \vforms ->
    let verbHol = verbHollow vforms ;
        facc = vforms ! 8 ;
        facic = vforms ! 9 ;
        patImp : Gender => Number => Str = \\g,n =>
          case <g,n> of {
            <Fem,Pl> => facic ;
            _        => facc
          }
    in { s = table { -- Jussive and imperative have fatha instead of sukun
           VImpf Jus v pgn => verbHol.s ! VImpf Cnj v pgn ;
           VImp        g n => patImp ! g ! n + suffixImpfCJ Cnj ! Per2 g n ;
           x               => verbHol.s ! x
           }
       } ;

  ------------------------------------------------------------
  -- Macro for defective verbs:
  verbDef : (normalAlif : Bool) -> DefForms -> Vowel -> Verb =
    let isDoubleDef : Bool = False in verbDefBool isDoubleDef ;

  verbDoubleDef : DefForms -> Vowel -> Verb =
    let normalAlif : Bool = False ;
        isDoubleDef : Bool = True ;
     in verbDefBool isDoubleDef normalAlif ;

  -- if last radical is و, then use alif (instead of alif maqsuura) in suffixPerfDef
  normalAlif : Root3 -> Bool = \r -> case r.l of {"و" => True ; _ => False} ;

  verbDefBool : Bool -> Bool -> DefForms -> Vowel -> Verb =
    \isDoubleDef,normalAlif,vforms,vowImpf -> { s = table {
      VPerf   v   pgn =>                    patPerf ! v ! pgn + suffixPerf v         ! pgn ;
      VImpf m Act pgn => prefixImpf ! pgn + patImpfAct  ! pgn + suffixImpf Act ! m   ! pgn ;
      VImpf m Pas pgn => prefixImpf ! pgn + urma              + suffixImpf Pas ! m   ! pgn ;
      VImp        g n =>                    patImp ! g ! n    + suffixImpf Act ! Jus ! Per2 g n ;
      VPPart          => ppart ;
      Masdar          => masdar }
    } where {
      rama  = vforms ! 0 ; -- VPerf Act (Per3 Masc Sg)
      ramay = vforms ! 1 ; -- VPerf Act (Per3 Fem  Pl)
      rumi  = vforms ! 2 ; -- VPerf Pas (Per3 _    Sg)
      rumu  = vforms ! 3 ; -- VPerf Pas (Per3 Masc Pl)
      rumiy = vforms ! 4 ; -- VPerf Pas (Per3 Fem  Pl)
      armi  = vforms ! 5 ; -- VImpf _ Act (Per1 _ _ | Per3 Fem _ | Per2/3 Masc Sg)
      armu  = vforms ! 6 ; -- VImpf _ Act (Per2/3 Masc Pl)
      ad3i  = vforms ! 7 ; -- VImpf _ Act (Per2 Fem)
      urma  = vforms ! 8 ; -- VImpf _ Pas
      Irmi  = vforms ! 9 ; -- VImp Masc Sg | VImp Fem _
      Irmu  = vforms ! 10 ; -- VImp Masc Pl
      ppart = vforms ! 11 ; -- VPPart
      masdar = vforms ! 12 ; -- verbal noun

      patPerf : Voice => PerGenNum => Str = table {
        Act => table {
          Per3 Fem Pl => ramay ;
          Per3 _   _  => rama ;
          _           => ramay } ;
        Pas => table {
          Per3 Masc Pl => rumu ;
          Per3 Fem Pl  => rumiy ;
          Per3 _   _   => rumi ;
          _            => rumiy }
        } ;

      patImpfAct : PerGenNum => Str = table {
        Per3 Masc Pl => armu ;
        Per2 Masc Pl => armu ;
        Per2 Fem  Sg => ad3i ; -- for 1d3:    d3i different, rmi = rmu
        _            => armi   -- for others: rmu different, rmi = d3i
        } ;

      patImp : Gender => Number => Str = \\g,n =>
        case <g,n> of { <Masc,Pl> => Irmu ; _ => Irmi } ;

      suffixPerf : Voice -> PerGenNum => Str = \vc ->
        let p3ms = case vc of {
                     Act => if_then_Str normalAlif "ا" "ى" ;
                     Pas => "يَ" } ;
            ya = case vc of {
                   Act => "" ;
                   Pas => "يَ" }
         in table {
              Per3 Masc Sg => p3ms ;
              Per3 Masc Dl => "يَا" ;
              Per3 Masc Pl => "وْا" ;
              Per3 Fem  Sg => ya + "تْ" ;
              Per3 Fem  Dl => ya + "تَا" ;
              Per3 Fem  Pl => "نَ" ;
              Per2 Masc Sg => "تَ" ;
              Per2 _    Dl => "تُمَا" ;
              Per2 Masc Pl => "تُمْ" ;
              Per2 Fem  Sg => "تِ" ;
              Per2 Fem  Pl => "تُنَّ" ;
              Per1 Sing    => "تُ" ;
              Per1 Plur    => "نَا"
            } ;

      suffixImpfDef : Voice -> Mood => PerGenNum => Str = \vc ->
        let default : Mood -> Str = \m -> case vc of {
              Pas => case m of {Jus => "" ; _ => "ى"} ;
              Act => case vowImpf of {
                u => case m of {Ind => "و" ; Cnj => "وَ" ; Jus => ""} ;
                i => case m of {Ind => "ي" ; Cnj => "يَ" ; Jus => ""} ;
                a => case m of {Ind => "ى" ; Cnj => "ى"  ; Jus => ""} }
            }
         in table { -- TODO: check whether to remove sukuns
              Ind => table {
                Per3 Masc Pl => "وْنَ" ;
                Per2 Masc Pl => "وْنَ" ;
                (Per3 _ Dl|Per2 _ Dl) => case vowImpf of {
                           u => "وَانِ" ;
                           _ => "يَانِ" } ;
                Per3 Fem  Pl => "يْنَ" ;
                Per2 Fem  _  => "يْنَ" ;
                _            => default Ind
              } ;
              mood => table {
                Per3 Masc Pl => "وْا" ;
                Per2 Masc Pl => "وْا" ;
                (Per3 _ Dl|Per2 _ Dl) => case vowImpf of {
                           u => "وَا" ;
                           _ => "يَا" } ;
                Per3 Fem  Pl => "يْنَ" ;
                Per2 Fem  Pl => "يْنَ" ;
                Per2 Fem  Sg => "ي" ;
                _            => default mood
              }
        } ;

      suffixImpf : Voice -> Mood => PerGenNum => Str = \vc -> case isDoubleDef of {
        False => suffixImpfDef vc ;
        True  => \\m,p => rmSukun (suffixImpfDef vc ! m ! p) } ;

  } ;
  ------------------------------------------------------------
  -- Common affixes

  --affixes of sound verbs
  suffixPerf : PerGenNum => Str =
    table {
      Per3 Masc Sg => "َ" ;
      Per3 Masc Dl => "َا" ;
      Per3 Masc Pl => "ُوا" ;
      Per3 Fem  Sg => "َتْ" ;
      Per3 Fem  Dl => "َتَا" ;
      Per3 Fem  Pl => "ْنَ" ;
      Per2 Masc Sg => "ْتَ" ;
      Per2 _    Dl => "ْتُمَا" ;
      Per2 Masc Pl => "ْتُمْ" ;
      Per2 Fem  Sg => "ْتِ" ;
      Per2 Fem  Pl => "ْتُنَّ" ;
      Per1 Sing    => "ْتُ" ;
      Per1 Plur    => "ْنَا"
    } ;

  prefixImpf : PerGenNum => Str =
    table {
      Per1 Sing    => "أ" ;
      Per1 Plur    => "ن" ;
      Per3 Masc _  => "ي" ;
      Per3 Fem  Pl => "ي" ;
      _     => "ت"
    } ;

  suffixImpfInd : PerGenNum => Str =
    table {
      Per3  Masc Pl => "ُونَ" ;
      Per3  Fem  Pl => "ْنَ" ;
      Per3  g  Dl => "َانِ" ;
      Per2  Masc Pl => "ُونَ" ;
      Per2 Fem  Sg => "ِينَ" ;
      Per2  g  Dl => "َانِ" ;
      Per2  Fem  Pl => "ْنَ" ;
      _     => "ُ"
    } ;

  suffixImpfCJ : Mood -> PerGenNum => Str = \m ->
    table {
      Per3  Masc Pl => "ُوا" ;
      Per3  Fem  Pl => "ْنَ" ;
      Per3  g  Dl => "َا" ;
      Per2  Masc Pl => "ُوا" ;
      Per2 Fem  Sg => "ِي" ;
      Per2  g  Dl => "َا" ;
      Per2  Fem  Pl => "ْنَ" ;
      _     => endVowel ! m
    } ;

  endVowel : Mood => Str =
    table {
    Cnj => "َ" ;
    Jus => "ْ" ;
    Ind => ""
    } ;

  prefixImp : Vowel => Str =
    table {
      u => "اُ" ;
      _ => "اِ"
    } ;

  ------------------------------------------------------------
  -- Verb paradigms: forms 1-11.


  ---------
  -- Form I

  --is used for the sound, assimilated (weak C1), and when C1 = hamza: /AED
  v1soundForms : Root3 -> Vowel -> Vowel -> (masdar:Str) -> SoundForms =
    \fcl,vowPerf,vowImpf,masdar ->
    let qf = {f = fcl.c ; c = fcl.l} ;
        qif = mkBilit (fvc ! vowImpf) qf;
        katab = mkStrong (facvl ! vowPerf) fcl ;
        kutib = mkStrong fucil fcl ;
        ktub  = mkStrong (fcvl ! vowImpf) fcl ;
        aktub = "َ" + case fcl.f of {
                        #weak => qif ;
                        _     => ktub } ;
        uktab = mkStrong ufcal fcl ;
        euktub = case fcl.f of {
                   ("ء"|"و"|"ي") => qif ;
                    _            => prefixImp ! vowImpf + ktub } ;
        maktUb = mkStrong mafcUl fcl
     in toSoundForms katab kutib aktub uktab euktub maktUb masdar ;

  v1geminateForms : Str -> Vowel -> Vowel -> (masdar:Str) -> DefForms =
    \rootStr,vowPerf,vowImpf,masdar ->
    let mdd = mkRoot3 rootStr ; --fcc
        md  = mkRoot2 rootStr ; --fc
        madd = mkBilit facc md ;
        madad = mkStrong (facvl ! vowPerf) mdd ;
        mudd = mkBilit fucc md ;
        mudid = mkStrong fucil mdd ;
        mudd' = mkBilit (fvcc ! vowImpf) md ;
        amudd = "َ" + mudd' ;
        mdud = mkStrong (fcvl ! vowImpf) mdd ;
        amdud = "َ" + mdud ;
        umadd = "ُ" + madd ;
        umdad = "ُ" + mkStrong fcal mdd ;
        Umdud = (prefixImp ! vowImpf) + mdud;
        mamdUd = mkStrong mafcUl mdd
     in toDefForms
          madd madad mudd mudid   -- VPerf
          amudd amdud umadd umdad -- VImpf
          Umdud mudd' mamdUd masdar ;

  v1hollow : Root3 -> Vowel -> (masdar:Str) -> Verb =
    \xwf,vowImpf,masdar ->
    let patHol1 : Vowel => Pattern =
          table { u => fuc ; _ => fic} ;

        patHol2 : Vowel => Pattern =
          table { u => fic ; _ => fuc} ;

        xif = mkHollow (patHol1 ! vowImpf) xwf ; -- VPerf Act (Per3 Fem Pl)
        xAf = mkHollow fAc xwf ;                 -- VPerf Act _
        xuf = mkHollow (patHol2 ! vowImpf) xwf ; -- VPerf Pas (Per3 Fem Pl)
        xIf = mkHollow fIc xwf ;                 -- VPerf Pas _
        xaf = mkHollow (fvc ! vowImpf) xwf ; -- VImp Sg Masc / Pl Fem
        xAf'= mkHollow (fVc ! vowImpf) xwf ; -- VImp Pl Masc / Sg Fem
        axaf= "َ" + xaf ;          -- VImpf Act (Per2/Per3 Fem Pl)
        axAf= "َ" + xAf';          -- VImpf Act _
        uxaf= "ُ" + xaf ;          -- VImpf Pas (Per2/Per3 Fem Pl)
        uxAf= mkHollow ufAc xwf ; -- VImpf Pas _
        ppart = "مَ" + xAf' -- FIXME actually wierd anomalies happen with the a vowel.. /AED
     in verbHollow (toDefForms
          xAf xif xIf xuf
          axAf axaf uxAf uxaf
          xaf xAf' ppart masdar) ;

  v1defForms_perfA : Root3 -> Vowel -> (masdar:Str) -> DefForms = \rmy,vowImpf,masdar ->
   let fca_fcu : Vowel => Pattern =
         table { a => fca ; _ => fcu } ;

       _rmi = mkDefective (fcv ! vowImpf) rmy ;
       _rmu = mkDefective (fca_fcu ! vowImpf) rmy ;
       rama = mkDefective faca rmy ;
       ramay = mkStrong facalo rmy ;
       rumi = mkDefective fuci rmy ;
       rumu = mkDefective fucu rmy ;
       rumiy = mkStrong fucilo rmy ;
       ad3i = "َ" + mkDefective fci rmy ; -- Per2 Sg Fem: always i
       armu = "َ" + _rmu ;                -- Per2/Per3 Pl Masc: always u/a
       armi = "َ" + _rmi ;                -- rest of the forms: depends on vowImpf
       urma = mkDefective ufca rmy ;
       eirmi = prefixImp ! vowImpf + _rmi;
       eirmu = prefixImp ! vowImpf + _rmu;
       marmiy = mkStrong mafcil rmy
    in toDefForms
         rama ramay rumi rumu rumiy -- VPerf
         armi armu ad3i urma        -- VImpf
         eirmi eirmu marmiy masdar ;

  v1defForms_perfI : Root3 -> Vowel -> (masdar:Str) -> DefForms = \bqy,vowImpf,masdar ->
    let vforms_a = v1defForms_perfA bqy vowImpf masdar ;
        baqI  = mkDefective facIl bqy ;
        baqiy = mkDefective facil bqy ;
     in table { 0 => baqI ;
                1 => baqiy ;
                x => vforms_a ! x } ;

  v1sound : Root3 -> Vowel -> Vowel -> (masdar:Str) -> Verb =
    \fcl,vp,vi,masdar -> verb' (v1soundForms fcl vp vi masdar) ;

  v1geminate : Str -> Vowel -> Vowel -> (masdar:Str) -> Verb =
    \fcl,vp,vi,masdar -> verbGeminate (v1geminateForms fcl vp vi masdar) ;

  v1defective_a : Root3 -> Vowel -> (masdar:Str) -> Verb = \rmy,vowImpf,masdar ->
    let vforms = v1defForms_perfA rmy vowImpf masdar
     in verbDef (normalAlif rmy) vforms vowImpf ;

  v1defective_i : Root3 -> Vowel -> (masdar:Str) -> Verb = \bqy,vowImpf,masdar -> -- IL (conjugation 1d4)
    let vforms_i = v1defForms_perfI bqy vowImpf masdar ;
     in verbDef (normalAlif bqy) vforms_i vowImpf ;

  v1doubleweak : Root3 -> (masdar:Str) -> Verb = \r'y,masdar ->
    let ry = r'y ** {c = ""} ;
        vforms_doubleweak : DefForms = \\x => rmSukun (v1defForms_perfA ry a masdar ! x) ; -- only remove the first sukun
        vforms_weak : DefForms = v1defForms_perfA r'y a masdar ;
        vforms = table { 0 => vforms_weak ! 0 ; -- all perfect forms
                         1 => vforms_weak ! 1 ;
                         2 => vforms_weak ! 2 ;
                         3 => vforms_weak ! 3 ;
                         4 => vforms_weak ! 4 ;
                         x => vforms_doubleweak ! x } ;
     in verbDoubleDef vforms a ; -- sukun in suffixes is removed in verbDoubleDef

  v1assimilated_defective : Root3 -> Vowel -> Vowel -> (masdar:Str) -> Verb = \root,vPerf,vImpf,msdr ->
    let vffun = case vPerf of {i => v1defForms_perfI ; _ => v1defForms_perfA } ;
        vforms_def : DefForms = vffun root vImpf msdr ;
        vforms_ass : DefForms = \\x => rmSukun (vffun (root ** {f = ""}) vImpf msdr ! x) ;
        vforms : DefForms =
           table { 4 => vforms_ass ! 4 ;
                   5 => vforms_ass ! 5 ;
                   6 => vforms_ass ! 6 ;
                   7 => vforms_ass ! 7 ;
                   8 => vforms_ass ! 8 ;
                   9 => vforms_ass ! 9 ;
                   10 => vforms_ass ! 10 ;
                   x => vforms_def ! x } ;
     in verbDef (normalAlif root) vforms vImpf ;

  ----------
  -- Form II

  v2sound : Root3 -> Verb = \qsm ->
    let {
      qassam = mkStrong faccal qsm ;
      qussim = mkStrong fuccil qsm ;
      qassim  = mkStrong faccil qsm ;
      uqassim = "ُ" + qassim ;
      uqassam = "ُ" + qassam ;
      muqassam = "مُ" + qassam ;
      taqsIm = "تَ" + mkStrong fcIl qsm ;
    } in
    verb qassam qussim uqassim uqassam qassim muqassam taqsIm ;

  v2defective : Root3 -> Verb = \gny ->
    let {
      ganna = mkDefective facca gny ;
      gannay = mkStrong faccalo gny ;
      gunni = mkDefective fucci gny ;
      gunnu = mkDefective fuccu gny ;
      gunniy = mkStrong fuccilo gny ;
      ganni = mkDefective facci gny;
      uganni = "ُ" + ganni;
      gannu = mkDefective faccu gny;
      ugannu = "ُ" + gannu;
      uganna = "ُ" + ganna;
      mugannaY = "مُ" + ganna + "ى";
      tagniyat = "تَ" + mkStrong fcil (gny ** {l="ي"}) + "َة" ;
    } in verbDef False (toDefForms
            ganna gannay gunni gunnu gunniy -- VPerf
            uganni ugannu uganni uganna     -- VImpf
            ganni gannu mugannaY tagniyat) i ;

  -----------
  -- Form III

  v3sound : Root3 -> Verb =
    \tbc ->
    let {
      tAbac = mkStrong fAcal tbc ;
      twbic = mkStrong fUcil tbc ;
      tAbic = mkStrong fAcil tbc ;
      utAbic  = "ُ" + tAbic ;
      utAbac = mkStrong ufAcal tbc ;
      mutAbac = "م" + utAbac ;
      mutAbacAt = mutAbac + "َاَة"
    } in verb tAbac twbic utAbic utAbac tAbic mutAbac mutAbacAt ;

  ----------
  -- Form IV

  v4soundForms : Root3 -> SoundForms = \qnc ->
    let eaqnac = mkStrong eafcal qnc;
        euqnic = mkStrong eufcil qnc;
        uqnic = mkStrong ufcil qnc;
        uqnac = mkStrong ufcal qnc;
        eaqnic = mkStrong eafcil qnc;
        muqnac = "م" + uqnac;
        eiqnAc = mkStrong eifcAl qnc
     in toSoundForms eaqnac euqnic uqnic uqnac eaqnic muqnac eiqnAc ;

  v4DefForms : Root3 -> DefForms = \cTy ->
    let _cTa = mkDefective fca cTy;
        _cTu = mkDefective fcu cTy;
        _cTi = mkDefective fci cTy;
        eacTa = "أَ" + _cTa;           -- VPerf Act (Per3 Masc Sg)
        eacTay = mkStrong eafcal cTy ; -- VPerf Act (Per3 Fem  Pl)
        eucTi = "أُ" + _cTi;           -- VPerf Pas (Per3 _    Sg)
        eucTu = "أُ" + _cTu;           -- VPerf Pas (Per3 Masc Pl)
        eucTiy = mkStrong eufcil cTy ; -- VPerf Pas (Per3 Fem  Pl)
        ucTi = "ُ" + _cTi;  -- VImpf Act
        ucTu = "ُ" + _cTu;  -- VImpf Act (Per2/3 Masc Pl)
        ucTa = "ُ" + _cTa;  -- VImpf Pas
        eacTi = "أَ" + _cTi; -- VImp (Masc Sg / Fem _)
        eacTu = "أَ" + _cTu; -- VImp Masc Pl
        mucTaY = "م" + ucTa +"ى" ;
        eicTA' = mkStrong eifcAl (cTy ** {l="ء"}) ;
    in toDefForms eacTa eacTay eucTi eucTu eucTiy -- VPerf
                  ucTi ucTu ucTi ucTa             -- VImpf
                  eacTi eacTu mucTaY eicTA' ;

  v4hollow : Root3 -> Verb = \rwd ->
    let earad = mkHollow eafac rwd ; -- VPerf Act (Per3 Fem Pl) etc.
        earAd = mkHollow eafAc rwd ; -- VPerf Act
        eurid = mkHollow eufic rwd ; -- VPerf Pas (Per3 Fem Pl) etc.
        eurId = mkHollow eufIc rwd ; -- VPerf Pas

        urid = mkHollow ufic rwd ; -- VImpf Act (Per2/Per3 Fem Pl)
        urId = mkHollow ufIc rwd ; -- VImpf Act
        urad = mkHollow ufac rwd ; -- VImpf Pas (Per2/Per3 Fem Pl)
        urAd = mkHollow ufAc rwd ; -- VImpf Pas

        earid = mkHollow eafic rwd ; -- VImp (Sg Masc / Pl Fem)
        earId = mkHollow eafIc rwd ; -- VImp (Pl Masc / Sg Fem)

        ppart = "م" + urAd ;
        eirAdat = mkHollow eifcAl rwd + "َة" ;
    in verbHollow (toDefForms
                     earAd earad eurId eurid -- VPerf
                     urId urid urAd urad     -- VImpf
                     earId earid ppart eirAdat) ;

  v4sound : Root3 -> Verb = \qnc ->
    verb' (v4soundForms qnc) ;

  -- TODO: other differences
  v4assimilated : Root3 -> Verb = \wqf ->
    let eIqAf = mkStrong eIfcAl (wqf ** {f=""}) ;
        vforms_snd = v4soundForms wqf ;
        vforms_ass = table {6 => eIqAf ; n => vforms_snd ! n}
    in verb' vforms_ass ;

  v4defective : Root3 -> Verb = \cTy ->
    verbDef False (v4DefForms cTy) i ;

  v4doubleweak : Root3 -> Verb = \r'y ->
    let ry = r'y ** {c = ""} ;
        r' = ry  ** {l = "ء"} ;
        eirA'at = mkStrong eifcAl r' + "َة" ;
        vforms : DefForms = table {
          12 => rmSukun eirA'at ;
          n  => rmSukun (v4DefForms ry ! n) -- only remove the first sukun
        } ;
     in verbDoubleDef vforms i ; -- sukun in suffixes is removed in verbDoubleDef

  ---------
  -- Form V
  v5sound : Root3 -> Verb = \nfs ->
    let tanaffas = mkStrong tafaccal nfs ;
        tunuffis = mkStrong tufuccil nfs ;
        atanaffas  = "َ" + tanaffas ;
        utanaffas = "ُ" + tanaffas ;
        mutanaffas = "م" + tanaffas ;
        tanaffus = mkStrong tafaccul nfs
     in verb tanaffas tunuffis atanaffas utanaffas tanaffas mutanaffas tanaffus ;

  ----------
  -- Form VI
  v6sound : Root3 -> Verb = \fqm ->
    let tafAqam = mkStrong tafAcal fqm ;
        tufUqim = mkStrong tufUcil fqm ;
        atafAqam = "َ" + tafAqam ;
        utafAqam = "ُ" + tafAqam ;
        mutafAqam = "م" + utafAqam ;
        tafAqum = mkStrong tafAcul fqm ;
     in verb tafAqam tufUqim atafAqam utafAqam tafAqam mutafAqam tafAqum;

  -----------
  -- Form VII
  v7sound : Root3 -> Verb = \fcl ->
    let inficAl = "اِ" + mkStrong ficAl fcl ;
        vforms = v1soundForms fcl a i inficAl ;
        _nfacil = "نْ" + mkStrong facil fcl ;
        infacal = "اِنْ" + vforms ! 0 ;
        anfacil = "َ" + _nfacil ;
        infacil = "اِ" + _nfacil ;
        munfacil = "مُ" + _nfacil ;
     in verb' (table {
           0 => infacal ;
           2 => anfacil ;
           4 => infacil ;
           5 => munfacil ;
           n => "ُنْ" + vforms ! n -- doesn't exist for form 7
        }) ;

  v7geminate : Str -> Verb = \fcl ->
    let inficAc = "اِنْ" + mkStrong ficAl (mkRoot3 fcl) ;
        vforms = v1geminateForms fcl a i inficAc ;
        _nfacc = "نْ" + vforms ! 0  ;
        _nfacic = "نْ" + mkStrong facil (mkRoot3 fcl) ;
        infacc = "اِ" + _nfacc ;        -- VPerf Act
        infacac = "اِنْ" + vforms ! 1 ; -- VPerf Act Pl3F
        anfacc = "َ" + _nfacc ;         -- VImpf Act
        anfacic = "َ" + _nfacic ;       -- VImpf Act Pl3F
        infacic = "اِ" + _nfacic ;      -- VImp PlF
        munfacc = "مُ" + _nfacc ;       -- VPPart
     in verbGeminate (table {
           0 => infacc ;
           1 => infacac ;
           4 => anfacc ;
           5 => anfacic ;
           8 => infacc ;
           9 => infacic ;
           10 => munfacc ;
           n => "ُنْ" + vforms ! n -- doesn't exist for form 7
      }) ;

  ------------
  -- Form VIII
  v8sound : Root3 -> Verb = \rbT ->
    let rtabiT = mkStrong ftacil rbT ;
        rtabaT = mkStrong ftacal rbT ;
        rtibAT = mkStrong fticAl rbT ;
        eirtabaT = "إِ" + rtabaT ;
        eurtubiT = "أُ" + mkStrong ftucil rbT ;
        artabiT = "َ" + rtabiT ;
        urtabaT  = "ُ" + rtabaT ;
        eirtabiT = "إِ" + rtabiT ;
        murtabaT =  "م" + urtabaT ;
        irtibAT = "اِ" + rtibAT ;
     in verb eirtabaT eurtubiT artabiT urtabaT eirtabiT murtabaT irtibAT ;

  v8geminate : Str -> Verb = \rootStr ->
    let mdd = mkRoot3 rootStr ; --fcc
        md  = mkRoot2 rootStr ; --fc
        _mtadd = mkBilit ftacc md ;
        _mtadad = mkStrong ftacal mdd ;
        _mtadid = mkStrong ftacil mdd ;
        _mtudd = mkBilit ftucc md ;
        _mtudid = mkStrong ftucil mdd ;
        imtadd = "اِ" + _mtadd ;
        imtadad = "اِ" + _mtadad ;
        umtudd = "اُ" + _mtudd ;
        umtudid = "اُ" + _mtudid ;
        amtadd = "َ" + _mtadd ;
        amtadid = "َ" + _mtadid ;
        umtadd = "ُ" + _mtadd ;
        umtadad = "ُ" + _mtadad ;
        imtadid = "اِ" + _mtadid ;
        mumtadd = "مُ" + _mtadd ;
        imtidAd = "اِ" + mkStrong fticAl mdd ;
     in verbGeminate (toDefForms
          imtadd imtadad umtudd umtudid -- VPerf
          amtadd amtadid umtadd umtadad -- VPres
          imtadd imtadid mumtadd imtidAd) ;

  v8assimilated : Root3 -> Verb = \wfq ->
    let ttafiq = mkWeak ttacil wfq ;
        ttafaq = mkWeak ttacal wfq ;
        ttifAq = mkWeak tticAl wfq ;
        ittafaq = "اِ" + ttafaq ;
        euttufiq = mkWeak euttucil wfq ; -- TODO check
        attafiq = "َ" + ttafiq ;
        uttafaq  = "ُ" + ttafaq ;
        ittafiq = "اِ" + ttafiq ;
        muttafaq =  "م" + uttafaq ;
        ittifAq = "اِ" + ttifAq ;
     in verb ittafaq euttufiq attafiq uttafaq ittafiq muttafaq ittifAq ;

  v8hollow : Root3 -> Verb = \Hwj ->
    let _Htaj = mkHollow ftacal Hwj ;
        _HtAj = mkHollow ftAcal Hwj ;
        _Htij = mkHollow ftical Hwj ;
        _HtIj = mkHollow ftIcal Hwj ;
        iHtaj = "اِ" + _Htaj ;  -- VPerf Act (Per3 Fem Pl)
        iHtAj = "اِ" + _HtAj ;  -- VPerf Act _
        uHtij = "اُ" + _Htij ;  -- VPerf Pas (Per3 Fem Pl)
        uHtIj = "اُ" + _HtIj ;  -- VPerf Pas _
        aHtaj = "َ" + _Htaj ; -- VImpf Act (Per2/Per3 Fem Pl)
        aHtAj = "َ" + _HtAj ; -- VImpf Act _
        uHtaj = "ُ" + _Htaj ; -- VImpf Pas (Per2/Per3 Fem Pl)
        uHtAj = "ُ" + _Htaj ; -- VImpf Pas _
        -- iHtaj again          -- VImp Sg Masc / Pl Fem
        -- iHtAj again          -- VImp Pl Masc / Sg Fem
        ppart = "مُ" + _HtAj ;  -- PPart
        iHtiyAj = "اِ" + mkStrong fticAl (Hwj ** {c="ي"}) ;
     in verbHollow (toDefForms
                      iHtAj iHtaj uHtIj uHtij
                      aHtAj aHtaj uHtAj uHtaj
                      iHtAj iHtaj ppart iHtiyAj) ;

  ---------
  -- Form X
  v10sound : Root3 -> Verb = \qtl ->
    let _staqtal = "ستَ" + mkStrong fcal qtl ;
        _staqtil = "ستَ" + mkStrong fcil qtl ;
        istaqtal = "اِ" + _staqtal ; -- VPerf Act
        ustuqtil = "اُسْتُ" + mkStrong fcil qtl; -- VPerf Pas
        astaqtil = "َ"  + _staqtil ; -- VImpf _ Act
        astaqtal = "َ"  + _staqtal ;  -- VImpf _ Pas
        istaqtil = "اِ" + _staqtil ; -- VImp
        mustaqtal = "مُ" + _staqtal ;  -- VPPart
        istiqtAl = "اِستِ" + mkStrong fcAl qtl ;
     in verb istaqtal ustuqtil astaqtil astaqtal istaqtil mustaqtal istiqtAl ;

  v10hollow : Root3 -> Verb = \xwf ->
    let _staxaf = "سْتَ" + mkHollow fac xwf ;
        _staxAf = "سْتَ" + mkHollow fAc xwf ;
        _staxif = "سْتَ" + mkHollow fic xwf ;
        _staxIf = "سْتَ" + mkHollow fIc xwf ;
        istaxaf = "اِ" + _staxaf ;            -- VPerf Act (Per3 Fem Pl)
        istaxAf = "اِ" + _staxAf ;            -- VPerf Act _
        ustuxif = "اُسْتُ" + mkHollow fic xwf ; -- VPerf Pas (Per3 Fem Pl)
        ustuxIf = "اُسْتُ" + mkHollow fIc xwf ; -- VPerf Pas _
        istaxif = "اِ" + _staxif ;  -- VImp Sg Masc / Pl Fem
        istaxIf = "اِ" + _staxIf ;  -- VImp Pl Masc / Sg Fem
        astaxif = "َ" + _staxif ; -- VImpf Act (Per2/Per3 Fem Pl)
        astaxIf = "َ" + _staxIf ; -- VImpf Act _
        ustaxaf = "ُ" + _staxaf ; -- VImpf Pas (Per2/Per3 Fem Pl)
        ustaxAf = "ُ" + _staxAf ; -- VImpf Pas _
        ppart = "مُ" + _staxIf ; -- PPart ("weird anomalies" here too?)
        istixAfat = "اِسْتِ" + mkHollow fAc xwf + "َة" ;
     in verbHollow (toDefForms
                      istaxAf istaxaf ustuxIf ustuxif
                      astaxIf astaxif ustaxAf ustaxaf
                      istaxif istaxIf ppart istixAfat) ;

  v10defective : Root3 -> Verb = \lqy ->
    let _stalqa = "سْتَ" + mkDefective fca lqy ;
        _stalqu = "سْتَ" + mkDefective fcu lqy ;
        _stalqi = "سْتَ" + mkDefective fci lqy ;
        _stulqi = "سْتُ" + mkDefective fci lqy ;

        istalqa  = "اِ" + _stalqa ;               -- VPerf Act (Per3 Masc Sg)
        istalqay = "اِسْتَ" + mkStrong fcal lqy ; -- VPerf Act (Per3 Fem Pl)
        ustulqi = "اُ" + _stulqi; -- VPerf Pas (Per3 _ _)

        astalqu = "َ" + _stalqu ; -- VImpf Act (Per2/3 Masc Pl)
        astalqi = "َ" + _stalqi ; -- VImpf Act _
        ustalqa = "ُ" + _stalqa ; -- VImpf Pas _
        istalqi = "اِ" + _stalqi; -- VImp (Masc Sg / Fem _)
        istalqu = "اِ" + _stalqu; -- VImp Masc Pl
        mustalqin = "مُ" + _stalqi + "ت" ;

        lq' = lqy ** {l = "ء"} ;
        istilqA' = "اِسْتِ" + mkStrong fcAl lq' ;
     in verbDef False
               (toDefForms
                    istalqa istalqay ustulqi ustulqi ustulqi
                    astalqi astalqu astalqi ustalqa
                    istalqi istalqu mustalqin istilqA') i ;

  v10geminate : Str -> Verb = \fcl ->
    let istifcAc = "اِسْتِ" + mkStrong fcAl (mkRoot3 fcl) ;
        vforms = v1geminateForms fcl a i istifcAc ;
        _stafacc = "سْتَ" + vforms ! 0 ;
        _stafcac = "سْتَ" + mkStrong fcal (mkRoot3 fcl) ;
        _staficc = "سْت" + vforms ! 4 ; -- vowel is in the stem from vforms
        _stafcic = "سْت" + vforms ! 5 ; -- vowel is in the stem from vforms
        istafacc = "اِ" + _stafacc ;  -- VPerf Act
        istafcac = "اِ" + _stafcac ;  -- VPerf Act Pl3F
        astaficc = "َ" + _staficc ;   -- VImpf Act
        astafcic = "َ" + _stafcic ;   -- VImpf Act Pl3F
        istaficc = "اِ" + _staficc ;  -- VImp
        istafcic = "اِ" + _stafcic ;  -- VImp PlF
        mustafacc = "مُ" + _stafacc ; -- VPPart

     in verbGeminate (table {
           0 => istafacc ;
           1 => istafcac ;
           4 => astaficc ;
           5 => astafcic ;
           8 => istaficc ;
           9 => istafcic ;
           10 => mustafacc ;
           n@(2|3) => "ُسْتُ" + vforms ! n ; -- ???
           n@(6|7) => "ُسْتَ" + vforms ! n ; -- ???
           n => vforms ! n
      }) ;

  --------------------------
  -- Form XI (quadriliteral)
  v11sound : Root3 -> Verb = \fclb ->
    let faclabat = mkStrong facalp fclb ;
        faclib = mkStrong facil fclb ;
        faclab = mkStrong facal fclb ;
        vforms = table {
          2 => "ُ" + faclib ; -- VImpf Act
          3 => "ُ" + faclab ; -- VImpf Pas
          5 => "مُ" + faclab ; -- VPPart
          n => v1soundForms fclb a i faclabat ! n
        } ;
     in verb' vforms ;

}
