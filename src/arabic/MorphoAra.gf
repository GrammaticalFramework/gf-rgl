resource MorphoAra = PatternsAra ** open Prelude, OrthoAra, Predef in  {

flags optimize = all ;--noexpand;
  coding=utf8 ;

param

  Vowel   = u | a | i ;
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
          | VOS      -- Relative clauses with resumptive pronouns
          | Subord ; -- Nominal word order but subject in accusative

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
-- Verbal morphology

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

  Verb : Type = {s : VForm => Str} ;


-- IL -- Defective needs max 13 forms, hollow and geminate verbs need 12 forms.
      -- NB. the numbers don't always refer to the same forms!
      -- The verb(Def|Hollow|Geminate) constructors pick the right forms.
  DefForms   : Type = Predef.Ints 12 => Str ;
  SoundForms : Type = Predef.Ints 6  => Str ; -- To share code better

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

  patV1Perf : Vowel => Pattern =
    table {
      a => facal ; --katab
      u => facul ; --Hasun
      i => facil   --rabiH
    } ;

  patV1Impf : Vowel => Pattern =
    table {
      u => fcul ;  --ktub
      a => fcal ;  --rbaH
      i => fcil    --Hsin
    } ;

  patDef1 : Vowel => Pattern =
    table {
      u => fcu ;
      a => fca ;
      i => fci
    } ;

  patDef2 : Vowel => Pattern =
    table {
      a => fca ;
      _ => fcu
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

  patHollowPerf : (_,_,_,_ :Str) -> Voice => PerGenNum => Str = \xAf,xif,xIf,xuf ->
    table {
      Act =>
        table {
          Per3 Fem Pl => xif ;
          Per3 _   _  => xAf ;
          _           => xif
        } ;
      Pas =>
        table {
          Per3 Fem Pl => xuf ;
          Per3 _   _  => xIf ;
          _           => xuf
        }
     } ;

  --this is the pattern of imperfect  hollow (ind & conj) and geminate verbs (all)
  patHollowImpf : (_,_,_,_ :Str) -> Voice => PerGenNum => Str = \axAf,axaf,uxAf,uxaf ->
    table {
      Act =>
        table {
        Per3 Fem Pl => axaf ;
        Per2 Fem Pl => axaf ;
        _   => axAf
      } ;
      Pas =>
        table {
        Per3 Fem Pl => uxaf ;
        Per2 Fem Pl => uxaf ;
        _   => uxAf
      }
    } ;

  patHollowJus : (_,_,_,_ : Str) -> Voice => PerGenNum => Str =\axaf,axAf,uxaf,uxAf->
    table {
      Act =>
        table {
          Per3 _   Sg  => axaf ;
          Per3 Fem Pl  => axaf ;
          Per2 Fem Pl  => axaf ;
          Per2 Masc Sg => axaf ;
          Per1 _       => axaf ;
          _            => axAf
      } ;
      Pas =>
        table {
          Per3 _   Sg  => uxaf ;
          Per3 Fem Pl  => uxaf ;
          Per2 Fem Pl  => uxaf ;
          Per2 Masc Sg => uxaf ;
          Per1 _       => uxaf ;
          _            => uxAf
      }
    } ;

  patHollowImp : (_,_ :Str) -> Gender => Number => Str =\xaf,xAf ->
    table {
      Masc => table { Sg => xaf ; _ => xAf} ;
      Fem  => table { Pl => xaf ; _ => xAf}
    } ;

  patGeminateImp : (_,_ :Str) -> Gender => Number => Str = \facc,facic ->
    \\g,n => case <g,n> of {
        <Fem,Pl> => facic ;
        _        => facc
    } ;

  patDefPerf : (_,_,_,_,_ :Str) -> Voice => PerGenNum => Str = \rama,ramay,rumi,rumu,rumy ->
    table {
      Act =>
        table {
          Per3 Fem Pl => ramay ;
          Per3 _   _  => rama ;
          _           => ramay
          } ;
      Pas =>
        table {
          Per3 Masc Pl => rumu ;
          Per3 Fem Pl  => rumy ;
          Per3 _   _   => rumi ;
          _            => rumy
        }
    } ;

  --now includes the vowel=u case, eg "دعو" /IL 2019-01-18
  patDefImpfAct :  (x1,_,x3 : Str) -> PerGenNum => Str = \rmi,rmu,d3i ->
  table {
    Per3 Masc Pl => rmu ;
    Per2 Masc Pl => rmu ;
    Per2 Fem  Sg => d3i ; -- for 1d3:    d3i different, rmi = rmu
    _            => rmi   -- for others: rmu different, rmi = d3i
    } ;


  patDefImp : (_,_ : Str) -> Gender => Number => Str = \rmi, rmu ->
    table {
      Masc => table {Pl => rmu ; _ => rmi} ;
      _    => table {_ => rmi}
    } ;

  patHol1 : Vowel => Pattern =
    table { u => fuc ; _ => fic} ;

  patHol2 : Vowel => Pattern =
    table { u => fic ; _ => fuc} ;

  fVc : Vowel => Pattern =
    table {
      u => fUc ;
      i => fIc ;
      a => fAc
    } ;

  --used in assimilated (wqf -> qif, wqc -> qac..) and hollow (qwl -> qul, xwf->xaf..)
  fvc : Vowel => Pattern =
    table {
      u => fuc ;
      i => fic ;
      a => fac
    } ;

  --macro for sound verb
  --PerfAct, PerfPas, ImpfAct, ImpfPas, Imp, PPart, Masdar
  verb : (x1,_,_,_,_,_,x7 : Str) -> Verb =
    \katab,kutib,aktub,uktab,euktub,maktUb,katb -> {
      s = \\vf => rectifyHmz (case vf of {
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


  --macro for hollow verbs:
  verbHollow : DefForms -> Verb =
  \vforms ->
    let { xAf = vforms ! 0 ; -- VPerf Act _
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

          patPerf = patHollowPerf xAf xif xIf xuf ;
          patImpf = patHollowImpf axAf axaf uxAf uxaf ;
          patJus  = patHollowJus axaf axAf uxaf uxAf ;
          patImp  = patHollowImp impSg impPl ;
    } in
    { s = table {
        VPerf     v pgn =>                    patPerf ! v ! pgn + suffixPerf ! pgn ;
        VImpf Ind v pgn => prefixImpf ! pgn + patImpf ! v ! pgn + suffixImpfInd ! pgn ;
        VImpf Cnj v pgn => prefixImpf ! pgn + patImpf ! v ! pgn + suffixImpfCJ Cnj ! pgn ;
        VImpf Jus v pgn => prefixImpf ! pgn + patJus  ! v ! pgn + suffixImpfCJ Jus ! pgn ;
        VImp        g n =>                    patImp  ! g ! n   + suffixImpfCJ Jus ! Per2 g n ;
        VPPart          => ppart ;
        Masdar          => masdar
        }
    } ;

  -- macro for geminate verbs: same behaviour as hollow verbs,
  -- except for jussive and imperative. /IL
  verbGeminate : DefForms -> Verb = \vforms ->
    let verbHol = verbHollow vforms ;
        patImp = patGeminateImp (vforms ! 8) (vforms ! 9)
    in { s = table { -- Jussive and imperative have fatha instead of sukun
           VImpf Jus v pgn => verbHol.s ! VImpf Cnj v pgn ;
           VImp        g n => patImp ! g ! n + suffixImpfCJ Cnj ! Per2 g n ;
           x               => verbHol.s ! x
           }
       } ;

  --macro for defective verbs:                              -- isDoubleDef
  verbDef : (normalAlif : Bool) -> DefForms -> Vowel -> Verb = verbDefBool False ;
  verbDoubleDef : DefForms -> Vowel -> Verb = verbDefBool True False ;

  -- if the last radical is waaw, then use normal alif instead of alif maqsuura
  normalAlif : Root3 -> Bool = \r -> case r.l of {"و" => True ; _ => False} ;

  verbDefBool : Bool -> Bool -> DefForms -> Vowel -> Verb =
    \isDoubleDef,alif,vforms,vowImpf ->
    let {
       rama  = vforms ! 0 ; -- VPerf Act (Per3 Masc Sg)
       ramay = vforms ! 1 ; -- VPerf Act (Per3 Fem  Pl)
       rumi  = vforms ! 2 ; -- VPerf Pas (Per3 _    Sg)
       rumu  = vforms ! 3 ; -- VPerf Pas (Per3 Masc Pl)
       rumiy = vforms ! 4 ; -- VPerf Pas (Per3 Fem  Pl)
       armi  = vforms ! 5 ; -- VImpf _ Act (Per1 _ _ | Per3 Fem _ | Per2/3 Masc Sg)
       armu  = vforms ! 6 ; -- VImpf _ Act (Per2/3 Masc Pl)
       ad3i  = vforms ! 7 ; -- Per2 Fem
       urma  = vforms ! 8 ; -- VImpf _ Pas
       Irmi  = vforms ! 9 ; -- VImp Masc Sg | VImp Fem _
       Irmu  = vforms ! 10 ; -- VImp Masc Pl
       ppart = vforms ! 11 ; -- VPPart
       masdar = vforms ! 12 ; -- verbal noun

       patPerf = patDefPerf rama ramay rumi rumu rumiy ;
       patImpfAct = patDefImpfAct armi armu ad3i ;
       patImp = patDefImp Irmi Irmu ;
       suffixImpf = case isDoubleDef of {True => suffixImpfDoubleDef ; _ => suffixImpfDef}
    } in
    { s = table {
        VPerf   v   pgn =>                    patPerf ! v ! pgn + suffixPerfDef v alif   ! pgn ;
        VImpf m Act pgn => prefixImpf ! pgn + patImpfAct  ! pgn + suffixImpf Act vowImpf ! m ! pgn ;
        VImpf m Pas pgn => prefixImpf ! pgn + urma              + suffixImpf Pas vowImpf ! m ! pgn ;
        VImp        g n =>                    patImp ! g ! n    + suffixImpf Act vowImpf ! Jus ! Per2 g n ;
        VPPart          => ppart ;
        Masdar          => masdar
        }
    } ;


  suffixPerfDef : Voice -> Bool -> PerGenNum => Str = \v,normalAlif ->
    let p3ms = case v of {
                 Act => if_then_Str normalAlif "ا" "ى" ;
                 Pas => "يَ" } ;
        ya = case v of {
               Act => "" ;
               Pas => "يَ" }
    in
    table {
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

  suffixImpfDef : Voice -> Vowel -> Mood => PerGenNum => Str = \vc,vw ->
    let {
      default : Mood -> Str = \m ->
        case vc of {
          Pas => case m of {Jus => "" ; _ => "ى"} ;
          Act => case vw of {
            u => case m of {Ind => "و" ; Cnj => "وَ" ; Jus => ""} ;
            i => case m of {Ind => "ي" ; Cnj => "يَ" ; Jus => ""} ;
            a => case m of {Ind => "ى" ; Cnj => "ى" ; Jus => ""}
            }
        }
    } in
    table {
      Ind =>
        table {
          Per3 Masc Pl => "وْنَ" ;
          Per2 Masc Pl => "وْنَ" ;
          (Per3 _ Dl|Per2 _ Dl) => case vw of {
                     u => "وَانِ" ;
                     _ => "يَانِ" } ;
          Per3 Fem  Pl => "يْنَ" ;
          Per2 Fem  _  => "يْنَ" ;
          _            => default Ind
        } ;
      m => -- TODO: check whether to remove sukuns
        table {
          Per3 Masc Pl => "وْا" ;
          Per2 Masc Pl => "وْا" ;
          (Per3 _ Dl|Per2 _ Dl) => case vw of {
                     u => "وَا" ;
                     _ => "يَا" } ;
          Per3 Fem  Pl => "يْنَ" ;
          Per2 Fem  Pl => "يْنَ" ;
          Per2 Fem  Sg => "ي" ;
          _            => default m
        }
    } ;

  -- does this even happen other than with رءي? /IL
  suffixImpfDoubleDef : Voice -> Vowel -> Mood => PerGenNum => Str = \vc,vw ->
    \\m,p => rmSukun (suffixImpfDef vc vw ! m ! p) ;

  --is used for the sound, assimilated (weak C1), and when C1 = hamza: /AED
  -- TODO check if this is still true /IL
  v1soundForms : Root3 -> Vowel -> Vowel -> (masdar:Str) -> SoundForms =
    \fcl,vowPerf,vowImpf,masdar ->
    let {
      qf = {f = fcl.c ; c = fcl.l} ;
      qif = mkBilit (fvc ! vowImpf) qf;
      katab = mkStrong (patV1Perf ! vowPerf) fcl ;
      kutib = mkStrong fucil fcl ; --FIXME no passive if vowPerf == u
      ktub  = mkStrong (patV1Impf ! vowImpf) fcl ;
      aktub = "َ" +
        case fcl.f of {
          "و"|"ي" => qif ;
          _       => ktub
        };
      uktab = mkStrong ufcal fcl ;
      euktub = case fcl.f of {
        "ء"|"و"|"ي" => qif ;
         _          => prefixImp ! vowImpf + ktub
        };
      maktUb = mkStrong mafcUl fcl
    } in
    toSoundForms katab kutib aktub uktab euktub maktUb masdar ;

  v1sound : Root3 -> Vowel -> Vowel -> (masdar:Str) -> Verb =
    \fcl,vp,vi,masdar -> verb' (v1soundForms fcl vp vi masdar) ;

  v1hollow : Root3 -> Vowel -> (masdar:Str) -> Verb =
    \xwf,vowImpf,masdar ->
    let {
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
      ppart = "مَ" + xAf' -- FIXME actually wierd anomalies happen with the a vowel..

    } in verbHollow (toDefForms
           xAf xif xIf xuf
           axAf axaf uxAf uxaf
           xaf xAf' ppart masdar) ;

  v1geminate : Str -> Vowel -> Vowel -> (masdar:Str) -> Verb =
    \r,vp,vi,masdar -> verbGeminate (v1geminateForms r vp vi masdar) ;

  v1geminateForms : Str -> Vowel -> Vowel -> (masdar:Str) -> DefForms =
    \rootStr,vowPerf,vowImpf,masdar ->
    let {
      mdd = mkRoot3 rootStr ; --fcc
      md  = mkRoot2 rootStr ; --fc
      madd = mkBilit facc md ;
      madad = mkStrong (patGem1 ! vowPerf) mdd ;
      mudd = mkBilit fucc md ;
      mudid = mkStrong fucil mdd ;
      mudd' = mkBilit (patGem2 ! vowImpf) md ;
      amudd = "َ" + mudd' ;
      mdud = mkStrong (patGem3 ! vowImpf) mdd ;
      amdud = "َ" + mdud ;
      umadd = "ُ" + madd ;
      umdad = "ُ" + mkStrong fcal mdd ;
      Umdud = (prefixImp ! vowImpf) + mdud;
      mamdUd = mkStrong mafcUl mdd
     } in toDefForms
            madd madad mudd mudid   -- VPerf
            amudd amdud umadd umdad -- VImpf
            Umdud mudd' mamdUd masdar ;

  patGem1 : Vowel => Pattern =
    table {
      a => facal ;
      u => facul ;
      i => facil
    } ;

  patGem2 : Vowel => Pattern =
    table {
      u => fucc ;
      a => facc ;
      i => ficc                --no such verb probably exists
    } ;

  patGem3 : Vowel => Pattern =
    table {
      u => fcul ;
      a => fcal ;
      i => fcil                --no such verb probably exists
    } ;

  v1defForms_perfA : Root3 -> Vowel -> (masdar:Str) -> DefForms = \rmy,vowImpf,masdar ->
   let {
     _rmi = mkDefective (patDef1 ! vowImpf) rmy ;
     _rmu = mkDefective (patDef2 ! vowImpf) rmy ;
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
   } in toDefForms
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

v4soundForms : Root3 -> SoundForms = \qnc ->
  let {
    eaqnac = mkStrong eafcal qnc;
    euqnic = mkStrong eufcil qnc;
    uqnic = mkStrong ufcil qnc;
    uqnac = mkStrong ufcal qnc;
    eaqnic = mkStrong eafcil qnc;
    muqnac = "م" + uqnac;
    eiqnAc = mkStrong eifcAl qnc
  } in
  toSoundForms eaqnac euqnic uqnic uqnac eaqnic muqnac eiqnAc;

v4sound : Root3 -> Verb = \qnc ->
  verb' (v4soundForms qnc) ;
-- TODO: other differences
v4assimilated : Root3 -> Verb = \wqf ->
  let eIqAf = mkStrong eIfcAl (wqf ** {f=""}) ;
      vforms_snd = v4soundForms wqf ;
      vforms_ass = table {6 => eIqAf ; n => vforms_snd ! n}
  in verb' vforms_ass ;

v4hollow : Root3 -> Verb =
  \rwd ->
  let {
    earad = mkHollow eafac rwd ; -- VPerf Act (Per3 Fem Pl) etc.
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

  } in verbHollow (toDefForms
                      earAd earad eurId eurid -- VPerf
                      urId urid urAd urad     -- VImpf
                      earId earid ppart eirAdat) ;

 v4DefForms : Root3 -> DefForms = \cTy ->
  let {
    _cTa = mkDefective fca cTy;
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
  } in toDefForms eacTa eacTay eucTi eucTu eucTiy -- VPerf
                  ucTi ucTu ucTi ucTa             -- VImpf
                  eacTi eacTu mucTaY eicTA' ;

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

v5sound : Root3 -> Verb =
  \nfs ->
  let {
    tanaffas = mkStrong tafaccal nfs ;
    tunuffis = mkStrong tufuccil nfs ;
    atanaffas  = "َ" + tanaffas ;
    utanaffas = "ُ" + tanaffas ;
    mutanaffas = "م" + tanaffas ;
    tanaffus = mkStrong tafaccul nfs
  } in verb tanaffas tunuffis atanaffas utanaffas tanaffas mutanaffas tanaffus;

v6sound : Root3 -> Verb =
  \fqm ->
  let {
    tafAqam = mkStrong tafAcal fqm ;
    tufUqim = mkStrong tufUcil fqm ;
    atafAqam = "َ" + tafAqam ;
    utafAqam = "ُ" + tafAqam ;
    mutafAqam = "م" + utafAqam ;
    tafAqum = mkStrong tafAcul fqm ;
  } in verb tafAqam tufUqim atafAqam utafAqam tafAqam mutafAqam tafAqum;

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

v8sound : Root3 -> Verb =
  \rbT ->
  let {
    rtabiT = mkStrong ftacil rbT ;
    rtabaT = mkStrong ftacal rbT ;
    rtibAT = mkStrong fticAl rbT ;
    eirtabaT = "إِ" + rtabaT ;
    eurtubiT = "أُ" + mkStrong ftucil rbT ;
    artabiT = "َ" + rtabiT ;
    urtabaT  = "ُ" + rtabaT ;
    eirtabiT = "إِ" + rtabiT ;
    murtabaT =  "م" + urtabaT ;
    irtibAT = "اِ" + rtibAT ;
  } in verb eirtabaT eurtubiT artabiT urtabaT eirtabiT murtabaT irtibAT;

v8geminate : Str -> Verb =
  \rootStr ->
  let {
    mdd = mkRoot3 rootStr ; --fcc
    md  = mkRoot2 rootStr ; --fc
    _mtadd = mkBilit ftacc md ;
    _mtadad = mkStrong ftacal mdd ;
    _mtadid = mkStrong ftacil mdd ;
    _mtudd = mkBilit ftucc md ;
    _mtudid = mkStrong ftucil mdd ;
    _mtidAd = mkStrong fticAl mdd ;
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
    imtidAd = "اِ" + _mtidAd ;
  } in verbGeminate (toDefForms
        imtadd imtadad umtudd umtudid -- VPerf
        amtadd amtadid umtadd umtadad -- VPres
        imtadd imtadid mumtadd imtidAd) ;

v8assimilated : Root3 -> Verb = --- IL 8a1
  \wfq ->
  let {
    ttafiq = mkWeak ttacil wfq ;
    ttafaq = mkWeak ttacal wfq ;
    ttifAq = mkWeak tticAl wfq ;
    ittafaq = "اِ" + ttafaq ;
    euttufiq = mkWeak euttucil wfq ; -- TODO check
    attafiq = "َ" + ttafiq ;
    uttafaq  = "ُ" + ttafaq ;
    ittafiq = "اِ" + ttafiq ;
    muttafaq =  "م" + uttafaq ;
    ittifAq = "اِ" + ttifAq ;
  } in verb ittafaq euttufiq attafiq uttafaq ittafiq muttafaq ittifAq;

v8hollow : Root3 -> Verb = -- IL
  \Hwj ->
  let {
    _Htaj = mkHollow ftacal Hwj ;
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

  }  in verbHollow (toDefForms
                     iHtAj iHtaj uHtIj uHtij aHtAj aHtaj
                     uHtAj uHtaj iHtAj iHtaj ppart iHtiyAj) ;

v10sound : Root3 -> Verb = -- IL 10s -- to be checked
  \qtl ->
  let {
    _staqtal = "ستَ" + mkStrong fcal qtl ;
    _staqtil = "ستَ" + mkStrong fcil qtl ;
    _stiqtAl = "ستِ" + mkStrong fcAl qtl ;
    istaqtal = "اِ" + _staqtal ; -- VPerf Act
    ustuqtil = "اُسْتُ" + mkStrong fcil qtl; -- VPerf Pas
    astaqtil = "َ"  + _staqtil ; -- VImpf _ Act
    astaqtal = "َ"  + _staqtal ;  -- VImpf _ Pas
    istaqtil = "اِ" + _staqtil ; -- VImp
    mustaqtal = "مُ" + _staqtal ;  -- VPPart
    istiqtAl = "اِ" + _stiqtAl ;
  } in
  verb istaqtal ustuqtil astaqtil astaqtal istaqtil mustaqtal istiqtAl ;

v10hollow : Root3 -> Verb = -- IL 10h -- to be checked
  \xwf ->
  let {
    _staxaf = "سْتَ" + mkHollow fac xwf ;
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

  } in verbHollow (toDefForms
                     istaxAf istaxaf ustuxIf ustuxif astaxIf astaxif
                     ustaxAf ustaxaf istaxif istaxIf ppart istixAfat) ;

v10defective : Root3 -> Verb = -- IL
  \lqy ->
  let {
    _stalqa = "سْتَ" + mkDefective fca lqy ;
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

  } in verbDef False (toDefForms
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
