    --# -path=.:../abstract:../common:../../prelude
--
----1 Arabic auxiliary operations.
--
---- This module contains operations that are needed to make the
---- resource syntax work. To define everything that is needed to
---- implement $Test$, it moreover contains regular lexical
---- patterns needed for $Lex$.
--
resource ResAra = PatternsAra ** open  Prelude, Predef, OrthoAra, ParamX  in {

  flags optimize=noexpand ; coding=utf8 ;


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

  oper

-----------------------------------------------------------------------------
-- General morphology with roots, patterns, and making words:

    Pattern : Type = {h, m1, m2, t : Str};
    Root    : Type = {f : Str};
    Root2   : Type = Root ** {c : Str} ;
    Root3   : Type = Root2 ** {l : Str} ;

-- AR 7/12/2009 changed this to avoid duplication of consonants
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

    --opers to interdigitize (make words out of roots and patterns:

  oper
    --regular case, 3 non-weak consonants
    mkStrong : Pattern -> Root3 -> Str = \p,fcl ->
      p.h + fcl.f + p.m1 + fcl.c + p.m2 + fcl.l + p.t;

    mkDefective : Pattern -> Root3 -> Str = \p,fcl ->
      p.h + fcl.f + p.m1 + fcl.c + p.t;

    mkHollow : Pattern -> Root3 -> Str = \p,fcl ->
      p.h + fcl.f + p.m1 + fcl.l + p.t;

    mkAssimilated : Pattern -> Root3 -> Str = \p,fcl ->
      p.h + fcl.c + p.m1 + fcl.l + p.t;

    -- takes a weak pattern and a triliteral root and makes
    -- a word, deducing which root consonant is weak
    mkWeak : Pattern -> Root3 -> Str = \pat,fcl ->
      case <fcl.f,fcl.c,fcl.l> of {
       <_,_,("و"|"ي"|"ّ")> => mkDefective   pat fcl;
       <_,("و"|"ي"),_>    => mkHollow      pat fcl;
       <("و"|"ي"),_,_>    => mkAssimilated pat fcl
      };

    mkBilit : Pattern -> Root2 -> Str = \p,fcl ->
      p.h + fcl.f + p.m1 + fcl.c + p.t;

    --takes a pattern string and root string and makes a word
    mkWord : Str -> Str -> Str  =\pS, rS ->
      case pS of {
        w + "ف" + x + "ع" + y + "ل" + z =>
          let pat = { h = w ; m1 = x; m2 = y; t = z} in
          case rS of {
            x@? + y@? + "ّ" => mkStrong pat (mkRoot3 (x+y+y)) ; -- In principle, shadda shouldn't be in the root, but if someone puts one, this should fix it. /IL
            _               => mkStrong pat (mkRoot3 rS) } ;
        w + "ف" + x + "ع" + y =>
          let pat = { h = w ; m1 = x; m2 = ""; t = y} in
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

    --types of open classes:

    NTable = Number => State => Case => Str;
    emptyNTable : NTable = \\n,s,c => [] ;

    Preposition : Type = {s : Str ; c : Case} ;
    Noun : Type = {
      s,s2 : NTable ;
      g : Gender ;
      h : Species ;
      isDual : Bool -- whether it takes dual instead of plural: eyes, twins, ...
      } ;
    Noun2 : Type = Noun ** {c2 : Preposition} ;
    Noun3 : Type = Noun2 ** {c3 : Preposition} ;

    mkPreposition = overload {
      mkPreposition : Str -> Case -> Preposition = \s,c -> {s=s;c=c} ;
      mkPreposition : Str -> Preposition = \s -> {s=s;c=Gen} ;
    } ;

    noPrep : Preposition = mkPreposition [] Nom ;
    liPrep : Preposition = mkPreposition (
      pre { #pronSuffAndOther => "لِ" ;
            #pronSuff         => "لَ" ;
            _                 => "لِ" 
          }  ++ BIND) Dat ;
    biPrep : Preposition = mkPreposition ("بِ"++BIND) ;

    pronSuff : pattern Str = #("كَ"|"كِ"|"كُمَا"|"كُمْ"|"كُنَّ"|"هُ"|"ها"|"هُمَا"|"هُمْ"|"هُنَّ") ;
    pronSuffAndOther : pattern Str = #( "كَم" ) ; -- TODO list words that begin like pron.suff. but aren't

    Adj  : Type = {s : AForm => Str} ;
    Adj2 : Type = Adj ** {c2 : Preposition} ;

    Verb : Type = {s : VForm => Str} ;
    Verb2 : Type = Verb ** {c2 : Preposition} ;
    Verb3 : Type = Verb2 ** {c3 : Preposition} ;

    AP : Type = {s : Species => Gender => NTable } ;
    uttAP : AP -> (Gender => Str) ;
    uttAP ap = \\g => ap.s ! NoHum ! g ! Sg ! Def ! Nom ; ----IL

    CN : Type = Noun ** {np : Case => Str};

    -- All fields of NP
    cn2str : CN -> Number -> State -> Case -> Str = \cn,n,s,c ->
      cn.s   ! n ! s ! c ++
      cn.s2  ! n ! s ! c ++
      cn.np ! c ;

    useN : Noun -> CN = \n -> n ** {np = \\_ => []} ;

    uttCN : CN -> (Gender => Str) ;
    uttCN cn = \\_ => cn2str cn Sg Indef Bare ;

    NumOrdCard : Type = {
      s : Gender => State => Case => Str ;
      n : Size ;
      isNum : Bool
      } ;

    uttNum : NumOrdCard -> (Gender => Str) ;
    uttNum n = \\g => n.s ! g ! Def ! Nom ;  ----IL

  param
    VForm =
      VPerf Voice PerGenNum
      | VImpf Mood Voice PerGenNum
      | VImp Gender Number
      | VPPart ; -- TODO: add gender and number (or check if easy to use BIND)

    PerGenNum =
      Per3 Gender Number
      | Per2 Gender Number
      | Per1 SgPl;

    SgPl = Sing | Plur;

    AForm =
      APosit Gender Number State Case
      | AComp State Case ;

    --verbal morphology

oper

  --macro for sound verb
  --PerfAct, PerfPas, ImpfAct, ImpfPas, Imp, PPart
  verb : (_,_,_,_,_,_ : Str) -> Verb =
    \katab,kutib,aktub,uktab,euktub,maktUb -> {
      s = table {
        VPerf Act pgn     => katab + suffixPerf ! pgn ;
        VPerf Pas pgn     => kutib + suffixPerf ! pgn ;
        VImpf Ind Act pgn => prefixImpf!pgn + aktub + suffixImpfInd !pgn;
        VImpf Ind Pas pgn => prefixImpf!pgn + uktab + suffixImpfInd !pgn;
        VImpf m Act pgn => prefixImpf!pgn + aktub + suffixImpfCJ m ! pgn;
        VImpf m Pas pgn => prefixImpf !pgn + uktab + suffixImpfCJ m !pgn;
        VImp  g n  => euktub + suffixImpfCJ Jus ! (Per2 g n);
        VPPart     => maktUb
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
      _			=> "ت"
    } ;

  suffixImpfInd : PerGenNum => Str =
    table {
      Per3	Masc Pl => "ُونَ" ;
      Per3	Fem  Pl => "ْنَ" ;
      Per3	g	 Dl => "َانِ" ;
      Per2	Masc Pl => "ُونَ" ;
      Per2 Fem  Sg => "ِينَ" ;
      Per2	g	 Dl => "َانِ" ;
      Per2	Fem  Pl => "ْنَ" ;
      _			=> "ُ"
    } ;

  suffixImpfCJ : Mood -> PerGenNum => Str = \m ->
    table {
      Per3	Masc Pl => "ُوا" ;
      Per3	Fem  Pl => "ْنَ" ;
      Per3	g	 Dl => "َا" ;
      Per2	Masc Pl => "ُوا" ;
      Per2 Fem  Sg => "ِي" ;
      Per2	g	 Dl => "َا" ;
      Per2	Fem  Pl => "ْنَ" ;
      _			=> endVowel ! m
    } ;


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
        VPPart          => ppart
        }
    } ;

  --macro for geminate verbs: same behaviour as hollow verbs, except for jussive.
  -- IL -- to be tested: there are no geminate verbs in LexiconAra
  verbGeminate : DefForms -> Verb = \vforms ->
    let verbHol = verbHollow vforms
    in { s = table {
           VImpf Jus v pgn => verbHol.s ! VImpf Cnj v pgn ;
           x               => verbHol.s ! x
           }
       } ;

  --macro for defective verbs:
  verbDef : DefForms -> Vowel -> Verb = verbDefBool False ;
  verbDoubleDef : DefForms -> Vowel -> Verb = verbDefBool True ;

  verbDefBool : Bool -> DefForms -> Vowel -> Verb =
    \isDoubleDef,vforms,vowImpf ->
    let {
       rama  = vforms ! 0 ; -- VPerf Act (Per3 Masc Sg)
       ramay = vforms ! 1 ; -- VPerf Act (Per3 Fem  Pl)
       rumi  = vforms ! 2 ; -- VPerf Pas (Per3 _    Sg)
       rumu  = vforms ! 3 ; -- VPerf Pas (Per3 Masc Pl)
       rumiy = vforms ! 4 ; -- VPerf Pas (Per3 Fem  Pl)
       armi  = vforms ! 5 ; -- VImpf _ Act (Per1 _ _ | Per2/3 Fem _ | Per2/3 Masc Sg)
       armu  = vforms ! 6 ; -- VImpf _ Act (Per2/3 Masc Pl)
       urma  = vforms ! 7 ; -- VImpf _ Pas
       Irmi  = vforms ! 8 ; -- VImp Masc Sg | VImp Fem _
       Irmu  = vforms ! 9 ; -- VImp Masc Pl
       ppart = vforms ! 10 ; -- VPPart

       patPerf = patDefPerf rama ramay rumi rumu rumiy ;
       patImpfAct = patDefImpfAct armi armu ;
       patImp = patDefImp Irmi Irmu ;
       suffixImpf = case isDoubleDef of {True => suffixImpfDoubleDef ; _ => suffixImpfDef}
    } in
    { s = table {
        VPerf   v   pgn =>                    patPerf ! v ! pgn + suffixPerfDef v        ! pgn ;
        VImpf m Act pgn => prefixImpf ! pgn + patImpfAct  ! pgn + suffixImpf Act vowImpf ! m ! pgn ;
        VImpf m Pas pgn => prefixImpf ! pgn + urma              + suffixImpf Pas vowImpf ! m ! pgn ;
        VImp        g n =>                    patImp ! g ! n    + suffixImpf Act vowImpf ! Jus ! Per2 g n ;
        VPPart          => ppart
        }
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

  --ignores the vowel=u case, eg "دعو"
  patDefImpfAct :  (_,_ : Str) -> PerGenNum => Str = \rmi,rmu ->
  table {
    Per3 Masc Pl => rmu ;
    Per2 Masc Pl => rmu ;
    _            => rmi
    } ;


  patDefImp : (_,_ : Str) -> Gender => Number => Str = \rmi, rmu ->
    table {
      Masc => table {Pl => rmu ; _ => rmi} ;
      _	   => table {_ => rmi}
    } ;


  suffixPerfDef : Voice -> PerGenNum => Str = \v ->
    let {p3ms =
           case v of {
		     Act => "ى" ;
		     Pas => "يَ"
		   } ;
	     ya =
           case v of {
		     Act => "" ;
		     Pas => "يَ"
		   }
    } in
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
          Per3 g    Dl => "يَانِ" ;
          Per2 g    Dl => "يَانِ" ;
          Per3 Fem  Pl => "يْنَ" ;
          Per2 Fem  _  => "يْنَ" ;
          _            => default Ind
        } ;
      m =>
        table {
          Per3 Masc Pl => "وْا" ;
          Per2 Masc Pl => "وْا" ;
          Per3 g    Dl => "يَا" ;
          Per2 g    Dl => "يَا" ;
          Per3 Fem  Pl => "يْنَ" ;
          Per2 Fem  Pl => "يْنَ" ;
          Per2 Fem  Sg => "ي" ;
          _            => default m
        }
    } ;

  -- does this even happen other than with رءي? /IL
  suffixImpfDoubleDef : Voice -> Vowel -> Mood => PerGenNum => Str = \vc,vw ->
    \\m,p => rmSukun (suffixImpfDef vc vw ! m ! p) ;

--now is used for the sound, assimilated (weak C1), and when C1 = hamza:

v1sound : Root3 -> Vowel -> Vowel -> Verb =
  \fcl,vowPerf,vowImpf ->
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
  verb katab kutib aktub uktab euktub maktUb ;

v1hollow : Root3 -> Vowel -> Verb =
  \xwf,vowImpf ->
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

  } in verbHollow (toDefForms xAf xif xIf xuf axAf axaf uxAf uxaf xaf xAf' ppart) ;

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


v1geminate : Str -> Vowel -> Vowel -> Verb =
  \rootStr,vowPerf,vowImpf ->
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
   } in verbGeminate (toDefForms
                        madd madad mudd mudid amudd amdud
                        umadd umdad Umdud mudd' mamdUd) ;

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

-- IL -- Defective, hollow and geminate verbs all need 11 forms.
      {- NB. the numbers don't always refer to the same forms!
         The verb(Def|Hollow|Geminate) constructors pick the right forms. -}
DefForms : Type = Predef.Ints 10 => Str ;

toDefForms : (x1,_,_,_,_,_,_,_,_,_,x11 : Str) -> DefForms =
  \a,b,c,d,e,f,g,h,i,j,k ->
  table {
    0 => a ; 1 => b ; 2 => c ; 3 => d ; 4 => e ; 5 => f ; 6 => g ;
    7 => h ; 8 => i ; 9 => j ; 10 => k
  } ;

v1DefForms_perfA : Root3 -> Vowel -> DefForms = \rmy,vowImpf ->
 let {
   _rmi = mkDefective (patDef1 ! vowImpf) rmy ;
   _rmu = mkDefective (patDef2 ! vowImpf) rmy ;
   rama = mkDefective faca rmy ;
   ramay = mkStrong facalo rmy ;
   rumi = mkDefective fuci rmy ;
   rumu = mkDefective fucu rmy ;
   rumiy = mkStrong fucilo rmy ;
   armi = "َ" + _rmi ;
   armu = "َ" + _rmu ;
   urma = mkDefective ufca rmy ;
   eirmi = prefixImp ! vowImpf + _rmi;
   eirmu = prefixImp ! vowImpf + _rmu;
   marmiy = mkStrong mafcil rmy
 } in toDefForms rama ramay rumi rumu rumiy armi armu urma eirmi eirmu marmiy ;

v1defective_a : Root3 -> Vowel -> Verb = \rmy,vowImpf ->
  let vforms = v1DefForms_perfA rmy vowImpf
   in verbDef vforms vowImpf ;

v1defective_i : Root3 -> Vowel -> Verb = \bqy,vowImpf -> -- IL (conjugation 1d4)
  let vforms_a = v1DefForms_perfA bqy vowImpf ;
      baqI  = mkDefective facIl bqy ;
      baqiy = mkDefective facil bqy ;
      vforms_i = table { 0 => baqI ;
                         1 => baqiy ;
                         x => vforms_a ! x } ;
   in verbDef vforms_i vowImpf ;

v1doubleweak : Root3 -> Verb = \r'y ->
  let ry = r'y ** {c = ""} ;
      vforms_doubleweak : DefForms = \\x => rmSukun (v1DefForms_perfA ry a ! x) ; -- only remove the first sukun
      vforms_weak : DefForms = v1DefForms_perfA r'y a ;
      vforms = table { 0 => vforms_weak ! 0 ; -- all perfect forms
                       1 => vforms_weak ! 1 ;
                       2 => vforms_weak ! 2 ;
                       3 => vforms_weak ! 3 ;
                       4 => vforms_weak ! 4 ;
                       x => vforms_doubleweak ! x } ;
   in verbDoubleDef vforms a ; -- sukun in suffixes is removed in verbDoubleDef


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

v2sound : Root3 -> Verb = \qsm ->
  let {
    qassam = mkStrong faccal qsm ;
    qussim = mkStrong fuccil qsm ;
    qassim  = mkStrong faccil qsm ;
    uqassim = "ُ" + qassim ;
    uqassam = "ُ" + qassam ;
    muqassam = "مُ" + qassam
  } in
  verb qassam qussim uqassim uqassam qassim muqassam;

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
    mugannaY = "مُ" + ganna + "ى"
  } in verbDef (toDefForms ganna gannay gunni gunnu gunniy uganni ugannu uganna ganni gannu mugannaY) i;

v3sound : Root3 -> Verb =
  \tbc ->
  let {
    tAbac = mkStrong fAcal tbc ;
    twbic = mkStrong fUcil tbc ;
    tAbic = mkStrong fAcil tbc ;
    utAbic  = "ُ" + tAbic ;
    utAbac = mkStrong ufAcal tbc ;
    mutAbac = "م" + utAbac
  } in verb tAbac twbic utAbic utAbac tAbic mutAbac;

v4sound : Root3 -> Verb =
  \qnc ->
  let {
    eaqnac = mkStrong eafcal qnc;
    euqnic = mkStrong eufcil qnc;
    uqnic = mkStrong ufcil qnc;
    uqnac = mkStrong ufcal qnc;
    eaqnic = mkStrong eafcil qnc;
    muqnac = "م" + uqnac
  } in
  verb eaqnac euqnic uqnic uqnac eaqnic muqnac;

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

  } in verbHollow (toDefForms
                      earAd earad eurId eurid
                      urId urid urAd urad
                      earId earid ppart) ;

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
    mucTaY = "م" + ucTa +"ى"
  } in toDefForms eacTa eacTay eucTi eucTu eucTiy ucTi ucTu ucTa eacTi eacTu mucTaY ;

v4defective : Root3 -> Verb = \cTy ->
  verbDef (v4DefForms cTy) i ;

v4doubleweak : Root3 -> Verb = \r'y ->
  let ry = r'y ** {c = ""} ;
      vforms : DefForms = \\x => rmSukun (v4DefForms ry ! x) ; -- only remove the first sukun
   in verbDoubleDef vforms i ; -- sukun in suffixes is removed in verbDoubleDef

v5sound : Root3 -> Verb =
  \nfs ->
  let {
    tanaffas = mkStrong tafaccal nfs ;
    tunuffis = mkStrong tufuccil nfs ;
    atanaffas  = "َ" + tanaffas ;
    utanaffas = "ُ" + tanaffas ;
    mutanaffas = "م" + tanaffas
  } in verb tanaffas tunuffis atanaffas utanaffas tanaffas mutanaffas;

v6sound : Root3 -> Verb =
  \fqm ->
  let {
    tafAqam = mkStrong tafAcal fqm ;
    tufUqim = mkStrong tufUcil fqm ;
    atafAqam = "َ" + tafAqam ;
    utafAqam = "ُ" + tafAqam ;
    mutafAqam = "م" + utafAqam
  } in verb tafAqam tufUqim atafAqam utafAqam tafAqam mutafAqam;

-- v7sound : Root3 -> Verb = -- TODO 7s
--   \fcl ->
--   let {
--     _facal = mkStrong facal fcl ;
--     _facil = mkStrong facil fcl;
--     infacal = "اِنْ" + _facal ; -- VPerf Act
--      ; -- VPerf Pas
--     anfacil = "َنْ" + _facil ; -- VImpf _ Act
--      ; -- VImpf _ Pas
--      ; -- VImp
--       -- VPPart
--   } in
--   verb  ;

v7geminate : Root3 -> Verb = -- IL 7g -- very likely wrong
  \fcl ->
  let {
    _nfacc = "نْ" + mkHollow facc fcl ;
    infacal = "اِنْ" + mkStrong facal fcl ; -- VPerf Act -- TODO use another constructor, this is wrong for 3rd person
    unfucc = "اُنْ" + mkHollow fucc fcl ; -- VPerf Pas
    anfacc = "َ"  + _nfacc ; -- VImpf _ Act
    unfacc = "ُ"  + _nfacc ;  -- VImpf _ Pas
    infacc = "اِ" + _nfacc ; -- VImp
    munfacc = "مُ" +_nfacc  -- VPPart
  } in
  verb infacal unfucc anfacc unfacc infacc munfacc ;

v8sound : Root3 -> Verb =
  \rbT ->
  let {
    rtabiT = mkStrong ftacil rbT ;
    rtabaT = mkStrong ftacal rbT ;
    eirtabaT = "إِ" + rtabaT ;
    eurtubiT = mkStrong euftucil rbT ;
    artabiT = "َ" + rtabiT ;
    urtabaT  = "ُ" + rtabaT ;
    eirtabiT = "إِ" + rtabiT ;
    murtabaT =  "م" + urtabaT
  } in verb eirtabaT eurtubiT artabiT urtabaT eirtabiT murtabaT;

v8assimilated : Root3 -> Verb = --- IL 8a1
  \wfq ->
  let {
    ttafiq = mkWeak ttacil wfq ;
    ttafaq = mkWeak ttacal wfq ;
    eittafaq = "إِ" + ttafaq ;
    euttufiq = mkWeak euttucil wfq ;
    attafiq = "َ" + ttafiq ;
    uttafaq  = "ُ" + ttafaq ;
    eittafiq = "إِ" + ttafiq ;
    muttafaq =  "م" + uttafaq
  } in verb eittafaq euttufiq attafiq uttafaq eittafiq muttafaq;

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
    ppart = "مُ" + _HtAj  -- PPart

  }  in verbHollow (toDefForms
                     iHtAj iHtaj uHtIj uHtij aHtAj aHtaj
                     uHtAj uHtaj iHtAj iHtaj ppart) ;
v10sound : Root3 -> Verb = -- IL 10s -- to be checked
  \qtl ->
  let {
    _staqtal = "ستَ" + mkStrong fcal qtl ;
    _staqtil = "ستَ" + mkStrong fcil qtl;
    istaqtal = "اِ" + _staqtal ; -- VPerf Act
    ustuqtil = "اُسْتُ" + mkStrong fcil qtl; -- VPerf Pas
    astaqtil = "َ"  + _staqtil ; -- VImpf _ Act
    astaqtal = "َ"  + _staqtal ;  -- VImpf _ Pas
    istaqtil = "اِ" + _staqtil ; -- VImp
    mustaqtal = "مُ" + _staqtal  -- VPPart
  } in
  verb istaqtal ustuqtil astaqtil astaqtal istaqtil mustaqtal ;

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
    ppart = "مُ" + _staxIf -- PPart ("weird anomalies" here too?)

  } in verbHollow (toDefForms
                     istaxAf istaxaf ustuxIf ustuxif astaxIf astaxif
                     ustaxAf ustaxaf istaxif istaxIf ppart) ;

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

  } in verbDef (toDefForms
                  istalqa istalqay ustulqi ustulqi ustulqi
                  astalqi astalqu ustalqa istalqi istalqu mustalqin) i ;

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
      _		=> axAf
    } ;
    Pas =>
      table {
      Per3 Fem Pl => uxaf ;
      Per2 Fem Pl => uxaf ;
      _		=> uxAf
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

--Nominal Morphology

  caseTbl : Case => Str =
    table {
      Bare => [] ;
      Nom  => "ُ";
      Acc  => "َ";
      _Gen  => "ِ" -- dat is the same as gen, except in definite before لِ
    };

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
        Pl => sing Humr ! d ! c
        };
      APosit Fem n d c => case n of {
        Sg => indeclN HamrA'  ! d ! c;
        Dl => dual ((tk 2 HamrA') + "و") ! d ! c;
        Pl => sing Humr ! d ! c
        };
      AComp d c => indeclN aHmar ! d ! c
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
        Pl => sing kutub
      };

    --takes the sound noun in singular and gives the
    --complete noun inflection table of sound feminine plural
    sndf : Str -> NTable  =
      \lawHa ->
      table {
        Sg => sing lawHa ;
        Dl => dual lawHa ;
        Pl => plurF lawHa
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
    sing : Str -> State => Case => Str = \word ->
      \\s,c => defArt s c (case word of {
        lemma + "ِيّ" => fixShd word  (decNisba ! s ! c) ;
        lemma + "ِي"  => fixShd lemma (dec2sg ! s ! c) ;
        _ + ("ا"|"ى") => fixShd word  (dec3sg ! s ! c) ;
        lemma + ("ء"|"أ"|"ئ"|"ؤ") => word + dec1sgNoDoubleAlif ! s ! c ;
        lemma + "ة"   => case s of {
                            Poss => lemma + "ت" + dec1sg ! s ! c ;
                            _    => word        + dec1sgNoDoubleAlif ! s ! c
                          } ;
         _             => fixShd word  (dec1sg ! s ! c)
      }) ;


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
    indecl :  Case => Str =
      table {
        (Gen|Dat) => "َ" ;
        x         => caseTbl ! x
      };


    --declension 2 (ends in yaa')
    dec2sg : State => Case => Str = \\s,c =>
      case <s,c> of {
        <_,   Bare> => [] ;
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

    --declension 2 (ends in yaa')
    decNisba : State => Case => Str = \\s,c =>
      case <s,c> of {
        <_,   Bare> => [] ;
        <Indef,Acc> => "اً" ;
        <Indef>     => "ٍ" ;
        <_,    Acc> => "َ" ;
        _           => []
      };

    --dual suffixes
    dl : State => Case => Str =
      table {
        (Const|Poss) =>
          table {
            Nom => "َا";
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


    mkAt : Str -> Str = \bayDo ->
      case bayDo of {
        bayD + "ة"  => bayD + "ات";
        bayD + "اء" => bayD + "اوات";
        bayD + "ى"  => bayD + "يَات";
        _           => bayDo + "ات"
      };


  oper

    sizeToNumber : Size -> Number = \s ->
      case s of {
        ThreeTen | None  => Pl;
        Two => Dl;
        _ => Sg
      } ;

    numberToSize : Number -> Size = \n ->
      case n of {
        Pl => ThreeTen;
        Dl => Two ;
        Sg => One -- or Hundreds or NonTeen
      } ;

    detGender : Gender -> Size -> Gender =
      \g,s ->
      case s of {
        ThreeTen | Teen => genPolarity ! g;
        _ => g
      };

    nounState : State -> Number -> State =
      \s,n ->
      case <s,n> of {
        <Const,Pl> => Def;   --kullu l-kutubi, bacDu l-kutubi
        <Const,Sg> => Indef; --kullu kitaabin
        <Indef> => Indef;    --kitaabun
        <Poss>  => Poss;
        _       => Def       --Lkitaabu
      };

    --FIXME needs testing
    nounCase : Case -> Size -> State -> Case =
      \c,size,s ->
      case <size,s> of {
        <Teen,_> => Acc;
        <NonTeen,_> => Acc;
        <ThreeTen,_> => Gen;
        <Hundreds,_> => Gen;
        <_,Const> => Gen; -- not sure if this is an actual rule /IL
        _     => c
      };

    definite : State => State =
      table {
        Indef => Indef;
        _     => Def
      };

    --things like mi{aö and vlAva and >alf should be treated as Const
    --before the counted noun, so (vlAvaöN kutubK) is wrong
    toDef : State -> Size -> State =
      \s,n ->
      case <s,n> of {
        <Indef,Hundreds> => Const;
        <Indef,ThreeTen> => Const;
        _ => s
      };

    -- in a NP, sometimes the common noun preceedes the determiner
    -- e.g. some determiners act as adjectives modifying the noun they count
    -- 'the three children, two children'
    -- e.g. possesive pronouns: his book ('kitaabuhu'
    cnB4det : Bool -> Bool -> Size -> State -> Bool = \isPron,isNum,s,d ->
      case <isPron,isNum,s,d> of {
        <True,_,_,_> => True;
        <_,False,_,_> => False; --non-numerals
        <_,True,_,Def> => True; --definite numbers act as adjectives
        <_,True,Two,_> => True; --numerals one and two always adjectives
        <_,True,One,_> => True; --numerals one and two always adjectives
        _ => False
      };

    agrP3 : Species -> Gender -> Number -> PerGenNum= \h,g,n ->
      case <h,n> of {
        <NoHum,Pl> => Per3 Fem Sg;
        _          => Per3 g n
      };

    pgn2gn : PerGenNum -> {g : Gender; n : Number} = \pgn ->
      case pgn of {
        Per3 gn nm => {g = gn; n = nm};
        Per2 gn nm => {g = gn; n = nm};
        Per1 nm    => {g = Masc;  --randomly
                       n = case nm of {
                              Sing => Sg ;
                              Plur => Pl}
                      }
      };

    gn2pgn : {g : Gender; n : Number} -> PerGenNum = \gn ->
      case gn of { {g = gn; n = nm} => Per3 gn nm } ;

    -- these are chosen in many places, trying to be consistent
    toOrder : QForm -> Order = \qf ->
      case qf of { QIndir => Nominal ;
                   QDir   => Verbal } ;


    mkOrd : (_,_ : Str) -> Size -> NumOrdCard =
      \aysar,yusra,sz ->
      { s = \\g,s,c =>
          case g of {
            Masc => (sing aysar) ! s ! c;
            Fem  => (sing yusra) ! s ! c
          };
        n = sz ;
        isNum = False
      };



-----------------------------------------------------------------------------
-- Det, Quant

    BaseQuant : Type = {
      d : State;
      is1sg : Bool; -- To force no case marker for 1st person poss. suff.
      isNum : Bool;
      -- for genitive pronouns (suffixes). if true, then "cn ++ det"
      --should be used instead of "det ++ cn" when constructing the NP
      isPron: Bool} ;

    baseQuant = { d = Indef ;
                  is1sg,isNum,isPron = False } ;

    Quant : Type = BaseQuant ** {
      s : ResAra.Number => Species => Gender => Case => Str
      } ;

    Det : Type = BaseQuant ** {
      s : Species => Gender => Case => Str ;
      n : Size
      } ;

    Predet : Type = {
      s : Case => Str;
      isDecl : Bool
      };

    Agr = { pgn : PerGenNum; isPron : Bool} ;
    AAgr = { g : Gender ; n : Number} ;


-----------------------------------------------------------------------------
-- NP, Pron

    NP : Type = {
      s : Case => Str ;
      a : Agr ;
      empty : Str -- to prevent ambiguities with prodrop
      } ;

    mkPron : (_,_,_ : Str) -> PerGenNum -> NP = \ana,nI,I,pgn ->
     { s =
        table {
          (Nom|Bare) => ana;
          Acc => nI ; -- object suffix
          Gen => I ;  -- possessive suffix
          Dat => I -- will only be used with preposition لِ
        };
      a = {pgn = pgn; isPron = True };
      empty = []
    };

    proDrop : NP -> NP = \np ->
      case np.a.isPron of {
        True => np ** {s = table {Nom => [] ; x => np.s ! x}};
        _    => np
      } ;

    emptyNP : NP = {
      s = \\_ => [] ;
      a = {pgn = Per3 Masc Sg ; isPron = False} ;
      empty = [] } ;

    agrNP : Agr -> NP = \agr -> emptyNP ** {a = agr} ;

    i_Pron  : NP = mkPron "أَنَا" "نِي" "ي" (Per1 Sing) ;
    we_Pron : NP = mkPron "نَحنُ" "نا" "نا" (Per1 Plur) ;

    youSgMasc_Pron : NP = mkPron "أَنتَ"    "كَ"    "كَ"    (Per2 Masc Sg) ;
    youSgFem_Pron  : NP = mkPron "أَنتِ"    "كِ"    "كِ"    (Per2 Fem  Sg) ;
    youDlMasc_Pron : NP = mkPron "أَنتُمَا" "كُمَا" "كُمَا" (Per2 Masc Dl) ;
    youDlFem_Pron  : NP = mkPron "أَنتُمَا" "كُمَا" "كُمَا" (Per2 Fem  Dl) ;
    youPlMasc_Pron : NP = mkPron "أَنتُمْ"  "كُمْ"  "كُمْ"  (Per2 Masc Pl) ;
    youPlFem_Pron  : NP = mkPron "أَنتُنَّ" "كُنَّ" "كُنَّ" (Per2 Fem  Pl) ;

    he_Pron         : NP = mkPron "هُوَ"  "هُ"    "هُ"    (Per3 Masc Sg) ;
    she_Pron        : NP = mkPron "هِيَ"  "ها"    "ها"    (Per3 Fem  Sg) ;
    theyDlMasc_Pron : NP = mkPron "هُمَا" "هُمَا" "هُمَا" (Per3 Masc Dl) ;
    theyDlFem_Pron  : NP = mkPron "هُمَا" "هُمَا" "هُمَا" (Per3 Fem  Dl) ;
    theyMasc_Pron   : NP = mkPron "هُمْ"  "هُمْ"  "هُمْ"  (Per3 Masc Pl) ;
    theyFem_Pron    : NP = mkPron "هُنَّ" "هُنَّ" "هُنَّ" (Per3 Fem  Pl) ;


    -- Used e.g. to encode the subject as an object clitic
    -- or to find a possessive suffix corresponding to the NP.
    -- If the NP is a pronoun, just use itself.
    np2pron : NP -> NP = \np -> case np.a.isPron of {
      True  => np ;
      False => pgn2pron np.a.pgn
      } ;

    pgn2pron : PerGenNum -> NP = \pgn ->
      case pgn of {
        Per1 Sing => i_Pron ;
        Per1 Plur => we_Pron ;
        Per2 Fem  Sg => youSgFem_Pron ;
        Per2 Masc Sg => youSgMasc_Pron ;
        Per2 Fem  Dl => youDlFem_Pron ;
        Per2 Masc Dl => youDlMasc_Pron ;
        Per2 Fem  Pl => youPlFem_Pron ;
        Per2 Masc Pl => youPlMasc_Pron ;
        Per3 Fem  Sg => she_Pron ;
        Per3 Masc Sg => he_Pron ;
        Per3 Fem  Dl => theyDlFem_Pron ;
        Per3 Masc Dl => theyDlMasc_Pron ;
        Per3 Fem  Pl => theyFem_Pron ;
        Per3 Masc Pl => theyMasc_Pron
      } ;

    pron2np : NP -> NP = \np -> np ** {
      a = np.a ** {isPron=False} -- hack, sometimes we *don't* want prodrop
    } ;

    reflPron : Case -> PerGenNum -> Str = \c,pgn ->
      let pron : NP = pgn2pron pgn
       in "نَفْس" + caseTbl ! c ++ pron.s ! Gen ;

    reflV : Verb -> Verb = \v -> v ** {
      s = \\vf => case vf of {
        VPerf _ pgn   => v.s ! vf ++ reflPron Acc pgn ;
        VImpf _ _ pgn => v.s ! vf ++ reflPron Acc pgn ;
        VImp g n      => v.s ! vf ++ reflPron Acc (Per2 g n) ;
        VPPart        => v.s ! vf ++ reflPron Acc (Per3 Masc Sg) ----
        }
      } ;
-----------------------------------------------------------------------------
-- IP, questions

    IP : Type = {
      s : Bool -- different forms for "what is this" and "what do you do"
       => Gender -- because an IP can be made into an IComp
       => State => Case -- because of PrepIP: e.g. "in which" chooses definite accusative
       => Str ;
      a : Agr -- can be both subject and object of a QCl, needs full agr. info (stupid given that s depends on gender but meh)
      } ;

    mkIP = overload {
       mkIP : Str -> Number -> IP = \maa,n -> {
          s = \\_p,_g,_s,_c => maa ;
          a = { pgn = agrP3 NoHum Masc n ; isPron = False }
          } ;
      mkIP : (_,_ : Str) -> Number -> IP = \maa,maadhaa,n -> {
          s = table { True  => \\_g,_s,_c => maa ;
                      False => \\_g,_s,_c => maadhaa } ;
          a = { pgn = agrP3 NoHum Masc n ; isPron = False }
          }
      } ;

    ip2np : IP -> Bool -> NP = \ip,isPred -> ip ** { s = ip.s ! isPred ! Masc ! Def ; empty = [] } ;
    np2ip : NP -> IP = \np -> np ** {s = \\_,_,_ => np.s} ;

    IDet : Type = {
      s : Gender -- IdetCN needs to choose the gender of the CN
        => State -- Needs to be retained variable for IP; PrepIP chooses the state of IP
        => Case => Str ;
      n : Number ;
      d : State -- in IdetCN, chooses the state of the CN
      } ;

    IQuant : Type = {
      s : State => Case => Str
      } ;

    IComp : Type = {
      s : AAgr     -- "how old": masc or fem for adjective
                   -- no need for Case, IComp is only used by QuestIComp, as grammatical subject
       => Str ;
      } ;

-----------------------------------------------------------------------------
-- VP

  param VPForm =
        VPPerf
      | VPImpf Mood
      | VPImp ;

  oper

    BaseVP : Type = { -- to minimise duplication of code for VPS
      sc : Preposition ; -- subject case: e.g.  يُمْكِنُ *لِ*Xِ
      obj : Obj;
      pred : Comp;
      isPred : Bool; --indicates if there is a predicate (xabar)
      s2 : Str
      } ;

    VP : Type = BaseVP ** {
      s : PerGenNum => VPForm => Str ;
      } ;

    uttVP : VP -> (Gender=>Str) = \vp ->
     \\g => vp.s ! Per3 g Sg ! VPPerf
         ++ vp.obj.s ++ vp.pred.s ! {n = Sg ; g = g} ! Nom
         ++ vp.s2 ;

    predV : Verb -> VP = \v ->
      { s = \\pgn,vf =>
          let gn = pgn2gn pgn in
          case vf of {
            VPPerf => v.s ! (VPerf Act pgn);
            VPImpf m => v.s ! (VImpf m Act pgn);
            VPImp => v.s ! (VImp gn.g gn.n)
          };
        sc = noPrep ;
        obj = emptyObj ;
        s2 = [];
        pred = {s = \\_,_ => []} ;
        isPred = False
      };

    passPredV : Verb -> VP = \v ->
      let actVP = predV v in actVP ** {
        s = \\pgn,vf =>
          case vf of {
            VPPerf   => v.s ! (VPerf   Pas pgn) ;
            VPImpf m => v.s ! (VImpf m Pas pgn) ;
            _        => actVP.s ! pgn ! vf
        }
      };

    predVP : NP -> VP -> Cl = \np,vp ->
      { s =\\t,p,o =>
          let {
            pgn =
              case <o,np.a.isPron> of {
                <Verbal, False> => verbalAgr np.a.pgn;
                _               => np.a.pgn
              };
            sc : Preposition = case o of { -- very unsure of this /IL
                  Subord => {s=[]; c=Acc} ; -- to prevent weird stuff with VVs
                  _ => case np.a.isPron of {True => noPrep; _ => vp.sc} 
                } ;
            subj = np.empty ++ sc.s
                ++ case vp.isPred of {
                      False => (proDrop np).s ! sc.c ; -- prodrop if it's not predicative
                      True  =>           np.s ! sc.c
                   } ;
          } in wordOrder o
                  vp.obj.a.isPron np.a.isPron
                  (vStr vp pgn t p)
                  vp.obj.s
                  (pred vp pgn t p) 
                  vp.s2
                  subj
      } ;

    -- seems complicated, but this is to share code with VPS and other similar structures
    wordOrder : Order -> (objIsPron,subjIsPron : Bool) -> (verb,obj,pred,adv,subj : Str) -> Str =
      \o,objIsPron,subjIsPron,verb,obj,pred,adv,subj ->
          let cl = wordOrderNoSubj o objIsPron verb obj pred adv in
          case o of {
            Subord => 
              let bind = if_then_Str subjIsPron BIND [] -- in subord. clause, subj. pronoun binds to the main verb
               in cl.before ++ bind ++ subj ++ cl.after ;
            _  => cl.before         ++ subj ++ cl.after
          } ;

    wordOrderNoSubj : Order -> (objIsPron : Bool) -> (verb,obj,pred,adv : Str) -> {before,after : Str} =
      \o,objIsPron,verb,obj,pred,adv ->
        case o of {
            VOS => {before = verb ++ obj ++ pred ++ adv; after = []} ;
            Verbal => case objIsPron of {
                        True  => {before = verb ++ obj ; after = adv ++ pred} ; -- obj. clitic attaches directly to the verb
                        False => {before = verb ; after = obj ++ adv ++ pred} 
                      } ;
            (Nominal|Subord) => {before = [] ; after = verb ++ obj ++ adv ++ pred}
          } ;

    pred : VP -> PerGenNum -> ParamX.Tense -> Polarity -> Str = \vp,pgn,tn,pl -> 
      let gn = pgn2gn pgn
       in case <vp.isPred,tn,pl> of {
            <True, Pres, Pos> => vp.pred.s ! gn ! Nom; --xabar marfooc
            _                 => vp.pred.s ! gn ! Acc --xabar kaana wa laysa manSoob
          } ;

    vStr : VP -> PerGenNum -> ParamX.Tense -> Polarity -> Str = \vp,pgn,tn,pl -> 
      let kataba  = vp.s ! pgn ! VPPerf ;
          yaktubu = vp.s ! pgn ! VPImpf Ind ;
          yaktuba = vp.s ! pgn ! VPImpf Cnj ;
          yaktub  = vp.s ! pgn ! VPImpf Jus ;
       in case <vp.isPred,tn,pl> of {
            <False, Pres, Pos> => yaktubu ;
            <False, Pres, Neg> => "لَا" ++ yaktubu ;
            <True, Pres, Pos> => "" ;      --no verb "to be" in present
            <True, Pres, Neg> => "لَيسَ" ;--same here, just add negation particle
            <_, Past, Pos> => kataba ;
            <_, Past, Neg> => "لَمْ" ++ yaktub ;
            <_, Cond, _  > => yaktuba ;
            <_, Fut,  Pos> => glue "سَ" yaktubu ;
            <_, Fut,  Neg> => "لَنْ" ++ yaktuba
          } ;

    -- in verbal sentences, the verb agrees with the subject
    -- in Gender but not in number
    verbalAgr : PerGenNum -> PerGenNum = \pgn ->
      case pgn of {
        Per3 g _ => Per3 g Sg;
        _        => pgn
      };

-----------------------------------------------------------------------------
-- Comp, arguments for VP

    Comp : Type = {
      s : AAgr => Case => Str ;
      } ;

    Obj : Type = {
      s : Str ;
      a : Agr -- default Agr in a VP without real Obj is Per3 Masc Sg.
      };      -- need isPron for word order in predVP, and pgn for ImpersCl

    Subj : Type = {s : Case => Str ; isPron : Bool} ;

    np2subj : NP -> Subj = \np -> np ** {isPron = np.a.isPron} ;
    subj2np : Subj -> NP = \su -> su ** {a = {pgn = emptyNP.a.pgn ; isPron = su.isPron} ; empty=[]} ;
    emptyObj : Obj = emptyNP ** {s=[]} ;

    insertObj : NP -> VPSlash -> VP = \np,vp -> vp ** { 
      obj = {s = vp.obj.s -- old object, if there was one
              ++ bindIfPron np vp -- new object, bind if pronoun and not pred
              ++ vp.agrObj ! np.a.pgn ; -- only used for SlashV2V
             a = np.a} 
      } ;

    bindIfPron : NP -> {c2:Preposition; isPred:Bool} -> Str = \np,vp ->
      let bind = case <vp.isPred,np.a.isPron> of {
                 <False,True> => BIND ;
                 _            => [] } 
       in vp.c2.s ++ bind ++ np.s ! vp.c2.c ;

    insertPred : Comp -> VP -> VP = \p,vp -> vp **
      { pred = p;
        isPred = True
      };

    insertStr : Str -> VP -> VP = \str,vp -> vp **
      { s2 = vp.s2 ++ str };

    kaan : {s : AAgr => Case => Str} -> VP = \xabar ->
      insertPred xabar (predV copula);

    copula : Verb = v1hollow {f = "ك"; c = "و" ; l = "ن"} u ;

-----------------------------------------------------------------------------
-- Slash categories

    VPSlash : Type = VP ** {c2 : Preposition ; agrObj : PerGenNum => Str} ;
    ClSlash : Type = VPSlash ** {subj : Subj} ;

    emptyVPslash : VP -> VPSlash = \vp -> vp ** {
      c2 = noPrep ; agrObj = \\_ => []
      } ;

    slashV2 : Verb2 -> VPSlash = \v ->
      predV v ** {c2 = v.c2 ; agrObj = \\_ => []} ;

    -- Add subject string, fix agreement to the subject,
    -- but keep the structure as VP, because later on
    -- we might need different word orders for the ClSlash.
    predVPSlash : NP -> VPSlash -> ClSlash = \np,v -> v ** {
      subj = np2subj np ;
      s = \\_pgn,vf => v.s ! np.a.pgn ! vf -- so we can throw away subject's pgn
      } ;

    complClSlash = overload {
      complClSlash : NP -> ClSlash -> Cl = \obj,cls ->
        predVP (subj2np cls.subj) (insertObj obj cls) ;
      complClSlash :       ClSlash -> Cl = \cls ->
        predVP (subj2np cls.subj) (insertObj emptyNP cls) -- Empty subject and object
      } ;

    Cl  : Type = {s : Tense => Polarity => Order => Str} ;
    QCl : Type = {s : Tense => Polarity => QForm => Str} ;

    forceOrder : Order -> Cl -> Cl = \o,cl ->
      {s = \\t,p,_ => cl.s ! t ! p ! o} ;

-----------------------------------------------------------------------------
-- Relative

  param
    RAgr = RSg Gender | RPl Gender | RDl Gender Case ;

  oper
    agr2ragr = overload {
      agr2ragr : Agr -> Case -> RAgr = \a,c ->
        let gn = pgn2gn a.pgn in case <gn.n,gn.g,a> of {
          <Sg,x> => RSg x ;
          <Dl,x> => RDl x c ;
          <Pl,x> => RPl x } ;
      agr2ragr : Number -> Case -> Gender -> RAgr = \n,c,g ->
        case n of {
          Sg => RSg g ;
          Dl => RDl g c ;
          Pl => RPl g }
      } ;

    RCl : Type = {s : Tense => Polarity => Agr => Case => Str} ;
    RP  : Type = {s : RAgr => Str } ;

-----------------------------------------------------------------------------
-- Num

  param

    Size = One | Two | ThreeTen | Teen | NonTeen | Hundreds | None ;
    DForm = unit | ten ;
    CardOrd = NCard | NOrd ;

  oper
    --digits 1, 3 - 10: take the lemmas of the card ords & in masculine
    --form and calculates the whole table

    regNum : Str -> Str ->
      {s : DForm => CardOrd => Gender => State => Case => Str} =
      \xams,xAmis ->
      let { xamsa = xams + "َة";
            xAmisa = xAmis +  "َة"} in
      mkNum xamsa xAmis xAmisa;

    mkNum : Str -> Str -> Str ->
      {s : DForm => CardOrd => Gender => State => Case => Str} =
      \wAhid,awwal,Ula ->
      let wAhida : Str = case wAhid of {
            x + "ة" => mkAt wAhid ;
            _       => wAhid + "َة" }
      in
      { s= table {
          unit => table {
            NCard => table {
              Masc => \\s,c => (sing wAhid) ! s ! c ;
              --all fem are first declension:
              Fem => \\s,c => defArt s c wAhida + dec1sgNoDoubleAlif ! s ! c
              };
            NOrd => table {
              Masc => \\s,c => defArt s c awwal + dec1sg ! s ! c;
              Fem => \\s,c => (sing Ula) ! s ! c
              }
            };
          ten => table {
            NCard => \\_,s,c => defArt s c wAhid + m_pl ! Indef ! c;
            NOrd => \\_,s,c => defArt s c awwal + m_pl ! Indef ! c
            }
          }
      };

    num3_10 : Str -> Str -> { s : DForm => CardOrd => Gender
                                => State => Case => Str ; n : Size } =
      \xams,xAmis ->
      regNum xams xAmis ** { n = ThreeTen };

    num2 : { s : DForm => CardOrd => Gender => State => Case => Str} =
      { s = table {
          unit => table {
              NCard => table {
                Masc => \\s,c => Al ! s + "ٱِثن" + dl ! s ! c ;
                Fem => \\s,c => Al ! s + "ٱِثنَت" + dl ! s ! c
                };
              NOrd => table {
                Masc => \\s,c => Al ! s + "ثان" + dec2sg ! s ! c ;
                Fem => \\s,c => Al ! s + "ثانِيَة" + dec1sg ! s ! c
                }
            };
          ten => \\_,_,s,c => Al ! s + "عِشر" + m_pl ! Indef ! c
          }
      };

    num100 : State => Case => Str =
      \\s,c => Al ! s + "مِٱَة" + dec1sg ! s ! c;

    num200 : State => Case => Str =
      \\s,c => Al ! s + "مِٱَة" + dl ! s ! c ;

    num1000 : State => Case => Str =
      \\s,c => Al ! s + "أَلف" + dec1sg ! s ! c;

    num2000 : State => Case => Str =
      \\s,c => Al ! s + "أَلف" + dl ! s ! c ;

    teen : Gender => Str =
      table {
        Masc => "عَشَرَ";
        Fem  => "عَشرَةَ"
      };

    genPolarity : Gender => Gender =
      table {
        Masc => Fem;
        Fem => Masc
      };
}
