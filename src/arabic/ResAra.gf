    --# -path=.:../abstract:../common:../../prelude
--
----1 Arabic auxiliary operations.
--
---- This module contains operations that are needed to make the
---- resource syntax work. To define everything that is needed to
---- implement $Test$, it moreover contains regular lexical
---- patterns needed for $Lex$.
--
resource ResAra = MorphoAra ** open Prelude, Predef, OrthoAra, ParamX  in {

  flags optimize=noexpand ; coding=utf8 ;

oper

    NTable = Number => State => Case => Str;
    emptyNTable : NTable = \\n,s,c => [] ;

    Preposition : Type = {s : Str ; c : Case ; binds : Bool} ;

    Noun : Type = {
      s,s2 : NTable ;
      g : Gender ;
      h : Species ;
      isDual : Bool -- whether it takes dual instead of plural: eyes, twins, ...
      } ;
    Noun2 : Type = Noun ** {c2 : Preposition} ;
    Noun3 : Type = Noun2 ** {c3 : Preposition} ;

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

    pronSuff : pattern Str = #("كَ"|"كِ"|"كُمَا"|"كُمْ"|"كُنَّ"|"هُ"|"ها"|"هُمَا"|"هُمْ"|"هُنَّ") ;
    pronSuffAndOther : pattern Str = #( "كَم" ) ; -- TODO list words that begin like pron.suff. but aren't

    Adj  : Type = {s : AForm => Str} ;
    Adj2 : Type = Adj ** {c2 : Preposition} ;


    -- Sometimes a verb is only used in one form (per3 masc sg);
    -- ideally, one would use an impersonal syntactic construction,
    -- less ideally, hardcode the verb to only contain forms of one person.
    forcePerson : PerGenNum -> Verb -> Verb = \pgn,verb -> verb ** {
      s = \\vf => case vf of {
                   VPerf   v _ => verb.s ! VPerf v pgn ;
                   VImpf m v _ => verb.s ! VImpf m v pgn ;
                   _           => verb.s ! vf }
      } ;

    AP : Type = {s : Species => Gender => NTable } ;
    uttAP : AP -> (Gender => Str) ;
    uttAP ap = \\g => ap.s ! NoHum ! g ! Sg ! Const ! Bare ; ----IL

    CN : Type = Noun ** {np : Case => Str ; isHeavy : Bool};

    -- All fields of NP
    cn2str : CN -> Number -> State -> Case -> Str = \cn,n,s,c ->
      cn.s  ! n ! s ! c ++
      cn.s2 ! n ! s ! c ++
      cn.np ! c ;

    useN : Noun -> CN = \n -> n ** {
      np = \\_ => [] ;
      isHeavy = False } ;

    uttCN : CN -> (Gender => Str) ;
    uttCN cn = \\_ => cn2str cn Sg Indef Bare ;

    NumOrdCard : Type = {
      s : Gender => State => Case => Str ;
      n : Size ;
      isNum : Bool
      } ;

    uttNum : NumOrdCard -> (Gender => Str) ;
    uttNum n = \\g => n.s ! Fem ! Const ! Bare ;


param
    AForm =
      APosit Gender Number State Case
      | AComp State Case ;



oper
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
        Pl => brkPl Humr ! d ! c
        };
      APosit Fem n d c => case n of {
        Sg => indeclN HamrA'  ! d ! c;
        Dl => dual ((tk 2 HamrA') + "و") ! d ! c;
        Pl => brkPl Humr ! d ! c
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
    indecl :  Case => Str =
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
    cnB4det : Det -> Bool = \det ->
      case <det.isEmpty,det.isPron,det.isNum,det.n,det.d> of {
        <True,_,_,_,_> => True; -- hack to make liPrep work
        <_,True,_,_,_> => True;
        <_,_,False,_,_> => False; --non-numerals
        <_,_,True,_,Def> => True; --definite numbers act as adjectives
        <_,_,True,Two,_> => True; --numerals one and two always adjectives
        <_,_,True,One,_> => True; --numerals one and two always adjectives
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
      isPron: Bool;
      isEmpty: Bool} ; -- to know if liPrep should attach to the noun

    baseQuant = { d = Indef ;
                  is1sg,isNum,isPron,isEmpty = False } ;

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

    Agr = {pgn : PerGenNum; isPron : Bool} ;
    AgrLite = {gn : AAgr ; isPron : Bool} ; --used in ImpersCl
    AAgr = {g : Gender ; n : Number} ;

    agrLite : Agr -> AgrLite = \a -> a ** {gn = pgn2gn a.pgn} ;
    is1sg : Agr -> Bool = \a ->
      case a.pgn of {Per1 Sing => True; _ => False} ;

    mkDet = overload {
      mkDet : Str -> Number -> State -> Det
        = mkDetDecl True ;
      mkDet : (m,f : Str) -> Number -> State -> Det
        = \m,f,n,d ->
        let detM = mkDetDecl True m n d ;
            detF = mkDetDecl True f n d ;
         in detM ** {
              s = \\h,g,c => case g of {
                        Fem  => detF.s ! h ! g ! c ;
                        Masc => detM.s ! h ! g ! c }
              }
    } ;

    mkDetDecl : Bool -> Str -> Number -> State -> Det
      = \decl,word,num,state -> baseQuant **
      { s = \\_,_,c => word + if_then_Str decl (caseTbl ! c) [] ;
        n = numberToSize num;
        d = state;  --only Const is used now. check StructuralAra
      } ;

    mkPredet : Str -> Bool -> Predet
      = \word,decl ->
      { s = \\c =>
          case decl of {
            True => word + caseTbl!c;
            False => word
          };
        isDecl = decl
      };

   mkQuantNum : Str -> Number -> State -> {
      s: Species => Gender => Case => Str; n: Number; d : State; isPron: Bool; isNum : Bool} =
      \waHid,num,state ->
     let waHida = waHid + "َة" in
      { s = \\_,g,c =>
          let word =
          case g of {
            Masc => waHid;
            Fem => waHida
          } in defArt state c waHid + word + dec1sg ! state ! c;
        n = num;
        d = state;
        isPron = False;
        isNum = True
      };
-----------------------------------------------------------------------------
-- NP, Pron

    NP : Type = {
      s : Case => Str ;
      a : Agr ;
      isHeavy : Bool ; -- overrides verbal word order, if the subject is very complicated; e.g. built out of RelNP or similar
      empty : Str -- to prevent ambiguities with prodrop
      } ;

    -- hack, but better to have it here than to define ad hoc in every application grammar /IL
    forceCase : NP -> Case -> NP = \np,c -> np ** {
      s = \\_ => np.s ! c
      } ;

    mkPron : (_,_,_ : Str) -> PerGenNum -> NP = \ana,nI,I,pgn ->
     emptyNP ** {s =
        table {
          (Nom|Bare) => ana;
          Acc => nI ; -- object suffix
          Gen => I ;  -- possessive suffix
          Dat => I -- will only be used with preposition لِ
        };
      a = {pgn = pgn; isPron = True}
    };

    proDrop : NP -> NP = \np ->
      case np.a.isPron of {
        True => np ** {s = table {Nom => [] ; x => np.s ! x}};
        _    => np
      } ;

    emptyNP : NP = {
      s = \\_ => [] ;
      a = {pgn = Per3 Masc Sg ; isPron = False} ;
      isHeavy = False ;
      empty = [] } ;

    agrNP : Agr -> NP = \agr -> emptyNP ** {a = agr} ;

    -- e.g. al-jamii3, 2a7ad
    regNP : Str -> Number -> State -> NP = \word,n,s ->
      agrNP {pgn = Per3 Masc n ; isPron = False} ** {
        s = \\c => fixShd word (dec1sg ! s ! c) ;
        } ;

    -- e.g. hadha, dhaalika
    indeclNP : Str -> Number -> NP = \word,n -> emptyNP ** {
      s = \\c => word
      } ;

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

    gn2pron : AAgr -> NP = \gn ->
      pgn2pron (gn2pgn gn) ;

    pron2np : NP -> NP = \np -> np ** {
      a = np.a ** {isPron=False} -- hack, sometimes we *don't* want prodrop
    } ;

    reflPron : Case -> PerGenNum -> Str = \c,pgn ->
      let pron : NP = pgn2pron pgn
       in "نَفْس" + caseTbl ! c ++ BIND ++ pron.s ! Gen ;

    reflV : Verb -> Verb = \v -> v ** {
      s = \\vf => case vf of {
        VPerf _ pgn   => v.s ! vf ++ reflPron Acc pgn ;
        VImpf _ _ pgn => v.s ! vf ++ reflPron Acc pgn ;
        VImp g n      => v.s ! vf ++ reflPron Acc (Per2 g n) ;
        _             => v.s ! vf ++ reflPron Acc (Per3 Masc Sg) ----
        }
      } ;

  ladaa_V : Verb =
    let laday : PerGenNum -> Str = \pgn -> case pgn of {
          Per1 Sing    => "لَدَيَّ" ;
          Per3 Masc Sg => "لَدَيْهِ" ;    -- vowel assimilation
          Per3 Masc Dl => "لَدَيْهِمَا" ; -- vowel assimilation
          Per3 Masc Pl => "لَدَيْهِم" ;   -- vowel assimilation
          Per3 Fem  Pl => "لَدَيْهِنَّ" ; -- vowel assimilation
          _            => "لَدَيْ" + (pgn2pron pgn).s ! Gen
        } ;
     in { s = table {
      VImpf Ind Act pgn  => laday pgn ;
      vf@(VImpf _ _ pgn) => copula.s ! vf ++ laday pgn ;
      vf@(VPerf _ pgn)   => copula.s ! vf ++ laday pgn ;
      vf@(VImp g n)      => copula.s ! vf ++ laday (Per2 g n) ;
      x                  => copula.s ! x ++ "لَدَى" }

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

    ip2np : IP -> Bool -> NP = \ip,isPred -> emptyNP ** ip ** {s = ip.s ! isPred ! Masc ! Def} ;
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
-- V & VP
-- Verb morphology and type Verb in MorphoAra

  Verb2 : Type = Verb ** {c2 : Preposition} ;
  Verb3 : Type = Verb2 ** {c3 : Preposition} ;

  param VPForm =
        VPPerf
      | VPImpf Mood
      | VPImp
      | VPGer ;

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

    uttVP : VPForm -> VP -> (Gender=>Str) = \vpf,vp ->
     \\g => vp.s ! Per3 g Sg ! vpf
         ++ vp.obj.s ++ vp.pred.s ! {n = Sg ; g = g} ! Nom
         ++ vp.s2 ;

    predV : Verb -> VP = \v ->
      { s = \\pgn,vf =>
          let gn = pgn2gn pgn in
          case vf of {
            VPPerf => v.s ! VPerf Act pgn ;
            VPImpf m => v.s ! VImpf m Act pgn ;
            VPImp => v.s ! VImp gn.g gn.n ;
            VPGer => v.s ! Masdar
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
            VPPerf   => v.s ! VPerf   Pas pgn ;
            VPImpf m => v.s ! VImpf m Pas pgn ;
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

            -- very unsure about this /IL
            sc : Preposition = case o of {
                  Subord => {s=[]; c=Acc; binds=False} ;
                  _ => case np.a.isPron of {
                         True => noPrep ; -- to prevent weird stuff with VVs, might be overly specific
                         _    => vp.sc }
                } ;
            subj = np.empty ++ sc.s ++ bindIf sc.binds
                ++ case vp.isPred of {
                      False => (proDrop np).s ! sc.c ; -- prodrop if it's not predicative
                      True  =>           np.s ! sc.c
                   } ;
          } in wordOrder o
                  vp.obj.a.isPron np.a.isPron np.isHeavy
                  (vStr vp pgn t p o)
                  vp.obj.s
                  (pred vp pgn t p)
                  vp.s2
                  subj
      } ;

    -- seems complicated, but this is to share code with VPS and other similar structures
    wordOrder : Order -> (objIsPron,subjIsPron,subjIsHeavy : Bool) -> (verb,obj,pred,adv,subj : Str) -> Str =
      \o,objIsPron,subjIsPron,subjIsHeavy,verb,obj,pred,adv,subj ->
          let cl = wordOrderNoSubj o objIsPron verb obj pred adv in
          case o of {
            -- If subject is pronoun, affix it in Subord word order.
            Subord =>
              let bind = if_then_Str subjIsPron BIND []
               in cl.before ++ bind ++ subj ++ cl.after ;

            -- If subject is "heavy" (e.g. contains a relative clause),
            Verbal =>  -- then override Verbal word order.
              case subjIsHeavy of {
                True  => subj ++ cl.before ++ cl.after ;
                False => cl.before ++ subj ++ cl.after
              } ;

            -- Any other word order, no special checks.
            _  => cl.before ++ subj ++ cl.after
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

    vStr : VP -> PerGenNum -> ParamX.Tense -> Polarity -> Order -> Str = \vp,pgn,tn,pl,o ->
      let kataba  = vp.s ! pgn ! VPPerf ;
          yaktubu = vp.s ! pgn ! VPImpf Ind ;
          yaktuba = vp.s ! pgn ! VPImpf Cnj ;
          yaktub  = vp.s ! pgn ! VPImpf Jus ;
          -- Various negative particles
          la    = "لَا" ;
          laysa = "لَيسَ" ;  -- "neg. copula"
          lam   = "لَمْ" ;   -- neg. past
          alla  = "أَلَّا" ; -- neg. subjunctive
          lan   = "لَنْ" ;   -- neg. future
       in case <vp.isPred,tn,pl,o> of {
            <False, Pres, Pos, _> => yaktubu ;
            <False, Pres, Neg, _> => la ++ yaktubu ;
            <True, Pres, Pos, _> => [] ;    --no verb "to be" in present
            <True, Pres, Neg, _> => laysa ; --same here, just add negation particle
            <_, Past, Pos, _> => kataba ;
            <_, Past, Neg, _> => lam ++ yaktub ;
            <_, Cond, Pos, _> => yaktuba ;
            <_, Cond, Neg, _> => alla ++ yaktuba ;
            <_, Fut,  Pos, _> => glue "سَ" yaktubu ;
            <_, Fut,  Neg, Subord> => alla ++ yaktuba ; -- might be too specific for just one case /IL
            <_, Fut,  Neg, _> => lan ++ yaktuba
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
      a : AgrLite -- default Agr in a VP without real Obj is Per3 Masc Sg.
      };      -- need isPron for word order in predVP, and pgn for ImpersCl

    Subj : Type = {s : Case => Str ; isPron : Bool} ;

    np2subj : NP -> Subj = \np -> np ** {isPron = np.a.isPron} ;
    subj2np : Subj -> NP = \su -> emptyNP ** su ** {a = {pgn = emptyNP.a.pgn ; isPron = su.isPron}} ;
    emptyObj : Obj = {a = {gn = {g=Masc ; n=Sg} ; isPron = False}; s = []} ;

    insertObj : NP -> VPSlash -> VP = \np,vp -> vp ** {
      obj = {s = vp.obj.s -- old object, if there was one
              ++ bindIfPron np vp -- new object, bind if pronoun and not pred
              ++ vp.agrObj ! np.a.pgn ; -- only used for SlashV2V and Slash3V3
             a = agrLite np.a} ;
      agrObj = \\_ => []
      } ;

    bindIf : Bool -> Str = \b -> if_then_Str b BIND [] ;

    bindIfPron : NP -> {c2:Preposition; isPred:Bool} -> Str = \np,vp ->
      let notNom : Case -> Bool = \c -> case c of {Nom => False; _=>True} ;
          bind = case vp.isPred of {
                   False => bindIf (
                              orB (andB np.a.isPron (notNom vp.c2.c)) --if np is pron, not in nominative
                                   vp.c2.binds) ;
                   True  => [] }
       in vp.c2.s ++ bind ++ np.s ! vp.c2.c ;

    insertPred : Comp -> VP -> VP = \p,vp -> vp **
      { pred = p;
        isPred = True
      };

    insertStr : Str -> VP -> VP = \str,vp -> vp **
      { s2 = vp.s2 ++ str };

    kaan : {s : AAgr => Case => Str} -> VP = \xabar ->
      insertPred xabar (predV copula);

    copula : Verb = v1hollow {f = "ك"; c = "و" ; l = "ن"} u "كَوْن" ;

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

    -- TODO: what to do with vp.c2 if there is no object /IL
    complClSlash = overload {
      complClSlash : NP -> ClSlash -> Cl = \obj,cls ->
        predVP (subj2np cls.subj) (insertObj obj cls) ;
      complClSlash :       ClSlash -> Cl = \cls ->
        predVP (subj2np cls.subj) (insertObj emptyNP cls) -- Empty subject and object
      } ;

    Cl  : Type = {s : Tense => Polarity => Order => Str} ;
    QCl : Type = {s : Tense => Polarity => QForm => Str} ;

    -- To override the default order; forces all orders in a Cl to be the chosen order.
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
      \xams, xAmis ->
      let { xamsa = xams ; -- + "َة";
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
                Masc => \\s,c => defArt s c "ٱِثن" + dl ! s ! c ;
                Fem => \\s,c => defArt s c "ٱِثنَت" + dl ! s ! c
                };
              NOrd => table {
                Masc => \\s,c => defArt s c "ثَان" + dec2sg ! s ! c ;
                Fem => \\s,c => defArt s c "ثَانِيَة" + dec1sg ! s ! c

                }
            };
          ten => \\_,_,s,c => defArt s c "عِشر" + m_pl ! Indef ! c
          }
      };

    num100 : State => Case => Str =
      \\s,c => defArt s c "مِٱَة" + dec1sg ! s ! c;

    num200 : State => Case => Str =
      \\s,c => defArt s c "مِٱَة" + dl ! s ! c ;

    num1000 : State => Case => Str =
      \\s,c => defArt s c "أَلف" + dec1sg ! s ! c;

    num2000 : State => Case => Str =
      \\s,c => defArt s c "أَلف" + dl ! s ! c ;

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
