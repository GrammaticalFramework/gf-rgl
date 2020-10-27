    --# -path=.:../abstract:../common:../../prelude
--
----1 Arabic auxiliary operations.

resource ResAra = MorphoAra ** open Prelude, Predef, OrthoAra, ParamX  in {

  flags optimize=noexpand ; coding=utf8 ;

oper

-- Root-and-pattern based morphology is in MorphoAra: specifically, the opers
-- that construct nouns, adjectives and verbs.
-- Here are opers that
-- * construct simpler lexical categories (without root and pattern morphology)
-- * construct phrasal categories
-- * manipulate any of the categories.

-----------------------------------------------------------------------------
-- Det, Quant, Ord

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

  NumOrdCard : Type = {
    s : Gender => State => Case => Str ;
    n : Size ;
    isNum : Bool
    } ;

  uttNum : NumOrdCard -> (Gender => Str) ;
  uttNum n = \\g => n.s ! Fem ! Const ! Bare ;

  Agr = {pgn : PerGenNum; isPron : Bool} ;
  AgrLite = {gn : AAgr ; isPron : Bool} ; --used in ImpersCl
  AAgr = {g : Gender ; n : Number} ;

  agrLite : Agr -> AgrLite = \a -> a ** {gn = pgn2gn a.pgn} ;
  is1sg : Agr -> Bool = \a ->
    case a.pgn of {Per1 Sing => True; _ => False} ;

  someSg_Det = mkDet "أَحَد" Sg Const ;
  somePl_Det = mkDet "بَعض" Pl Const ;

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
-- CN, AP

  AP : Type = {s : Species => Gender => NTable } ;
  uttAP : AP -> (Gender => Str) ;
  uttAP ap = \\g => ap.s ! NoHum ! g ! Sg ! Const ! Bare ;

  CN : Type = Noun ** {np : Case => Str ; isHeavy : Bool};

  -- All fields of CN
  cn2str : CN -> Number -> State -> Case -> Str = \cn,n,s,c ->
    cn.s  ! n ! s ! c ++
    cn.s2 ! n ! s ! c ++
    cn.np ! c ;

  useN : Noun -> CN = \n -> n ** {
    np = \\_ => [] ;
    isHeavy = False } ;

  uttCN : CN -> (Gender => Str) ;
  uttCN cn = \\_ => cn2str cn Sg Indef Bare ;


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

  pron2np : NP -> NP = \np -> np ** {
    a = np.a ** {isPron=False} -- hack, sometimes we *don't* want prodrop
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

  reflPron : Case -> PerGenNum -> Str = \c,pgn ->
    let pron : NP = pgn2pron pgn
     in "نَفْس" + caseTbl ! c ++ BIND ++ pron.s ! Gen ;

-- Used in NounAra for DetCN

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
-- V, VP
-- Verb morphology and types Verb(2,3) in MorphoAra

  reflV : Verb -> Verb = \v -> v ** {
    s = \\vf => case vf of {
      VPerf _ pgn   => v.s ! vf ++ reflPron Acc pgn ;
      VImpf _ _ pgn => v.s ! vf ++ reflPron Acc pgn ;
      VImp g n      => v.s ! vf ++ reflPron Acc (Per2 g n) ;
      _             => v.s ! vf ++ reflPron Acc (Per3 Masc Sg) ----
      }
    } ;

  laysa : PerGenNum => Str = table {
    Per1 Sing => "لَسْتُ" ;
    Per1 Plur => "لَسْنَا" ;
    Per2 Fem  Sg => "لَسْتِ" ;
    Per2 Masc Sg => "لَسْتَ" ;
    Per2 _    Dl => "لَسْتُمَا" ;
    Per2 Fem  Pl => "لَسْتُنَّ" ;
    Per2 Masc Pl => "لَسْتُمْ" ;
    Per3 Fem  Sg => "لَيْسَتْ" ;
    Per3 Masc Sg => "لَيْسَ" ;
    Per3 Fem  Dl => "لَيْسَتَا"  ;
    Per3 Masc Dl => "لَيْسَا" ;
    Per3 Fem  Pl => "لَسْنَ" ;
    Per3 Masc Pl => "لَيْسُوا"
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

  -- Sometimes a verb is only used in one form (per3 masc sg);
  -- ideally, one would use an impersonal syntactic construction,
  -- less ideally, hardcode the verb to only contain forms of one person.
  forcePerson : PerGenNum -> Verb -> Verb = \pgn,verb ->
    let gn = pgn2gn pgn in verb ** {
    s = \\vf => case vf of {
                  VPerf   v _ => verb.s ! VPerf v pgn ;
                  VImpf m v _ => verb.s ! VImpf m v pgn ;
                  VImp _g _n  => verb.s ! VImp gn.g gn.n ;
                  _           => verb.s ! vf }
    } ;

param
  VPForm = VPPerf
         | VPImpf Mood
         | VPImp
         | VPGer ;

  VType = -- indicates if there is a predicate (xabar):
          Copula  -- 1) disappears in equational sentences
                  -- 2) its argument ('xabar') is in the pred field
                  -- 3) it is negated with laysa
         | Have   -- stays in eq. sentence, argument is in obj field, but is negated with laysa.
         | NotPred ; -- any other verb but copula and have_V2

oper

  BaseVP : Type = { -- to minimise duplication of code for VPS
    sc : Preposition ; -- subject case: e.g.  يُمْكِنُ *لِ*Xِ
    obj : Obj;
    pred : Comp;
    vtype : VType ; -- copula, have_V2 or normal verb
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
      vtype = NotPred
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
            ++ vp.agrObj ! np.a.pgn ; -- used for SlashV2V, Slash3V3 and SlashV2S
           a = agrLite np.a} ;
    agrObj = \\_ => []
    } ;

  bindIf : Bool -> Str = \b -> if_then_Str b BIND [] ;

  bindIfPron : NP -> {c2:Preposition; vtype:VType} -> Str = \np,vp ->
    let notNom : Case -> Bool = \c -> case c of {Nom => False; _=>True} ;
        bind = case vp.vtype of {
                 Copula => [] ;
                 _ => bindIf (orB (andB np.a.isPron (notNom vp.c2.c)) --if np is pron, not in nominative
                                  vp.c2.binds)
                 }
     in vp.c2.s ++ bind ++ np.s ! vp.c2.c ;

  insertPred : Comp -> VP -> VP = \p,vp -> vp **
    { pred = p;
      vtype = Copula
    };

  insertStr : Str -> VP -> VP = \str,vp -> vp **
    { s2 = vp.s2 ++ str };

  kaan : {s : AAgr => Case => Str} -> VP = \xabar ->
    insertPred xabar (predV copula);

  copula : Verb = v1hollow (mkRoot3 "كون") u "كَوْن" ;

-----------------------------------------------------------------------------
-- Cl

  predVP : NP -> VP -> Cl = \np,vp -> {
    s = \\t,p,o =>
      let pgn =
            case <o,vp.vtype,np.a.isPron> of {
              <Verbal, Copula, False> => np.a.pgn;
              <Verbal, _,      False> => verbalAgr np.a.pgn;
              _                       => np.a.pgn
            };

          -- very unsure about this /IL
          sc : Preposition = case o of {
                Subord => {s=[]; c=Acc; binds=False} ;
                _ => case np.a.isPron of {
                       True => noPrep ; -- to prevent weird stuff with VVs, might be overly specific
                       _    => vp.sc }
              } ;
          subj = np.empty ++ sc.s ++ bindIf sc.binds
              ++ case vp.vtype of {
                    Copula =>      np.s ! sc.c ;
                    _ => (proDrop np).s ! sc.c -- prodrop if it's not predicative
                 } ;
      in wordOrder o
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
     in case <vp.vtype,tn,pl> of {
          <Copula, Pres, Pos> => vp.pred.s ! gn ! Nom; --xabar marfooc
          _                   => vp.pred.s ! gn ! Acc --xabar kaana wa laysa manSoob
        } ;

  vStr : VP -> PerGenNum -> ParamX.Tense -> Polarity -> Order -> Str = \vp,pgn,tn,pl,o ->
    let kataba  = vp.s ! pgn ! VPPerf ;
        yaktubu = vp.s ! pgn ! VPImpf Ind ;
        yaktuba = vp.s ! pgn ! VPImpf Cnj ;
        yaktub  = vp.s ! pgn ! VPImpf Jus ;
        -- Various negative particles
        la    = "لَا" ;
        lam   = "لَمْ" ;   -- neg. past
        alla  = "أَلَّا" ; -- neg. subjunctive
        lan   = "لَنْ" ;   -- neg. future
     in case <vp.vtype,tn,pl,o> of {
          <Have, Pres, Neg, _,> => laysa ! Per3 Masc Sg ++ yaktubu ; -- special case for have_V2
          <Copula, Pres, Pos, _> => [] ;    --no verb "to be" in present
          <Copula, Pres, Neg, _> => laysa ! pgn ; -- negative copula
          <_, Pres, Pos, _> => yaktubu ;
          <_, Pres, Neg, _> => la ++ yaktubu ;
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
-- Slash categories

  VPSlash : Type = VP ** {c2 : Preposition ; agrObj : PerGenNum => Str} ;
  ClSlash : Type = VPSlash ** {subj : Subj} ;

  emptyVPslash : VP -> VPSlash = \vp -> vp ** {
    c2 = accPrep ; agrObj = \\_ => []
    } ;

  slashV2 : Verb2 -> VPSlash = \v ->
    predV v ** {
      c2 = v.c2 ; agrObj = \\_ => [] ;
      vtype = case v.c2.c of {
                 Nom => Have ; -- for have_V2
                 _   => NotPred } ;
      } ;

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
    } ; -- If VP has a c2, its placement should be handled in the function that calls complClSlash.
        -- See QuestSlash (in QuestionAra) for an example.

  Cl  : Type = {s : Tense => Polarity => Order => Str} ;
  QCl : Type = {s : Tense => Polarity => QForm => Str} ;

  -- To override the default order; forces all orders in a Cl to be the chosen order.
  forceOrder : Order -> Cl -> Cl = \o,cl ->
    {s = \\t,p,_ => cl.s ! t ! p ! o} ;

  -- these are chosen in many places, trying to be consistent
  toOrder : QForm -> Order = \qf ->
    case qf of { QDir   => Nominal ; -- works for wh-questions, not for y/n
                 QIndir => Verbal } ;

-----------------------------------------------------------------------------
-- Relative

param
  RAgr = RSg Gender | RPl Gender | RDl Gender Case ;

oper
  agr2ragr = overload {
    agr2ragr : PerGenNum -> Case -> RAgr = \pgn,c ->
      let gn = pgn2gn pgn in case <gn.n,gn.g,a> of {
        <Sg,x> => RSg x ;
        <Dl,x> => RDl x c ;
        <Pl,x> => RPl x } ;
    agr2ragr : Number -> Case -> Gender -> RAgr = \n,c,g ->
      case n of {
        Sg => RSg g ;
        Dl => RDl g c ;
        Pl => RPl g }
    } ;

  RCl : Type = {s : Tense => Polarity => PerGenNum => Case => Str} ;
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
    let { xAmisa = xAmis +  "َة"} in
    mkNum xams xAmis xAmisa ;

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
