resource ResRus = ParamRus ** open Prelude, ZaliznyakAlgo, Maybe in {
flags coding=utf8 ; optimize=all ;

---------------
-- Nouns -- Существительные
---------------

-- novel idea (for RGL): lexical items stored as records rather than tables. See [???]
-- advantages:
-- - easier to make exceptions to paradigms (by ** {})
-- - easier to keep the number of forms minimal
-- - easier to see what is happening than with lots of anonymous arguments to mkN, mkA, mkV

-- so this is the lincat of N

oper

-- Mnemonics for cases: (add to lexicon as well)
-- Nom есть (кто? что?)
-- Gen нет (кого? чего?)
-- Dat дать (кому? чему?)
-- Acc вижу (кого? что?)
-- Ins горжусь (кем? чем?)
-- Pre думать (о ком? о чём?)
-- Loc нахожусь (где? на чём? в чём?)  -- add if different from Pre
-- Ptv налей (чего?)                   -- add variant if different from Gen
-- VocRus "здравствуй, {}!"            -- add variant if different from Nom

  NounForms : Type = {
    snom, sgen, sdat, sacc, sins, sprep, sloc, sptv, svoc,
    pnom, pgen, pdat, pacc, pins, pprep : Str ;
    g : Gender ;
    mayben : MaybeNumber ;
    anim : Animacy
  } ;
  Noun2Forms = NounForms ** {c2 : ComplementCase} ;
  Noun3Forms = NounForms ** {c2,c3 : ComplementCase} ;

-- But traditional tables make agreement easier to handle in syntax
-- so this is the lincat of CN

  Noun : Type = {
    s : Number => Case => Str ;
    g : Gender ;
    mayben : MaybeNumber ;  -- used to control dependent words
    anim : Animacy
  } ;

  NounPhrase = {
    s : Case => Str ;
    pron : Bool ; -- this only indicates n-prefixable pronouns
    a : Agr
    } ;

  nounFormsNoun : NounForms -> Noun
    = \forms -> {
      s = table {
        Sg => table {
          Nom => forms.snom ;
          Gen => forms.sgen ;
          Dat => forms.sdat ;
          Acc => forms.sacc ;
          Ins => forms.sins ;
          Pre => forms.sprep ;
          Loc => forms.sloc ;
          Ptv => forms.sptv ;
          VocRus => forms.svoc
        } ;
        Pl => table {
          Nom => forms.pnom ;
          Gen => forms.pgen ;
          Dat => forms.pdat ;
          Acc => forms.pacc ;
          Ins => forms.pins ;
          Pre => forms.pprep ;
          Loc => forms.pprep ;
          Ptv => forms.pgen ;
          VocRus => forms.pnom
        }
      } ;
      g = forms.g ;
      mayben=forms.mayben ;
      anim = forms.anim
    } ;

  guessNounForms : Str -> NounForms
    = \word ->
    let nfb : NounFormsBase =
    case word of {
      _ + "уть"                            => makeNoun word Masc Inanimate (ZN 8 No B NoC) ;
      _ + "ий"                             => makeNoun word Masc Inanimate (ZN 7 No A NoC) ;
      _ + "ия"                             => makeNoun word Fem Inanimate (ZN 7 No A NoC) ;
      _ + "ие"                             => makeNoun word Neut Inanimate (ZN 7 No A NoC) ;
      _ + "ье"                             => makeNoun word Neut Inanimate (ZN 6 Ast A NoC) ;
      _ + "тель"                           => makeNoun word Masc Inanimate (ZN 2 No A NoC) ;
      _ + "ь"                              => makeNoun word Fem Inanimate (ZN 8 No A NoC) ;
      _ + "и"                              => makeNoun word Neut Inanimate ZN0 ;
      _ + #consonant + ("к"|"х"|"г") + "а" => makeNoun word Fem Inanimate (ZN 3 Ast A NoC) ;
      _ + ("к" | "х" | "г")                => makeNoun word Masc Inanimate (ZN 3 No A NoC) ;
      _ + ("к" | "х" | "г") + "а"          => makeNoun word Fem Inanimate (ZN 3 No A NoC) ;
      _ + "ца"                             => makeNoun word Fem Animate (ZN 5 No A NoC) ;
      _ + "й"                              => makeNoun word Masc Inanimate (ZN 6 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ")          => makeNoun word Masc Inanimate (ZN 4 No A NoC) ;
      _ + "ша"                             => makeNoun word Fem Animate (ZN 4 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ") + "а"    => makeNoun word Fem Inanimate (ZN 4 No A NoC) ;
      _ + "ц"                              => makeNoun word Masc Inanimate (ZN 5 Ast A NoC) ;
      _ + "о"                              => makeNoun word Neut Inanimate (ZN 1 No A NoC) ;
      _ + "а"                              => makeNoun word Fem Inanimate (ZN 1 No A NoC) ;
      _                                    => makeNoun word Masc Inanimate (ZN 1 No A NoC)
    } in
    noMinorCases nfb ;

  guessLessNounForms : Str -> Gender -> Animacy -> NounForms
    = \word, g, anim ->
    let nfb : NounFormsBase =
    case word of {
      _ + "уть"                            => makeNoun word g anim (ZN 8 No B NoC) ;
      _ + "ий"                             => makeNoun word g anim (ZN 7 No A NoC) ;
      _ + "ия"                             => makeNoun word g anim (ZN 7 No A NoC) ;
      _ + "ие"                             => makeNoun word g anim (ZN 7 No A NoC) ;
      _ + "ье"                             => makeNoun word g anim (ZN 6 Ast A NoC) ;
      _ + "тель"                           => makeNoun word g anim (ZN 2 No A NoC) ;
      _ + "ь"                              => makeNoun word g anim
                                               (case g of {Fem => (ZN 8 No A NoC); _ => (ZN 2 No A NoC)});
      _ + "и"                              => makeNoun word g anim ZN0 ;
      _ + #consonant + ("к"|"х"|"г") + "а" => makeNoun word g anim (ZN 3 Ast A NoC) ;
      _ + ("к" | "х" | "г")                => makeNoun word g anim (ZN 3 No A NoC) ;
      _ + ("к" | "х" | "г") + "а"          => makeNoun word g anim (ZN 3 No A NoC) ;
      _ + "ца"                             => makeNoun word g anim (ZN 5 No A NoC) ;
      _ + "й"                              => makeNoun word g anim (ZN 6 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ")          => makeNoun word g anim (ZN 4 No A NoC) ;
      _ + "ша"                             => makeNoun word g anim (ZN 4 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ") + "а"    => makeNoun word g anim (ZN 4 No A NoC) ;
      _ + "ц"                              => makeNoun word g anim (ZN 5 Ast A NoC) ;
      _ + "о"                              => makeNoun word g anim (ZN 1 No A NoC) ;
      _ + "а"                              => makeNoun word g anim (ZN 1 No A NoC) ;
      _                                    => makeNoun word g anim (ZN 1 No A NoC)
    } in
    noMinorCases nfb ;

  immutableNounForms : Str -> Gender -> Animacy -> NounForms
    = \s, g, anim -> noMinorCases (immutableNounCases s g anim ) ;

  noMinorCases : NounFormsBase -> NounForms
    = \base -> base ** {
      sloc = base.sprep ;
      sptv = base.sgen ;
      svoc = base.snom ;
      mayben = BothSgPl
    } ;

  mkNAltPl : NounForms -> NounForms -> NounForms
    = \sgn, pln -> sgn ** {
      pnom  = pln.pnom ;
      pgen  = pln.pgen ;
      pdat  = pln.pdat ;
      pacc  = pln.pacc ;
      pins  = pln.pins ;
      pprep = pln.pprep
    } ;

  applyMaybeNumber : NounForms -> NounForms
    = \nf -> case <nf.mayben.exists, fromMaybe Number Sg nf.mayben> of {
      <True,Sg> => nf **  {
        pnom  = nf.snom ;
        pgen  = nf.sgen ;
        pdat  = nf.sdat ;
        pacc  = nf.sacc ;
        pins  = nf.sins ;
        pprep = nf.sprep
        } ;
      <True,Pl> => nf ** {
        snom  = nf.pnom ;
        sgen  = nf.pgen ;
        sdat  = nf.pdat ;
        sacc  = nf.pacc ;
        sins  = nf.pins ;
        sprep = nf.pprep ;
        sloc  = nf.pprep ;
        sptv  = nf.pgen ;
        svoc  = nf.pnom ;
        } ;
      _ => nf
      } ;

  mkNplus : NounForms -> NounForms
    = \nf -> nf ;

  mkN2plus : Noun2Forms -> Noun2Forms
    = \nf -> nf ;

  mkFun : NounForms -> ComplementCase -> Noun2Forms = \f, p -> f ** {c2 = p} ;
  mkFun2 : NounForms -> ComplementCase -> ComplementCase -> Noun3Forms = \f, p2, p3 -> f ** {c2=p2; c3=p3} ;

  ellNoun : NounForms -> NounForms
   = \n -> noMinorCases (immutableNounCases "" n.g n.anim) ;

  AgrTable = Agr => Str ;

  agree : ComplementCase -> AgrTable
    = \c -> table {
      _ => c.s  -- TODO: implement
    } ;

  selectCase : (Case => Str) -> ComplementCase -> Str
    = \np,prep -> prep.s ++ np ! prep.c ;  -- TODO: NP - pronoun special treatment

  from2 = {s="из"; c=Gen; hasPrep=True} ;

  mkCompoundN : NounForms -> Str -> NounForms -> NounForms
    = \n1,link,n2 ->
      let l : Str=case link of {x+"-" => BIND ++ "-" ++ BIND ; _ => link} in
      n1 ** {
        snom = n1.snom ++ l ++ n2.snom ;
        sgen = n1.sgen ++ l ++ n2.sgen ;
        sdat = n1.sdat ++ l ++ n2.sdat ;
        sacc = n1.sacc ++ l ++ n2.sacc ;
        sins = n1.sins ++ l ++ n2.sins ;
        sprep = n1.sprep ++ l ++ n2.sprep ;
        sloc = n1.sloc ++ l ++ n2.sloc ;
        sptv = n1.sptv ++ l ++ n2.sptv ;
        svoc = n1.svoc ++ l ++ n2.svoc ;
        pnom = n1.pnom ++ l ++ n2.pnom ;
        pgen = n1.pgen ++ l ++ n2.pgen ;
        pdat = n1.pdat ++ l ++ n2.pdat ;
        pacc = n1.pacc ++ l ++ n2.pacc ;
        pins = n1.pins ++ l ++ n2.pins ;
        pprep = n1.pprep ++ l ++ n2.pprep
      } ;

---------------------------
-- Adjectives -- Прилагательные

  AdjTable = GenNum => Animacy => Case => Str ;

  Adjective : Type = {
    s : AdjTable ;
    short : AgrTable ;
    preferShort : ShortFormPreference
    } ;

  noShorts : PronForms -> AdjForms  -- ???
    = \base -> base ** {
      sm = [] ;
      sf = [] ;
      sn = [] ;
      sp = [] ;
      comp = [] ;
      preferShort = PrefFull
    } ;

  immutableAdjForms = immutableAdjectiveCases ;

  mkAplus : AdjForms -> AdjForms
    = \af -> af ;

  mkAltShort : AdjForms -> AdjForms -> AdjForms
    = \full, short -> full ** {
      sm =  short.sm ;
      sf =  short.sf ;
      sn =  short.sn ;
      sp =  short.sp
    } ;

  adjFormsAdjective : AdjForms -> Adjective
    = \forms -> {
      short = adjFormsToShort forms ;
      s = table {
        GSg Fem => table {
          (Inanimate|Animate) => table {
            Nom => forms.fsnom ;
            Gen => forms.fsgen ;
            Dat => forms.fsgen ;
            Acc => forms.fsacc ;
            Ins => forms.fsins ;
            Pre => forms.fsgen ;
            Loc => forms.fsgen ;
            Ptv => forms.fsgen ;
            VocRus => forms.fsnom
          }
        } ;
        GSg Masc => table {
          Inanimate => table {
            Nom => forms.msnom ;
            Gen => forms.msgen ;
            Dat => forms.msdat ;
            Acc => forms.msnom ;
            Ins => forms.msins ;
            Pre => forms.msprep ;
            Loc => forms.msprep ;
            Ptv => forms.msgen ;
            VocRus => forms.msnom
          } ;
          Animate => table {
            Nom => forms.msnom ;
            Gen => forms.msgen ;
            Dat => forms.msdat ;
            Acc => forms.msgen ;
            Ins => forms.msins ;
            Pre => forms.msprep ;
            Loc => forms.msprep ;
            Ptv => forms.msgen ;
            VocRus => forms.msnom
          }
        } ;
        GSg Neut => table {
          (Inanimate | Animate) => table {
            Nom => forms.nsnom ;
            Gen => forms.msgen ;
            Dat => forms.msdat ;
            Acc => forms.nsnom ;
            Ins => forms.msins ;
            Pre => forms.msprep ;
            Loc => forms.msprep ;
            Ptv => forms.msgen ;
            VocRus => forms.nsnom
          }
        } ;
        GPl => table {
          Inanimate => table {
            Nom => forms.pnom ;
            Gen => forms.pgen ;
            Dat => forms.msins ;
            Acc => forms.pnom ;
            Ins => forms.pins ;
            Pre => forms.pgen ;
            Loc => forms.pgen ;
            Ptv => forms.pgen ;
            VocRus => forms.pnom
          } ;
          Animate => table {
            Nom => forms.pnom ;
            Gen => forms.pgen ;
            Dat => forms.msins ;
            Acc => forms.pgen ;
            Ins => forms.pins ;
            Pre => forms.pgen ;
            Loc => forms.pgen ;
            Ptv => forms.pgen ;
            VocRus => forms.pnom
          }
        }
      } ;
      g = forms.g ;
      -- a = forms.a ;
      preferShort = forms.preferShort
    } ;

  guessAdjectiveForms : Str -> AdjForms
    = \word ->
      let stem = Predef.tk 2 word in
      case word of {
        _ + "шеий"                 => makeAdjective word (ZA 6 No A_ NoC) PrefFull ;
        _ + "цый"                  => makeAdjective word (ZA 5 No A_ NoC) PrefFull ;
        _ + ("к"|"г"|"х") +"ий"    => makeAdjective word (ZA 3 No A_ NoC) PrefFull ;
        _ + ("ш"|"ж"|"ч"|"щ")+"ий" => makeAdjective word (ZA 4 No A_ NoC) PrefFull ;
        _ + #consonant + "ный"     => makeAdjective word (ZA 1 Ast A_ NoC) PrefFull ;
        _ + #consonant + "ний"     => makeAdjective word (ZA 2 Ast A_ NoC) PrefFull ;
        _ + "ый"                   => makeAdjective word (ZA 1 No A_ NoC) PrefFull ;
        _ + "ой"                   => makeAdjective word (ZA 1 No B_ NoC) PrefFull ;
        _ + "ий"                   => makeAdjective word (ZA 2 No A_ NoC) PrefFull ;
        _                          => makeAdjective word (ZA 1 No A_ NoC) PrefFull
    } ;

  makeAdjectiveForms : Str -> Str -> Str -> ShortFormPreference -> AdjForms
    = \nom, comp, zi, spf ->
        let af = makeAdjective nom (parseAdjIndex zi) spf in
        let comp' = case (Predef.length comp) of {0 => af.comp; _ => comp} in
        af ** {comp=comp'} ;

  makeAdjectiveFromNoun : Noun -> Adjective
    = \n -> {
       s = \\gn,anim,cas=> n.s ! numGenNum gn ! cas ;
       short=\\a=>[] ;
       preferShort=PrefFull
    } ;

  the_most = guessAdjectiveForms "самый" ;
  utmost_Adv = makeAdverb "наиболее" ;

  -- [ISACHENKO],p.220 there are three forms in Russian: самый важный; наиболее важный/важен; важнее (всех, всего)
  -- here only first one:
  long_superlative : AdjForms -> AdjForms
    = \af -> {
      msnom = the_most.msnom  ++ af.msnom ;
      fsnom = the_most.fsnom  ++ af.fsnom ;
      nsnom = the_most.nsnom  ++ af.nsnom ;
      pnom  = the_most.pnom   ++ af.pnom  ;
      msgen = the_most.msgen  ++ af.msgen ;
      fsgen = the_most.fsgen  ++ af.fsgen ;
      pgen  = the_most.pgen   ++ af.pgen  ;
      msdat = the_most.msdat  ++ af.msdat ;
      fsacc = the_most.fsacc  ++ af.fsacc ;
      msins = the_most.msins  ++ af.msins ;
      fsins = the_most.fsins  ++ af.fsins ;
      pins  = the_most.pins   ++ af.pins  ;
      msprep= the_most.msprep ++ af.msprep;
      sm    = the_most.sm     ++ af.sm    ;
      sf    = the_most.sf     ++ af.sf    ;
      sn    = the_most.sn     ++ af.sn    ;
      sp    = the_most.sp     ++ af.sp    ;
      comp  = the_most.comp   ++ af.comp  ;
      g=af.g ;
      -- a=af.a ;
      preferShort = PrefFull
    } ;

  makeNFFromAF : AdjForms -> Gender -> Animacy -> NounForms
    = \af, g, anim ->
      case g of {
        Fem => {
          snom = af.fsnom ;
          pnom = af.pnom ;
          sgen = af.fsgen ;
          pgen = af.pgen ;
          sdat = af.fsgen ;
          pdat = af.msins ;
          sacc = af.fsacc ;
          pacc = case anim of {Animate => af.pgen ; Inanimate => af.pnom} ;
          sins = af.fsins ;  -- TODO: there is also variant fsins == fsgen
          pins = af.pins ;
          sprep= af.fsgen ;
          pprep= af.pgen ;
          sloc = af.fsgen ;
          sptv = af.fsgen ;
          svoc = af.fsnom ;
          g=g ;
          mayben=BothSgPl ;
          anim=anim
        } ;
        Masc => {
          snom = af.msnom ;
          pnom = af.pnom ;
          sgen = af.msgen ;
          pgen = af.pgen ;
          sdat = af.msdat ;
          pdat = af.msins ;
          sacc = case anim of {Animate => af.msgen ; Inanimate => af.msnom} ;
          pacc = case anim of {Animate => af.pgen ; Inanimate => af.pnom} ;
          sins = af.msins ;
          pins = af.pins ;
          sprep= af.msprep ;
          pprep= af.pgen ;
          sloc = af.msprep ;
          sptv = af.msgen ;
          svoc = af.msnom ;
          g=g ;
          mayben=BothSgPl ;
          anim=anim
        } ;
        Neut => {
          snom = af.nsnom ;
          pnom = af.pnom ;
          sgen = af.msgen ;
          pgen = af.pgen ;
          sdat = af.msdat ;
          pdat = af.msins ;
          sacc = af.nsnom ;
          pacc = case anim of {Animate => af.pgen ; Inanimate => af.pnom} ;
          sins = af.msins ;
          pins = af.pins ;
          sprep= af.msprep ;
          pprep= af.pgen ;
          sloc = af.msprep ;
          sptv = af.msgen ;
          svoc = af.nsnom ;
          g=g ;
          mayben=BothSgPl ;
          anim=anim
        }
      } ;

  adjFormsToShort : AdjForms -> AgrTable
    = \af -> table {
      Ag (GSg Fem) _ => af.sf ;
      Ag (GSg Masc) _ => af.sm ;
      Ag (GSg Neut) _ => af.sn ;
      Ag GPl _ => af.sp
    } ;

---------------------
-- Verbs -- Глаголы

-- Note 1. Passive voice can be formed only for transitive imperfective verbs
-- Passive has no P1, P2, imperative,
-- Reflexive verbs are to provides as as separate lexical entries.
-- Note 2. Imperative Sg P2 of reflexive verbs, can be сь as well as ся, but because there is no passive forms
-- we can store the sya-schema and 'BIND++' as necessary.

oper

  guessVerbForms : Aspect -> Transitivity -> Str -> Str -> Str -> VerbForms
    = \asp,tran,inf,sg1,sg3 ->
      let guessed : ZVIndex * Reflexivity = guessRegularIndex inf sg1 sg3 in
      let corr_tran = case guessed.p2 of {Reflexive=>Intransitive ; NonReflexive=>tran} in
      makeVerb inf sg1 sg3 guessed.p1 asp corr_tran guessed.p2 ;

  quickGuessVerbForms : Str -> VerbForms
    = \inf ->
      let stem_info = infStemFromVerb inf in
      let stem = stem_info.p1 in
      guessVerbForms Imperfective Transitive inf (stem+"ю") (stem+"ет") ;

  passivateNonReflexive : VerbForms -> VerbForms
    = \vf -> vf ** {refl=Reflexive} ;

  passivate : VerbForms -> VerbForms
    = \vf ->
      case vf.refl of {
        Reflexive => vf ;
        NonReflexive => passivateNonReflexive vf
      } ;

  shortPastPassPart : VerbForms -> GenNum -> Str
    = \vf,gn ->
      case gn of {
        GSg Masc => vf.pppss ;
        GSg Fem => vf.pppss ++ BIND ++ "а" ;
        GSg Neut => vf.pppss ++ BIND ++ "о" ;
        GPl => vf.pppss ++ BIND ++ "ы"
        } ;

  copula : VerbForms
    = {
      inf="быть" ;
      infrefl="являться" ;  --?
      prsg1="—";
      prsg2="—";
      prsg3="есть";
      prpl1="—";
      prpl2="—";
      prpl3="—";   -- also "суть"
      fut=BeFuture ;
      psgm="был";
      psgs="бы";
      isg2="будь";
      isg2refl="явись" ; -- ?
      ipl1="давайте будем";
      pppss="";
      prtr="будучи";
      ptr="быв";
      asp=Imperfective;
      refl=NonReflexive;
      tran=Intransitive
    } ;

  -- normal copula require Nom in Pres. So this is Ins-friendly substitute.
  -- TODO: Provide also Nom-based as idiomatic (?)
  copulaIns : VerbForms
    = copula ** {
      prsg1="являюсь" ;
      prsg2="являешься" ;
      prsg3="является" ;
      prpl1="являемся" ;
      prpl2="являетесь" ;
      prpl3="являются"
    } ;

  copulaEll : VerbForms
    = copula ** {
      prsg1="" ;
      prsg2="" ;
      prsg3="" ;
      prpl1="" ;
      prpl2="" ;
      prpl3=""
    } ;

  copulaFull : VerbForms
    = copula ** {
      prsg1="есть" ;
      prsg2="есть" ;
      prsg3="есть" ;
      prpl1="есть" ;
      prpl2="есть" ;
      prpl3="есть"
    } ;

  selectCopula : CopulaType -> VerbForms
    = \cop -> case cop of {
       NomCopula => copula ;
       ExplicitCopula => copulaFull ;
       EllCopula => copulaEll ;
       InsCopula => copulaIns
    } ;

  can : VerbForms
    = {
      inf="мочь";
      infrefl="мочь" ;
      prsg1="могу";
      prsg2="можешь";
      prsg3="может";
      prpl1="можем";
      prpl2="можете";
      prpl3="могут";
      fut=CanFuture ;
      psgm="мог";
      psgs="мог";
      isg2refl="будь способны" ;   -- *
      isg2="будь способен";  -- some improvisation here
      ipl1="давайте будем способны";   -- maybe, special like for future?
      pppss="";
      prtr="";
      ptr="могши";
      asp=Imperfective;
      refl=NonReflexive;
      tran=Intransitive
    } ;

  want : VerbForms
    = {
      inf="хотеть";
      infrefl="хотеться" ;
      prsg1="хочу";
      prsg2="хочешь";
      prsg3="хочет";
      prpl1="хотим";
      prpl2="хотите";
      prpl3="хотят";
      fut=WantFuture ;
      psgm="хотел";
      psgs="хоте";
      isg2="желай";
      isg2refl="желайся" ;
      ipl1="давайте будем хотеть";
      pppss="";
      prtr="хотя";
      ptr="хотев";
      asp=Imperfective;
      refl=NonReflexive;
      tran=Transitive
    } ;

  nullVerb : VerbForms
    = {
      inf, infrefl,
      prsg1, prsg2, prsg3, prpl1, prpl2, prpl3,
      psgm, psgs,
      isg2, isg2refl, ipl1,
      pppss,
      prtr, ptr ="";
      fut=NullFuture ;
      asp=Imperfective;
      refl=NonReflexive;
      tran=Intransitive
    } ;

  verbPastAgree : VerbForms -> Agr -> Str -> TempParts
    = \vf,a,after -> <"", case a of {
      Ag (GSg Masc) _ => vf.psgm ++ (verbReflAfterConsonant vf) ++ after ;
      Ag (GSg Fem) _ => vf.psgs ++ BIND ++ "ла" ++ (verbRefl vf) ++ after ;
      Ag (GSg Neut) _ => vf.psgs ++ BIND ++ "ло" ++ (verbRefl vf) ++ after ;
      Ag GPl _ => vf.psgs ++ BIND ++ "ли"++ (verbRefl vf) ++ after
    }> ;

  verbReflAfterConsonant : VerbForms -> Str
    = \vf -> case vf.refl of {Reflexive => BIND ++ "ся" ; NonReflexive => ""} ;

  verbRefl : VerbForms -> Str
    = \vf -> case vf.refl of {Reflexive => BIND ++ "сь" ; NonReflexive => ""} ;

  verbInf : VerbForms -> Str
    = \vf -> case vf.refl of {Reflexive => vf.infrefl ; NonReflexive => vf.inf} ;

  verbPresAgree : VerbForms -> Agr -> Str
    = \vf,a -> case a of {
      Ag (GSg _) P1 => vf.prsg1 ++ (verbRefl vf) ;
      Ag (GSg _) P2 => vf.prsg2 ++ (verbReflAfterConsonant vf) ;
      Ag (GSg _) P3 => vf.prsg3 ++ (verbReflAfterConsonant vf) ;
      Ag GPl P1 => vf.prpl1 ++ (verbReflAfterConsonant vf) ;
      Ag GPl P2 => vf.prpl2 ++ (verbRefl vf) ;
      Ag GPl P3 => vf.prpl3 ++ (verbReflAfterConsonant vf)
    } ;

  beFuture : Agr -> Str
    = \a -> case a of {
      Ag (GSg _) P1 => "буду" ;
      Ag (GSg _) P2 => "будешь" ;
      Ag (GSg _) P3 => "будет" ;
      Ag GPl P1     => "будем" ;
      Ag GPl P2     => "будете" ;
      Ag GPl P3     => "будут"
    } ;

  verbFutAgree : VerbForms -> Agr -> Str
    = \vf,a -> case <vf.fut,a> of {
      <NullFuture,_> => [] ;
      <WantFuture,Ag (GSg _) P1> => "захочу" ;
      <WantFuture,Ag (GSg _) P2> => "захочешь" ;
      <WantFuture,Ag (GSg _) P3> => "захочет" ;
      <WantFuture,Ag GPl P1    > => "захотим" ;
      <WantFuture,Ag GPl P2    > => "захотите" ;
      <WantFuture,Ag GPl P3    > => "захотят" ;
      <CanFuture,Ag (GSg _) P1> => "смогу" ;
      <CanFuture,Ag (GSg _) P2> => "сможешь" ;
      <CanFuture,Ag (GSg _) P3> => "сможет" ;
      <CanFuture,Ag GPl P1    > => "сможем" ;
      <CanFuture,Ag GPl P2    > => "сможете" ;
      <CanFuture,Ag GPl P3    > => "смогут" ;
      <BeFuture,_> => beFuture a ;
      _ => case vf.asp of {
        Perfective => verbPresAgree vf a ;
        Imperfective => (beFuture a) ++ verbInf vf
        }
      } ;

  verbImperativeAgree : VerbForms -> Agr -> TempParts
    = \vf,a -> case a of {
      Ag (GSg _) P1 => <"", (verbInf vf)> ;  -- ?
      Ag (GSg _) P2 => <"", case vf.refl of {NonReflexive=>vf.isg2; Reflexive=>vf.isg2refl}> ;
      Ag (GSg x) P3 => <"пусть", verbFutAgree vf (Ag (GSg x) P3)> ;  -- ?
      Ag GPl P1 => <"", vf.ipl1 ++ verbReflAfterConsonant vf> ;
      Ag GPl P2 => <"", vf.isg2 ++ BIND ++ "те" ++ (verbRefl vf)> ;
      Ag GPl P3 => <"пусть", verbFutAgree vf (Ag GPl P3)>
    } ;

  verbAgr : VerbForms -> Mood -> Tense -> Agr -> Polarity -> TempParts
    = \vf,m,temp,a,pol ->
      case <vf.fut,m,temp> of {
        <NullFuture, _, _> => <"",""> ;
        <_, Ind, Past> => verbPastAgree vf a "";
        <_, Ind, Pres> => <"",verbPresAgree vf a>;
        <_, Ind, Fut> => <"",verbFutAgree vf a>;
        <_, Ind, Cond> => verbPastAgree vf a "бы" ;
        <_, Sbjv, _> => verbPastAgree vf a "бы" ;
        <_, Imperative, _> => verbImperativeAgree vf a ;
        <_, Infinitive, _> => <"",verbInf vf>
      } ;

---------------------------
-- Pronouns -- Местоимения

  PronounForms : Type = {
    nom, gen, dat, acc, ins, prep : Str ;
    nPrefix : Bool ;  -- can have forms with prepended "н". Only with personal pronouns, not possessive
    poss : PronForms ;
    a : Agr
  } ;

  IPronounForms : Type = {
    nom, gen, dat, acc, ins, prep : Str ;
    poss : PronForms ;
    anim : Animacy ;
    a : Agr
  } ;

  RPronounForms : Type = {
    s : AdjTable ;
    a : Agr
  } ;

  PronTable = GenNum => Animacy => Case => Str ;

  mkPronTable : PronForms -> PronTable
    = \forms -> table {
      GSg Fem => table {
        (Inanimate|Animate) => table {
          Nom => forms.fsnom ;
          Gen => forms.fsgen ;
          Dat => forms.fsgen ;
          Acc => forms.fsacc ;
          Ins => forms.fsins ;
          Pre => forms.fsgen ;
          Loc => forms.fsgen ;
          Ptv => forms.fsgen ;
          VocRus => forms.fsnom
        }
      } ;
      GSg Masc => table {
        Inanimate => table {
          Nom => forms.msnom ;
          Gen => forms.msgen ;
          Dat => forms.msdat ;
          Acc => forms.msnom ;
          Ins => forms.msins ;
          Pre => forms.msprep ;
          Loc => forms.msprep ;
          Ptv => forms.msgen ;
          VocRus => forms.msnom
        } ;
        Animate => table {
          Nom => forms.msnom ;
          Gen => forms.msgen ;
          Dat => forms.msdat ;
          Acc => forms.msgen ;
          Ins => forms.msins ;
          Pre => forms.msprep ;
          Loc => forms.msprep ;
          Ptv => forms.msgen ;
          VocRus => forms.msnom
        }
      } ;
      GSg Neut => table {
        (Inanimate | Animate) => table {
          Nom => forms.nsnom ;
          Gen => forms.msgen ;
          Dat => forms.msdat ;
          Acc => forms.nsnom ;
          Ins => forms.msins ;
          Pre => forms.msprep ;
          Loc => forms.msprep ;
          Ptv => forms.msgen ;
          VocRus => forms.nsnom
        }
      } ;
      GPl => table {
        Inanimate => table {
          Nom => forms.pnom ;
          Gen => forms.pgen ;
          Dat => forms.msins ;
          Acc => forms.pnom ;
          Ins => forms.pins ;
          Pre => forms.pgen ;
          Loc => forms.pgen ;
          Ptv => forms.pgen ;
          VocRus => forms.pnom
        } ;
        Animate => table {
          Nom => forms.pnom ;
          Gen => forms.pgen ;
          Dat => forms.msins ;
          Acc => forms.pgen ;
          Ins => forms.pins ;
          Pre => forms.pgen ;
          Loc => forms.pgen ;
          Ptv => forms.pgen ;
          VocRus => forms.pnom
        }
      }
    } ;

  Pronoun = {
    s : Case => Str ;
    pron : Bool ;
    poss : PronTable ;
    a : Agr
    } ;

  -- From [RUSGRAM]:
  -- personal      -- личные
  -- possessive    -- притяжательные
  -- reflexive     -- возвратные
  -- indefinite    -- неопределённые
  -- demonstrative -- указательные
  -- interrogative -- вопросительные
  -- relative      -- относительные
     -- TODO: animacy - see [KHOLODILOVA1]
  -- reciprocal    -- взаимные
  -- determinative -- определительные
  -- negative      -- отрицательные
  -- Also [RUWIKT]:
  -- exclamative   -- восклицательные

  personalPron : Agr -> PronounForms
    = \a -> {a=a} **
      case a of {
        Ag (GSg _) P1 => {
          nom, voc = "я" ;
          gen, acc, ptv = "меня" ;
          dat, prep, loc = "мне" ;
          ins = variants {"мной" ; "мною"} ;
          nPrefix = False ;
          poss = doPossessivePronSgP1P2 "мо"
        } ;
        Ag (GSg _) P2 => {
          nom, voc = "ты" ;
          gen, acc, ptv = "тебя" ;
          dat, prep, loc = "тебе" ;
          ins = variants {"тобой" ; "тобою"} ;
          nPrefix = False ;
          poss = doPossessivePronSgP1P2 "тво"
        } ;
        Ag (GSg Masc) P3 => {
          nom, voc = "он" ;
          gen, acc, ptv = "его" ;   -- TODO: n
          dat = "ему" ;   -- TODO: n
          ins = "им" ;   -- TODO: n
          prep, loc = "нём" ;
          nPrefix = True ;
          poss = doPossessivePronP3 "его"
        } ;
        Ag (GSg Fem) P3 => {
          nom, voc = "она" ;
          gen, ptv = variants { "её"; "ей" } ;           -- TODO: n
          dat = "ей" ;                     -- TODO: n
          acc = "её" ;           -- TODO: n
          ins = variants { "ей"; "ею" } ;   -- TODO: n
          prep, loc = "ней" ;
          nPrefix = True ;
          poss = doPossessivePronP3 "её"
        } ;
        Ag (GSg Neut) P3 => {
          nom, voc = "оно" ;
          gen, acc, ptv = "его" ;   -- TODO: n
          dat = "ему" ;   -- TODO: n
          ins = "им" ;   -- TODO: n
          prep, loc = "нём" ;
          nPrefix = True ;
          poss = doPossessivePronP3 "его"
        } ;
        Ag GPl P1 => {
          nom, voc = "мы" ;
          gen, acc, ptv = "нас" ;
          dat = "нам" ;
          ins = "нами" ;
          prep, loc = "нас" ;
          nPrefix = False ;
          poss = doPossessivePronPlP1P2 "наш"
        } ;
        Ag GPl P2 => {
          nom, voc = "вы" ;
          gen, acc, ptv = "вас" ;
          dat = "вам" ;
          ins = "вами" ;
          prep, loc = "вас" ;
          nPrefix = False ;
          poss = doPossessivePronPlP1P2 "ваш"
        } ;
        Ag GPl P3 => {
          nom, voc = "они" ;
          gen, acc, ptv = "их" ;   -- TODO: n
          dat = "им" ;   -- TODO: n
          ins = "ими" ;   -- TODO: n
          prep, loc = "них" ;
          nPrefix = False ;
          poss = doPossessivePronP3 "их"
        }
      } ;

-- Possessive pronouns are more like adjectives

  doPossessivePronSgP1P2 : Str -> PronForms
    = \mo -> {
      msnom = mo + "й" ;
      fsnom = mo + "я" ;
      nsnom = mo + "ё" ;
      pnom = mo + "и" ;
      msgen = mo + "его" ;
      fsgen = mo + "ей" ;
      pgen  = mo + "их" ;
      msdat = mo + "ему" ;
      fsacc = mo + "ю" ;
      msins = mo + "им" ;
      fsins = mo + "ей" ;
      pins  = mo + "ими" ;
      msprep = mo + "ём"
    } ;

  doPossessivePronPlP1P2 : Str -> PronForms
    = \nash -> {
      msnom = nash ;
      fsnom = nash + "а" ;
      nsnom = nash + "е" ;
      pnom = nash + "и" ;
      msgen = nash + "его" ;
      fsgen = nash + "ей" ;
      pgen  = nash + "их" ;
      msdat = nash + "ему" ;
      fsacc = nash + "у" ;
      msins = nash + "им" ;
      fsins = nash + "ей" ;
      pins  = nash + "ими" ;
      msprep = nash + "ем"
    } ;

  doPossessivePronP3 : Str -> PronForms
    = \ego -> {
      msnom,
      fsnom,
      nsnom,
      pnom,
      msgen,
      fsgen,
      pgen,
      msdat,
      fsacc,
      msins,
      fsins,
      pins,
      msprep = ego
    } ;

  selectPronCase : PronounForms -> Case -> Str  -- apply nPrefix ?
    = \forms,cas -> case cas of {
      (Nom | VocRus) => forms.nom ;
      (Gen | Ptv)    => forms.gen ;
      Dat            => forms.dat ;
      Acc            => forms.acc ;
      Ins            => forms.ins ;
      (Pre | Loc)    => forms.prep
    } ;

  selectIPronCase : IPronounForms -> Case -> Str  -- apply nPrefix ?
    = \forms,cas -> case cas of {
      (Nom | VocRus) => forms.nom ;
      (Gen | Ptv)    => forms.gen ;
      Dat            => forms.dat ;
      Acc            => forms.acc ;
      Ins            => forms.ins ;
      (Pre | Loc)    => forms.prep
    } ;


  pronFormsPronoun : PronounForms -> Pronoun
    = \forms -> {
      s = \\cas => selectPronCase forms cas ;
      pron = forms.nPrefix ;
      poss = mkPronTable forms.poss ;
      a = forms.a
    } ;

  doReflexivePron : Str -> Agr -> PronounForms
    -- Nominative is not strictly correct, but also usually not needed
    = \nom,a -> {
      nom=nom ; gen="себя" ; dat="себе" ; acc="себя" ; ins="собой" ; prep="себе" ;
      nPrefix=False ;
      poss=doPossessivePronSgP1P2 "сво" ;  -- "myself's" to "my own" this may be too artificially put here...
      a=a} ;

  reflexivePron : Agr -> PronounForms
    = \a -> {a = a; nPrefix=False} **
      case a of {
        Ag (GSg Masc) _ => doReflexivePron "сам" a;
        Ag (GSg Fem) _ => doReflexivePron "сама" a;
        Ag (GSg Neut) _ => doReflexivePron "само" a;
        Ag GPl _ => doReflexivePron "сами" a
        };

  sebya = pronFormsPronoun (reflexivePron (Ag (GSg Masc) P3)) ;
  sam = (guessAdjectiveForms "самый") ** {
    fsnom="сама" ;
    msnom="сам" ;
    nsnom="само" ;
    fsacc="саму" ;
    fsins=variants {"самой"; "самою"} ;
    msins="самим";
    nsins="самим";
    pgen="самих" ;
    pnom="сами" ;
    pins="самими" ;
    sp="сами"
    } ;

  all_Pron = pronoun2AstB "весь" ;
  only_Pron = guessAdjectiveForms "единственный" ;

  vse : PronounForms = {
    nom="все" ; gen="всех" ; dat="всем" ; acc="всех" ; ins="всеми" ; prep="всех" ;
    nPrefix=False ;
    poss=all_Pron ;
    a=Ag GPl P3
    } ;

  vse_ina : PronounForms = {
    nom="всё" ; gen="всего" ; dat="всему" ; acc="всё" ; ins="всем" ; prep="всём" ;
    nPrefix=False ;
    poss=all_Pron ;
    a=Ag (GSg Neut) P3
    } ;

  doChPron : Str -> Agr -> Animacy -> IPronounForms  -- что, ничто
    = \ch, a, anim -> {  -- "ч", "нич"
      a = a ;
      anim=anim ;
      nom, voc = ch + "то" ;
      gen, acc, ptv = ch + "его" ;
      dat = ch + "ему" ;
      prep, loc = ch + "ём" ;
      ins = ch + "ем" ;
      poss = {
        msnom = ch + "ей" ;
        fsnom = ch + "ья" ;
        nsnom = ch + "ьё" ;
        pnom = ch + "ьи" ;
        msgen = ch + "ьего" ;
        fsgen = ch + "ьей" ;
        pgen  = ch + "ьих" ;
        msdat = ch + "ьему" ;
        fsacc = ch + "ью" ;
        msins = ch + "ьим" ;
        fsins = ch + "ьей" ;
        pins  = ch + "ьими" ;
        msprep = ch + "ьём"
      }
    } ;

  doKPron : Str -> Agr -> Animacy -> IPronounForms  -- кто, никто
    = \ch, a, anim ->   -- "к", "ник"
      let subPoss = (Predef.tk 1 ch) + "ч" in {
      a = a ;
      anim=anim ;
      nom, voc = ch + "то" ;
      gen, acc, ptv = ch + "ого" ;
      dat = ch + "ому" ;
      prep, loc = ch + "ом" ;
      ins = ch + "ем" ;
      poss = (doChPron subPoss a anim).poss
      } ;

  doKotoryjPron : Str -> Agr -> RPronounForms
    = \w, a -> {   -- "который"
      a = a ;
      s=(adjFormsAdjective (guessAdjectiveForms w)).s
      } ;

  prependIP : Str -> IPronounForms -> IPronounForms
    = \s,ip -> ip ** {
      nom=s ++ ip.nom ;
      gen=s ++ ip.gen ;
      dat=s ++ ip.dat ;
      acc=s ++ ip.acc ;
      ins=s ++ ip.ins ;
      prep=s ++ ip.prep ;
      poss={
        msnom = s ++ ip.poss.msnom ;
        fsnom = s ++ ip.poss.fsnom ;
        nsnom = s ++ ip.poss.nsnom ;
        pnom  = s ++ ip.poss.pnom ;
        msgen = s ++ ip.poss.msgen ;
        fsgen = s ++ ip.poss.fsgen ;
        pgen  = s ++ ip.poss.pgen ;
        msdat = s ++ ip.poss.msdat ;
        fsacc = s ++ ip.poss.fsacc ;
        msins = s ++ ip.poss.msins ;
        fsins = s ++ ip.poss.fsins ;
        pins  = s ++ ip.poss.pins ;
        msprep= s ++ ip.poss.msprep ;
        }
      } ;

  appendToIP : IPronounForms -> Str -> IPronounForms
    = \ip,s -> ip ** {
      nom=ip.nom ++ s;
      gen=ip.gen ++ s;
      dat=ip.dat ++ s;
      acc=ip.acc ++ s;
      ins=ip.ins ++ s;
      prep=ip.prep ++ s;
      poss={
        msnom = ip.poss.msnom ++ s;
        fsnom = ip.poss.fsnom ++ s;
        nsnom = ip.poss.nsnom ++ s;
        pnom  = ip.poss.pnom ++ s;
        msgen = ip.poss.msgen ++ s;
        fsgen = ip.poss.fsgen ++ s;
        pgen  = ip.poss.pgen ++ s;
        msdat = ip.poss.msdat ++ s;
        fsacc = ip.poss.fsacc ++ s;
        msins = ip.poss.msins ++ s;
        fsins = ip.poss.fsins ++ s;
        pins  = ip.poss.pins ++ s;
        msprep= ip.poss.msprep ++ s;
      }
    } ;

  nounToNounForm : Noun -> NounForms
    = \n -> {
      snom=n.s ! Sg ! Nom ;
      sgen=n.s ! Sg ! Gen ;
      sdat=n.s ! Sg ! Dat ;
      sacc=n.s ! Sg ! Acc ;
      sins=n.s ! Sg ! Ins ;
      sprep=n.s ! Sg ! Pre ;
      sloc=n.s ! Sg ! Loc ;
      sptv=n.s ! Sg ! Ptv ;
      svoc=n.s ! Sg ! VocRus ;
      pnom=n.s ! Pl ! Nom ;
      pgen=n.s ! Pl ! Gen ;
      pdat=n.s ! Pl ! Dat ;
      pacc=n.s ! Pl ! Acc ;
      pins=n.s ! Pl ! Ins ;
      pprep=n.s ! Pl ! Pre ;
      ploc=n.s ! Pl ! Loc ;
      pptv=n.s ! Pl ! Ptv ;
      pvoc=n.s ! Pl ! VocRus ;
      g=n.g ;
      mayben=n.mayben ;
      anim=n.anim
    } ;

  caseTableToRecord : (Case => Str) -> Agr -> Animacy -> IPronounForms
    = \ct,a,anim -> {
      nom=ct ! Nom ;
      gen=ct ! Gen ;
      dat=ct ! Dat ;
      acc=ct ! Acc ;
      ins=ct ! Ins ;
      prep=ct ! Pre ;
      poss={
        msnom = [] ;
        fsnom = [] ;
        nsnom = [] ;
        pnom  = [] ;
        msgen = [] ;
        fsgen = [] ;
        pgen  = [] ;
        msdat = [] ;
        fsacc = [] ;
        msins = [] ;
        fsins = [] ;
        pins  = [] ;
        msprep= [] ;
      } ;
      a=a ;
      anim=anim
    } ;

  that_forms = {
    msnom, sm = "тот" ;
    fsnom, sf = "та" ;
    nsnom, sn = "то" ;
    pnom, sp = "те" ;
    msgen = "того" ;
    fsgen = "той" ;
    pgen  = "тех" ;
    msdat = "тому" ;
    fsacc = "ту" ;
    msins = "тем" ;
    fsins = "той" ;
    pins  = "тех" ;
    msprep = "том" ;
    preferShort = PrefFull ;
    comp = []
    } ;

  this_forms = {
    msnom, sm = "этот" ;
    fsnom, sf = "эта" ;
    nsnom, sn = "это" ;
    pnom, sp = "эти" ;
    msgen = "этого" ;
    fsgen = "этой" ;
    pgen  = "этих" ;
    msdat = "этому" ;
    fsacc = "эту" ;
    msins = "этим" ;
    fsins = "этой" ;
    pins  = "этими" ;
    msprep = "этом" ;
    preferShort = PrefFull ;
    comp = []
    } ;

  a_forms = { -- this pronoun is an approximate translation of indef article; preventing DetNP parsing problems
    msnom, sm = "некий" ;
    fsnom, sf = "некая" ;
    nsnom, sn = "некое" ;
    pnom, sp = "некие" ;
    msgen = "некого" ;
    fsgen = "некой" ;
    pgen  = "неких" ;
    msdat = "некому" ;
    fsacc = "некую" ;
    msins = "неким" ;
    fsins = "некой" ;
    pins  = "неким" ;
    msprep = "некой" ;
    preferShort = PrefFull ;
    comp = []
    } ;

  a_Det = {
    s : DetTable = \\g => (adjFormsAdjective a_forms).s ! GSg g;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = Num1 ;
    } ;

  a_Pl_Det = {
    s : DetTable = \\g => (adjFormsAdjective a_forms).s ! GPl;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = NumAll ;
    } ;


  the_forms = { -- this pronoun is an approximate translation of def article; preventing DetNP parsing problems
    msnom, sm = "данный" ;
    fsnom, sf = "данная" ;
    nsnom, sn = "данное" ;
    pnom, sp = "данные" ;
    msgen = "данного" ;
    fsgen = "данной" ;
    pgen  = "данных" ;
    msdat = "данному" ;
    fsacc = "данную" ;
    msins = "данным" ;
    fsins = "данной" ;
    pins  = "данных" ;
    msprep = "данном" ;
    preferShort = PrefFull ;
    comp = []
    } ;

  the_Det = {
    s : DetTable = \\g => (adjFormsAdjective the_forms).s ! GSg g;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = Num1 ;
    } ;

  the_Pl_Det = {
    s : DetTable = \\g => (adjFormsAdjective the_forms).s ! GPl;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = NumAll ;
    } ;

---------------
-- Numerals -- Числительные
---------------

param DForm = unit | teen | ten | hund ;
param Place = attr | indep ;
oper
  mille : Noun = nounFormsNoun ((guessNounForms "тысяча") ** {sins=variants {"тысячей" ; "тысячью"}});

---------------
-- Adverbs -- Наречия

  Adverb = { s : Str ; } ;

  makeAdverb : Str -> Adverb
    = \word -> {s=word} ;

--------------------------------
-- combining nouns with numerals

oper
  DetTable = Gender => Animacy => Case => Str ;

  NumDet : Type = {
    s : DetTable ;
    size : NumSize
    } ;

  -- choose number, force limited number if necessary
  forceMaybeNum : MaybeNumber -> Number -> Number
    = \mbn,n -> fromMaybe Number n mbn;

  -- Number from size to be used in agreement after numeral has been applied
  numSizeNumber : NumSize -> Number
    = \ns -> case ns of {Num1 => Sg ; NumAll | Num2_4 | Num5 => Pl} ;

  -- The following two used in tandem to form the word, controlled by numeral
  numSizeNum : Case -> NumSize -> Number
    = \cas,ns -> case <cas,ns> of {
      <Nom|Acc,Num2_4> => Sg ;
      <_,Num1> => Sg ;
      _ => Pl
      } ;
  numSizeCase : Case -> NumSize -> Case
    = \cas,ns -> case <cas,ns> of {
      <Nom,Num1 | NumAll> => Nom ;
      <Nom,Num2_4 | Num5> => Gen ;
      <Acc,Num1 | NumAll> => Nom ;
      <Acc,Num2_4 | Num5> => Gen ;
      _ => cas
      } ;
  numSizeGenAgr : NumSize -> Gender -> Person -> Agr
    = \ns,g,p -> Ag (case ns of {Num1 => GSg g ; NumAll | Num2_4 | Num5 => GPl}) p ;

----------------
-- Misc

oper
  ComplementCase : Type = {s : Str ; c : Case ; hasPrep : Bool} ;

  applyPrep : ComplementCase -> NounPhrase -> Str
    = \prep,np -> case <np.pron, prep.hasPrep, prep.c> of {
      <True, True, Gen|Dat|Acc|Ins|Ptv> => prep.s ++ "н" ++ BIND ++ (np.s ! prep.c) ;
      _ => prep.s ++ np.s ! prep.c
    } ;

  applyIPronPrep : ComplementCase -> IPronounForms -> Str
    = \prep,ip -> prep.s ++ selectIPronCase ip prep.c ;

}
