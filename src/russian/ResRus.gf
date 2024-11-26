--# -path=.:../abstract:../common
resource ResRus = ParamRus ** open Prelude, InflectionRus, Maybe in {
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
    anim : Animacy ;
    rel : AdjForms ;
    rt : NRelType ;
  } ;
  Noun2Forms = NounForms ** {c2 : ComplementCase} ;
  Noun3Forms = NounForms ** {c2,c3 : ComplementCase} ;

-- But traditional tables make agreement easier to handle in syntax
-- so this is the lincat of CN

  Noun : Type = {
    s : Number => Case => Str ;
    g : Gender ;
    mayben : MaybeNumber ;  -- used to control dependent words
    anim : Animacy ;
    rel : AdjForms ;
    rt : NRelType ;
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
      anim = forms.anim ;
      rel = forms.rel ;
      rt = forms.rt ;
    } ;

  guessNounForms : Str -> AdjForms -> NRelType -> NounForms
    = \word, rel, rt ->
    let nfb : NounFormsBase =
    case word of {
      _ + "уть"                            => makeNoun word Masc Inanimate rel rt (ZN 8 No B NoC) ;
      _ + "ий"                             => makeNoun word Masc Inanimate rel rt (ZN 7 No A NoC) ;
      _ + "ия"                             => makeNoun word Fem Inanimate rel rt (ZN 7 No A NoC) ;
      _ + "ие"                             => makeNoun word Neut Inanimate rel rt (ZN 7 No A NoC) ;
      _ + "ье"                             => makeNoun word Neut Inanimate rel rt (ZN 6 Ast A NoC) ;
      _ + "тель"                           => makeNoun word Masc Inanimate rel rt (ZN 2 No A NoC) ;
      _ + "ь"                              => makeNoun word Fem Inanimate rel rt (ZN 8 No A NoC) ;
      _ + "и"                              => makeNoun word Neut Inanimate rel rt ZN0 ;
      _ + #consonant + ("к"|"х"|"г") + "а" => makeNoun word Fem Inanimate rel rt (ZN 3 Ast A NoC) ;
      _ + ("к" | "х" | "г")                => makeNoun word Masc Inanimate rel rt (ZN 3 No A NoC) ;
      _ + ("к" | "х" | "г") + "а"          => makeNoun word Fem Inanimate rel rt (ZN 3 No A NoC) ;
      _ + "ца"                             => makeNoun word Fem Animate rel rt (ZN 5 No A NoC) ;
      _ + "й"                              => makeNoun word Masc Inanimate rel rt (ZN 6 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ")          => makeNoun word Masc Inanimate rel rt (ZN 4 No A NoC) ;
      _ + "ша"                             => makeNoun word Fem Animate rel rt (ZN 4 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ") + "а"    => makeNoun word Fem Inanimate rel rt (ZN 4 No A NoC) ;
      _ + "ц"                              => makeNoun word Masc Inanimate rel rt (ZN 5 Ast A NoC) ;
      _ + "о"                              => makeNoun word Neut Inanimate rel rt (ZN 1 No A NoC) ;
      _ + "а"                              => makeNoun word Fem Inanimate rel rt (ZN 1 No A NoC) ;
      _                                    => makeNoun word Masc Inanimate rel rt (ZN 1 No A NoC)
    } in
    noMinorCases nfb ;

  guessLessNounForms : Str -> Gender -> Animacy -> AdjForms -> NRelType -> NounForms
    = \word, g, anim, rel, rt ->
    let nfb : NounFormsBase =
    case word of {
      _ + "уть"                            => makeNoun word g anim rel rt (ZN 8 No B NoC) ;
      _ + "ий"                             => makeNoun word g anim rel rt (ZN 7 No A NoC) ;
      _ + "ия"                             => makeNoun word g anim rel rt (ZN 7 No A NoC) ;
      _ + "ие"                             => makeNoun word g anim rel rt (ZN 7 No A NoC) ;
      _ + "ье"                             => makeNoun word g anim rel rt (ZN 6 Ast A NoC) ;
      _ + "тель"                           => makeNoun word g anim rel rt (ZN 2 No A NoC) ;
      _ + "ь"                              => makeNoun word g anim rel rt
                                               (case g of {Fem => (ZN 8 No A NoC); _ => (ZN 2 No A NoC)});
      _ + "и"                              => makeNoun word g anim rel rt ZN0 ;
      _ + #consonant + ("к"|"х"|"г") + "а" => makeNoun word g anim rel rt (ZN 3 Ast A NoC) ;
      _ + ("к" | "х" | "г")                => makeNoun word g anim rel rt (ZN 3 No A NoC) ;
      _ + ("к" | "х" | "г") + "а"          => makeNoun word g anim rel rt (ZN 3 No A NoC) ;
      _ + "ца"                             => makeNoun word g anim rel rt (ZN 5 No A NoC) ;
      _ + "й"                              => makeNoun word g anim rel rt (ZN 6 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ")          => makeNoun word g anim rel rt (ZN 4 No A NoC) ;
      _ + "ша"                             => makeNoun word g anim rel rt (ZN 4 No A NoC) ;
      _ + ("ж" | "ш" | "ч" | "щ") + "а"    => makeNoun word g anim rel rt (ZN 4 No A NoC) ;
      _ + "ц"                              => makeNoun word g anim rel rt (ZN 5 Ast A NoC) ;
      _ + "о"                              => makeNoun word g anim rel rt (ZN 1 No A NoC) ;
      _ + "а"                              => makeNoun word g anim rel rt (ZN 1 No A NoC) ;
      _                                    => makeNoun word g anim rel rt (ZN 1 No A NoC)
    } in
    noMinorCases nfb ;

  immutableNounForms : Str -> Gender -> Animacy -> NounForms
    = \s, g, anim -> noMinorCases (immutableNounCases s g anim ) ;

  noMinorCases : NounFormsBase -> NounForms
    = \base -> base ** {
      sloc = base.sprep ;
      sptv = base.sgen ;
      svoc = base.snom ;
      mayben = BothSgPl ;
      rt = base.rt ;
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

  orPol : Polarity -> Polarity -> Polarity = \p1,p2 ->
      case p1 of {
        Neg => Neg;
        Pos => p2
      } ;

  AgrTable = Agr => Str ;
  ComplTable = Polarity => Agr => Str ;
  PolarityTable = Polarity => Str ;

  from2 = {s="из" ; c=Gen ; neggen=True ; hasPrep=True} ;

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
        pprep = n1.pprep ++ l ++ n2.pprep ;
      } ;


---------------------------
-- Adjectives -- Прилагательные

  AdjTable = GenNum => Animacy => Case => Str ;

  Adjective : Type = {
    s : AdjTable ;
    short : AgrTable ;
    preferShort : ShortFormPreference
    } ;

  pronToAdj : PronForms -> AdjForms
    = \base -> base ** {
      sm = base.msnom ; -- these are incorrect, but empty causes parsing problems
      sf = base.fsnom ;
      sn = base.nsnom ;
      sp = base.pnom ;
      comp = base.nsnom ;
      preferShort = PreferFull ;
      p = False
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

  doGuessAdjectiveForms : Str -> AdjForms
    = \word -> case word of {
      _ + "шеий"                 => makeAdjective word (ZA 6 No A_ NoC) PreferFull ;
      _ + "цый"                  => makeAdjective word (ZA 5 No A_ NoC) PreferFull ;
      _ + ("к"|"г"|"х") +"ий"    => makeAdjective word (ZA 3 No A_ NoC) PreferFull ;
      _ + ("ш"|"ж"|"ч"|"щ")+"ий" => makeAdjective word (ZA 4 No A_ NoC) PreferFull ;
      _ + #consonant + "ный"     => makeAdjective word (ZA 1 Ast A_ NoC) PreferFull ;
      _ + #consonant + "ний"     => makeAdjective word (ZA 2 Ast A_ NoC) PreferFull ;
      _ + "ый"                   => makeAdjective word (ZA 1 No A_ NoC) PreferFull ;
      _ + "ой"                   => makeAdjective word (ZA 1 No B_ NoC) PreferFull ;
      _ + "ий"                   => makeAdjective word (ZA 2 No A_ NoC) PreferFull ;
      _                          => makeAdjective word (ZA 1 No A_ NoC) PreferFull
      } ;

  guessAdjectiveForms : Str -> AdjForms
    = \word -> case word of {
      s + "ся" => appendToAF (doGuessAdjectiveForms s) "ся" ;
      _        => doGuessAdjectiveForms word
      } ;

  doMakeAdjectiveForms : Str -> Str -> ZAIndex -> ShortFormPreference -> AdjForms
    = \nom, comp, zi, spf ->
      let af = makeAdjective nom zi spf in
      let comp' = case (Predef.length comp) of {0 => af.comp; _ => comp} in
      af ** {comp=comp'} ;

  makeAdjectiveForms : Str -> Str -> Str -> ShortFormPreference -> AdjForms
    = \nom, comp, zi_str, spf ->
      let zi = parseAdjIndex zi_str in case nom of {
        s + "ся" => appendToAF (doMakeAdjectiveForms s comp zi spf) "ся" ;
        _ => doMakeAdjectiveForms nom comp zi spf
        } ;

  makeAdjectiveFormsUseIndex : Str -> Str -> ZAIndex -> ShortFormPreference -> AdjForms
    = \nom, comp, zi, spf -> case nom of {
      s + "ся" => appendToAF (doMakeAdjectiveForms s comp zi spf) "ся" ;
      _ => doMakeAdjectiveForms nom comp zi spf
      } ;

  makeAdjectiveFromNoun : Noun -> Adjective
    = \n -> {
       s = \\gn,anim,cas=> n.s ! numGenNum gn ! cas ;
       short=\\a=> [] ;
       preferShort=PreferFull
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
      preferShort = PreferFull ;
      p = af.p
    } ;

  ord_long_superlative : PronForms -> AdjForms -> AdjForms
    = \pf,af -> {
      msnom = pf.msnom ++ the_most.msnom  ++ af.msnom ;
      fsnom = pf.fsnom ++ the_most.fsnom  ++ af.fsnom ;
      nsnom = pf.nsnom ++ the_most.nsnom  ++ af.nsnom ;
      pnom  = pf.pnom  ++ the_most.pnom   ++ af.pnom  ;
      msgen = pf.msgen ++ the_most.msgen  ++ af.msgen ;
      fsgen = pf.fsgen ++ the_most.fsgen  ++ af.fsgen ;
      pgen  = pf.pgen  ++ the_most.pgen   ++ af.pgen  ;
      msdat = pf.msdat ++ the_most.msdat  ++ af.msdat ;
      fsacc = pf.fsacc ++ the_most.fsacc  ++ af.fsacc ;
      msins = pf.msins ++ the_most.msins  ++ af.msins ;
      fsins = pf.fsins ++ the_most.fsins  ++ af.fsins ;
      pins  = pf.pins  ++ the_most.pins   ++ af.pins  ;
      msprep= pf.msprep++ the_most.msprep ++ af.msprep;
      sm    = pf.msnom ++ the_most.sm     ++ af.sm    ;
      sf    = pf.fsnom ++ the_most.sf     ++ af.sf    ;
      sn    = pf.nsnom ++ the_most.sn     ++ af.sn    ;
      sp    = pf.pnom  ++ the_most.sp     ++ af.sp    ;
      comp  = pf.msnom ++ the_most.comp   ++ af.comp  ;
      g=af.g ;
      preferShort = PreferFull ;
      p = af.p
    } ;

  prependPF : Str -> PronForms -> PronForms
    = \s,pf -> {
      msnom = s ++ pf.msnom ;
      fsnom = s ++ pf.fsnom ;
      nsnom = s ++ pf.nsnom ;
      pnom  = s ++ pf.pnom  ;
      msgen = s ++ pf.msgen ;
      fsgen = s ++ pf.fsgen ;
      pgen  = s ++ pf.pgen  ;
      msdat = s ++ pf.msdat ;
      fsacc = s ++ pf.fsacc ;
      msins = s ++ pf.msins ;
      fsins = s ++ pf.fsins ;
      pins  = s ++ pf.pins  ;
      msprep= s ++ pf.msprep
    } ;

  appendToAF : AdjForms -> Str -> AdjForms
    = \af,s -> af ** {
      msnom = af.msnom ++ BIND ++ s;
      fsnom = af.fsnom ++ BIND ++ s;
      nsnom = af.nsnom ++ BIND ++ s;
      pnom  = af.pnom ++ BIND ++ s;
      msgen = af.msgen ++ BIND ++ s;
      fsgen = af.fsgen ++ BIND ++ s;
      pgen  = af.pgen ++ BIND ++ s;
      msdat = af.msdat ++ BIND ++ s;
      fsacc = af.fsacc ++ BIND ++ s;
      msins = af.msins ++ BIND ++ s;
      fsins = af.fsins ++ BIND ++ s;
      pins  = af.pins ++ BIND ++ s;
      msprep= af.msprep ++ BIND ++ s;
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
          rel = af ;
          rt = GenType ;
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
          rel = af ;
          rt = GenType ;
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
          rel = af ;
          rt = GenType ;
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

  mkCompoundA : AdjForms -> Str -> AdjForms -> AdjForms
    = \a1,link,a2 ->
      let l : Str=case link of {x+"-" => BIND ++ "-" ++ BIND ; _ => link} in
      a2 ** {
      msnom = a1.sn ++ l ++ a2.msnom ;
      fsnom = a1.sn ++ l ++ a2.fsnom ;
      nsnom = a1.sn ++ l ++ a2.nsnom ;
      pnom  = a1.sn ++ l ++ a2.pnom  ;
      msgen = a1.sn ++ l ++ a2.msgen ;
      fsgen = a1.sn ++ l ++ a2.fsgen ;
      pgen  = a1.sn ++ l ++ a2.pgen  ;
      msdat = a1.sn ++ l ++ a2.msdat ;
      fsacc = a1.sn ++ l ++ a2.fsacc ;
      msins = a1.sn ++ l ++ a2.msins ;
      fsins = a1.sn ++ l ++ a2.fsins ;
      pins  = a1.sn ++ l ++ a2.pins  ;
      msprep= a1.sn ++ l ++ a2.msprep;
      sm    = a1.sn ++ l ++ a2.sm    ;
      sf    = a1.sn ++ l ++ a2.sf    ;
      sn    = a1.sn ++ l ++ a2.sn    ;
      sp    = a1.sn ++ l ++ a2.sp    ;
      comp  = a1.sn ++ l ++ a2.comp  ;
      } ;



---------------------
-- Verbs -- Глаголы

-- Note 1. Passive voice can be formed only for transitive imperfective verbs
-- Passive has no P1, P2, imperative,
-- Reflexive verbs are to provides as as separate lexical entries.
-- Note 2. Imperative Sg P2 of reflexive verbs, can be сь as well as ся, but because there is no passive forms
-- we can store the sya-schema and 'BIND++' as necessary.

oper
  VP : Type = {
    adv : AgrTable ;  -- modals are in position of adverbials ones numgen gets fixed
    verb : ResRus.VerbForms ;
    dep : Str ;  -- dependent infinitives and such
    compl : ComplTable ;
    p : Polarity
    } ;

  VPSlash = {
    adv : AgrTable ;  -- modals are in position of adverbials ones numgen gets fixed
    verb : VerbForms ;
    dep : Str ;  -- dependent infinitives and such
    compl1 : ComplTable ;
    compl2 : ComplTable ;
    c : ComplementCase ;
    isSimple : Bool ;    -- regulates the place of participle used as adjective
    p : Polarity
    } ; ----

  slashV : VerbForms -> ComplementCase -> VPSlash = \verb,c -> {
      verb     = verb ;
      adv      = \\a => [];
      compl1   = \\_,a => [] ;
      compl2   = \\_,a => [] ;
      dep      = [] ;
      c        = c ;
      p        = Pos ;
      isSimple = True
    } ;

  insertSlashObjA : Adjective -> ComplementCase -> VPSlash -> VPSlash = \ap,c,slash -> {
      verb     = slash.verb ;
      adv     = slash.adv ;
      compl1 = slash.compl1 ;
      compl2 = \\p,a => case p of {
           Pos => case ap.preferShort of {
             PreferFull => slash.compl2 ! p ! a ++ ap.s ! agrGenNum a ! Animate ! slash.c.c ;
             PrefShort => slash.compl2 ! p ! a ++ ap.short ! a
             } ;
           Neg => case ap.preferShort of {
             PreferFull => case neggen slash.c of {
                 False => slash.compl2 ! p ! a ++ ap.s ! agrGenNum a ! Animate ! slash.c.c ;
                 True  => slash.compl2 ! p ! a ++ ap.s ! agrGenNum a ! Animate ! Gen
              } ;
             PrefShort => slash.compl2 ! p ! a ++ ap.short ! a
             }
           } ;
      c = {s="" ; c=Acc ; neggen=True ; hasPrep=False};
      dep = slash.dep ;
      isSimple = False ;
      p = slash.p
      } ;

  insertSlashObj1 : (Polarity => Agr => Str) -> ComplementCase -> VPSlash -> VPSlash = \obj,c,slash -> {
      verb     = slash.verb ;
      adv     = slash.adv;
      compl1 =\\p,a => slash.compl1 ! p ! a ++ obj ! p ! a;
      compl2 = slash.compl2 ;
      c     = slash.c ;
      dep = slash.dep ;
      isSimple = False ;
      p = slash.p
      } ;

   insertSlashObj2 : (Polarity => Agr => Str) -> ComplementCase -> VPSlash -> VPSlash = \obj,c,slash -> {
      verb     = slash.verb ;
      adv     = slash.adv;
      compl1 = slash.compl1 ;
      compl2 =\\p,a => slash.compl2 ! p ! a ++ obj ! p ! a;
      c     = slash.c ;
      dep = slash.dep ;
      isSimple = False ;
      p = slash.p
      } ;


  guessVerbForms : Aspect -> Transitivity -> Str -> Str -> Str -> VerbForms
    = \asp,tran,inf,sg1,sg3 ->
      let guessed : ZVIndex * Reflexivity = guessRegularIndex inf sg1 sg3 in
      let corr_tran = case guessed.p2 of {Reflexive=>Intransitive ; NonReflexive=>tran} in
      makeVerb inf sg1 sg3 guessed.p1 asp corr_tran guessed.p2 ;

  guessIrregularVerbForms : Aspect -> Transitivity -> Str -> VerbForms
    = \asp,tran,inf -> case inf of {
     s + ("есть"  |"есться") => makeVerbEst asp tran inf ;
     s + ("дать"  |"даться") => makeVerbDat6 asp tran inf ;
     s + ("хотеть"|"хотеться") => makeVerbKhotet6 asp tran inf ;
     s + ("бежать"|"бежаться") => makeVerbBezhat6 asp tran inf ;
     s + ("быть"  |"быться") => makeVerbByt6 asp tran inf ;
     "идти" => makeVerbJti asp tran inf "ё";
     s + ("выйти" |"выйтись") => makeVerbJti asp tran inf "е";
     s + ("йти" |"йтись") => makeVerbJti asp tran inf "ё";
      _ => let stem_info = infStemFromVerb inf in
        let stem = stem_info.p1 in
        guessVerbForms asp tran inf (stem+"ю") (stem+"ет")
    } ;

  mkVplus : VerbForms -> VerbForms
    = \vf -> vf ;
  mkV2plus : VerbForms2 -> VerbForms2
    = \vf -> vf ;
  mkV3plus : VerbForms3 -> VerbForms3
    = \vf -> vf ;

  quickGuessVerbForms : Str -> VerbForms
    = \inf ->
      let stem_info = infStemFromVerb inf in
      let stem = stem_info.p1 in
      guessVerbForms Imperfective Transitive inf (stem+"ю") (stem+"ет") ;

  passivateNonReflexive : VerbForms -> VerbForms
    = \vf -> vf ** {refltran=Refl} ;

  passivate : VerbForms -> VerbForms
    = \vf ->
      case vf.refltran of {
        Refl => vf ;
        _ => passivateNonReflexive vf
      } ;

  shortPastPassPart : VerbForms -> GenNum -> Str
    = \vf,gn ->
      case vf.refltran of {
        Trans => case <vf.fut,gn> of {
          <NormalFuture,GSg Masc> => vf.pppss ;
          <NormalFuture,GSg Fem> => vf.pppss ++ BIND ++ "а" ;
          <NormalFuture,GSg Neut> => vf.pppss ++ BIND ++ "о" ;
          <NormalFuture,GPl> => vf.pppss ++ BIND ++ "ы" ;
          _ => vf.pppss } ;
        _ => variants {}
        } ;

  copula : VerbForms
    = {
      inf="быть" ;
      infrefl="являться" ;  --?
      prsg1="—";
      prsg2="—";
      prsg3="—";
      prpl1="—";
      prpl2="—";
      prpl3="—";   -- also "суть"
      fut=BeFuture ;
      psgm="был";
      psgs="бы";
      isg2="будь";
      isg2refl="явись" ; -- ?
      ipl1="давайте будем";
      ppps="явленн";   --*
      pppss="явлен";   --*
      prtr="будучи";
      ptr="быв";
      asp=Imperfective;
      refltran = Intrans ; -- used to be refl=NonReflexive; tran=Intransitive
    } ;

  -- normal copula require Nom in Pres. So this is Ins-friendly substitute.
  -- TODO: Provide also Nom-based as idiomatic (?)
  copulaIns : VerbForms
    = copula ** {
      fut=BeFuture2 ;
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
      ppps=""; --*
      pppss=""; --*
      prtr="могши";  --*
      ptr="могши";
      asp=Imperfective;
      refltran = Intrans ; -- used to be refl=NonReflexive; tran=Intransitive
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
      ppps="хотим";  -- *
      pppss="хотим";  -- *
      prtr="хотя";
      ptr="хотев";
      asp=Imperfective;
      refltran = Trans ; -- used to be refl=NonReflexive; tran=Transitive
    } ;

  nullVerb : VerbForms
    = {
      inf, infrefl,
      prsg1, prsg2, prsg3, prpl1, prpl2, prpl3,
      psgm, psgs,
      isg2, isg2refl, ipl1,
      ppps, pppss,
      prtr, ptr ="";
      fut=NullFuture ;
      asp=Imperfective;
      refltran = Trans ; -- used to be refl=NonReflexive; tran=Transitive
    } ;

  verbPastAgree : VerbForms -> Agr -> Str -> Str
    = \vf,a,after -> case a of {
      Ag (GSg Masc) _ => vf.psgm ++ (verbReflAfterConsonant vf) ++ after ;
      Ag (GSg Fem) _ => vf.psgs ++ BIND ++ "ла" ++ (verbRefl vf) ++ after ;
      Ag (GSg Neut) _ => vf.psgs ++ BIND ++ "ло" ++ (verbRefl vf) ++ after ;
      Ag GPl _ => vf.psgs ++ BIND ++ "ли"++ (verbRefl vf) ++ after
    } ;

  verbReflAfterConsonant : VerbForms -> Str
    = \vf -> case vf.refltran of {Refl => BIND ++ "ся" ; _ => ""} ;

  verbRefl : VerbForms -> Str
    = \vf -> case vf.refltran of {Refl => BIND ++ "сь" ; _ => ""} ;

  verbInf : VerbForms -> Str
    = \vf -> case vf.refltran of {Refl => vf.infrefl ; _ => vf.inf} ;

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
      <BeFuture | BeFuture2,_> => beFuture a ;
      _ => case vf.asp of {
        Perfective => verbPresAgree vf a ;
        Imperfective => (beFuture a) ++ verbInf vf
        }
      } ;

  verbImperativeAgree : VerbForms -> Agr -> TempParts
    = \vf,a -> case a of {
      Ag (GSg Neut) (P1|P2|P3) => <"", (verbInf vf)> ;  -- reused neuter for immediate imperative
      Ag (GSg _) P1 => <"", (verbInf vf)> ;  -- ?
      Ag (GSg _) P2 => <"", case vf.refltran of {Refl=>vf.isg2refl; _=>vf.isg2}> ;
      Ag (GSg x) P3 => <"пусть", verbFutAgree vf (Ag (GSg x) P3)> ;  -- ?
      Ag GPl P1 => <"", vf.ipl1 ++ verbReflAfterConsonant vf> ;
      Ag GPl P2 => <"", vf.isg2 ++ BIND ++ "те" ++ (verbRefl vf)> ;
      Ag GPl P3 => <"пусть", verbFutAgree vf (Ag GPl P3)>
    } ;

  verbEnvAgr : Str -> Str -> VerbForms -> Mood -> Tense -> Agr -> Pol -> Str
    = \subj,adv,vf,m,temp,a,pol ->
      case vf.fut of {
        NullFuture => subj ++ pol.s ++ adv ;
        BeFuture => case <m,temp, pol.p> of {
          <Ind, Past, _> => subj ++ pol.s ++ adv ++ verbPastAgree vf a "" ;
          <Ind, Pres, Pos> => subj ++ pol.s ++ adv ++ verbPresAgree vf a ;
          <Ind, Pres, Neg> => subj ++ pol.s ++ adv ;
          <Ind, Fut, _> => subj ++ pol.s ++ adv ++ verbFutAgree vf a ;
          <Ind, Cond, _> => subj ++ pol.s ++ adv ++ verbPastAgree vf a "бы" ;
          <Sbjv, _, _> => subj ++ pol.s ++ adv ++ verbPastAgree vf a "бы" ;
          <Imperative, _, _> => let p = verbImperativeAgree vf a in p.p1 ++ subj ++ pol.s ++ adv ++ p.p2 ;
          <Infinitive, _, _> => subj ++ pol.s ++ adv ++  verbInf vf
          } ;
        _ => case <m,temp> of {
          <Ind, Past> => subj ++ adv ++ pol.s ++ verbPastAgree vf a "" ;
          <Ind, Pres> => subj ++ adv ++ pol.s ++ verbPresAgree vf a ;
          <Ind, Fut> => subj ++ adv ++ pol.s ++ verbFutAgree vf a ;
          <Ind, Cond> => subj ++ adv ++ pol.s ++ verbPastAgree vf a "бы" ;
          <Sbjv, _> => subj ++ adv ++ pol.s ++ verbPastAgree vf a "бы" ;
          <Imperative, _> => let p = verbImperativeAgree vf a in p.p1 ++ subj ++ adv ++ pol.s ++ p.p2 ;
          <Infinitive, _> => subj ++ adv ++ pol.s ++  verbInf vf
          }
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
          nPrefix = True ;
          poss = doPossessivePronP3 "их"
        }
      } ;

-- Possessive pronouns are more like adjectives

  mkP : Str -> Str -> PronForms
    = \msnom, zi ->
      case zi of {
        "6*a" => pronoun6AstA msnom ;
        "2*b" => pronoun2AstB msnom ;
        _ => pronoun1A msnom   -- add more when needed
      } ;

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
      anim=n.anim ;
      rel=n.rel ;
      rt =n.rt ;
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
    preferShort = PreferFull ;
    comp = [] ;
    p = False
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
    preferShort = PreferFull ;
    comp = [] ;
    p = False
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
    preferShort = PreferFull ;
    comp = [] ;
    p = False
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
    preferShort = PreferFull ;
    comp = [] ;
    p = False
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
  mille : Noun = nounFormsNoun ((guessNounForms "тысяча" (doGuessAdjectiveForms "тысячный") AdjType) ** {sins=variants {"тысячей" ; "тысячью"}});

  ith_forms : Str -> AdjForms
    = \s -> {
      msnom = s ++ BIND ++ "-й" ;  -- after vowel
      fsnom = s ++ BIND ++ "-я" ;
      nsnom = s ++ BIND ++ "-е" ;
      pnom  = s ++ BIND ++ "-е" ;
      msgen = s ++ BIND ++ "-го" ; -- after consonant
      fsgen = s ++ BIND ++ "-й" ;
      pgen  = s ++ BIND ++ "-х" ;
      msdat = s ++ BIND ++ "-му" ;
      fsacc = s ++ BIND ++ "-ю" ;
      msins = s ++ BIND ++ "-м" ;
      fsins = s ++ BIND ++ "-й" ;
      pins  = s ++ BIND ++ "-ми" ;
      msprep= s ++ BIND ++ "-м" ;
      sm, sf, sn, sp = s ;
      comp = s ++ BIND ++ "-е" ; --*
      p = False ;
      preferShort=PreferFull
    } ;

---------------
-- Adverbs -- Наречия

  Adverb = { s : Str ; } ;

  makeAdverb : Str -> Adverb
    = \word -> {s=word} ;


--------------------------------
-- combining nouns with numerals

oper
  DetTable = Gender => Animacy => Case => Str ;

  NumeralForms : Type = {
    s : DetTable ;
    o : PronForms ;
    size : NumSize
    } ;

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
  animNumSizeNum : Animacy -> Case -> NumSize -> Number
    = \anim,cas,ns -> case <anim,cas,ns> of {
      <Animate,Acc,Num2_4> => Pl ;
      <Inanimate,Acc,Num2_4> => Sg ;
      <_,Nom,Num2_4> => Sg ;
      <_,_,Num1> => Sg ;
      _ => Pl
      } ;
  numSizeCase : Case -> NumSize -> Case
    = \cas,ns -> case <cas,ns> of {
      <Nom,Num1 | NumAll> => Nom ;
      <Nom,Num2_4 | Num5> => Gen ;
      <Acc,Num2_4> => Gen ;
      <Acc,Num5> => Gen ;
      _ => cas
      } ;
  numSizeGenAgr : NumSize -> Gender -> Person -> Agr
    = \ns,g,p -> Ag (case ns of {Num1 => GSg g ; NumAll | Num2_4 | Num5 => GPl}) p ;

----------------
-- Misc

oper

  applyPrep : ComplementCase -> NounPhrase -> Str
    = \prep,np -> case <np.pron, prep.hasPrep, prep.c> of {
      <True, True, Gen|Dat|Acc|Ins|Ptv|Loc> => prep.s ++ "н" ++ BIND ++ (np.s ! prep.c) ;
      _ => prep.s ++ np.s ! prep.c
    } ;

  applyPolPrep : Polarity -> ComplementCase -> NounPhrase -> Str
    = \pol,prep,np ->
      let prep'=prep ** {
        c=case <pol, neggen prep> of {<Neg, True> => Gen ; _ => prep.c}
      } in applyPrep prep' np ;

  applyIPronPrep : ComplementCase -> IPronounForms -> Str
    = \prep,ip -> prep.s ++ selectIPronCase ip prep.c ;

  printNounInflections : NounForms -> Str = \nf ->
  nf.snom ++ "-"
    ++ nf.snom  ++ "," ++ nf.pnom ++ ","
    ++ nf.sgen  ++ "," ++ nf.pgen ++ ","
    ++ nf.sdat  ++ "," ++ nf.pdat ++ ","
    ++ nf.sacc  ++ "," ++ nf.pacc ++ ","
    ++ nf.sins  ++ "," ++ nf.pins ++ ","
    ++ nf.sprep ++ "," ++ nf.pprep ++ ","
    ++ nf.sloc  ++ "," ++ nf.sptv ;

  printAdjectiveInflections : AdjForms -> Str = \af ->
    let mf = makeNFFromAF af Masc Inanimate in
    let ff = makeNFFromAF af Fem Inanimate in
    let nf = makeNFFromAF af Neut Inanimate in
    let mfa = makeNFFromAF af Masc Animate in
    let ffa = makeNFFromAF af Fem Animate in
    let nfa = makeNFFromAF af Neut Animate in
    af.msnom ++ "-"
      ++ mf.snom  ++ "," ++ ff.snom  ++ "," ++ nf.snom  ++ "," ++ mf.pnom  ++ ","
      ++ mf.sgen  ++ "," ++ ff.sgen  ++ "," ++ nf.sgen  ++ "," ++ mf.pgen  ++ ","
      ++ mf.sdat  ++ "," ++ ff.sdat  ++ "," ++ nf.sdat  ++ "," ++ mf.pdat  ++ ","
      ++ mf.sacc  ++ "," ++ ff.sacc  ++ "," ++ nf.sacc  ++ "," ++ mf.pacc  ++ ","
      ++ mfa.sacc ++ "," ++ ffa.sacc ++ "," ++ nfa.sacc ++ "," ++ mfa.pacc ++ ","
      ++ mf.sins  ++ "," ++ ff.sins  ++ "," ++ nf.sins  ++ "," ++ mf.pins  ++ ","
      ++ mf.sprep ++ "," ++ ff.sprep ++ "," ++ nf.sprep ++ "," ++ mf.pprep ++ ","
      ++ af.sm    ++ "," ++ af.sf    ++ "," ++ af.sn    ++ "," ++ af.sp    ++ ","
      ++ af.comp
      ;

  printVerbInflections : VerbForms -> Str = \v ->
    let fut : Agr=>Str = \\a => verbFutAgree v a in
    let pres : Agr=>Str = \\a => verbPresAgree v a in
    let past : Agr=>Str = \\a => verbPastAgree v a "" in
    let imp : Agr=>Str = \\a => ((verbImperativeAgree v a).p1 ++ (verbImperativeAgree v a).p2) in
    let ppp : GenNum=>Str = \\gn => shortPastPassPart v gn in
    let inf = verbInf v in
    inf ++ "-"
      ++ inf ++ ","
      ++ pres ! Ag (GSg Masc) P1 ++ "," ++ pres ! Ag GPl P1 ++ ","
      ++ pres ! Ag (GSg Masc) P2 ++ "," ++ pres ! Ag GPl P2 ++ ","
      ++ pres ! Ag (GSg Masc) P3 ++ "," ++ pres ! Ag GPl P3 ++ ","
      ++ v.prtr ++ verbRefl v ++ ","
      ++ past ! Ag (GSg Masc) P1 ++ ","  ++ past ! Ag (GSg Fem) P1 ++ "," ++ past ! Ag (GSg Neut) P1 ++ "," ++ past ! Ag GPl P1 ++ ","
      ++ imp ! Ag (GSg Masc) P2 ++ "," ++ imp ! Ag GPl P2 ++ ","
      ++ v.ptr ++ verbRefl v
      ++ case v.refltran of {
        Trans => "," ++ ppp ! (GSg Masc) ++ "," ++ ppp ! (GSg Fem) ++ "," ++ ppp ! (GSg Neut) ++ "," ++ ppp ! GPl ;
        _ => ""
        } ;

}
