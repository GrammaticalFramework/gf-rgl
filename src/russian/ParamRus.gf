resource ParamRus = ParamX - [mkAdV, AdV], CommonX [Temp] ** open Prelude, Maybe in {
  -- Mostly follows https://en.wikipedia.org/wiki/List_of_glossing_abbreviations
  -- see theory.txt

oper
  consonant : pattern Str = #("б"|"в"|"г"|"д"|"ж"|"з"|"й"|"к"|"л"|"м"|"н"|"п"|"р"|"с"|"т"|"ф"|"х"|"ц"|"ч"|"ш"|"щ") ;
  consonant_minus : pattern Str = #("б"|"в"|"г"|"д"|"з"|"й"|"к"|"л"|"м"|"н"|"п"|"р"|"с"|"т"|"ф"|"х") ; -- шжчщц
  vowel : pattern Str = #("а"|"е"|"ё"|"и"|"о"|"у"|"ы"|"э"|"ю"|"я") ;
  vowel_but_i : pattern Str = #("а"|"е"|"ё"|"о"|"у"|"ы"|"э"|"ю"|"я") ;
  digit : pattern Str = #("0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9") ;
  small_num : pattern Str = #("1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"10"|"11"|"12"|"13"|"14"|"15"|"16") ;
  stress_schema : pattern Str = #("a'"|"a"|"b'"|"b"|"c''"|"c'"|"c"|"d'"|"d"|"e"|"f''"|"f'"|"f") ;
  adj_stress_schema : pattern Str = #("b/c''"|"a/c''"|"a/b'"|"a/c'"|"b/a'"|"b/b'"|"b/c'"|"b/c"|"b/a"|"b/b"|"a/a'"|"a/a"|"a/c"|"a/b"|"a'"|"b'"|"a"|"b"|"c") ;
  verb_stress_schema : pattern Str = #("a/c'"|"b/c'"|"c/c'"|"a/c"|"b/c"|"c/c"|"a/b"|"a/c"|"b/b"|"b/c"|"c/b"|"c/c"|"c'"|"a"|"b"|"c") ;

param
  Gender        = Masc | Fem | Neut ;  -- род
  Animacy       = Animate | Inanimate ;  -- одушевлённый / неодушевлённый
  Voice         = Act | Pass ;  -- залог
  Aspect        = Imperfective | Perfective ;  -- вид / аспект
  Reflexivity   = Reflexive | NonReflexive ;  -- возвратность -- keep just for the API
  Transitivity  = Transitive | Intransitive ;  -- возвратность
  ReflTran      = Refl | Trans | Intrans ;  -- this is what's inside VerbForms
  Mood          = Infinitive | Sbjv | Imperative | Ind ;  -- SBJV and COND will be treated as same for now

  GenNum   = GSg Gender | GPl ; -- The plural never makes a gender distinction
  NumSize  = Num1 | NumAll | Num2_4 | Num5 ;   -- Num1 - Sg, NumAll - Pl, Num2_4 - "Dual"
  Agr      = Ag GenNum Person ;

  Case     = Nom | Gen | Dat | Acc | Ins | Pre  -- падеж, "малые падежи":
            | Loc | Ptv | VocRus ;  -- "minor cases", usually Loc = Pre, Ptv = Gen, VocRus = Nom
  ShortFormPreference = PrefShort | PreferFull ;
  CopulaType = NomCopula | InsCopula | EllCopula | ExplicitCopula ;
  SpecialFuture = NormalFuture | BeFuture | BeFuture2 | CanFuture | WantFuture | NullFuture ;
  DetType = NormalDet | EmptyDef | EmptyIndef ; -- artificial parameter to side-step DetNP parsing issues
  NRelType = GenType | AdjType ;
  AForm ;

oper
  MaybeAgr = Maybe Agr ;
  MaybeNumber = Maybe Number ;
  JustSg = Just Number Sg ;
  JustPl = Just Number Pl ;
  BothSgPl = Nothing Number Sg ;  -- Both are possible, nothing limited
  -- GenNum helpers and coercions
  MSg        = GSg Masc ;
  FSg        = GSg Fem ;
  NSg        = GSg Neut ;
  gennum : Gender -> Number -> GenNum
    = \g,n -> case n of {Sg => GSg g ; Pl => GPl} ;
  numGenNum : GenNum -> Number
    = \gn -> case gn of {GSg _ => Sg ; GPl => Pl} ;
  genGenNum : GenNum -> Gender
    = \gn -> case gn of {GSg x => x ; GPl => Neut} ;
  agrGenNum : Agr -> GenNum
    = \a -> case a of {Ag gn _ => gn} ;
  genNumAgrP3 : GenNum -> Agr
    = \gn -> Ag gn P3 ;

  DeclType     = Predef.Ints 8 ;        -- Declension type

  NounFormsBase : Type = {
    snom, sgen, sdat, sacc, sins, sprep,
    pnom, pgen, pdat, pacc, pins, pprep : Str ;
    g : Gender ;
    anim : Animacy ;
    rel : AdjForms ;
    rt : NRelType ;
  } ;

  AdjForms : Type = {
    msnom, fsnom, nsnom, pnom,  -- pvoc = pnom
    msgen, fsgen, pgen,         -- nsgen = msgen ; ploc = pprep = pgen = pptv (?)
    msdat,                      -- nsdat = msdat ; fsdat = fsgen
    fsacc,                      -- amsacc = msgen, imsacc = msnom, nsacc = nsnom
    msins, fsins, pins,         -- nsins = msins, pdat = msins ; there is also variant fsins == fsgen
    msprep,                     -- nsprep = msprep, fsprep = fsgen, msloc = msprep
    sm, sf, sn, sp,             -- short forms
    comp                        -- comparative variants
    : Str ;
    p : Bool ;
    preferShort : ShortFormPreference
  } ;

  PronForms : Type = {
    msnom, fsnom, nsnom, pnom,  -- pvoc = pnom
    msgen, fsgen, pgen,         -- nsgen = msgen = msptv = nsptv; fsgen = fsptv; ploc = pprep = pgen = pptv
    msdat,                      -- nsdat = msdat, fsdat = fsgen
    fsacc,                      -- amsacc = msgen, imsacc = msnom, nsacc = nsnom, pacc = pgen
    msins, fsins, pins,         -- nsins = msins, pdat = msins ; there is also variant fsins == fsgen
    msprep                      -- nsprep = msprep, fsprep = fsgen, msloc = msprep
                                -- unlike adjective forms, short forms are not here
    : Str ;
  } ;

  ConjType     = Predef.Ints 16 ;        -- Conjugation type
  TempParts = {p1: Str; p2: Str} ;
  VerbForms : Type = {
    inf, infrefl,
    prsg1, prsg2, prsg3, prpl1, prpl2, prpl3,
    psgm, psgs,
    isg2, ipl1, isg2refl,
    ppps,   -- past passive participle, stem
    pppss,   -- past passive participle, short stem
    prtr, ptr  -- present and past transgressives (converbs)
    : Str ;
    fut : SpecialFuture ;
    asp : Aspect ;
    refltran : ReflTran ;
    } ;
  ComplementCase : Type = {s : Str ; c : Case ; hasPrep : Bool} ;
  VerbForms2 : Type = VerbForms ** {c : ComplementCase} ;
  VerbForms3 : Type = VerbForms ** {c : ComplementCase ; c2 : ComplementCase} ;

  reflTran : Reflexivity -> Transitivity -> ReflTran = \r,t ->
    case <r,t> of {
      <Reflexive,_> => Refl ;
      <_,Transitive> => Trans ;
      <_,Intransitive> => Intrans
    } ;

  -- Whether the noun that the preposition governs turns genitive in negative context
  -- NB. I'm just guessing here, this is an exercise in shaving concrete categories. /IL
  neggen : ComplementCase -> Bool = \c -> case c.c of {
    Nom|Acc => True ;
    _       => False
    } ;
}
