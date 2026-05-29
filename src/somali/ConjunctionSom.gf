concrete ConjunctionSom of Conjunction =
  CatSom ** open ResSom, Coordination, Prelude in {

  flags optimize=all_subs ;

    {- Conjunction for category X needs four things:
       lincat [X]
       lin BaseX
       lin ConsX
       lin ConjX

    For example, if X is defined as

      lincat X   = {s     : Number => Str ;   g : Gender} ;

    then [X] will split its s field into two, and retain its other fields as is:

      lincat [X] = {s1,s2 : Number => Str ;   g : Gender} ;

    Let us look at a simple case: Adv is of type {s : Str}
    Then [Adv] is {s1,s2 : Str}.
    BaseAdv, ConsAdv and ConjAdv can all use functions defined in prelude/Coordination:

      BaseAdv = twoSS ;
      ConsAdv = consrSS comma ;
      ConjAdv = conjunctSS ;

    --}


-- Adverbs have language-specific fields, so lists keep only their
-- realized strings and rebuild a plain adverb at conjunction time.
lincat
  [Adv],[AdV] = {s1,s2 : Str} ;
  [IAdv] = {s1 : Str; s2 : IAdv} ;

lin
  BaseAdv x y = {s1 = linAdv x ; s2 = linAdv y} ;
  ConsAdv x xs = xs ** {s1 = linAdv x ++ comma ++ xs.s1} ;
  ConjAdv co xs = conjAdv (co.s1 ++ xs.s1 ++ co.s2 ! Indefinite ++ xs.s2) ;

  BaseAdV x y = {s1 = x.s ; s2 = y.s} ;
  ConsAdV x xs = xs ** {s1 = x.s ++ comma ++ xs.s1} ;
  ConjAdV co xs = {s = co.s1 ++ xs.s1 ++ co.s2 ! Indefinite ++ xs.s2} ;

  BaseIAdv x y = {s1 = x.s ; s2 = y} ;
  ConsIAdv x xs = xs ** {s1 = x.s ++ comma ++ xs.s1} ;
  ConjIAdv co xs = xs.s2 ** {
    s = co.s1 ++ xs.s1 ++ co.s2 ! Indefinite ++ xs.s2.s ;
    berri = co.s1 ++ xs.s1 ++ co.s2 ! Indefinite ++ xs.s2.s
    } ;


-- RS depends on state, gender and case, otherwise exactly like previous.
-- RS can modify CNs, which are open for state, number and case, and have inherent gender.
lincat
  [RS] = {s1,s2 : State => GenNum => Case => Str} ;

lin
  BaseRS = twoTable3 State GenNum Case ;
  ConsRS = consrTable3 State GenNum Case comma ;
  ConjRS = conjunctRSTable ;

lincat
  [S] = {s1,s2 : Bool => Str} ;

lin
  BaseS = twoTable Bool ;
  ConsS x xs =
    consrTable Bool comma x xs ;
  ConjS co xs = conjunctDistrTable' Bool co xs ;

lincat
  [AP] = {s1,s2 : AForm => Str ; compar : Str} ;

lin
  BaseAP x y = twoTable AForm x y ** {compar = y.compar} ;
  ConsAP x xs = consrTable AForm comma x xs ** {compar = xs.compar} ;
  ConjAP co xs = {
    s = \\af => co.s1 ++ xs.s1 ! af ++ co.s2 ! Indefinite ++ xs.s2 ! af ;
    compar = xs.compar
    } ;

lincat
  [CN] = {s1,s2 : Number => Case => Str ; cn : CNoun} ;

lin
  BaseCN x y = {
    s1 = \\n,c => cn2str n c x ;
    s2 = \\n,c => cn2str n c y ;
    cn = y
    } ;
  ConsCN x xs = xs ** {
    s1 = \\n,c => cn2str n c x ++ "," ++ xs.s1 ! n ! c
    } ;
  ConjCN co xs = xs.cn ** {
    s = \\nf =>
      let n = case nf of {
                Def n => n ;
                Indef n => n ;
                _ => Sg } ;
       in co.s1 ++ xs.s1 ! n ! Abs ++ co.s2 ! Indefinite ++ xs.s2 ! n ! Abs ;
    mod = \\_,_,_ => [] ;
    modtype = NoMod
    } ;

lincat
  [DAP] = {s1,s2 : Gender => Case => Str ; det : Determiner} ;

lin
  BaseDAP x y = {
    s1 = x.sp ;
    s2 = y.sp ;
    det = y
    } ;
  ConsDAP x xs = xs ** {
    s1 = \\g,c => x.sp ! g ! c ++ "," ++ xs.s1 ! g ! c
    } ;
  ConjDAP co xs = xs.det ** {
    sp = \\g,c => co.s1 ++ xs.s1 ! g ! c ++ co.s2 ! Indefinite ++ xs.s2 ! g ! c
    } ;

-- Noun phrases
lincat
  [NP] = {s1,s2 : Case => Str} ** BaseNP ;

lin
  BaseNP x y =
    let x' = np2objpron x ;
        y' = np2objpron y
     in twoTable Case x' y' ** consNP x' y' ;
  ConsNP x xs =
    let x' = np2objpron x
     in consrTable Case comma x' xs ** consNP x' xs ;
  ConjNP conj xs = conjunctNPTable conj xs ** conjNP xs conj ;

oper

  ConjDistr : Type = {s2 : State => Str ; s1 : Str} ;

  conjAdv : Str -> Adverb = \s -> {
    berri = s ;
    c2 = NoAdp ;
    np = {s = [] ; a = ZeroObj} ;
    sii,dhex,miscAdv = []
    } ;

  conjunctDistrSS : ConjDistr -> ListX -> SS = \or,xs ->
    ss (or.s1 ++ xs.s1 ++ or.s2 ! Indefinite ++ xs.s2) ;

  conjunctDistrTable' :
    (P : PType) -> ConjDistr -> ListTable P -> {s : P => Str} = \P,or,xs ->
    {s = table P {p => or.s1 ++ xs.s1 ! p ++ or.s2 ! Indefinite ++ xs.s2 ! p}} ;

  conjunctDistrTable2' :
    (P,Q : PType) -> ConjDistr -> ListTable2 P Q -> {s : P => Q => Str} =
    \P,Q,or,xs ->
    {s =
    table P {p => table Q {q => or.s1 ++ xs.s1 ! p ! q ++ or.s2 ! Indefinite ++ xs.s2 ! p ! q}}} ;

 -- Like conjunctTable from prelude/Coordination.gf,
 -- but forces the first argument into absolutive.
  conjunctNPTable : ConjDistr -> ({s1,s2 : Case => Str} ** BaseNP) -> NP = \co,xs -> lin NP (xs ** {
    s = \\c => co.s1 ++ xs.s1 ! Abs ++ co.s2 ! xs.st ++ xs.s2 ! c
    }) ;

  conjunctRSTable : ConjDistr -> {s1,s2 : State => GenNum => Case => Str} -> RS = \co,xs -> lin RS (xs ** {
    s = \\st,g,c => co.s1
                ++ xs.s1 ! st ! g ! c
                ++ co.s2 ! st
                ++ xs.s2 ! st ! g ! c
    }) ;

  np2objpron : NounPhrase -> NounPhrase = \np -> np ** {
    s = objpron np
    } ;

  consNP : BaseNP -> BaseNP -> BaseNP = \x,y ->
    x ** { a = conjAgr x.a (getNum y.a) } ;

  conjNP : BaseNP -> Conj -> BaseNP = \xs,conj ->
    xs ** { a = conjAgr xs.a conj.n } ;

  conjAgr : Agreement -> Number -> Agreement = \a,n ->
    case n of { Pl => ResSom.plAgr a ; _  => a } ;

  conjNbr : Number -> Number -> Number = \n,m ->
    case n of { Pl => Pl ; _ => m } ;
}
