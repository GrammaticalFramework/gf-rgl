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


-- Adverb and other simple {s : Str} types.
lincat
  [Adv],[AdV],[IAdv] = {s1,s2 : Str} ;

lin
  BaseAdv, BaseAdV, BaseIAdv = twoSS ;
  ConsAdv, ConsAdV, ConsIAdv = consrSS comma ;
  ConjAdv, ConjAdV, ConjIAdv = conjunctDistrSS ;


--RS depends on gender and case, otherwise exactly like previous.
lincat
  [RS] = {s1,s2 : Gender => Case => Str} ;

lin
  BaseRS x y = twoTable2 Gender Case x y ;
  ConsRS xs x = consrTable2 Gender Case comma xs x ;
  ConjRS co xs = conjunctDistrTable2' Gender Case co xs ;

{-
lincat
  [S] = {} ;

lin
  BaseS x y = y ** { } ;
  ConsS x xs =
    xs ** { } ;
  ConjS co xs = {} ;

lincat
  [AP] = {} ;

lin
  BaseAP x y = twoTable Agr x y ** y ; --choose all the other fields from second argument
  ConsAP as a = consrTable Agr comma as a ** as ;
  ConjAP co as = conjunctDistrTable Agr co as ** as ;

lincat
  [CN] = { } ;

lin
  BaseCN = {} ;
  ConsCN = {} ;
  ConjCN co cs = conjunctDistrTable Agr co cs ** cs ;

lincat
  [DAP] = Determiner ** { pref2 : Str } ;

lin
  BaseDAP x y = x ** { pref2 = y.pref } ;
  ConsDAP xs x = xs ** { pref2 = x.pref } ;
  ConjDet conj xs = xs ** { pref = conj.s1 ++ xs.pref ++ conj.s2 ++ xs.pref2 } ;
-}

-- Noun phrases
lincat
  [NP] = {s1,s2 : Case => Str} ** BaseNP ;

lin
  BaseNP x y = twoTable Case x y ** consNP x y ;
  ConsNP xs x = consrTable Case comma xs x ** consNP xs x ;
  ConjNP conj xs = conjunctNPTable conj xs ** conjNP xs conj ;

oper

  ConjDistr : Type = {s2 : State => Str ; s1 : Str} ;

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
  conjunctNPTable : ConjDistr -> ({s1,s2 : Case => Str} ** BaseNP) -> {s : Case => Str ; st : State} = \co,xs -> xs **
   {s = -- TODO if xs is a pronoun, make them use (pronTable ! xs.a).sp
      table { cas => co.s1 ++ xs.s1 ! Abs ++ co.s2 ! xs.st ++ xs.s2 ! cas}} ;

  consNP : BaseNP -> BaseNP -> BaseNP = \x,y ->
    x ** { agr = conjAgr x.agr (getNum y.agr) } ;

  conjNP : BaseNP -> Conj -> BaseNP = \xs,conj ->
    xs ** { agr = conjAgr xs.agr conj.nbr } ;

  conjAgr : Agreement -> Number -> Agreement = \a,n ->
    case n of { Pl => plAgr a ; _  => a } ;

  conjNbr : Number -> Number -> Number = \n,m ->
    case n of { Pl => Pl ; _ => m } ;
}
