concrete ConjunctionKor of Conjunction =
  CatKor ** open ResKor, Prelude in {

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

    -}


lincat
  [Adv],[AdV],[IAdv] = ConjSS ;
lin
  BaseAdv, BaseAdV, BaseIAdv = baseSS ;
  ConsAdv, ConsAdV, ConsIAdv = consSS ;
  ConjAdv, ConjAdV, ConjIAdv = conjSS ;

oper
  ConjSS : Type = SS ** {firstSS : ConjType => Str} ;

  baseSS : SS -> SS -> ConjSS = \s1,s2 -> s2 ** {
    firstSS = mkFirstSS s1 ;
    } ;

  consSS : SS -> ConjSS -> ConjSS = \s,ss -> ss ** {
    firstSS = \\conj =>
      mkFirstSS s ! conj ++ ss.firstSS ! conj ;
    } ;

  conjSS : Conj -> ConjSS -> SS = \co,ss -> {
    s = co.s1 ++ ss.firstSS ! co.c ++ ss.s
    } ;

-- Versions with commas, no repeated conjunctions
  baseSScomma : SS -> SS -> ConjSS = \s1,s2 -> s2 ** {
    firstSS = \\conj => s1.s ++ SOFT_BIND ++ "," ;
    } ;

  consSScomma : SS -> ConjSS -> ConjSS = \s,ss -> ss ** {
    firstSS = \\conj =>
      s.s ++ SOFT_BIND ++ "," ++ ss.firstSS ! conj ;
    } ;

  conjSScomma : Conj -> ConjSS -> SS = \co,ss -> {
    s = co.s1
      ++ glue (ss.firstSS ! co.c) co.s2
      ++ ss.s
    } ;

oper
  mkFirstSS : SS -> ConjType => Str = \s ->
    \\conj => glue s.s (conjTable ! VStar ! conj) ;



lincat
  [S], [RS] = ResKor.Sentence ** {firstS : ConjType => Str} ;

lin
  BaseS,BaseRS = \s1,s2 -> s2 ** {
    firstS = mkFirstS s1
    } ;

  ConsS,ConsRS = \s,ss -> ss ** {
    firstS = \\conj =>
      mkFirstS s ! conj ++ ss.firstS ! conj ;
    } ;

  ConjS,ConjRS = \co,ss -> ss ** {
    s = \\st => co.s1 ++ ss.firstS ! co.c ++ ss.s ! st
    } ;

oper
  mkFirstS : ResKor.Sentence -> ConjType => Str = \s ->
    \\conj => glue (s.s ! WithConj) (conjTable ! VStar ! conj) ;

lincat
  [AP] = ResKor.AdjPhrase ** {firstAP : VForm => ConjType => Str} ;

lin
  BaseAP a1 a2 = a2 ** {
    firstAP = mkFirstAP a1 ;
    } ;

  ConsAP a as = as ** {
    firstAP = \\af,conj =>
      mkFirstAP a ! af ! conj ++ as.firstAP ! af ! conj ;
    } ;

  ConjAP co as = as ** {
    s = \\af => co.s1 ++ as.firstAP ! af ! co.c ++ as.s ! af
    } ;


oper
  mkFirstAP : ResKor.AdjPhrase -> VForm => ConjType => Str = \ap ->
    \\af,conj => ap.compar ++ glue (ap.s ! VStem) (conjTable ! VStar ! conj) ;

{-
lincat
  [CN] = { } ;

lin
  BaseCN = {} ;
  ConsCN = {} ;
  ConjCN co cs = conjunctDistrTable Agr co cs ** cs ;

lincat
  [DAP] =

lin
  BaseDAP x y = x **
  ConsDAP xs x = xs **
  ConjDet conj xs = xs **

-}
-- Noun phrases
lincat
  [NP] = ResKor.NounPhrase ** {firstNP : ConjType => Str} ;

lin
  BaseNP np1 np2 = np2 ** {firstNP = mkFirstNP np1} ;
  ConsNP np nps = nps ** {
    firstNP = \\conj => mkFirstNP np ! conj ++ nps.firstNP ! conj
    } ;
  ConjNP co nps = nps ** {
    s = \\nf => co.s1 ++ nps.firstNP ! co.c ++ nps.s ! nf ;
    n = co.n
    } ;

oper
  mkFirstNP : ResKor.NounPhrase -> ConjType => Str = \np ->
    \\conj => glue (np.s ! Bare) (conjTable ! NStar ! conj) ;


}
