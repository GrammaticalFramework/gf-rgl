concrete ConjunctionKor of Conjunction =
  CatKor ** open ResKor, Coordination, Prelude in {

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


{-
-- RS depends on X, Y and Z, otherwise exactly like previous.
-- RS can modify CNs, which are open for …, and have inherent …
lincat
  [RS] = {s1,s2 : … => Str} ;

lin
  BaseRS = twoTable3 … ;
  ConsRS = consrTable3 … comma ;
  ConjRS = conjunctRSTable ;
-}

lincat
  [S] = ResKor.Sentence ** {firstS : ConjType => Str} ;

lin
  BaseS s1 s2 = s2 ** {
    firstS = mkFirstS s1
    } ;

  ConsS s ss = ss ** {
    firstS = \\conj =>
      mkFirstS s ! conj ++ ss.firstS ! conj ;
    } ;

  ConjS co ss = ss ** {
    s = \\st => co.s1 ++ ss.firstS ! co.c ++ ss.s ! st
    } ;

oper
  mkFirstS : ResKor.Sentence -> ConjType => Str = \s ->
    \\conj => glue (s.s ! Subord) (conjTable ! NStar ! conj) ;

lincat
  [AP] = ResKor.AdjPhrase ** {firstAP : AForm => ConjType => Str} ;

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
  mkFirstAP : ResKor.AdjPhrase -> AForm => ConjType => Str = \ap ->
    \\af,conj => case af of {
      AAttr   => glue (ap.s ! AAttr) (conjTable ! NStar ! conj) ;
      APred _ => glue (ap.s ! APred VStem) (conjTable ! VStar ! conj) } ;

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
