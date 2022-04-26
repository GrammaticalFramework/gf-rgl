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
  ConjSS : Type = {s : ConjType => Str} ;

  baseSS : SS -> SS -> ConjSS = \s1,s2 -> {
    s = \\conj => glue s1.s (conjTable ! NStar ! conj ! Vowel) ++ s2.s ; -- TODO check phono
    } ;

  consSS : SS -> ConjSS -> ConjSS = \s,ss -> ss ** {
    s = \\conj => glue s.s (conjTable ! NStar ! conj ! Vowel) ++ ss.s ! conj ;
    } ;

  conjSS : Conj -> ConjSS -> SS = \co,ss -> {
    s = co.s1 ++ ss.s ! co.c
    } ;

  -- Version with commas, no repeated conjunctions!
  -- baseSS works for both: always conjunction between penultimate and last.
  -- Difference from consSS: conjTable ! NStar ! conj isn't used, only comma.
  consSScomma : SS -> ConjSS -> ConjSS = \s,ss -> ss ** {
    s = \\conj => s.s
              ++ SOFT_BIND ++ ","   -- Don't add conjunction, only comma
              ++ ss.s ! conj ;
    } ;


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
    \\conj => glue (s.s ! WithConj) (conjTable ! VStar ! conj ! s.p) ;

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
    \\af,conj => ap.compar ++ case isPos af of {
       True  => glue (ap.s ! VStem Pos) (conjTable ! VStar ! conj ! ap.p) ;
       False => glue (ap.s ! VStem Neg) (conjTable ! VStar ! conj ! ap.pNeg)
    } ;

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
    \\conj => glue (np.s ! Bare) (conjTable ! NStar ! conj ! np.p) ;
    -- Versions with commas, no repeated conjunctions

  baseNPcomma : NP -> NP -> ListNP = \x,y -> y ** {
    firstNP = \\conj => x.s ! Bare ++ BIND ++ "," ;
    } ;

  consNPcomma : NP -> ListNP -> ListNP = \x,xs -> xs ** {
    firstNP = \\conj =>
      x.s ! Bare ++ BIND ++ "," ++ xs.firstNP ! conj ;
    } ;

  conjNPcomma : Conj -> ListNP -> NP = \co,xs -> xs ** {
    s = \\nf => co.s1
             ++ xs.firstNP ! co.c
             ++ co.s2
             ++ xs.s ! nf
    } ;

}
