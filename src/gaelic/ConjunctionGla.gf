concrete ConjunctionGla of Conjunction =
  CatGla ** open ResGla, Coordination, Prelude in {

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

-----------------------------------------------------------------------------
-- Adverb and other simple {s : Str} types.
lincat
  [Adv],[AdV],[IAdv] = {s1,s2 : Str} ;
  [S], [RS] = {s1,s2 : Str} ;
  [NP] = {s1,s2 : Case => Str} ;
  [AP] = {s1,s2 : AForm => Str} ;
  [CN] = {s1,s2 : Case => Species => Number => Str ; g : Gender} ;
  [DAP] = {s1,s2 : Gender => Case => Str} ;

lin
  BaseAdv, BaseAdV, BaseIAdv = twoSS ;
  ConsAdv, ConsAdV, ConsIAdv = consrSS comma ;
  ConjAdv, ConjAdV, ConjIAdv = conjunctDistrSS ;

  BaseS, BaseRS = twoSS ;
  ConsS, ConsRS = consrSS comma ;
  ConjS, ConjRS = conjunctDistrSS ;

  BaseNP x y = {s1 = \\c => x.art ! c ++ x.s ! c ; s2 = \\c => y.art ! c ++ y.s ! c} ;
  ConsNP x xs = {s1 = \\c => x.art ! c ++ x.s ! c ++ comma ++ xs.s1 ! c ; s2 = xs.s2} ;
  ConjNP conj xs = emptyNP ** {
    s = \\c => conj.s1 ++ xs.s1 ! c ++ conj.s2 ++ xs.s2 ! c ;
    voc = conj.s1 ++ xs.s1 ! NOM ++ conj.s2 ++ xs.s2 ! NOM ;
    a = NotPron (DDef conj.n Def)
    } ;

  BaseAP x y = {s1 = x.s ; s2 = y.s} ;
  ConsAP x xs = {s1 = \\f => x.s ! f ++ comma ++ xs.s1 ! f ; s2 = xs.s2} ;
  ConjAP conj xs = {
    s = \\f => conj.s1 ++ xs.s1 ! f ++ conj.s2 ++ xs.s2 ! f ;
    voc = \\g => conj.s1 ++ xs.s1 ! ASg NOM g ++ conj.s2 ++ xs.s2 ! ASg NOM g
    } ;

  BaseCN x y = {s1 = x.s ; s2 = y.s ; g = x.g} ;
  ConsCN x xs = {s1 = \\c,d,n => x.s ! c ! d ! n ++ comma ++ xs.s1 ! c ! d ! n ; s2 = xs.s2 ; g = xs.g} ;
  ConjCN conj xs = {
    s = \\c,d,n => conj.s1 ++ xs.s1 ! c ! d ! n ++ conj.s2 ++ xs.s2 ! c ! d ! n ;
    voc = \\n => conj.s1 ++ xs.s1 ! NOM ! Indef ! n ++ conj.s2 ++ xs.s2 ! NOM ! Indef ! n ;
    g = xs.g
    } ;

  BaseDAP x y = {s1 = x.s ; s2 = y.s} ;
  ConsDAP x xs = {s1 = \\g,c => x.s ! g ! c ++ comma ++ xs.s1 ! g ! c ; s2 = xs.s2} ;
  ConjDet conj xs = {
    s = \\g,c => conj.s1 ++ xs.s1 ! g ! c ++ conj.s2 ++ xs.s2 ! g ! c ;
    s2 = \\g,c => [] ; sp = [] ; dt = DDef conj.n Def
    } ;

{-

-----------------------------------------------------------------------------
-- S is sometimes already {s : Str}, sometimes open for mood or word order.
-- Simply take the lincat of S, and split the s field into s1 and s2.
-- Then make sure that all of the other fields are retained.

lincat
  [S] = {s1, s2 : …} ;

lin
  -- : S -> S -> ListS ;      -- John walks, Mary runs
  BaseS x y =

  -- : S -> ListS -> ListS ;  -- John walks, Mary runs, Bill swims
  ConsS x xs =

  -- : Conj -> ListS -> S ;       -- he walks and she runs
  ConjS conj xs =

-----------------------------------------------------------------------------
-- RS is variable on … and has inherent …
-- RS can modify CNs, which are open for …, and have inherent …

lincat
  [RS] = {s1,s2 : … => Str} ;

lin

  -- : RS -> RS -> ListRS ;       -- who walks, whom I know
  BaseRS x y =

  -- : RS -> ListRS -> ListRS ;   -- who wals, whom I know, who is here
  ConsRS x xs =

  -- : Conj -> ListRS -> RS ;     -- who walks and whose mother runs
  ConjRS conj xs =


-----------------------------------------------------------------------------
-- NP  is variable on … and has inherent …

lincat
  [NP] = {s1, s2 : …} ;

lin
  -- : NP -> NP -> ListNP ;      -- John, Mary
  BaseNP x y =

  -- : NP -> ListNP -> ListNP ;  -- John, Mary, Bill
  ConsNP x xs =

  -- : Conj -> ListNP -> NP ;     -- she or we
  ConjNP conj xs =

-----------------------------------------------------------------------------
-- AP  is variable on … and has an inherent …

lincat
  [AP] = {s1, s2 : …} ;

lin
  -- : AP -> AP -> ListAP ;       -- red, white
  BaseAP x y =

  -- : AP -> ListAP -> ListAP ;   -- red, white, blue
  ConsAP x xs =

  -- : Conj -> ListAP -> AP ;     -- cold and warm
  ConjAP conj xs =

-----------------------------------------------------------------------------
-- CN  is variable on …
-- CN conjunction is not in the API, so this can be lower prio

lincat
  [CN] = {s1, s2 : …} ;

lin
  -- : CN -> CN -> ListCN ;      -- man, woman
  BaseCN x y =

  -- : CN -> ListCN -> ListCN ;  -- man, woman, child
  ConsCN x xs =

  -- : Conj -> ListCN -> CN ;     -- man and woman
  ConjCN conj xs =

-----------------------------------------------------------------------------
-- Det and DAP
-- Note that there is no [Det], the way to coordinate Dets is to make them
-- into DAP first, using Noun.DetDAP : Det -> DAP ;
-- DAP ("three small") isn't used in any API functions, so lower prio.

lincat
  [DAP] = {s1, s2 : …} ;

lin
  -- : DAP -> DAP -> ListDAP ;
  BaseDAP x y =

  -- : DAP -> ListDAP -> ListDAP ;
  ConsDAP xs x =

  -- : Conj -> ListDAP -> Det ;   -- his or her
  ConjDet conj xs =
-}
}
