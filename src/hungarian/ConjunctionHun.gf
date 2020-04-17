concrete ConjunctionHun of Conjunction =
  CatHun ** open ResHun, Coordination, Prelude in {

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


-- Adverb and other simple {s : Str} types.
lincat
  [Adv],[AdV],[IAdv],[S] = {s1,s2 : Str} ;

lin
  BaseAdv, BaseAdV, BaseIAdv, BaseS = twoSS ;
  ConsAdv, ConsAdV, ConsIAdv, ConsS = consrSS comma ;
  ConjAdv, ConjAdV, ConjIAdv, ConjS = conjunctDistrSS ;


lincat
  [AP] = {s1,s2 : Number => Str}  ;

lin
  BaseAP x y =
    -- Don't try to have discontinuous comparative forms
    let xCont : AP = x ** {s = \\n => x.s ! n ++ x.compar} ;
        yCont : AP = y ** {s = \\n => y.s ! n ++ y.compar} ;
     in twoTable Number xCont yCont ;
  ConsAP = consrTable Number comma ;
  ConjAP co as = conjunctDistrTable Number co as ** {compar = []} ;

-- Noun phrases
lincat
  [NP] = ResHun.BaseNP ** {s1,s2 : Case => Str} ;

lin
  BaseNP x y = twoTable Case x y ** y ;
  ConsNP x xs = consrTable Case comma x xs ** xs ;
  ConjNP co xs = conjunctDistrTable Case co xs ** xs ;

{-
-- RS depends on X, Y and Z, otherwise exactly like previous.
-- RS can modify CNs, which are open for …, and have inherent …
lincat
  [RS] = {s1,s2 : … => Str} ;

lin
  BaseRS = twoTable … ;
  ConsRS = consrTable … comma ;
  ConjRS = conjunctDistrTable ;

lincat
  [CN] = { } ;

lin
  BaseCN = {} ;
  ConsCN = {} ;
  ConjCN co cs = conjunctDistrTable … co cs ** cs ;

lincat
  [DAP] =

lin
  BaseDAP x y = x **
  ConsDAP xs x = xs **
  ConjDet conj xs = xs **

-}

}
