concrete ConjunctionTam of Conjunction =
  CatTam ** open ResTam, Coordination, Prelude in {

--  flags optimize=all_subs ;

--    {- Conjunction for category X needs four things:
--       lincat [X]
--       lin BaseX
--       lin ConsX
--       lin ConjX

--    For example, if X is defined as

--      lincat X   = {s     : Number => Str ;   g : Gender} ;

--    then [X] will split its s field into two, and retain its other fields as is:

--      lincat [X] = {s1,s2 : Number => Str ;   g : Gender} ;

--    Let us look at a simple case: Adv is of type {s : Str}
--    Then [Adv] is {s1,s2 : Str}.
--    BaseAdv, ConsAdv and ConjAdv can all use functions defined in prelude/Coordination:

--      BaseAdv = twoSS ;
--      ConsAdv = consrSS comma ;
--      ConjAdv = conjunctSS ;

--    --}


-- Adverb and other simple {s : Str} types.
--lincat
--  [Adv],[AdV],[IAdv] = {s1,s2 : Str} ;

--lin
--  BaseAdv, BaseAdV, BaseIAdv = twoSS ;
--  ConsAdv, ConsAdV, ConsIAdv = consrSS comma ;
--  ConjAdv, ConjAdV, ConjIAdv = conjunctDistrSS ;


--{-
-- RS depends on X, Y and Z, otherwise exactly like previous.
-- RS can modify CNs, which are open for …, and have inherent …
--lincat
--  [RS] = {s1,s2 : … => Str} ;

--lin
--  BaseRS = twoTable3 … ;
--  ConsRS = consrTable3 … comma ;
--  ConjRS = conjunctRSTable ;


--lincat
--  [S] = {} ;

--lin
--  BaseS x y = y ** { } ;
--  ConsS x xs =
--    xs ** { } ;
--  ConjS co xs = {} ;

--lincat
--  [AP] = {} ;

--lin
--  BaseAP x y = twoTable … x y ** y ; --choose all the other fields from second argument
--  ConsAP as a = consrTable … comma as a ** as ;
--  ConjAP co as = conjunctDistrTable … co as ** as ;

--lincat
--  [CN] = { } ;

--lin
--  BaseCN = {} ;
--  ConsCN = {} ;
--  ConjCN co cs = conjunctDistrTable Agr co cs ** cs ;

--lincat
--  [DAP] =

--lin
--  BaseDAP x y = x **
--  ConsDAP xs x = xs **
--  ConjDet conj xs = xs **


-- Noun phrases
--lincat
--  [NP] =

--lin
--  BaseNP x y =
--  ConsNP x xs =
--  ConjNP conj xs =

---}
--}
}
