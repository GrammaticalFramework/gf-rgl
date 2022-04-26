concrete ConjunctionZul of Conjunction =
  CatZul ** open ResZul, Coordination, Prelude in {

  flags optimize=all_subs ;

  lin

    -- should deal correctly with futhi, not with na-
    -- ConjS conj ss = {
    --   s = \\dm => ss.s1!dm ++ conj.s!RC ++ ss.s2!dm ;
    --   subjs = ss.subjs1 ++ conj.s!RC ++ ss.subjs2 ;
    --   pots = \\dm => ss.pots1!dm ++ conj.s!RC ++ ss.pots2!dm
    -- } ;
--
--     ConjAdv = conjunctDistrSS ;
--     ConjAdV = conjunctDistrSS ;

    -- ConjAdv conj advs =  ;

    -- ConjNP conj nps =
    -- let
    --   np2_loose = nps.s2!Full ;
    --   np2_fixed = nps.s2!Reduced
    -- in {
    --   empty = [] ;
    --   s = \\f =>
    --         nps.s1!f ++ np1.desc
    --         ++
    --         (link_conj
    --           (conj.s!(nominit!np2.agr))
    --           (np2_loose)
    --           (np2_fixed)
    --           conj.fix) ++
    --         np2.desc ;
    --   loc = np1.loc ++
    --         np1.desc ++
    --         (link_conj
    --           (conj.s!(locinit!np2.agr))
    --           np2.loc
    --           np2.loc
    --           conj.fix) ++
    --         np2.desc ;
    --   desc = [] ;
    --   agr = np2.agr ;
    --   isPron = False ;
    --   reqLocS = np1.reqLocS
    -- } ;

--     ConjAP conj ss = conjunctDistrTable Agr conj ss ** {
--       isPre = ss.isPre
--       } ;
--
--     ConjRS conj ss = conjunctDistrTable Agr conj ss ** {
--       c = ss.c
--       } ;
--
--     ConjIAdv = conjunctDistrSS ;
--
--     ConjCN co ns = conjunctDistrTable2 Number Case co ns ** {g = Neutr} ; --- gender?
--
--     ConjDet c xs = let cxs = (conjunctDistrSS c xs).s in {s = cxs ; sp = \\_,_,_ => cxs ++ "one" ; hasNum = False ; n = xs.n} ;
--
-- -- These fun's are generated from the list cat's.
--
    -- BaseS = twoSS ;
--     ConsS = consrSS comma ;
    -- BaseAdv x y = twoSS ** { reqLocS = x.reqLocS } ;
    -- ConsAdv x y = consrSS comma x y ** { reqLocS = x.reqLocS } ;
--     BaseAdV = twoSS ;
--     ConsAdV = consrSS comma ;
    BaseNP x y = twoTable NForm x y ** {
      agr = conjAgr x.agr y.agr ;
      empty = x.empty ++ y.empty ;
      loc = x.loc
    } ;
    ConsNP xs x = consrTable NPCase comma xs x ** {a = conjAgr xs.a x.a} ;
--     BaseAP x y = twoTable Agr x y ** {isPre = andB x.isPre y.isPre} ;
--     ConsAP xs x = consrTable Agr comma xs x ** {isPre = andB xs.isPre x.isPre} ;
--     BaseRS x y = twoTable Agr x y ** {c = y.c} ;
--     ConsRS xs x = consrTable Agr comma xs x ** {c = xs.c} ;
--     BaseIAdv = twoSS ;
--     ConsIAdv = consrSS comma ;
--     BaseCN = twoTable2 Number Case ;
--     ConsCN = consrTable2 Number Case comma ;
--     BaseDAP x y = twoSS x y ** {n = y.n} ; --- the last number decides: one big and two small cars
--     ConsDAP x xs = consrSS comma x xs ** {n = xs.n} ;

  lincat
    [S] = {
      s1 : DMood => Str ;
      subjs1 : Str ;
      pots1 : DMood => Str ;
      s2 : DMood => Str ;
      subjs2 : Str ;
      pots2 : DMood => Str
    } ;
    -- [Adv] = {s1,s2 : Str} ;
    -- [AdV] = {s1,s2 : Str} ;
    -- [IAdv] = {s1,s2 : Str} ;
    -- [NP] = {s1,s2 : NPCase => Str ; a : Agr} ;
    -- [AP] = {s1,s2 : Agr => Str ; isPre : Bool} ;
    -- [RS] = {s1,s2 : Agr => Str ; c : NPCase} ;
    -- [CN] = {s1,s2 : Number => Case => Str} ;
    -- [DAP] = {s1,s2 : Str ; n : Number} ;

  oper
    conjAgr : Agr -> Agr = \a1,a2 -> case <a1,a2> of {
        <First _,First _> => First Pl ;
        <First _,Second _> => First Pl ;
        <First _,Third _ _> => First Pl ;
        <Second _,First _> => First Pl ;
        <Second _,Second _> => Second Pl ;
        <Second _,Third _ _> => Second Pl ;
        <Third _ _,First _> => First Pl ;
        <Third _ _,Second _> => Second Pl ;
        <Third c1 _,Third c2 _> => Third c1 Pl
      } ;

}
