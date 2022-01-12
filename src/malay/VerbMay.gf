concrete VerbMay of Verb = CatMay ** open ResMay, AdverbMay, Prelude in {


lin

-----
-- VP
  -- : V -> VP
  UseV = ResMay.useV ;

  --  : V2 -> VP ; -- be loved
  PassV2 v2 = useV {s = \\_ => v2.passive} ;

  -- : VPSlash -> VP ;
  -- ReflVP = ResMay.insertRefl ;

  -- : VV  -> VP -> VP ;
  ComplVV vv vp = vp ** useV {
    s = \\vf => vv.s ++ vp.s ! Root ! Pos
    } ;

  -- : VS  -> S  -> VP ;
  -- ComplVS vs s =
  --   let vps = useV vs ;
  --       subord = SubjS {s=""} s ;
  --    in vps ** {} ;

{-
  -- : VQ -> QS -> VP ;
  ComplVQ vq qs = ;

  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = ResMay.insertComp (CompAP ap).s (useV va) ;

-}
--------
-- Slash

  -- : V2 -> VPSlash
  SlashV2a v2 = useV v2 ** {
    c2 = v2.c2 ;
    adjCompl = []
    } ;


  -- : V3 -> NP -> VPSlash ; -- give it (to her)
  Slash2V3 v3 dobj = useV {
    s = \\vf => v3.s ! vf ++ applyPrep v3.c2 dobj

    } ** {
      c2 = v3.c3; -- Now the VPSlash is missing only the indirect object
      adjCompl = []
    } ;

  -- : V3 -> NP -> VPSlash ; -- give (it) to her
  Slash3V3 v3 iobj = useV {
    s = \\vf => v3.s ! vf ++ iobj.s ! Bare ++ applyPrep v3.c3 emptyNP;
    --iobj.s ! Bare -- applyPrep v3.c3 iobj -- TODO check if this works for all -- probably not
    } ** {
      c2 = v3.c2 ;-- Now the VPSlash is missing only the direct object
      adjCompl = []

    } ;

  -- insertObjc : (Agr => Str) -> SlashVP -> SlashVP = \obj,vp ->
  --   insertObj obj vp ** {c2 = vp.c2 ; gapInMiddle = vp.gapInMiddle ; missingAdv = vp.missingAdv } ;

  SlashV2A v2 adj = useV {
    s = \\vf => v2.s ! vf;
  } ** {
    c2 = v2.c2;
    adjCompl = adj.s
  } ;

 {-
  -- : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s =

  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = ;

  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = ;
-}
  -- : VPSlash -> NP -> VP
  ComplSlash vps np = vps ** {
    s = \\vf,pol =>
      vps.s ! vf ! pol
      ++ applyPrep vps.c2 np ++ vps.adjCompl
    -- s = \\vf,pol => vps.s ! vf ! pol ++ applyPrep vps.c2 np
    } ;


  -- : VV  -> VPSlash -> VPSlash ;
  SlashVV vv vps = ComplVV vv vps ** {
    c2 = vps.c2 ; -- like ComplVV except missing object
    passive = vv.s ++ vps.passive;
    adjCompl = vps.adjCompl ;
    } ;

  -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
  -- SlashV2VNP v2v np vps =

  -- : Comp -> VP ;
  UseComp comp = comp ;

  -- : VP -> Adv -> VP ;  -- sleep here
  AdvVP vp adv = vp ** {
    s = \\vf,pol => vp.s ! vf ! pol ++ adv.s
    } ;

{-
  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash = insertAdv ;

  -- : VP -> Adv -> VP ;  -- sleep , even though ...
  ExtAdvVP vp adv =  ;

  -- : AdV -> VP -> VP ;  -- always sleep
  AdVVP adv vp = vp ** {adv = adv} ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** { adv = adv.s ++ vps.adv } ;
-}
  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  -- VPSlashPrep vp prep = vp ** {c2 = prep} ;


--2 Complements to copula

-- Adjectival phrases, noun phrases, and adverbs can be used.

  -- : AP  -> Comp ;
  CompAP ap = useComp ap.s ;

  -- : CN  -> Comp ;
  CompCN cn = useComp (cn.s ! NF Sg Bare) ;

  --  NP  -> Comp ;
  CompNP np = useComp (np.s ! Bare) ;

  -- : Adv  -> Comp ;
  --"Both bukan and tidak may negate prepositional phrases. The choice of either
  --depends on whether it is the noun within the phrase that is being negated
  --or the implied verb associated with the phrase." Mintz p. 281 (10.1.4)
  CompAdv adv = useV {s = \\_ => adv.s} ;

  -- : VP -- Copula alone;
  UseCopula = useV copula ;

}
