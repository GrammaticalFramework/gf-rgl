concrete VerbSom of Verb = CatSom ** open ResSom, AdverbSom, Prelude in {


lin

-----
-- VP
  -- : V -> VP
  UseV = ResSom.useV ;

  --  : V2 -> VP ; -- be loved
  PassV2 = ResSom.passV2 ;

  -- : VPSlash -> VP ;
  ReflVP = ResSom.insertRefl ;

  -- : VV  -> VP -> VP ;
  ComplVV vv vp = let vc = vp.vComp in case vv.vvtype of {
    Waa_In => vp ** {
      vComp = vc ** {subjunc = vv.s ! VInf} ; -- it's always the word "in", and it will be placed before subject pronoun. it's placed in vv.s!VInf so that the VV would contribute with some string. /IL
      obj2 = vp.obj2 ** {s = []} ;      -- word order hack to avoid more parameters: 
      miscAdv = vp.miscAdv ++ vp.obj2.s -- dump the object to miscAdv
      } ;

    Subjunctive => useV vv ** {
      stm = Waxa ;
      vComp = vc ** { -- The whole previous VP becomes the subordinate clause
                subcl = \\agr => 
                          let subj = pronTable ! agr ;
                              cls = predVPSlash subj vp ;
                              rcl = mergeRCl (cls.s ! True) ;
                           in "in" ++ rcl.s ! Pres ! Simul ! Pos
              }
      } ;

    Infinitive => vp ** {
      s = vv.s ; -- check Saeed p. 169
      vComp = vc ** {
        inf = vc.inf ++ vp.s ! VInf
        } ;
      stm = Waa NoPred ;
      } 
    } ;

  -- : VS  -> S  -> VP ;
  ComplVS vs s = 
    let vps = useV vs ;
        subord = SubjS {s="in"} s ;
     in vps ** {obj2 = {s = subord.berri ; a = P3_Prep}} ;

{-
  -- : VQ -> QS -> VP ;
  ComplVQ vq qs = ;

  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = ResSom.insertComp (CompAP ap).s (useV va) ;

-}
--------
-- Slash

  -- : V2 -> VPSlash
  SlashV2a = useVc ;

  -- : V3 -> NP -> VPSlash ; -- give it (to her)
  -- : V3 -> NP -> VPSlash ; -- give (it) to her
  Slash2V3,
  Slash3V3 = \v3 -> insertComp (useVc3 v3) ;
{-
  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = ;

  -- : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s = ;

  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = slashDObj v2a **
    { comp = (CompAP ap).s } ;

-}
  -- : VPSlash -> NP -> VP
  ComplSlash = insertComp ;

{-
  -- : VV  -> VPSlash -> VPSlash ;
                  -- Just like ComplVV except missing subject!
  SlashVV vv vps = ComplVV vv vps ** { missing = vps.missing ;
                                       post = vps.post } ;

  -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
  SlashV2VNP v2v np vps =
    ComplVV v2v vps **
      { missing = vps.missing ;
        post = vps.post ;
        iobj = np ** { s = np.s ! Dat } } ;

-}

  -- : Comp -> VP ;
  UseComp comp = UseCopula ** comp ;

  -- : VP -> Adv -> VP ;  -- sleep here
  AdvVP = insertAdv ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash = insertAdv ;

{-
  -- : VP -> Adv -> VP ;  -- sleep , even though ...
  ExtAdvVP vp adv =  ;

  -- : AdV -> VP -> VP ;  -- always sleep
  AdVVP adv vp = vp ** {adv = adv} ;


  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** { adv = adv.s ++ vps.adv } ;
-}
  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  -- NB. We need possibly a MissingArg kind of solution here too
  -- VPSlashPrep vp prep = vp **
  --   { c2 = case vp.c2 of { NoPrep => prep.prep ;
  --                          x      => x }} ;




--2 Complements to copula

-- Adjectival phrases, noun phrases, and adverbs can be used.

  -- the house is big
  -- the houses are big
  -- I am [a house that sleeps here]
  -- we are [houses that sleep here]

  -- : AP  -> Comp ;
  CompAP ap = {
    comp = \\a => <[], ap.s ! AF (getNum a) Abs> ;
    stm = Waa Copula ;
    } ;

  -- : CN  -> Comp ;
  CompCN cn = {
    comp = \\a => <[], cn2str Sg Abs cn> ;
    stm = Waa NoCopula ;
    } ;

  --  NP  -> Comp ;
  CompNP np = {
    comp = \\a => <[], np.s ! Abs> ;
    stm = Waa NoCopula ;
    } ;

  -- : Adv  -> Comp ;
  CompAdv adv = {
    comp = \\a => <[], linAdv adv> ;
    stm = Waa Copula ;
    } ;

  -- : VP -- Copula alone;
  UseCopula = useV copula ;

}
