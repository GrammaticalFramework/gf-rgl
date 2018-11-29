concrete VerbSom of Verb = CatSom ** open ResSom, Prelude in {


lin

-----
-- VP

  UseV = ResSom.useV ;
{-
  -- : VV  -> VP -> VP ;
  ComplVV vv vp =  ;


  -- : VS  -> S  -> VP ;
  ComplVS vs s = ;

  -- : VQ -> QS -> VP ;
  ComplVQ vq qs = ;

  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = ResSom.insertComp (CompAP ap).s (useV va) ;


--------
-- Slash

  -- : V2 -> VPSlash
  SlashV2a = ResSom.slashDObj ;


  -- : V3 -> NP -> VPSlash ; -- give it (to her)
  Slash2V3 v3 npNori = slashDObj v3 **
    { iobj = { s = npNori.s ! Dat ;
               agr = npNori.agr }
    } ;

  -- : V3 -> NP -> VPSlash ; -- give (it) to her
  Slash3V3 v3 npNor = slashIObj v3 **
    { dobj = npNor ** { s = mkDObj npNor }
    } ;


  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = ;


  -- : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s = ;

  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = slashDObj v2a **
    { comp = (CompAP ap).s } ;


  -- : VPSlash -> NP -> VP
  ComplSlash vps np = ResSom.complSlash vps np ;


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

--2 Other ways of forming verb phrases

-- Verb phrases can also be constructed reflexively and from
-- copula-preceded complements.

  -- : VPSlash -> VP ;
  ReflVP vps = ;
-}
  -- : Comp -> VP ;
  UseComp comp = UseCopula ** comp ** {
    isPred = True
    } ;
{-
  --  : V2 -> VP ;               -- be loved
  PassV2 v2 =

  -- : VP -> Adv -> VP ;  -- sleep here
  AdvVP vp adv = vp ** {adv = adv} ; ---- TODO: how about combining adverbs?

  -- : VP -> Adv -> VP ;  -- sleep , even though ...
  ExtAdvVP vp adv =  ;

  -- : AdV -> VP -> VP ;  -- always sleep
  AdVVP adv vp = vp ** {adv = adv} ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash vps adv = vps ** { adv = vps.adv ++ adv.s } ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** { adv = adv.s ++ vps.adv } ;
-}
  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  -- NB. We need possibly a MissingArg kind of solution here too
  -- VPSlashPrep vp prep = vp **
  --   { c2 = case vp.c2 of { noPrep => prep.prep ;
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
    } ;
{-}
  -- : CN  -> Comp ;
  CompCN cn = { } ;

  --  NP  -> Comp ;
  CompNP np = {  } ;

  -- : Adv  -> Comp ;
  CompAdv adv = {  } ;

-}
  -- : VP -- Copula alone;
  UseCopula = useV copula ;

}
