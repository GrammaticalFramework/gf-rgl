concrete VerbKor of Verb = CatKor ** open ResKor, AdverbKor, Prelude in {


lin

-----
-- VP
  -- : V -> VP
  UseV = ResKor.useV ;

  --  : V2 -> VP ; -- be loved
  -- PassV2 = ResKor.passV2 ;

  -- : VPSlash -> VP ;
  -- ReflVP = ResKor.insertRefl ;

  -- : VV  -> VP -> VP ;
  ComplVV vv vp = useV vv ** {
    adv = vp.adv ;
    nObj = vp.nObj ;
    s = \\vf => vp.s ! VAttr Pos ++ "것을" ++ vv.s ! vf
    } ;

  -- : VS  -> S  -> VP ;
  ComplVS vs sent = useV vs ** {
    adv = sent.s ! Subord ++ "고" ;
    } ;

  -- : VQ -> QS -> VP ;
  ComplVQ vq qs = useV vq ** {adv = qs.s ! Plain ++ "고"} ;

  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = useV va ** {
    adv = ap.compar ++ ap.s ! VAttr Pos
    } ;
--------
-- Slash
  -- : V2 -> VPSlash
  SlashV2a = ResKor.useVc ;

  -- : V3 -> NP -> VPSlash ; -- give it (to her)
  -- : V3 -> NP -> VPSlash ; -- give (it) to her
  Slash2V3 v3 np = useVc v3 ** {
    nObj = \\_ => (prepNP v3.c3 v3.p3 np).s
    } ;
  Slash3V3 v3 np = useVc (v3 ** {c2=v3.c3; p2=v3.p3}) ** {
    nObj = \\_ => (prepNP v3.c2 v3.p2 np).s
    } ;

  -- : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s =
    let vps = useVc v2s ;
     in vps ** {adv = s.s ! Subord ++ "고"} ;


  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = useVc v2v ** {
    adv = vp.nObj ! DeclObj ++ vp.adv ++ vp.s ! VAttr Pos ++ "것을"
    } ;

  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = useVc v2q ** {adv = qs.s ! Plain ++ "고"} ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = useVc v2a ** {
    adv = ap.compar ++ glue (ap.s ! VStem Pos) "게"
  } ;
  -- : VPSlash -> NP -> VP
  ComplSlash = ResKor.insertComp ;

  -- : VV  -> VPSlash -> VPSlash ;
  SlashVV vv vps = vps ** {
    s = \\vf => vps.s ! VAttr Pos ++ "것을" ++ vv.s ! vf
    } ;

  -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
  SlashV2VNP v2v np vps =
    vps ** {
      adv = (prepNP v2v.c2 v2v.p2 np).s ++ vps.adv ;
      s = \\vf => vps.s ! VAttr Pos ++ "것을" ++ v2v.s ! vf
      } ;

  -- : Comp -> VP ;
  UseComp comp = UseCopula ** comp ;
  -- : VP -> Adv -> VP ;  -- sleep here
  AdvVP = insertAdv ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash = insertAdvSlash ;


  -- : VP -> Adv -> VP ;  -- sleep , even though ...
  ExtAdvVP = insertAdv ;

  -- : AdV -> VP -> VP ;  -- always sleep
  AdVVP adv vp = vp ** {adv = vp.adv ++ adv.s} ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** { adv = vps.adv ++ adv.s} ;

  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  VPSlashPrep vp prep = vp ** {c2=Bare; p2=prep} ;

--2 Complements to copula

-- Adjectival phrases, noun phrases, and adverbs can be used.

  -- : AP  -> Comp ;
  CompAP ap = emptyComp ** {
    s = \\vf => ap.compar ++ ap.s ! vf ;
    past = \\p => ap.compar ++ ap.past ! p ;
    pastAttr = \\p => ap.compar ++ ap.pastAttr ! p ;
    futureAttr = \\p => ap.compar ++ ap.futureAttr ! p
    } ;

  -- : CN  -> Comp ;
  -- : NP  -> Comp ;
  CompCN,
  CompNP = \n ->
    let cop = case n.p of {
                Vowel => copulaAfterVowel ;
                Consonant => copula }
    in emptyComp ** {
    s = \\vf =>
      case vf of {
        VF _ Neg => n.s ! Bare ++ cop.s ! vf ;
        _ => glue (n.s ! Bare) (cop.s ! vf)
        } ;
    past = \\p => glue (n.s ! Bare) (cop.past ! p) ;
    pastAttr = \\p => glue (n.s ! Bare) (cop.pastAttr ! p) ;
    futureAttr = \\p => glue (n.s ! Bare) (cop.futureAttr ! p)
    } ;

  -- : Adv  -> Comp ;
  CompAdv adv = emptyComp ** {
    s = \\vf => adv.s ++ have_V.s ! vf ;
    past = \\p => adv.s ++ have_V.past ! p ;
    pastAttr = \\p => adv.s ++ have_V.pastAttr ! p ;
    futureAttr = \\p => adv.s ++ have_V.futureAttr ! p
    } ;

  -- : VP -- Copula alone;
  UseCopula = useV copula ;

}
