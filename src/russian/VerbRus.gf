concrete VerbRus of Verb = CatRus ** open ResRus, Prelude, Coordination in {

lin
  -- : V -> VP ;        -- sleep
  UseV v = {
    adv = \\a=>[] ;
    verb = v ;
    dep=[] ;
    compl = \\_ => []
    } ;

  -- : V2 -> VP ;         -- be loved
  PassV2 v2 = {
    adv = \\a=>[] ;
    verb = passivate v2 ;
    dep=[] ;
    compl = \\a=>[]
  } ;

  -- : VV -> VP -> VP ;  -- want to run
  ComplVV vv vp = vp ** {
    verb=vv.v ;
    dep=verbInf vp.verb ;
    adv=\\a=>vv.modal ! a ++ vp.adv ! a
    } ;

  -- : VS -> S -> VP ;  -- say that she runs
  ComplVS vs s = {
    verb = vs ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\A=>comma ++ "что" ++ s.s ! Ind
    } ;

  -- : VQ -> QS -> VP ;  -- wonder who runs
  ComplVQ vq qs = {
    verb = vq ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\A=>comma ++ "что" ++ qs.s ! QDir
    } ;


  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = {
    verb=va ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=case ap.preferShort of {
      PrefFull => (\\a => ap.s ! agrGenNum a ! Inanimate ! Ins) ;
      PrefShort => ap.short
      }
    } ;

  -- : V2 -> VPSlash ;  -- love (it)
  SlashV2a v2 = {adv=\\a=>[] ; verb=v2 ; dep=[] ; compl=\\_ => [] ; c=v2.c} ; -- looses complement info?

  -- : V3 -> NP -> VPSlash ;  -- give it (to her)
  Slash2V3 v3 np = {
    adv=\\a=>[] ;
    verb=v3 ;
    dep=[] ;
    compl=\\a=> v3.c.s ++ np.s ! v3.c.c;
    c=v3.c2
    } ;

  -- : V3  -> NP -> VPSlash ;  -- give (it) to her
  Slash3V3 v3 np = {
    adv=\\a=>[] ;
    verb=v3 ;
    dep=[] ;
    compl=\\a=> v3.c2.s ++ np.s ! v3.c2.c;
    c=v3.c
    } ;

  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = vp ** {
    verb=v2v ;
    dep=verbInf vp.verb ;
    c=v2v.c
    } ;

  -- : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s = {
    adv=\\a=>[] ;
    verb=v2s ;
    dep=[] ;
    compl=\\a=> embedInCommas ("что" ++ s.s ! Ind) ;
    c=v2s.c
    } ;
  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = {
    adv=\\a=>[] ;
    verb=v2q ;
    dep=[] ;
    compl=\\a=>qs.s ! QDir;
    c=v2q.c
    } ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = {
    adv=\\a=>[] ;
    verb=v2a ;
    dep=[] ;
    compl=case ap.preferShort of {
      PrefFull => (\\a => ap.s ! agrGenNum a ! Inanimate ! v2a.c.c) ;   -- TODO: Check acc dep on animacy
      PrefShort => ap.short
      } ;
    c=v2a.c
    } ;

  -- : VPSlash -> NP -> VP ; -- love it
  ComplSlash vps np = vps ** {
    compl=\\a => vps.compl ! a ++ (applyPrep vps.c np)      -- hasPrep? order?
    } ;

  -- : VV -> VPSlash -> VPSlash ;       -- want to buy
  SlashVV vv vps = vps ** {
    verb=vv.v ;
    dep=(verbInf vps.verb) ++ vps.dep ;
    adv=\\a=>vv.modal ! a ++ vps.adv ! a
    } ;
  -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
  SlashV2VNP v2v np vps = vps ** {
    verb=v2v ;
    dep=(verbInf vps.verb) ++ vps.dep ;
    compl=\\a=>vps.compl ! a ++ (applyPrep vps.c np);   -- hasPrep? Order?
    c=v2v.c
    } ;

  -- : VPSlash -> VP ;         -- love himself
  ReflVP vps = vps ** {
    compl=\\a => vps.compl ! a ++ vps.c.s ++ sam.s ! vps.c.c
    } ;

  -- : Comp -> VP ;            -- be warm
  UseComp comp = {
    adv=\\a=>comp.adv ;
    compl=comp.s ;
    verb=selectCopula comp.cop ;
    dep=[] ;
    } ;

  -- : VP -> Adv -> VP ;        -- sleep here
  AdvVP vp adv = vp ** {compl=\\a => vp.compl ! a ++ adv.s} ;

  -- : VP -> Adv -> VP ;        -- sleep , even though ...
  ExtAdvVP vp adv = vp ** {compl=\\a => vp.compl ! a ++ embedInCommas adv.s};

  -- : AdV -> VP -> VP ;        -- always sleep
  AdVVP adv vp = vp ** {adv=\\a => adv.s ++ vp.adv ! a} ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash vps adv = vps ** {compl=\\a => vps.compl ! a ++ adv.s} ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** {adv=\\a=>adv.s ++ vps.adv ! a} ;

  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  VPSlashPrep vp prep = vp ** {c=prep} ;
  -- : AP -> Comp ;            -- (be) small
  CompAP ap = case ap.preferShort of {
    PrefFull => {s=\\a=>ap.s ! agrGenNum a ! Inanimate ! Ins ; adv=[] ; cop=InsCopula} ;
    PrefShort => {s=ap.short ; adv=[] ; cop=EllCopula}
    };

  -- : NP -> Comp ;            -- (be) the man
  CompNP np = {s=\\a=>np.s ! Ins ; adv=[] ; cop=InsCopula} ;

  -- : Adv -> Comp ;            -- (be) here
  CompAdv adv = {s=\\a=>[] ; adv=adv.s ; cop=NomCopula} ;
  -- : CN -> Comp ;             -- (be) a man/men
  CompCN cn = {s=\\a=>cn.s ! numGenNum (agrGenNum a) ! Ins ; adv=[] ; cop=InsCopula} ;

  -- : VP ;                     -- be
  UseCopula = {adv=\\a=>[] ; verb=copulaIns ; dep=[] ; compl=\\a=>[]} ;
}
