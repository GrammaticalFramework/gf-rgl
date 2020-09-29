concrete VerbRus of Verb = CatRus ** open ResRus, Prelude, Coordination in {

lin
  -- : V -> VP ;        -- sleep
  UseV v = {
    adv = \\a=>[] ;
    verb = v ;
    dep=[] ;
    compl = \\_,_ => []
    } ;

  -- : V2 -> VP ;         -- be loved
  PassV2 v2 = {
    adv = \\a=>[] ;
    verb = passivate v2 ;
    dep=[] ;
    compl = \\p,a => []
  } ;

  -- : VV -> VP -> VP ;  -- want to run
  ComplVV vv vp = vp ** {
    verb=vv.v ;
    dep=verbInf vp.verb ++ vp.dep ;
    adv=\\a=>vv.modal ! a ++ vp.adv ! a
    } ;

  -- : VS -> S -> VP ;  -- say that she runs
  ComplVS vs s = {
    verb = vs ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\_,_ => comma ++ "что" ++ s.s ! Ind
    } ;

  -- : VQ -> QS -> VP ;  -- wonder who runs
  ComplVQ vq qs = {
    verb = vq ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\_,_ => comma ++ "что" ++ qs.s ! QDir
    } ;


  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = {
    verb=va ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\_ => case ap.preferShort of {
      PreferFull => (\\a => ap.s ! agrGenNum a ! Inanimate ! Ins) ;
      PrefShort => ap.short
      }
    } ;

  -- : V2 -> VPSlash ;  -- love (it)
  SlashV2a v2 = {
    adv=\\a=>[] ;
    verb=v2 ;
    dep=[] ;
    compl=\\_,_ => [] ;
    c=v2.c
    } ;

  -- : V3 -> NP -> VPSlash ;  -- give it (to her)
  Slash2V3 v3 np = {
    adv=\\a=>[] ;
    verb=v3 ;
    dep=[] ;
    compl=\\p,a => applyPolPrep p v3.c np ;
    c=v3.c2
    } ;

  -- : V3 -> NP -> VPSlash ;  -- give (it) to her
  Slash3V3 v3 np = {
    adv=\\a=>[] ;
    verb=v3 ;
    dep=[] ;
    compl=\\p,a => applyPolPrep p v3.c2 np ;
    c=v3.c
    } ;

  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = vp ** {
    verb=v2v ;
    dep=verbInf vp.verb ;
    c=v2v.c
    } ;

  -- : V2S -> S -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s = {
    adv=\\a=>[] ;
    verb=v2s ;
    dep=[] ;
    compl=\\_,a=> embedInCommas ("что" ++ s.s ! Ind) ;
    c=v2s.c
    } ;
  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = {
    adv=\\a=>[] ;
    verb=v2q ;
    dep=[] ;
    compl=\\_,a=>qs.s ! QDir;
    c=v2q.c
    } ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = {
    adv=\\a=>[] ;
    verb=v2a ;
    dep=[] ;
    compl=table {
      Pos => case ap.preferShort of {
        PreferFull => \\a => ap.s ! agrGenNum a ! Animate ! v2a.c.c ;
        PrefShort => ap.short
        } ;
      Neg => case ap.preferShort of {
        PreferFull => case neggen v2a.c of {
            False => \\a => ap.s ! agrGenNum a ! Animate ! v2a.c.c ;
            True => \\a => ap.s ! agrGenNum a ! Animate ! Gen
          } ;
        PrefShort => ap.short
        }
      } ;
    c={s="" ; c=Acc ; neggen=True ; hasPrep=False}
    } ;

  -- : VPSlash -> NP -> VP ; -- love it
  ComplSlash vps np = vps ** {
    compl=\\p,a => applyPolPrep p vps.c np ++ vps.compl ! p ! a
    } ;

  -- : VV -> VPSlash -> VPSlash ;       -- want to buy
  SlashVV vv vps = vps ** {
    verb=vv.v ;
    dep=(verbInf vps.verb) ++ vps.dep ;
    adv=\\a=>vv.modal ! a ++ vps.adv ! a
    } ;

  {- This is very heavy, but can be replaced (see todo.txt)
  -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
  SlashV2VNP v2v np vps = vps ** {
    verb=v2v ;
    compl=\\p,a => vps.compl ! p ! a ++ applyPolPrep p vps.c np ;
    dep=(verbInf vps.verb) ++ vps.dep ;
    c=v2v.c
    } ;
  -}
  SlashV2VNP = variants {} ;

  -- : VPSlash -> VP ;         -- love himself
  ReflVP vps = vps ** {
    compl=\\p,a => vps.compl ! p ! a ++ vps.c.s ++ sebya.s ! vps.c.c
    } ;

  -- : Comp -> VP ;            -- be warm
  UseComp comp = {
    adv=\\a => comp.adv ;
    verb=selectCopula comp.cop ;
    dep=[] ;
    compl=\\p => comp.s
    } ;

  -- : VP -> Adv -> VP ;        -- sleep here
  AdvVP vp adv = vp ** {
    compl=\\p,a => vp.compl ! p ! a ++ adv.s
    } ;

  -- : VP -> Adv -> VP ;        -- sleep , even though ...
  ExtAdvVP vp adv = vp ** {
    compl=\\p,a => vp.compl ! p ! a ++ embedInCommas adv.s
    } ;

  -- : AdV -> VP -> VP ;        -- always sleep
  AdVVP adv vp = vp ** {adv=\\a => adv.s ++ vp.adv ! a} ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash vps adv = vps ** {compl=\\p,a => vps.compl ! p ! a ++ adv.s} ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** {adv=\\a=>adv.s ++ vps.adv ! a} ;

  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  VPSlashPrep vp prep = vp ** {c=prep} ;
  -- : AP -> Comp ;            -- (be) small
  CompAP ap = case ap.preferShort of {
    PreferFull => {s=\\a=>ap.s ! agrGenNum a ! Inanimate ! Ins ; adv=[] ; cop=InsCopula} ;
    PrefShort => {s=ap.short ; adv=[] ; cop=EllCopula}
    };

  -- : NP -> Comp ;            -- (be) the man
  CompNP np = {s=\\a=>np.s ! Ins ; adv=[] ; cop=InsCopula} ;

  -- : Adv -> Comp ;            -- (be) here
  CompAdv adv = {s=\\a=>[] ; adv=adv.s ; cop=ExplicitCopula} ;
  -- : CN -> Comp ;             -- (be) a man/men
  CompCN cn = {
    s=\\a=>cn.s ! numGenNum (agrGenNum a) ! Ins ;
    adv=[] ;
    cop=InsCopula
    } ;

  -- : VP ;                     -- be
  UseCopula = {adv=\\a=>[] ; verb=copulaIns ; dep=[] ; compl=\\p,a=>[]} ;
}
