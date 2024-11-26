concrete VerbRus of Verb = CatRus ** open ResRus, Prelude, Coordination in {

lin
  -- : V -> VP ;        -- sleep
  UseV v = {
    adv = \\a=>[] ;
    verb = v ;
    dep=[] ;
    compl = \\_,_ => [] ;
    p = Pos
    } ;

  -- : V2 -> VP ;         -- be loved
  PassV2 v2 = {
    adv = \\a=>[] ;
    verb = passivate v2 ;
    dep=[] ;
    compl = \\p,a => [] ;
    p = Pos
  } ;

  -- : VV -> VP -> VP ;  -- want to run
  ComplVV vv vp = vp ** {
    verb=vv.v ;
    dep=verbInf vp.verb ++ vp.dep ;
    adv=\\a=>vv.modal ! a ++ vp.adv ! a ;
    p = Pos
    } ;

  -- : VS -> S -> VP ;  -- say that she runs
  ComplVS vs s = {
    verb = vs ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\_,_ => comma ++ "что" ++ s.s ! Ind ;
    p = Pos
    } ;

  -- : VQ -> QS -> VP ;  -- wonder who runs
  ComplVQ vq qs = {
    verb = vq ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\_,_ => comma ++ "что" ++ qs.s ! QDir ;
    p = Pos
    } ;


  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = {
    verb=va ;
    dep=[] ;
    adv=\\a=>[] ;
    compl=\\_ => case ap.preferShort of {
      PreferFull => (\\a => ap.s ! agrGenNum a ! Inanimate ! Ins) ;
      PrefShort => ap.short
      } ;
    p = Pos
    } ;

  -- : V2 -> VPSlash ;  -- love (it)
  SlashV2a v2 = slashV v2 v2.c ;

  -- : V3 -> NP -> VPSlash ;  -- give it (to her)
  Slash2V3 v3 np = insertSlashObj1 (\\p,_ => applyPolPrep p v3.c np) v3.c2 (slashV v3 v3.c2) ;

  -- : V3 -> NP -> VPSlash ;  -- give (it) to her
  Slash3V3 v3 np = insertSlashObj2 (\\p,_ => applyPolPrep p v3.c2 np) v3.c (slashV v3 v3.c) ;

  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = insertSlashObj2 (\\_,a => verbInf vp.verb) v2v.c (slashV v2v v2v.c) ;

  -- : V2S -> S -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s = insertSlashObj2 (\\_,a=> embedInCommas ("что" ++ s.s ! Ind)) v2s.c (slashV v2s v2s.c) ;

  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = insertSlashObj2 (\\_,_=> qs.s ! QDir) v2q.c (slashV v2q v2q.c);

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = insertSlashObjA ap v2a.c (slashV v2a v2a.c) ;

 --  : VPSlash -> NP -> VP ; -- love it
 ComplSlash vps np =
     {verb   = vps.verb ;
          adv  = vps.adv ;
          dep = vps.dep ;
          compl = \\p,a => vps.compl1 ! p ! a  ++ applyPolPrep p vps.c np ++ vps.compl2 ! p ! a  ;
          p = Pos
         } ;

  -- : VV -> VPSlash -> VPSlash ;       -- want to buy
  SlashVV vv vps = vps ** {
    verb=vv.v ;
    dep=(verbInf vps.verb) ++ vps.dep ;
    adv=\\a=>vv.modal ! a ++ vps.adv ! a ;
    p = Pos
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
    compl=\\p,a => vps.compl1 ! p ! a ++ vps.c.s ++ vps.compl2 ! p ! a ++ sebya.s ! vps.c.c
    } ;

  -- : Comp -> VP ;            -- be warm
  UseComp comp = {
    adv=\\a => comp.adv ;
    verb=selectCopula comp.cop ;
    dep=[] ;
    compl=\\p => comp.s;
    p = Pos
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
  AdVVP adv vp = vp ** {adv=\\a => adv.s ++ vp.adv ! a; p = orPol adv.p vp.p} ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash vps adv = vps ** {compl1=\\p,a => vps.compl1 ! p ! a ++ adv.s; isSimple=False} ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** {adv=\\a=>adv.s ++ vps.adv ! a} ;

  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  VPSlashPrep vp prep = vp ** {c = prep ; compl1 = vp.compl ; compl2 = \\_,_ => []; dep=[]; isSimple=False} ;

  -- : AP -> Comp ;            -- (be) small
  CompAP ap = case ap.preferShort of {
    PreferFull => {s=\\a=>ap.s ! agrGenNum a ! Inanimate ! Ins ; adv=[] ; cop=InsCopula} ;
    PrefShort => {s=ap.short ; adv=[] ; cop=EllCopula}
    };

  -- : NP -> Comp ;            -- (be) the man
  CompNP np = {s=\\a=>np.s ! Nom ; adv=[] ; cop=NomCopula} ;

  -- : Adv -> Comp ;            -- (be) here
  CompAdv adv = {s=\\a=>[] ; adv=adv.s ; cop=ExplicitCopula} ;
  -- : CN -> Comp ;             -- (be) a man/men
  CompCN cn = {
    s=\\a=>cn.s ! numGenNum (agrGenNum a) ! Nom ;
    adv=[] ;
    cop=NomCopula
    } ;

  -- : VP ;                     -- be
  UseCopula = {adv=\\a=>[] ; verb=copulaIns ; dep=[] ; compl=\\p,a=>[]; p=Pos} ;
}
