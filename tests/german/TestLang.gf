abstract TestLang = 
  Grammar - [SlashVP, RelSlash],
  TestLexiconGerAbs
  , Construction
  ** {    
  flags startcat=Phr ;

  fun
    SlashV2Vneg : V2V -> VP -> VPSlash ; -- negative use of VP: promise, not to vp
  cat 
    VPSlashSlash ;
  fun
    ReflVPSlash : V3 -> VPSlash ;

    -- SlashV3a : V3 -> VPSlashSlash ; -- unneccessary

    Slash2V4 : V4 -> NP -> VPSlashSlash ;
    Slash3V4 : V4 -> NP -> VPSlashSlash ;
    Slash4V4 : V4 -> NP -> VPSlashSlash ;

    ComplSlashSlash: VPSlashSlash -> NP -> VPSlash ;

    -- Passive
    PastPartAP  : VPSlash -> AP ;  -- lost (opportunity) ; (opportunity) lost in space
    PassVPSlash : VPSlash -> VP ;  -- from ExtraGer, to be corrected

    PassV2S : V2S -> S -> VP ;
    PassV2Q : V2Q -> QS -> VP ;
    PassV2V : V2V -> VP -> VP ;

    Pass3V3 : V3 -> NP -> VP ;  -- den Beweis erklärt bekommen
    Pass2V3 : V3 -> NP -> VP ;  -- uns erklärt werden ; Eng give_V3[indir,dir]: we are given the book

    Pass2V4 : V4 -> NP -> VPSlash ; -- bei dir (für Gold) gekauft werden

  cat
    ClauseSlash ;

  fun
    RelSlash : RP -> ClauseSlash -> RCl ;
    SlashVP : NP -> VPSlash -> ClauseSlash ;

  } ;
