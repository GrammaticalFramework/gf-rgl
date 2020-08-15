abstract TestLang = 
  Grammar, 
  Lexicon
  , TestLexiconGerAbs
  , Construction
  ** {    
  flags startcat=Phr ;
  cat 
    VPSlashSlash ;
  fun
    ReflVPSlash : V3 -> VPSlash ;

    -- SlashV3a : V3 -> VPSlashSlash ; -- unneccessary

    Slash2V4 : V4 -> NP -> VPSlashSlash ;
    Slash3V4 : V4 -> NP -> VPSlashSlash ;
    Slash4V4 : V4 -> NP -> VPSlashSlash ;

    ComplSlashSlash: VPSlashSlash -> NP -> VPSlash ;
  } ;
