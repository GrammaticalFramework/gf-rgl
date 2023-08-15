abstract Infinitive =

  Grammar,
  Lexicon
    ** {

fun
  PresPartPassSubjVP : VP -> VP ;      -- (minun) on mentävä
  PresPartPassObjVP : VPSlash -> VP ;  -- (oluesta) on pidettävä

  PastPartPassAdv : NP -> VP -> Adv ;  -- junan mentyä
  AgentPartAP : NP -> VPSlash -> AP ;  -- koiran syömä      

  Inf2InessAdv : NP -> VP -> Adv ;     -- junan mennessä
  Inf2InessPassAdv : NP -> VPSlash -> Adv ; -- junaa odotettaessa

  

}