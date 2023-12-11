abstract Infinitive =

  Grammar - [
    VPSlashPrep,   --- to avoid certain spurious ambiguities, to be fixed
    PassV2         ---- temporarily disabled, to be fixed
    ],
  Lexicon
    ** {

flags startcat = Utt ;

cat
  RAdv ;  -- reflexive adverbs, e.g. mennäkse (ni/si/...)

fun
  UseV2 : V2 -> VP ;        -- to use V2 intransitively, suppressing object
  RAdvVP : VP -> RAdv -> VP ; -- syödä elääkseni

  X_NP, Y_NP, Z_NP : NP ;   -- unknown subjects and objects
  tulla_VV : VV ;           -- tulla (tekemään), explicit future tense

  PresPartPassSubjVP : VP -> VP ;      -- (minun) on mentävä
  PresPartPassObjVP : VPSlash -> VP ;  -- (oluesta) on pidettävä

  PastPartPassAdv : NP -> VP -> Adv ;  -- junan mentyä

  PresPartActAP  : VP -> AP ;        -- (lihaa) syövä      
  PastPartActAP  : VP -> AP ;        -- (lihaa) syönyt
  PresPartPassAP : VPSlash -> AP ;   -- (tänään) syötävä      
  PastPartPassAP : VPSlash -> AP ;   -- (tänään) syöty

  AgentPartAP   : NP -> VPSlash -> AP ;  -- koiran syömä      

  Inf1LongRAdv : VP -> RAdv ; -- mennäkse (ni/si/...)
  
  Inf2InessAdv : NP -> VP -> Adv ;  -- junan mennessä
  Inf2InessRAdv : VP -> RAdv ;       -- mennessään
  
  Inf2InessPassAdv    : VP -> Adv ;             -- odotettaessa (junaa), touhuttaessa (junan kanssa)
  Inf2InessPassInvAdv : NP -> VPSlash -> Adv ;  -- junaa odotettaessa, junan kanssa touhutessa

  Inf2InstrAdv    : VP -> Adv ;             -- odottaen (junaa)
  Inf2InstrInvAdv : NP -> VPSlash -> Adv ;  -- junaa odottaen

  Inf3InessAdv    : VP -> Adv ;             -- odottamassa (junaa)
  Inf3InessInvAdv : NP -> VPSlash -> Adv ;  -- junaa odottamassa
  
  Inf3ElatAdv     : VP -> Adv ;             -- odottamasta (junaa)
  Inf3ElatInvAdv  : NP -> VPSlash -> Adv ;  -- junaa odottamasta
  
  Inf3IllatAdv    : VP -> Adv ;             -- odottamaan (junaa)
  Inf3IllatInvAdv : NP -> VPSlash -> Adv ;  -- junaa odottamaan
  
  Inf3AdessAdv    : VP -> Adv ;             -- odottamalla (junaa)
  Inf3AdessInvAdv : NP -> VPSlash -> Adv ;  -- junaa odottamalla
  
  Inf3AbessAdv    : VP -> Adv ;             -- odottamatta (junaa)
  Inf3AbessInvAdv : NP -> VPSlash -> Adv ;  -- junaa odottamatta

  ComplPresPartActVS    : VS -> NP -> VP -> VP ;  -- sanoa junan menevän
  ComplPastPartActVS    : VS -> NP -> VP -> VP ;  -- sanoa junan menneen
  
  ComplPresPartActReflVS : VS -> VP -> VP ;        -- sanoa menevänsä
  ComplPastPartActReflVS : VS -> VP -> VP ;        -- sanoa menneensä

  ComplPresPartPassVS   : VS -> NP -> VPSlash -> VP ;  -- sanoa junaa odotettavan
  ComplPastPartPassVS   : VS -> NP -> VPSlash -> VP ;  -- sanoa junaa odotetun

  

}