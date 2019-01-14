abstract UDTranslate =
  Translate,
  Verb [UseCopula],
  Extensions [GenNP,ComplVPIVV,UseQuantPN]
     ** {

flags startcat = Top ;

cat
  Top ;
  Punct ;
  Numer ;

fun
  TopPhr : Phr -> Top ;
  TopPhrPunct : Phr -> Punct -> Top ;
  ParaTaxis   : Phr -> Phr -> Phr ;
  
  StringPunct : String -> Punct ;

  StringCard  : String -> Card ; ---- TODO proper numerals
  StringPN    : String -> PN ;
  PrefixPN    : PN -> PN -> PN ;
  PrefixCard  : Card -> Card -> Card ; 

-- backup functions for unknown dependents

cat
  Backups ;
  Backup ;
  [Backup] {0} ;

  QVPS ;  -- VPS with IPs inside

fun
  MkBackups : [Backup] -> [Backup] -> Backups ; -- pre- and post-backups

  BackupAdV: AdV -> Backups -> AdV ;
  BackupAdv: Adv -> Backups -> Adv ;
  BackupAP : AP  -> Backups -> AP ;
  BackupNP : NP  -> Backups -> NP ;
  BackupVP : VP  -> Backups -> VP ;
  BackupVPS : VPS  -> Backups -> VPS ;
  BackupVPSlash : VPSlash  -> Backups -> VPSlash ;
  BackupV2V : V2V  -> Backups -> V2V ;
  BackupQCl : QCl  -> Backups -> QCl ;
  BackupCl : Cl  -> Backups -> Cl ;
  BackupClSlash : ClSlash  -> Backups -> ClSlash ;
  BackupS  : S   -> Backups -> S ;
  BackupQS : QS  -> Backups -> QS ;
  BackupSC : SC  -> Backups -> SC ;
  BackupSubj : Subj -> Backups -> Subj ;
  BackupUtt : Utt -> Backups -> Utt ;
  BackupPhr : Phr -> Backups -> Phr ;
  BackupTop : Top -> Backups -> Top ;

  AdvBackup   : Adv -> Backup ;
  APBackup    : AP -> Backup ;
  NPBackup    : NP -> Backup ;
  VPBackup    : VP -> Backup ;
  VPSBackup   : VPS -> Backup ;
  VPSlashBackup : VPSlash -> Backup ;
  DetBackup   : Det -> Backup ;
  QuantBackup : Quant -> Backup ;
  InterjBackup : Interj -> Backup ;
  OrdBackup   : Ord -> Backup ;
  PNBackup    : PN -> Backup ;
  PrepBackup  : Prep -> Backup ;
  PunctBackup : Punct -> Backup ;
  ConjBackup  : Conj -> Backup ;
  SymbBackup  : Symb -> Backup ;
  SBackup     : S -> Backup ;
  QSBackup    : QS -> Backup ;
  SCBackup    : SC -> Backup ;
  SubjBackup  : Subj -> Backup ;
  UttBackup   : Utt -> Backup ;
  PhrBackup   : Phr -> Backup ;
  TopBackup   : Top -> Backup ;

-- extra lexicon to make it easier to connect to word-based dep trees
  all_Det   : Det ;
  no_Det    : Det ;
  these_Det : Det ;
  this_Det  : Det ;
  that_Det  : Det ;
  those_Det : Det ;
  these_NP  : NP ;
  this_NP   : NP ;
  that_NP   : NP ;
  those_NP  : NP ;
  which_Det : IDet ;

---- TODO: treat numbers with the Numeral grammar
  one_Card   : Card ;
  two_Card   : Card ;
  three_Card : Card ;
  four_Card  : Card ;
  five_Card  : Card ;
  ten_Card   : Card ;

---------- for tenses

  PredSCVPS     : SC -> VPS -> S ;  -- that he grows is evident
  ExplPredSCVPS : SC -> VPS -> S ;  -- it is evident that he grows

  SlashVPS    : Temp -> Pol -> NP -> VPSlash -> SSlash ; 
  AdvVPS      : VPS -> Adv -> VPS   ;
  AddAdvQVPS  : QVPS -> IAdv -> QVPS ;
  RelVPS      : RP -> VPS -> RS      ;
  RelSSlash   : RP -> SSlash  -> RS  ;
  QuestVPS    : IP -> VPS -> QS ; 
  QuestSSlash : IP -> SSlash -> QS ;
  QuestSIAdv  : IAdv -> S -> QS ;   
  MkQVPS      : Temp -> Pol -> QVP -> QVPS ;
  UttVPS      : VPS -> Utt ; 

  ImpersS     : VPS -> S ;
  GenericS    : VPS -> S ;

}
