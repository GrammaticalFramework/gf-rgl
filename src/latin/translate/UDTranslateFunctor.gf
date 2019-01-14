incomplete concrete UDTranslateFunctor of UDTranslate =
  Translate,
  Verb [UseCopula],
  Extensions [GenNP,ComplVPIVV,UseQuantPN]
     ** open Syntax, Symbolic, Extensions, Prelude in {

lincat
  Top = {s : Str} ;
  Punct = {s : Str} ;

lin
  TopPhr utt = utt ;
  TopPhrPunct utt punct = {s = utt.s ++ punct.s} ;
  ParaTaxis utt1 utt2 = {s = utt1.s ++ "-" ++ utt2.s} ;

  StringPunct s = {s = s.s} ;
  StringCard s = {s = \\_ => s.s ; n = plural} ;

-- backup functions for unknown dependents

lincat
  Backups = {s1,s2 : Str} ;
  Backup = {s : Str} ;
  [Backup] = {s : Str} ;

  QVPS = Extensions.VPS ;

oper
  backupsAdv : Backups -> Adv = \b -> lin Adv {s = bracket b.s2} ;
  bracket : Str -> Str = \s -> "[" ++ s ++ "]" ;

lin
  MkBackups b1 b2 = {s1 = b1.s ; s2 = b2.s} ;
  BaseBackup = {s = []} ;
  ConsBackup b bs = {s = b.s ++ bs.s} ;


lin
  BackupAdV adv b = lin AdV {s = bracket b.s1 ++ adv.s ++ bracket b.s2} ;
  BackupAdv adv b = lin Adv {s = bracket b.s1 ++ adv.s ++ bracket b.s2} ;
  BackupAP  xp b = mkAP (lin AdA {s = bracket b.s1}) (mkAP xp (backupsAdv b)) ;
  BackupNP  np b = np ** {s = \\c => bracket b.s1 ++ np.s ! c ++ bracket b.s2} ;
  BackupVPS vps b = vps ** {s = \\c => bracket b.s1 ++ vps.s ! c ++ bracket b.s2} ;
  BackupV2V v b = v ** {s = \\c => bracket b.s1 ++ v.s ! c ++ bracket b.s2} ;
  BackupVP  xp b = mkVP (lin AdV {s = bracket b.s1}) (mkVP xp (backupsAdv b)) ; ---- AdV not always the right place
  BackupVPSlash xp b = xp ** mkVP (lin AdV {s = bracket b.s1}) (mkVP <xp : VP> (backupsAdv b)) ; ---- AdV not always the right place
  BackupCl cl b = {s = \\t,a,p,o => bracket b.s1 ++ cl.s ! t ! a ! p ! o ++ bracket b.s2} ;
  BackupQCl cl b = {s = \\t,a,p,o => bracket b.s1 ++ cl.s ! t ! a ! p ! o ++ bracket b.s2} ;
  BackupClSlash cl b = cl ** {s = \\t,a,p,o => bracket b.s1 ++ cl.s ! t ! a ! p ! o ++ bracket b.s2} ;
  BackupS s b = {s = bracket b.s1 ++ s.s ++ bracket b.s2} ;
  BackupQS s b = {s = \\o => bracket b.s1 ++ s.s ! o ++ bracket b.s2} ;
  BackupSC sc b = lin Utt {s = bracket b.s1 ++ sc.s ++ bracket b.s2} ;
  BackupSubj adv b = lin Subj {s = bracket b.s1 ++ adv.s ++ bracket b.s2} ;
  BackupUtt utt b = lin Utt {s = bracket b.s1 ++ utt.s ++ bracket b.s2} ;
  BackupPhr utt b = lin Phr {s = bracket b.s1 ++ utt.s ++ bracket b.s2} ;
  BackupTop top b = lin Top {s = bracket b.s1 ++ top.s ++ bracket b.s2} ;

  APBackup ap = mkUtt ap ;
  AdvBackup adv = mkUtt adv ;
  DetBackup det = mkUtt (mkNP det) ;
  InterjBackup i = mkUtt i ;
  NPBackup np = mkUtt np ;
  VPBackup vp = mkUtt vp ;
  ---VPSBackup vps = mkUtt vps ;
  VPSlashBackup vp = mkUtt <vp : VP> ;
  QuantBackup q = mkUtt (mkNP (mkDet q singularNum)) ;
  OrdBackup ord = mkUtt (mkAP ord) ;
  PNBackup pn = mkUtt (mkNP pn) ;
  ---PrepBackup p = mkUtt (mkAdv p) ;
  PunctBackup p = p ;
  ConjBackup conj = mkUtt (mkNP conj (symb []) (symb [])) ; --- to get both discontinuous parts
  SymbBackup sy = sy ;
  SBackup s = mkUtt s ;
  QSBackup qs = mkUtt qs ;
  SCBackup sc = mkUtt sc ;
  SubjBackup subj = subj ;
  UttBackup utt = utt ;
  PhrBackup phr = phr ;
  TopBackup top = top ;


  these_Det = mkDet this_Quant pluralNum ;
  this_Det = mkDet this_Quant singularNum ;
  that_Det = mkDet that_Quant singularNum ;
  those_Det = mkDet that_Quant pluralNum ;
  these_NP = mkNP (mkDet this_Quant pluralNum) ;
  this_NP = mkNP (mkDet this_Quant singularNum) ;
  that_NP = mkNP (mkDet that_Quant singularNum) ;
  those_NP = mkNP (mkDet that_Quant pluralNum) ;
  which_Det = mkIDet which_IQuant ;
  no_Det = mkDet no_Quant ;
  all_Det = every_Det ; ---- used in translations, excluded in Eng

---- TODO: treat numbers with the Numeral grammar
lin
  one_Card = mkCard "1" ;
  two_Card = mkCard "2" ;
  three_Card = mkCard "3" ;
  four_Card = mkCard "4" ;
  five_Card = mkCard "5" ;
  ten_Card = mkCard "10" ;


  PredSCVPS sc vps = Extensions.PredVPS (symb (mkSymb sc.s)) vps ;
  ExplPredSCVPS sc vps = Extensions.PredVPS it_NP (AdvVPS vps (lin Adv sc)) ;

}
