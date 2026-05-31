concrete IdiomFao of Idiom = CatFao ** open ResFao in {

lin
  ImpersCl vp = {
    Converb = "tað" ++ vp.Converb ;
    Indicative = \\t,pol => "tað" ++ vp.Indicative ! t ! pol ! Neuter ! PSg P3 ;
    Nonfinite = "tað" ++ vp.Nonfinite ;
    Participle = \\t => "tað" ++ vp.Participle ! t
  } ;
  GenericCl vp = {
    Converb = "mann" ++ vp.Converb ;
    Indicative = \\t,pol => "mann" ++ vp.Indicative ! t ! pol ! Masc ! PSg P3 ;
    Nonfinite = "mann" ++ vp.Nonfinite ;
    Participle = \\t => "mann" ++ vp.Participle ! t
  } ;
  CleftNP np rs = {
    Converb = "tað er" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! PSg P3 ++ negStr pol ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Nonfinite = "vera" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Participle = \\_ => "verið" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p
  } ;
  CleftAdv adv s = {
    Converb = "tað er" ++ adv.s ++ s.s ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! PSg P3 ++ negStr pol ++ adv.s ++ s.s ;
    Nonfinite = "vera" ++ adv.s ++ s.s ;
    Participle = \\_ => "verið" ++ adv.s ++ s.s
  } ;
  ExistNP np = {
    Converb = "tað er" ++ np.s ! Nom ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ;
    Nonfinite = "vera" ++ np.s ! Nom ;
    Participle = \\_ => "verið" ++ np.s ! Nom
  } ;
  ExistIP ip = {
    s = \\t,pol => "hvat" ++ copula ! t ! persNum ip.n P3 ++ negStr pol
  } ;
  ExistNPAdv np adv = {
    Converb = "tað er" ++ np.s ! Nom ++ adv.s ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ++ adv.s ;
    Nonfinite = "vera" ++ np.s ! Nom ++ adv.s ;
    Participle = \\_ => "verið" ++ np.s ! Nom ++ adv.s
  } ;
  ExistIPAdv ip adv = {
    s = \\t,pol => ip.s ++ copula ! t ! persNum ip.n P3 ++ negStr pol ++ adv.s
  } ;
  ProgrVP vp = {
    Converb = "vera við at" ++ vp.Nonfinite ;
    Indicative = \\t,pol,g,p => copula ! t ! p ++ negStr pol ++ "við at" ++ vp.Nonfinite ;
    Nonfinite = "vera við at" ++ vp.Nonfinite ;
    Participle = \\_ => "verið við at" ++ vp.Nonfinite
  } ;
  ImpPl1 vp = {s = "lat okkum" ++ vp.Nonfinite} ;
  ImpP3 np vp = {s = "lat" ++ np.s ! Acc ++ vp.Nonfinite} ;
  SelfAdvVP vp = vp ** {
    Converb = vp.Converb ++ "sjálvur" ;
    Indicative = \\t,pol,g,p => vp.Indicative ! t ! pol ! g ! p ++ "sjálvur" ;
    Nonfinite = vp.Nonfinite ++ "sjálvur" ;
    Participle = \\t => vp.Participle ! t ++ "sjálvur"
  } ;
  SelfAdVVP = SelfAdvVP ;
  SelfNP np = np ** {s = \\c => np.s ! c ++ "sjálvur"} ;
}
