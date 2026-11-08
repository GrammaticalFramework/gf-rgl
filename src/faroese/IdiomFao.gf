concrete IdiomFao of Idiom = CatFao ** open ResFao in {

lin
  ImpersCl vp = {
    Converb = "tað" ++ vp.Converb ;
    Indicative = \\t,pol => "tað" ++ vp.Indicative ! t ! pol ! Neuter ! PSg P3 ;
    Interrogative = \\t,pol => vp.Finite ! t ! PSg P3 ++ "tað" ++ vp.Remainder ! pol ! Neuter ! PSg P3 ;
    Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ;
    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ vp.Nonfinite ;
    Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ;
    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ vp.Nonfinite ;
    Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ vp.Converb ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ vp.Converb ;
    Nonfinite = "tað" ++ vp.Nonfinite ;
    Participle = \\t => "tað" ++ vp.Participle ! t
  } ;
  GenericCl vp = {
    Converb = "mann" ++ vp.Converb ;
    Indicative = \\t,pol => "mann" ++ vp.Indicative ! t ! pol ! Masc ! PSg P3 ;
    Interrogative = \\t,pol => vp.Finite ! t ! PSg P3 ++ "mann" ++ vp.Remainder ! pol ! Masc ! PSg P3 ;
    Future = \\pol => "mann" ++ futureAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ;
    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "mann" ++ negStr pol ++ vp.Nonfinite ;
    Conditional = \\pol => "mann" ++ conditionalAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ;
    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "mann" ++ negStr pol ++ vp.Nonfinite ;
    Anterior = \\t,pol => "mann" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ vp.Converb ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "mann" ++ negStr pol ++ vp.Converb ;
    Nonfinite = "mann" ++ vp.Nonfinite ;
    Participle = \\t => "mann" ++ vp.Participle ! t
  } ;
  CleftNP np rs = {
    Converb = "tað er" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! PSg P3 ++ negStr pol ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Interrogative = \\t,pol => copula ! t ! PSg P3 ++ "tað" ++ negStr pol ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ "verið" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ "verið" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Nonfinite = "vera" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p ;
    Participle = \\_ => "verið" ++ np.s ! Nom ++ rs.s ! np.g ! persNum np.n np.p
  } ;
  CleftAdv adv s = {
    Converb = "tað er" ++ adv.s ++ s.s ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! PSg P3 ++ negStr pol ++ adv.s ++ s.s ;
    Interrogative = \\t,pol => copula ! t ! PSg P3 ++ "tað" ++ negStr pol ++ adv.s ++ s.s ;
    Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ "vera" ++ adv.s ++ s.s ;
    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ adv.s ++ s.s ;
    Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ "vera" ++ adv.s ++ s.s ;
    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ adv.s ++ s.s ;
    Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ "verið" ++ adv.s ++ s.s ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ "verið" ++ adv.s ++ s.s ;
    Nonfinite = "vera" ++ adv.s ++ s.s ;
    Participle = \\_ => "verið" ++ adv.s ++ s.s
  } ;
  ExistNP np = {
    Converb = "tað er" ++ np.s ! Nom ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ;
    Interrogative = \\t,pol => copula ! t ! persNum np.n P3 ++ "tað" ++ negStr pol ++ np.s ! Nom ;
    Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ;
    Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ;
    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ;
    Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ "verið" ++ np.s ! Nom ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ "verið" ++ np.s ! Nom ;
    Nonfinite = "vera" ++ np.s ! Nom ;
    Participle = \\_ => "verið" ++ np.s ! Nom
  } ;
  ExistIP ip = {
    s = \\t,pol => "hvat" ++ copula ! t ! persNum ip.n P3 ++ negStr pol ;
    anterior = \\t,pol => "hvat" ++ perfectAux ! t ! persNum ip.n P3 ++ negStr pol ++ "verið" ;
    future = \\pol => "hvat" ++ futureAux ! persNum ip.n P3 ++ negStr pol ++ "vera" ;
    conditional = \\pol => "hvat" ++ conditionalAux ! persNum ip.n P3 ++ negStr pol ++ "vera"
  } ;
  ExistNPAdv np adv = {
    Converb = "tað er" ++ np.s ! Nom ++ adv.s ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ++ adv.s ;
    Interrogative = \\t,pol => copula ! t ! persNum np.n P3 ++ "tað" ++ negStr pol ++ np.s ! Nom ++ adv.s ;
    Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ++ adv.s ;
    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ++ adv.s ;
    Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ "vera" ++ np.s ! Nom ++ adv.s ;
    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ np.s ! Nom ++ adv.s ;
    Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ "verið" ++ np.s ! Nom ++ adv.s ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ "verið" ++ np.s ! Nom ++ adv.s ;
    Nonfinite = "vera" ++ np.s ! Nom ++ adv.s ;
    Participle = \\_ => "verið" ++ np.s ! Nom ++ adv.s
  } ;
  ExistIPAdv ip adv = {
    s = \\t,pol => ip.s ++ copula ! t ! persNum ip.n P3 ++ negStr pol ++ adv.s ;
    anterior = \\t,pol => ip.s ++ perfectAux ! t ! persNum ip.n P3 ++ negStr pol ++ "verið" ++ adv.s ;
    future = \\pol => ip.s ++ futureAux ! persNum ip.n P3 ++ negStr pol ++ "vera" ++ adv.s ;
    conditional = \\pol => ip.s ++ conditionalAux ! persNum ip.n P3 ++ negStr pol ++ "vera" ++ adv.s
  } ;
  ProgrVP vp = {
    Converb = "vera við at" ++ vp.Nonfinite ;
    Imperative = \\n => case n of {Sg => "ver" ; Pl => "verið"} ++ "við at" ++ vp.Nonfinite ;
    Indicative = \\t,pol,g,p => copula ! t ! p ++ negStr pol ++ "við at" ++ vp.Nonfinite ;
    Finite = copula ;
    Remainder = \\pol,_,_ => negStr pol ++ "við at" ++ vp.Nonfinite ;
    Nonfinite = "vera við at" ++ vp.Nonfinite ;
    Participle = \\_ => "verið við at" ++ vp.Nonfinite
  } ;
  ImpPl1 vp = {s = "lat okkum" ++ vp.Nonfinite} ;
  ImpP3 np vp = {s = "lat" ++ np.s ! Acc ++ vp.Nonfinite} ;
  SelfAdvVP vp = vp ** {
    Converb = vp.Converb ++ "sjálvur" ;
    Indicative = \\t,pol,g,p => vp.Indicative ! t ! pol ! g ! p ++ "sjálvur" ;
    Remainder = \\pol,g,p => vp.Remainder ! pol ! g ! p ++ "sjálvur" ;
    Nonfinite = vp.Nonfinite ++ "sjálvur" ;
    Participle = \\t => vp.Participle ! t ++ "sjálvur"
  } ;
  SelfAdVVP = SelfAdvVP ;
  SelfNP np = np ** {s = \\c => np.s ! c ++ "sjálvur"} ;
}
