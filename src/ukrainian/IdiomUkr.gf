concrete IdiomUkr of Idiom = CatUkr ** open ResUkr, (R = ParamX) in {

lin
  ImpersCl vp = {
    s = \\t,pol => vp.s ! t ! pol ! Neuter ! Sg ! P3
  } ;
  GenericCl vp = {
    s = \\t,pol => "люди" ++ vp.s ! t ! pol ! Masc ! Pl ! P3
  } ;
  CleftNP np rs = {
    s = \\t,pol => "це" ++ np.s ! Nom ++ rs.s ! np.g ! np.n
  } ;
  CleftAdv adv s = {
    s = \\t,pol => "це" ++ adv.s ++ s.s
  } ;
  ExistNP np = {
    s = \\t,pol => case pol of {
      R.Pos => "є" ++ np.s ! Nom ;
      R.Neg => "немає" ++ np.s ! Gen
    }
  } ;
  ExistIP ip = {
    s = \\t,pol => "що є" ++ ip.s ! Nom
  } ;
  ExistNPAdv np adv = {
    s = \\t,pol => case pol of {
      R.Pos => adv.s ++ "є" ++ np.s ! Nom ;
      R.Neg => adv.s ++ "немає" ++ np.s ! Gen
    }
  } ;
  ExistIPAdv ip adv = {
    s = \\t,pol => adv.s ++ "що є" ++ ip.s ! Nom
  } ;
  ProgrVP vp = vp ;
  ImpPl1 vp = {s = "давайте" ++ vp.inf} ;
  ImpP3 np vp = {s = "нехай" ++ np.s ! Nom ++ vp.s ! R.Pres ! R.Pos ! np.g ! np.n ! np.p} ;
  SelfAdvVP vp = vp ** {
    s = \\t,pol,g,n,p => vp.s ! t ! pol ! g ! n ! p ++ "сам"
  } ;
  SelfAdVVP vp = vp ** {
    s = \\t,pol,g,n,p => "сам" ++ vp.s ! t ! pol ! g ! n ! p
  } ;
  SelfNP np = np ** {
    s = \\c => np.s ! c ++ "сам"
  } ;
}
