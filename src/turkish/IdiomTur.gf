concrete IdiomTur of Idiom = CatTur ** open Prelude, ResTur, SuffixTur in {

lin
  ImpersCl vp = {
    s = \\t,a,p => vp.compl ++ vp.s ! Perf ! VFin t a p (agrP3 Sg)
  } ;
  GenericCl vp = {
    s = \\t,a,p => "insan" ++ vp.compl ++ vp.s ! Perf ! VFin t a p (agrP3 Sg)
  } ;
  ExistNP np = {
    s = \\t,a,p => np.s ! Nom ++ case <t,p> of {
      <Pres,Pos> => "var" ;
      <Pres,Neg> => "yok" ;
      <_,Pos> => "var" ++ mkVerbForms olmak_V ! Perf ! VFin t a Pos (agrP3 Sg) ;
      <_,Neg> => "yok" ++ mkVerbForms olmak_V ! Perf ! VFin t a Pos (agrP3 Sg)
    }
  } ;
  ExistIP ip = {
    s = \\t,a,p => ip.s ++ case p of {Pos => "var"; Neg => "yok"}
  } ;
  ExistNPAdv np adv = {
    s = \\t,a,p => adv.s ++ (ExistNP np).s ! t ! a ! p
  } ;
  ExistIPAdv ip adv = {
    s = \\t,a,p => adv.s ++ (ExistIP ip).s ! t ! a ! p
  } ;
  CleftNP np rs = {
    s = \\t,a,p => rs.s ! np.a ++ np.s ! Nom
  } ;
  CleftAdv adv s = {
    s = \\t,a,p => adv.s ++ s.s
  } ;
  ImpPl1 vp = {s = "hadi" ++ vp.compl ++ vp.s ! Perf ! VImp Pos Pl} ;
  ImpP3 np vp = {s = np.s ! Nom ++ vp.compl ++ vp.s ! Perf ! VImp Pos Sg} ;

  ProgrVP vp = vp ** {
    s = \\asp,vform => vp.s ! Imperf ! vform
  } ;

  SelfAdvVP vp = vp ** {compl = vp.compl ++ "kendi"} ;
  SelfAdVVP vp = vp ** {
    s = \\asp,vf => "kendi" ++ vp.s ! asp ! vf
  } ;
  SelfNP np = np ** {s = \\c => np.s ! c ++ "kendi"} ;

}
