concrete IdiomGla of Idiom = CatGla ** open Prelude, ResGla in {
lin
  ImpersCl vp = {subj = "e" ; n = Sg ; pred = vp} ;
  ExistNP np = {subj = linNP np ; n = agrNumber np.a ; pred = idiomBiV} ;
  ExistIP ip = {s = \\_,_,_ => "dè tha ann" ++ ip.s} ;
  GenericCl vp = {subj = "neach" ; n = Sg ; pred = vp} ;
  CleftNP np rs = {subj = linNP np ; n = agrNumber np.a ; pred = appendVP idiomBiV rs.s} ;
  CleftAdv adv s = {subj = adv.s ; n = Sg ; pred = appendVP idiomBiV s.s} ;
  ExistNPAdv np adv = {subj = linNP np ; n = agrNumber np.a ; pred = appendVP idiomBiV adv.s} ;
  ExistIPAdv ip adv = {s = \\_,_,_ => "dè tha ann" ++ ip.s ++ adv.s} ;
  ProgrVP vp = vp ;
  ImpPl1 vp = {s = "rachamaid" ++ vp.s} ;
  ImpP3 np vp = {s = "leig le" ++ linNP np ++ vp.s} ;
  SelfAdvVP vp = appendVP vp "fhèin" ;
  SelfAdVVP vp = prependVP "fhèin" vp ;
  SelfNP np = np ** {s = \\c => np.s ! c ++ "fhèin"} ;

oper
  idiomBiV : LinV = {
    s = "bi" ; conditional = table {Sg => "bhiodh" ; Pl => "bhiodh"} ;
    imperative = table {
      P1 => table {Sg => "bitheam" ; Pl => "bitheamaid"} ;
      P2 => table {Sg => "bi" ; Pl => "bithibh"} ;
      P3 => table {Sg => "bitheadh" ; Pl => "bitheadh"}
      } ;
    future = table {Indep => "bidh" ; Dep => "bi"} ;
    past = table {Indep => "bha" ; Dep => "robh"} ;
    noun = "bhith" ; participle = "air a bhith" ;
    copular = True ; complement = []
    } ;
}
