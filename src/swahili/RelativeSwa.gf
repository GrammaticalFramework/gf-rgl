concrete RelativeSwa of Relative = CatSwa ** open Prelude, ResSwa in {
  flags optimize=all_subs ; coding=utf8 ;
  lin
    RelCl cl = {s = \\t,a,b,_ => "ambapo" ++ cl.s ! t ! a ! b ; c = npNom} ;
    RelVP rp vp = {s = \\t,a,b,ag => let f = agrFeatures ag in
      rp.s ! ag ++ vp.s ! t ! a ! b ! f ++ vp.obj ! f ; c = npNom} ;
    RelSlash rp cl = {s = \\t,a,b,ag => rp.s ! ag ++ cl.s ! t ! a ! b ++ cl.c2 ; c = NPAcc} ;
    IdRP = {s = relPron} ;
    FunRP p np rp = {s = \\ag => p.s ! Sg ! G1 ++ np.s ! NPoss ++ rp.s ! ag} ;
  oper
    relPron : Agr => Str = table {
      Ag G1 Sg _ => "ambaye" ; Ag G1 Pl _ => "ambao" ;
      Ag G2 Sg _ => "ambao" ; Ag G2 Pl _ => "ambayo" ;
      Ag G3 Sg _ => "ambalo" ; Ag G3 Pl _ => "ambayo" ;
      Ag G4 Sg _ => "ambacho" ; Ag G4 Pl _ => "ambavyo" ;
      Ag G5 Sg _ => "ambayo" ; Ag G5 Pl _ => "ambazo" ;
      _ => "ambayo"
      } ;
}
