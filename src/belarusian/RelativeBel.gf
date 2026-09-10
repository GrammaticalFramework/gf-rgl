concrete RelativeBel of Relative = CatBel ** open ResBel in {

lin
  RelCl cl = {s = \\t,p => "што" ++ cl.s ! t ! p} ;
  RelVP rp vp = {s = \\t,p => rp.s ++ vp.s ! t ! p ! defaultAgr} ;
  RelSlash rp cl = {s = \\t,p => rp.s ++ cl.s ! t ! p} ;

  IdRP = {s = "які"} ;
  FunRP prep np rp = {s = prepNP prep np ++ rp.s} ;

}
