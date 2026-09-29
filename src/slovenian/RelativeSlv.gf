concrete RelativeSlv of Relative = CatSlv ** open ResSlv, Prelude in {
lin
  IdRP = {s = \\_,_,_ => "ki"} ;
  RelVP rp vp = {
    s = \\a,t,ant,p => rp.s ! inanimateGender a.g ! Nom ! a.n ++
      predV False vp.isCop vp.refl vp.s ! t ! p ! a ++ vp.s2 ! a
  } ;
  RelSlash rp cl = {
    s = \\a,t,ant,p => rp.s ! inanimateGender a.g ! cl.c2.c ! a.n ++ cl.s ! t ! ant ! p
  } ;
  RelCl cl = {s = \\_,t,a,p => "kar" ++ cl.s ! t ! a ! p} ;
  FunRP prep np rp = {
    s = \\g,c,n => prep.s ++ np.s ! prep.c ++ rp.s ! g ! c ! n
  } ;
}
