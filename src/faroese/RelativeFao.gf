concrete RelativeFao of Relative = CatFao ** open ResFao in {
lin
  IdRP = {s = "sum"} ;
  RelCl cl = {
    s = \\t,pol,_,_ => "sum" ++ cl.Indicative ! t ! pol
  } ;
  RelVP rp vp = {
    s = \\t,pol,g,pn => rp.s ++ vp.Indicative ! t ! pol ! g ! pn
  } ;
  RelSlash rp cls = {
    s = \\t,pol,_,_ => rp.s ++ cls.s ! t ! pol
  } ;
  FunRP prep np rp = {
    s = prep.s ++ np.s ! prep.c ++ rp.s
  } ;
}
