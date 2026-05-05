concrete RelativeFao of Relative = CatFao ** open ResFao in {
lin
  IdRP = {s = "sum"} ;
  RelVP rp vp = {
    s = \\t,pol,g,pn => rp.s ++ vp.Indicative ! t ! pol ! g ! pn
  } ;
}
