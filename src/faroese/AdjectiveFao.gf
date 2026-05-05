concrete AdjectiveFao of Adjective = CatFao ** open ResFao in {
lin
  PositA a = a ;
  ComparA a np = {
    s = \\g,n,c => "meir" ++ a.s ! g ! n ! c ++ "enn" ++ np.s ! Nom
  } ;
}
