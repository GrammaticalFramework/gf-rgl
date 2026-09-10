concrete AdjectiveFao of Adjective = CatFao ** open ResFao in {
lin
  PositA a = a ;
  ComparA a np = {
    s = \\g,n,c => "meir" ++ a.s ! g ! n ! c ++ "enn" ++ np.s ! Nom
  } ;
  ComplA2 a np = {
    s = \\g,n,c => a.s ! g ! n ! c ++ a.c2.s ++ np.s ! a.c2.c
  } ;
  ReflA2 a = {
    s = \\g,n,c => a.s ! g ! n ! c ++ a.c2.s ++ "seg"
  } ;
  UseA2 a = a ;
  UseComparA a = {
    s = \\g,n,c => "meir" ++ a.s ! g ! n ! c
  } ;
  CAdvAP cadv ap np = {
    s = \\g,n,c => cadv.s ++ ap.s ! g ! n ! c ++ cadv.p ++ np.s ! Nom
  } ;
  AdjOrd ord = {
    s = ord.s
  } ;
  SentAP ap sc = {
    s = \\g,n,c => ap.s ! g ! n ! c ++ sc.s
  } ;
  AdAP ada ap = {
    s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c
  } ;
  AdvAP ap adv = {
    s = \\g,n,c => ap.s ! g ! n ! c ++ adv.s
  } ;
}
