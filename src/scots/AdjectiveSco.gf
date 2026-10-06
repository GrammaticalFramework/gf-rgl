concrete AdjectiveSco of Adjective = AdjectiveEng - [ComparA,UseComparA,ReflA2] ** open Prelude, ResSco in {

lin ComparA a np = {
      s = \\_ => getCompar Nom a ++ "than" ++ np.s ! npNom ;
      isPre = False
      } ;
    UseComparA a = {
      s = \\_ => getCompar Nom a ;
      isPre = a.isPre
      } ;
    ReflA2 a = {
      s = \\ag => a.s ! AAdj Posit Nom ++ a.c2 ++ reflPron ! ag ;
      isPre = False
      } ;

}
