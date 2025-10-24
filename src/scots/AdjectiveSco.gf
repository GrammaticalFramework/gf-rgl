concrete AdjectiveSco of Adjective = AdjectiveEng - [ComparA,UseComparA] ** open Prelude, ResSco in {

lin ComparA a np = {
      s = \\_ => getCompar Nom a ++ "than" ++ np.s ! npNom ;
      isPre = False
      } ;
    UseComparA a = {
      s = \\_ => getCompar Nom a ;
      isPre = a.isPre
      } ;

}
