concrete NounSwa of Noun =
  CatSwa ** NounBantu - [ApposCN, PPartNP, QuantityNP, MassNP] with
    (Structural = StructuralSwa), (ResBantu = ResSwa) **
  open Prelude, ResSwa in {
  lin
    ApposCN cn np = {
      s = \\n,c => cn.s ! n ! c ++ SOFT_BIND ++ "," ++ np.s ! npNom ;
      s2 = cn.s2 ; g = cn.g
      } ;
    -- Swahili mass nouns take singular noun-class agreement.  The Bantu
    -- functor leaves the number as a free variant, which made linearization
    -- nondeterministically select plural agreement and could even erase an
    -- adjective for classes without a plural form.
    MassNP cn = {
      s = \\c => cn.s ! Sg ! npcase2case c ++ cn.s2 ! Sg ;
      a = Ag cn.g Sg P3
      } ;
    PPartNP np v = {s = \\c => np.s ! c ++ v.s ! Pos ! VInf ; a = np.a} ;
    QuantityNP d mu = {s = \\_ => d.s ! NCard ! G5 ++ mu.s ; a = Ag G5 d.n P3} ;
}
