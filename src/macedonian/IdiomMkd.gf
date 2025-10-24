concrete IdiomMkd of Idiom = CatMkd ** open Prelude,ResMkd in {

  lin CleftAdv a s = {s = \\t,a2,p,o => a.s ++ s.s} ;
  lin CleftNP np rs = {s = \\t,a,p,o => np.s ! RSubj
                                          ++ rs.s ! np.a.g} ;
  lin ExistIP ip = {s = \\t,a,p => ip.s} ;
  lin ExistIPAdv ip a = {s = \\t,a2,p => ip.s ++ a.s} ;
  lin ExistNP np = {s = \\t,a,p,o => np.s ! RSubj} ;
  lin ExistNPAdv np a = {s = \\t,a2,p,o => np.s ! RSubj ++ a.s} ;
  lin GenericCl vp = {s = \\t,a,p,o => vp.present ! Imperfective ! Sg
                                         ! P1} ;
  lin ImpP3 np vp = {s = np.s ! RSubj
                           ++ vp.present ! Imperfective ! Sg ! np.a.p} ;
  lin ImpPl1 vp = {s = vp.present ! Imperfective ! Sg ! P1} ;
  lin ImpersCl vp = {s = \\t,a,p,o => vp.present ! Imperfective ! Sg
                                        ! P1} ;
  lin ProgrVP vp = vp ** {
       present = \\a,n,p => vp.present ! Imperfective ! n ! p ;
       imperfect = \\a,n,p => vp.imperfect ! Imperfective ! n ! p ;
       imperative = \\a,n => vp.imperative ! Imperfective ! n ;
       participle = {aorist = \\a,gn => vp.participle.aorist ! Imperfective ! gn ;
                     perfect = \\a => vp.participle.perfect ! Imperfective ;
                     imperfect = \\a => vp.participle.imperfect ! Imperfective ;
                     adjectival = vp.participle.adjectival ;
                     adverbial = vp.participle.adverbial}
    } ;

  lin SelfAdVVP vp = {present = \\a,n,p => vp.present ! a ! n ! p;
                      aorist = \\n,p => vp.aorist ! n ! p;
                      imperfect = \\a,n,p => vp.imperfect ! a ! n ! p;
                      imperative = \\a,n => vp.imperative ! a ! n;
                      participle = {aorist = \\a,g => vp.participle.aorist ! a ! g;
                                    imperfect = \\a,g => vp.participle.imperfect ! a ! g;
                                    perfect = \\a => vp.participle.perfect ! a;
                                    adjectival = \\a => vp.participle.adjectival ! a;
                                    adverbial = vp.participle.adverbial};
                      noun_from_verb = vp.noun_from_verb; vtype = vp.vtype;
                      compl = \\v => vp.compl ! {g = GSg Masc; p = P1}} ;
  lin SelfAdvVP vp = {present = \\a,n,p => vp.present ! a ! n ! p;
                      aorist = \\n,p => vp.aorist ! n ! p;
                      imperfect = \\a,n,p => vp.imperfect ! a ! n ! p;
                      imperative = \\a,n => vp.imperative ! a ! n;
                      participle = {aorist = \\a,g => vp.participle.aorist ! a ! g;
                                    imperfect = \\a,g => vp.participle.imperfect ! a ! g;
                                    perfect = \\a => vp.participle.perfect ! a;
                                    adjectival = \\a => vp.participle.adjectival ! a;
                                    adverbial = vp.participle.adverbial};
                      noun_from_verb = vp.noun_from_verb; vtype = vp.vtype;
                      compl = \\v => vp.compl ! {g = GSg Masc; p = P1}} ;
  lin SelfNP np = {s = \\r => np.s ! r; vocative = np.vocative;
                   a = {g = np.a.g; p = np.a.p}} ;
}
