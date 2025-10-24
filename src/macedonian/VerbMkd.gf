concrete VerbMkd of Verb = CatMkd ** open Prelude,ResMkd in {

  lin AdVVP a vp = {present = \\a2,n,p => a.s
                                            ++ vp.present ! a2 ! n ! p;
                    aorist = \\n,p => a.s ++ vp.aorist ! n ! p;
                    imperfect = \\a2,n,p => a.s ++ vp.imperfect ! a2 ! n ! p;
                    imperative = \\a2,n => a.s ++ vp.imperative ! a2 ! n;
                    participle = {aorist = \\a2,g => a.s
                                                       ++ vp.participle.aorist ! a2 ! g;
                                  imperfect = \\a2,g => a.s ++ vp.participle.imperfect ! a2 ! g;
                                  perfect = \\a2 => a.s ++ vp.participle.perfect ! a2;
                                  adjectival = \\a2 => a.s ++ vp.participle.adjectival ! a2;
                                  adverbial = a.s ++ vp.participle.adverbial};
                    noun_from_verb = a.s ++ vp.noun_from_verb;
                    vtype = vp.vtype;
                    compl = vp.compl} ;
  lin AdVVPSlash a v = {present = \\a2,n,p => a.s
                                                ++ v.present ! a2 ! n ! p;
                        aorist = \\n,p => a.s ++ v.aorist ! n ! p;
                        imperfect = \\a2,n,p => a.s ++ v.imperfect ! a2 ! n ! p;
                        imperative = \\a2,n => a.s ++ v.imperative ! a2 ! n;
                        participle = {aorist = \\a2,g => a.s
                                                           ++ v.participle.aorist ! a2 ! g;
                                      imperfect = \\a2,g => a.s ++ v.participle.imperfect ! a2 ! g;
                                      perfect = \\a2 => a.s ++ v.participle.perfect ! a2;
                                      adjectival = \\a2 => a.s ++ v.participle.adjectival ! a2;
                                      adverbial = a.s ++ v.participle.adverbial};
                        noun_from_verb = a.s ++ v.noun_from_verb;
                        vtype = v.vtype;
                        compl = v.compl;
                        c2 = v.c2} ;
  lin AdvVP vp adv = vp ** {compl = \\a => vp.compl ! a ++ adv.s} ;
  lin AdvVPSlash vps adv = vps ** {compl = \\a => vps.compl ! a ++ adv.s} ;
  lin CompAP ap = {s = \\gn => ap.s ! Indef ! gn} ;
  lin CompAdv a = {s = \\_ => a.s} ;
  lin CompCN cn = {s = \\_ => cn.s ! Indef ! Sg} ;
  lin CompNP np = {s = \\_ => np.s ! RSubj} ;
  lin ComplSlash vp np = vp ** {compl = \\a => vp.compl ! a ++ vp.c2.s ++ np.s ! RObj vp.c2.c} ;
  lin ComplVA va ap = va ** {compl = \\agr => ap.s ! Indef ! agr.g} ;
  lin ComplVQ vq qs = vq ** {compl = \\_ => qs.s} ;
  lin ComplVS vs s = vs ** {compl = \\_ => s.s} ;
  lin ComplVV vv vp = vv ** {compl = \\agr => "да" ++ vp.present ! Perfective ! genNum2num agr.g ! agr.p ++ vp.compl ! agr} ;
  lin ExtAdvVP vp a = vp ** {compl = \\agr => vp.compl ! agr ++ SOFT_BIND++"," ++ a.s} ;
  lin Slash2V3 v3 np = {present = \\a,n,p => v3.present ! a ! n ! p
                                               ++ np.s ! RSubj;
                        aorist = \\n,p => v3.aorist ! n ! p ++ np.s ! RSubj;
                        imperfect = \\a,n,p => v3.imperfect ! a ! n ! p ++ np.s ! RSubj;
                        imperative = \\a,n => v3.imperative ! a ! n ++ np.s ! RSubj;
                        participle = {aorist = \\a,g => v3.participle.aorist ! a ! g
                                                          ++ np.s ! RSubj;
                                      imperfect = \\a,g => v3.participle.imperfect ! a ! g
                                                             ++ np.s ! RSubj;
                                      perfect = \\a => v3.participle.perfect ! a ++ np.s ! RSubj;
                                      adjectival = \\a => v3.participle.adjectival ! a
                                                            ++ np.s ! RSubj;
                                      adverbial = v3.participle.adverbial ++ np.s ! RSubj};
                        noun_from_verb = v3.noun_from_verb ++ np.s ! RSubj;
                        vtype = v3.vtype;
                        compl = \\v => v3.present ! Imperfective ! Sg ! v.p
                                         ++ np.s ! RSubj;
                        c2 = {s = v3.c2.s ++ np.s ! RSubj; c = v3.c2.c}} ;
  lin Slash3V3 v3 np = {present = \\a,n,p => v3.present ! a ! n ! p
                                               ++ np.s ! RSubj;
                        aorist = \\n,p => v3.aorist ! n ! p ++ np.s ! RSubj;
                        imperfect = \\a,n,p => v3.imperfect ! a ! n ! p ++ np.s ! RSubj;
                        imperative = \\a,n => v3.imperative ! a ! n ++ np.s ! RSubj;
                        participle = {aorist = \\a,g => v3.participle.aorist ! a ! g
                                                          ++ np.s ! RSubj;
                                      imperfect = \\a,g => v3.participle.imperfect ! a ! g
                                                             ++ np.s ! RSubj;
                                      perfect = \\a => v3.participle.perfect ! a ++ np.s ! RSubj;
                                      adjectival = \\a => v3.participle.adjectival ! a
                                                            ++ np.s ! RSubj;
                                      adverbial = v3.participle.adverbial ++ np.s ! RSubj};
                        noun_from_verb = v3.noun_from_verb ++ np.s ! RSubj;
                        vtype = v3.vtype;
                        compl = \\v => v3.present ! Imperfective ! Sg ! v.p
                                         ++ np.s ! RSubj;
                        c2 = {s = v3.c2.s ++ np.s ! RSubj; c = v3.c2.c}} ;
  lin SlashV2A v ap = v ** {compl = \\agr => ap.s ! Indef ! agr.g} ;
  lin SlashV2Q v qs = v ** {compl = \\_ => qs.s} ;
  lin SlashV2S v s = v ** {compl = \\_ => s.s} ;
  lin SlashV2V v vp = v ** {compl = \\agr => "да" ++ vp.present ! Perfective ! genNum2num agr.g ! agr.p ++ vp.compl ! agr};
  lin SlashV2a v = v ** {compl = \\_ => []} ;

  lin UseComp comp = {
        present = \\_=>auxBe.present ;
        aorist = auxBe.imperfect ;
        imperfect = \\_=>auxBe.imperfect ;
        imperative = \\_=>auxBe.imperative ;
        participle = {aorist = \\_=>auxBe.participle.aorist ;
                      imperfect = \\_=>auxBe.participle.imperfect ;
                      perfect = \\_=>nonExist ;
                      adjectival = \\_=>nonExist ;
                      adverbial = nonExist} ;
        noun_from_verb = nonExist ;
        compl = \\agr=>comp.s ! agr.g ;
        vtype=VNormal
      } ;

  lin UseCopula = {
        present = \\_=>auxBe.present ;
        aorist = auxBe.imperfect ;
        imperfect = \\_=>auxBe.imperfect ;
        imperative = \\_=>auxBe.imperative ;
        participle = {aorist = \\_=>auxBe.participle.aorist ;
                      imperfect = \\_=>auxBe.participle.imperfect ;
                      perfect = \\_=>nonExist ;
                      adjectival = \\_=>nonExist ;
                      adverbial = nonExist} ;
        noun_from_verb = nonExist ;
        compl = \\_=>[] ;
        vtype=VNormal
      } ;

  lin UseV v = v ** {compl = \\_ => []} ;
  lin VPSlashPrep vp p = {present = \\a,n,p2 => vp.present ! a ! n
                                                  ! p2
                                                  ++ p.s;
                          aorist = \\n,p2 => vp.aorist ! n ! p2 ++ p.s;
                          imperfect = \\a,n,p2 => vp.imperfect ! a ! n ! p2 ++ p.s;
                          imperative = \\a,n => vp.imperative ! a ! n ++ p.s;
                          participle = {aorist = \\a,g => vp.participle.aorist ! a ! g
                                                            ++ p.s;
                                        imperfect = \\a,g => vp.participle.imperfect ! a ! g ++ p.s;
                                        perfect = \\a => vp.participle.perfect ! a ++ p.s;
                                        adjectival = \\a => vp.participle.adjectival ! a ++ p.s;
                                        adverbial = vp.participle.adverbial ++ p.s};
                          noun_from_verb = vp.noun_from_verb ++ p.s; vtype = vp.vtype;
                          compl = \\v => vp.compl ! {g = GSg Masc; p = P1} ++ p.s;
                          c2 = {s = vp.present ! Imperfective ! Sg ! P1 ++ p.s; c = p.c}} ;
}
