resource ResBel = open (R = ParamX), Prelude in {

param Case = Nom | Acc | Dat | Gen | Loc | Instr ;
param Number = Sg | Pl ;
param Gender = Masc | Fem | Neuter ;
oper Noun = {s: Case => Number => Str; voc: Str; g: Gender} ; -- 2696
oper mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Gender -> Noun =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,g ->
          { s = table {
                  Nom => table {
                           Sg => f1 ;
                           Pl => f2
                         } ;
                  Acc => table {
                           Sg => f3 ;
                           Pl => f4
                         } ;
                  Dat => table {
                           Sg => f5 ;
                           Pl => f6
                         } ;
                  Gen => table {
                           Sg => f7 ;
                           Pl => f8
                         } ;
                  Loc => table {
                           Sg => f9 ;
                           Pl => f10
                         } ;
                  Instr => table {
                             Sg => f11 ;
                             Pl => f12
                           }
                } ;
            voc = f13 ;
            g = g
          } ;


param Aspect = Imperf | Perf ;
param Person = P1 | P2 | P3 ;
param Tense = Pres | Past ;
oper Verb = {active: Aspect => {past: Str; pres: Person => Number => Str}; imperative: Number => Str; infinitive: Str; participle: Gender => Number => Str; passive: Aspect => Tense => Str} ; -- 703
oper mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27 ->
          { active = table {
                       Imperf => { past = f1 ;
                                   pres = table {
                                            P1 => table {
                                                    Sg => f2 ;
                                                    Pl => f3
                                                  } ;
                                            P2 => table {
                                                    Sg => f4 ;
                                                    Pl => f5
                                                  } ;
                                            P3 => table {
                                                    Sg => f6 ;
                                                    Pl => f7
                                                  }
                                          }
                                 } ;
                       Perf => { past = f8 ;
                                 pres = table {
                                          P1 => table {
                                                  Sg => f9 ;
                                                  Pl => f10
                                                } ;
                                          P2 => table {
                                                  Sg => f11 ;
                                                  Pl => f12
                                                } ;
                                          P3 => table {
                                                  Sg => f13 ;
                                                  Pl => f14
                                                }
                                        }
                               }
                     } ;
            imperative = table {
                           Sg => f15 ;
                           Pl => f16
                         } ;
            infinitive = f17 ;
            participle = table {
                           Masc => table {
                                     Sg => f18 ;
                                     Pl => f19
                                   } ;
                           Fem => table {
                                    Sg => f20 ;
                                    Pl => f21
                                  } ;
                           Neuter => table {
                                       Sg => f22 ;
                                       Pl => f23
                                     }
                         } ;
            passive = table {
                        Imperf => table {
                                    Pres => f24 ;
                                    Past => f25
                                  } ;
                        Perf => table {
                                  Pres => f26 ;
                                  Past => f27
                                }
                      }
          } ;


param GenNum = GSg Gender | GPl ;
oper genNum : Gender -> Number -> GenNum = \g,n ->
       case n of {
         Sg => GSg g ;
         Pl => GPl
       } ;

oper Adj = {s: Case => GenNum => Str} ; -- 704
oper mkAdj : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Adj =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24 ->
          { s = table {
                  Nom => table {
                           GSg Masc => f1 ;
                           GSg Fem => f2 ;
                           GSg Neuter => f3 ;
                           GPl => f4
                         } ;
                  Acc => table {
                           GSg Masc => f5 ;
                           GSg Fem => f6 ;
                           GSg Neuter => f7 ;
                           GPl => f8
                         } ;
                  Dat => table {
                           GSg Masc => f9 ;
                           GSg Fem => f10 ;
                           GSg Neuter => f11 ;
                           GPl => f12
                         } ;
                  Gen => table {
                           GSg Masc => f13 ;
                           GSg Fem => f14 ;
                           GSg Neuter => f15 ;
                           GPl => f16
                         } ;
                  Loc => table {
                           GSg Masc => f17 ;
                           GSg Fem => f18 ;
                           GSg Neuter => f19 ;
                           GPl => f20
                         } ;
                  Instr => table {
                             GSg Masc => f21 ;
                             GSg Fem => f22 ;
                             GSg Neuter => f23 ;
                             GPl => f24
                           }
                }
          } ;


oper Compl = {s : Str; c : Case} ;
oper noPrep : Compl = {s=""; c=Acc} ;

oper CommonNoun = Noun ;
oper AdjPhrase = Adj ;

oper Agr = {g : Gender ; n : Number ; p : Person} ;
oper defaultAgr : Agr = {g=Masc; n=Sg; p=P3} ;

oper NPhrase : Type = {s : Case => Str; a : Agr} ;
oper mkNPhrase : (Case => Str) -> Agr -> NPhrase =
  \s,a -> {s = s; a = a} ;
oper caseTable : Str -> Case => Str =
  \s -> table {Nom => s; Acc => s; Dat => s; Gen => s; Loc => s; Instr => s} ;
oper mkSimpleNP : Str -> Gender -> Number -> Person -> NPhrase =
  \s,g,n,p -> mkNPhrase (caseTable s) {g=g; n=n; p=p} ;

oper nounFromStr : Str -> Gender -> Noun =
  \s,g -> {
    s = \\_,_ => s ;
    voc = s ;
    g = g
  } ;

oper adjFromStr : Str -> Adj =
  \s -> {s = \\_,_ => s} ;

oper prepNP : Compl -> NPhrase -> Str =
  \prep,np -> prep.s ++ np.s ! prep.c ;

oper neg : R.Polarity -> Str =
  \p -> case p of {
    R.Pos => [] ;
    R.Neg => "не"
  } ;

oper futureBe : Agr -> Str =
  \a -> case <a.p,a.n> of {
    <P1,Sg> => "буду" ;
    <P2,Sg> => "будзеш" ;
    <P3,Sg> => "будзе" ;
    <P1,Pl> => "будзем" ;
    <P2,Pl> => "будзеце" ;
    <P3,Pl> => "будуць"
  } ;

oper pastBe : Agr -> Str =
  \a -> case <a.g,a.n> of {
    <Masc,Sg> => "быў" ;
    <Fem,Sg> => "была" ;
    <Neuter,Sg> => "было" ;
    <_,Pl> => "былі"
  } ;

oper copula : R.Tense -> R.Polarity -> Agr -> Str =
  \t,p,a -> case t of {
    R.Pres => neg p ;
    R.Past => neg p ++ pastBe a ;
    R.Fut => neg p ++ futureBe a ;
    R.Cond => neg p ++ pastBe a ++ "бы"
  } ;

oper finiteVerb : Verb -> R.Tense -> R.Polarity -> Agr -> Str =
  \v,t,p,a -> case t of {
    R.Pres => neg p ++ (v.active ! Imperf).pres ! a.p ! a.n ;
    R.Past => neg p ++ v.participle ! a.g ! a.n ;
    R.Fut => neg p ++ futureBe a ++ v.infinitive ;
    R.Cond => neg p ++ v.participle ! a.g ! a.n ++ "бы"
  } ;

oper VPhrase : Type = {
  s : R.Tense => R.Polarity => Agr => Str ;
  inf : Str ;
  imp : R.Polarity => Number => Str
} ;

oper mkVPhrase : Verb -> VPhrase =
  \v -> {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ;
    imp = \\p,n => neg p ++ v.imperative ! n
  } ;

oper VSlash : Type = {
  s : R.Tense => R.Polarity => Agr => Str ;
  inf : Str ;
  c : Compl ;
  imp : R.Polarity => Number => Str ;
  post : Str
} ;

oper mkVSlash : Verb -> Compl -> VSlash =
  \v,c -> {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ;
    c = c ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    post = []
  } ;

oper addAdvVP : VPhrase -> Str -> VPhrase =
  \vp,adv -> {
    s = \\t,p,a => vp.s ! t ! p ! a ++ adv ;
    inf = vp.inf ++ adv ;
    imp = \\p,n => vp.imp ! p ! n ++ adv
  } ;

oper addAdVVP : Str -> VPhrase -> VPhrase =
  \adv,vp -> {
    s = \\t,p,a => adv ++ vp.s ! t ! p ! a ;
    inf = adv ++ vp.inf ;
    imp = \\p,n => adv ++ vp.imp ! p ! n
  } ;

oper addAdvSlash : VSlash -> Str -> VSlash =
  \vp,adv -> {
    s = \\t,p,a => vp.s ! t ! p ! a ;
    inf = vp.inf ++ adv ;
    c = vp.c ;
    imp = \\p,n => vp.imp ! p ! n ;
    post = vp.post ++ adv
  } ;

}
