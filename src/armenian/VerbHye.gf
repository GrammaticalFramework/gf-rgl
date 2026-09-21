concrete VerbHye of Verb = CatHye ** open Prelude, ResHye, ParadigmsHye in {
  oper
    compl : Compl -> NP -> Str = \c,np -> case c.isPre of {
      True => c.s ++ np.s ! c.c;
      False => np.s ! c.c ++ c.s
    } ;
    toVP : Verb -> VP = \v -> lin VP {
      s=v.s; conditional=v.conditional; converb=v.converb; imperative=v.imperative;
      passive=v.passive; past=v.past; participle=v.participle; subjunctive=v.subjunctive
    } ;
    appendVP : VP -> Str -> VP = \v,x -> lin VP {
      s=v.s ++ x;
      conditional=\\a,p,n => v.conditional ! a ! p ! n ++ x;
      converb={imperfective=v.converb.imperfective ++ x; futCon1=v.converb.futCon1 ++ x;
        futCon2=v.converb.futCon2 ++ x; negative=v.converb.negative ++ x;
        perfective=v.converb.perfective ++ x; simultaneous=v.converb.simultaneous ++ x};
      imperative=\\n => v.imperative ! n ++ x; passive=v.passive ++ x;
      past=\\p,n => v.past ! p ! n ++ x;
      participle=\\p => v.participle ! p ++ x;
      subjunctive=\\a,p,n => v.subjunctive ! a ! p ! n ++ x
    } ;
    prependVP : Str -> VP -> VP = \x,v -> lin VP {
      s=x ++ v.s;
      conditional=\\a,p,n => x ++ v.conditional ! a ! p ! n;
      converb={imperfective=x ++ v.converb.imperfective; futCon1=x ++ v.converb.futCon1;
        futCon2=x ++ v.converb.futCon2; negative=x ++ v.converb.negative;
        perfective=x ++ v.converb.perfective; simultaneous=x ++ v.converb.simultaneous};
      imperative=\\n => x ++ v.imperative ! n; passive=x ++ v.passive;
      past=\\p,n => x ++ v.past ! p ! n;
      participle=\\p => x ++ v.participle ! p;
      subjunctive=\\a,p,n => x ++ v.subjunctive ! a ! p ! n
    } ;
  lin AdvVP vp adv = {s = adv.s ++ vp.s;
                      conditional = \\a,p,n => adv.s ++ vp.conditional ! a ! p ! n;
                      converb = {imperfective = adv.s ++ vp.converb.imperfective;
                                 futCon1 = adv.s ++ vp.converb.futCon1;
                                 futCon2 = adv.s ++ vp.converb.futCon2;
                                 negative = adv.s ++ vp.converb.negative;
                                 perfective = adv.s ++ vp.converb.perfective;
                                 simultaneous = adv.s ++ vp.converb.simultaneous};
                      imperative = \\n => vp.imperative ! n ++ adv.s;
                      passive = adv.s ++ vp.passive;
                      past = \\p,n => adv.s ++ vp.past ! p ! n;
                      participle = \\p => adv.s ++ vp.participle ! p;
                      subjunctive = \\a,p,n => adv.s ++ vp.subjunctive ! a ! p ! n} ;
  lin ComplSlash vp np = {s = compl vp.c2 np ++ vp.s;
                          conditional = \\a,p,n => compl vp.c2 np
                                                     ++ vp.conditional ! a ! p ! n;
                          converb = {imperfective = compl vp.c2 np
                                                      ++ vp.converb.imperfective;
                                     futCon1 = compl vp.c2 np ++ vp.converb.futCon1;
                                     futCon2 = compl vp.c2 np ++ vp.converb.futCon2;
                                     negative = compl vp.c2 np ++ vp.converb.negative;
                                     perfective = compl vp.c2 np
                                                    ++ vp.converb.perfective;
                                     simultaneous = compl vp.c2 np
                                                      ++ vp.converb.simultaneous};
                          imperative = \\n => compl vp.c2 np ++ vp.imperative ! n;
                          passive = compl vp.c2 np ++ vp.passive;
                          past = \\p,n => compl vp.c2 np ++ vp.past ! p ! n;
                          participle = \\p => compl vp.c2 np ++ vp.participle ! p;
                          subjunctive = \\a,p,n => compl vp.c2 np
                                                     ++ vp.subjunctive ! a ! p ! n} ;
  lin SlashV2a v = v ;
  lin UseV v = v ;
  lin ComplVS v s = appendVP (toVP v) s.s ;
  lin ComplVQ v qs = appendVP (toVP v) qs.s ;
  lin ComplVA v ap = appendVP (toVP v) (ap.s ! Indef ! Nom ! Sg) ;
  lin Slash2V3 v np = lin VPSlash (v ** {
    s = compl v.c2 np ++ v.s; c2 = v.c3
  }) ;
  lin Slash3V3 v np = lin VPSlash (v ** {
    s = compl v.c3 np ++ v.s; c2 = v.c2
  }) ;
  lin SlashV2S v s = lin VPSlash (v ** {s = v.s ++ s.s; c2=v.c2}) ;
  lin SlashV2Q v qs = lin VPSlash (v ** {s = v.s ++ qs.s; c2=v.c2}) ;
  lin SlashV2A v ap = lin VPSlash (v ** {
    s = v.s ++ ap.s ! Indef ! Nom ! Sg; c2=v.c2
  }) ;
  lin UseComp comp = prependVP comp.s (toVP (mkV "լինել")) ;
  lin UseCopula = toVP (mkV "լինել") ;
  lin ExtAdvVP vp adv = appendVP vp ("," ++ adv.s) ;
  lin AdVVP adv vp = prependVP adv.s vp ;
  lin AdvVPSlash vp adv = lin VPSlash (vp ** {s = vp.s ++ adv.s}) ;
  lin AdVVPSlash adv vp = lin VPSlash (vp ** {s = adv.s ++ vp.s}) ;
  lin VPSlashPrep vp prep = lin VPSlash (vp ** {causative=vp.s;c2=prep}) ;
  lin CompAP ap = {s = ap.s ! Indef ! Nom ! Sg} ;
  lin CompNP np = {s = np.s ! Nom} ;
  lin CompAdv adv = adv ;
  lin CompCN cn = {s = cn.s ! Indef ! Nom ! Sg} ;
}
