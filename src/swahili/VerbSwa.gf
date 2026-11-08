concrete VerbSwa of Verb = CatSwa ** open Prelude, Predef, ResSwa, MorphoSwa in {

  flags optimize=all_subs ; coding=utf8 ;

  lin
    UseV = predV ;
    ComplVV v vp = insertObj (\\a => infVP Pos a vp) (predV v) ;
    ComplVS v s  = insertObj (\\_ => "kwamba" ++ s.s) (predV v) ;
    ComplVQ v q  = insertObj (\\_ => q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertObj (\\a => ap.s ! a.g ! a.n) (predV v) ;
    SlashV2a v = predV v ** {c2 = []} ;
    Slash2V3 v np = insertObj (\\_ => np.s ! NPAcc) (predV v) ** {c2 = []} ;
    Slash3V3 v np = insertObj (\\_ => np.s ! NPAcc) (predV v) ** {c2 = []} ;
    SlashV2V v vp = insertObj (\\a => infVP Pos a vp) (predV v) ** {c2 = []} ;
    SlashV2S v s = insertObj (\\_ => "kwamba" ++ s.s) (predV v) ** {c2 = []} ;
    SlashV2Q v q = insertObj (\\_ => q.s ! QIndir) (predV v) ** {c2 = []} ;
    SlashV2A v ap = insertObj (\\a => ap.s ! a.g ! a.n) (predV v) ** {c2 = []} ;
    ComplSlash vp np = insertObj (\\_ => vp.c2 ++ np.s ! NPAcc) vp ;
    SlashVV v vp = insertObj (\\a => infVP Pos a vp) (predV v) ** {c2 = vp.c2} ;
    SlashV2VNP v np vp = insertObj (\\a => np.s ! NPAcc ++ infVP Pos a vp) (predV v) ** {c2 = vp.c2} ;
    ReflVP vp = insertObj (\\a => vp.c2 ++ "ji") vp ;
    UseComp comp = {
      s = \\t,ant,pos,a => case <t,pos> of {
        <Pres,Pos> => "ni" ; <Pres,Neg> => "si" ;
        <_,_> => (predV (regV "kuwa")).s ! t ! ant ! pos ! a
        } ;
      passive = \\t,ant,pos,a => case <t,pos> of {
        <Pres,Pos> => "ni" ; <Pres,Neg> => "si" ;
        <_,_> => (predV (regV "kuwa")).passive ! t ! ant ! pos ! a
        } ;
      obj = \\a => comp.s ! Ag a.g a.n a.p ;
      inf = \\pos,_ => case pos of {Pos => "kuwa" ; Neg => "kutokuwa"} ;
      passiveInf = \\pos,_ => case pos of {Pos => "kuwa" ; Neg => "kutokuwa"} ;
      imp = \\pos,n => case <pos,n> of {
        <Pos,Sg> => "kuwa" ; <Pos,Pl> => "kuweni" ;
        <Neg,Sg> => "usiwe" ; <Neg,Pl> => "msiwe"
        } ;
      passiveImp = \\pos,n => case <pos,n> of {
        <Pos,Sg> => "kuwa" ; <Pos,Pl> => "kuweni" ;
        <Neg,Sg> => "usiwe" ; <Neg,Pl> => "msiwe"
        }
      } ;
    PassV2 v = passiveVP (predV v) ;
    AdvVP vp adv = insertObj (\\_ => adv.s) vp ;
    ExtAdvVP vp adv = insertObj (\\_ => frontComma ++ adv.s ++ finalComma) vp ;
    AdVVP adv vp = insertAdV adv.s vp ;
    AdvVPSlash vp adv = insertObj (\\_ => adv.s) vp ** {c2 = vp.c2} ;
    AdVVPSlash adv vp = insertAdV adv.s vp ** {c2 = vp.c2} ;
    VPSlashPrep vp p = vp ** {c2 = p.s ! Sg ! G1} ;
    CompAP ap = {s = \\a => let f = agrFeatures a in ap.s ! f.g ! f.n} ;
    CompNP np = {s = \\_ => np.s ! NPAcc} ;
    CompAdv adv = {s = \\_ => adv.s} ;
    CompCN cn = {s = \\a => let f = agrFeatures a in cn.s ! f.n ! Nom ++ cn.s2 ! f.n} ;
    UseCopula = UseComp {s = \\_ => []} ;
}
