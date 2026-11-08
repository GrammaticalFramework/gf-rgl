concrete IdiomSqi of Idiom = CatSqi ** open Prelude, ParamX, ResSqi in {
lin
  ImpersCl vp = {s=\\t,a,p=>negation p++futureParticle t++vp.indicative!sqiTense t!Sg!P3!Masc!Nom} ;
  GenericCl vp = {s=\\t,a,p=>"njeriu"++negation p++futureParticle t++vp.indicative!sqiTense t!Sg!P3!Masc!Nom} ;
  CleftNP np rs = {s=\\_,_,_=>np.s!Nom++rs.s!np.a} ;
  CleftAdv adv s = {s=\\_,_,_=>adv.s++s.s} ;
  ExistNP np = {s=\\t,_,p=>negation p++case t of {ParamX.Past=>"pati";_=>"ka"}++np.s!Acc} ;
  ExistIP ip = {s=\\_,_,p=>negation p++"ka"++ip.s} ;
  ExistNPAdv np adv = {s=\\t,a,p=>(ExistNP np).s!t!a!p++adv.s} ;
  ExistIPAdv ip adv = {s=\\t,a,p=>(ExistIP ip).s!t!a!p++adv.s} ;
  ProgrVP vp = vp ;
  ImpPl1 vp = {s="le të"++vp.subjunctive!Pl!P1!Masc!Nom} ;
  ImpP3 np vp = {s="le"++np.s!Nom++"të"++vp.subjunctive!agrNumber np.a!np.a.p!agrGender np.a!Nom} ;
  SelfAdvVP vp = vp ** {
    indicative=\\t,n,p,g,c=>vp.indicative!t!n!p!g!c++"vetë";
    subjunctive=\\n,p,g,c=>vp.subjunctive!n!p!g!c++"vetë"
    } ;
  SelfAdVVP vp = vp ** {
    indicative=\\t,n,p,g,c=>"vetë"++vp.indicative!t!n!p!g!c;
    subjunctive=\\n,p,g,c=>"vetë"++vp.subjunctive!n!p!g!c
    } ;
  SelfNP np = np ** {s=\\c=>np.s!c++"vetë"} ;
}
