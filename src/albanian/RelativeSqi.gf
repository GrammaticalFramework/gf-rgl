concrete RelativeSqi of Relative = CatSqi ** open ParamX, ResSqi in {
lin
  RelCl cl = {s=\\_,t,a,p=>"që"++cl.s!t!a!p} ;
  RelVP rp vp = {s=\\agr,t,a,p=>rp.s!Nom!agr.gn++negation p++futureParticle t++vp.indicative!sqiTense t!agrNumber agr!agr.p!agrGender agr!Nom} ;
  RelSlash rp cl = {s=\\agr,t,a,p=>rp.s!cl.c2.c!agr.gn++cl.s!t!a!p++cl.c2.s} ;
  IdRP = {s=\\_,_=>"që"} ;
  FunRP prep np rp = {s=\\c,g=>prep.s++np.s!prep.c++rp.s!c!g} ;
}
