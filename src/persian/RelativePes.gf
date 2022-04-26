concrete RelativePes of Relative = CatPes ** open ResPes in {

  flags optimize=all_subs ;
  coding = utf8;

  lin

    RelCl cl = {
      s = \\t,p,agr => cl.s ! t ! p ! ODir ;
      rp = IdRP.s
      } ;

    RelVP rp vp = {
      s = \\t,p,ag =>
        let agr = case rp.a of {
              RNoAg => ag ;
              RAg a => a} ;
            cl = mkSClause [] agr vp;
         in cl.s ! t ! p ! ODir ;
      rp = rp.s
      } ;

    RelSlash rp slash = {
      s = \\t,p,agr => slash.c2.s ++ slash.subj ++ slash.vp ! t ! p ! ODir ;--case t of { ---- AR 18/8/2017 is this the right place of subj?
      rp = rp.s
      } ;

    FunRP p np rp = {
      s = \\ke => np.s ! Clitic ++ rp.s ! ke ++ p.s ++ getPron np.animacy (fromAgr np.a).n ;
      a = RAg np.a
      } ;

    IdRP = {
      s = table {Ke => "که" ; Ance => "آنچه"} ;
      a = RNoAg
      } ;

}
