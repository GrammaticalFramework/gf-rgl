concrete VerbExtZul of VerbExt = CatZul ** open ResZul, Prelude, ParamX in {

  lin

    CopAP ap = {
      s = case ap.t of {
        AdjType => table {
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = pre_cop_pref vform a ;
            adjpref =  adjPrefLookup!a ++BIND
          in
            (pre_cop_pref vform a) ++ adjpref ++ ap.s!(aformN a) ;
          RelCl => \\a,p,t => relAdjAgrLookup!p!a ++BIND++ ap.s!(aformN a)
        } ;
        RelType => table {
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = pre_cop_pref vform a ;
            adjpref =  adjPrefLookup!a ++BIND ;
          in
            (pre_cop_pref vform a) ++ ap.s!AF1 ;
          RelCl => \\a,p,t => (relConc p a) ++ ap.s!AF1
        } ;
        EnumType => table {
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = pre_cop_pref vform a ;
            adjpref =  adjPrefLookup!a ++BIND ;
          in
            (pre_cop_pref vform a) ++ adjpref ++ ap.s!AF1 ;
          RelCl => \\a,p,t => enumConcLookup!a ++BIND++ ap.s!AF1
        }
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      vptype = CopDescr
    } ;

    CopNP np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t
        in
          (id_pre_cop_pref vform p a) ++ (id_cop_pref a) ++ np.s!Full ;
        RelCl => \\a,p,t => (relConc p a) ++ (id_cop_pref a) ++ np.s!Full
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      vptype = CopIdent
    } ;

    CopNPAssoc np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t
        in
          (assoc_pre_cop_pref vform p a) ++ (assoc_cop_pref p np.agr) ++ np.s!Reduced ;
        RelCl => \\a,p,t => let
          vform = VFIndic RelCl p t
        in
          (relConc p a) ++ (assoc_cop_pref p np.agr) ++ np.s!Reduced
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      vptype = CopAssoc
    } ;

}
