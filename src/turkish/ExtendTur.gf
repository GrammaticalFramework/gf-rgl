concrete ExtendTur of Extend = CatTur ** open ResTur, SuffixTur, Predef in {

  lin
    GenRP n cn = {
      s = cn.gen ! n.n
    } ;

    GenModNP num np cn = {
      s = \\c => np.s ! Nom ++ num.s ! num.n ! c ++ cn.gen ! num.n ! np.a ;
      h = cn.h ;
      a = {n=num.n; p=P3} ;
    } ;

    UttVPShort vp = {s = vp.s ! Perf ! VInf Pos} ;

    TPastSimple = {s = []} ** {t = Past} ;  --# notpresent

    PositAdVAdj a = {s = a.s ! Sg ! Nom} ;

    PassVPSlash vps = {
      s = mkVerbForms {
            s = vps.stems ! VPass ++ BIND ++ suffixStr vps.h infinitiveSuffix ;
            stems = \\_ => vps.stems ! VPass ;
            aoristType = vps.aoristType ;
            h = vps.h ;
          } ;
      compl = []
    } ;

    PassAgentVPSlash vps np = {
      s = mkVerbForms {
            s = vps.stems ! VPass ++ BIND ++ suffixStr vps.h infinitiveSuffix ;
            stems = \\_ => vps.stems ! VPass ;
            aoristType = vps.aoristType ;
            h = vps.h ;
          } ;
      compl = np.s ! Acc ++ "tarafından"
    } ;

}
