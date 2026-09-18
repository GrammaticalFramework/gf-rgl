concrete RelativeTel of Relative = CatTel ** open ResTel in {
  lin
    RelCl cl = cl ;
    RelVP rp vp = {
      s = \\tense,pol => let f = vp.s ! pol ! case tense of {
        VPGenPres => VPRelPresent ;
        VPFut => VPRelPresent ;
        _ => VPPastPart
        } in vp.obj.s ++ vp.comp ! defaultAgr ++ f.neg ++ f.inf ++ f.fin
      } ;
    RelSlash rp slash = {
      s = \\t,p => slash.s ! t ! p ++ rp.s ++ slash.c2.s
      } ;
    IdRP = {s = "ఏ"} ;
    FunRP prep np rp = {s = np.s ! NPC Obl ++ prep.s ++ rp.s} ;
}
