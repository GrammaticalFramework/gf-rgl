concrete RelativeHye of Relative = CatHye ** open ResHye in {
  lin
    RelCl cl = {s = "որը" ++ cl.s} ;
    RelVP rp vp = {s = rp.s ++ vp.s} ;
    RelSlash rp slash = {s = rp.s ++ slash.s} ;
    IdRP = {s = "որը"} ;
    FunRP prep np rp = {s = case prep.isPre of {
      True=>prep.s++np.s!prep.c++rp.s;
      False=>np.s!prep.c++prep.s++rp.s
    }} ;
}
