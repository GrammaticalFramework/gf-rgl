concrete ExtraPes of ExtraPesAbs = CatPes **
  open ResPes, ExtendPes, Coordination, Prelude, MorphoPes, ParadigmsPes in {

  flags coding = utf8;

  lin
    GenNP = ExtendPes.GenNP ;

    IAdvAdv adv = {s = "تا چه" ++ adv.s} ;
}
