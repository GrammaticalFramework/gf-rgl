-- Seems like this is deprecated - need to use ExtendRus instead

concrete ExtraRus of ExtraRusAbs = CatRus **
  open ResRus, MorphoRus, (P = ParadigmsRus), Prelude, NounRus in {
  flags optimize=all ; coding=utf8 ;

lin
  obj_no_Prep = {s="" ; c=Acc ; hasPrep=False} ;
  to2_Prep = {s="в" ; c=Acc ; hasPrep=True};
  on_to_Prep = {s="до"; c=Gen; hasPrep=True} ;
  along_Prep = {s="по"; c=Loc; hasPrep=True} ;
  from2_Prep = from2 ;
  about_Prep = {s="о" ; c=Pre; hasPrep=True} ;
}