--# -path=.:../common:../abstract

concrete ExtendPol of Extend =
  CatPol ** ExtendFunctor - [
    iFem_Pron, youFem_Pron, theyFem_Pron, ProDrop
  ]
  with
    (Grammar = GrammarPol) **
  open PronounMorphoPol in {

lin iFem_Pron = pronJa FemSg ;
lin youFem_Pron = pronTy FemSg ;
lin theyFem_Pron = pronOneFem ;

lin ProDrop p = {
    nom = [] ;
    voc = p.voc ;
    dep = p.dep ;
    sp = p.sp ;
    p  = p.p ;
    gn = p.gn ;
 } ;

}
