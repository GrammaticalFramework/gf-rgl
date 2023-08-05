--# -path=.:../common:../abstract

concrete ExtendLit of Extend =
  CatLit ** ExtendFunctor - [
    iFem_Pron, youFem_Pron, theyFem_Pron, ProDrop
  ]
  with
    (Grammar = GrammarLit) **
  open MorphoPronounLit in {

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

lin
  UseDAP = dap2np Neut ;
  UseDAPMasc = dap2np (Masc Personal) ;
  UseDAPFem = dap2np Fem ;

oper
  dap2np : Gender -> DAP -> NP ;
  dap2np g dap = lin NP {
    nom = dap.sp ! Nom  ! g;
    voc = dap.sp ! VocP ! g;
    dep = \\cc => let c = extract_case ! cc
                  in dap.sp ! c ! g;
--    gn = accom_gennum ! <dap.a, g, dap.n>;
    gn = cast_gennum ! <g, dap.n>;
    p = P3        
  };

}
