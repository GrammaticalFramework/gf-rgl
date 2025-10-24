--# -path=.:../abstract:../common:prelude
concrete ExtendMkd of Extend = CatMkd ** open ResMkd, ParadigmsMkd, StructuralMkd in {

lin
  CompoundN n1 n2 = 
    let comp : Species => Number => Str
             = \\sp,n => case n1.relType of {
                           Pref   => n1.rel ! sp ! genNum n2.g n ++ n2.s ! sp ! n ;
                           AdjMod => n1.rel ! sp ! genNum n2.g n ++ n2.s ! Indef ! n ;
                           AdvMod => n2.s ! sp ! n ++ n1.rel ! sp ! genNum n2.g n
                         } ;
        voc : Number => Str
             = \\n => case n1.relType of {
                        Pref   => n1.rel ! Indef ! genNum n2.g n ++ n2.vocative ! n ;
                        AdjMod => n1.rel ! Indef ! genNum n2.g n ++ n2.vocative ! n ;
                        AdvMod => n2.vocative ! n ++ n1.rel ! Indef ! genNum n2.g n
                      }
    in {
         s   = comp ;
         count_form = comp ! Indef ! Pl ;
         vocative = voc ;
         rel = \\sp,n => "на" ++ comp ! sp ! Sg ;  relType = AdvMod ;
         g   = n2.g
    } ;

  iFem_Pron      = mkPron "јас" "мене" "ме" "мене" "ми" "мене" "мој" "мојот" "моја" "мојата" "мое" "моето" "мои" "моите" "ми" (GSg Fem) P1 ;
  youFem_Pron    = mkPron "ти" "тебе" "те" "тебе" "ти" "тебе" "твој" "твојот" "твоја" "твојата" "твое" "твоето" "твои" "твоите" "ти" (GSg Fem) P2 ;
  weFem_Pron     = we_Pron ;
  youPlFem_Pron  = youPl_Pron ;
  theyFem_Pron   = they_Pron ;
  youPolFem_Pron = mkPron "вие" "вас" "ве" "вам" "ви" "вас" "ваш" "вашиот" "ваша" "вашата" "ваше" "вашето" "ваши" "вашите" "ви" (GSg Fem) P2 ;
  youPolPl_Pron  = youPol_Pron ;
  youPolPlFem_Pron = youPol_Pron ;

lin TPastSimple = {s = []} ** {t = VPastSimple} ;  --# notpresent

}

