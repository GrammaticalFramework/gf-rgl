concrete StructuralMkd of Structural = CatMkd ** open ResMkd,MorphoMkd,ParadigmsMkd in {

lin i_Pron = mkPron "јас" "мене" "ме" "мене" "ми" "мене" "мој" "мојот" "моја" "мојата" "мое" "моето" "мои" "моите" "ми" (GSg Masc) P1 ;
lin youSg_Pron = mkPron "ти" "тебе" "те" "тебе" "ти" "тебе" "твој" "твојот" "твоја" "твојата" "твое" "твоето" "твои" "твоите" "ти" (GSg Masc) P2 ;
lin he_Pron = mkPron "тој" "него" "го" "нему" "му" "него" "негов" "неговиот" "негова" "неговата" "негово" "неговото" "негови" "неговите" "му" (GSg Masc) P3 ;
lin she_Pron = mkPron "таа" "неа" "ја" "нејзе" "ѝ" "неа" "нејзин" "нејзиниот" "нејзина" "нејзината" "нејзино" "нејзиното" "нејзини" "нејзините" "ѝ" (GSg Fem) P3 ;
lin it_Pron = mkPron "тоа" "него" "го" "нему" "му" "него" "негов" "неговиот" "негова" "неговата" "негово" "неговото" "негови" "неговите" "му" (GSg Neuter) P3 ;
lin we_Pron = mkPron "ние" "нас" "нѐ" "нам" "ни" "нас" "наш" "нашиот" "наша" "нашата" "наше" "нашето" "наши" "нашите" "ни" GPl P1 ;
lin youPl_Pron = mkPron "вие" "вас" "ве" "вам" "ви" "вас" "ваш" "вашиот" "ваша" "вашата" "ваше" "вашето" "ваши" "вашите" "ви" GPl P2 ;
lin youPol_Pron = mkPron "вие" "вас" "ве" "вам" "ви" "вас" "ваш" "вашиот" "ваша" "вашата" "ваше" "вашето" "ваши" "вашите" "ви" (GSg Masc) P2 ;
lin they_Pron = mkPron "тие" "нив" "ги" "ним" "им" "нив" "нивен" "нивниот" "нивна" "нивната" "нивно" "нивното" "нивни" "нивните" "им" GPl P3 ;
lin this_Quant = mkQuant "овој" "оваа" "ова" "овие" ;
lin that_Quant = mkQuant "тој" "таа" "тоа" "тие" ;
lin how_IAdv = mkIAdv "как" ;
lin how8many_IDet = mkIDet "колку" ;
lin how8much_IAdv = mkIAdv "колку" ;
lin whatSg_IP = mkIP "што" (GSg Masc) ;
lin whatPl_IP = mkIP "што" GPl ;
lin when_IAdv = mkIAdv "кога" ;
lin where_IAdv = mkIAdv "каде" ;
lin which_IQuant = mkIQuant "кој" "која" "кое" "кои" ;
lin whoSg_IP = mkIP "кој" (GSg Masc) ;
lin whoPl_IP = mkIP "кои" GPl ;
lin and_Conj = mkConj "и" Pl ;
lin or_Conj = mkConj "или" Sg ;
lin both7and_DConj = mkConj "и" Pl ** {sep=0} ;
lin either7or_DConj = mkConj "или" Sg ** {sep=1} ;

}
