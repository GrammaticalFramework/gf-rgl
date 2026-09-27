concrete ConstructionKaz of Construction = CatKaz **
  open Prelude, ResKaz, ParadigmsKaz in {

  lincat Timeunit, Hour, Weekday, Month, Monthday, Year = {s : Str} ;

  lin
    ready_VP=prefixVerb "дайын" (mkV "болу");
    has_age_VP card=prefixVerb (card.s++"жаста") (mkV "болу");
    n_units_AP card cn a={s=card.s++cn.s!Nom!Sg++a.s};
    n_units_of_NP card cn np={s=\\c=>card.s++cn.s!Nom!Sg++np.s!c;a=np.a};
    cup_of_CN np={s=\\_,_=>"бір кесе"++np.s!Nom;poss=\\_,_,_=>"бір кесе"++np.s!Nom};
    monday_Weekday={s="дүйсенбі"}; tuesday_Weekday={s="сейсенбі"};
    wednesday_Weekday={s="сәрсенбі"}; thursday_Weekday={s="бейсенбі"};
    friday_Weekday={s="жұма"}; saturday_Weekday={s="сенбі"}; sunday_Weekday={s="жексенбі"};
    january_Month={s="қаңтар"}; february_Month={s="ақпан"}; march_Month={s="наурыз"};
    april_Month={s="сәуір"}; may_Month={s="мамыр"}; june_Month={s="маусым"};
    july_Month={s="шілде"}; august_Month={s="тамыз"}; september_Month={s="қыркүйек"};
    october_Month={s="қазан"}; november_Month={s="қараша"}; december_Month={s="желтоқсан"};
    weekdayPunctualAdv w={s=w.s++"күні"}; weekdayHabitualAdv w={s="әр"++w.s};
    weekdayN w={s=\\_,_=>w.s;poss=\\_,_,_=>w.s};
    monthN m={s=\\_,_=>m.s;poss=\\_,_,_=>m.s};
    intYear i={s=i.s}; yearAdv y={s=y.s++"жылы"};
}
