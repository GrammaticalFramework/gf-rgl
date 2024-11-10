resource ParadigmsKaz = MorphoKaz  ** open Prelude, CatKaz, ResKaz in {
oper
regN : Str -> N
= \form -> case form of {
		_ + "пап" => mkN001 form;
		_ + "нып" => mkN028 form;
		_ + "ірт" => mkN007 form;
		_ + "ілт" => mkN007 form;
		_ + "ент" => mkN007 form;
		_ + "рық" => mkN013 form;
		_ + "уық" => mkN013 form;
		_ + "қық" => mkN013 form;
		_ + "йық" => mkN013 form;
		_ + "бық" => mkN013 form;
		_ + "зық" => mkN013 form;
		_ + "ңеш" => mkN036 form;
		_ + "дам" => mkN002 form;
		_ + "шам" => mkN002 form;
		_ + "нам" => mkN004 form;
		_ + "ным" => mkN032 form;
		_ + "тым" => mkN032 form;
		_ + "мар" => mkN004 form;
		_ + "ңір" => mkN011 form;
		_ + "діл" => mkN011 form;
		_ + "тіл" => mkN011 form;
		_ + "ріл" => mkN011 form;
		_ + "иыл" => mkN010 form;
		_ + "рік" => mkN023 form;
		_ + "сік" => mkN023 form;
		_ + "шік" => mkN023 form;
		_ + "пік" => mkN023 form;
		_ + "зік" => mkN023 form;
		_ + "біз" => mkN008 form;
		_ + "сөз" => mkN008 form;
		_ + "іру" => mkN011 form;
		_ + "ьша" => mkN056 form;
		_ + "ыша" => mkN056 form;
		_ + "опа" => mkN056 form;
		_ + "нші" => mkN022 form;
		_ + "кші" => mkN022 form;
		_ + "зші" => mkN022 form;
		_ + "иші" => mkN022 form;
		_ + "лші" => mkN022 form;
		_ + "пші" => mkN022 form;
		_ + "ңкі" => mkN022 form;
		_ + "кте" => mkN022 form;
		_ + "рпе" => mkN022 form;
		_ + "кше" => mkN022 form;
		_ + "йде" => mkN022 form;
		_ + "еде" => mkN055 form;
		_ + "өбе" => mkN022 form;
		_ + "ңге" => mkN055 form;
		_ + "іс" => mkN007 form;
		_ + "ес" => mkN007 form;
		_ + "үс" => mkN007 form;
		_ + "өс" => mkN007 form;
		_ + "ап" => mkN028 form;
		_ + "оп" => mkN001 form;
		_ + "ып" => mkN001 form;
		_ + "ит" => mkN007 form;
		_ + "өт" => mkN007 form;
		_ + "ет" => mkN007 form;
		_ + "үт" => mkN007 form;
		_ + "іт" => mkN007 form;
		_ + "ст" => mkN007 form;
		_ + "ақ" => mkN013 form;
		_ + "яқ" => mkN013 form;
		_ + "еш" => mkN007 form;
		_ + "іш" => mkN007 form;
		_ + "үш" => mkN007 form;
		_ + "ам" => mkN032 form;
		_ + "ым" => mkN002 form;
		_ + "ұм" => mkN002 form;
		_ + "ән" => mkN014 form;
		_ + "ін" => mkN014 form;
		_ + "ен" => mkN014 form;
		_ + "үн" => mkN014 form;
		_ + "өң" => mkN014 form;
		_ + "ең" => mkN014 form;
		_ + "ің" => mkN014 form;
		_ + "үң" => mkN014 form;
		_ + "вр" => mkN004 form;
		_ + "ір" => mkN024 form;
		_ + "ер" => mkN024 form;
		_ + "үр" => mkN024 form;
		_ + "өр" => mkN024 form;
		_ + "әр" => mkN024 form;
		_ + "ел" => mkN025 form;
		_ + "іл" => mkN025 form;
		_ + "өл" => mkN025 form;
		_ + "үл" => mkN025 form;
		_ + "ек" => mkN023 form;
		_ + "із" => mkN016 form;
		_ + "өз" => mkN016 form;
		_ + "үз" => mkN016 form;
		_ + "ез" => mkN016 form;
		_ + "ий" => mkN011 form;
		_ + "ей" => mkN011 form;
		_ + "еу" => mkN024 form;
		_ + "ва" => mkN056 form;
		_ + "уа" => mkN056 form;
		_ + "йы" => mkN056 form;
		_ + "ия" => mkN056 form;
		_ + "ні" => mkN022 form;
		_ + "рі" => mkN022 form;
		_ + "сі" => mkN022 form;
		_ + "ке" => mkN022 form;
		_ + "ме" => mkN022 form;
		_ + "зе" => mkN022 form;
		_ + "же" => mkN022 form;
		_ + "пе" => mkN055 form;
		_ + "с" => mkN001 form;
		_ + "п" => mkN007 form;
		_ + "т" => mkN001 form;
		_ + "д" => mkN001 form;
		_ + "қ" => mkN001 form;
		_ + "ш" => mkN001 form;
		_ + "х" => mkN001 form;
		_ + "м" => mkN014 form;
		_ + "н" => mkN002 form;
		_ + "ң" => mkN002 form;
		_ + "р" => mkN010 form;
		_ + "л" => mkN015 form;
		_ + "к" => mkN007 form;
		_ + "з" => mkN012 form;
		_ + "и" => mkN010 form;
		_ + "й" => mkN010 form;
		_ + "ю" => mkN010 form;
		_ + "у" => mkN010 form;
		_ + "ж" => mkN012 form;
		_ + "а" => mkN017 form;
		_ + "ы" => mkN017 form;
		_ + "я" => mkN017 form;
		_ + "і" => mkN057 form;
		_ + "е" => mkN057 form;
		_ + "ә" => mkN057 form	
} ;

reg2N : Str -> Str -> N
= \form1, form2 -> case <form1, form2> of {
		<_ + "пап", _> => mkN001 form1;
		<_ + "нып", _> => mkN028 form1;
		<_ + "ірт", _> => mkN007 form1;
		<_ + "ілт", _> => mkN007 form1;
		<_ + "ент", _> => mkN007 form1;
		<_ + "рық", _> => mkN013 form1;
		<_ + "уық", _> => mkN013 form1;
		<_ + "қық", _> => mkN013 form1;
		<_ + "йық", _> => mkN013 form1;
		<_ + "бық", _> => mkN013 form1;
		<_ + "зық", _> => mkN013 form1;
		<_ + "ңеш", _> => mkN036 form1;
		<_ + "дам", _> => mkN002 form1;
		<_ + "шам", _> => mkN002 form1;
		<_ + "нам", _> => mkN004 form1;
		<_ + "ным", _> => mkN032 form1;
		<_ + "тым", _> => mkN032 form1;
		<_ + "мар", _> => mkN004 form1;
		<_ + "ңір", _> => mkN011 form1;
		<_ + "діл", _> => mkN011 form1;
		<_ + "тіл", _> => mkN011 form1;
		<_ + "ріл", _> => mkN011 form1;
		<_ + "иыл", _> => mkN010 form1;
		<_ + "рік", _> => mkN023 form1;
		<_ + "сік", _> => mkN023 form1;
		<_ + "шік", _> => mkN023 form1;
		<_ + "пік", _> => mkN023 form1;
		<_ + "зік", _> => mkN023 form1;
		<_ + "біз", _> => mkN008 form1;
		<_ + "сөз", _> => mkN008 form1;
		<_ + "іру", _> => mkN011 form1;
		<_ + "ьша", _> => mkN056 form1;
		<_ + "ыша", _> => mkN056 form1;
		<_ + "опа", _> => mkN056 form1;
		<_ + "нші", _> => mkN022 form1;
		<_ + "кші", _> => mkN022 form1;
		<_ + "зші", _> => mkN022 form1;
		<_ + "иші", _> => mkN022 form1;
		<_ + "лші", _> => mkN022 form1;
		<_ + "пші", _> => mkN022 form1;
		<_ + "ңкі", _> => mkN022 form1;
		<_ + "кте", _> => mkN022 form1;
		<_ + "рпе", _> => mkN022 form1;
		<_ + "кше", _> => mkN022 form1;
		<_ + "йде", _> => mkN022 form1;
		<_ + "еде", _> => mkN055 form1;
		<_ + "өбе", _> => mkN022 form1;
		<_ + "ңге", _> => mkN055 form1;
		<_ + "іс", _> => mkN007 form1;
		<_ + "ес", _> => mkN007 form1;
		<_ + "үс", _> => mkN007 form1;
		<_ + "өс", _> => mkN007 form1;
		<_ + "ап", _> => mkN028 form1;
		<_ + "оп", _> => mkN001 form1;
		<_ + "ып", _> => mkN001 form1;
		<_ + "ит", _> => mkN007 form1;
		<_ + "өт", _> => mkN007 form1;
		<_ + "ет", _> => mkN007 form1;
		<_ + "үт", _> => mkN007 form1;
		<_ + "іт", _> => mkN007 form1;
		<_ + "ст", _> => mkN007 form1;
		<_ + "ақ", _> => mkN013 form1;
		<_ + "яқ", _> => mkN013 form1;
		<_ + "еш", _> => mkN007 form1;
		<_ + "іш", _> => mkN007 form1;
		<_ + "үш", _> => mkN007 form1;
		<_ + "ам", _> => mkN032 form1;
		<_ + "ым", _> => mkN002 form1;
		<_ + "ұм", _> => mkN002 form1;
		<_ + "ән", _> => mkN014 form1;
		<_ + "ін", _> => mkN014 form1;
		<_ + "ен", _> => mkN014 form1;
		<_ + "үн", _> => mkN014 form1;
		<_ + "өң", _> => mkN014 form1;
		<_ + "ең", _> => mkN014 form1;
		<_ + "ің", _> => mkN014 form1;
		<_ + "үң", _> => mkN014 form1;
		<_ + "вр", _> => mkN004 form1;
		<_ + "ір", _> => mkN024 form1;
		<_ + "ер", _> => mkN024 form1;
		<_ + "үр", _> => mkN024 form1;
		<_ + "өр", _> => mkN024 form1;
		<_ + "әр", _> => mkN024 form1;
		<_ + "ел", _> => mkN025 form1;
		<_ + "іл", _> => mkN025 form1;
		<_ + "өл", _> => mkN025 form1;
		<_ + "үл", _> => mkN025 form1;
		<_ + "ек", _> => mkN023 form1;
		<_ + "із", _> => mkN016 form1;
		<_ + "өз", _> => mkN016 form1;
		<_ + "үз", _> => mkN016 form1;
		<_ + "ез", _> => mkN016 form1;
		<_ + "ий", _> => mkN011 form1;
		<_ + "ей", _> => mkN011 form1;
		<_ + "еу", _> => mkN024 form1;
		<_ + "ва", _> => mkN056 form1;
		<_ + "уа", _> => mkN056 form1;
		<_ + "йы", _> => mkN056 form1;
		<_ + "ия", _> => mkN056 form1;
		<_ + "ні", _> => mkN022 form1;
		<_ + "рі", _> => mkN022 form1;
		<_ + "сі", _> => mkN022 form1;
		<_ + "ке", _> => mkN022 form1;
		<_ + "ме", _> => mkN022 form1;
		<_ + "зе", _> => mkN022 form1;
		<_ + "же", _> => mkN022 form1;
		<_ + "пе", _> => mkN055 form1;
		<_ + "с", _> => mkN001 form1;
		<_ + "п", _> => mkN007 form1;
		<_ + "т", _> => mkN001 form1;
		<_ + "д", _> => mkN001 form1;
		<_ + "қ", _> => mkN001 form1;
		<_ + "ш", _> => mkN001 form1;
		<_ + "х", _> => mkN001 form1;
		<_ + "м", _> => mkN014 form1;
		<_ + "н", _> => mkN002 form1;
		<_ + "ң", _> => mkN002 form1;
		<_ + "р", _> => mkN010 form1;
		<_ + "л", _> => mkN015 form1;
		<_ + "к", _> => mkN007 form1;
		<_ + "з", _> => mkN012 form1;
		<_ + "и", _> => mkN010 form1;
		<_ + "й", _> => mkN010 form1;
		<_ + "ю", _> => mkN010 form1;
		<_ + "у", _> => mkN010 form1;
		<_ + "ж", _> => mkN012 form1;
		<_ + "а", _> => mkN017 form1;
		<_ + "ы", _> => mkN017 form1;
		<_ + "я", _> => mkN017 form1;
		<_ + "і", _> => mkN057 form1;
		<_ + "е", _> => mkN057 form1;
		<_ + "ә", _> => mkN057 form1	
} ;

regV : Str -> V
= \form -> case form of {
		_ + "лту" => mkV005 form;
		_ + "рту" => mkV007 form;
		_ + "ету" => mkV007 form;
		_ + "iту" => mkV007 form;
		_ + "үту" => mkV010 form;
		_ + "тау" => mkV006 form;
		_ + "сау" => mkV006 form;
		_ + "нау" => mkV011 form;
		_ + "қау" => mkV011 form;
		_ + "рау" => mkV026 form;
		_ + "ару" => mkV003 form;
		_ + "ыру" => mkV003 form;
		_ + "iру" => mkV023 form;
		_ + "ту" => mkV001 form;
		_ + "ау" => mkV002 form;
		_ + "лу" => mkV033 form;
		_ + "шу" => mkV010 form;
		_ + "уу" => mkV006 form;
		_ + "еу" => mkV009 form;
		_ + "су" => mkV010 form;
		_ + "бу" => mkV020 form;
		_ + "ңу" => mkV023 form;
		_ + "у" => mkV016 form	
} ;

reg2V : Str -> Str -> V
= \form1, form2 -> case <form1, form2> of {
		<_ + "лту", _> => mkV005 form1;
		<_ + "рту", _> => mkV007 form1;
		<_ + "ету", _> => mkV007 form1;
		<_ + "iту", _> => mkV007 form1;
		<_ + "үту", _> => mkV010 form1;
		<_ + "тау", _> => mkV006 form1;
		<_ + "сау", _> => mkV006 form1;
		<_ + "нау", _> => mkV011 form1;
		<_ + "қау", _> => mkV011 form1;
		<_ + "рау", _> => mkV026 form1;
		<_ + "ару", _> => mkV003 form1;
		<_ + "ыру", _> => mkV003 form1;
		<_ + "iру", _> => mkV023 form1;
		<_ + "ау", _ + "iн"> => mkV011 form1;
		<_ + "ту", _> => mkV001 form1;
		<_ + "ау", _> => mkV002 form1;
		<_ + "лу", _> => mkV033 form1;
		<_ + "шу", _> => mkV010 form1;
		<_ + "уу", _> => mkV006 form1;
		<_ + "еу", _> => mkV009 form1;
		<_ + "су", _> => mkV010 form1;
		<_ + "бу", _> => mkV020 form1;
		<_ + "ңу", _> => mkV023 form1;
		<_ + "у", _> => mkV016 form1	
} ;

reg3V : Str -> Str -> Str -> V
= \form1, form2, form3 -> case <form1, form2, form3> of {
		<_ + "лту", _, _> => mkV005 form1;
		<_ + "рту", _, _> => mkV007 form1;
		<_ + "ету", _, _> => mkV007 form1;
		<_ + "iту", _, _> => mkV007 form1;
		<_ + "үту", _, _> => mkV010 form1;
		<_ + "тау", _, _> => mkV006 form1;
		<_ + "сау", _, _> => mkV006 form1;
		<_ + "нау", _, _> => mkV011 form1;
		<_ + "қау", _, _> => mkV011 form1;
		<_ + "рау", _, _> => mkV026 form1;
		<_ + "ару", _, _> => mkV003 form1;
		<_ + "ыру", _, _> => mkV003 form1;
		<_ + "iру", _, _> => mkV023 form1;
		<_ + "ау", _ + "iн", _> => mkV011 form1;
		<_ + "ту", _, _> => mkV001 form1;
		<_ + "ау", _, _> => mkV002 form1;
		<_ + "лу", _, _> => mkV033 form1;
		<_ + "шу", _, _> => mkV010 form1;
		<_ + "уу", _, _> => mkV006 form1;
		<_ + "еу", _, _> => mkV009 form1;
		<_ + "су", _, _> => mkV010 form1;
		<_ + "бу", _, _> => mkV020 form1;
		<_ + "ңу", _, _> => mkV023 form1;
		<_ + "у", _, _> => mkV016 form1	
} ;

reg4V : Str -> Str -> Str -> Str -> V
= \form1, form2, form3, form4 -> case <form1, form2, form3, form4> of {
		<_ + "лту", _, _, _> => mkV005 form1;
		<_ + "рту", _, _, _> => mkV007 form1;
		<_ + "ету", _, _, _> => mkV007 form1;
		<_ + "iту", _, _, _> => mkV007 form1;
		<_ + "үту", _, _, _> => mkV010 form1;
		<_ + "тау", _, _, _> => mkV006 form1;
		<_ + "сау", _, _, _> => mkV006 form1;
		<_ + "нау", _, _, _> => mkV011 form1;
		<_ + "қау", _, _, _> => mkV011 form1;
		<_ + "рау", _, _, _> => mkV026 form1;
		<_ + "ару", _, _, _> => mkV003 form1;
		<_ + "ыру", _, _, _> => mkV003 form1;
		<_ + "iру", _, _, _> => mkV023 form1;
		<_ + "ау", _ + "iн", _, _> => mkV011 form1;
		<_ + "ту", _, _, _> => mkV001 form1;
		<_ + "ау", _, _, _> => mkV002 form1;
		<_ + "лу", _, _, _> => mkV033 form1;
		<_ + "шу", _, _, _> => mkV010 form1;
		<_ + "уу", _, _, _> => mkV006 form1;
		<_ + "еу", _, _, _> => mkV009 form1;
		<_ + "су", _, _, _> => mkV010 form1;
		<_ + "бу", _, _, _> => mkV020 form1;
		<_ + "ңу", _, _, _> => mkV023 form1;
		<_ + "у", _, _, _> => mkV016 form1	
} ;

mkN = overload {
  mkN : Str -> N = regN;
  mkN : Str -> Str -> N = reg2N
} ;

mkN2 = overload {
  mkN2 : N -> N2 = \n -> lin N2 n ** {c2=noPrep};
  mkN2 : N -> Prep -> N2 = \n,p -> lin N2 n ** {c2=p};
} ;

mkPN : Str -> PN = \s -> lin PN {s=s} ;
mkLN : Str -> LN = \s -> lin LN {s=s} ;
mkGN : Str -> GN = \s -> lin GN {s=s} ;
mkSN : Str -> SN = \s -> lin SN {s=s} ;

mkV = overload {
  mkV : Str -> V = regV;
  mkV : Str -> Str -> V = reg2V;
  mkV : Str -> Str -> Str -> V = reg3V;
  mkV : Str -> Str -> Str -> Str -> V = reg4V
} ;

mkV2 = overload {
  mkV2 : V -> V2 = \v -> lin V2 v ** {c2=noPrep} ;
  mkV2 : V -> Prep -> V2 = \v,p -> lin V2 v ** {c2=p} ;
} ;

mkVV : V -> VV = \v -> lin VV v ;
mkVS : V -> VS = \v -> lin VS v ;
mkVQ : V -> VQ = \v -> lin VQ v ;
mkVA : V -> VA = \v -> lin VA v ;

mkV2V = overload {
  mkV2V : V -> V2V = \v -> lin V2V v ** {c2,c3=noPrep} ;
  mkV2V : V -> Prep -> Prep -> V2V = \v,p2,p3 -> lin V2V v ** {c2=p2; c3=p3} ;
} ;

mkV2S = overload {
  mkV2S : V -> V2S = \v -> lin V2S v ** {c2,c3=noPrep} ;
  mkV2S : V -> Prep -> Prep -> V2S = \v,p2,p3 -> lin V2S v ** {c2=p2; c3=p3} ;
} ;

mkV2Q = overload {
  mkV2Q : V -> V2Q = \v -> lin V2Q v ** {c2,c3=noPrep} ;
  mkV2Q : V -> Prep -> Prep -> V2Q = \v,p2,p3 -> lin V2Q v ** {c2=p2; c3=p3} ;
} ;

mkV2A = overload {
  mkV2A : V -> V2A = \v -> lin V2A v ** {c2,c3=noPrep} ;
  mkV2A : V -> Prep -> Prep -> V2A = \v,p2,p3 -> lin V2A v ** {c2=p2; c3=p3} ;
} ;

mkV3 = overload {
  mkV3 : V -> V3 = \v -> lin V3 v ** {c2,c3=noPrep} ;
  mkV3 : V -> Prep -> Prep -> V3 = \v,p2,p3 -> lin V3 v ** {c2=p2; c3=p3} ;
} ;

mkA : Str -> A = \s -> lin A {s=s} ;
mkA2 : A -> A2 = \a -> lin A2 a ** {c2=noPrep} ;

mkAdv : Str -> Adv = \s -> lin Adv {s=s} ;
mkAdV : Str -> AdV = \s -> lin AdV {s=s} ;
mkAdA : Str -> AdA = \s -> lin AdA {s=s} ;
mkAdN : Str -> AdN = \s -> lin AdN {s=s} ;

mkInterj : Str -> Interj = \s -> lin Interj {s=s} ;

mkVoc : Str -> Voc = \s -> lin Voc {s=s} ;

mkPrep : Str -> Prep = \s -> lin Prep {s=s} ;
noPrep : Prep = lin Prep {s=""} ;

}
