resource ParadigmsKaz = MorphoKaz  ** open Predef, Prelude, CatKaz, ResKaz in {
oper
  regN : Str -> N   -- s;Nom;Sg
    = \form -> case form of {
		_ + "дас" => mkN004 form;
		_ + "нас" => mkN004 form;
		_ + "йыс" => mkN004 form;
		_ + "дыс" => mkN020 form;
		_ + "ніс" => mkN009 form;
		_ + "гіс" => mkN009 form;
		_ + "тіс" => mkN022 form;
		_ + "мес" => mkN009 form;
		_ + "ғат" => mkN020 form;
		_ + "нат" => mkN004 form;
		_ + "пат" => mkN004 form;
		_ + "жат" => mkN004 form;
		_ + "зат" => mkN020 form;
		_ + "ұлт" => mkN004 form;
		_ + "ілт" => mkN021 form;
		_ + "уыт" => mkN004 form;
		_ + "ірт" => mkN009 form;
		_ + "орт" => mkN020 form;
		_ + "ырт" => mkN020 form;
		_ + "шот" => mkN004 form;
		_ + "ант" => mkN004 form;
		_ + "ент" => mkN021 form;
		_ + "хит" => mkN004 form;
		_ + "нөт" => mkN009 form;
		_ + "тет" => mkN009 form;
		_ + "лет" => mkN022 form;
		_ + "ует" => mkN022 form;
		_ + "шіт" => mkN009 form;
		_ + "гіт" => mkN009 form;
		_ + "ныш" => mkN001 form;
		_ + "лыш" => mkN001 form;
		_ + "ңеш" => mkN055 form;
		_ + "піш" => mkN021 form;
		_ + "ніш" => mkN021 form;
		_ + "ліш" => mkN021 form;
		_ + "ріш" => mkN022 form;
		_ + "шам" => mkN003 form;
		_ + "нам" => mkN064 form;
		_ + "йым" => mkN026 form;
		_ + "рым" => mkN026 form;
		_ + "жым" => mkN026 form;
		_ + "ным" => mkN066 form;
		_ + "тым" => mkN066 form;
		_ + "нім" => mkN016 form;
		_ + "рім" => mkN035 form;
		_ + "дем" => mkN035 form;
		_ + "тан" => mkN003 form;
		_ + "қан" => mkN003 form;
		_ + "ран" => mkN003 form;
		_ + "жан" => mkN003 form;
		_ + "оян" => mkN003 form;
		_ + "мән" => mkN028 form;
		_ + "сін" => mkN028 form;
		_ + "кін" => mkN028 form;
		_ + "гін" => mkN035 form;
		_ + "лен" => mkN028 form;
		_ + "рен" => mkN028 form;
		_ + "йың" => mkN003 form;
		_ + "заң" => mkN019 form;
		_ + "пап" => mkN004 form;
		_ + "лып" => mkN004 form;
		_ + "қып" => mkN020 form;
		_ + "сық" => mkN004 form;
		_ + "шық" => mkN004 form;
		_ + "зақ" => mkN004 form;
		_ + "уақ" => mkN004 form;
		_ + "қақ" => mkN004 form;
		_ + "ыла" => mkN030 form;
		_ + "ола" => mkN030 form;
		_ + "рда" => mkN030 form;
		_ + "ұра" => mkN030 form;
		_ + "йра" => mkN030 form;
		_ + "аға" => mkN030 form;
		_ + "ұма" => mkN030 form;
		_ + "йна" => mkN030 form;
		_ + "хна" => mkN030 form;
		_ + "ина" => mkN030 form;
		_ + "зба" => mkN030 form;
		_ + "шқа" => mkN030 form;
		_ + "сқа" => mkN030 form;
		_ + "ьша" => mkN024 form;
		_ + "ыша" => mkN024 form;
		_ + "опа" => mkN024 form;
		_ + "ашы" => mkN005 form;
		_ + "лшы" => mkN005 form;
		_ + "яшы" => mkN005 form;
		_ + "юшы" => mkN005 form;
		_ + "озы" => mkN005 form;
		_ + "аты" => mkN024 form;
		_ + "ция" => mkN030 form;
		_ + "бар" => mkN017 form;
		_ + "уар" => mkN017 form;
		_ + "сар" => mkN017 form;
		_ + "пар" => mkN017 form;
		_ + "мыр" => mkN025 form;
		_ + "сыр" => mkN017 form;
		_ + "тыр" => mkN017 form;
		_ + "жыр" => mkN017 form;
		_ + "ңір" => mkN018 form;
		_ + "қай" => mkN017 form;
		_ + "най" => mkN025 form;
		_ + "уру" => mkN006 form;
		_ + "іру" => mkN018 form;
		_ + "жау" => mkN006 form;
		_ + "ғау" => mkN006 form;
		_ + "ұну" => mkN017 form;
		_ + "діл" => mkN018 form;
		_ + "тіл" => mkN018 form;
		_ + "ріл" => mkN018 form;
		_ + "пал" => mkN038 form;
		_ + "сал" => mkN038 form;
		_ + "иыл" => mkN025 form;
		_ + "қыл" => mkN038 form;
		_ + "жол" => mkN038 form;
		_ + "дек" => mkN009 form;
		_ + "жек" => mkN009 form;
		_ + "тек" => mkN009 form;
		_ + "рік" => mkN033 form;
		_ + "жік" => mkN009 form;
		_ + "сік" => mkN033 form;
		_ + "шік" => mkN033 form;
		_ + "пік" => mkN033 form;
		_ + "зік" => mkN033 form;
		_ + "біз" => mkN010 form;
		_ + "көз" => mkN010 form;
		_ + "сөз" => mkN067 form;
		_ + "быз" => mkN039 form;
		_ + "кше" => mkN042 form;
		_ + "рпе" => mkN042 form;
		_ + "еде" => mkN023 form;
		_ + "йде" => mkN042 form;
		_ + "өбе" => mkN042 form;
		_ + "ңге" => mkN023 form;
		_ + "кте" => mkN042 form;
		_ + "мле" => mkN042 form;
		_ + "кші" => mkN042 form;
		_ + "нші" => mkN042 form;
		_ + "зші" => mkN042 form;
		_ + "иші" => mkN042 form;
		_ + "лші" => mkN042 form;
		_ + "пші" => mkN042 form;
		_ + "ңкі" => mkN042 form;
		_ + "іс" => mkN021 form;
		_ + "ес" => mkN021 form;
		_ + "үс" => mkN021 form;
		_ + "өс" => mkN021 form;
		_ + "ит" => mkN021 form;
		_ + "өт" => mkN021 form;
		_ + "ет" => mkN021 form;
		_ + "іт" => mkN021 form;
		_ + "ст" => mkN021 form;
		_ + "ят" => mkN020 form;
		_ + "үт" => mkN022 form;
		_ + "аш" => mkN001 form;
		_ + "еш" => mkN021 form;
		_ + "іш" => mkN009 form;
		_ + "үш" => mkN021 form;
		_ + "ам" => mkN002 form;
		_ + "ым" => mkN003 form;
		_ + "ұм" => mkN003 form;
		_ + "әм" => mkN016 form;
		_ + "ем" => mkN016 form;
		_ + "зм" => mkN035 form;
		_ + "ын" => mkN003 form;
		_ + "он" => mkN003 form;
		_ + "ән" => mkN016 form;
		_ + "ін" => mkN016 form;
		_ + "үн" => mkN016 form;
		_ + "ен" => mkN016 form;
		_ + "ың" => mkN026 form;
		_ + "аң" => mkN003 form;
		_ + "оң" => mkN026 form;
		_ + "өп" => mkN022 form;
		_ + "іп" => mkN009 form;
		_ + "оп" => mkN020 form;
		_ + "еп" => mkN022 form;
		_ + "ық" => mkN020 form;
		_ + "оқ" => mkN020 form;
		_ + "ңқ" => mkN020 form;
		_ + "та" => mkN030 form;
		_ + "ша" => mkN030 form;
		_ + "ва" => mkN024 form;
		_ + "ка" => mkN030 form;
		_ + "уа" => mkN024 form;
		_ + "са" => mkN030 form;
		_ + "жа" => mkN030 form;
		_ + "қы" => mkN005 form;
		_ + "жы" => mkN005 form;
		_ + "йы" => mkN024 form;
		_ + "вр" => mkN006 form;
		_ + "ур" => mkN006 form;
		_ + "ор" => mkN006 form;
		_ + "ар" => mkN006 form;
		_ + "ыр" => mkN006 form;
		_ + "яр" => mkN006 form;
		_ + "ір" => mkN036 form;
		_ + "ұр" => mkN025 form;
		_ + "әр" => mkN036 form;
		_ + "ий" => mkN018 form;
		_ + "ей" => mkN018 form;
		_ + "ау" => mkN017 form;
		_ + "қу" => mkN017 form;
		_ + "еу" => mkN034 form;
		_ + "ел" => mkN007 form;
		_ + "іл" => mkN007 form;
		_ + "үл" => mkN037 form;
		_ + "өл" => mkN037 form;
		_ + "ек" => mkN033 form;
		_ + "үк" => mkN009 form;
		_ + "із" => mkN029 form;
		_ + "үз" => mkN029 form;
		_ + "өз" => mkN029 form;
		_ + "ез" => mkN010 form;
		_ + "аз" => mkN039 form;
		_ + "әж" => mkN029 form;
		_ + "пе" => mkN023 form;
		_ + "ке" => mkN042 form;
		_ + "ме" => mkN042 form;
		_ + "зе" => mkN042 form;
		_ + "же" => mkN042 form;
		_ + "рі" => mkN042 form;
		_ + "ні" => mkN042 form;
		_ + "сі" => mkN042 form;
		_ + "с" => mkN001 form;
		_ + "т" => mkN001 form;
		_ + "ш" => mkN004 form;
		_ + "м" => mkN028 form;
		_ + "н" => mkN026 form;
		_ + "ң" => mkN016 form;
		_ + "п" => mkN043 form;
		_ + "д" => mkN004 form;
		_ + "қ" => mkN015 form;
		_ + "х" => mkN004 form;
		_ + "а" => mkN005 form;
		_ + "ы" => mkN030 form;
		_ + "я" => mkN024 form;
		_ + "р" => mkN034 form;
		_ + "и" => mkN006 form;
		_ + "й" => mkN006 form;
		_ + "ю" => mkN006 form;
		_ + "у" => mkN025 form;
		_ + "л" => mkN027 form;
		_ + "к" => mkN022 form;
		_ + "з" => mkN013 form;
		_ + "ж" => mkN013 form;
		_ + "е" => mkN032 form;
		_ + "і" => mkN032 form;
		_ + "ә" => mkN032 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2N : Str -> Str -> N   -- s;Nom;Sg  poss;Sg;P3;Pl
    = \form1, form2 -> case <form1, form2> of {
		<_ + "ғат", _ + "ы"> => mkN001 form1;
		<_ + "нат", _ + "ы"> => mkN001 form1;
		<_ + "уыт", _ + "ы"> => mkN001 form1;
		<_ + "пат", _ + "ы"> => mkN001 form1;
		<_ + "дас", _ + "ы"> => mkN001 form1;
		<_ + "йым", _ + "ы"> => mkN002 form1;
		<_ + "тан", _ + "ы"> => mkN026 form1;
		<_ + "қан", _ + "ы"> => mkN026 form1;
		<_ + "ран", _ + "ы"> => mkN026 form1;
		<_ + "сық", _ + "ы"> => mkN015 form1;
		<_ + "шық", _ + "ы"> => mkN015 form1;
		<_ + "зба", _ + "ы"> => mkN005 form1;
		<_ + "мыр", _ + "ы"> => mkN017 form1;
		<_ + "дек", _ + "і"> => mkN033 form1;
		<_ + "тек", _ + "і"> => mkN033 form1;
		<_ + "көз", _ + "і"> => mkN029 form1;
		<_ + "ым", _ + "ы"> => mkN002 form1;
		<_ + "ын", _ + "ы"> => mkN026 form1;
		<_ + "аң", _ + "ы"> => mkN026 form1;
		<_ + "ық", _ + "ы"> => mkN015 form1;
		<_ + "та", _ + "ы"> => mkN005 form1;
		<_ + "ша", _ + "ы"> => mkN005 form1;
		<_ + "ор", _ + "ы"> => mkN017 form1;
		<_ + "ар", _ + "ы"> => mkN017 form1;
		<_ + "ыр", _ + "ы"> => mkN017 form1;
		<_ + "үл", _ + "і"> => mkN007 form1;
		<_ + "өл", _ + "і"> => mkN007 form1;
		<_ + "іш", _ + "і"> => mkN021 form1;
		<_ + "ір", _ + "і"> => mkN034 form1;
		<_ + "оп", _ + "ы"> => mkN043 form1;
		<_ + "т", _ + "і"> => mkN021 form1;
		<_ + "ш", _ + "ы"> => mkN001 form1;
		<_ + "ы", _ + "ы"> => mkN005 form1;
		<_ + "я", _ + "ы"> => mkN005 form1;
		<_ + "й", _ + "ы"> => mkN017 form1;
		<_ + "й", _ + "і"> => mkN034 form1;
		<_ + "у", _ + "ы"> => mkN017 form1;
		<_ + "к", _ + "і"> => mkN033 form1;
		<_ + "з", _ + "ы"> => mkN039 form1;
		<_ + "м", _ + "і"> => mkN016 form1;
		<_ + "е", _ + "і"> => mkN042 form1;
		<_ + "і", _ + "і"> => mkN042 form1;
		_ => regN form1
  } ;

  regV : Str -> V   -- Infinitive
    = \form -> case form of {
		_ + "лту" => mkV005 form;
		_ + "pту" => mkV007 form;
		_ + "eту" => mkV007 form;
		_ + "iту" => mkV007 form;
		_ + "үту" => mkV010 form;
		_ + "тaу" => mkV006 form;
		_ + "сaу" => mkV006 form;
		_ + "нaу" => mkV011 form;
		_ + "қaу" => mkV011 form;
		_ + "paу" => mkV026 form;
		_ + "apу" => mkV003 form;
		_ + "ыpу" => mkV003 form;
		_ + "ipу" => mkV023 form;
		_ + "ту" => mkV001 form;
		_ + "aу" => mkV002 form;
		_ + "лу" => mkV033 form;
		_ + "шу" => mkV010 form;
		_ + "уу" => mkV006 form;
		_ + "eу" => mkV009 form;
		_ + "су" => mkV010 form;
		_ + "бу" => mkV020 form;
		_ + "ңу" => mkV023 form;
		_ + "у" => mkV016 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2V : Str -> Str -> V   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg
    = \form1, form2 -> case <form1, form2> of {
		<_ + "aу", _ + "iн"> => mkV011 form1;
		_ => regV form1
  } ;

  reg3V : Str -> Str -> Str -> V   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg  Indicative;Pres;Progressive;Pos;P1;Pl
    = \form1, form2, form3 -> case <form1, form2, form3> of {
		_ => reg2V form1 form2
  } ;

mkN = overload {
  mkN : Str -> N = regN;   -- s;Nom;Sg
  mkN : Str -> Str -> N = reg2N   -- s;Nom;Sg  poss;Sg;P3;Pl
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
  mkV : Str -> V = regV;   -- Infinitive
  mkV : Str -> Str -> V = reg2V;   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg
  mkV : Str -> Str -> Str -> V = reg3V   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg  Indicative;Pres;Progressive;Pos;P1;Pl
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
