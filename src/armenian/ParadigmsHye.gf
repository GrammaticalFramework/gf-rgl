resource ParadigmsHye = MorphoHye  ** open Predef, Prelude, CatHye, ResHye in {
oper
  regV : Str -> V   -- s
    = \form -> case form of {
		_ + "ղալ" => mkV002 form;
		_ + "ձալ" => mkV002 form;
		_ + "զալ" => mkV002 form;
		_ + "լալ" => mkV002 form;
		_ + "թալ" => mkV002 form;
		_ + "ռալ" => mkV002 form;
		_ + "րալ" => mkV002 form;
		_ + "ւալ" => mkV002 form;
		_ + "ջալ" => mkV002 form;
		_ + "գալ" => mkV002 form;
		_ + "տալ" => mkV002 form;
		_ + "ճալ" => mkV002 form;
		_ + "սալ" => mkV002 form;
		_ + "փալ" => mkV002 form;
		_ + "կալ" => mkV002 form;
		_ + "վալ" => mkV002 form;
		_ + "բալ" => mkV002 form;
		_ + "ծալ" => mkV002 form;
		_ + "չալ" => mkV002 form;
		_ + "նալ" => mkV004 form;
		_ + "ել" => mkV001 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2V : Str -> Str -> V   -- s  Imperative_Jussive;Pl
    = \form1, form2 -> case <form1, form2> of {
		<_ + "լ", _ + "է՛ք"> => mkV012 form1;
		_ => regV form1
  } ;

  regN : Str -> N   -- s;Nom;Sg
    = \form -> case form of {
		_ + "իւն" => mkN008 form;
		_ + "ժամ" => mkN007 form;
		_ + "մոմ" => mkN007 form;
		_ + "ւնտ" => mkN007 form;
		_ + "րստ" => mkN007 form;
		_ + "խոտ" => mkN007 form;
		_ + "ելտ" => mkN007 form;
		_ + "տառ" => mkN007 form;
		_ + "երդ" => mkN007 form;
		_ + "ձող" => mkN007 form;
		_ + "փող" => mkN007 form;
		_ + "հող" => mkN007 form;
		_ + "խաղ" => mkN007 form;
		_ + "իկղ" => mkN007 form;
		_ + "շոր" => mkN007 form;
		_ + "զոր" => mkN007 form;
		_ + "շեր" => mkN016 form;
		_ + "եգր" => mkN007 form;
		_ + "թել" => mkN007 form;
		_ + "խել" => mkN007 form;
		_ + "ճոճ" => mkN007 form;
		_ + "իցք" => mkN007 form;
		_ + "ենք" => mkN007 form;
		_ + "ուրծք" => mkN005 form;
		_ + "յծք" => mkN007 form;
		_ + "նչք" => mkN007 form;
		_ + "ծոց" => mkN007 form;
		_ + "այց" => mkN007 form;
		_ + "կաց" => mkN007 form;
		_ + "ջիջ" => mkN013 form;
		_ + "եղջ" => mkN007 form;
		_ + "ւրթ" => mkN007 form;
		_ + "յոթ" => mkN007 form;
		_ + "ճապ" => mkN001 form;
		_ + "րապ" => mkN001 form;
		_ + "լեպ" => mkN001 form;
		_ + "ծագ" => mkN007 form;
		_ + "ենգ" => mkN007 form;
		_ + "գիչ" => mkN013 form;
		_ + "միչ" => mkN013 form;
		_ + "տիչ" => mkN013 form;
		_ + "նիչ" => mkN013 form;
		_ + "ւրծ" => mkN007 form;
		_ + "թու" => mkN012 form;
		_ + "զու" => mkN012 form;
		_ + "ռու" => mkN012 form;
		_ + "ճու" => mkN012 form;
		_ + "ղու" => mkN012 form;
		_ + "ճաշ" => mkN007 form;
		_ + "վիշ" => mkN013 form;
		_ + "ուրձ" => mkN005 form;
		_ + "դհի" => mkN006 form;
		_ + "ւդի" => mkN006 form;
		_ + "ուն" => mkN010 form;
		_ + "սն" => mkN007 form;
		_ + "կն" => mkN007 form;
		_ + "շն" => mkN007 form;
		_ + "մն" => mkN007 form;
		_ + "ռն" => mkN007 form;
		_ + "ձն" => mkN007 form;
		_ + "նն" => mkN007 form;
		_ + "ւմ" => mkN004 form;
		_ + "րմ" => mkN007 form;
		_ + "ղմ" => mkN007 form;
		_ + "յմ" => mkN007 form;
		_ + "հմ" => mkN007 form;
		_ + "լմ" => mkN007 form;
		_ + "յտ" => mkN007 form;
		_ + "յռ" => mkN007 form;
		_ + "ղդ" => mkN007 form;
		_ + "տղ" => mkN007 form;
		_ + "ղխ" => mkN007 form;
		_ + "լխ" => mkN007 form;
		_ + "ճխ" => mkN007 form;
		_ + "չխ" => mkN007 form;
		_ + "ղկ" => mkN007 form;
		_ + "սկ" => mkN007 form;
		_ + "տր" => mkN007 form;
		_ + "կր" => mkN007 form;
		_ + "նր" => mkN007 form;
		_ + "ղր" => mkN007 form;
		_ + "խր" => mkN007 form;
		_ + "բր" => mkN007 form;
		_ + "օր" => mkN016 form;
		_ + "յլ" => mkN007 form;
		_ + "րս" => mkN007 form;
		_ + "մս" => mkN007 form;
		_ + "փս" => mkN007 form;
		_ + "լս" => mkN007 form;
		_ + "րճ" => mkN007 form;
		_ + "եճ" => mkN007 form;
		_ + "աճ" => mkN007 form;
		_ + "նճ" => mkN007 form;
		_ + "ջք" => mkN007 form;
		_ + "մք" => mkN007 form;
		_ + "վք" => mkN007 form;
		_ + "թք" => mkN007 form;
		_ + "բք" => mkN007 form;
		_ + "խց" => mkN007 form;
		_ + "ղց" => mkN007 form;
		_ + "վթ" => mkN007 form;
		_ + "ղբ" => mkN007 form;
		_ + "րբ" => mkN007 form;
		_ + "ուբ" => mkN005 form;        
		_ + "զբ" => mkN007 form;
		_ + "եբ" => mkN007 form;
		_ + "ոպ" => mkN001 form;
		_ + "ւպ" => mkN001 form;
		_ + "իպ" => mkN001 form;
		_ + "եգ" => mkN007 form;
		_ + "իգ" => mkN007 form;
		_ + "ոգ" => mkN007 form;
		_ + "յգ" => mkN007 form;
		_ + "ուրչ" => mkN005 form;
		_ + "շչ" => mkN007 form;
		_ + "նծ" => mkN007 form;
		_ + "եծ" => mkN007 form;
		_ + "յծ" => mkN007 form;
		_ + "եւ" => mkN001 form;
		_ + "աւ" => mkN001 form;
		_ + "ւժ" => mkN007 form;
		_ + "իժ" => mkN013 form;
		_ + "քշ" => mkN007 form;
		_ + "րշ" => mkN007 form;
		_ + "ուզ" => mkN005 form;
		_ + "րզ" => mkN007 form;
		_ + "ավ" => mkN007 form;
		_ + "յվ" => mkN007 form;
		_ + "աֆ" => mkN001 form;
		_ + "լֆ" => mkN001 form;
		_ + "իփ" => mkN001 form;
		_ + "ափ" => mkN001 form;
		_ + "ոյ" => mkN007 form;
		_ + "լի" => mkN006 form;
		_ + "բի" => mkN006 form;
		_ + "խի" => mkN006 form;
		_ + "թի" => mkN006 form;
		_ + "պի" => mkN006 form;
		_ + "վի" => mkN006 form;
		_ + "փի" => mkN006 form;
		_ + "աի" => mkN006 form;
		_ + "ջի" => mkN006 form;
		_ + "ն" => mkN001 form;
		_ + "մ" => mkN001 form;
		_ + "տ" => mkN001 form;
		_ + "ռ" => mkN001 form;
		_ + "դ" => mkN001 form;
		_ + "ղ" => mkN001 form;
		_ + "խ" => mkN001 form;
		_ + "կ" => mkN001 form;
		_ + "ր" => mkN001 form;
		_ + "լ" => mkN001 form;
		_ + "ս" => mkN001 form;
		_ + "ճ" => mkN001 form;
		_ + "ք" => mkN001 form;
		_ + "ց" => mkN001 form;
		_ + "ջ" => mkN001 form;
		_ + "թ" => mkN001 form;
		_ + "բ" => mkN001 form;
		_ + "պ" => mkN007 form;
		_ + "գ" => mkN001 form;
		_ + "չ" => mkN001 form;
		_ + "ծ" => mkN001 form;
		_ + "ւ" => mkN006 form;
		_ + "ժ" => mkN001 form;
		_ + "շ" => mkN001 form;
		_ + "զ" => mkN001 form;
		_ + "ձ" => mkN007 form;
		_ + "վ" => mkN001 form;
		_ + "ֆ" => mkN007 form;
		_ + "հ" => mkN001 form;
		_ + "փ" => mkN007 form;
		_ + "յ" => mkN001 form;
		_ + "ա" => mkN002 form;
		_ + "ո" => mkN002 form;
		_ + "ի" => mkN003 form;
		_ + "ե" => mkN006 form;
		_ + "է" => mkN006 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2N : Str -> Str -> N   -- s;Nom;Sg  s;Dat;Sg
    = \form1, form2 -> case <form1, form2> of {
		<_ + "թու", _ + "ւի"> => mkN006 form1;
		<_ + "շեր", _ + "ի"> => mkN001 form1;
		<_ + "ժամ", _ + "ա"> => mkN016 form1;
		<_ + "ուն", _ + "բնի"> => mkN005 form1;
		<_ + "ւն", _ + "վան"> => mkN009 form1;
		<_ + "ւն", _ + "տան"> => mkN028 form1;
		<_ + "ւն", _ + "շան"> => mkN028 form1;
		<_ + "ւզ", _ + "ւզի"> => mkN001 form1;
		<_ + "ւն", _ + "ի"> => mkN001 form1;
		<_ + "ւն", _ + "ա"> => mkN032 form1;
		<_ + "ւմ", _ + "ի"> => mkN001 form1;
		<_ + "լի", _ + "ւ"> => mkN003 form1;
		<_ + "բի", _ + "ւ"> => mkN003 form1;
		<_ + "խի", _ + "ւ"> => mkN003 form1;
		<_ + "թի", _ + "ւ"> => mkN003 form1;
		<_ + "տ", _ + "մտի"> => mkN005 form1;
		<_ + "տ", _ + "գտի"> => mkN005 form1;
		<_ + "տ", _ + "վտի"> => mkN013 form1;
		<_ + "ռ", _ + "ճռի"> => mkN013 form1;
		<_ + "ն", _ + "ծնի"> => mkN013 form1;
		<_ + "ն", _ + "տնի"> => mkN013 form1;
		<_ + "ն", _ + "ցնի"> => mkN013 form1;
		<_ + "ն", _ + "ձնի"> => mkN013 form1;
		<_ + "ն", _ + "բնի"> => mkN013 form1;
		<_ + "ն", _ + "խնի"> => mkN013 form1;
		<_ + "ղ", _ + "ւղի"> => mkN007 form1;
		<_ + "ղ", _ + "վղի"> => mkN013 form1;
		<_ + "ղ", _ + "փղի"> => mkN015 form1;
		<_ + "ղ", _ + "տղի"> => mkN031 form1;
		<_ + "խ", _ + "ծխի"> => mkN005 form1;
		<_ + "խ", _ + "բխի"> => mkN031 form1;
		<_ + "կ", _ + "ղկի"> => mkN013 form1;
		<_ + "կ", _ + "տկի"> => mkN013 form1;
		<_ + "կ", _ + "պկի"> => mkN013 form1;
		<_ + "կ", _ + "զկի"> => mkN031 form1;
		<_ + "ս", _ + "մսի"> => mkN013 form1;
		<_ + "ճ", _ + "վճի"> => mkN013 form1;
		<_ + "ճ", _ + "հճի"> => mkN013 form1;
		<_ + "ց", _ + "տցի"> => mkN005 form1;
		<_ + "ց", _ + "կցի"> => mkN013 form1;
		<_ + "ր", _ + "դրի"> => mkN013 form1;
		<_ + "ր", _ + "գրի"> => mkN015 form1;
		<_ + "ր", _ + "տրի"> => mkN005 form1;
		<_ + "ր", _ + "ջրի"> => mkN005 form1;
		<_ + "ր", _ + "լրի"> => mkN005 form1;
		<_ + "ր", _ + "նրի"> => mkN013 form1;
		<_ + "ր", _ + "խրի"> => mkN013 form1;
		<_ + "ր", _ + "կրի"> => mkN013 form1;
		<_ + "ր", _ + "ցրի"> => mkN013 form1;
		<_ + "չ", _ + "պչի"> => mkN013 form1;
		<_ + "չ", _ + "կչի"> => mkN013 form1;
		<_ + "չ", _ + "րչի"> => mkN013 form1;
		<_ + "չ", _ + "ցչի"> => mkN013 form1;
		<_ + "չ", _ + "վչի"> => mkN013 form1;
		<_ + "ծ", _ + "գծի"> => mkN015 form1;
		<_ + "շ", _ + "փշի"> => mkN005 form1;
		<_ + "ի", _ + "իու"> => mkN027 form1;
		<_ + "ի", _ + "ձիի"> => mkN017 form1;
		<_ + "ւ", _ + "վի"> => mkN012 form1;
		<_ + "տ", _ + "ա"> => mkN016 form1;
		<_ + "ռ", _ + "ն"> => mkN026 form1;
		<_ + "ն", _ + "ջ"> => mkN034 form1;
		<_ + "կ", _ + "ա"> => mkN016 form1;
		<_ + "կ", _ + "ն"> => mkN023 form1;
		<_ + "ր", _ + "ջ"> => mkN043 form1;
		<_ + "ի", _ + "ի"> => mkN006 form1;
		_ => regN form1
  } ;

  regA : Str -> A   -- s;Nom;Sg
    = \form -> case form of {
		_ + "կիչ" => mkA006 form;
		_ + "ենգ" => mkA005 form;
		_ + "աղջ" => mkA005 form;
		_ + "սկի" => mkA003 form;
		_ + "աղի" => mkA003 form;
		_ + "ղց" => mkA005 form;
		_ + "ջն" => mkA005 form;
		_ + "եպ" => mkA005 form;
		_ + "նտ" => mkA005 form;
		_ + "ոդ" => mkA005 form;
		_ + "ղծ" => mkA005 form;
		_ + "յծ" => mkA005 form;
		_ + "եծ" => mkA005 form;
		_ + "ձր" => mkA005 form;
		_ + "նր" => mkA005 form;
		_ + "ծր" => mkA005 form;
		_ + "մր" => mkA005 form;
		_ + "սր" => mkA005 form;
		_ + "քր" => mkA005 form;
		_ + "ցր" => mkA005 form;
		_ + "եւ" => mkA001 form;
		_ + "ոկ" => mkA005 form;
		_ + "ղմ" => mkA005 form;
		_ + "տք" => mkA005 form;
		_ + "իղ" => mkA006 form;
		_ + "նչ" => mkA008 form;
		_ + "ոխ" => mkA005 form;
		_ + "ղխ" => mkA005 form;
		_ + "ղթ" => mkA005 form;
		_ + "ւթ" => mkA011 form;
		_ + "րշ" => mkA005 form;
		_ + "եշ" => mkA005 form;
		_ + "քշ" => mkA005 form;
		_ + "ոռ" => mkA005 form;
		_ + "ւգ" => mkA005 form;
		_ + "եգ" => mkA005 form;
		_ + "րճ" => mkA005 form;
		_ + "րզ" => mkA005 form;
		_ + "եզ" => mkA005 form;
		_ + "ւփ" => mkA005 form;
		_ + "նջ" => mkA005 form;
		_ + "աջ" => mkA005 form;
		_ + "մբ" => mkA001 form;
		_ + "ոյ" => mkA001 form;
		_ + "բի" => mkA003 form;
		_ + "սի" => mkA003 form;
		_ + "ց" => mkA001 form;
		_ + "ն" => mkA001 form;
		_ + "պ" => mkA001 form;
		_ + "տ" => mkA001 form;
		_ + "դ" => mkA001 form;
		_ + "վ" => mkA001 form;
		_ + "ծ" => mkA001 form;
		_ + "ձ" => mkA001 form;
		_ + "ր" => mkA001 form;
		_ + "լ" => mkA001 form;
		_ + "ւ" => mkA002 form;
		_ + "կ" => mkA001 form;
		_ + "մ" => mkA001 form;
		_ + "ք" => mkA001 form;
		_ + "ղ" => mkA001 form;
		_ + "չ" => mkA001 form;
		_ + "ժ" => mkA001 form;
		_ + "խ" => mkA001 form;
		_ + "թ" => mkA001 form;
		_ + "ս" => mkA001 form;
		_ + "շ" => mkA001 form;
		_ + "հ" => mkA001 form;
		_ + "ռ" => mkA001 form;
		_ + "գ" => mkA001 form;
		_ + "ճ" => mkA001 form;
		_ + "զ" => mkA001 form;
		_ + "փ" => mkA001 form;
		_ + "ջ" => mkA001 form;
		_ + "բ" => mkA005 form;
		_ + "յ" => mkA005 form;
		_ + "ե" => mkA002 form;
		_ + "ի" => mkA002 form;
		_ + "ա" => mkA004 form;
		_ + "ո" => mkA004 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2A : Str -> Str -> A   -- s;Nom;Sg  s;Nom;Pl
    = \form1, form2 -> case <form1, form2> of {
		<_ + "ոռ", _ + "ներ"> => mkA001 form1;
		<_ + "տ", _ + "տեր"> => mkA005 form1;
		<_ + "վ", _ + "վեր"> => mkA005 form1;
		<_ + "ծ", _ + "ծեր"> => mkA005 form1;
		<_ + "ձ", _ + "ձեր"> => mkA005 form1;
		<_ + "լ", _ + "լեր"> => mkA005 form1;
		<_ + "կ", _ + "կեր"> => mkA005 form1;
		<_ + "մ", _ + "մեր"> => mkA005 form1;
		<_ + "ք", _ + "քեր"> => mkA005 form1;
		<_ + "ղ", _ + "ղեր"> => mkA005 form1;
		<_ + "խ", _ + "խեր"> => mkA005 form1;
		<_ + "թ", _ + "թեր"> => mkA005 form1;
		<_ + "շ", _ + "շեր"> => mkA005 form1;
		<_ + "հ", _ + "հեր"> => mkA005 form1;
		<_ + "ռ", _ + "ռեր"> => mkA005 form1;
		<_ + "գ", _ + "գեր"> => mkA005 form1;
		<_ + "ճ", _ + "ճեր"> => mkA005 form1;
		<_ + "զ", _ + "զեր"> => mkA005 form1;
		<_ + "ջ", _ + "ջեր"> => mkA005 form1;
		_ => regA form1
  } ;

  mkV = overload {
    mkV : Str -> V = regV;   -- s
    mkV : Str -> Str -> V = reg2V   -- s  Imperative_Jussive;Pl
  } ;

  mkVV : V -> VV = \v -> v ;
  mkVS : V -> VS = \v -> v ;
  mkVQ : V -> VQ = \v -> v ;
  mkVA : V -> VA = \v -> v ;

  mkV2 = overload {
     mkV2 : V -> V2 = \v -> v ** {c2 = noPrep} ;
     mkV2 : V -> Prep -> V2 = \v,p -> v ** {c2 = p} ;
  } ;

  mkV3 = overload {
     mkV3 : V -> V3 = \v -> v ** {c2 = noPrep; c3 = noPrep} ;
     mkV3 : V -> Prep -> Prep -> V3 = \v,p1,p2 -> v ** {c2 = p1; c3 = p2} ;
  } ;

  mkV2A = overload {
     mkV2A : V -> V2A = \v -> v ** {c2 = noPrep; c3 = noPrep} ;
     mkV2A : V -> Prep -> Prep -> V2A = \v,p1,p2 -> v ** {c2 = p1; c3 = p2} ;
  } ;

  mkV2S = overload {
     mkV2S : V -> V2S = \v -> v ** {c2 = noPrep; c3 = noPrep} ;
     mkV2S : V -> Prep -> Prep -> V2S = \v,p1,p2 -> v ** {c2 = p1; c3 = p2} ;
  } ;

  mkV2Q = overload {
     mkV2Q : V -> V2Q = \v -> v ** {c2 = noPrep; c3 = noPrep} ;
     mkV2Q : V -> Prep -> Prep -> V2Q = \v,p1,p2 -> v ** {c2 = p1; c3 = p2} ;
  } ;

  mkV2V = overload {
     mkV2V : V -> V2V = \v -> v ** {c2 = noPrep; c3 = noPrep} ;
     mkV2V : V -> Prep -> Prep -> V2V = \v,p1,p2 -> v ** {c2 = p1; c3 = p2} ;
  } ;

  mkN = overload {
    mkN : Str -> N = regN;   -- s;Nom;Sg
    mkN : Str -> Str -> N = reg2N   -- s;Nom;Sg  s;Dat;Sg
  } ;

  mkN2 = overload {
     mkN2 : N -> N2 = \n -> n ** {c2 = noPrep} ;
     mkN2 : N -> Prep -> N2 = \n,p -> n ** {c2 = p} ;
  } ;

  mkN3 = overload {
     mkN3 : N -> N3 = \n -> n ** {c2 = noPrep; c3 = noPrep} ;
     mkN3 : N -> Prep -> Prep -> N3 = \n,p1,p2 -> n ** {c2 = p1; c3 = p2} ;
  } ;

  mkA = overload {
    mkA : Str -> A = regA;   -- s;Nom;Sg
    mkA : Str -> Str -> A = reg2A   -- s;Nom;Sg  s;Nom;Pl
  } ;

  mkA2 = overload {
     mkA2 : A -> A2 = \a -> a ** {c2 = noPrep} ;
     mkA2 : A -> Prep -> A2 = \a,p -> a ** {c2 = p} ;
  } ;

  mkAdv : Str -> Adv = \s -> lin Adv {s=s} ;
  mkAdV : Str -> AdV = \s -> lin AdV {s=s} ;
  mkAdA : Str -> AdA = \s -> lin AdA {s=s} ;
  mkAdN : Str -> AdN = \s -> lin AdN {s=s} ;
  mkCAdv : Str -> CAdv = \s -> lin CAdv {s=s; p=""} ;
  mkInterj : Str -> Interj = \s -> lin Interj {s=s} ;
  mkMU : Str -> MU = \s -> lin MU {s=s; isPre=False} ;

  mkPrep : Str -> Prep = \s -> lin Prep {s=s; c=Dat} ;

  mkIAdv : Str -> IAdv = \s -> lin IAdv {s=s} ;
  mkIP : Str -> IP = \s -> lin IP {s=s} ;
  mkIQuant : Str -> IQuant = \s -> lin IQuant {s=s} ;
  mkIDet : Str -> IDet = \s -> lin IDet {s=s} ;
  mkSubj : Str -> Subj = \s -> lin Subj {s=s} ;
  mkQuant : Str -> Quant = \s -> lin Quant {s=s} ;
  mkPredet : Str -> Predet = \s -> lin Predet {s=s} ;
  mkDet : Str -> Det = \s -> lin Det {s=s} ;
  mkCard : Str -> Card = \s -> lin Card {s=s} ;
  mkConj : Str -> Conj = \s -> lin Conj {s=s} ;
  mkPConj : Str -> PConj = \s -> lin PConj {s=s} ;
  mkVoc : Str -> Voc = \s -> lin Voc {s=s} ;

  mkLN : Str -> LN = \s -> lin LN {s=s} ;
  mkGN : Str -> GN = \s -> lin GN {s=s} ;
  mkSN : Str -> SN = \s -> lin SN {s=s} ;
  mkPN : Str -> PN = \s -> lin PN {s=s} ;

}
