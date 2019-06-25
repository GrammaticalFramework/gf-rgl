concrete NumeralLat of Numeral = CatLat, ParamX[Number] ** open ParadigmsLat, Prelude, ResLat, Predef in {
  lincat 
    Digit      = TDigit ;
    Sub10      = TNumeral ;
    Sub100     = TNumeral ;
    Sub1000    = TNumeral ;
    Sub1000000 = TNumeral ;

  lin
    num x = x ;
    n2 = lin Digit ( mkDigit "duo"      "duodecim"      "viginti"      "ducenti"      "secundus" "triginta"     Yes ) ;
    n3 = lin Digit ( mkDigit "tres"     "tredecim"      "triginta"     "trecenti"     "tertius"  "quadraginta"  Yes ) ;
    n4 = lin Digit ( mkDigit "quattuor" "quattuordecim" "quadraginta"  "quadringenti" "quartus"  "quinquaginta" Yes ) ;
    n5 = lin Digit ( mkDigit "quinque"  "quindecim"     "quinquaginta" "quingenti"    "quintus"  "sexaginta"    Yes ) ;
    n6 = lin Digit ( mkDigit "sex"      "sedecim"       "sexaginta"    "sescenti"     "sextus"   "septuaginta"  Yes ) ;
    n7 = lin Digit ( mkDigit "septem"   "septendecim"   "septuaginta"  "septingenti"  "septimus" "octoginta"    Yes ) ;
    n8 = lin Digit ( mkDigit "octo"     "duodeviginti"  "octoginta"    "octingenti"   "ocatvus"  "nonaginta"    No8 ) ;
    n9 = lin Digit ( mkDigit "novem"    "undeviginti"   "nonaginta"    "nongenti"     "nonus"    "centum"       No9 ) ;

    -- 1
   pot01 = { s = n1.s ! one ; d = n1.s ; n = singular ; below8 = n1.below8 }  ;
   -- d * 1
   pot0 d = {
     s = d.s ! one ;
     d = table {
       thousand => \\g,c => d.s ! one ! g ! c ++ d.s ! thousand ! g ! c ;
       u => \\g,c => d.s ! u ! g ! c
       } ;
     n = plural ;
     below8 = d.below8
     } ;
     -- 10
   pot110 = {
     s = n1.s ! ten ;
     d = table {
       thousand => \\g,c => n1.s ! ten ! g ! c ++ n1.s ! thousand ! g ! c ;
       u => \\g,c => n1.s ! u ! g ! c
       } ;
     n = singular ;
     below8 = Yes
     }  ;
     -- 11
   pot111 = pot1to19 n1 ;
   -- 10 + d
   pot1to19 d = {
     s = d.s ! eleven ;
     d = table {
       thousand => \\g,c => d.s ! eleven ! g ! c ++ n1.s ! thousand ! g ! c ;
       u => \\g,c => d.s ! u ! g ! c
       } ;
     n = plural ;
     below8 = Ign
     } ;
   -- coercion of 1..9
   pot0as1 n = n ;
   -- d * 10
   pot1 d = {
     s = d.s ! ten ;
     d = table {
       thousand => \\g,c => d.s ! ten ! g ! c ++ n1.s ! thousand ! g ! c ;
       u => \\g,c => d.s ! u ! g ! c
       } ;
     n = plural ;
     below8 = Yes
     } ;
   -- d * 10 + n
   pot1plus d n =
     let
       newS : Gender => Case => Str = \\g,c => case n.below8 of {
	 No8 => "duo" ++ Prelude.BIND ++ "-" ++ Prelude.BIND ++ "de" ++ Prelude.BIND ++ "-" ++ Prelude.BIND ++ d.tenNext ;
	 No9 => "un"  ++ Prelude.BIND ++ "-" ++ Prelude.BIND ++ "de" ++ Prelude.BIND ++ "-" ++ Prelude.BIND ++ d.tenNext ;
	 _ => d.s ! ten ! g ! c ++ n.s ! g ! c 
	 } in
     {
     s = newS ;
     d = table {
       thousand => \\g,c => newS ! g ! c ++ n1.s ! thousand ! g ! c ;
       u => \\g,c => n.d ! u ! g ! c
       } ;
     below8 = Ign ;
     n = plural 
     } ;
   -- coercion of 1..99
   pot1as2 n = n ;
   -- m * 100
   pot2 n = {
     s = n.d ! hundred ;
     d = table {
       thousand => \\g,c => n.d ! hundred ! g ! c ++ n1.s ! thousand ! g ! c ;
       u => \\g,c => n.d ! u ! g ! c
       } ;
     n = plural ;
     below8 = Yes} ;
   -- d * 100 + n
   pot2plus d n =
     let
       newS : Gender => Case => Str = \\g,c => d.d ! hundred ! g ! c ++ "et" ++ n.s ! g ! c 
     in
     {
       s = newS ;
	 d = table {
	   thousand => \\g,c => newS ! g ! c ++ n1.s ! thousand ! g ! c ;
	   u => \\g,c => n.d ! u ! g ! c
	 } ;
       below8 = Ign ;
       n = plural 
     } ;
   -- coercion of 1..999
   pot2as3 n = n ;
     -- m * 1000
   pot3 n = {
     s = \\g,c => n.s ! g ! c ++ n.d ! thousand ! g ! c ;
     d = table { thousand => \\g,c => n.s ! g ! c ++ n.d ! thousand ! g ! c ;
		 u => \\g,c => n.d ! u ! g ! c
       } ;
     below8 = Ign ;
     n = plural
     } ;

   -- d * 1000 + n
   pot3plus d n = {
     s = \\g,c => d.d ! thousand ! g ! c ++ "et" ++ n.s ! g ! c ;
     d = n.d ;
     below8 = Ign ;
     n = plural 
     } ;

  oper
    mkDigit : (ones, eleven, tens, hundreds, ord : Str) -> Str -> Below8 -> TDigit =
      \ones, eleven, tens, hundreds, ord ->
      case <tens,hundreds> of {
      	<"decem",_> => fullDigit ones eleven tens hundreds ord "decimus" "centesimus" ;
      	<"viginti",_> => fullDigit ones eleven tens hundreds ord "vicesimus" "ducentesimus" ;
      	<"triginta",_> => fullDigit ones eleven tens hundreds ord "tricesimus" "trecentesimus" ;
      	<quadra + "ginta",quadringent + "i"> => fullDigit ones eleven tens hundreds ord (quadra + "gesimus") (quadringent + "esimus") ;
	_ => Predef.error "Invalid number"
      } ;
    fullDigit : (ones, eleven, tens, hundreds, ord1,ord10,ord100, tenNext : Str) -> Below8 -> TDigit =
      \ones, eleven, tens, hundreds, ord1,ord10,ord100,tenNext,b8 ->
      { s = table { one => cardFlex ones ;
		    eleven => cardFlex eleven ; 
		    ten => cardFlex tens ;
		    hundred => cardFlex hundreds ;
		    thousand => cardFlex "milia" ;
		    ten_thousand => \\_,_ => nonExist ;
		    hundred_thousand => \\_,_ => nonExist
	  } ;
--    	n = case ones of { "unus" => singular ; _ => plural } ;
	--   ord =
	--     \\_,_ => [] ;
	-- --     table { one => (mkA ord1).s ! Posit;
	-- -- 	  ten => (mkA ord10).s ! Posit ;
	-- -- 	  hundred => (mkA ord100).s ! Posit ;
	-- -- 	  thousand => \\_,_ => nonExist ;
	-- -- 	  ten_thousand => \\_ => nonExist ;
	-- -- 	  hundred_thousand => \\_ => nonExist } ;
	tenNext = tenNext ;
	below8 = b8 
      } ;    
    n1 : Digit = lin Digit ( fullDigit "unus" "decem" "undecim" "primus" "decimus" "centesimus" "millesimus" "viginti" Yes ) ;

-- numerals as sequences of digits

  lincat 
    Dig = TDig ;

  lin
    IDig d = {s = d.s ! one; unit = ten} ;

    IIDig d i = {
      s = d.s ! i.unit ++ i.s ;
      unit = inc i.unit
    } ;

    D_0 = mkDig ""     ""      ""     ""     ""       ""       "" ;
    D_1 = mkDig "I"    "XI"    "X"    "C"    "M"      "(X)"    "(C)" ;
    D_2 = mkDig "II"   "XII"   "XX"   "CC"   "MM"     "(XX)"   "(CC)" ;
    D_3 = mkDig "III"  "XIII"  "XXX"  "CCC"  "MMM"    "(XXX)"  "(CCC)" ;
    D_4 = mkDig "IV"   "XIV"   "XL"   "CD"   "(IV)"   "(XL)"   "(CD)" ;
    D_5 = mkDig "V"    "XV"    "L"    "D"    "(V)"    "(L)"    "(D)" ;
    D_6 = mkDig "VI"   "XVI"   "LX"   "DC"   "(VI)"   "(LX)"   "(DC)" ;
    D_7 = mkDig "VII"  "XVII"  "LXX"  "DCC"  "(VII)"  "(LXX)"  "(DCC)" ;
    D_8 = mkDig "VIII" "XVIII" "LXXX" "DCCC" "(VIII)" "(LXXX)" "(DCCC)" ;
    D_9 = mkDig "IX"   "XIX"   "XC"   "CM"    "(IX)"   "(XC)"   "(CM)" ;

  oper
    TDig = {
      s : Unit => Str
    } ;

    mkDig : Str -> Str -> Str -> Str -> Str -> Str -> Str -> TDig = 
      \one,eleven,ten,hundred,thousand,ten_thousand,hundred_thousand -> {
          s = table Unit [one;eleven;ten;hundred;thousand;ten_thousand;hundred_thousand]
        } ;
        
    inc : Unit -> Unit = \u ->
      case u of {
        one              => ten ;
	eleven           => hundred ;
        ten              => hundred ;
        hundred          => thousand ;
        thousand         => ten_thousand ;
        ten_thousand     => hundred_thousand ;
        hundred_thousand => hundred_thousand
      } ;
}
