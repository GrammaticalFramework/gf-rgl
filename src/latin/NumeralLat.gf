concrete NumeralLat of Numeral = CatLat ** open ResLat,ParadigmsLat,Prelude in {
  lincat 
    Digit      = TDigit ;
    Sub10      = Numeral ;
    Sub100     = Numeral ;
    Sub1000    = Numeral ;
    Sub1000000 = Numeral ;

  lin
    num x = x ;
    -- n2 = lin Digit ( mkDigit "duo" "viginti" "ducenti" "secundus" ) ;
    -- n3 = lin Numeral ( mkNum "tres" "triginta" "trecenti" "tertius" ) ;
    -- n4 = lin Numeral ( mkNum "quattuor" "quadraginta" "quadringenti" "quartus" ) ;
    -- n5 = lin Numeral ( mkNum "quinque" "quinquaginta" "quingenti"  "quintus" ) ;
    -- n6 = lin Numeral ( mkNum "sex" "sexaginta" "sescenti" "sextus" ) ;
    -- n7 = lin Numeral ( mkNum "septem" "septuaginta" "septingenti" "septimus" ) ;
    -- n8 = lin Numeral ( mkNum "octo" "octoginta" "octingenti" "ocatvus" ) ;
    -- n9 = lin Numeral ( mkNum "novem" "nonaginta" "nongenti" "nonus") ;

--     -- 1
--    pot01 = 
--     -- d * 1
--     pot0 d = d ;
--     -- 10
--     pot110 = pot01 ; 
--     -- 11
-- --    pot111 = pot1to19 pot01 ;
--     -- 10 + d
-- --    pot1to19 d = mkNum "" "" "" "" ; -- {s = d.s ! teen} ** {n = Pl} ;
--     -- coercion of 1..9
--     pot0as1 n = n ;
--     -- d * 10
--     --lin pot1 d = {s = d.s ! ten} ** {n = Pl} ;
--     -- d * 10 + n
--     --lin pot1plus d e = {
--     --   s = \\c => d.s ! ten ! NCard ++ "-" ++ e.s ! unit ! c ; n = Pl} ;
--     -- coercion of 1..99
--     pot1as2 n = n ;
--     -- m * 100
--     --lin pot2 d = {s = \\c => d.s ! unit ! NCard ++ mkCard c "hundred"}  ** {n = Pl} ;
--     -- m * 100 + n
--     --lin pot2plus d e = {
--     --  s = \\c => d.s ! unit ! NCard ++ "hundred" ++ "and" ++ e.s ! c ; n = Pl} ;
--     -- coercion of 1..999
--     pot2as3 n = n ;
--     -- m * 1000
--     --lin pot3 n = {
--     --  s = \\c => n.s ! NCard ++ mkCard c "thousand" ; n = Pl} ;
--     -- m * 1000 + n
--     --lin pot3plus n m = {
--     --  s = \\c => n.s ! NCard ++ "thousand" ++ m.s ! c ; n = Pl} ;

  oper
    n1 : Digit = lin Digit ( fullDigit "unus" "decem" "primus" "decimus" "centesimus" "millesimus" ) ;

    mkDigit : (ones, tens, hundreds, ord : Str) -> TDigit =
      \ones, tens, hundreds, ord ->
      case <tens,hundreds> of {
      	-- <"decem",_> => fullDigit ones tens hundreds ord "decimus" "centesimus" ;
      	-- <"viginta",_> => fullDigit ones tens hundreds ord "vicesimus" "ducentesimus" ;
      	-- <"triginta",_> => fullDigit ones tens hundreds ord "tricesimus" "trecentesimus" ;
      	<quadra + "ginta",quadringent + "i"> => fullDigit ones tens hundreds ord (quadra + "gesimus") (quadringent + "esimus")
      } ;
    fullDigit : (ones, tens, hundreds, ord1,ord10,ord100 : Str) -> TDigit =
      \ones, tens, hundreds, ord1,ord10,ord100 ->
      { s = table { one => cardFlex ones ;
		    ten => cardFlex tens ;
		    hundred => cardFlex hundreds ;
		    thousand => \\_,_ => nonExist ;
		    ten_thousand => \\_,_ => nonExist ;
		    hundred_thousand => \\_,_ => nonExist }
--    	n = case ones of { "unus" => Sg ; _ => Pl } ;
	--   ord =
	--     \\_,_ => [] ;
	-- --     table { one => (mkA ord1).s ! Posit;
	-- -- 	  ten => (mkA ord10).s ! Posit ;
	-- -- 	  hundred => (mkA ord100).s ! Posit ;
	-- -- 	  thousand => \\_,_ => nonExist ;
	-- -- 	  ten_thousand => \\_ => nonExist ;
	-- -- 	  hundred_thousand => \\_ => nonExist } ;
	} ;
-- numerals as sequences of digits

  lincat 
    Dig = TDig ;

  lin
    IDig d = {s = d.s ! one; unit = ten} ;

    IIDig d i = {
      s = d.s ! i.unit ++ i.s ;
      unit = inc i.unit
    } ;

    D_0 = mkDig ""     ""     ""     ""       ""       "" ;
    D_1 = mkDig "I"    "X"    "C"    "M"      "(X)"    "(C)" ;
    D_2 = mkDig "II"   "XX"   "CC"   "MM"     "(XX)"   "(CC)" ;
    D_3 = mkDig "III"  "XXX"  "CCC"  "MMM"    "(XXX)"  "(CCC)" ;
    D_4 = mkDig "IV"   "XL"   "CD"   "(IV)"   "(XL)"   "(CD)" ;
    D_5 = mkDig "V"    "L"    "D"    "(V)"    "(L)"    "(D)" ;
    D_6 = mkDig "VI"   "LX"   "DC"   "(VI)"   "(LX)"   "(DC)" ;
    D_7 = mkDig "VII"  "LXX"  "DCC"  "(VII)"  "(LXX)"  "(DCC)" ;
    D_8 = mkDig "VIII" "LXXX" "DCCC" "(VIII)" "(LXXX)" "(DCCC)" ;
    D_9 = mkDig "IX"   "XC"   "CM"   "(IX)"   "(XC)"   "(CM)" ;

  oper
    TDig = {
      s : Unit => Str
    } ;

    mkDig : Str -> Str -> Str -> Str -> Str -> Str -> TDig = 
      \one,ten,hundred,thousand,ten_thousand,hundred_thousand -> {
          s = table Unit [one;ten;hundred;thousand;ten_thousand;hundred_thousand]
        } ;
        
    inc : Unit -> Unit = \u ->
      case u of {
        one              => ten ;
        ten              => hundred ;
        hundred          => thousand ;
        thousand         => ten_thousand ;
        ten_thousand     => hundred_thousand ;
        hundred_thousand => hundred_thousand
      } ;
}
