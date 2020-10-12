concrete NumeralKor of Numeral = CatKor [Numeral,Digits] **
  open Prelude, ResKor in {

lincat
  Digit = LinDigit ;
  Sub10,
  Sub100,
  Sub1000,
  Sub1000000 = ResKor.Numeral ;

lin
  -- : Sub1000000 -> Numeral ;              -- 123456 [coercion to top category]
  num x = x ;

  -- : Digit ;
  n2 = mkNum4 "이" "둘" "두" "스물" ** {isTwo=True} ;
  n3 = mkNum4 "삼" "셋" "세" "서른" ;
  n4 = mkNum4 "사" "넷" "네" "마흔" ;
  n5 = mkNum4 "오" "다섯" "다섯" "쉰" ;
  n6 = mkNum2 "육" "여섯" ;
  n7 = mkNum2 "칠" "일곱" ;
  n8 = mkNum2 "팔" "여덟" ;
  n9 = mkNum2 "구" "아홉" ;

  -- : Sub10 ;                              -- 1
  pot01 = mkNum5 "일" "하나" "한" "첫" "열" ;

  -- : Digit -> Sub10 ;                     -- d * 1
  pot0 x = x ;

  -- : Sub100 ;                             -- 10
  pot110 = mkNum2 "십" "열" ;

  -- : Sub100 ;                             -- 11
  pot111 = mkNum4 "십일" "열하나" "열한" "십첫" ; -- TODO check

  -- : Digit -> Sub100 ;                    -- 10 + d
  pot1to19 d = let newS = xPlus "십" "열" d.s in d ** {
    s = newS ;
    n = numNumber ;
    ord = newS ! NK ! Attrib ++ "번째" ;
    } ;

  -- : Sub10 -> Sub100 ;                    -- coercion of 1..9
  pot0as1 x = x ;

  -- : Digit -> Sub100 ;                    -- d * 10
  pot1 d = let sk = potTimes "십" "열" d in sk ** {
    s = table {
          SK => sk.s ! SK ;
          NK => case d.isTwo of {
                        True  => table {Attrib => "스무" ; Indep => "스물"} ;
                        False => table {_ => d.ten}
                      }
        }
    } ;

  -- : Digit -> Sub10 -> Sub100 ;           -- d * 10 + n
  pot1plus d n =
    let dx10 = pot1 d ;
        tenSK = glue (dx10.s ! SK ! Indep) "십" ;
        tenNK = d.ten ;
        newS = xPlus tenSK tenNK n.s
     in dx10 ** {
          s = newS ;
          ord = newS ! NK ! Attrib ++ "번째" ; -- TODO check
          } ;

  -- : Sub100 -> Sub1000 ;                  -- coercion of 1..99
  pot1as2 x = x ;

  -- : Sub10 -> Sub1000 ;                   -- m * 100
  pot2 = potTimes "백" "백" ;

  -- : Sub10 -> Sub100 -> Sub1000 ;         -- m * 100 + n
  pot2plus m n = TODO ;

  -- : Sub1000 -> Sub1000000 ;              -- coercion of 1..999
  pot2as3 x = x ;

  -- : Sub1000 -> Sub1000000 ;              -- m * 1000
  pot3 m = TODO ;

  -- : Sub1000 -> Sub1000 -> Sub1000000 ;   -- m * 1000 + n
  pot3plus m n = TODO ;

oper
  LinDigit : Type = ResKor.Numeral ** {isTwo : Bool ; ten : Str} ;

  mkNum2 : (sk, nk : Str) -> LinDigit =
    \sk,nk -> mkNum5 sk nk nk nk (sk+"십") ; -- Digits >5: no NK form for d*10

  mkNum4 : (x1,_,_,x4 : Str) -> LinDigit =
    \sk,nk,ord,ten -> mkNum5 sk nk ord ord ten ; -- Digits >4: NK attrib==indep

  mkNum5 : (x1,_,_,_,x5 : Str) -> LinDigit = \sk,nk,nkAttr,ord,ten -> {
    s = table {
          SK => \\_ => sk ;
          NK => table {Indep => nk ; _ => nkAttr }
          } ;
    n = numNumber ;
    numtype = IsNum ;
    isTwo = False ;
    ten = ten ;
    ord = ord ++ "번째" ;
  } ;

  NumForm : Type = NumOrigin => DForm => Str ;

  xTimes : (tenSK, tenNK : Str) -> NumForm -> NumForm = \sk,nk,tbl ->
    table {SK => \\df => glue (tbl ! SK ! df)    sk ;
           NK => \\df => glue (tbl ! NK ! Indep) nk } ;
  xPlus : (tenSK, tenNK : Str) -> NumForm -> NumForm = \sk,nk,tbl ->
    table {SK => \\df => glue sk (tbl ! SK ! df) ;
           NK => \\df => glue nk (tbl ! NK ! df) } ;

  potTimes : (sk,nk : Str) -> ResKor.Numeral -> ResKor.Numeral = \sk,nk,num ->
    let newS = xTimes sk nk num.s in num ** {
      s = newS ;
      n = numNumber ;
      ord = newS ! NK ! Attrib ++ "번째" ; -- TODO check
    } ;

  TODO : ResKor.Numeral = mkNum2 "TODO" "TODO" ;


-- numerals as sequences of digits

lincat
  Dig = TDigit ;

lin
  -- : Dig -> Digits ;       -- 8
  IDig d = d ;

  -- : Dig -> Digits -> Digits ; -- 876
  IIDig d i = {
    s = \\o => d.s ! NCard ++ BIND ++ i.s ! o ;
    n = numNumber
  } ;

  D_0 = mkDig "0" ;
  D_1 = mk3Dig "1" "1번째" ResKor.Sg ;
  D_2 = mkDig "2" ;
  D_3 = mkDig "3" ;
  D_4 = mkDig "4" ;
  D_5 = mkDig "5" ;
  D_6 = mkDig "6" ;
  D_7 = mkDig "7" ;
  D_8 = mkDig "8" ;
  D_9 = mkDig "9" ;

oper
  mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o numNumber ;
  mkDig : Str -> TDigit = \c -> mk2Dig c (c + "번째") ;

  mk3Dig : Str -> Str -> ResKor.Number -> TDigit = \c,o,n -> {
    s = table {NCard => c ; NOrd  => o} ;
    n = n
    } ;

  TDigit = {
    n : ResKor.Number ;
    s : CardOrd => Str
  } ;

  numNumber = Sg ; -- No need for 들 with numerals
}
