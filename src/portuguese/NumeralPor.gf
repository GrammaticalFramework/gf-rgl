concrete NumeralPor of Numeral = CatPor [Numeral,Digits] **
  open CommonRomance, ResRomance, MorphoPor, Prelude, Predef in {

  flags coding=utf8 ;

  param
    DForm = unit | teen | ten | hundred ;

  lincat
  --- cardinals are generally not inflected by gender, however 1 and 2
  --- are, as are the hundreds from 2 to 9
    Digit = {s : DForm => CardOrd => Str} ;
    Sub10 = {s : DForm => CardOrd => Str ; n : Number} ;
    Sub100 = {s : CardOrd => Str ; n : Number} ;
    Sub1000 = {s : CardOrd => Str ; n : Number} ;
    Sub1000000 = {s : CardOrd => Str ; n : Number} ;

  lin
    num x = x ;

    -- digits
    n2 = let dois = mkTal "dois" "doze" "vinte" "duzentos"
               "segundo" "vigésimo" "duocentésimo"
      in {s =\\f,g => case <f, g> of {
            <unit, NCard Fem> => "duas" ;
            _ => dois.s ! f ! g
            }
      } ;
    n3 =
      mkTal "três" "treze" "trinta" "trezentos"
      "terceiro" "trigésimo" "tricentésimo" ;
    n4 =
      mkTal "quatro" ("catorze" | "quatorze") "quarenta"
      "quatrocentos" "quarto" "quadragésimo" "quadringentésimo" ;
    n5 =
      mkTal "cinco" "quinze" "cinquenta" "quinhentos"
      "quinto" "quinquagésimo" "guingentésimo" ;
    n6 =
      mkTal "seis" ("dezesseis" | "dezasseis") "sessenta" "seiscentos"
      "sexto" "sexagésimo" "sexcentésimo" ;
    n7 =
      mkTal "sete" ("dezessete" | "dezassete") "setenta"
      "setecentos" "sétimo" "septuagésimo" "septingentésimo" ;
    n8 =
      mkTal "oito" "dezoito"  "oitenta"   "oitocentos"
        "oitavo" "octogésimo" "octingentésimo" ;
    n9 =
      mkTal "nove" ("dezenove" | "dezanove") "noventa"
      "novecentos" "nono" "nonagésimo" "noningentésimo";

    pot01 =
      let um = (mkTal "um" "onze" "dez" "centos" "primeiro"
                  "décimo" "centésimo").s in
      {s =\\f,g => case <f,g> of {
         <unit, NCard Fem> => "uma" ;
         <hundred, NCard _> => "cento" ;
         _ => um ! f ! g
         } ;
       n = Sg
      } ;

    pot0 d = {s = d.s ; n = Pl} ;

    pot110 = spl (pot01.s ! ten) ;

    pot111 = spl (pot01.s ! teen) ;

    pot1to19 d = spl (d.s ! teen) ;

    pot0as1 n = {s = n.s ! unit ; n = n.n} ;

    pot1 d = spl (d.s ! ten) ;

    pot1plus d e =
      {s = \\g => d.s ! ten ! g
         ++ e_CardOrd g ++ e.s ! unit ! g ;
       n = Pl} ;

    pot1as2 n = n ;

    pot2 d =
      let n = case d.n of {
            Sg => mkNumStr "cem" "centésimo" ;
            _ => d.s ! hundred
            }
      in spl n ;

    pot2plus d e =
      {s = \\g => d.s ! hundred ! g
         ++ e_CardOrd g ++ e.s ! g ;
       n = Pl} ;

    pot2as3 n = n ;

    pot3 n =
      let n = case n.n of {
            Sg => [] ;
            _ => n.s ! NCard Masc
            } ;
      in spl (\\co => n ++ mil ! co) ;

    pot3plus n m =
      let n = case n.n of {
            Sg => [] ;
            _ => n.s ! NCard Masc
            } ;
      in {s = \\co => n ++ mil ! co
            -- actually, 'e' only if m is exact hundred (pot2) or
            -- lower
            ++ e_CardOrd co
            ++ m.s ! co ;
          n = Pl} ;

  oper
    mkTal : (_,_,_,_,_,_,_ : Str) -> {s : DForm => CardOrd => Str} =
      \dois,doze,vinte,duzentos,segundo,vigesimo,duocentesimo ->
      {s = \\d,co => case <d,co> of {
         <unit, NCard _>     => dois ;
         <teen, NCard _>     => doze ;
         <ten, NCard _>      => vinte ;
         <hundred, NCard g>  => regCard (tk 1 duzentos) g Pl ;
         <unit, NOrd g n>    => regCard segundo g n ;
         <teen, NOrd g n>    => (regCard "décimo") g n ++ (regCard segundo) g n ;
         <ten, NOrd g n>     => regCard vigesimo g n ;
         <hundred, NOrd g n> => regCard duocentesimo g n
         }
      } ;

    regCard : Str -> Gender -> Number -> Str ;
    regCard vigesimo = case vigesimo of {
      -- to handle milhão case (in ParseExtend module)
      milh + "ão" => \g, n -> genNumForms vigesimo vigesimo (milh + "ões") vigesimo ! g ! n;
      _ => pronForms (mkAdjReg vigesimo)
      } ;

    spl : (CardOrd => Str) -> {s : CardOrd => Str ; n : Number} = \s -> {
      s = s ;
      n = Pl
      } ;

    mkNumStr : Str -> Str -> CardOrd => Str ;
    mkNumStr cem centesimo = \\co =>
      case co of {
        NCard _ => cem ;
        NOrd g n => regCard centesimo g n
      } ;

    mil : CardOrd => Str ;
    mil = mkNumStr "mil" "milésimo" ;

    e_CardOrd : CardOrd -> Str = \co -> case co of {
      NCard _ => "e" ;
      _ => []
      } ;

---
-- numerals as sequences of digits

  lincat
    Dig = TDigit ;

  lin
    IDig d = d ;

    IIDig d i = {
      s = \\o => d.s ! NCard Masc ++ BIND ++ i.s ! o ;
      n = Pl
      } ;

    D_0 = mkDig "0" Sg ;
    D_1 = mkDig "1" Sg ;
    D_2 = mkDig "2" ;
    D_3 = mkDig "3" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

  oper
    mk4Dig : Str -> Str -> Str -> Number -> TDigit = \c,o,a,n -> {
      s = table {
        NCard _ => c ;
        NOrd Masc _ => o ;
        NOrd Fem _ => a
        } ;
      n = n
      } ;

    mk3Dig : Str -> Str -> Str -> TDigit =
      \c,mo,fo -> mk4Dig c mo fo Pl ;

    mk2Dig : Str -> Number -> TDigit = \c,n -> mk1Dig c ** {n = n} ;

    mk1Dig : Str -> TDigit = \c -> mk3Dig c (c + "º") (c + "ª") ;

    mkDig = overload {
      mkDig : Str -> TDigit = mk1Dig ;
      mkDig : Str -> Number -> TDigit = mk2Dig ;
      } ;

    TDigit = {
      n : Number ;
      s : CardOrd => Str
    } ;

}
