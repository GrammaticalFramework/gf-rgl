concrete NamesMkd of Names = CatMkd ** open Prelude,ResMkd in {
  lin AdjLN ap ln = {s = ap.s ! Indef ! GSg Masc ++ ln.s} ;
  lin FemaleSurname sn = {s = \\r => sn.s; vocative = sn.s;
                          a = {g = GSg Masc; p = P1}} ;
  lin FullName gn sn = {s = \\r => gn.s ++ sn.s;
                        vocative = gn.s ++ sn.s; a = {g = GSg Masc; p = P1}} ;
  lin GivenName gn = {s = \\r => gn.s; vocative = gn.s;
                      a = {g = GSg Masc; p = P1}} ;
  lin InLN ln = {s = ln.s} ;
  lin MaleSurname sn = {s = \\r => sn.s; vocative = sn.s;
                        a = {g = GSg Masc; p = P1}} ;
  lin PlSurname sn = {s = \\r => sn.s; vocative = sn.s;
                      a = {g = GSg Masc; p = P1}} ;
  lin PlainLN ln = {s = \\r => ln.s; vocative = ln.s;
                    a = {g = GSg Masc; p = P1}} ;
  lin UseLN ln = {s = \\r => ln.s; vocative = ln.s;
                  a = {g = GSg Masc; p = P1}} ;
}
