--# -path=.:alltenses:gflibsrc/translator:gflibsrc/chunk
-- you need a symlink gflibsrc to your GF/lib/src

concrete UDTranslateLat of UDTranslate =
  TranslateLat,
  VerbLat [UseCopula],
  ExtensionsLat [GenNP,UseQuantPN,ComplVPIVV] **
 UDTranslateFunctor - [all_Det] with
  (Syntax = SyntaxLat),
  (Symbolic = SymbolicLat),
  (Extensions = ExtensionsLat)
  ** open ParadigmsLat, (M=MorphoLat) in {

-- lin
--   StringPN s = {s = \\_ => s.s ; g = human} ;
--   PrefixPN pn1 pn2 = {s = \\c => pn1.s ! c ++ pn2.s ! c ; g = pn2.g} ; --- Thomas+Mann

--   all_Det = M.mkDeterminer plural "all" ;


--   SlashVPS t p np vps = ExtensionsLat.PredVPS np (ExtensionsLat.MkVPS t p vps) ** {c2 = vps.c2} ;
--   AdvVPS vps adv = lin VPS {s = \\a => vps.s ! a ++ adv.s} ;

--   UttVPS vps = lin Utt {s = vps.s ! it_NP.a} ;
}

