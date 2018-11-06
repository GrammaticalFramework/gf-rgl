-- make a script for computing examples
-- usage: runghc MkExx.hs <koe.txt >koe.gfs
-- then:  gf -retain -s ../alltenses/TryRon.gfo <koe.gfs
-- called automatically by 'make exx'

main = interact (unlines . concatMap mkScript . takeWhile (/="--.") . lines)

mkScript l = case l of
  ' ':_ ->
     let ident = mkIdent $ unwords $ takeWhile (/="--") $ words l
     in [add $ psq ident]
  '-':_ -> []
  _ -> [
     add $ psq l,
     add $ "cc -one " ++ l,
     add $ psq "*"
     ]

add = ('\n':)

psq s = "ps \"" ++ s ++ "\""

-- makes mkUtt : QS -> Utt to mkUtt-QS-Utt
mkIdent :: String -> String
mkIdent = concatMap unspec where
  unspec c = case c of
    ' ' -> ""
    '>' -> ""
    '(' -> ""
    ')' -> ""
    ':' -> "-"
    _   -> [c]
