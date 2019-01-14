let $d := db:open('Latin-Treebank')
for $s in $d//sentence//word[starts-with(@form,'qui')]/..
let $ss := (
  let $w := $s/word
  (: for $w in $s/word:)
  let $f := ($w//data(@form))
  let $l := ($w/data(@lemma))
  let $r := ($w/data(@relation))
  return <s lemma="{$l}" relation="{$r}" > {$f} </s>
)
return <t> {$ss}</t>

