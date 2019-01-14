use strict;
use warnings;
use Switch;
use File::Basename;
use Data::Dumper;

our %LEX;
our %TEST;

sub add_it
{
    my ($POSTAG,$FORM,$LEMMA,$FEATS) = @_;
    if(!defined($LEX{$LEMMA}{$POSTAG}{$FEATS}{$FORM}))
    {
	$LEX{$LEMMA}{$POSTAG}{$FEATS}{$FORM} = 1;
    }
    else
    {
	$LEX{$LEMMA}{$POSTAG}{$FEATS}{$FORM}++;
    }
}
sub to_hash
{
    my ($POSTAG,$FORM,$LEMMA,$FEATS) = @_;
    if ($LEMMA eq "greekexpression" || $LEMMA eq "calendarexpression" || $LEMMA eq "monetaryexpression")
    {
	return;
    }
    $LEMMA =~ s/#(\d+)$//g; # remove all strange #\d+ after the lemma
    if ($POSTAG ne "Ne") # Only proper names stay upper case
    {
	$LEMMA = lc $LEMMA;
	$FORM = lc $FORM;
    }
    if ($LEMMA =~ /(?<pre>\w+)\((?<mid>\w+)\)(?<post>\w+)/) # split cases with parenthesis in a lemma into multiple cases
    {
	add_it($POSTAG,$FORM,$+{pre}.$+{post},$FEATS);
	add_it($POSTAG,$FORM,$+{pre}.$+{mid}.$+{post},$FEATS);
    }
    else
    {
	add_it($POSTAG,$FORM,$LEMMA,$FEATS);
    }
}

sub to_gf_lex
{
    my ($abslexfh,$conclexfh) = @_;
    my $count = 0;
    foreach my $LEMMA (keys %LEX)
    {
	foreach my $POSTAG (keys %{$LEX{$LEMMA}})
	{
	    my @FEATS = keys %{$LEX{$LEMMA}{$POSTAG}};
	    my @FORMS;
	    foreach my $FEAT (keys %{$LEX{$LEMMA}{$POSTAG}})
	    {
		foreach my $FORM (keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}})
		{
		    push @FORMS, $FORM;
		}
	    }
	    switch ($POSTAG) {
		case "A-" { adjective($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # adjective
		case "Df" { adverb($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # adverb
		#	    case "S-" {} # article
		#	    case "Ma" {} # cardinal numeral
		case "Nb" { noun($POSTAG,$LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # common noun
		#	    case "C-" {} # conjunction
		case "Pd" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # demonstrative pronoun
		#	    case "F-" {} # foreign word
		case "Px" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # indefinite pronoun
		#	    case "N-" {} # infinitive marker			#	    case "I-" {} # interjection
		case "Du" { adverb($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # interrogative adverb
		case "Pi" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # interrogative pronoun
		#	    case "Mo" {} # ordinal numeral
		case "Pp" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # personal pronoun
		case "Pk" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # personal reflexive pronoun
		case "Ps" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # possessive pronoun
		case "Pt" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # possessive reflexive pronoun
		case "R-" { preposition($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # preposition
		case "Ne" { noun($POSTAG,$LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # proper noun
		#	    case "Py" {} # quantifier
		#	    case "Pc" {} # reciprocal pronoun
		case "Dq" { adverb($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # relative adverb
		case "Pr" { pronoun($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # relative pronoun
		case "G-" { adverb($LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # subjunction
		case "V-" { verb($POSTAG,$LEMMA,\@FEATS,\@FORMS,$abslexfh,$conclexfh,\$count) } # verb
		#	    case "X-" {} # unassigned
	    }
	}
    }
}

sub adjective    
{
    my ($LEMMA,$FEATS,$FORMS,$abslexfh,$conclexfh,$count) = @_ ;
    if ($LEMMA =~ /(us|er)$/) {  print $conclexfh "\tlex".$$count."_A = mkA \"$LEMMA\" ;\n";  }
    elsif ($LEMMA =~ /(?<stem>.*)is$/) { print $conclexfh  "\tlex".$$count."_A = mkA \"$LEMMA\" \"".$+{stem}."e\" ;\n" ; }
    elsif ($LEMMA =~ /(?<stem>.*)ns$/) { print $conclexfh "\tlex".$$count."_A = mkA \"$LEMMA\" \"".$+{stem}."ntis\" ;\n" ; }
#    elsif ($LEMMA =~ /(?<stem>.*[^e])r$/) { print $conclexfh "\tlex".$$count."_A = mkA \"$LEMMA\" \"".$+{stem}."ris\" ;\n" ; } # prior, memor, interior, deterior, par, victor, ulterior, immemor broken. to fix 
#    elsif ($LEMMA =~ /(?<stem>.*)rs$/) { print $conclexfh "\tlex".$$count."_A = mkA \"$LEMMA\" \"".$+{stem}."rtis\" ;\n" ; } # iners, expers, misericors broken. to fix
    elsif ($LEMMA =~ /(?<stem>.*)x$/) { print $conclexfh "\tlex".$$count."_A = mkA \"$LEMMA\" \"".$+{stem}."cis\" ;\n"; }
    else { print "missing case for Adjective $LEMMA - ".join(", ",@$FORMS)."\n"; return }
    print $abslexfh "\tlex".$$count."_A : A ; -- $LEMMA\n";
    foreach my $FORM (@$FORMS) { $TEST{$FORM} = 1 };
    $$count++;
}

sub noun
{
    my ($POSTAG,$LEMMA,$FEATS,$FORMS,$abslexfh,$conclexfh,$count) = @_;
    # try to generate lexicon entry of form nominativ singular, genetive singular, gender
    my $nomsg = undef ;
    my $gensg = undef ;
    my $gender = undef ;
    foreach my $FEAT (@$FEATS)
    {
	if ($FEAT =~ /GEND(\w)/)
	{
	    switch ($1)
	    {
		case "m" { $gender = "masculine" }
		case "f" { $gender = "feminine" }
		case "n" { $gender = "neuter" }
		case "p" { $gender = "( masculine|feminine )" }
		case "o" { $gender = "( masculine|neuter )" }
		case "r" { $gender = "( feminine|neuter )" }
		case "q" { $gender = "( masculine|feminine|neuter )" }
		case "x" { $gender = "nonExist"} # uncertain gender
		else { print "Unhandled gender $1\n" }
	    }
	}
    	if ($FEAT =~ /NUMBs\|.*\|CASEn/)
    	{
     	    $nomsg = (keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}})[0] if (scalar(keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}}) == 1);
    	}
	elsif ($FEAT =~ /NUMBs\|.*\|CASEg/)
    	{
     	    $gensg = (keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}})[0] if (scalar(keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}}) == 1);
    	}
    }

    if (defined($nomsg) && defined($gensg) && defined($gender))
    {
	print $conclexfh "\tlex".$$count."_N = mkN \"$nomsg\" \"$gensg\" $gender ;\n";
    }
    elsif ($LEMMA =~ /(us|is)$/) {
	if (scalar(grep(/$LEMMA/,@$FORMS)) !=0) # there is at least one more time the same form as the lemma in the paradigm
	{
	    	print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" \"$LEMMA\" $gender ;\n" if (defined($gender));
	}
	elsif ($LEMMA =~ /(us)$/) # then it is quite likely second declension
	{
	    	print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" ;\n";  
	}
	else
	{
	    print "missing case for Noun $LEMMA - ".join(", ",@$FORMS)."\n"; 
	    return;
	}
	    
    }
    elsif ($LEMMA =~ /(a|um|er|ir|u)$/) 
    {  
	print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" ;\n";  
    }
    elsif ($LEMMA =~ /(or)$/ && defined($gender))
    {  
	print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" \"".$LEMMA."is\" $gender;\n";  
    }
    elsif ($LEMMA =~ /(o)$/ && defined($gender))
    {  
	print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" \"".$LEMMA."nis\" $gender;\n";  
    }
    elsif ($LEMMA =~ /(?<stem>\w+)(ns)$/ && defined($gender))
    {  
	print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" \"".$+{stem}."ntis\" $gender;\n";  
    }
    elsif ($LEMMA =~ /(?<stem>\w+)(x)$/ && defined($gender))
    {
	if (grep(/$+{stem}g.*/,@$FORMS))
	{
	    print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" \"".$+{stem}."gis\" $gender;\n";
	}
	elsif (grep(/$+{stem}c.*/,@$FORMS))
	{
	    print $conclexfh "\tlex".$$count."_N = mkN \"$LEMMA\" \"".$+{stem}."cis\" $gender;\n";
	}
	else
	{
	    print "missing case for Noun $LEMMA - ".join(", ",@$FORMS)."\n"; 
	    return;
	}
    }
    else { 
	print "missing case for Noun $LEMMA - ".join(", ",@$FORMS)."\n"; 
	return; 
    }
    print $abslexfh "\tlex".$$count."_N : N ; -- $LEMMA\n";
    foreach my $FORM (@$FORMS) { $TEST{$FORM} = 1 };
    $$count++;
}

sub verb
{
    my ($POSTAG,$LEMMA,$FEATS,$FORMS,$abslexfh,$conclexfh,$count) = @_;
    my $infpresact = undef;
    my $infperfact = undef;
    my $partperfpass = undef;
    foreach my $FEAT (@$FEATS)
    {
	if ($FEAT =~ /TENSp\|MOODn\|VOICa/)
	{
	    $infpresact = (keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}})[0] if (scalar(keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}}) == 1)
	}
	elsif ($FEAT =~ /PERS1\|NUMBs\|TENSr\|MOODi\|VOICa/)
	{
	    $infperfact = (keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}})[0] if (scalar(keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}}) == 1)
	}
	elsif ($FEAT =~ /TENSr\|MOODp\|VOICp/)
	{
	    $partperfpass = (keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}})[0] if (scalar(keys %{$LEX{$LEMMA}{$POSTAG}{$FEAT}}) == 1)
	}
    }
    if (defined($infpresact) && defined($infperfact) && defined($partperfpass)) 
    {
	print $conclexfh "\tlex".$$count."_V = mkV \"$infpresact\" \"$infperfact\" \"$partperfpass\" ;\n";
    }
    else { 
	print "missing case for Verb $LEMMA - ".join(", ",@$FORMS)."\n"; 
	return; 
    }
    print $abslexfh "\tlex".$$count."_V : V ; -- $LEMMA\n";
    foreach my $FORM (@$FORMS) { $TEST{$FORM} = 1 };
    $$count++;
}

sub pronoun 
{
    my ($LEMMA,$FEATS,$FORMS,$abslexfh,$conclexfh,$count) = @_;
    if (0) {}
    else { 
	print "missing case for Pronoun $LEMMA - ".join(", ",@$FORMS)."\n"; 
	return; 
    }

}

sub preposition 
{
    my ($LEMMA,$FEATS,$FORMS,$abslexfh,$conclexfh,$count) = @_;
    if (grep(/$LEMMA/,@$FORMS) == @$FORMS) {
	print $conclexfh "\tlex".$$count."_Prep = mkPrep \"$LEMMA\" Acc ;\n";
    }
    else {
	print "missing case for Preposition $LEMMA - ".join(", ",@$FORMS)."\n"; 
	return; 
    }
    print $abslexfh "\tlex".$$count."_Prep : Prep ; -- $LEMMA\n";
    foreach my $FORM (@$FORMS) { $TEST{$FORM} = 1 };
    $$count++;
}

sub adverb
{
    my ($LEMMA,$FEATS,$FORMS,$abslexfh,$conclexfh,$count) = @_;
    if (grep(/$LEMMA/,@$FORMS) == @$FORMS) {
	print $conclexfh "\tlex".$$count."_Adv = ss \"$LEMMA\" ;\n";
    }
    else { 
	print "missing case for Adverb $LEMMA - ".join(", ",@$FORMS)."\n"; 
	return; 
    } 
    print $abslexfh "\tlex".$$count."_Adv : Adv ; -- $LEMMA\n";
    foreach my $FORM (@$FORMS) { $TEST{$FORM} = 1 };
    $$count++;
    
}

# Main
{
    if (scalar(@ARGV)==0)
    {
	die "CONLL file needed as parameter\n";
    }
    my $conllfile = $ARGV[0];
    open my $conllfh, "<", $conllfile;
    while (my $line = <$conllfh>)
    {
	chomp $line;
	next if ($line eq "");
	my ($ID,$FORM,$LEMMA,$CPOSTAG,$POSTAG,$FEATS,$HEAD,$DEPREL,$PHEAD,$PDEPREL) = split /\s/,$line;
	to_hash($POSTAG,$FORM,$LEMMA,$FEATS)
    }
    my ($conllname,$conllpath,$conllsuffix) = fileparse($conllfile, qr/\.[^.]*/);
    $conllname =~ s/\W//g;
    open my $abslexfh, ">", $conllname."Abs.gf";
    open my $conclexfh, ">", $conllname.".gf";
    print $abslexfh "abstract ".$conllname."Abs = Cat ** open Cat in {\nfun\n";
    print $conclexfh "concrete $conllname of ".$conllname."Abs = CatLat ** open ParadigmsLat,Prelude,ParamX,ResLat in {\nlin\n";
    to_gf_lex($abslexfh,$conclexfh);
    print $abslexfh "}\n";
    print $conclexfh "}\n";
    close $abslexfh;
    close $conclexfh;
    open my $missingfh, ">", $conllname."Missing.gfs";
    open my $knownfh, ">", $conllname."Known.gfs";
    print $missingfh "i $conllname.gf\n";
    print $knownfh "i $conllname.gf\n";
    foreach my $t (keys %TEST)
    {
	print $missingfh "ma -missing \"$t\"\n";
	print $knownfh "ma -known \"$t\"\n";
    }
    close $missingfh;
    close $knownfh;
    #    print Dumper(%TEST);
}

