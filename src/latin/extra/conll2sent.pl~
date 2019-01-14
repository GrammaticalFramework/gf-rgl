use strict;
use warnings;
use File::Basename;

our %SENT;
# Main
{
    if (scalar(@ARGV)==0)
    {
	die "CONLL file needed as parameter\n";
    }
    my $conllfile = $ARGV[0];
    open my $conllfh, "<", $conllfile;
    my $words=[];
    while (my $line = <$conllfh>)
    {
	chomp $line;
	if ($line eq "")
	{
	    $SENT{join(" ", @$words)}=1;
	    $words=[];
	}
	else
	{
	    my ($ID,$FORM,$LEMMA,$CPOSTAG,$POSTAG,$FEATS,$HEAD,$DEPREL,$PHEAD,$PDEPREL) = split /\s/,$line;
	    if (scalar(@$words) != 0 || $POSTAG eq "Ne")
	    {
		push @$words,$FORM;
	    }
	    else
	    {
		push @$words,lc $FORM;
	    }
	    
	}
    }
    my ($conllname,$conllpath,$conllsuffix) = fileparse($conllfile, qr/\.[^.]*/);
    open my $sentfile,">",$conllname."Sent.txt";
    foreach my $sent (sort {$a cmp $b } keys %SENT)
    {
	print $sentfile "$sent\n";
    }
}

