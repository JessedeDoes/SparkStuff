use strict;
open(umap,"Karakters/wntchars.tab") || die;

my %map;
my %bijgemaakt;
my %unresolved_entities;
my %ismapping;
my %o2n;

while(<umap>)
{
  chomp();
  s/\r//;
  my ($ent,$mapping,$x,$bgmkt) = split(/\t/,$_);
  $bijgemaakt{$ent} = $bgmkt;
  $map{$ent} = $mapping;
  $ismapping{$mapping}++;
}

foreach my $x (sort keys %map)
{
  if ($ismapping{$x})
  {
#    warn "doorverwijzing $x --> $map{$x}";
     delete $map{$x};
  }
}

sub read_verplaatsing
{
  my $vp = "Karakters/VERPLAATSING.tab";
  open(vp, $vp) || die $vp;
  while(<vp>)
  {
    chomp();
    s/\r//;
    my ($x,$y,$o,$n) = split(/\t/,$_);
    $o2n{$o} = $n; 
  }
}

&read_verplaatsing();

sub print_verplaatst
{
  my %newmap;
  sub onhex
  {
    return $newmap{$a} cmp $newmap{$b};
  }  

  foreach my $x (keys %map)
  {
    my $z = $map{$x};
    if ($z =~ /&#x(.*?);/)
    {
      $z = $1;
      if ($o2n{$z})
      {
        $z = $o2n{$z};
      }
    }
    $newmap{$x} = $z;
  }


  print <<END;
<style type='text/css'>
.first { }
.next { font-size: 14pt; background-color: pink;}
.row { clear: both; }
</style>
END
  print "<div style='column-count: 2'>";
  print "<table border=1 style='border-collapse:collapse; border-width:.5pt'>";
  foreach my $x (sort onhex keys %newmap)
  {
    my $f = $newmap{$x};
    my $formatted = "&#x$f;";
    if ($bijgemaakt{$x})
    {
      $formatted = "<font face='Inl vmnw wnt'>$formatted</font>";
    }
    if ($f =~ /</)
    {
      $formatted = $f;
    }
    my $code = $newmap{$x};
    $code =~ s/</&lt;/g;
    $x =~ s/&/&amp;/;
    print "	<tr><td class='first'>$x</td><td class='first'>$code</td><td class='next'><div class='next'>$formatted</div></td></tr>\n";
  }
  print "</table>>";
}

##&print_verplaatst();

sub repair_entity_declaration
{
  my $decl = shift;
  if ($decl =~ /<!ENTITY\s+(\S+)\s/)
  {
    my $ent = $1;
    my $z;
    if (($z = $map{"&$ent;"}) && ($z =~ /^&[^&]*;$/))
    {
      $decl =~ s/&.*?;/$z/;
      return $decl;
    }
  }
  return $decl;
}

sub escape_ent
{
  my $ent = shift;
  if ($ent eq "&sects;")
  {
     return &escape_ent("&sect;") . &escape_ent("&sect;");
  }
  if ($ent eq "&amp;")
  {
    return $ent;
  } else
  {
    if ($map{$ent})
    {
      my $m = $map{$ent};
      if ($bijgemaakt{$ent})
      {
        ##warn "<font face=\"Inl vmnw wnt\">$m</font>";
        return "<font face=\"Inl vmnw wnt\">$m</font>";
      } else
      {
        return $m;
      }
    }

    if ($ent =~ /&#x[0-9A-F]{4};/i)
    {
      return $ent;
    }

    if ($ent =~ /&#[0-9]+;/i)
    {
      return $ent;
    }

    if ($ent =~ /&sup(.);/)
    {
      return "<SUP>$1<\/SUP>";
    }
    $unresolved_entities{$ent}++;
    $ent =~ s/[&;]//g;
    return "[$ent]";
  }
}

sub utf8_ent
{
  my $ent = shift;

  if ($ent eq "&amp;")
  {
    return $ent;
  } else
  {
    if ($ent =~ /&#x([0-9A-F]{4});/i)
    {
      my $hex = $1;
      my $d = eval("0x$hex");
      return sprintf("%c",$d);
    }

    if ($ent =~ /&#([0-9]+);/i)
    {
      return sprintf("%c",$1);
    }

    if ($map{$ent})
    {
      return &utf8_ent($map{$ent}); # gevaarlijk als map loopjes bevat
    }

    if ($ent =~ /&sup(.);/)
    {
      return "<SUP>$1<\/SUP>";
    }
    $unresolved_entities{$ent}++;
    $ent =~ s/[&;]//g;
    return "[$ent]";
  }
}

sub ents_to_utf8
{
  my $z = shift;
  $z =~ s/&(.*?);/&utf8_ent($&)/eg;
  return $z;
}

sub escape_amp
{
  my $z = shift;
  $z =~ s/&/&amp;/g;
  $z =~ s/</&lt;/g;
  $z =~ s/'/&#x0027;/g;
  return $z;
}
1;

