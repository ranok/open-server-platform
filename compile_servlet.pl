#!/usr/bin/perl

use strict;
use XML::Simple;
use Data::Dumper;

if(scalar(@ARGV) eq 0) {
    print "compile_servlet.pl - Compiles an OSP servlet and its XML metadata\n";
    print "Usage:\n\tcompile_servlet.pl <input file>\n\n";
    exit(-1);
}

my $inputfile = $ARGV[0];
my $parser = new XML::Simple;

my $data = $parser->XMLin($inputfile);

my $author = "'".$data->{author}."'";
my $module = $data->{name};
my $version = "'".$data->{version}."'";
my $code = $data->{code}->{server};
my $initfunc = $data->{code}->{init};
my $cleanup = $data->{code}->{cleanup};
my $proto = $data->{proto};


`cat header.erl | sed -e s/AUTHOR/$author/ -e s/PROTO/$proto/ -e s/VSN/$version/ -e s/MOD/$module/g > $module.erl`;
open(FP, ">>$module.erl");
print FP $code;
close(FP);
open(FP, ">>$module.erl");
print FP $initfunc;
close(FP);
open(FP, ">>$module.erl");
print FP $cleanup;
close(FP);
#`erlc $module.erl`;
#`rm $module.erl`;

print "$module generated successfully!\nRun erlc $module.erl to compile\n";
