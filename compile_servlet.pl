#!/usr/bin/perl

use strict;
use XML::Simple;
use Data::Dumper;

our $inputfile;
our $outputdir;

if(scalar(@ARGV) eq 0) {
    print "compile_servlet.pl - Compiles an OSP servlet and its XML metadata\n";
    print "Usage:\n\tcompile_servlet.pl [-d dir] <input file>\n\n";
    exit(-1);
} elsif(scalar(@ARGV) eq 1) {
    $inputfile = $ARGV[0];
    $outputdir = "./"
} else {
    $inputfile = $ARGV[2];
    $outputdir = $ARGV[1];
}

our $tempdir;
our $basename = $inputfile;
$basename =~ s/\.[a-z]*$//;
$basename =~ s/^.*\///;
my $extension = $inputfile;
$extension =~ s/^(.*\.)([a-z]+)$/$2/;
my $ret;
if($extension =~ /erl/) {
    $ret = erl();
} elsif($extension =~ /sdf/) {
    $ret = sdf();
} elsif($extension =~ /sap/) {
    $ret = sap();
} else {
    print "Error, invalid filetype\n";
    exit(-1);
}
if($ret eq 0) {
    print "$basename - Compilation complete\n";
    exit(0);
} else {
    print "Error in compilation of $basename\n";
    exit($ret);
}

sub sap {
    `mkdir $basename`;
    `unzip -d $basename $inputfile`;
    opendir(DIR, $basename);
    while(defined(my $file = readdir(DIR))) {
	if($file =~ /^\.$/ || $file =~ /^\.\.$/) {
	    # Do nothing
	} else {
	    my $ret = `./compile_servlet.pl $basename/$file`;
	    if($ret =~ /complete/) {
		# Do nothing
		my $generatedfile = $file;
		$generatedfile =~ s/\.[a-z]*$//;
		$generatedfile .= ".erl";
		`mv -f $generatedfile $basename/`;
	    } else {
		return -1;
	    }
	}
    }
    closedir(DIR);
    `mv $basename $outputdir`;
    return 0;
}

sub erl {
    `mv $inputfile $outputdir`;
    return 0;
}

sub sdf {
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

    if (-e "$module.erl") {
	`mv $module.erl $outputdir`;
	return 0;
    } else {
	return -1;
    }

}
