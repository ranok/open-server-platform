#!/usr/bin/perl

use strict;

my $output = "join.sh";

print "gen-join.pl v0.1\n(C) Jacob Torrey 2008\n\n";
print "Enter a name for this node: ";
my $name = <STDIN>; chomp($name);
print "Enter the IP of the bootserver (leave blank for local OSP distribution): ";
my $server = <STDIN>; chomp($server);
print "Use FQDN for node names (0/1): ";
my $fqdn = <STDIN>; chomp($fqdn);
print "OSP masternode name: ";
my $master = <STDIN>; chomp($master);
print "Cookie: ";
my $cookie = <STDIN>; chomp($cookie);
open(FP, ">$output");
print FP "#!/bin/bash\n\nNODENAME=\"$name\"\nBOOTSERVER=\"$server\"\nFQDN=$fqdn # Use FQDN for node name: 0 for false, 1 for true\nMASTERNODE=\"$master\"\n";
    print FP "COOKIE=\"$cookie\"\nNAME=\"-sname\"\nif [ \$FQDN -eq 1 ]\nthen\n\tNAME=\"-name\"\nfi\n";
print FP "LOADER=\"-loader inet -hosts\"\nif [ -a \"osp.beam\" ]\nthen\n\tLOADER=\"\"\n\tBOOTSERVER=\"\"\nfi\nepmd -daemon\nerl -noshell -detached \$NAME \$NODENAME -id \$NODENAME \$LOADER \$BOOTSERVER -setcookie \$COOKIE -s osp join \$MASTERNODE\n";
close(FP);
`chmod +x $output`;
print "\nRun ./join.sh to join an existing OSP cluster\n";
