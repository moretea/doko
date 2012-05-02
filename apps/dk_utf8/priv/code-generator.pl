#! /usr/bin/env perl

open(INPUT, "<", "CaseFolding.txt") 
  or die "Couldn't open CaseFolding.txt for reading: $!\n";

while (<INPUT>) {
    next if /^#/;
    ($code, $status, $mapping) = split(/;\s*/);
    if ($status eq "C" or $status eq "S") {
        print "16#$code -> 16#$mapping;\n";
    }
}
