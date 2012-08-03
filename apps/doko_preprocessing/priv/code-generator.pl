#! /usr/bin/env perl

$lang = $ARGV[0];

open(INPUT, "<", "stop_$lang.txt") 
  or die "Couldn't open stop_$lang.txt for reading: $!\n";

while (<INPUT>) {
    chomp;
    s/\s*\|.*$//g;
    next unless $_;
    next if /'/;
    print "<<\"$_\">>,\n";
}
