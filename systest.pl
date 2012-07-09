#!/usr/bin/env perl

print "Running system test...\n";
$output = `ct_run -dir systest -suit doko_SUITE -logdir systest/logs -erl_flags -pa apps/*/ebin -pa deps/*/ebin`;

if ($output =~ /TEST COMPLETE, (\d+) ok, 0 failed of \1 test cases/i) {
    print "All $1 tests passed.\n";
    exit 0;
}
else {
    print $output;
    exit 1;
}
