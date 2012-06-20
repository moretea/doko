#!/bin/sh

erl +c -pa `pwd`/../apps/*/ebin `pwd`/../deps/*/ebin -sname doko_systest \
    -run reloader
