#!/bin/sh

erl +P 134217727 -pa `pwd`/apps/*/ebin -pa `pwd`/deps/*/ebin +c -run reloader
