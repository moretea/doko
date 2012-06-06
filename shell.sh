#!/bin/sh

erl -pa `pwd`/apps/*/ebin -pa `pwd`/deps/*/ebin +c -run reloader
