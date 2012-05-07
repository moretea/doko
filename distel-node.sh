#!/bin/sh

erl -pa `pwd`/apps/*/ebin `pwd`/deps/*/ebin -run reloader -sname emacs -detached
