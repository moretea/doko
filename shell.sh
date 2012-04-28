#!/bin/sh

erl -pa `pwd`/apps/*/ebin +c -run reloader
