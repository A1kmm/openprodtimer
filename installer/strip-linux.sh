#!/bin/sh

strip -R .comment ./dist/build/prodtimer/prodtimer
chrpath -d ./dist/build/prodtimer/prodtimer
