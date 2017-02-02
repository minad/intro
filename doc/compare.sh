#!/bin/bash

getExports() {
    stack exec -- ghc --show-iface $1 | perl -ne 'if (/^exports:$/ .. /^module dependencies:/) { if (/^  (.*?)\|?\{(.*)\}/) { print "$1\n"; my $x = $2; $x =~ s/ /\n/g; print "$x\n"; } elsif (/^  (.*)/) { print "$1\n"; } }' | sort
}

compare() {
    local name1=$1
    local file1=$2
    local name2=$3
    local file2=$4
    getExports $file1 > $name1.exports
    getExports $file2 > $name2.exports
    (echo -e "# Only in $name1\n"
     diff -U0 $name2.exports $name1.exports | grep -v -- '^+++' | grep '^+' | sed 's/^+/* /g'
     echo -e "\n# Only in $name2\n"
     diff -U0 $name2.exports $name1.exports | grep -v -- '^---' | grep '^-' | sed 's/^-/* /g'
    ) > "Compare_${name1}_${name2}.md"
}

stack build
stack install basic-prelude
stack install classy-prelude
stack install protolude
#stack install validity validity-containers introduction

INTRO=$(find ../.stack-work -name '*.hi' | grep 'Intro.hi' | head -n1)
PRELUDE=$(find -L ~/.stack -name '*.hi' | grep 'base-4.*/Prelude.hi' | head -n1)
BASIC=$(find -L ~/.stack -name '*.hi' | grep 'BasicPrelude.hi' | head -n1)
CORE=$(find -L ~/.stack -name '*.hi' | grep 'CorePrelude.hi' | head -n1)
PROTO=$(find -L ~/.stack -name '*.hi' | grep 'Protolude.hi' | head -n1)
CLASSY=$(find -L ~/.stack -name '*.hi' | grep 'ClassyPrelude.hi' | head -n1)
#INTRODUCTION=$(find -L ~/.stack -name '*.hi' | grep 'Introduction.hi' | head -n1)

compare 'Intro' $INTRO 'Prelude' $PRELUDE
compare 'Intro' $INTRO 'BasicPrelude' $BASIC
compare 'Intro' $INTRO 'CorePrelude' $CORE
compare 'Intro' $INTRO 'Protolude' $PROTO
compare 'Intro' $INTRO 'ClassyPrelude' $CLASSY
#compare 'Intro' $INTRO 'Introduction' $INTRODUCTION
