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

stack build --fast
stack install protolude
#stack install classy-prelude
#stack install basic-prelude

INTRO=$(find ../.stack-work -name '*.hi' | grep 8.8.2 | grep 'Intro.hi' | head -n1)
PRELUDE=$(find -L ~/.stack -name '*.hi' | grep 8.8.2 | grep 'Prelude.hi' | head -n1)
BASIC=$(find -L ~/.stack -name '*.hi' | grep 8.8.2 | grep 'BasicPrelude.hi' | head -n1)
CORE=$(find -L ~/.stack -name '*.hi' | grep 8.8.2 | grep 'CorePrelude.hi' | head -n1)
PROTO=$(find -L ~/.stack -name '*.hi' | grep 8.8.2 | grep 'Protolude.hi' | head -n1)
CLASSY=$(find -L ~/.stack -name '*.hi' | grep 8.8.2 | grep 'ClassyPrelude.hi' | head -n1)

compare 'Intro' $INTRO 'Prelude' $PRELUDE
compare 'Intro' $INTRO 'Protolude' $PROTO
#compare 'Intro' $INTRO 'BasicPrelude' $BASIC
#compare 'Intro' $INTRO 'CorePrelude' $CORE
#compare 'Intro' $INTRO 'ClassyPrelude' $CLASSY
