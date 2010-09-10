#!/bin/sh
#
# run repl with all libs and sources of a leiningen project
# author Steffen Dienst <steffen.dienst@gmail.com>
#
#use all libs in ./lib and ./lib/dev
PROJECT_CP="src:test:classes"
PROJECT_LIBS=$( echo lib/*.jar lib/dev/*.jar | sed 's/ /:/g' )
CLASSPATH="${PROJECT_LIBS}:${PROJECT_CP}"
#change path to windows layout under cygwin
if [ "$OSTYPE" = "cygwin" ]; then
    CLASSPATH=`cygpath -wp $CLASSPATH`
fi
#echo $CLASSPATH
BREAK_CHARS="(){}[],^%$#@\"\";:''|\\"
touch ${HOME}/.completions
rlwrap --remember -c -f "${HOME}/.cljcompletions" -b "${BREAKCHARS}" java -cp ${CLASSPATH} clojure.main devrepl.clj