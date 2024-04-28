SRCS = objects/cons.kt objects/object.kt objects/symbol.kt \
	objects/environment.kt objects/number.kt objects/string.kt \
	objects/table.kt objects/vector.kt objects/regexp.kt objects/char.kt \
	io/reader.kt io/token.kt io/repl.kt io/stream.kt io/load.kt \
	functions/function.kt functions/builtin.kt functions/lambda.kt \
	functions/macro.kt \
	builtins/helpers.kt builtins/numbers.kt builtins/cxr.kt \
	builtins/basic.kt builtins/system.kt builtins/alists.kt \
	builtins/environments.kt builtins/io.kt builtins/strings.kt \
	builtins/tables.kt builtins/utils.kt builtins/vectors.kt \
	builtins/sequences.kt builtins/macros.kt builtins/factor.kt \
	builtins/chars.kt \
	utils/lists.kt utils/div.kt utils/interfaces.kt \
	sys/exception.kt sys/eval.kt sys/messages.kt sys/main.kt \
	sys/globalvars.kt sys/exit.kt
GENSRCS = generated/buildtag.kt generated/init-builtins.kt \
	generated/preload.kt
ALLSRCS = $(SRCS) $(GENSRCS)
PRELOAD = $(shell echo preload/*.lisp) generated/10-types.lisp

BUILDSCRIPTS = scripts/buildtag.sh scripts/generate-builtin-init

BUILTINSRC = $(shell ls builtins/*.kt | egrep -v '(helpers)\.kt')

COMP = kotlinc

INSTALLDIR=/opt/w21/lyk
INSTALLBIN=/opt/w21/bin

# JAR=$(basename $1 .kt).jar
# kotlinc $1 -include-runtime -d $JAR && java -jar $JAR
# rm -f $JAR


build: lyk.jar

lyk.jar: $(ALLSRCS) generated/org generated/jline Makefile
	$(COMP) -cp generated $(ALLSRCS) -include-runtime -d lyk.jar
	cd generated; jar -f ../lyk.jar -u org jline
	./scripts/lyk -J . -e '(build-info t)'

generated/org generated/jline:
	cd generated; jar -xf ../jline/jline.jar

generated/10-types.lisp: Makefile scripts/list-types.sh $(SRCS)
	./scripts/list-types.sh > generated/10-types.lisp

generated:
	mkdir -p generated

generated/preload.kt: Makefile generated scripts/preload.sh $(PRELOAD)
	./scripts/preload.sh $(PRELOAD) > generated/preload.kt

generated/buildtag.kt: Makefile generated $(SRCS) $(BUILDSCRIPTS)
	scripts/buildtag.sh lyk > generated/buildtag.kt

generated/init-builtins.kt: Makefile generated scripts/generate-builtin-init \
			    $(BUILTINSRC)
	-rm -f generated/Makefile && \
	    ln -s ../scripts/Subdirmakefile generated/Makefile
	scripts/generate-builtin-init $(BUILTINSRC) > generated/init-builtins.kt

new: clean build

test: lyk.jar
	./run-tests.lisp

clean:
	-rm -rf *~ */*~ org *.dSYM *.kexe *.class *.jar *.log generated

install: test
	mkdir -p $(INSTALLDIR)
	-rm -rf $(INSTALLDIR)/*
	install -c lyk.jar $(INSTALLDIR)
	install -c scripts/lyk $(INSTALLBIN)
