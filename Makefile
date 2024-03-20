SRCS = objects/cons.kt objects/object.kt objects/symbol.kt \
	objects/environment.kt objects/number.kt objects/string.kt \
	objects/table.kt objects/vector.kt objects/regexp.kt \
	interp/exception.kt interp/eval.kt \
	io/reader.kt io/token.kt io/repl.kt io/stream.kt io/load.kt \
	functions/function.kt functions/builtin.kt functions/lambda.kt \
	functions/macro.kt \
	builtins/helpers.kt builtins/numbers.kt builtins/cxr.kt \
	builtins/basic.kt builtins/system.kt builtins/alists.kt \
	builtins/environments.kt builtins/io.kt builtins/strings.kt \
	builtins/tables.kt builtins/utils.kt builtins/vectors.kt \
	builtins/sequences.kt builtins/macros.kt \
	utils/lists.kt utils/div.kt utils/interfaces.kt \
	sys/messages.kt sys/main.kt sys/globalvars.kt
GENSRCS = generated/buildtag.kt generated/init-builtins.kt \
	generated/preload.kt
ALLSRCS = $(SRCS) $(GENSRCS)
PRELOAD = $(shell echo preload/*.lisp)

BUILDSCRIPTS = scripts/buildtag.sh scripts/gen-bi-init

BUILTINSRC = $(shell ls builtins/*.kt | egrep -v '(helpers)\.kt')

COMP = kotlinc
NATIVECOMP = kotlinc-native

INSTALLDIR=/opt/w21/lyk
INSTALLBIN=/opt/w21/bin

# JAR=$(basename $1 .kt).jar
# kotlinc $1 -include-runtime -d $JAR && java -jar $JAR
# rm -f $JAR


build: lyk.jar

lyk.jar: $(ALLSRCS) Makefile
	$(COMP) $(ALLSRCS) -include-runtime -d lyk.jar
	java -jar lyk.jar -qe '(build-info t)'

generated:
	mkdir -p generated

generated/preload.kt: Makefile generated scripts/preload.sh $(PRELOAD)
	./scripts/preload.sh $(PRELOAD) > generated/preload.kt

generated/buildtag.kt: Makefile generated $(SRCS) $(BUILDSCRIPTS)
	scripts/buildtag.sh lyk > generated/buildtag.kt

generated/init-builtins.kt: Makefile generated scripts/gen-bi-init $(BUILTINSRC)
	-rm -f generated/Makefile && \
	    ln -s ../scripts/Subdirmakefile generated/Makefile
	scripts/gen-bi-init $(BUILTINSRC) > generated/init-builtins.kt

new: $(SRCS) functions/function.kt
	$(COMP) $(SRCS)

native: $(SRCS) Makefile
	$(NATIVECOMP) $(SRCS) -o lykn
	mv lykn.pexe lykn

clean:
	-rm -rf *~ */*~ org *.dSYM *.kexe *.class *.jar generated META-INF lyk

install: lyk.jar
	mkdir -p $(INSTALLDIR)
	-rm -rf $(INSTALLDIR)/*
	install -c lyk.jar $(INSTALLDIR)
	install -c scripts/lyk $(INSTALLBIN)
