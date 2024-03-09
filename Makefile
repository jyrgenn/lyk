SRCS = objects/cons.kt objects/object.kt objects/symbol.kt \
	objects/environment.kt objects/number.kt objects/string.kt \
	objects/table.kt objects/vector.kt objects/regexp.kt \
	interp/exception.kt interp/eval.kt \
	io/reader.kt io/token.kt io/repl.kt io/stream.kt \
	functions/function.kt functions/builtin.kt functions/lambda.kt \
	functions/macro.kt \
	builtins/helpers.kt builtins/numbers.kt builtins/cxr.kt \
	builtins/basic.kt builtins/system.kt builtins/alists.kt \
	builtins/environments.kt builtins/io.kt \
	utils/lists.kt utils/div.kt utils/interfaces.kt \
	sys/debug.kt sys/main.kt sys/globalvars.kt \
	generated/buildtag.kt generated/init-builtins.kt

BUILTINSRC = $(shell ls builtins/*.kt | egrep -v '(helpers)\.kt')
COMP = kotlinc
NATIVECOMP = kotlinc-native

# JAR=$(basename $1 .kt).jar
# kotlinc $1 -include-runtime -d $JAR && java -jar $JAR
# rm -f $JAR


build: Makefile buildtag generated/init-builtins.kt lyk.jar

lyk.jar: $(SRCS)
	$(COMP) $(SRCS) -include-runtime -d lyk.jar

generated/buildtag.kt: buildtag
buildtag:
	mkdir -p generated
	scripts/buildtag.sh lyk > generated/buildtag.kt

generated/init-builtins.kt: scripts/gen-bi-init Makefile $(BUILTINSRC)
	mkdir -p generated
	-rm -f generated/Makefile && \
	    ln -s ../scripts/Subdirmakefile generated/Makefile
	scripts/gen-bi-init $(BUILTINSRC) > generated/init-builtins.kt

new: $(SRCS) functions/function.kt
	$(COMP) $(SRCS)

native: $(SRCS) Makefile
	$(NATIVECOMP) $(SRCS) -o lykn
	mv lykn.pexe lykn

clean:
	-rm -rf *~ */*~ org *.dSYM *.kexe *.class *.jar generated/* META-INF lyk
