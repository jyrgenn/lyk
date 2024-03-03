
SRCS = objects/cons.kt objects/object.kt objects/symbol.kt \
	objects/environment.kt objects/number.kt objects/string.kt \
	objects/table.kt objects/vector.kt objects/regexp.kt \
	objects/comparable.kt \
	interp/basedefs.kt interp/exception.kt \
	io/reader.kt io/repl.kt io/stream.kt \
	functions/function.kt functions/builtin.kt functions/lambda.kt \
	functions/macro.kt \
	interp/eval.kt \
	builtins/init-builtins.kt builtins/helpers.kt builtins/numbers.kt \
	builtins/basic.kt builtins/system.kt \
	utils/lists.kt utils/div.kt utils/interfaces.kt \
	sys/debug.kt sys/main.kt \
	generated/buildtag.kt


#SRCS = cons.kt object.kt symbol.kt basedefs.kt main.kt environment.kt \
#	utils.kt exception.kt reader.kt stream.kt number.kt string.kt \
#	regexp.kt table.kt vector.kt repl.kt

COMP = kotlinc
NATIVECOMP = kotlinc-native

# JAR=$(basename $1 .kt).jar
# kotlinc $1 -include-runtime -d $JAR && java -jar $JAR
# rm -f $JAR


build: Makefile buildtag init-builtins lyk.jar

lyk.jar: $(SRCS)
	$(COMP) $(SRCS) -include-runtime -d lyk.jar

generated/buildtag.kt: buildtag
buildtag:
	scripts/buildtag.sh lyk > generated/buildtag.kt

init-builtins:
	$(MAKE) -C builtins init-builtins.kt


new: $(SRCS) functions/function.kt
	$(COMP) $(SRCS)

native: $(SRCS) Makefile
	$(NATIVECOMP) $(SRCS) -o lykn
	mv lykn.pexe lykn

clean:
	-rm -rf *~ org *.dSYM *.kexe *.class *.jar META-INF lyk
