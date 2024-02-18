
SRCS = objects/cons.kt objects/object.kt objects/symbol.kt \
	objects/environment.kt objects/number.kt objects/string.kt \
	objects/table.kt objects/vector.kt objects/regexp.kt \
	objects/comparable.kt \
	interp/basedefs.kt interp/main.kt interp/exception.kt \
	div/utils.kt \
	io/reader.kt io/repl.kt io/stream.kt \
	functions/function.kt functions/builtin.kt functions/helpers.kt \
	functions/lambda.kt \
	interp/eval.kt

#SRCS = cons.kt object.kt symbol.kt basedefs.kt main.kt environment.kt \
#	utils.kt exception.kt reader.kt stream.kt number.kt string.kt \
#	regexp.kt table.kt vector.kt repl.kt

COMP = kotlinc
NATIVECOMP = kotlinc-native

# JAR=$(basename $1 .kt).jar
# kotlinc $1 -include-runtime -d $JAR && java -jar $JAR
# rm -f $JAR


build: $(SRCS) Makefile
	$(COMP) $(SRCS) -include-runtime -d lyk.jar

new: $(SRCS) functions/function.kt
	$(COMP) $(SRCS) interp/eval.kt

native: $(SRCS) Makefile
	$(NATIVECOMP) $(SRCS) -o lykn
	mv lykn.pexe lykn

clean:
	-rm -rf *~ org *.dSYM *.kexe *.class *.jar META-INF lyk
