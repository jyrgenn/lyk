
SRCS = cons.kt object.kt symbol.kt basedefs.kt main.kt environment.kt \
	utils.kt exception.kt reader.kt stream.kt number.kt string.kt \
	regexp.kt table.kt vector.kt repl.kt

COMP = kotlinc
NATIVECOMP = kotlinc-native

# JAR=$(basename $1 .kt).jar
# kotlinc $1 -include-runtime -d $JAR && java -jar $JAR
# rm -f $JAR


build: $(SRCS) Makefile
	$(COMP) $(SRCS) -include-runtime -d lyk.jar

native: $(SRCS) Makefile
	$(NATIVECOMP) $(SRCS) -o lykn
	mv lykn.pexe lykn

clean:
	-rm -rf *~ org *.dSYM *.kexe *.class *.jar META-INF lyk
