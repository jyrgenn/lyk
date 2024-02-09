
SRCS = cons.kt object.kt symbol.kt basedefs.kt main.kt environment.kt \
	utils.kt
COMP = kotlinc-native

compile-new:
	kotlinc $(SRCS) reader.kt stream.kt

build: $(SRCS) Makefile
	$(COMP) $(SRCS)

clean:
	-rm -rf *~ org *.dSYM *.kexe *.class META-INF lyk
