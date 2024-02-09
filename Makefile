
SRCS = cons.kt object.kt symbol.kt basedefs.kt main.kt environment.kt \
	utils.kt
COMP = kotlinc-native


build: $(SRCS) Makefile
	$(COMP) $(SRCS)

clean:
	-rm -rf *~ org *.dSYM *.kexe *.class META-INF lyk
