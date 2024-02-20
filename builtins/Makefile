
all:
	cd .. && echo "make[1]: Entering directory \`$$PWD'" && $(MAKE)

init-builtins.kt: generate-init.py builtin-descs Makefile
	./generate-init.py builtin-descs > init-builtins.kt

