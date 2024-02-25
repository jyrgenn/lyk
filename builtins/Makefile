
BUILTINSRC = $(shell ls *.kt | egrep -v '(init-builtins|helpers)\.kt')

all:
	cd .. && echo "make[1]: Entering directory \`$$PWD'" && $(MAKE)

init-builtins.kt: generate-init.py Makefile $(BUILTINSRC)
	./generate-init.py $(BUILTINSRC) > init-builtins.kt
