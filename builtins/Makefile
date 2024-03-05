
BUILTINSRC = $(shell ls *.kt | egrep -v '(init-builtins|helpers)\.kt')

all:
	cd .. && echo "make[1]: Entering directory \`$$PWD'" && $(MAKE)

../generated/init-builtins.kt: ../scripts/generate-init.py Makefile $(BUILTINSRC)
	../scripts/generate-init.py $(BUILTINSRC) > ../generated/init-builtins.kt
