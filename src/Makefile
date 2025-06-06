SRCS = objects/cons.kt objects/object.kt objects/symbol.kt \
	objects/environment.kt objects/number.kt objects/string.kt \
	objects/table.kt objects/vector.kt objects/regexp.kt objects/char.kt \
	io/reader.kt io/token.kt io/repl.kt io/stream.kt io/load.kt \
	io/format.kt \
	functions/function.kt functions/builtin.kt functions/lambda.kt \
	functions/macro.kt \
	builtins/helpers.kt builtins/numbers.kt builtins/cxr.kt \
	builtins/basic.kt builtins/system.kt builtins/alists.kt \
	builtins/environments.kt builtins/io.kt builtins/strings.kt \
	builtins/tables.kt builtins/utils.kt builtins/vectors.kt \
	builtins/sequences.kt builtins/macros.kt builtins/factor.kt \
	builtins/chars.kt builtins/files.kt \
	utils/lists.kt utils/div.kt utils/interfaces.kt \
	sys/exception.kt sys/eval.kt sys/messages.kt sys/main.kt \
	sys/globalvars.kt sys/exit.kt sys/hooks.kt
GENSRCS = generated/buildtag.kt generated/init-builtins.kt \
	generated/preload.kt
ALLSRCS = $(SRCS) $(GENSRCS)
PRELOAD = $(shell echo preload/*.lisp) generated/10-types.lisp

BUILDSCRIPTS = scripts/buildtag.sh scripts/generate-builtin-init

BUILTINSRC = $(shell ls builtins/*.kt | egrep -v '(helpers)\.kt')

COMP = kotlinc

INSTALLBASE = /opt/w21
STAGE_BASE  = stage
STAGE_DIR   = $(STAGE_BASE)/lib/lyk
INSTALLDIR =  $(INSTALLBASE)/lib/lyk
INSTALLBIN =  $(INSTALLBASE)/bin
SITELYKDIR =  $(INSTALLBASE)/lib/site-lyk

# JAR=$(basename $1 .kt).jar
# kotlinc $1 -include-runtime -d $JAR && java -jar $JAR
# rm -f $JAR


build: lyk.jar

lyk.jar: $(ALLSRCS) generated/jline Makefile tags l/alldocs.lisp
	@echo compile...
	$(COMP) -cp generated/jline $(ALLSRCS) -include-runtime -d $@ 2>&1 |\
		sed 's/\x1b\[[0-9;]*[a-zA-Z]//g'
	cd generated/jline; jar -f ../../lyk.jar -u [a-z]*
	./scripts/lyk -J . -V


generated/DOCSTRINGS.md: lyk.jar
	./scripts/lyk -J . l/alldocs.lisp > generated/DOCSTRINGS.md

generated/jline: generated
	mkdir -p generated/jline
	cd generated/jline; jar -xf ../../../external/jline/jline.jar

generated/10-types.lisp: Makefile generated scripts/list-types.sh $(SRCS)
	./scripts/list-types.sh > generated/10-types.lisp

generated:
	mkdir -p generated

generated/preload.kt: Makefile generated scripts/preload.sh $(PRELOAD)
	./scripts/preload.sh $(PRELOAD) > generated/preload.kt

generated/buildtag.kt: Makefile generated $(SRCS) $(BUILDSCRIPTS)
	scripts/buildtag.sh lyk $(INSTALLDIR) > generated/buildtag.kt

generated/init-builtins.kt: Makefile generated scripts/generate-builtin-init \
			    $(BUILTINSRC)
	-rm -f generated/Makefile && \
	    ln -s ../scripts/Subdirmakefile generated/Makefile
	scripts/generate-builtin-init $(BUILTINSRC) > generated/init-builtins.kt

tags: $(PRELOAD) $(ALLSRCS)
	etags \
	      -r "/[ \t]*fun[ \t]+\([A-Za-z0-9_]+\)/\1/"      \
	      -r "/\(var\|val\)[ \t]+\([A-Za-z0-9_]+\)/\2/"   \
	      -r "/\/\/\/[ \t]+builtin[ \t]+\([^ \t]+\)/\1/"  \
	      -r "/.* ?class[ \t]+\([^ \t]+\)/\1/"            \
	      -r "/[ \t]+\([A-Za-z0-9_]+\)[ \t]+to[ \t]+/\1/" \
	      $(PRELOAD) $(ALLSRCS)

measure:
	./scripts/lyk -J . -vl l/new-factor.lisp -e '(factors 1000000000001)'

new: clean build

test: lyk.jar
	./run-tests.lisp

clean:
	-rm -rf *.jar *.tmp *~ */*~ TAGS *.log generated stage

pkg: $(STAGE_BASE)/lyk.tar.gz

$(STAGE_BASE)/lyk.tar.gz: stage
	cd $(STAGE_BASE) && tar zcvf lyk.tar.gz bin lib

install: pkg
	mkdir -p $(INSTALLDIR) $(INSTALLBIN)
	tar -C $(INSTALLBASE) -zxmvf $(STAGE_BASE)/lyk.tar.gz

stage: generated/DOCSTRINGS.md
	-rm -rf $(STAGE_BASE)
	mkdir -p $(STAGE_BASE)/lib/lyk $(STAGE_BASE)/bin
	rsync -a l $(STAGE_DIR)
	rsync -a ../doc $(STAGE_DIR)/
	install -c lyk.jar $(STAGE_DIR)
	install -c ../README.md ../LICENSE $(STAGE_DIR)/
	install -c generated/DOCSTRINGS.md $(STAGE_DIR)/doc/
	install -d $(STAGE_DIR)/jline
	install -c ../external/jline/LICENSE $(STAGE_DIR)/jline/
	install -c scripts/lyk $(STAGE_BASE)/bin
	chmod +x $(STAGE_BASE)/bin/lyk
	sed -e "s|@UNRELEASED@|$$(scripts/lyk -V)|" l/lyc \
		> $(STAGE_BASE)/bin/lyc
	chmod +x $(STAGE_BASE)/bin/lyc
