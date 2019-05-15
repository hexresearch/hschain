MAKEFLAGS += --no-builtin-rules
.SUFFIXES:
.PHONY: all clean

# Input files 
CFGS     = $(wildcard *.cfg)
TLA_MAIN = FastMutex.tla
# Outputs
TLA_OUT  = build/$(TLA_MAIN)
CFG_MAIN = $(TLA_OUT:.tla=.cfg)
CFG_OUT  = $(addprefix build/, $(CFGS))
WRAPPERS = $(CFG_OUT:.cfg=.sh)

## ================================================================
all: build $(WRAPPERS) $(CFG_OUT)
clean:
	rm -rf build *.log

build:
	mkdir -p build
# Run PlusCal translator
build/%.tla: %.tla | build
	cp $< $@
	pcal $@
# pcal generates both *.TLA & *.cfg
$(CFG_MAIN): $(TLA_OUT)
# Generate config for running model
build/%.cfg: %.cfg $(CFG_MAIN) | build
	cat $< $(CFG_MAIN) > $@
# Generate wrapper script for running model checker
build/%.sh: build/%.cfg $(CFG_OUT)
	( echo "#!/bin/sh"; \
	  echo "cd ${PWD}/build"; \
	  echo "tlc2 $(TLA_MAIN) -config $(patsubst build/%,%,$<) $$@" ) > $@
	chmod a+x $@
