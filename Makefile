
SBT ?= sbt
SBT_FLAGS ?= -Dsbt.log.noformat=true
# If a chiselVersion is defined, use that.
# Otherwise, if we're making either "smoke" or "check" use the snapshot.
# Otherwise, use the latest release.
#ifneq (,$(chiselVersion))
#	SBT_FLAGS += -DchiselVersion="$(chiselVersion)"
#else
#	ifneq (,$(filter smoke check,$(MAKECMDGOALS)))
#		SBT_FLAGS += -DchiselVersion="2.3-SNAPSHOT"
#	else
#		SBT_FLAGS += -DchiselVersion="latest.release"
#	endif
#endif

CHISEL_FLAGS :=

top_srcdir ?= .
srcdir ?= src/main/scala/*
executables := $(filter-out top, $(notdir $(basename $(wildcard $(srcdir)/*.scala))))
outs := $(addsuffix .out, $(executables))
source_files := $(wildcard $(srcdir)/*.scala)

# Handle staging.
staging_dir := ~/.sbt/0.13/staging
staging_dirs := $(foreach dir, $(wildcard $(staging_dir)/*), $(wildcard $(dir)/*)) # Get the directory of each staging project.
staging_targets := $(addsuffix /update.stage, $(staging_dirs)) # Add a phoney target to staging dir.

default: emulator

all: emulator verilog # dreamer

clean:
	-rm -f *.h *.hex *.flo *.cpp *.o *.out *.v *.vcd $(executables)
	-rm -rf project/target/ target/

cleanall: clean
	-rm -rf $(staging_dir)/*

emulator: $(outs)

dreamer: $(addsuffix .hex, $(executables))

verilog: $(addsuffix .v, $(executables))

$(staging_dirs)/%.stage:
	cd $(@D); git pull

download: $(staging_targets)

# We need to set the shell options -e -o pipefail here or the exit
# code will be the exit code of the last element of the pipeline - the tee.
# We should be able to do this with .POSIX: or .SHELLFLAGS but they don't
# appear to be support by Make 3.81

%.out: $(srcdir)/%.scala $(source_files)
	set -e pipefail; $(SBT) $(SBT_FLAGS) "run $(notdir $(basename $<)) --genHarness --compile --test --backend c --vcd $(CHISEL_FLAGS)" | tee $@

%.hex: $(srcdir)/%.scala $(source_files)
	$(SBT) $(SBT_FLAGS) "run $(notdir $(basename $<)) --backend flo --genHarness --compile --test $(CHISEL_FLAGS)"

%.v: $(srcdir)/%.scala $(source_files)
	$(SBT) $(SBT_FLAGS) "run $(notdir $(basename $<)) --genHarness --backend v $(CHISEL_FLAGS)"

smoke:
	$(SBT) $(SBT_FLAGS) compile

.PHONY: all check clean cleanall emulator verilog smoke download
