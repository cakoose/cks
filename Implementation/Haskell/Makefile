program := cks-test
src_dir := Source
out_dir := Output
obj_dir := $(out_dir)/Object
gen_c_dir := $(out_dir)/GeneratedC
interface_dir := $(out_dir)/Interface
include_dirs := $(src_dir)

p1 := @
p2 := @
ifeq ($(SHOW),y)
	p1 :=
	p2 := @
endif
ifeq ($(SHOW),a)
	p1 :=
	p2 :=
endif

# The way to keep ".hs.c" files around is "make genc=y".
# This places a ".hs.c" file next to each ".hs" source file.  I don't want
# it to do this, but I can't figure out how to avoid it.
genc ?= n
ifeq ($(genc),y)
	ghc_opts += -keep-hc-file
endif

ghc_opts += -W -fwarn-incomplete-patterns

default:
	# Compile with GHC
	$(p2)mkdir -p $(out_dir) $(obj_dir) $(interface_dir)
	$(p1)ghc \
		--make \
		$(include_dirs:%=-i%) \
		-odir $(obj_dir) \
		-hidir $(interface_dir) \
		$(ghc_opts) \
		-hcsuf hs.c \
		$(src_dir)/Data/Cks/*.hs

clean:
	rm -rf $(out_dir)
