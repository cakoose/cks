# -----------------------------------------------------------------------
# Compile:                    make compile
# Build JARs:                 make jars
# Javadoc documentation:      make javadoc
# Javadoc, link standard API: make javadoclink

# Need 'javac' version 5 or greater.
JavaC ?= javac

# Need 'javacc' version 4 or greater.
JavaCc ?= javacc

JFlex ?= jflex

# -----------------------------------------------------------------------

ThunkExpr = $$(eval $(1) = $$($(1)_Gen))$$($(1))
DefThunk = $(eval $(1) = $(call ThunkExpr,$(1)))

# On Unix, ":" (colon), on Windows, ";" (semicolon)
PathSep = $(eval PathSep := $(PathSep_Gen))$(PathSep)
PathSep_Gen = $(shell if expr "$(uname -s)" : 'CYGWIN' >/dev/null ; then printf ';' ; else printf ':' ; fi)

include Config.mk

JdkVersion ?= 1.5

Output = Output
Source = Source
Libs   = Libraries
Bytecode  = $(Output)/Bytecode
Stamps    = $(Output)/Stamps
Generated = $(Output)/Generated
Javadoc   = $(Output)/Javadoc
JavadocLink = $(Output)/JavadocLink

Components_FN := $(subst /,-,$(Components))
Components_ID := $(subst /,_,$(Components))

JavaC_Opt := -source $(JdkVersion) -target $(JdkVersion)

Empty :=
Space := $(Empty) $(Empty)

p1 := @
p2 := @
ifeq ($(SHOW),y)
	p1 :=
endif
ifeq ($(SHOW),a)
	p1 :=
	p2 :=
endif

DepDirs = $(patsubst %,$(Bytecode)/%,$(subst /,-,$(1)))
LibJars = $(patsubst %,$(Libs)/%.jar,$(subst /,-,$(1)))
MkClasspath = $$(subst $$(Space),$(PathSep),$$(call DepDirs,$$(DEP_$(1))) $$(call LibJars,$$(LIB_$(1))))$(PathSep)
MkStampDeps = $$(patsubst %,$(Stamps)/%.$(2),$$(subst /,-,$$(DEP_$(1))))
MkJavadocLinks = $$(patsubst %,-link ../%,$$(subst /,-,$$(DEP_$(1))))

.PHONY: default jars javadoc javadoclink compile

default: compile
all: compile javadoc jars

compile: $(Components_FN:%=$(Stamps)/%.compile)
jars: $(Components_FN:%=$(Output)/$(JarPrefix)%.jar)
javadoc: $(Components_FN:%=$(Stamps)/%.javadoc)
javadoclink: $(Components_FN:%=$(Stamps)/%.javadoclink)

# $(1): path, $(2): filename, $(3): varname
define MkModule
$(2): $(Stamps)/$(2).compile

JavaCcSources_$(3) = $$(shell find $(Source)/$(1) -name '*.jj')
JavaCcGeneratedDirs_$(3) = $$(JavaCcSources_$(3):$(Source)/$(1)/%.jj=$(Generated)/$(1)/%.jj)
JFlexSources_$(3) = $$(shell find $(Source)/$(1) -name '*.jflex')
JFlexGeneratedDirs_$(3) = $$(JFlexSources_$(3):$(Source)/$(1)/%.jflex=$(Generated)/$(1)/%.jflex)

$(Generated)/$(1)/%.jj/: $(Source)/$(1)/%.jj
	# JavaCC: $(1): $$(patsubst $(Source)/$(1)/%,%,$$<)
	$(p2)[ ! -e $$@ ] || rm -r $$@
	$(p2)mkdir -p $$@
	$(p1)./Support/javacc-silencer $$@/javacc.out $$@/javacc.err $(JavaCc) \
		-JDK_VERSION=$(JdkVersion) -OUTPUT_DIRECTORY=$$@ $$< \
		|| (rm -r $$@ && false)

$(Generated)/$(1)/%.jflex/: $(Source)/$(1)/%.jflex
	# JFlex: $(1): $$(patsubst $(Source)/$(1)/%,%,$$<)
	$(p2)[ ! -e $$@ ] || rm -r $$@
	$(p2)mkdir -p $$@
	$(p1)$(JFlex) --quiet -d $$@ $$< || (rm -r $$@ && false)

JavaSourceRoots_$(3) = $(Source)/$(1) $$(JavaCcGeneratedDirs_$(3)) $$(JFlexGeneratedDirs_$(3))
JavaSources_$(3) = $$(shell find $(Source)/$(1) -name '*.java' -type f)
AllJavaInputs_$(3) = $$(shell find $$(JavaSourceRoots_$(3)) -name '*.java' -type f)

# Compile to bytecode
$(Stamps)/$(2).compile: $$(JavaSourceRoots_$(3)) $$(JavaSources_$(3)) $$(JavaSourceDirs_$(3)) $(call MkStampDeps,$(3),compile) $$(call LibJars,$$(LIB_$(3)))
	$(p2)[ ! -e $(Bytecode)/$(2) ] || rm -r $(Bytecode)/$(2)
	$(p2)mkdir -p $(Bytecode)/$(2)
ifeq ($$(JavaSources_$(3))$$(JavaCcSources_$(3)),)
	$(p1)# Compile: $(1) (empty)
else
	# Compile: $(1)
	$(p1)$(JavaC) $(JavaC_Opt) $$(JavaC_Opt_$(3)) -d $(Bytecode)/$(2) \
		-classpath $(call MkClasspath,$(3)) \
		$$(AllJavaInputs_$(3))
endif
	$(p2)mkdir -p $(Stamps)
	$(p2)touch $$@

# Make Jar
$(Output)/$(JarPrefix)$(2).jar: $(Stamps)/$(2).compile
	# Jar: $(JarPrefix)$(2).jar
	$(p1)jar 0cf $$@ -C $(Bytecode)/$(2) .

# Make Javadoc
$(Stamps)/$(2).javadoc: $(Stamps)/$(2).compile $(call MkStampDeps,$(3),javadoc)
	$(p2)[ ! -e $(Javadoc)/$(2) ] || rm -r $(Javadoc)/$(2)
	$(p2)mkdir -p $(Javadoc)/$(2)
ifeq ($$(JavaSources_$(3))$$(JavaCcSources_$(3)),)
	$(p1)# Javadoc: $(1) (empty)
	$(p2)touch $(Javadoc)/$(2)/package-list
else
	# Javadoc: $(1)
	$(p1)javadoc -quiet \
		-d $(Javadoc)/$(2) \
		-windowtitle "Stoop API - $(2)" \
		-doctitle "Stoop API - $(2)" \
		-classpath $(call MkClasspath,$(3)) \
		$(call MkJavadocLinks,$(3)) \
		$$(AllJavaInputs_$(3))
endif
	$(p2)touch $$@

# Make JavadocLink
$(Stamps)/$(2).javadoclink: $(Stamps)/$(2).compile
	$(p2)[ ! -e $(JavadocLink)/$(2) ] || rm -r $(JavadocLink)/$(2)
	$(p2)mkdir -p $(JavadocLink)/$(2)
ifeq ($$(JavaSources_$(3))$$(JavaCcSources_$(3)),)
	$(p1)# JavadocLink: $(1) (empty)
	$(p2)touch $(JavadocLink)/$(2)/package-list
else
	# JavadocLink: $(1)
	$(p1)javadoc -quiet \
		-d $(JavadocLink)/$(2) \
		-windowtitle "Stoop API - $(2)" \
		-doctitle "Stoop API - $(2)" \
		-classpath $(call MkClasspath,$(3)) \
		$(call MkJavadocLinks,$(3)) \
		-link http://java.sun.com/j2se/1.5/docs/api \
		$$(AllJavaInputs_$(3))
endif
	$(p2)touch $$@

endef

$(foreach c,$(Components),$(eval $(call MkModule,$(c),$(subst /,-,$(c)),$(subst /,_,$(c)))))

clean:
ifneq ($(wildcard $(Output)),)
	# Deleting $(Output)/**
	-rm -r $(Output)
endif
