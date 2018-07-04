MAKEFILE := $(lastword $(MAKEFILE_LIST))
BASE_DIR := $(realpath $(dir $(MAKEFILE)))
BUILD_DIR := $(BASE_DIR)/build
XBUILD_BIN := xbuild
LIBS := Mono.Cecil.dll ICSharpCode.Decompiler.dll ICSharpCode.NRefactory.dll ICSharpCode.NRefactory.CSharp.dll

SOURCES := decompile.cs

.SUFFIXES: .cs .exe
.PHONY: all clean depends


all: decompile.exe


depends:
	$(eval csproj := ILSpy/ICSharpCode.Decompiler/ICSharpCode.Decompiler.csproj)
	$(XBUILD_BIN) /property:OutputPath=$(BUILD_DIR) $(csproj)

decompile.exe: depends
	$(eval flags := $(foreach lib,$(LIBS),-r:$(BUILD_DIR)/$(lib)))
	dmcs -out:$(BUILD_DIR)/$(patsubst %.cs,%.exe,$@) $(flags) $(SOURCES)

clean:
	test -d $(BUILD_DIR) && rm -rf $(BUILD_DIR)
