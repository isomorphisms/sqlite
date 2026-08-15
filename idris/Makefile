IDRIS2 ?= idris2
BUILD_DIR ?= build
OUTPUT_DIR ?= build/exec

.PHONY: build check test

build:
	$(IDRIS2) --build-dir $(BUILD_DIR) --output-dir $(OUTPUT_DIR) --build sqlite-first-pass.ipkg

check:
	$(IDRIS2) --typecheck sqlite-first-pass.ipkg
	$(IDRIS2) --typecheck tests.ipkg

test:
	$(IDRIS2) --build-dir $(BUILD_DIR) --output-dir $(OUTPUT_DIR) --build tests.ipkg
	$(OUTPUT_DIR)/sqlite-first-pass-tests
