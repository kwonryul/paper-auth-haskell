define read_config
$(shell grep "^$1" makefile.cfg | cut -d'=' -f2)
endef

DOCS_DIR := $(call read_config,docsDir)

.PHONY: build install test asciidoctor

build: asciidoctor
	echo "Building..."
	cabal build

install: asciidoctor
	echo "Installing..."
	cabal install --overwrite-policy=always

asciidoctor: test
	echo "Generating HTML files..."
	asciidoctor -R ./docs -D $(DOCS_DIR) '**/*.adoc'

test:
	echo "Running tests..."
	cabal test