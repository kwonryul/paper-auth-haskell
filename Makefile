.PHONY: build install test asciidoctor

build: asciidoctor
	echo "Building..."
	cabal build

install: asciidoctor
	echo "Installing..."
	cabal install --overwrite-policy=always

asciidoctor: test
	echo "Generating HTML files..."
	asciidoctor --attribute snippetDir=generated/snippets -R docs -D generated/docs 'docs/**/*.adoc'

test:
	echo "Running tests..."
	cabal test