.PHONY: init build install test asciidoctor build_definitions asciidoctor_no_test

current_dir := $(shell pwd)

init:
	echo "Setting project-directory..."
	mkdir -p ~/.paper-auth
	echo -n "${current_dir}/" > ~/.paper-auth/project-directory
	sudo mkdir -p /root/.paper-auth
	sudo cp ~/.paper-auth/project-directory /root/.paper-auth/project-directory

build: asciidoctor
	echo "Building..."
	cabal build

install: asciidoctor
	echo "Installing..."
	cabal install --overwrite-policy=always

asciidoctor: test build_definitions
	echo "Generating HTML files..."
	asciidoctor --attribute examplesDir=../generated/snippets/examples/ --attribute definitionsDir=../generated/snippets/definitions/ -R docs -D generated/docs 'docs/**/*.adoc'
	echo "Building generated files..."
	asciidoctor -R generated/snippets/definitions -D generated/docs 'generated/snippets/definitions/**/*.adoc'

test:
	echo "Running tests..."
	cabal test

build_definitions:
	echo "Building definition snippets..."
	~/.cabal/bin/asciidoc-gen ${current_dir}/definitions ${current_dir}/generated/snippets/definitions

asciidoctor_no_test: build_definitions
	echo "Generating HTML files..."
	asciidoctor --attribute examplesDir=../generated/snippets/examples/ --attribute definitionsDir=../generated/snippets/definitions/ -R docs -D generated/docs 'docs/**/*.adoc'
	echo "Building generated files..."
	asciidoctor -R generated/snippets/definitions -D generated/docs 'generated/snippets/definitions/**/*.adoc'