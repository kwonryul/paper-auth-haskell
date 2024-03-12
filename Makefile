current_dir := $(shell pwd)
current_time := $(shell date)
ghc_version := $(shell ghc --version | head -n 1 | grep -oP '\d+\.\d+\.\d+')
project_version := ${shell grep -E '^version:' paper-auth.cabal | grep -oP '\d+\.\d+\.\d+\.\d+'}

.PHONY: init
init:
	echo "Setting project-directory..."
	mkdir -p ~/.paper-auth
	echo -n "${current_dir}/" > ~/.paper-auth/project-directory
	sudo mkdir -p /root/.paper-auth
	sudo cp ~/.paper-auth/project-directory /root/.paper-auth/project-directory

.PHONY: clean_docs
clean_docs:
	echo "Cleaning generated docs..."
	-rm -rf generated/docs

.PHONY: clean_definitions
clean_definitions:
	echo "Cleaning generated definitions..."
	-rm -rf generated/snippets/definitions

.PHONY: clean_examples
clean_examples:
	echo "Cleaning generated examples..."
	-rm -rf generated/snippets/examples

.PHONY: test
test: clean_examples
	echo "Running tests..."
	cabal test

.PHONY: build
build: asciidoctor
	echo "Building..."
	cabal build

.PHONY: clean_install
clean_install:
	echo "Cleaning install..."
	find ~/.cabal/store/ghc-${ghc_version}/ -name 'paper-auth-${project_version}*' -exec rm -rf {} +
	-rm ~/.cabal/bin/paper-auth-exe

.PHONY: install
install: clean_install asciidoctor
	echo "Installing..."
	cabal install

.PHONY: build_definitions
build_definitions: clean_definitions
	echo "Building definition snippets..."
	~/.cabal/bin/asciidoc-gen ${current_dir}/definitions ${current_dir}/generated/snippets/definitions

.PHONY: asciidoctor_no_test
asciidoctor_no_test: clean_docs build_definitions
	echo "Replacing template..."
	cat "templates/html5/document-raw.html.slim" | sed "s/{current_time}/${current_time}/g" > "templates/html5/document.html.slim"
	echo "Building generated files..."
	asciidoctor -R generated/snippets/definitions -D generated/docs/definitions 'generated/snippets/definitions/**/*.adoc' -T templates -E slim
	echo "Generating index.html..."
	asciidoctor --attribute examplesDir=../generated/snippets/examples/ --attribute definitionsDir=/docs/definitions/ -R docs -D generated/docs 'docs/index.adoc' -T templates -E slim
	-rm templates/html5/document.html.slim

.PHONY: asciidoctor
asciidoctor: test
	make asciidoctor_no_test