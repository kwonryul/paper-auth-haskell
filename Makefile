current_dir := $(shell pwd)
project_dir := $(shell cat ~/.paper-auth/project-directory)
current_time := $(shell date)
ghc_version := $(shell ghc --version | head -n 1 | grep -oP '\d+\.\d+\.\d+')
project_version := ${shell grep -E '^version:' paper-auth.cabal | grep -oP '\d+\.\d+\.\d+\.\d+'}

.PHONY: init
init:
	sudo -v
	echo "Initializing..."
	mkdir -p ~/.paper-auth
	echo -n "${current_dir}/" > ~/.paper-auth/project-directory
	sudo mkdir -p /root/.paper-auth
	sudo cp ~/.paper-auth/project-directory /root/.paper-auth/project-directory
	sh/install_grpc.sh

.PHONY: update_cabal
update_cabal:
	echo "Updating Cabal..."
	cat paper-auth.cabal.raw | sed "s#\$${project_dir}#${project_dir}#g" > paper-auth.cabal

.PHONY: clean_docs
clean_docs:
	echo "Cleaning Generated Docs..."
	-rm -rf generated/docs

.PHONY: clean_definitions
clean_definitions:
	echo "Cleaning Generated Definitions..."
	-rm -rf generated/snippets/definitions

.PHONY: clean_examples
clean_examples:
	echo "Cleaning Generated Examples..."
	-rm -rf generated/snippets/examples

.PHONY: cpp_make
cpp_make:
	cd c++ && make build

.PHONY: cpp_make_with_hs
cpp_make_with_hs:
	cd c++ && make build_with_hs

.PHONY: test
test: clean_examples update_cabal cpp_make
	echo "Running Tests..."
	export LD_LIBRARY_PATH=${project_dir}c++/out:${project_dir}out && cabal test

.PHONY: clean_build
clean_build:
	echo "Cleaning Build..."
	-rm -rf dist-newstyle

.PHONY: build
build: clean_build update_cabal asciidoctor
	echo "Building..."
	cabal build
	mkdir -p out
	cp dist-newstyle/build/x86_64-linux/ghc-${ghc_version}/paper-auth-${project_version}/build/libHSpaper-auth-${project_version}-inplace-ghc${ghc_version}.so out/
	cp dist-newstyle/build/x86_64-linux/ghc-${ghc_version}/paper-auth-${project_version}/build/libHSpaper-auth-${project_version}-inplace.a out/
	cp dist-newstyle/build/x86_64-linux/ghc-${ghc_version}/paper-auth-${project_version}/x/paper-auth-exe/build/paper-auth-exe/paper-auth-exe out/
	cp dist-newstyle/build/x86_64-linux/ghc-${ghc_version}/paper-auth-${project_version}/t/paper-auth-test/build/paper-auth-test/paper-auth-test out/

.PHONY: build_no_test
build_no_test: clean_build update_cabal asciidoctor_no_test cpp_make
	echo "Building..."
	cabal build
	mkdir -p out
	cp dist-newstyle/build/x86_64-linux/ghc-${ghc_version}/paper-auth-${project_version}/build/libHSpaper-auth-${project_version}-inplace-ghc${ghc_version}.so out/
	cp dist-newstyle/build/x86_64-linux/ghc-${ghc_version}/paper-auth-${project_version}/build/libHSpaper-auth-${project_version}-inplace.a out/
	cp dist-newstyle/build/x86_64-linux/ghc-${ghc_version}/paper-auth-${project_version}/x/paper-auth-exe/build/paper-auth-exe out/

#.PHONY: clean_install
#clean_install:
#	echo "Cleaning install..."
#	find ~/.cabal/store/ghc-${ghc_version}/ -name 'paper-auth-${project_version}*' -exec rm -rf {} +
#	-rm ~/.cabal/bin/paper-auth-exe

#.PHONY: install
#install: clean_install asciidoctor
#	echo "Installing..."
#	cabal install

.PHONY: build_definitions
build_definitions: clean_definitions
	echo "Building Definition Snippets..."
	~/.cabal/bin/asciidoc-gen definitions generated/snippets/definitions

.PHONY: asciidoctor_no_test
asciidoctor_no_test: clean_docs build_definitions
	echo "Replacing Template..."
	cat "templates/html5/document-raw.html.slim" | sed "s/\$${current_time}/${current_time}/g" > "templates/html5/document.html.slim"
	echo "Building generated files..."
	asciidoctor -R generated/snippets/definitions -D generated/docs/definitions 'generated/snippets/definitions/**/*.adoc' -T templates -E slim
	echo "Generating index.html..."
	asciidoctor --attribute examplesDir=../generated/snippets/examples/ --attribute definitionsDir=/docs/definitions/ -R docs -D generated/docs 'docs/index.adoc' -T templates -E slim
	-rm templates/html5/document.html.slim

.PHONY: asciidoctor
asciidoctor: test
	make asciidoctor_no_test

.PHONY: run
run:
	sudo -v
	echo "Running..."
	sudo bash -c "export LD_LIBRARY_PATH=${project_dir}c++/out:${project_dir}out && out/paper-auth-exe"

.PHONY: rund
rund:
	sudo -v
	echo "Running Daemon..."
	sudo pkill paper-auth-exe
	sudo bash -c "export LD_LIBRARY_PATH=${project_dir}c++/out:${project_dir}out && nohup out/paper-auth-exe &"

.PHONY: killd
killd:
	sudo -v
	echo "Killing Daemon..."
	sudo pkill paper-auth-exe