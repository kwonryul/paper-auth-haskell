#!/bin/bash

project_dir=$(<"~/.paper-auth/project-directory")
install_dir=($project_dir)grpc

sudo apt install -y
if [ ! -d $install_dir ]; then
    echo "Createing Directory..."
    mkdir -p $install_dir
    mkdir -p temp
    pushd temp
    echo "Cloning Grpc Repo..."
    git clone --recurse-submodules --depth 1 --shallow-submodules https://github.com/grpc/grpc
    pushd grpc
    mkdir -p cmake/build
    pushd cmake/build
    cmake -DgRPC_INSTALL=ON \
        -DgRPC_BUILD_TEStSS=OFF \
        -DCMAKE_INSTALL_PREFIX=$install_dir \
        ../..
    make -j 4
    make install
    popd
    popd
    popd
    rm -rf temp
else
    echo "Directory $install_dir already exists. No action taken."
fi