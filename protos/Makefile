.PHONY: build
build:
	-rm -rf build
	mkdir build
	../grpc/bin/protoc -I . --grpc_out=build --plugin=protoc-gen-grpc=../grpc/bin/grpc_cpp_plugin oauth2_client_socket.proto
	../grpc/bin/protoc -I . --cpp_out=build oauth2_client_socket.proto