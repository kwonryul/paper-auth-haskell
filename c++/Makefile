project_dir := $(shell cat ~/.paper-auth/project-directory)

PROTOBUF_ABSL_DEPS = absl_absl_check absl_absl_log absl_algorithm absl_base absl_bind_front absl_bits absl_btree absl_cleanup absl_cord absl_core_headers absl_debugging absl_die_if_null absl_dynamic_annotations absl_flags absl_flat_hash_map absl_flat_hash_set absl_function_ref absl_hash absl_layout absl_log_initialize absl_log_severity absl_memory absl_node_hash_map absl_node_hash_set absl_optional absl_span absl_status absl_statusor absl_strings absl_synchronization absl_time absl_type_traits absl_utility absl_variant
PROTOBUF_UTF8_RANGE_LINK_LIBS = -lutf8_validity

CXX = g++
CPPFLAGS = -L$(project_dir)grpc/lib `export PKG_CONFIG_PATH=$(project_dir)grpc/lib/pkgconfig; pkg-config --cflags protobuf grpc absl_flags absl_flags_parse`
LDFLAGS = -L$(project_dir)grpc/lib `export PKG_CONFIG_PATH=$(project_dir)grpc/lib/pkgconfig; pkg-config --libs --static protobuf grpc++ absl_flags absl_flags_parse $(PROTO_BUF_ABSL_DEPTS)` $(PROTOBUF_UTF8_RANGE_LINK_LIBS) -pthread -Wl,--no-as-needed -lgrpc++_reflection -Wl,--as-needed -ldl
INCLUDEFLAGS = -Iinclude -Iinclude/ghc -Iinclude/grpc -I../grpc/include

.PHONY: clean
clean:
	rm -rf build
	rm -rf out

build/grpc/oauth2_client_socket.grpc.pb.o: src/grpc/oauth2_client_socket.grpc.pb.cc
	mkdir -p build/grpc
	$(CXX) $(CPPFLAGS) -c -fPIC $(INCLUDEFLAGS) $< -o $@

build/grpc/oauth2_client_socket.pb.o: src/grpc/oauth2_client_socket.pb.cc
	mkdir -p build/grpc
	$(CXX) $(CPPFLAGS) -c -fPIC $(INCLUDEFLAGS) $< -o $@

build/util.o: src/util.cc
	mkdir -p build
	$(CXX) $(CPPFLAGS) -c -fPIC $(INCLUDEFLAGS) $< -o $@

build/oauth2_client_socket_client.o: src/oauth2_client_socket_client.cc
	mkdir -p build
	$(CXX) $(CPPFLAGS) -c -fPIC $(INCLUDEFLAGS) $< -o $@

build/oauth2_client_socket_server.o: src/oauth2_client_socket_server.cc
	mkdir -p build
	$(CXX) $(CPPFLAGS) -c -fPIC $(INCLUDEFLAGS) $< -o $@

out/liboauth2_client_socket.so: build/util.o build/oauth2_client_socket_client.o build/oauth2_client_socket_server.o build/grpc/oauth2_client_socket.grpc.pb.o build/grpc/oauth2_client_socket.pb.o
	mkdir -p out
	$(CXX) $(CPPFLAGS) -shared -fPIC $^ -o $@ $(LDFLAGS)

.PHONY: build
build:
	echo "Building CPP..."
	make clean
	make out/liboauth2_client_socket.so

.PHONY: huxa
huxa:
	$(CXX) $(CPPFLAGS) -o huxa -fPIC $(INCLUDEFLAGS) src/cla.cc build/grpc/oauth2_client_socket.pb.o build/grpc/oauth2_client_socket.grpc.pb.o $(LDFLAGS)