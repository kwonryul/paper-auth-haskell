#include <dlfcn.h>
#include <iostream>
#include <memory>
#include <string>

#include "absl/strings/str_format.h"

#include <grpcpp/ext/proto_server_reflection_plugin.h>
#include <grpcpp/grpcpp.h>
#include <grpcpp/health_check_service_interface.h>

#include <HsFFI.h>

#include "oauth2_client_socket.grpc.pb.h"

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::Status;
using oauth2ClientSocket::OAuth2ClientSocket;
using oauth2ClientSocket::SocketIdWithToken;
using oauth2ClientSocket::Empty;

void *haskell_handle;

extern "C" {
  typedef char *(*SendTokenAndCloseHs)(int, const char *, const char *);
  SendTokenAndCloseHs send_token_and_close_hs;
}

class OAuth2ClientSocketServiceImpl final : public OAuth2ClientSocket::Service {
  Status SendTokenAndClose(ServerContext* context, const SocketIdWithToken* request, Empty* reply) override {
    try {
      char *res = send_token_and_close_hs((int) request->socketid(), request->accesstoken().c_str(), request->refreshtoken().c_str());
      std::string res_str(res);
      free(res);
      if (res_str == "OK")
        return Status::OK;
      else
        return grpc::Status(grpc::StatusCode::INTERNAL, res);
    } catch (const std::exception& e) {
      return grpc::Status(grpc::StatusCode::INTERNAL, e.what());
    } catch (...) {
      return grpc::Status(grpc::StatusCode::INTERNAL, "error while sending token and closing");
    }
  }
};

void RunServer(uint16_t port) {
  std::string server_address = absl::StrFormat("0.0.0.0:%d", port);
  OAuth2ClientSocketServiceImpl service;

  grpc::EnableDefaultHealthCheckService(true);
  grpc::reflection::InitProtoReflectionServerBuilderPlugin();
  ServerBuilder builder;
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);
  std::unique_ptr<Server> server(builder.BuildAndStart());
  server->Wait();
}

int length_c(const char* str) {
  int length = 0;
  while (*str != '\0') {
    length++;
    str++;
  }
  return length;
}

extern "C" {
    const char *run_oauth2_client_socket_server_c(int p, SendTokenAndCloseHs stac) {
      try {
        send_token_and_close_hs = stac;
        RunServer((uint16_t) p);
        const char *msg = "OK";
        char *cstr = (char *)malloc(sizeof(char) * (length_c(msg) + 1));
        strcpy(cstr, msg);
        return cstr;
      } catch (const std::exception& e) {
        std::string msg = e.what();
        char *cstr = (char *)malloc(sizeof(char) * (msg.length() + 1));
        strcpy(cstr, msg.c_str());
        return cstr;
      } catch (...) {
        const char *msg = "unexpected server error whiile running oauth2 client socket server";
        char *cstr = (char *)malloc(sizeof(char) * (length_c(msg) + 1));
        strcpy(cstr, msg);
        return cstr;
      }
    }
}