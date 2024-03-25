#include <dlfcn.h>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <mutex>

#include "absl/strings/str_format.h"

#include <grpcpp/ext/proto_server_reflection_plugin.h>
#include <grpcpp/grpcpp.h>
#include <grpcpp/health_check_service_interface.h>

#include "oauth2_client_socket.grpc.pb.h"
#include "util.h"
#include "thread_safe_map.h"
#include "one_shot.h"

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::Status;
using oauth2ClientSocket::OAuth2ClientSocket;
using oauth2ClientSocket::ClientSocketMessage;
using oauth2ClientSocket::ServerSocketMessage;
using oauth2ClientSocket::Tokens;
using oauth2ClientSocket::SocketIdWithToken;
using oauth2ClientSocket::Empty;

extern "C" {
  typedef int (*SaveConnectionHs)(void *, void *);
  typedef char *(*CleanConnectionHs)(int);
  typedef char *(*SendTokenAndCloseHs)(int, const char *, const char *);
}

ThreadSafeMap<int, SaveConnectionHs> save_connection_hs_map;
ThreadSafeMap<int, CleanConnectionHs> clean_connection_hs_map;
ThreadSafeMap<int, SendTokenAndCloseHs> send_token_and_close_hs_map;

class OAuth2ClientSocketServiceImpl final : public OAuth2ClientSocket::Service {
  Status NewConnection(ServerContext* context, grpc::ServerReaderWriter<ServerSocketMessage, ClientSocketMessage> *stream) override {
    try {
      OneShot<bool> oneShot;

      std::thread t([stream, &oneShot]() {
        ClientSocketMessage clientSocketMessage;
        while (stream->Read(&clientSocketMessage)) {}
        oneShot.SetValue(false);
      });

/*
      const std::string& peer = context->;
    
      std::size_t portStart = peer.find_last_of(':');
      int port;
      if (portStart != std::string::npos) {
        std::string portStr = peer.substr(portStart + 1);
        port = std::stoi(portStr);
      } else {
        return Status(grpc::StatusCode::INTERNAL, "connection metadata invalid");
      }*/

      SaveConnectionHs save_connection_hs = save_connection_hs_map.get(9895);
      CleanConnectionHs clean_connection_hs = clean_connection_hs_map.get(9895);
      int socketId = save_connection_hs(stream, &oneShot);
      if (oneShot.GetValue()) {
        printf("this should be\n");
        char *res = clean_connection_hs(socketId);
        printf("should should\n");
        std::string res_str(res);
        free(res);
        if (res_str == "OK")
          return Status::OK;
        else
          return Status(grpc::StatusCode::INTERNAL, res_str);
      } else {
        printf("this must be\n");
        return Status(grpc::StatusCode::INTERNAL, "socket closed early");
      }
    } catch (const std::exception& e) {
      return Status(grpc::StatusCode::INTERNAL, std::string(e.what()));
    } catch (...) {
      return Status(grpc::StatusCode::INTERNAL, "error while new connection");
    }
  }

  Status SendTokenAndClose(ServerContext* context, const SocketIdWithToken* request, Empty* reply) override {
    try {
      SendTokenAndCloseHs send_token_and_close_hs = send_token_and_close_hs_map.get(request->port());
      char *res = send_token_and_close_hs((int) request->socketid(), request->accesstoken().c_str(), request->refreshtoken().c_str());
      std::string res_str(res);
      free(res);
      if (res_str == "OK")
        return Status::OK;
      else
        return Status(grpc::StatusCode::INTERNAL, res_str);
    } catch (const std::exception& e) {
      return Status(grpc::StatusCode::INTERNAL, std::string(e.what()));
    } catch (...) {
      return Status(grpc::StatusCode::INTERNAL, "error while sending token and closing");
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

extern "C" {
  const char *run_oauth2_client_socket_server_c(int p, SaveConnectionHs sc, CleanConnectionHs cc, SendTokenAndCloseHs stac) {
    try {
      save_connection_hs_map.insert(p, sc);
      clean_connection_hs_map.insert(p, cc);
      send_token_and_close_hs_map.insert(p, stac);
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

  const char *send_state_native_c(void *streamPtr, char *state) {
    try {
      grpc::ServerReaderWriter<ServerSocketMessage, ClientSocketMessage> *stream = (grpc::ServerReaderWriter<ServerSocketMessage, ClientSocketMessage> *)streamPtr;
      ServerSocketMessage serverSocketMessage;
      serverSocketMessage.set_state(state);
      stream->Write(serverSocketMessage);
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
      const char *msg = "unexpected server error whiile running send_state_native_c";
      char *cstr = (char *)malloc(sizeof(char) * (length_c(msg) + 1));
      strcpy(cstr, msg);
      return cstr;
    }
  }

  const char *send_token_and_close_native_c(void *streamPtr, void *oneShotPtr, const char *accessToken, const char *refreshToken, int socketId) {
    try {
      Tokens tokens;
      tokens.set_accesstoken(accessToken);
      tokens.set_refreshtoken(refreshToken);
      ServerSocketMessage serverSocketMessage;
      serverSocketMessage.set_allocated_tokens(&tokens);
      grpc::ServerReaderWriter<ServerSocketMessage, ClientSocketMessage> *stream = (grpc::ServerReaderWriter<ServerSocketMessage, ClientSocketMessage> *)streamPtr;
      stream->Write(serverSocketMessage);
      OneShot<bool> *oneShot = (OneShot<bool> *)oneShotPtr;
      oneShot->SetValue(true);
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
      const char *msg = "unexpected server error whiile send_token_and_close_native_c";
      char *cstr = (char *)malloc(sizeof(char) * (length_c(msg) + 1));
      strcpy(cstr, msg);
      return cstr;
    }
  }
}