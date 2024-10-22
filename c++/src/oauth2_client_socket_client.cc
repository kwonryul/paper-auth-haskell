#include <iostream>
#include <memory>
#include <string>

#include <grpcpp/grpcpp.h>

#include "oauth2_client_socket.grpc.pb.h"
#include "util.h"

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;
using oauth2ClientSocket::OAuth2ClientSocket;
using oauth2ClientSocket::SocketIdWithToken;
using oauth2ClientSocket::Empty;

class OAuth2ClientSocketClient {
 public:
  OAuth2ClientSocketClient(std::shared_ptr<Channel> channel)
      : stub_(OAuth2ClientSocket::NewStub(channel)) {}

  std::string SendTokenAndClose(int port, std::int32_t socketId, const std::string& accessToken, const std::string& refreshToken) {
    SocketIdWithToken request;
    request.set_port(port);
    request.set_socketid(socketId);
    request.set_accesstoken(accessToken);
    request.set_refreshtoken(refreshToken);

    Empty reply;

    ClientContext context;

    Status status = stub_->SendTokenAndClose(&context, request, &reply);

    if (status.ok()) {
      return "OK";
    } else
      return status.error_message();
  }

 private:
  std::unique_ptr<OAuth2ClientSocket::Stub> stub_;
};

extern "C" {
  const char *send_token_and_close_c(const char *h, int p, int si, const char *at, const char *rt) {
    try {
      std::string host(h);
      std::string port = std::to_string(p);
      std::string accessToken(at);
      std::string refreshToken(rt);
      std::string target_str = host + ":" + port;
      OAuth2ClientSocketClient client(grpc::CreateChannel(target_str, grpc::InsecureChannelCredentials()));
      std::string res = client.SendTokenAndClose(p, (int32_t) si, accessToken, refreshToken);
      char *cstr = (char *)malloc(sizeof(char) * (res.length() + 1));
      strcpy(cstr, res.c_str());
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
}