#include <iostream>
#include <memory>
#include <string>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"

#include <grpcpp/grpcpp.h>

#include "grpc/oauth2_client_socket.grpc.pb.h"

ABSL_FLAG(std::string, target, "localhost:9895", "Server address");

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

  std::string SendTokenAndClose(std::int32_t socketId, const std::string& accessToken, const std::string& refreshToken) {
    SocketIdWithToken request;
    request.set_socketid(socketId);
    request.set_accesstoken(accessToken);
    request.set_refreshtoken(refreshToken);

    Empty reply;

    ClientContext context;

    Status status = stub_->SendTokenAndClose(&context, request, &reply);

    if (status.ok()) {
      return "OK";
    } else
      return "FAILED";
  }

 private:
  std::unique_ptr<OAuth2ClientSocket::Stub> stub_;
};

extern "C" {
  const char *send_token_and_close_c(const char *h, int p, int si, const char *at, const char *rt) {
    std::string target_str = absl::GetFlag(FLAGS_target);
    OAuth2ClientSocketClient client(
        grpc::CreateChannel(target_str, grpc::InsecureChannelCredentials()));
    std::string accessToken(at);
    std::string refreshToken(rt);
    std::string res = client.SendTokenAndClose((int32_t) si, accessToken, refreshToken);
    char *ret = (char *)malloc(sizeof(char) * (res.length() + 1));
    strcpy(ret, res.c_str());
    return ret;
  }
}
