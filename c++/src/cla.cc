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
using oauth2ClientSocket::ClientSocketMessage;
using oauth2ClientSocket:: ServerSocketMessage;
using oauth2ClientSocket::Tokens;
using oauth2ClientSocket::SocketIdWithToken;
using oauth2ClientSocket::Empty;

class OAuth2ClientSocketClient {
 public:
  OAuth2ClientSocketClient(std::shared_ptr<Channel> channel)
      : stub_(OAuth2ClientSocket::NewStub(channel)) {}

 void NewConnection() {
    ClientContext context;
    std::shared_ptr<grpc::ClientReaderWriter<ClientSocketMessage, ServerSocketMessage>> stream(
        stub_->NewConnection(&context)
    );
    ServerSocketMessage serverSocketMessage;
    if (stream->Read(&serverSocketMessage)) {
        printf("s:\t%s\n", serverSocketMessage.state().c_str());
    }  else {
        printf("error\n");
        return;
    }
    if (stream->Read(&serverSocketMessage)) {
        printf("a:\t%s\n", serverSocketMessage.tokens().accesstoken().c_str());
        printf("r:\t%s\n", serverSocketMessage.tokens().refreshtoken().c_str());
    } else {
        printf("error\n");
        return;
    }
  }

 private:
  std::unique_ptr<OAuth2ClientSocket::Stub> stub_;
};

int main() {
    OAuth2ClientSocketClient client(grpc::CreateChannel(
        "localhost:9895", grpc::InsecureChannelCredentials()));
    client.NewConnection();

    return 0;
}