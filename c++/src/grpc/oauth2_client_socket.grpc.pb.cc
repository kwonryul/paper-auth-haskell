// Generated by the gRPC C++ plugin.
// If you make any local change, they will be lost.
// source: oauth2_client_socket.proto

#include "grpc/oauth2_client_socket.pb.h"
#include "grpc/oauth2_client_socket.grpc.pb.h"

#include <functional>
#include <grpcpp/support/async_stream.h>
#include <grpcpp/support/async_unary_call.h>
#include <grpcpp/impl/channel_interface.h>
#include <grpcpp/impl/client_unary_call.h>
#include <grpcpp/support/client_callback.h>
#include <grpcpp/support/message_allocator.h>
#include <grpcpp/support/method_handler.h>
#include <grpcpp/impl/rpc_service_method.h>
#include <grpcpp/support/server_callback.h>
#include <grpcpp/impl/server_callback_handlers.h>
#include <grpcpp/server_context.h>
#include <grpcpp/impl/service_type.h>
#include <grpcpp/support/sync_stream.h>
namespace oauth2ClientSocket {

static const char* OAuth2ClientSocket_method_names[] = {
  "/oauth2ClientSocket.OAuth2ClientSocket/SendTokenAndClose",
};

std::unique_ptr< OAuth2ClientSocket::Stub> OAuth2ClientSocket::NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options) {
  (void)options;
  std::unique_ptr< OAuth2ClientSocket::Stub> stub(new OAuth2ClientSocket::Stub(channel, options));
  return stub;
}

OAuth2ClientSocket::Stub::Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options)
  : channel_(channel), rpcmethod_SendTokenAndClose_(OAuth2ClientSocket_method_names[0], options.suffix_for_stats(),::grpc::internal::RpcMethod::NORMAL_RPC, channel)
  {}

::grpc::Status OAuth2ClientSocket::Stub::SendTokenAndClose(::grpc::ClientContext* context, const ::oauth2ClientSocket::SocketIdWithToken& request, ::oauth2ClientSocket::Empty* response) {
  return ::grpc::internal::BlockingUnaryCall< ::oauth2ClientSocket::SocketIdWithToken, ::oauth2ClientSocket::Empty, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(channel_.get(), rpcmethod_SendTokenAndClose_, context, request, response);
}

void OAuth2ClientSocket::Stub::async::SendTokenAndClose(::grpc::ClientContext* context, const ::oauth2ClientSocket::SocketIdWithToken* request, ::oauth2ClientSocket::Empty* response, std::function<void(::grpc::Status)> f) {
  ::grpc::internal::CallbackUnaryCall< ::oauth2ClientSocket::SocketIdWithToken, ::oauth2ClientSocket::Empty, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(stub_->channel_.get(), stub_->rpcmethod_SendTokenAndClose_, context, request, response, std::move(f));
}

void OAuth2ClientSocket::Stub::async::SendTokenAndClose(::grpc::ClientContext* context, const ::oauth2ClientSocket::SocketIdWithToken* request, ::oauth2ClientSocket::Empty* response, ::grpc::ClientUnaryReactor* reactor) {
  ::grpc::internal::ClientCallbackUnaryFactory::Create< ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(stub_->channel_.get(), stub_->rpcmethod_SendTokenAndClose_, context, request, response, reactor);
}

::grpc::ClientAsyncResponseReader< ::oauth2ClientSocket::Empty>* OAuth2ClientSocket::Stub::PrepareAsyncSendTokenAndCloseRaw(::grpc::ClientContext* context, const ::oauth2ClientSocket::SocketIdWithToken& request, ::grpc::CompletionQueue* cq) {
  return ::grpc::internal::ClientAsyncResponseReaderHelper::Create< ::oauth2ClientSocket::Empty, ::oauth2ClientSocket::SocketIdWithToken, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(channel_.get(), cq, rpcmethod_SendTokenAndClose_, context, request);
}

::grpc::ClientAsyncResponseReader< ::oauth2ClientSocket::Empty>* OAuth2ClientSocket::Stub::AsyncSendTokenAndCloseRaw(::grpc::ClientContext* context, const ::oauth2ClientSocket::SocketIdWithToken& request, ::grpc::CompletionQueue* cq) {
  auto* result =
    this->PrepareAsyncSendTokenAndCloseRaw(context, request, cq);
  result->StartCall();
  return result;
}

OAuth2ClientSocket::Service::Service() {
  AddMethod(new ::grpc::internal::RpcServiceMethod(
      OAuth2ClientSocket_method_names[0],
      ::grpc::internal::RpcMethod::NORMAL_RPC,
      new ::grpc::internal::RpcMethodHandler< OAuth2ClientSocket::Service, ::oauth2ClientSocket::SocketIdWithToken, ::oauth2ClientSocket::Empty, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(
          [](OAuth2ClientSocket::Service* service,
             ::grpc::ServerContext* ctx,
             const ::oauth2ClientSocket::SocketIdWithToken* req,
             ::oauth2ClientSocket::Empty* resp) {
               return service->SendTokenAndClose(ctx, req, resp);
             }, this)));
}

OAuth2ClientSocket::Service::~Service() {
}

::grpc::Status OAuth2ClientSocket::Service::SendTokenAndClose(::grpc::ServerContext* context, const ::oauth2ClientSocket::SocketIdWithToken* request, ::oauth2ClientSocket::Empty* response) {
  (void) context;
  (void) request;
  (void) response;
  return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}


}  // namespace oauth2ClientSocket
