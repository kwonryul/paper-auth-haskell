module CallStack(
    CallStackI(
        callStack'
      )
) where

import Monad.ProfileT

import Data.Proxy
import GHC.Stack

class Profile p => CallStackI p where
    callStack' :: HasCallStack => Proxy p -> CallStack
    callStack' = callStack'Impl


callStack'Impl :: (HasCallStack, CallStackI p) => Proxy p -> CallStack
callStack'Impl _ = callStack