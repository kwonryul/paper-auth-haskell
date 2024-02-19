module CallStack(
    callStack'
) where

import GHC.Stack

callStack' :: HasCallStack => CallStack
callStack' = callStack