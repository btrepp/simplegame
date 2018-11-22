module Blob where




import Data.Function.Uncurried (runFn1,Fn1)
import Effect (Effect)
import Web.File.Blob (Blob)


foreign import blobUriImpl :: Fn1 Blob (Effect String)

blobUri :: Blob -> Effect String
blobUri blob = runFn1 blobUriImpl blob
