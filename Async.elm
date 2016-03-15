module Async where

type Async a = NotRequested
             | Requested
             | Finished a
             | Error String
