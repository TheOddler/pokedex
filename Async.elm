module Async where

type Async a = Waiting String
             | Finished a
             | Error String
