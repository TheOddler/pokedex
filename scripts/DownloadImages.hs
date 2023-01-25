#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (ps: [ ps.HTTP ps.http-conduit ])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Conduit
import Network.URI (parseURI)
import System.FilePath.Posix (takeFileName)

main :: IO ()
main = do
  urlsFile <- readFile "imageurls.txt"
  let urls = lines urlsFile
  forM_ urls $ \url -> do
    putStrLn url
    body <- simpleHttp url
    B.writeFile ("images/" ++ takeFileName url) (LB.toStrict body)
