with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    ghc
    haskell-language-server
    cabal-install
    libwebp # for webp
    zlib # for JuicyPixels
  ];

  nativeBuildInputs = [
    pkg-config # for libwebp
  ];
}
