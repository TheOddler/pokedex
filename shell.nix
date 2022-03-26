with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-live # a live server for development
    elmPackages.elm-json # Install, upgrade and uninstall Elm dependencies
  ];
}
