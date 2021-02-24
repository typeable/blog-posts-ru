{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "efc6d923c633207d18bd4d8cae3e20110a377864";
    sha256 = "121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5";
    })
}:
(import reflex-platform {}).project ({ pkgs, ... }:{
  useWarp = true;

  packages = {
    todo-common = ./todo-common;
    todo-server = ./todo-server;
    todo-client = ./todo-client;
  };

  shells = {
    ghc = ["todo-common" "todo-server" "todo-client"];
    ghcjs = ["todo-common" "todo-client"];
  };
})
