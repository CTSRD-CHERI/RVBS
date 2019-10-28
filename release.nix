{ nixpkgs-path, ctsrd-pkgs-path }:
let nixpkgs    = import nixpkgs-path    { config.allowUnfree = true; };
    ctsrd-pkgs = import ctsrd-pkgs-path { inherit nixpkgs; };
in
  {
    rvbs-cache = ctsrd-pkgs.rvbs { cache = true; };
    rvbs-nocache = ctsrd-pkgs.rvbs { cache = false; };
}
