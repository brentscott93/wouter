{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      dbal = pkgs.rPackages.buildRPackage {
        name = "dbalance";
        src = pkgs.fetchFromGitHub{
          owner = "brentscott93";
          repo = "dbalance";
          rev = "0bd74b6c8e86235e697d4484638b89df4ba8f28c";
          sha256 = "01pa8mjyy3jlri9ag5yl1k7dc7xksa8zd71mqh5sb18644mmfnj7";
        };
         propagatedBuildInputs = with pkgs.rPackages; [dplyr purrr magrittr tibble stringr cowplot ggplot2];
     };
      rPkgs = with pkgs.rPackages; [shiny 
                                tidyverse 
                                cowplot 
                                rhandsontable 
                                RColorBrewer
                                DT
                                ggpubr
                                rsconnect ];

      rstudio = pkgs.rstudioWrapper.override{ packages = [rPkgs dbal]; };
                                
    in {
      devShells.default = pkgs.mkShell {
        packages = [ pkgs.bashInteractive 
                     pkgs.R
                     rPkgs
                     dbal
                     rstudio ];
      };
    });
}
