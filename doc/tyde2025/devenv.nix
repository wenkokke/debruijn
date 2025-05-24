{ pkgs, ... }:

{
  packages = with pkgs; [
    pandoc
    lhs2tex
  ];

  languages = {
    haskell.enable = true;
    typst.enable = true;
    texlive = {
      enable = true;
      base = pkgs.texliveFull;
      packages = [
        "footmisc"
        "lastpage"
        "hypdoc"
        "framed"
        "latexmk"
        "stmaryrd"
        "polytable"
        "lazylist"
        "xstring"
        "totpages"
      ];
    };
  };

  }
