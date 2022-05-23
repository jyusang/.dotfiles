{ config, pkgs, ... }:

let
  me = import ./me.nix;
in
{
  nixpkgs.config.allowUnfree = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = me.username;
  home.homeDirectory = me.homeDirectory;

  home.packages = with pkgs; [

    # Utils
    blackbox
    briss
    fd
    fzf
    jq
    qmk
    ranger
    ripgrep
    tmux
    tree

    # Editors
    neovim
    vim

    # Dev tools
    black
    clang-tools
    elixir
    elixir_ls
    gcc
    ghc
    gnumake
    go
    gopls
    haskellPackages.brittany
    haskellPackages.haskell-language-server
    nixpkgs-fmt
    nodePackages.bash-language-server
    nodePackages.prettier
    nodePackages.typescript
    nodePackages.vscode-langservers-extracted
    pyright
    python3Full
    rnix-lsp
    rubocop
    rubyPackages.solargraph
    rust-analyzer
    rustfmt
    sbcl
    shfmt

  ];

  # Workaround unknown terminal 'xterm-kitty' problem, see:
  # https://github.com/nix-community/home-manager/issues/423
  home.sessionVariables = {
    TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = "nicoulaj";
      plugins = [
        "asdf"
        "common-aliases"
        "docker"
        "docker-compose"
        "flutter"
        "fzf"
        "git"
        "helm"
        "kubectl"
        "pip"
      ];
    };
    shellAliases = {
      l = "la";
    };
  };
}
