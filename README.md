# dotfiles

## Dependencies

- [stow](https://www.gnu.org/software/stow/)

## Usage

### Clone this repo

```sh
cd ~
git clone git@gitlab.com:jyusang/.dotfiles.git
```

### Link configuration files

```sh
cd ~/.dotfiles
stow tmux vim
```

### Unlink configuration files

```sh
cd ~/.dotfiles
stow -D vim
```

## References

- https://dotfiles.github.io/
- https://github.com/webpro/awesome-dotfiles
- https://alexpearce.me/2016/02/managing-dotfiles-with-stow/
