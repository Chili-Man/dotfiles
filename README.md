# dotfiles
Dot files

# Set Up
Requires the use of GNU stow to properly place the files.
We must also have a `environment.sh` file that is customized with the following
variables:
```
# ${HOME}/environment.sh
```

# Run it

```
cd "${HOME}/dotfiles/"

stow bash
stow emacs
```
