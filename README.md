# My dotfiles

The files are structured as [`stow`](https://www.gnu.org/software/stow/) packages.

## Usage

- Install [homebrew](https://brew.sh).

- Clone this repository in the home directory, and cd to it.

- Run `brew bundle` to install common packages, including `stow`.

- Run `stow <package>` in `.dotfiles` to install a config package.

- Optionally run `brew bundle --file <package>/Brewfile` or other setup scripts.

## Note about Java

Homebrew defaults to use openjdk for Java and packages that depend on it.
That version of Java does not receive security updates for long, even LTS versions.

This is where the `sdkman` package comes in.

## Package specific instructions

### bat

Install the `bat` package and stow the configuration:

```
brew bundle --file bat/Brewfile
stow bat
```

### clojure

Though `leiningen` is available on homebrew, it drags in a JVM.
Install it using `sdkman` (described below) via:

```
sdk install leiningen
stow clojure
```

### coursier

Install it manually, as per instructions on [get-coursier.io](https://get-coursier.io/docs/cli-installation).

```
curl -fL "https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-pc-linux.gz" | gzip -d > cs
sudo mv cs /urs/local/bin
sudo chmod +x /usr/local/bin/cs
stow coursier
```

Afterwards, the `metals` LSP tool (used by emacs Scala development) can be installed:

```
cs install metals
```

Coursier is not used for installing `sbt` or `scala`.
This is done by `sdkman` (described below).

### emacs

Install using homebrew and stow it:

```
brew bundle --file emacs/Brewfile
stow emacs
```

### sdkman

Install it manually, with the option to not modify the shell config:

```
curl -s "https://get.sdkman.io?rcupdate=false" | bash
stow sdkman
```

Next install a suitable JDK, like Eclipse Adoptium Temurin:

```
sdk install java 17.0.12-tem
```

It can also be used to install `sbt` and `leiningen`, instead of via homebrew.

### tmux

Install using homebrew and stow it:

```
brew bundle --file tmux/Brewfile
stow tmux
```

For macOS Terminal to always start and continue in tmux, use the following as "Shells open with:" in Terminal's General preferences.

```
/opt/homebrew/bin/tmux new-session -A
```

### zsh

This only concerns a config, so just stow it:

```
stow zsh
```

For a Terminal with correct settings and Dracula theme, import the `zsh/Dracula.terminal` file in macOS Terminal.
