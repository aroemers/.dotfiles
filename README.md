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
See README.md in that package.
