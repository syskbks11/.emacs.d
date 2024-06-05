# .emacs.d
emacs configurations
- updated from https://gist.github.com/syskbks11/8a19b16aa21daf59e687549f8c1afc83

# Requirements

- emacs >= 29.3

# Checks before starting installation

- Read references
  - https://zenn.dev/takeokunn/articles/56010618502ccc

- confirm necessary libraries installed in your machine
```
ls /usr/bin/xml2-config
```

# Installation

## emacs

- download emacs and compile with option `with-native-compilation=aot`
  - adjust cpu core used during compilation (8 cpus will be used with below example)
  - options for configure command can be added/removed as you want
```
git clone git://git.sv.gnu.org/emacs.git --depth 1
cd emacs
./autogen.sh && ./configure --with-native-compilation=aot --without-ns --without-x --with-libxml2=/usr/bin/xml2-config && make -j8
```

- setup to use this emacs as default
  - choose one of two methods
```
alias emacs="[DIRECTORY_OF_YOUR_EMACS]/emacs/src/emacs" # write in bashrc or zshrc
```
```
make install # install in system
```

## .emacs.d

```
cd
mv .emacs.d dot.emacs.d_old # rename current .emacs.d
git clone git@github.com:syskbks11/.emacs.d.git
```

## emacs library

- when starting emacs, all libraries of emacs will be downloaded automatically
  - it takes several minutes


