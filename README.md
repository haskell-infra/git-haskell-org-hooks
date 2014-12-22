git-haskell-org-hooks
=====================

Git pre-receive/update hook scripts for https://git.haskell.org

 - submodule gitlink reference check

## Gitolite 2.x instructions

Copy files from `update.secondary.d/` to `~/.gitolite/hooks/common/update.secondary.d/`

Build executables:

```
cabal sandbox init
cabal install
```

Copy resulting binaries into `~/bin`
