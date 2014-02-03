# Dynamo Documentation

## Updating

```sh-session
$ cd dynamo
$ git clone --branch gh-pages `git config --get remote.origin.url` docs
$ MIX_ENV=docs mix deps.get
$ MIX_ENV=docs mix compile
$ MIX_ENV=docs mix docs
$ GIT_DIR=docs git commit -m "Updated docs"
$ GIT_DIR=docs git push origin gh-pages
$ rm -rf docs
```
