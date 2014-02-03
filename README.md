# Dynamo Documentation

## Updating

```sh-session
$ cd dynamo
$ git clone --branch gh-pages `git config --get remote.origin.url` docs
$ MIX_ENV=docs mix deps.get
$ MIX_ENV=docs mix compile
$ MIX_ENV=docs mix docs
$ cd docs
$ git add -A .
$ git commit -m "Updated docs"
$ git push origin gh-pages
$ cd ..
$ rm -rf docs
```
