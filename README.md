economy
=======

A simple command-line tool for tracking montly incomes and expenses.
It's written in Haskell for no reason other than fun, and it's backed by JSON because [Aeson](https://github.com/bos/aeson) is cool.

Installation
------------
Installing `economy` should be as easy as `make` (and maybe `make deps` for installing its dependencies).
But for the lazy, here's a copy-pastable mini-guide:
```
git clone https://github.com/rootmos/economy
cd economy
make deps
make
```

Usage
-----
The data for `economy` is kept in a JSON file, usually kept in `~/.config/economy.json`.
The format for the file can be seen in the `example.json` file in the repository.

To see a summary of your yearly economy, type:
```
economy year
```
and to see details for a specific month, type:
```
economy month may
```

If you want to explicitly specify which datafile to use the `--file` option:
```
economy --file example.json year
```

Tag feature
---
If you specify `tags` in your incomes and expenses in the JSON then you can apply filters with them:
```
economy month jun --without=fun
```
where you might have tagged your beer expenses with `"tags": ["fun"]`.

