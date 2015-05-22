# purescript-lists

A library for strict linked lists (```Data.List```), lazy linked lists (```Data.List.Lazy```) and for the list monad transformer (```Control.Monad.ListT```).

- [Module Documentation](docs/)

## Importing

Add the following to your ```bower.json``` file under ```dependencies```:

```
"purescript-lists" : "~0.3.8"
```

If you would like to use quickcheck bindings for ```List``` and ```ListT```, 
provide the following files to the compiler:

```
"bower_components/purescript-*/quickcheck-test-src/**/*.purs"
```

## Building

```
npm install
bower update
grunt
```
