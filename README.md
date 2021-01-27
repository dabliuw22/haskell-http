# haskell-http

In this project an API rest with **servant**, **postgresql-simple** was created. Using **Type-Classes** and **Tagless Final**.

## Requirements:
   * Haskell Stack.
   * Cabal.
   * Docker.
   * Docker Compose.
   * libpq.

## Install `libpq`

### On Mac OS
```shell
$ brew install libpq # or brew install postgresql
$ brew link --force libpq # export PATH="/usr/local/opt/libpq/bin:$PATH"
```

### On Linux (Debian, Ubuntu)
```shell
$ sudo apt-get install libpq-dev # or sudo apt-get install postgresql postgresql-contrib
```
    
## Run Containers: 
```shell
$ docker-compose up -d
```

## Build: 
```shell
$ stack build
```

## Run: 
```shell
$ stack exec haskell-http-exe
```

## Test with cURL:

### All Products:
```shell
$ curl --location --request GET 'http://localhost:8080/products'
```

### Get Product By ID:
```shell
$ curl --location --request GET 'http://localhost:8080/products/{UUID}'
``` 

### Create Product:
```shell
$ curl --location --request POST 'http://localhost:8080/products' \
--header 'Content-Type: application/json' \
--data-raw '{
    "product_name": "New Product Name",
    "product_stock": 22.0
}'
```
   
## Test with frontend client:
[elm-http-client-app](https://github.com/dabliuw22/elm-http-client-app)
 
## Apply formatter:

### With Stack
```shell
$ stack install ormolu --resolver=lts-16.11
$ ormolu --mode inplace $(find . -name '*.hs')
```
### With Cabal
```shell
$ cabal update
$ cabal new-install ormolu
$ ormolu --mode inplace $(find . -name '*.hs')
```

## Run tests:
```shell
$ stack test # or `stack ghci haskell-http:haskell-http-test`
``` 