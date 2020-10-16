# haskell-http

In this project an API rest with **servant**, **postgresql-simple** was created. Using **Type-Classes** and **Tagless Final**.

Requirements:

   * Haskell Stack
   * Docker
   * Docker Compose
    
1. Run Containers: 
    ```
    $ docker-compose up -d
    ```

2. Build: 
    ```
    $ stack build
    ```

3. Run: 
    ```
    $ stack exec haskell-http-exe
    ```

4. Test with cURL:

    * All Products:
    ```
    curl --location --request GET 'http://localhost:8080/products'
    ```
   
    * Get Product By ID:
    ```
    curl --location --request GET 'http://localhost:8080/products/{UUID}'
    ``` 
   
    * Create Product:
    ```
    curl --location --request POST 'http://localhost:8080/products' \
    --header 'Content-Type: application/json' \
    --data-raw '{
        "product_name": "New Product Name",
        "product_stock": 22.0
    }'
    ```
   
5. Test with frontend client:
    [elm-http-client](https://github.com/dabliuw22/elm-http-client)
 
 
6. Apply formatter:
    ```
    $ stack install ormolu --resolver=lts-16.3
    $ cabal update
    $ cabal new-install ormolu
    $ ormolu --mode inplace $(find . -name '*.hs')
    ```
6. Run tests:
    ```
    $ stack test -- or `stack ghci haskell-http:haskell-http-test`
    ``` 