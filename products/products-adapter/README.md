# Products Adapter

## Init
```shell
$ mkdir products-adapter
$ cd products-adapter
$ cabal init -p products-adapter
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./envs
- ./postgres
- ./logger
- ./products/products-domain
- ./products/products-application
- ./products/products-adapter
```