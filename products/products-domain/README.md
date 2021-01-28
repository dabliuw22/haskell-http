# Products Domain

## Init
```shell
$ mkdir products-domain
$ cd products-domain
$ cabal init -p products-domain
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./envs
- ./postgres
- ./logger
- ./products-domain
```