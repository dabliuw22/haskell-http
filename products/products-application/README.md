# Products Application

## Init
```shell
$ mkdir products-application
$ cd products-application
$ cabal init -p products-application
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./envs
- ./postgres
- ./logger
- ./products-domain
- ./products-application
```