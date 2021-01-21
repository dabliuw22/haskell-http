# Postgres

## Init
```shell
$ mkdir postgres
$ cd postgres
$ cabal init -p postgres
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./envs
- ./logger
- ./postgres
```