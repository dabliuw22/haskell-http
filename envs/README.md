# Envs

## Init
```shell
$ mkdir envs
$ cd envs
$ cabal init -p envs
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./envs
```