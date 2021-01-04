# Common Lisp Test Runner

(_c.f._ For full details and up to date documentation on automated test running for Exercism see [the v3-docs here][automated-test-runner].)

## Interface

The script that the docker image runs takes three arguments:

1. the test slug
2. the input directory namestring 
3. the output directory namestring

## Local Test Running

There is a shell script `./bin/run-local.sh` which will allow one to run the docker image on tests on one's local machine. It creates appropriate mount points so the test-runner can access all needed files.

Note: this script mush be run from the root directory of the repository.

For example:

```
./bin/run-local.sh basics ../v3/languages/common-lisp/exercises/concept/basics .
```

Will run the tests from the provided directory and put the `results.json` file in the current directory.


[automated-test-runner]: https://github.com/exercism/v3-docs/tree/master/anatomy/track-tooling/test-runners
