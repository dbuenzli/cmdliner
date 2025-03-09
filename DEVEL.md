This project uses (perhaps the development version of) [`b0`] for
development. Consult [b0 occasionally] for quick hints on how to
perform common development tasks.

[`b0`]: https://erratique.ch/software/b0
[b0 occasionally]: https://erratique.ch/software/b0/doc/occasionally.html

# Build system for distribution

The build system used for distribution is in the `Makefile`.

# Testing

Testing is done with `B0_testing` from `b0.std`. The catch is that
`B0_testing` depends on `cmdliner` so we need a build of `b0.std` with
our build of `cmdliner` to link against our test executables.

To do so we do a checkout of `b0`'s repo in `test/b0` (which is ignored
by `git`).

    cd test
    git clone https://erratique.ch/repos/b0.git

The [`B0.ml`](B0.ml) file of cmdliner includes `test/b0/B0.ml` and the
`default` pack of `B0.ml` is unlocked so that when a test executable
requires `b0.std` it is looked up and built againt the development
version of cmdliner. After that testing remains [as usual].

[as usual]: https://erratique.ch/software/b0/doc/occasionally.html#test

## Manual renderings

Various manual renderings are snapshot tested in the test executables,
mostly in plain text.

The `test_man` test can be invoked with `--test-help[=FMT]` to interactively
test the various `--help[=FMT]` invocations, including paging.

    b0 -- test_man --test-help
