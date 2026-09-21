RGL tests
=========

This directory contains tests for the resource grammar library.

Test cases
----------

Test cases are written as gf scripts. The file names must end with ".gfs" to
be recognized as a script.

The script are run using ``gf --run``.

For a test to pass
- gf must exit without an error code (exit 0)
- the output must matchh the corresponding ``.out`` file

For instance, if there is a test named ``my-test``, the gf commands should be
in ``my-test.gfs`` and the expected output in ``my-test.out``

Czech tests
-----------

From the checkout root, run ``sh tests/czech/check.sh``. See the
`Czech test README <czech/README.md>`_ for executable selection and coverage.
The runner uses the same ``.gfs``/``.out`` convention and additionally checks
API imports and generation/parsing through finite PGF fragments.
