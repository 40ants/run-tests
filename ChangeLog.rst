===========
 ChangeLog
===========

2.1.2 (2021-02-25)
==================

Run tests under the qlot.
This fixes test running when Roswell and Qlot are restored from cache.

2.1.1 (2021-02-19)
==================

Fixed test running on CLISP where cl-coveralls installation
fails with error: "CFFI requires CLISP compiled with dynamic FFI support."

2.1.0 (2021-02-18)
==================

Now it is possible to conditionally select if coverage
report should be generated.

2.0.1 (2021-02-18)
==================

Fixed issue when tests were run always on sbcl-bin implementation.

2.0.0 (2021-02-18)
==================

Added Coveralls support.

1.0.0 (2021-01-07)
==================

Initial release.
