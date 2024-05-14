(defpackage #:docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package docs/changelog)


(defchangelog (:ignore-words ("CLISP"
                              "CFFI"
                              "ERROR"
                              "SERIOUS-CONDITION"
                              "ECL"
                              "FFI"))
  (2.3.0 2024-05-13 "
* Now run-tests catches SERIOUS-CONDITION instead of ERROR and prevents handing of tests in the interactive prompt and timeouts on ECL.")
  (2.2.0 2024-01-30 "
* Shell, used to run all commands was changed from bash to `lispsh -eo pipefail {0}`. This fixes runner for Windows.
* Documentation builder workflow switched from `actions/checkout@v3` to `v4` version.")
  (2.1.2 2021-02-25
         "Run tests under the qlot.
          This fixes test running when Roswell and Qlot are restored from cache.")
  (2.1.1 2021-02-19
         "Fixed test running on CLISP where cl-coveralls installation
          fails with error: `CFFI requires CLISP compiled with dynamic FFI support`.")
  (2.1.0 2021-02-18
         "Now it is possible to conditionally select if coverage
          report should be generated.")
  (2.0.1 2021-02-18
         "Fixed issue when tests were run always on sbcl-bin implementation.")
  (2.0.0 2021-02-18
         "Added Coveralls support.")
  (1.0.0 2021-01-07
         "Initial release."))
