(defpackage #:docs/docs
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package docs/docs)


(defmethod docs-config ((system (eql (asdf:find-system "docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (ql:quickload :40ants-doc-theme-40ants)
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "GitHub Action to Run Tests for a Common Lisp Library"
                    :ignore-words ("SBCL"
                                   "CCL"
                                   "ASDF"))
  "
This is a Github Action can be used to run tests for any Common Lisp supporting `(asdf:test-system :my-system)`.

It should be used after the [setup-lisp](https://40ants.com/setup-lisp/) action.
"
  (@features section)
  (@typical-usage section)
  (@custom-test-runner section)
  (@coveralls section)
  (@roadmap section))


(defsection-copy @readme @index)


(defsection @features (:title "What this action does for you?")
  "
* It runs (asdf:test-system :the-system-name) by default.
* But you can provide your own lisp code.
* It automatically searches a test system name if it is present.
")


(defsection @typical-usage (:title "A typical usage")
  "
Here is how a minimal GitHub Workflow might look like:

```yaml
name: 'CI'

on:
  push:
    branches:
      - 'main'
      - 'master'
  pull_request:

jobs:
  tests:
    runs-on: ubuntu-latest
    
    strategy:
      matrix:
        lisp:
          - sbcl-bin
          - ccl-bin
          
    env:
      LISP: ${{ matrix.lisp }}

    steps:
      - uses: actions/checkout@v1
      - uses: 40ants/setup-lisp@v1
        with:
          asdf-system: cl-info
      - uses: 40ants/run-tests@v2
        with:
          asdf-system: cl-info
```

The part, corresponding to an action call is:

```yaml
- uses: 40ants/run-tests@v2
  with:
    asdf-system: cl-info
```

Here we provided a system name `cl-info`, but
action is smart enough to detect that really
there is a separate `cl-info-test` ASDF system.

To guess a system name to run tests against, action
will check `${asdf-system}-test`, `${asdf-system}-tests`,
`${asdf-system}/test` and `${asdf-system}/tests`. And if none
of them found - fall back to `${asdf-system}`.

**Please, note, that `(asdf:test-system :your-system-name)`
should signal error in case if some tests were failed.** Only
in this case action will exit with error code.
")


(defsection @custom-test-runner (:title "Custom test runner")
  "
Sometimes you might want to use something special instead of
`(asdf:test-system :your-system-name)`. You can pass any lisp
code to the action:

```
- uses: 40ants/run-tests@v2
  with:
    asdf-system: cl-info
    run-tests: |
      (ql:quickload :cl-info-test)

      (unless (rove:run :cl-info-test)
         (error \"Tests failed\"))
```

")


(defsection @coveralls (:title "Publishing reports to Coveralls")
  "
This action automates coverage collection and reporting. To publish report
to the [Coveralls](https://coveralls.io/), pass your github token as
a `coveralls-token` argument:

```yaml
- uses: 40ants/run-tests@v2
  with:
    asdf-system: cl-info
    coveralls-token: ${{ secrets.github_token }}
```

If you are using \"matrix\", then it is good idea to collect coverage report
only on one matrix combination. To do this, use a logical expression which
will check some variables and returns a token only if all of them are true:

```yaml
- uses: 40ants/run-tests@v2
  with:
    asdf-system: cl-info
    coveralls-token: |
      ${{ matrix.lisp == 'sbcl-bin' &&
          matrix.os == 'ubuntu-latest' &&
          matrix.quicklisp-dist == 'ultralisp' &&
          secrets.github_token }}
```

Here is an example how your report on Coveralls can look like:

https://coveralls.io/github/40ants/cl-info

**Note**, that coverage reporting currently works only on SBCL and CCL 1.4.
You can contribute support for other implementations to
[cl-coveralls](https://github.com/fukamachi/cl-coveralls).
")


(defsection @roadmap (:title "Roadmap")
  "
- Support uploading code coverage reports to CodeCov.
- Vendor all dependencies, to make action more reliable and secure.
")
