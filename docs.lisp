(defpackage #:docs
  (:use #:cl)
  (:nicknames #:docs/docs)
  (:import-from #:mgl-pax-minimal
                #:defsection
                #:section))
(in-package docs)


(defsection @index (:title "GitHub Action to Run Tests for a Common Lisp Library")
  "
This is a Github Action can be used to run tests for any Common Lisp supporting `(asdf:test-system :my-system)`.

It should be used after the [setup-lisp](https://40ants.com/setup-lisp/) action.
"
  (@features section)
  (@typical-usage section)
  (@custom-test-runner section)
  (@roadmap section))


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
      - uses: 40ants/run-tests@v1
        with:
          asdf-system: cl-info
```

The part, corresponding to an action call is:

```yaml
- uses: 40ants/run-tests@v1
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
- uses: 40ants/run-tests@v1
  with:
    asdf-system: cl-info
    run-tests: |
      (ql:quickload :cl-info-test)

      (unless (rove:run :cl-info-test)
         (error \"Tests failed\"))
```

")


(defsection @roadmap (:title "Roadmap")
  "
- Support uploading code coverage reports to Coveralls and CodeCov.
- Vendor all dependencies, to make action more reliable and secure.
")
