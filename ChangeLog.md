<a id="x-28DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C2-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.3.0 (2024-05-13)

* Now run-tests catches `SERIOUS-CONDITION` instead of `ERROR` and prevents handing of tests in the interactive prompt and timeouts on `ECL`.

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C2-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.2.0 (2024-01-30)

* Shell, used to run all commands was changed from bash to `lispsh -eo pipefail {0}`. This fixes runner for Windows.
* Documentation builder workflow switched from `actions/checkout@v3` to `v4` version.

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C2-2E1-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.1.2 (2021-02-25)

Run tests under the qlot.
This fixes test running when Roswell and Qlot are restored from cache.

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C2-2E1-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.1.1 (2021-02-19)

Fixed test running on `CLISP` where cl-coveralls installation
fails with error: `CFFI requires CLISP compiled with dynamic FFI support`.

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C2-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.1.0 (2021-02-18)

Now it is possible to conditionally select if coverage
report should be generated.

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C2-2E0-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.0.1 (2021-02-18)

Fixed issue when tests were run always on sbcl-bin implementation.

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C2-2E0-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.0.0 (2021-02-18)

Added Coveralls support.

<a id="x-28DOCS-2FCHANGELOG-3A-3A-7C1-2E0-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 1.0.0 (2021-01-07)

Initial release.


* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
