## Testing
`test.toku` is standalone executable script used for running tests.
Every directory in this directory represents a testsuite, and any Tokudae
script file (filenames ending with `.toku` that do not start with `.`) in
the testsuite directory is considered as a test belonging to that testsuite.

This also holds special directories which are used by tests and so they
are to be ignored, these directories start with `_`, such as `_libs`
(they are not considered to be a testsuite).

Finally the `test.toku` contains a table data structure which you can
configure in order to control which tests are executed and how.
For example the `__opts` table in `test.toku` contains a field
`tests_script`, which holds the filename of the script that generates tests
table. This is a table describing the names of testsuites together with
the tests belonging to these testsuites, the default value of that field
is `"testsuites.toku"` (see the script file as a quick reference).

However the field `tests_script` can be omitted (or set to nil), in which case
the script will try to discover the testsuite directories and their tests by
the protocol mentioned above, however this method is only portable on
windows and posix environments.

For more information do `./test.toku -h` or see `test.toku`.
