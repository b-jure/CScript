from pathlib import Path, PosixPath
import subprocess
import sys


# Mapping of testsuite names to their relative paths,
# assuming the script is ran from the cscript git repository
# root directory.
cscript_testsuite_paths = {
    "syntax": Path("./test/syntax"),
    "basic": Path("./test/libs/basic"),
    "package": Path("./test/libs/package"),
    "string": Path("./test/libs/string"),
    "math": Path("./test/libs/math"),
}


# Interpreter binary
cscript_bin = "./cscript"


# Coloring
def red(s):
    return f"\x1b[31m{s}\x1b[0m"
def green(s):
    return f"\x1b[32m{s}\x1b[0m"
def yellow(s):
    return f"\x1b[33m{s}\x1b[0m"


def begin_testsuite(testsuite, path, total):
    skip = ""
    isatty = sys.stdout.isatty()
    head = yellow("BEGIN") if isatty else "BEGIN"
    if total == 0:
        skip = yellow("SKIPPING") if isatty else "SKIPPING"
    print(f"{head} {testsuite} at \'{str(path)}\' ({total} tests) {skip}")


def end_testsuite(testsuite, total, failed):
    isatty = sys.stdout.isatty()
    passed = str(total - failed);
    failed = str(failed)
    end = "END"
    if isatty: # 'stdout' is terminal?
        passed = green(passed)
        failed = red(failed)
        end = yellow(end)
    if total > 0:
        print(f"Total tests passed: {passed}")
        print(f"Total tests failed: {failed}")
    print(f"{end} {testsuite}")


# Print individual test result
def print_test_result(test, code):
    tname = str(test)
    isatty = sys.stdout.isatty()
    if code != 0:
        if isatty:
            print('{:60} {}'.format(red(tname), red("fail")))
        else:
            print('{:60} {}'.format(tname, "fail"))
        return 1
    else:
        if isatty:
            print('{:60} {}'.format(green(tname), green("ok")))
        else:
            print('{:60} {}'.format(tname, "ok"))
        return 0


# Run all CScript testsuite tests located at 'path'
def run_cscript_testsuite(testsuite, path):
    tests = list(path.glob('*.cst'))
    total = len(tests)
    failed = 0
    begin_testsuite(testsuite, path, total)
    for test in tests:
        cp = subprocess.run([cscript_bin, str(test)],
                            stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL)
        failed += print_test_result(cp.args[1], cp.returncode)
    end_testsuite(testsuite, total, failed)


# Get testsuite path (with error handling)
def get_testsuite_path(testsuite):
    try:
        return cscript_testsuite_paths[testsuite]
    except:
        raise ValueError("invalid testsuite '{}', here is the list of available testsuites: {}"
                         .format(testsuite, list(cscript_testsuite_paths.keys())))


# Entry, runs all testsuites or specific ones if command line
# arguments are present.
def run_tests():
    argv = sys.argv
    argc = len(argv)
    if (argc > 1): # run specific testsuites?
        while (True):
            argc -= 1;
            testsuite = argv[argc]
            run_cscript_testsuite(testsuite, get_testsuite_path(testsuite))
            if argc <= 1: break;
    else: # otherwise run all testsuites
        for testsuite in cscript_testsuite_paths.keys():
            run_cscript_testsuite(testsuite, cscript_testsuite_paths[testsuite])


run_tests()
