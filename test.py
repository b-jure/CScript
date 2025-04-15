from pathlib import Path, PosixPath
import subprocess
import sys


# Interpreter binary
cscript_bin = "./cscript"

# Root test directory
cscript_root = "./test"

# Script file extension
cscript_extension = "cscript"

# Total number of failed/passed tests
cscript_total_fail = 0
cscript_total_pass = 0

# List of failed tests
cscript_failed_tests = []


# Return path relative to root test directory
def rootpath(path):
    return Path(f"{cscript_root}/{path}")

# Testsuites
cscript_testsuite_paths = {
    "syntax": rootpath("syntax"),
    "basic": rootpath("stdlibs/basic"),
    "package": rootpath("stdlibs/package"),
    "string": rootpath("stdlibs/string"),
    "math": rootpath("stdlibs/math"),
    "io": rootpath("stdlibs/io"),
    "os": rootpath("stdlibs/os"),
}


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
    global cscript_total_fail
    global cscript_total_pass
    isatty = sys.stdout.isatty()
    cscript_total_fail += failed
    cscript_total_pass += (total - failed)
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



def print_test_result(test, code):
    tname = str(test)
    isatty = sys.stdout.isatty()
    if code != 0:
        if isatty:
            print('{:60} {}'.format(red(tname), red("fail")))
        else:
            print('{:60} {}'.format(tname, "fail"))
        cscript_failed_tests.append(red(tname));
        return 1
    else:
        if isatty:
            print('{:60} {}'.format(green(tname), green("ok")))
        else:
            print('{:60} {}'.format(tname, "ok"))
        return 0



def run_cscript_testsuite(testsuite, path):
    tests = list(path.glob(f"*.{cscript_extension}"))
    total = len(tests)
    failed = 0
    begin_testsuite(testsuite, path, total)
    for test in tests:
        cp = subprocess.run([cscript_bin, str(test)],
                            stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL)
        failed += print_test_result(cp.args[1], cp.returncode)
    end_testsuite(testsuite, total, failed)



def get_testsuite_path(testsuite):
    try:
        return cscript_testsuite_paths[testsuite]
    except:
        raise ValueError("invalid testsuite '{}', here is the list of available testsuites: {}"
                         .format(testsuite, list(cscript_testsuite_paths.keys())))



def run_tests():
    global cscript_testsuite_paths
    argv = sys.argv
    argc = len(argv)

    # Handle command-line arguments
    if (argc > 1): # run specific testsuites?
        while (True):
            argc -= 1;
            testsuite = argv[argc]
            run_cscript_testsuite(testsuite, get_testsuite_path(testsuite))
            if argc <= 1: break;
    else: # run all testsuites
        for testsuite in cscript_testsuite_paths.keys():
            run_cscript_testsuite(testsuite, cscript_testsuite_paths[testsuite])

    # Output aggregate test counts
    print(f">> Total tests  ===> {yellow(cscript_total_pass+cscript_total_fail)}")
    print(f">> Total passed ===> {green(cscript_total_pass)}")
    print(f">> Total fail   ===> {red(cscript_total_fail)}")
    for test in cscript_failed_tests:
        print(f"\t{test}");


run_tests()
