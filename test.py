from pathlib import Path
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

# Padding between test name and test result
cscript_test_result_padding = 65

# test suites excluded by the option provided via cli
excluded_testsuites = {};

# testsuites which are to be run, provided explicitly via cli
explicit_testsuites = {};

cli_options = {"-e": "excludes a testsuite"}

# Return path relative to root test directory
def rootpath(path):
    return Path(f"{cscript_root}/{path}");


def myprint(msg):
    print(msg, end='', flush=True);


# Testsuites
cscript_testsuite_paths = {
    "other": rootpath("other"),
    "syntax": rootpath("syntax"),
    "basic": rootpath("stdlibs/basic"),
    "package": rootpath("stdlibs/package"),
    "string": rootpath("stdlibs/string"),
    "math": rootpath("stdlibs/math"),
    "io": rootpath("stdlibs/io"),
    "os": rootpath("stdlibs/os"),
    "debug": rootpath("stdlibs/debug"),
    "list": rootpath("stdlibs/list"),
    "utf8": rootpath("stdlibs/utf8"),
    "reg": rootpath("stdlibs/reg"),
}


# Coloring
def red(s):
    return f"\x1b[31m{s}\x1b[0m";
def green(s):
    return f"\x1b[32m{s}\x1b[0m";
def yellow(s):
    return f"\x1b[33m{s}\x1b[0m";


def begin_testsuite(testsuite, path, total, isatty):
    skip = "";
    head = yellow("BEGIN") if isatty else "BEGIN";
    if total == 0:
        skip = yellow("SKIPPING") if isatty else "SKIPPING";
    print(f"{head} {testsuite} at \'{str(path)}\' ({total} tests) {skip}");


def end_testsuite(testsuite, total, failed, isatty):
    global cscript_total_fail;
    global cscript_total_pass;
    cscript_total_fail += failed;
    cscript_total_pass += (total - failed);
    passed = str(total - failed);
    failed = str(failed);
    end = "END";
    if isatty: # 'stdout' is terminal?
        passed = green(passed);
        failed = red(failed);
        end = yellow(end);
    if total > 0:
        print(f"Total tests passed: {passed}");
        print(f"Total tests failed: {failed}");
    print(f"{end} {testsuite}");


def print_test_name(testname):
    myprint('{:{}}'.format(testname, cscript_test_result_padding));
        

def print_test_result(testname, code, isatty):
    if code != 0:
        print('{}'.format(red("fail") if isatty else "fail"));
        cscript_failed_tests.append(red(testname) if isatty else testname);
    else:
        print('{}'.format(green("ok") if isatty else "ok"));


def run_cscript_testsuite(testsuite, path, isatty):
    tests = list(path.glob(f"*.{cscript_extension}"));
    total = len(tests);
    failed = 0;
    begin_testsuite(testsuite, path, total, isatty);
    for test in tests:
        testname = str(test);
        print_test_name(testname);
        cp = subprocess.run([cscript_bin, testname],
                            stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL);
        print_test_result(testname, cp.returncode, isatty);
        if cp.returncode != 0:
            failed += 1;
    end_testsuite(testsuite, total, failed, isatty);


def get_testsuite_path(tsuite):
    try:
        return cscript_testsuite_paths[tsuite];
    except:
        raise ValueError("Invalid testsuite '{}', it should be one of: {}."
                         .format(tsuite, list(cscript_testsuite_paths.keys())));


class MissingArgError(Exception):
    def __init__(self, extra):
        super().__init__(f"Missing command line argument. {extra}");

class InvalidOptError(Exception):
    def __init__(self, option):
        super().__init__(f"Invalid command line option '{option}', available options are {cli_options}.");


# Handle CLI option
def handleoption(argv, argc, i):
    if len(argv[i]) <= 1:
        raise InvalidOptError("-");
    option = argv[i][1:];
    if option == "e":
        if i >= argc:
            raise MissingArgError("Option 'e' requires a testsuite.");
        i += 1;
        tsuite = argv[i];
        get_testsuite_path(tsuite); # check if tsuite is valid
        excluded_testsuites[tsuite] = True;
    else:
        raise InvalidOptError(option);
    return i;


def handle_cli_args(argv, argc):
    i = 1; # skip script name
    while (i <= argc): # while have more cli args
        arg = argv[i];
        if arg[0] == "-": # option?
            i = handleoption(argv, argc, i);
        else: # otherwise a testsuite
            get_testsuite_path(arg); # check if 'tsuite' is valid
            explicit_testsuites[arg] = True; # mark it inside of hashset
        i += 1; # advance to next arg


def run_testsuites(isatty):
    if 0 < len(explicit_testsuites): # have explicit testsuites?
        for tsuite in list(explicit_testsuites.keys()):
            run_cscript_testsuite(tsuite, get_testsuite_path(tsuite), isatty);
    else: # otherwise try running all testsuites
        for tsuite in cscript_testsuite_paths.keys():
            if not excluded_testsuites.get(tsuite, False): # not excluded?
                run_cscript_testsuite(tsuite, cscript_testsuite_paths[tsuite], isatty);


# main()
handle_cli_args(sys.argv, len(sys.argv)-1);
run_testsuites(sys.stdout.isatty());
# output aggregate counts
print(f">> Total tests  ===> {yellow(cscript_total_pass+cscript_total_fail)}");
print(f">> Total passed ===> {green(cscript_total_pass)}");
print(f">> Total fail   ===> {red(cscript_total_fail)}");
# one more time output failed tests
for test in cscript_failed_tests:
    print(f"\t{test}");
