import sys
import time

def main(module=None):
    # Discover TestCase subclasses from the calling module's globals
    # Since we're imported, we need to find the caller's module
    # The test file does: if __name__ == "__main__": unittest.main()
    # So we look in the __main__ module's globals

    # Get the caller's global namespace by looking at sys.modules
    # The test is run as __main__
    import unittest
    from unittest.case import TestCase, SkipTest

    # Collect test classes from __main__ module only
    test_classes = []
    main_mod = sys.modules.get('__main__')
    if main_mod is not None and hasattr(main_mod, '__dict__'):
        md = main_mod.__dict__
        if isinstance(md, dict):
            for name, obj in md.items():
                if isinstance(obj, type) and issubclass(obj, TestCase) and obj is not TestCase:
                    if obj not in test_classes:
                        test_classes.append(obj)

    total = 0
    failures = 0
    errors = 0
    skipped = 0
    failed_tests = []

    start_time = time.monotonic()

    for cls in test_classes:
        # Find test methods
        methods = []
        for name in sorted(dir(cls)):
            if name.startswith('test_'):
                methods.append(name)

        for method_name in methods:
            sys.stderr.write(">> %s.%s\n" % (cls.__name__, method_name))
            tc = cls(method_name)
            total = total + 1
            result, detail = tc._run_test()
            if result == 'ok':
                sys.stdout.write('.')
            elif result == 'skip':
                sys.stdout.write('s')
                skipped = skipped + 1
            elif result == 'fail':
                sys.stdout.write('F')
                failures = failures + 1
                failed_tests.append((cls.__name__, method_name, detail))
            elif result == 'subfail':
                sys.stdout.write('F')
                failures = failures + 1
                for msg, params, exc in detail:
                    failed_tests.append((cls.__name__, method_name, exc))

    elapsed = time.monotonic() - start_time

    print()
    print()

    if failed_tests:
        for cls_name, method_name, exc in failed_tests:
            print("FAIL: %s.%s" % (cls_name, method_name))
            print("  %s: %s" % (type(exc).__name__, exc))
            print()

    print("----------------------------------------------------------------------")
    print("Ran %d tests in %.3fs" % (total, elapsed))
    print()

    if failures:
        extra = ""
        if skipped:
            extra = ", skipped=%d" % skipped
        print("FAILED (failures=%d%s)" % (failures, extra))
        sys.exit(1)
    else:
        extra = ""
        if skipped:
            extra = " (skipped=%d)" % skipped
        print("OK%s" % extra)
        sys.exit(0)
