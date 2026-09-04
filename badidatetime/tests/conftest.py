# -*- coding: utf-8 -*-
#
# tests/conftest.py
#
# Used by pytest to set fixtures and other setup code to be available
# for all tests.
#

import time
import pytest
import tracemalloc
tracemalloc.start()

start = time.perf_counter()
_test_times = {}
SLOW_THRESHOLD = 1.0  # seconds


def pytest_sessionfinish(session, exitstatus):
    total = time.perf_counter() - start
    print(f"\n[PROFILE] Total test duration: {total:.2f} seconds.")


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_call(item):
    start = time.perf_counter()
    yield
    _test_times[item.nodeid] = time.perf_counter() - start


def pytest_terminal_summary(terminalreporter, exitstatus, config):
    slow_tests = [(name, elapsed) for name, elapsed in _test_times.items()
                  if elapsed >= SLOW_THRESHOLD]

    if not slow_tests:
        terminalreporter.write_line(
            f"\nNo tests exceeded {SLOW_THRESHOLD:.1f}s.")
        return

    terminalreporter.write_sep("-", f"Tests slower than {SLOW_THRESHOLD:.1f}s")

    for name, elapsed in sorted(slow_tests, key=lambda x: x[1], reverse=True,):
        terminalreporter.write_line(f"{elapsed:8.3f}s  {name}")
