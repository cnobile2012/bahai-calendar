# -*- coding: utf-8 -*-
#
# tests/conftest.py
#
# Used by pytest to set fixtures and other setup code to be available
# for all tests.
#

import time
import tracemalloc
tracemalloc.start()

start = time.perf_counter()


def pytest_sessionfinish(session, exitstatus):
    total = time.perf_counter() - start
    print(f"\n[PROFILE] Total test duration: {total:.2f} seconds.")
