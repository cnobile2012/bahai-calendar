#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from bahai_calendar import BahaiCalendar, GregorianCalendar


class DateTests(BahaiCalendar):
    START_K = -5
    END_K = 5
    START_G = 1844
    END_G = 2200

    def __init__(self):
        self.gc = GregorianCalendar()

    def _calc_kvymd(self, days, k, v, y, m, data):
        year = (k - 1) * 361 + (v - 1) * 19 + y

        for d in reversed(range(1, days)):
            data.append(((k, v, y, m, d), (year, m, d)))

    def create_date_lists(self):
        data = []

        for k in reversed(range(self.START_K, self.END_K)):
            for v in reversed(range(1, 20)):
                for y in reversed(range(1, 20)):
                    for m in reversed(range(0, 20)):
                        if m == 0:
                            self._calc_kvymd(5, k, v, y, m, data)
                        else:
                            self._calc_kvymd(20, k, v, y, m, data)

        return data

    def check_b_date_from_date(self, data):
        items = []

        for item in data:
            b_date, date = item
            bd = self.b_date_from_date(date)

        if bd != (b_date + (0, 0, 0)):
            items.append((item, bd))

        return items

    def _create_gregorian_date_range(self, md=(3, 20)):
        return [(year,) + md for year in range(self.START_G, self.END_G, 10)]

    def _create_jd_for_gulian_date(self, data):
        return [self.gc.jd_from_gregorian_date(date) for date in data]

    def analize_date_error(self):
        g_data = self._create_gregorian_date_range()
        jds = self._create_jd_for_gulian_date(g_data)

        #for item in data:
        #    pass

        return jds


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test Badi date ranges."))
    parser.add_argument(
        '-l', '--list', action='store_true', default=False, dest='list',
        help="Generate a list of Badi dates both ling and short versions.")
    parser.add_argument(
        '-c', '--ck-dates', action='store_true', default=False, dest='ck_dates',
        help="Check that b_date_from_date() works correctly.")
    parser.add_argument(
        '-a', '--analize', action='store_true', default=False, dest='analize',
        help="Analize Badi date errors when converting to jd.")
    options = parser.parse_args()
    exclusive_error = (options.list, options.ck_dates, options.analize)
    assert exclusive_error.count(True) <= 1, (
        "Options -l, -c, and -a are exclusive.")

    dt = DateTests()

    if options.list:
        data = dt.create_date_lists()
        pprint.pprint(data)

    if options.ck_dates:
        data = dt.create_date_lists()
        bad_items = dt.check_b_date_from_date(data)
        bad_items = bad_items if bad_items else "All dates match."
        pprint.pprint(bad_items)

    if options.analize:
        data = dt.analize_date_error()
        pprint.pprint(data)

    sys.exit(0)
