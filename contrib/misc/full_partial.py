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
    END_G = 2400

    TMP_ANS_DATES = (
        (2394645.5, (  1, 1, 1)), # (1844, 3, 20)
        (2398297.5, ( 11, 1, 1)), # (1854, 3, 20)
        (2401950.5, ( 21, 1, 1)), # (1864, 3, 20)
        (2405602.5, ( 31, 1, 1)), # (1874, 3, 20)
        (2409255.5, ( 41, 1, 1)), # (1884, 3, 20)
        (2412907.5, ( 51, 1, 1)), # (1894, 3, 20)
        (2414733.5, ( 56, 1, 1)), # (1899, 3, 20) =
        (2415098.5, ( 57, 1, 1)), # (1900, 3, 20) +1
        (2415463.5, ( 58, 1, 1)), # (1901, 3, 20) +1
        (2415828.5, ( 59, 1, 1)), # (1902, 3, 20) +1
        (2416193.5, ( 60, 1, 1)), # (1903, 3, 20) +1
        (2416559.5, ( 61, 1, 1)), # (1904, 3, 20) +1
        (2420211.5, ( 71, 1, 1)), # (1914, 3, 20)
        (2423864.5, ( 81, 1, 1)), # (1924, 3, 20)
        (2427516.5, ( 91, 1, 1)), # (1934, 3, 20)
        (2431169.5, (101, 1, 1)), # (1944, 3, 20)
        (2434821.5, (111, 1, 1)), # (1954, 3, 20)
        (2438474.5, (121, 1, 1)), # (1964, 3, 20)
        (2442126.5, (131, 1, 1)), # (1974, 3, 20)
        (2445779.5, (141, 1, 1)), # (1984, 3, 20)
        (2449431.5, (151, 1, 1)), # (1994, 3, 20)
        (2453084.5, (161, 1, 1)), # (2004, 3, 20)
        (2456736.5, (171, 1, 1)), # (2014, 3, 20)
        (2460389.5, (181, 1, 1)), # (2024, 3, 20)
        (2464041.5, (191, 1, 1)), # (2034, 3, 20)
        (2467694.5, (201, 1, 1)), # (2044, 3, 20)
        (2471346.5, (211, 1, 1)), # (2054, 3, 20)
        (2474999.5, (221, 1, 1)), # (2064, 3, 20)
        (2478651.5, (231, 1, 1)), # (2074, 3, 20)
        (2482304.5, (241, 1, 1)), # (2084, 3, 20)
        (2485956.5, (251, 1, 1)), # (2094, 3, 20)
        (2487782.5, (256, 1, 1)), # (2099, 3, 20) +1
        (2488147.5, (257, 1, 1)), # (2100, 3, 20) +2
        (2488512.5, (258, 1, 1)), # (2101, 3, 20) +2
        (2488877.5, (259, 1, 1)), # (2102, 3, 20) +2
        (2489242.5, (260, 1, 1)), # (2103, 3, 20) +2
        (2489608.5, (261, 1, 1)), # (2104, 3, 20) +2
        (2493260.5, (271, 1, 1)), # (2114, 3, 20)
        (2496913.5, (281, 1, 1)), # (2124, 3, 20)
        (2500565.5, (291, 1, 1)), # (2134, 3, 20)
        (2504218.5, (301, 1, 1)), # (2144, 3, 20)
        (2507870.5, (311, 1, 1)), # (2154, 3, 20)
        (2511523.5, (321, 1, 1)), # (2164, 3, 20)
        (2515175.5, (331, 1, 1)), # (2174, 3, 20)
        (2518828.5, (341, 1, 1)), # (2184, 3, 20)
        (2522480.5, (351, 1, 1)), # (2194, 3, 20)
        (2526132.5, (361, 1, 1)), # (2204, 3, 20) +2
        (2529784.5, (371, 1, 1)), # (2214, 3, 20) +3
        (2533437.5, (381, 1, 1)), # (2224, 3, 20)
        (2537089.5, (391, 1, 1)), # (2234, 3, 20)
        (2540742.5, (401, 1, 1)), # (2244, 3, 20)
        (2544394.5, (411, 1, 1)), # (2254, 3, 20)
        (2548047.5, (421, 1, 1)), # (2264, 3, 20)
        (2551699.5, (431, 1, 1)), # (2274, 3, 20)
        (2555352.5, (441, 1, 1)), # (2284, 3, 20)
        (2559004.5, (451, 1, 1)), # (2294, 3, 20) +3
        (2562656.5, (461, 1, 1)), # (2304, 3, 20) +4
        (2566308.5, (471, 1, 1)), # (2314, 3, 20)
        (2569961.5, (481, 1, 1)), # (2324, 3, 20)
        (2573613.5, (491, 1, 1)), # (2334, 3, 20)
        (2577266.5, (501, 1, 1)), # (2344, 3, 20)
        (2580918.5, (511, 1, 1)), # (2354, 3, 20)
        (2584571.5, (521, 1, 1)), # (2364, 3, 20)
        (2588223.5, (531, 1, 1)), # (2374, 3, 20)
        (2591876.5, (541, 1, 1)), # (2384, 3, 20)
        (2595528.5, (551, 1, 1)), # (2394, 3, 20)
        )

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
        #g_data = self._create_gregorian_date_range()
        #jds = self._create_jd_for_gulian_date(g_data)
        #z = zip(jds, g_data)
        #pprint.pprint([d for d in z])
        data = []

        for jd, date in self.TMP_ANS_DATES:
            year = date[0]
            month = date[1]
            day = date[2]

            if month == 0: # Ayyam-i-Ha
                d = 18 * 19 + day
            elif month < 19:
                d = (month - 1) * 19 + day
            else: # month == 19:
                d = 18 * 19 + 4 + day

            badi_epoch_m_o = self.BADI_EPOCH - 1
            jey_y_m_o = self.JULIAN_YEAR * (year - 1)
            floor_jey = math.floor(jey_y_m_o)
            bjd = badi_epoch_m_o + floor_jey + d

            #if jd != bjd:
            data.append((date, jd, bjd, jey_y_m_o, floor_jey))

        return data


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
