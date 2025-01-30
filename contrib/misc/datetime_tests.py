#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/datetime_tests.py
#

import os
import sys
import datetime as dtime

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar, datetime


class DatetimeTests(BahaiCalendar):
    LOCAL_COORDS = (35.5894, -78.7792, -5.0)
    BADI_COORDS = BahaiCalendar._BAHAI_LOCATION[:3]
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)
    GREG_BADI_DATES = (
        ((1, 3, 19, 18, 14, 32, 553600), (-1842, 1, 1)),
        ((43, 3, 20, 18, 14, 26, 246400), (-1800, 1, 1)),
        ((143, 3, 20, 18, 15, 5, 558400), (-1700, 1, 1)),
        ((243, 3, 20, 18, 16, 31, 8000), (-1600, 1, 1)),
        ((343, 3, 20, 18, 17, 9, 715200), (-1500, 1, 1)),
        ((443, 3, 20, 18, 17, 3, 235200), (-1400, 1, 1)),
        ((543, 3, 20, 18, 17, 42, 115200), (-1300, 1, 1)),
        ((643, 3, 20, 18, 18, 20, 131200), (-1200, 1, 1)),
        ((743, 3, 20, 18, 19, 45, 667200), (-1100, 1, 1)),
        ((843, 3, 20, 18, 19, 37, 27200), (-1000, 1, 1)),
        ((943, 3, 20, 18, 20, 14, 870400), (-900, 1, 1)),
        ((1043, 3, 20, 18, 20, 53, 318400), (-800, 1, 1)),
        ((1143, 3, 20, 18, 22, 17, 817600), (-700, 1, 1)),
        ((1243, 3, 20, 18, 22, 8, 40000), (-600, 1, 1)),
        ((1343, 3, 20, 18, 22, 45, 984000), (-500, 1, 1)),
        ((1443, 3, 20, 18, 23, 22, 790400), (-400, 1, 1)),
        ((1543, 3, 20, 18, 24, 48, 844800), (-300, 1, 1)),
        ((1582, 3, 20, 18, 24, 27, 158400), (-261, 1, 1)),
        ((1582, 4, 7), (-261, 1, 19)),
        ((1582, 4, 26), (-261, 2, 19)),
        ((1582, 5, 15), (-261, 3, 19)),
        ((1582, 6, 3), (-261, 4, 19)),
        ((1582, 6, 22), (-261, 5, 19)),
        ((1582, 7, 11), (-261, 6, 19)),
        ((1582, 7, 30), (-261, 7, 19)),
        ((1582, 8, 18), (-261, 8, 19)),
        ((1582, 9, 6), (-261, 9, 19)),
        ((1582, 9, 25), (-261, 10, 19)),
        ((1582, 10, 4), (-261, 11, 9)),
        ((1582, 10, 4, 17, 41, 27, 686400), (-261, 11, 9)),
        ((1582, 10, 14), (-261, 11, 19)),
        ((1643, 3, 20, 18, 16, 28, 675200), (-200, 1, 1)),
        ((1743, 3, 20, 18, 16, 13, 641600), (-100, 1, 1)),
        ((1752, 9, 2), (-91, 9, 16)),
        ((1752, 9, 3), (-91, 9, 17)),
        ((1752, 9, 13), (-91, 10, 8)),
        ((1752, 9, 14), (-91, 10, 9)),
        ((1843, 3, 20, 18, 16, 48, 806400), (0, 1, 1)),
        ((1844, 3, 19, 18, 16, 36, 710400), (1, 1, 1)),
        ((1943, 3, 20, 18, 16, 32, 563200), (100, 1, 1)),
        ((2024, 3, 19, 18, 15, 57, 312000), (181, 1, 1)),
        ((2024, 4, 6), (181, 1, 19)),
        ((2024, 4, 25), (181, 2, 19)),
        ((2024, 5, 14), (181, 3, 19)),
        ((2024, 6, 2), (181, 4, 19)),
        ((2024, 6, 21), (181, 5, 19)),
        ((2043, 3, 20, 18, 16, 16, 406400), (200, 1, 1)),
        ((2143, 3, 20, 18, 15, 59, 731200), (300, 1, 1)),
        ((2243, 3, 20, 18, 15, 41, 587200), (400, 1, 1)),
        ((2343, 3, 20, 18, 16, 15, 283200), (500, 1, 1)),
        ((2443, 3, 20, 18, 15, 56, 707200), (600, 1, 1)),
        ((2543, 3, 20, 18, 15, 37, 353600), (700, 1, 1)),
        ((2643, 3, 20, 18, 15, 18, 345600), (800, 1, 1)),
        ((2743, 3, 20, 18, 15, 50, 54400), (900, 1, 1)),
        ((2843, 3, 20, 18, 15, 29, 664000), (1000, 1, 1)),
        ((2943, 3, 20, 18, 15, 9, 100800), (1100, 1, 1)),
        ((3004, 3, 20, 18, 15, 14, 630400), (1161, 1, 1)),
        )

    def __init__(self):
        super().__init__()
        self._gc = GregorianCalendar()

    def analyze_ordinal_error_list(self, options):
        """
        Find the errors between the Badi datetime ordinals and the Python
        date time ordinals.

        -a
        """
        data = []

        for g_date, b_date in self.GREG_BADI_DATES:
            g_dt = dtime.datetime(*g_date)
            g_ord = g_dt.toordinal()
            b_dt = datetime(*b_date)
            b_ord = b_dt.toordinal()
            same = g_ord == b_ord
            diff = g_ord - b_ord
            g_jd = self._gc.jd_from_gregorian_date(g_date, exact=True)
            b_jd = self.jd_from_badi_date(b_date, *self.LOCAL_COORDS)
            jd_diff = round(g_jd - b_jd, 6)
            date = self.badi_date_from_jd(b_jd, *self.LOCAL_COORDS,
                                          short=True, trim=True, rtd=True)
            d_diff = self._subtract_tuples(b_date, date)
            leap = self._is_leap_year(b_date[0])
            data.append((g_date, g_ord, b_date, b_ord, same, diff,
                         g_jd, jd_diff, date, d_diff, leap))

        return data

    def analyze_ordinal_error_create(self, options):
        """
        Find the errors between conversion between badi dates to JD and
        vice versa. This test in reality tests the _adjust_date method

        -b
        Also if -S and -E are used they must be used together and refer
        to Gregorian years.
        """
        data = []
        start = options.start
        end = options.end

        for year in range(start, end):
            #print(year, file=sys.stderr)
            is_leap = self._is_leap_year(year)

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + is_leap

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    jd = self.jd_from_badi_date(date)
                    # Get the Badi date from the Julian Period day.
                    #on = False if (year + 1) == 1162 else True
                    b_date = self.badi_date_from_jd(
                        jd, short=True, trim=True, rtd=True)
                    diff0 = self._subtract_tuples(b_date, date)
                    g_date = self._gc.gregorian_date_from_jd(jd, exact=True)
                    # The ordinal date for visual comparison.
                    ordinal = self._ordinal_from_jd(jd)
                    o = datetime.fromordinal(ordinal, short=True)
                    o_date = (o.year, o.month, o.day)
                    diff1 = self._subtract_tuples(o_date, date)
                    data.append((g_date, jd, o_date, b_date,
                                 date, diff0, diff1))

        return data

    def _subtract_tuples(self, t0, t1):
        return t0[0] - t1[0], t0[1] - t1[1], t0[2] - t1[2]


if __name__ == "__main__":
    import time
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test datetime date ranges."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Analyze ordinal dates from list.")
    parser.add_argument(
        '-b', '--analyze1', action='store_true', default=False,
        dest='analyze1', help="Analyze Badi and Gregorian dates.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End Badi year of sequence.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start Badi year of sequence.")

    options = parser.parse_args()
    dt = DatetimeTests()
    ret = 0

    if options.analyze:  # -a
        print("Gregorian Date                   Greg ord Badi Date     "
              "Badi ord Same  Df | JD             JD Diff    Badi Date     "
              "Date Diff Leap")
        print('-'*131)
        [print(f"{str(g_date):33} "
               f"{g_ord:>7} "
               f"{str(b_date):14} "
               f"{b_ord:>7} "
               f"{str(same):<5} "
               f"{diff:02} | "
               f"{g_jd:<14} "
               f"{jd_diff:+09} "
               f"{str(date):14} "
               f"{str(d_diff)} "
               f"{str(leap)}"
               ) for (g_date, g_ord, b_date, b_ord, same, diff,
                      g_jd, jd_diff, date, d_diff, leap)
         in dt.analyze_ordinal_error_list(options)]
    elif options.analyze1:  # -b
        if options.start is None or options.end is None:
            # Set default Gregorian years.
            options.start = -1842  # Julian year 1
            options.end = 1162     # Gregorian year 3005

        start_time = time.time()
        print(" "*84, "Badi - Orig    Ord - Orig")
        print("Greg Date             JD             Ordinal Date    Badi Date"
              "       Orig Date       B Date Diff    O Date Diff")
        print('-'*111)
        data = dt.analyze_ordinal_error_create(options)
        total_diff0 = total_diff1 = 0
        items = []

        for g_date, jd, o_date, b_date, date, diff0, diff1 in data:
            if diff0 != (0, 0, 0):
                total_diff0 += 1

            if diff1 != (0, 0, 0):
                total_diff1 += 1

            if (0, 0, 0) not in (diff0, diff1):
                items.append((g_date, jd, o_date, b_date, date, diff0, diff1))

        [print(f"{str(g_date):21} "
               f"{jd:<14} "
               f"{str(o_date):15} "
               f"{str(b_date):15} "
               f"{str(date):15} "
               f"{str(diff0):15}"
               f"{str(diff1):15}")
         for g_date, jd, o_date, b_date, date, diff0, diff1 in items]
        total_errors = total_diff0 + total_diff1
        print(f"Analyzing year {options.start} to year {options.end-1}.")
        print(f"Ordinal Errors: {total_diff1}")
        print(f"   Badi Errors: {total_diff0}")
        print(f"  Total Errors: {total_errors}")
        end_time = time.time()
        days, hours, minutes, seconds = dt._dhms_from_seconds(
            end_time - start_time)
        print(f"  Elapsed time: {hours:02} hours, {minutes:02} minutes, "
              f"{round(seconds, 6):02.6} seconds.")
    else:
        parser.print_help()

    sys.exit(ret)
