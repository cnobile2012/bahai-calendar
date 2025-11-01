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

from _timestamp import TimestampUtils
from badidatetime import BahaiCalendar, GregorianCalendar, datetime


class DatetimeTests(BahaiCalendar, TimestampUtils):
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
        self.gc = GregorianCalendar()

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
            g_jd = self.gc.jd_from_gregorian_date(g_date, exact=True)
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
        vice versa. This tests the BahaiCalendar._adjust_date method.

        -b
        Also if -S and -E are used they must be used together and refer
        to Badi years.
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
                    # We default to the Epoch Coordinates.
                    jd = self.jd_from_badi_date(date)
                    # Get the Badi date from the Julian Period day.
                    b_date = self.badi_date_from_jd(
                        jd, short=True, trim=True, rtd=True)
                    # Difference of the date converted to a JD then back to a
                    # date again then subtract the converted date from the
                    # original date. They should be the same.
                    diff0 = self._subtract_tuples(b_date, date)
                    # The ordinal date for visual comparison.
                    ordinal = self._ordinal_from_jd(jd)
                    o = datetime.fromordinal(ordinal, short=True)
                    o_date = (o.year, o.month, o.day)
                    # Difference of the Badi datetime derived from an ordinal
                    # then subtract the derived date original date. They
                    # should be the same.
                    diff1 = self._subtract_tuples(o_date, date)
                    # Get the Gregorian date.
                    g_date = self.gc.gregorian_date_from_jd(jd, exact=True)
                    data.append((g_date, jd, date, b_date, o_date,
                                 diff0, diff1))

        return data

    def analyze_timestamp_errors(self, options):
        """
        Fins the errors in timestamp convbersions to Badi dates. This test
        ensures that the datetime.date.today method changes date on sunset
        based on local time.
        https://www.unixtimestamp.com

        -c
        Also if -S and -E are used they must be used together and refer
        to Gregorian/Julian years.
        """
        data = []
        start = options.start
        end = options.end
        lat = options.latitude
        lon = options.longitude
        zone = options.zone
        tz = dtime.timezone(dtime.timedelta(hours=zone))

        for year in range(start, end):
            is_leap = self._is_leap_year(year)

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + is_leap

                for day in range(1, dm + 1):
                    criterion_date = (year, month, day)
                    # Get the Gregorian timestamp for reference.
                    g_date = self.gregorian_date_from_badi_date(criterion_date,
                                                                lat, lon, zone)
                    g_ts = dtime.datetime(*g_date, tzinfo=tz).timestamp()
                    # Timestamp only methods, 1st get UTC timestamp for sunset.
                    ts_ss = self.timestamp_at_sunset(g_date, lat, lon)
                    ts_b_date = self.posix_timestamp(g_ts, lat, lon, zone,
                                                     short=True, trim=True,
                                                     rtd=False)
                    # Julian Period day methods.
                    #print(year, month, day, file=sys.stderr)
                    jd_ts_ss = datetime(year, month, day).timestamp()
                    jd_b_date = self.posix_timestamp(jd_ts_ss, lat, lon, zone,
                                                     short=True, trim=True,
                                                     rtd=False, _chk_on=False)
                    diff = ts_ss - jd_ts_ss
                    data.append((criterion_date, g_date, g_ts, ts_ss,
                                 ts_b_date, jd_ts_ss, jd_b_date, diff))

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
        '-c', '--analyze2', action='store_true', default=False,
        dest='analyze2', help="Analyze timestamps relative to sunset..")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End Badi year of sequence.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start Badi year of sequence.")
    parser.add_argument(
        '-T', '--timezone', type=float, default=0.0, dest='timezone',
        help="Timezone offset floating point value.")
    parser.add_argument(
        '-A', '--latitude', type=float, default=51.477928, dest='latitude',
        help="Latitude")
    parser.add_argument(
        '-O', '--logitude', type=float, default=-0.001545, dest='longitude',
        help="Longitude")
    parser.add_argument(
        '-Z', '--zone', type=float, default=0.0, dest='zone',
        help="Time zone.")

    options = parser.parse_args()
    dt = DatetimeTests()
    ret = 0
    basename = os.path.basename(__file__)

    if options.analyze:  # -a
        print(f"./contrib/misc/{basename} -a")
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
        print(f"./contrib/misc/{basename} -bS {options.start} "
              f"-E {options.end}")
        print(" "*78, "Orig - Badi   Orig - Ord")
        print("Greg Date             JD             Orig Date     Badi Date"
              "     Ordinal Date  B Date Diff   O Date Diff   HMS from JD")
        print('-'*123)
        data = dt.analyze_ordinal_error_create(options)
        total_diff0 = total_diff1 = 0
        items = []
        #print(data, file=sys.stderr)

        for g_date, jd, date, b_date, o_date, diff0, diff1 in data:
            if diff0 != (0, 0, 0):
                total_diff0 += 1

            if diff1 != (0, 0, 0):
                total_diff1 += 1

        [print(f"{str(g_date):21} "
               f"{jd:<14} "
               f"{str(date):13} "
               f"{str(b_date):13} "
               f"{str(o_date):13} "
               f"{str(diff0):13} "
               f"{str(diff1):13} "
               f"{dt._hms_from_decimal_day(jd)}"
               )
         for g_date, jd, b_date, o_date, date, diff0, diff1 in data]
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
    elif options.analyze2:  # -c
        if options.start is None or options.end is None:
            # Set default Badi years.
            options.start = -1842
            options.end = 1162

        start_time = time.time()
        underline_length = 201
        print(f"./contrib/misc/{basename} -cS {options.start} -E {options.end} "
              f"-A {options.latitude} -O {options.longitude} -Z {options.zone}"
              )
        print("Crit Badí' Dt  Gregorian Date", " " * 19, "Gregorian TS       "
              "Sunset Timestamp   Badí' Date (TS)", " " * 16,
              "Sunset TS (JD)      Badí' Date (JD)", " " * 16,
              "Sunset TS Diff      TSD JDD")
        print('-' * underline_length)
        data = dt.analyze_timestamp_errors(options)
        items = []

        # Adjust for Badi calendar
        for (criterion_date, g_date, ts, ts_ss,
             ts_b_date, jd_ts_ss, jd_b_date, diff) in data:
            year, month, day = criterion_date
            #print(day, b_date[2], jd_b_date[2], file=sys.stderr)
            ts_badi_err = day - ts_b_date[2]
            jd_badi_err = day - jd_b_date[2]
            items.append((criterion_date, g_date, ts, ts_ss,
                          ts_b_date, jd_ts_ss, jd_b_date, diff,
                          ts_badi_err, jd_badi_err))

        [print(f"{str(criterion_date):14} "
               f"{str(g_date):34} "
               f"{ts:<18} "
               f"{ts_ss:<18} "
               f"{str(ts_b_date):32} "
               f"{jd_ts_ss:<19} "
               f"{str(jd_b_date):32} "
               f"{diff:<19} "
               f"{ts_badi_err:>3} "
               f"{jd_badi_err:>3}"
               )
         for (criterion_date, g_date, ts, ts_ss, ts_b_date, jd_ts_ss,
              jd_b_date, diff, ts_badi_err, jd_badi_err) in items]
        print('-' * underline_length)
        ss_ts_diff_ts = ss_ts_diff_jd = 0

        for item in items:
            #print(item[-2], item[-1], file=sys.stderr)
            if item[-2] != 0:
                ss_ts_diff_ts += 1

            if item[-1] != 0:
                ss_ts_diff_jd += 1

        print(f"Analyzing year {options.start} to year {options.end} "
              "Gregorian.")
        print(f"                   Total days: {len(items):>4}")
        print(f"Sunset derived from TS errors: {ss_ts_diff_ts:>4}")
        print(f"Sunset derived from JD errors: {ss_ts_diff_jd:>4}")
        print("=" * 35)
        print("                 Total errors: "
              f"{ss_ts_diff_ts + ss_ts_diff_jd:>4}")
    else:
        parser.print_help()

    sys.exit(ret)
