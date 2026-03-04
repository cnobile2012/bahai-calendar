#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/datetime_tests.py
#

import os
import sys
import importlib
import datetime as dtime
from unittest.mock import patch

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from _timestamp import TimestampUtils
from badidatetime import (BahaiCalendar, GregorianCalendar, datetime,
                          date as badi_date, timezone, timedelta)
from badidatetime._timedateutils import _td_utils
badidt = importlib.import_module('badidatetime.datetime')


class DatetimeTests(BahaiCalendar, TimestampUtils):
    GMT_COORDS = (51.477928, -0.001545, 0.0)
    BADI_COORDS = BahaiCalendar._BAHAI_LOCATION[:3]
    LOCAL_COORDS = (35.5894, -78.7792, -5.0)
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
    TIMESTAMP_DATES = (
        # Sunset on day
        (1, 3, 19),
        (1844, 3, 19),
        (1930, 11, 15),
        (1970, 1, 1),
        (1970, 1, 2),
        (2025, 3, 19),
        (2026, 1, 16),
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
        vice versa. This tests the BahaiCalendar.badi_date_from_jd() method.
        Both the jd_from_badi_date() and badi_date_from_jd() methods default
        to the the time zone in Tehran.

        -b
        Also if -S and -E are used they must be used together and refer
        to Badi years.
        """
        lat, lon, zone = self.GMT_COORDS
        data = []
        start = options.start
        end = options.end

        for year in range(start, end):
            is_leap = self._is_leap_year(year)

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + is_leap

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    jd = self.jd_from_badi_date(date, lat, lon, zone)

                    # Get the Badi date from the Julian Period day.
                    b_date = self.badi_date_from_jd(
                        jd, lat, lon, zone, short=True)
                    # Difference of the date converted to a JD then back to
                    # a date again then subtract the converted date from the
                    # original date. They should be the same.
                    diff0 = self._subtract_tuples(b_date, date)

                    # The ordinal date. *** TODO *** Use the new ordinal code.
                    ord = self._ordinal_from_jd(jd)
                    o = datetime.fromordinal(ord, short=True)
                    o_date = (o.year, o.month, o.day)
                    # Difference of the Badi datetime derived from an ordinal
                    # then subtract the derived date original date. They
                    # should be the same.
                    diff1 = self._subtract_tuples(o_date, date)

                    # Get the Gregorian date.
                    g_date = self.gc.gregorian_date_from_jd(
                        jd, hms=True, exact=True)

                    data.append((g_date, jd, ord, date, b_date, o_date,
                                 diff0, diff1))

        return data

    def analyze_timestamp_errors(self, options):
        """
        Find the errors in timestamp conversions to Badi dates. This test
        tries to ensures that the badidatetime.date.today() method changes
        date on sunset based on local time.
        https://www.unixtimestamp.com

        -c with -A, -O, -Z, and -M
        """
        lat = options.latitude
        lon = options.longitude
        zone = options.zone
        # Get min and max minutes.
        mins = options.minutes // 2
        min_delta = - mins
        max_delta = + mins + 1
        data = []

        with patch.object(badidt, 'LOCAL_COORD', (lat, lon, zone)):
            for g_date in self.TIMESTAMP_DATES:
                hist_jd = self.gc.jd_from_gregorian_date(g_date)
                hist_ss = self._sun_setting(hist_jd, lat, lon)
                astro_ss = self._exact_from_meeus(hist_ss)
                ss_ts = (astro_ss - self._POSIX_EPOCH) * self._SECONDS_PER_DAY
                ss_items = (g_date, astro_ss, ss_ts)
                items = []

                for delta in range(min_delta, max_delta):
                    test_jd = astro_ss + (delta / 1440.0)
                    ts = (test_jd - self._POSIX_EPOCH) * self._SECONDS_PER_DAY
                    #ts += 1 if g_date > (1970, 1, 1) else 0
                    today = badi_date.fromtimestamp(ts, short=True)
                    items.append((delta, test_jd, ts, today))

                    #diff_seconds = (test_jd - astro_ss) * 86400
                    #print(diff_seconds)

                data.append((ss_items, items))

        return data

    def check_dates(self, options):
        """
        Check for correct dates between different methods.

        -d with -S and -E (Gregorian) -A -O -Z
        """
        data = []
        start = options.start
        end = options.end
        lat = options.latitude
        lon = options.longitude
        zone = options.zone
        tz = dtime.timezone(dtime.timedelta(hours=zone))

        for year in range(start, end):
            leap = self._gc._is_leap_year(year)

            for month, days in enumerate(self.gc._MONTHS, start=1):
                max_days = days + leap

                for day in range(1, max_days + 1):
                    if year == 1 and (month < 3 or month == 3 and day < 19):
                        continue

                    date = (year, month, day)
                    # Get Badi date from ordinal.
                    g_ord = dtime.datetime(*date, tzinfo=tz).toordinal()
                    dt0 = datetime.fromordinal(g_ord, short=True)
                    b_date0 = self._trim_hms(dt0.b_date + dt0.b_time)
                    # Get Badi date from JD.
                    jd = self.gc.jd_from_gregorian_date(date, exact=True)
                    b_date1 = self.badi_date_from_jd(
                        jd, lat, lon, zone, us=True, short=True)
                    # Get Badi date from the Gregorian date.
                    b_date2 = self.badi_date_from_gregorian_date(
                        date, lat, lon, zone, us=True, short=True)
                    # Get Badi date from timestamp.
                    g_ts = dtime.datetime(*date, tzinfo=tz).timestamp()
                    dt1 = datetime.fromtimestamp(g_ts, short=True)
                    b_date3 = dt1.b_date + dt1.b_time
                    diff0 = [(i, a, b) for i, (a, b) in enumerate(
                        zip(b_date1, b_date2)) if a != b]
                    diff1 = [(i, a, b) for i, (a, b) in enumerate(
                        zip(b_date1, b_date3)) if a != b]
                    data.append((date, b_date0, b_date1, b_date2, b_date3,
                                 diff0, diff1))

        return data

    def test_ordinals(self, options):
        """
        Check that the ordinals are correct.

        -o with -S and -E
        """
        data = []
        start = options.start
        end = options.end
        assert (self.MINYEAR - 1) < start < (self.MAXYEAR + 1), (
            f"Start '{start}' must be from {self.MINYEAR} "
            f"to {self.MAXYEAR}.")
        assert self.MINYEAR < end < (self.MAXYEAR + 2), (
            f"End '{end}' must be from {self.MINYEAR} "
            f"to {self.MAXYEAR}")
        prev_ord = 0

        for year in range(start, end):
            is_leap = self._is_leap_year(year)

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + is_leap

                for day in range(1, dm + 1):
                    date0 = (year, month, day)
                    ordinal = _td_utils._ymd2ord(*date0)
                    date1 = _td_utils._ord2ymd(ordinal, short=True)

                    if (options.previous and
                       (prev_ord != 0 and (prev_ord + 1) != ordinal)):
                        data.append((date0, prev_ord, date1, ordinal))
                    elif not options.previous:
                        flag = date0 != date1

                        if flag:
                            data.append((date0, ordinal, date1, is_leap, flag))

                    prev_ord = ordinal

            if not year % 100:
                print(year, file=sys.stderr)

        return data

    def analyze_timestamp_for_today(self, options):
        """
        See if we get close to the correct time after sunset so we can prove
        that sunset can be found with a timestamp.

        -t -S and -E in Gregorian years
        """
        data = []
        start = options.start
        end = options.end
        #lat = options.latitude
        lon = options.longitude

        if lon:
            zone = lon / 15
        else:
            zone = options.zone

        tz0 = dtime.timezone(dtime.timedelta(hours=zone))
        tz1 = timezone(timedelta(hours=zone))

        for year in range(start, end):
            g_date = (year, 1, 1)
            g_ts = dtime.datetime(*g_date, tzinfo=tz0).timestamp()
            bd = datetime.fromtimestamp(g_ts, tz1)
            b_date = bd.b_date
            b_ts = bd.timestamp()
            diff = b_ts - g_ts
            data.append((g_date, g_ts, b_date, b_ts, diff))

        return data

    def _subtract_tuples(self, t0, t1):
        return t0[0] - t1[0], t0[1] - t1[1], t0[2] - t1[2]


def fmt_float(value, left=4, right=4):
    """
    Format one float so that it is visually centered on the decimal point.

    Parameters
    ----------
    value : float | int | str
        The number to format.
    left : int
        Width to reserve on the left of the decimal (including any minus sign).
    right : int
        Number of digits to show after the decimal.
    """
    s = f"{value:.{right}f}"
    left_part, right_part = s.split(".")
    return f"{left_part.rjust(left)}.{right_part.ljust(right)}"


def find_elapse_time(start_time):
    end_time = time.time()
    days, hours, minutes, seconds = dt._dhms_from_seconds(
        end_time - start_time)
    print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
          f"{round(seconds, 6):02.6} seconds.")


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
        dest='analyze2', help="Analyze timestamps relative to sunset.")
    parser.add_argument(
        '-d', '--date', action='store_true', default=False,
        dest='date', help="Check dates by different methods.")
    parser.add_argument(
        '-o', '--ordinal', action='store_true', default=False,
        dest='ordinal', help="Test that _ymd2ord and _ord2ymd produce "
        "the correct dates.")
    parser.add_argument(
        '-t', '--analyze3', action='store_true', default=False,
        dest='analyze3', help="Analyze timestamps relative to sunset.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End of year of sequence.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start of year of sequence.")
    parser.add_argument(
        '-T', '--timezone', type=float, default=0.0, dest='timezone',
        help="Timezone offset floating point value.")
    parser.add_argument(
        '-A', '--latitude', type=float, default=None, dest='latitude',
        help="Latitude")
    parser.add_argument(
        '-O', '--logitude', type=float, default=None, dest='longitude',
        help="Longitude")
    parser.add_argument(
        '-Z', '--zone', type=float, default=None, dest='zone',
        help="Time zone.")
    parser.add_argument(
        '-P', '--previous', action='store_true', default=False,
        dest='previous', help="Dump the previous and current ordinals.")
    parser.add_argument(
        '-M', '--minutes', type=int, default=None, dest='minutes',
        help="The spread in minutes.")

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
        data = dt.analyze_ordinal_error_create(options)
        underline_length = 149
        print(f"./contrib/misc/{basename} -bS {options.start} "
              f"-E {options.end}")
        print('-' * underline_length)
        print(" " * 123, "Orig - Badi   Orig - Ord")
        print("Greg Date", ' ' * 21, "JD", ' ' * 15, "Ordinal",
              "Orig Date       "
              "Badi Date", ' ' * 22, "Ordinal Date    B Date Diff   "
              "O Date Diff")
        print('-' * underline_length)
        total_diff0 = total_diff1 = 0
        items = []

        for g_date, jd, ord, date, b_date, o_date, diff0, diff1 in data:
            if diff0 != (0, 0, 0):
                total_diff0 += 1

            if diff1 != (0, 0, 0):
                total_diff1 += 1

        [print(f"{str(g_date):31} "
               f"{jd:<18} "
               f"{ord:7} "
               f"{str(date):15} "
               f"{str(b_date):32} "
               f"{str(o_date):15} "
               f"{str(diff0):13} "
               f"{str(diff1):13} "
               )
         for g_date, jd, ord, date, b_date, o_date, diff0, diff1 in data]
        print('-' * underline_length)
        total_errors = total_diff0 + total_diff1
        print(f"Analyzing year {options.start} to year {options.end-1}.")
        print(f"Ordinal Errors: {total_diff1}")
        print(f"   Badi Errors: {total_diff0}")
        print(f"  Total Errors: {total_errors}")
        errors = []

        for g_date, jd, ord, date, b_date, o_date, diff0, diff1 in data:
            if diff0 != (0, 0, 0):
                errors.append((date, jd, ord, b_date[:3], diff0, ''))

            if diff1 != (0, 0, 0):
                errors.append((date, jd, ord, o_date, '', diff1))

        if errors:
            print("\nDate            JD                 Ordinal "
                  "Offending Date  Badi Diff 0  Ordinal Diff 1")
            print('-' * 86)
            [print(f"{str(date):15} "
                   f"{jd:<18} "
                   f"{ord:7} "
                   f"{str(offending_date):15} "
                   f"{str(diff0):13}"
                   f"{str(diff1):13}"
                   )
             for date, jd, ord, offending_date, diff0, diff1 in errors]
            print('-' * 86)

        find_elapse_time(start_time)
    elif options.analyze2:  # -c
        delta = options.minutes
        assert delta < 119, ("The minutes option cannot be more that 118, "
                             f"found {delta}.")
        start_time = time.time()
        underline_length = 114
        print(f"./contrib/misc/{basename} -cA {options.latitude} "
              f"-O {options.longitude} -Z {options.zone} -M {delta}")
        print('-' * underline_length)
        print("Gregorian Date Astro Sunset JD    Badí' Timestamp   "
              "Minute Offset Astro JD           Day Timestamp     Today")
        print('-' * underline_length)
        data = dt.analyze_timestamp_errors(options)

        for ss_items, items in data:
            g_date, astro_ss, ss_ts = ss_items
            print(f"{str(g_date):14} "
                  f"{fmt_float(astro_ss, 7, 10)} "
                  f"{fmt_float(ss_ts, 12, 4)} ",
                  end='')
            items_len = len(items)

            for idx, (delta, test_jd, ts, today) in enumerate(items):
                print(f"{delta:>2}            "
                      f"{fmt_float(test_jd, 7, 10)} "
                      f"{fmt_float(ts, 12, 4)} "
                      f"{str(today):>11} "
                      )

                if idx < items_len - 1:
                    print(" " * 52, end='')

            print('-' * underline_length)

        find_elapse_time(start_time)
    elif options.date:  # -d
        if None in (options.start, options.end):
            print("If option -d is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        elif None in (options.latitude, options.longitude, options.zone):
            print("If option -d is used, -A, -O and -Z must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            start_time = time.time()
            data = dt.check_dates(options)
            print(f"./contrib/misc/{basename} -dS {options.start} "
                  f"-E {options.end} -A {options.latitude} "
                  f"-O {options.longitude} -Z {options.zone}")
            underline_length = 215
            print('-' * underline_length)
            print("Gregorian Date|Badi Date from |"
                  "Badi Date from JD",
                  ' ' * 16, "|Badi Date from Gregorian",
                  ' ' * 9, "|Badi Date from Timestamp",
                  ' ' * 9, "|Tuples are:")
            print("at Midnight   |Ordinal        |", ' ' * 33, "|",
                  ' ' * 33, "|", ' ' * 33, "|(idx, diff val 0, diff val 1)")
            print('-' * underline_length)
            [print(f"{str(date):14} "
                   f"{str(b_date0):15} "
                   f"{str(b_date1):35} "
                   f"{str(b_date2):35} "
                   f"{str(b_date3):35} "
                   f"{diff0} "
                   f"{diff1}"
                   ) for (date, b_date0, b_date1, b_date2,
                          b_date3, diff0, diff1) in data]
            print('-' * underline_length)
            find_elapse_time(start_time)
    elif options.ordinal:  # -o
        if options.start is None or options.end is None:
            print("If option -o is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        elif options.previous:  # with -P
            start_time = time.time()
            data = dt.test_ordinals(options)
            print(f"./contrib/misc/{basename} -oPS{options.start} "
                  f"-E{options.end}")
            print("Start Date      Prev Ord Result Date     Cur Ord")
            underline_length = 48
            print('-' * underline_length)
            [print(f"{str(date0):15}  "
                   f"{prev_ord:>7} "
                   f"{str(date1):15} "
                   f"{ordinal:>7} "
                   ) for date0, prev_ord, date1, ordinal in data]
            print('-' * underline_length)
            end_time = time.time()
            days, hours, minutes, seconds = dt._dhms_from_seconds(
                end_time - start_time)
            print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
                f"{round(seconds, 6):02.6} seconds.")
        else:
            start_time = time.time()
            data = dt.test_ordinals(options)
            print(f"./contrib/misc/{basename} -oS{options.start} "
                  f"-E{options.end}")
            print("Start Date      Ordinal Result Date     Leap  Error")
            underline_length = 51
            print('-' * underline_length)
            [print(f"{str(date0):15} "
                   f"{ordinal:>7} "
                   f"{str(date1):15} "
                   f"{str(leap):5} "
                   f"{str(flag):5}"
                   ) for date0, ordinal, date1, leap, flag in data]
            print('-' * underline_length)
            print(f"    Total Years Tested: {options.end - options.start}")
            errors = [l[4] is True for l in data].count(True)
            print(f"Total Number of Errors: {errors}")
            find_elapse_time(start_time)
    elif options.analyze3:  # -t
        if options.start is None or options.end is None:
            # Set default Gregorian years.
            options.start = 2  # Year 1 would be before that oldest Badi date.
            options.end = 3005

        start_time = time.time()
        underline_length = 76
        lon = options.longitude
        zone = lon / 14 if options.zone is None else options.zone
        lon_text = "" if lon is None else f" -O {lon}"
        print(f"./contrib/misc/{basename} -tS {options.start} "
              f"-E {options.end}{lon_text} -Z {zone}")
        print("Gregorian DT Greg TS        Badi Date      Badi Timestamp      "
              "Diff")
        print('-' * underline_length)
        data = dt.analyze_timestamp_for_today(options)
        [print(f"{str(g_date):12} "
               f"{fmt_float(g_ts, 12, 1)} "
               f"{str(b_date):14} "
               f"{fmt_float(b_ts, 12, 6)} "
               f"{fmt_float(diff, 6, 6)}"
               )
         for g_date, g_ts, b_date, b_ts, diff in data]
        print('-' * underline_length)
        deviation = {'n': 0, 'p': 0, 'max_n': 0, 'max_p': 0, 'total': 0}

        for items in data:
            diff = items[-1]

            if diff < 0:
                deviation['n'] += 1
            elif diff > 0:
                deviation['p'] += 1

            if diff < deviation['max_n']:
                deviation['max_n'] = diff
            elif diff > deviation['max_p']:
                deviation['max_p'] = diff

            deviation['total'] += diff

        total_years = options.end - options.start
        mean_deviation = deviation['total'] / total_years
        print(f"Analyzing year {options.start} to year {options.end-1} "
              "Gregorian.")
        print(f"Total years:                          {total_years}")
        print(f"Total Timestamp negative deviations:  {deviation['n']}")
        print(f"Maximum negative deviation (seconds): {deviation['max_n']}")
        print(f"Total Timestamp positive deviations:  {deviation['p']}")
        print(f"Maximum positive deviation (seconds): {deviation['max_p']}")
        print(f"Mean deviation (seconds):             {mean_deviation}")
        find_elapse_time(start_time)
    else:
        parser.print_help()

    sys.exit(ret)
