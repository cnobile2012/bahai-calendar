#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import sys
import math
import pprint
from datetime import timedelta

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)


class JulianPeriodTests:

    JULIAN_YEAR = 365.25
    GREGORIAN_EPOCH = 1721423.5
    MEAN_TROPICAL_YEAR = 365.2421897
    MEAN_SIDEREAL_YEAR = 365.256363004
    ROUNDING_PLACES = 6
    GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)
    GREGORIAN_LEAP_YEAR_ALT = lambda self, year: (
        (year % 4 == 0) * (year % 128 != 0) == 1)
    _MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    TEST_DATES = (
        (1, 1, 1),
        (1, 3, 20),
        (2, 1, 1),
        (3, 1, 1),
        (4, 1, 1),
        (5, 1, 1),
        (6, 1, 1),
        (8, 1, 1),
        (9, 1, 1),
        (100, 2, 28),
        (100, 2, 29), # Should be wrong for standard leap year
        (100, 3, 1),
        (128, 2, 28),
        (128, 2, 29), # Should be wrong for alternitive leap year
        (128, 3, 1),
        (200, 2, 28),
        (200, 2, 29), # Should be wrong for standard leap year
        (200, 3, 1),
        (256, 2, 28),
        (256, 2, 29), # Should be wrong for alternitive leap year
        (256, 3, 1),
        (300, 2, 28),
        (300, 2, 29), # Should be wrong for standard leap year
        (300, 3, 1),
        (384, 2, 28),
        (384, 2, 29), # Should be wrong for alternitive leap year
        (384, 3, 1),
        (400, 2, 28),
        (400, 2, 29), # Should be wrong for standard leap year
        (400, 3, 1),
        (800, 1, 1),
        (800, 12, 31),
        (1200, 1, 1),
        (1200, 12, 31),
        (1300, 1, 1),
        (1300, 12, 31),
        (1400, 1, 1),
        (1400, 12, 31),
        (1500, 1, 1),
        (1500, 12, 31),
        (1582, 10, 4),
        (1582, 10, 5),
        (1582, 10, 6),
        (1582, 10, 7),
        (1582, 10, 8),
        (1582, 10, 9),
        (1582, 10, 10),
        (1582, 10, 11),
        (1582, 10, 12),
        (1582, 10, 13),
        (1582, 10, 14),
        (1582, 10, 15),
        (1844, 3, 20),
        (2020, 12, 7.25),
        (2024, 3, 20),
        )

    def jd_from_gregorian_date_0(self, g_date):
        """
        Meeus
        """
        year, month, day = g_date[:3]

        if month <= 2:
            year -= 1
            month += 12

        if (year, month, day) >= (1582, 10, 15):
            a = math.floor(year / 100)
            b = 2 - a + math.floor(a / 4)
        else:
            b = 0

        return round(math.floor(self.JULIAN_YEAR * year) + math.floor(
            30.6001 * (month + 1)) + day + b + 1720994.5,
                     self.ROUNDING_PLACES)

    def jd_from_gregorian_date_1(self, g_date, alt=False):
        """
        Mine
        """
        # Check for valid Gregorian date here
        GLY = self.GREGORIAN_LEAP_YEAR_ALT if alt else self.GREGORIAN_LEAP_YEAR
        year, month, day = g_date[:3]
        #y = self.MEAN_SIDEREAL_YEAR * (year - 1) # 365.256363004 Too high
        #y = self.MEAN_TROPICAL_YEAR * (year - 1) # Way too low
        y = self.JULIAN_YEAR * (year - 1)
        y = math.floor(y)
        month_days = list(self._MONTHS)
        month_days[1] = 29 if GLY(year) else 28
        md = sum([v for v in month_days[:month-1]])
        md += day + self.GREGORIAN_EPOCH - 1
        return round(y + md, self.ROUNDING_PLACES)

    def jd_from_gregorian_date_1a(self, g_date):
        """
        Meeus Kinda
        """
        year, month, day = g_date[:3]

        if month <= 2:
            year -= 1
            month += 12

        if (year, month, day) <= (1582, 10, 15):
            day -= 2

        a = math.floor(year / 100)
        b = 2 - a + math.floor(a / 4)
        return round(math.floor(self.JULIAN_YEAR * year) + math.floor(
            30.6001 * (month + 1)) + day + b + 1720994.5,
                     self.ROUNDING_PLACES)

    def jd_from_gregorian_date_2(self, g_date):
        """
        Wikipedia
        """
        year, month, day = g_date[:3]
        return (math.floor(1461 * (year + 4800 + (month - 14) / 12)) / 4 +
                math.floor(367 * (month - 2 - 12 * ((month - 14) / 12))) / 12 -
                math.floor(3 * ((year + 4900 + (month - 14) / 12) / 100)) / 4 +
                day - 32075)

    def jd_from_gregorian_date_3(self, g_date):
        """
        Orbital
        """
        year, month, day = g_date[:3]
        a = int((month - 14) / 12)
        b = 1461 * (year + 4800 + a)
        c = 367 * (month - 2 - 12 * a)
        e = int((year + 4900 + a) / 100)
        return (int(b / 4) + int(c / 12) - int(3 * e / 4) + day - 32075)

    def gregorian_date_from_jd_0(self, jd):
        """
        Convert Julian day to Gregorian date.
        """
        j_day = jd + 0.5
        z = math.floor(j_day)
        f = j_day % 1

        if z >= 2299161: # 1582-10-15 Julian and Gregorian crossover.
            alpha = math.floor((z - 1867216.25) / 36524.25)
            a = z + 1 + alpha - math.floor(alpha / 4)
        else:
            a = z

        b = a + 1524
        c = math.floor((b - 122.1) / 365.25)
        d = math.floor(365.25 * c)
        e = math.floor((b - d) / 30.6001)
        day = b - d - math.floor(30.6001 * e) + f
        month = 0
        year = 0

        if e > 13:
            month = e - 13
        else:
            month = e - 1

        if month > 2:
            year = c - 4716
        else:
            year = c - 4715

        return year, month, round(day, self.ROUNDING_PLACES)

    def gregorian_date_from_jd_1(self, jd, alt=False):
        GLY = self.GREGORIAN_LEAP_YEAR_ALT if alt else self.GREGORIAN_LEAP_YEAR
        md = jd - (self.GREGORIAN_EPOCH - 1)
        y = math.floor(md / self.JULIAN_YEAR)
        days = md - (y * self.JULIAN_YEAR)
        year = y + 1
        leap = GLY(year)
        month_days = list(self._MONTHS)
        month_days[1] = 29 if GLY(year) else 28
        d = day = 0

        for month, ds in enumerate(month_days, start=1):
            d += ds
            if days > d: continue
            day = math.ceil(days - (d - ds))
            break

        return year, month, day + (jd % 1) - 0.5

    def gregorian_date_from_jd_3(self, jdn):
        """
        Convert the Julian Day Number to the proleptic Gregorian Year,
        Month, Day.
        """
        L = jdn + 68569
        N = math.floor(4 * L / 146_097)
        L = L - math.floor((146097 * N + 3) / 4)
        I = math.floor(4000 * (L + 1) / 1_461_001)
        L = L - math.floor(1461 * I / 4) + 31
        J = math.floor(80 * L / 2447)
        day = L - math.floor(2447 * J / 80)
        L = math.floor(J / 11)
        month = J + 2 - 12 * L
        year = 100 * (N - 49) + I + L
        return year, month, day

    def analyze(self, alt=False):
        GLY = self.GREGORIAN_LEAP_YEAR_ALT if alt else self.GREGORIAN_LEAP_YEAR
        data = []
        values = range(1, 1583)

        for date in self.TEST_DATES:
            date = date if isinstance(date, tuple) else (date, 1, 1)
            leap = GLY(date[0])
            jd0 = self.jd_from_gregorian_date_0(date)
            jd1 = self.jd_from_gregorian_date_1(date, alt=alt)
            #jd1 = self.jd_from_gregorian_date_1a(date)
            #jd2 = self.jd_from_gregorian_date_2(date)
            #jd3 = self.jd_from_gregorian_date_3(date)

            if not alt and date[1:] == (2, 29) and not leap:
                gd0 = 'INVALID'
            else:
                gd0 = self.gregorian_date_from_jd_0(jd0)

            if alt and date[1:] == (2, 29) and not leap:
                gd1 = 'INVALID'
            else:
                gd1 = self.gregorian_date_from_jd_1(jd1, alt=alt)

            data.append((date, leap, jd0, jd1, gd0, gd1))

        return data

    def analyze_1(self, start, end, alt=False):
        data = []
        GLY = self.GREGORIAN_LEAP_YEAR_ALT if alt else self.GREGORIAN_LEAP_YEAR
        data = []

        for year in range(start, end + 1):
            leap = GLY(year)

            for month, days in enumerate(self._MONTHS, start=1):
                if month == 2 and not leap:
                    max_days = days - 1
                else:
                    max_days = days

                for day in range(1, max_days + 1):
                    date = (year, month, day)
                    jd0 = self.jd_from_gregorian_date_0(date)
                    jd1 = self.jd_from_gregorian_date_1(date, alt=alt)
                    gd0 = self.gregorian_date_from_jd_0(jd0)
                    gd1 = self.gregorian_date_from_jd_0(jd1)
                    data.append((date, leap, jd0, jd1, gd0, gd1))

        return data

    def compare_leap_year_algorithms(self, year=None):
        """                           std alt
        Year  100 GLY_STD 0 GLY_ALT 1  1
        Year  128 GLY_STD 1 GLY_ALT 0      1
        Year  200 GLY_STD 0 GLY_ALT 1  2
        Year  256 GLY_STD 1 GLY_ALT 0      2
        Year  300 GLY_STD 0 GLY_ALT 1  3
        Year  384 GLY_STD 1 GLY_ALT 0      3
        Year  500 GLY_STD 0 GLY_ALT 1  4
        Year  512 GLY_STD 1 GLY_ALT 0      4
        Year  600 GLY_STD 0 GLY_ALT 1  5
        Year  640 GLY_STD 1 GLY_ALT 0      5
        Year  700 GLY_STD 0 GLY_ALT 1  6
        Year  768 GLY_STD 1 GLY_ALT 0      6
        Year  896 GLY_STD 1 GLY_ALT 0      7
        Year  900 GLY_STD 0 GLY_ALT 1  7
        Year 1000 GLY_STD 0 GLY_ALT 1  8
        Year 1024 GLY_STD 1 GLY_ALT 0      8
        Year 1100 GLY_STD 0 GLY_ALT 1  9
        Year 1152 GLY_STD 1 GLY_ALT 0      9
        Year 1280 GLY_STD 1 GLY_ALT 0      10
        Year 1300 GLY_STD 0 GLY_ALT 1  10
        Year 1400 GLY_STD 0 GLY_ALT 1  11
        Year 1408 GLY_STD 1 GLY_ALT 0      11   Up to 1482-10-15
        --------------------------------------------------------
        Year 1500 GLY_STD 0 GLY_ALT 1
        Year 1536 GLY_STD 1 GLY_ALT 0
        Year 1664 GLY_STD 1 GLY_ALT 0
        Year 1700 GLY_STD 0 GLY_ALT 1
        Year 1792 GLY_STD 1 GLY_ALT 0
        Year 1800 GLY_STD 0 GLY_ALT 1
        Year 1900 GLY_STD 0 GLY_ALT 1
        Year 1920 GLY_STD 1 GLY_ALT 0
        Year 2048 GLY_STD 1 GLY_ALT 0
        Year 2100 GLY_STD 0 GLY_ALT 1
        Year 2176 GLY_STD 1 GLY_ALT 0
        Year 2200 GLY_STD 0 GLY_ALT 1
        Year 2300 GLY_STD 0 GLY_ALT 1
        Year 2304 GLY_STD 1 GLY_ALT 0
        Year 2432 GLY_STD 1 GLY_ALT 0
        Year 2500 GLY_STD 0 GLY_ALT 1
        Year 2560 GLY_STD 1 GLY_ALT 0
        Year 2600 GLY_STD 0 GLY_ALT 1
        Year 2688 GLY_STD 1 GLY_ALT 0
        Year 2700 GLY_STD 0 GLY_ALT 1
        Year 2816 GLY_STD 1 GLY_ALT 0
        Year 2900 GLY_STD 0 GLY_ALT 1
        Year 2944 GLY_STD 1 GLY_ALT 0
        Year 3000 GLY_STD 0 GLY_ALT 1
        """
        data = []

        if year <= 0:
            for y in range(1, 3004):
                y0 = self.GREGORIAN_LEAP_YEAR(y)
                y1 = self.GREGORIAN_LEAP_YEAR_ALT(y)

                if y0 != y1:
                    data.append((y, y0, y1))
        else:
            y0 = self.GREGORIAN_LEAP_YEAR(year)
            y1 = self.GREGORIAN_LEAP_YEAR_ALT(year)
            data.append((year, y0, y1))

        return data


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test Julian Period methods."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Generate a list of Julian Period days using different methods.")
    parser.add_argument(
        '-1', '--analyze-1', action='store_true', default=False,
        dest='analyze_1',
        help=("Generate a list of Julian Period days for different start "
              "and end year."))
    parser.add_argument(
        '-c', '--compare', type=int, dest='compare',
        help=("Compare two algorithms for determining the leap year. "
              "A value <= 0 processes dates from 1 to 3004"))
    parser.add_argument(
        '-A', '--alt-leap', action='store_true', default=False,
        dest='alt_leap', help="Use alternative leap year metnod.")
    parser.add_argument(
        '-S', '--start', type=int, default=0, dest='start',
        help="Start year of sequence.")
    parser.add_argument(
        '-E', '--end', type=int, default=0, dest='end',
        help="End year of sequence.")
    parser.add_argument(
        '-D', '--debug', action='store_true', default=False, dest='debug',
        help="Run in debug mode.")
    options = parser.parse_args()

    jpt = JulianPeriodTests()
    ret = 0

    if options.debug:
        sys.stderr.write("DEBUG--options: {}\n".format(options))

    if options.analyze:
        data = [f"{str(date):<17} "
                f"{str(leap):<5} "
                f"{meeus:<10} "
                #f"{wiki:<10} "
                #f"{orb:<10} "
                f"{str(gd0):<17} "
                f"{mine:<10} "
                f"{str(gd1):<17} "
                #f"{str(gd2):<17} "
                #f"{str(gd3):<17} "
                for (date,
                     leap,
                     meeus,
                     mine,
                     #wiki,
                     #orb,
                     gd0,
                     gd1) in jpt.analyze(alt=options.alt_leap)]
        print("Start date        "
              "Leap  "
              "Meeus      "
              "Mine       "
              #"Wiki       "
              #"Orbital    "
              "Meeus             "
              "Mine              "
              #"Wiki              "
              #"Orbital"
              )
        [print(item) for item in data]
    elif options.analyze_1:
        if options.start == 0 or options.end == 0:
            print("If option -1 is used, -S and -E must also be used.")
            ret = 1
        else:
            data = [f"{str(date):<13} "
                    f"{str(leap):<5} "
                    f"{meeus:<10} "
                    f"{mine:<10} "
                    f"{str(gd0):<14} "
                    f"{str(gd1):<14} "
                    f"{mine - meeus:<4}"
                    for (date,
                         leap,
                         meeus,
                         mine,
                         gd0,
                         gd1) in jpt.analyze_1(options.start, options.end,
                                               options.alt_leap)]
            print("Start date    "
                  "Leap  "
                  "Meeus      "
                  "Mine       "
                  "Meeus          "
                  "Mine           "
                  "Diff"
                  )
            [print(item) for item in data]
    elif isinstance(options.compare, int):
        data = [
            f"Year {year:>4} "
            f"GLY_STD {gly_std:<1} "
            f"GLY_ALT {gly_alt:<1}"
            for year, gly_std, gly_alt in jpt.compare_leap_year_algorithms(
                options.compare)]
        [print(item) for item in data]
    else:
        parser.print_help()

    sys.exit(ret)
