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
    MEAN_TROPICAL_YEAR = 365.242189
    MEAN_SOLAR_YEAR = 365.2421897
    MEAN_SOLAR_MONTH = round(MEAN_SOLAR_YEAR / 12, 10)
    ROUNDING_PLACES = 6
    GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)
    GREGORIAN_LEAP_YEAR_ALT = lambda self, year: (
        (year % 4 == 0) * (year % 128 != 0) == 1)
    _MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    TEST_DATES = (
        (1, 1, 1),
        (2, 1, 1),
        (3, 1, 1),
        (4, 1, 1),
        (5, 1, 1),
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
        year, month, day = g_date[:3]
        sys.stderr.write(str(self.MEAN_SOLAR_YEAR * (year - 1)) + ', ')
        y = math.floor(self.MEAN_SOLAR_YEAR * (year - 1))

        if month >= 2:
            m = sum([v for v in self._MONTHS[:month-1]])

            if alt:
                d = 0 if self.GREGORIAN_LEAP_YEAR_ALT(year) else -1
            else:
                d = 0 if self.GREGORIAN_LEAP_YEAR(year) else -1
        else:
            m = 0
            d = 0

        sys.stderr.write(str(m) + ', ')
        d += day + self.GREGORIAN_EPOCH - 1
        sys.stderr.write(str(d) + '\n')
        return round(y + m + d, self.ROUNDING_PLACES)

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

    def analyze(self):
        data = []

        for date in self.TEST_DATES:
            jd0 = self.jd_from_gregorian_date_0(date)
            jd1 = self.jd_from_gregorian_date_1(date, True)
            jd2 = self.jd_from_gregorian_date_2(date)
            jd3 = self.jd_from_gregorian_date_3(date)
            dates = []

            for jd in (jd0, jd1, jd2, jd3):
                gd0 = self.gregorian_date_from_jd_0(jd)
                #gd3 = self.gregorian_date_from_jd_3(jd)
                dates.append(gd0)
                #dates.append((gd0, gd3))

            data.append((date, jd0, jd1, jd2, jd3, dates))

        return data

    def compare_leap_year_algorithms(self, year=None):
        """
        Year  100 GLY 0 GLY_ALT 1
        Year  128 GLY 1 GLY_ALT 0
        Year  200 GLY 0 GLY_ALT 1
        Year  256 GLY 1 GLY_ALT 0
        Year  300 GLY 0 GLY_ALT 1
        Year  384 GLY 1 GLY_ALT 0
        Year  500 GLY 0 GLY_ALT 1
        Year  512 GLY 1 GLY_ALT 0
        Year  600 GLY 0 GLY_ALT 1
        Year  640 GLY 1 GLY_ALT 0
        Year  700 GLY 0 GLY_ALT 1
        Year  768 GLY 1 GLY_ALT 0
        Year  896 GLY 1 GLY_ALT 0
        Year  900 GLY 0 GLY_ALT 1
        Year 1000 GLY 0 GLY_ALT 1
        Year 1024 GLY 1 GLY_ALT 0
        Year 1100 GLY 0 GLY_ALT 1
        Year 1152 GLY 1 GLY_ALT 0
        Year 1280 GLY 1 GLY_ALT 0
        Year 1300 GLY 0 GLY_ALT 1
        Year 1400 GLY 0 GLY_ALT 1
        Year 1408 GLY 1 GLY_ALT 0
        Year 1500 GLY 0 GLY_ALT 1
        Year 1536 GLY 1 GLY_ALT 0
        Year 1664 GLY 1 GLY_ALT 0
        Year 1700 GLY 0 GLY_ALT 1
        Year 1792 GLY 1 GLY_ALT 0
        Year 1800 GLY 0 GLY_ALT 1
        Year 1900 GLY 0 GLY_ALT 1
        Year 1920 GLY 1 GLY_ALT 0
        Year 2048 GLY 1 GLY_ALT 0
        Year 2100 GLY 0 GLY_ALT 1
        Year 2176 GLY 1 GLY_ALT 0
        Year 2200 GLY 0 GLY_ALT 1
        Year 2300 GLY 0 GLY_ALT 1
        Year 2304 GLY 1 GLY_ALT 0
        Year 2432 GLY 1 GLY_ALT 0
        Year 2500 GLY 0 GLY_ALT 1
        Year 2560 GLY 1 GLY_ALT 0
        Year 2600 GLY 0 GLY_ALT 1
        Year 2688 GLY 1 GLY_ALT 0
        Year 2700 GLY 0 GLY_ALT 1
        Year 2816 GLY 1 GLY_ALT 0
        Year 2900 GLY 0 GLY_ALT 1
        Year 2944 GLY 1 GLY_ALT 0
        Year 3000 GLY 0 GLY_ALT 1
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
        '-c', '--compare', type=int, dest='compare',
        help=("Compare two algorithms for determining the leap year. "
              "A value <= 0 processes dates from 1 to 3004"))
    parser.add_argument(
        '-D', '--debug', action='store_true', default=False, dest='debug',
        help="Run in debug mode.")
    options = parser.parse_args()

    jpt = JulianPeriodTests()

    if options.debug:
        sys.stderr.write("DEBUG--options: {}\n".format(options))

    if options.analyze:
        data = [f"{str(date):<17} "
                f"{meeus:<10} "
                f"{mine:<10} "
                f"{wiki:<10} "
                f"{orb:<10} "
                f"{str(gd[0]):<17} "
                f"{str(gd[1]):<17} "
                f"{str(gd[2]):<17} "
                f"{str(gd[3]):<17} "
                for (date, meeus, mine, wiki, orb, gd) in jpt.analyze()]
        print("Start date        Meeus      Mine       Wiki       Orbital"
              "    Meeus             Mine              Wiki"
              "              Orbital")
        [print(item) for item in data]
    elif isinstance(options.compare, int):
        data = [
            f"Year {year:>4} "
            f"GLY {gly:<1} "
            f"GLY_ALT {gly_alt:<1}"
            for year, gly, gly_alt in jpt.compare_leap_year_algorithms(
                options.compare)]
        [print(item) for item in data]
    else:
        parser.print_help()

    sys.exit(0)
