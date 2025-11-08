#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import sys
import math

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar


class JulianPeriodTests:

    JULIAN_YEAR = 365.25
    GREGORIAN_EPOCH = 1721423.5
    MEAN_TROPICAL_YEAR = 365.2421897
    #MEAN_SIDEREAL_YEAR = 365.256363004
    ROUNDING_PLACES = 6
    JULIAN_LEAP_YEAR = lambda self, year: year % 4 == 0
    GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)
    GREGORIAN_LEAP_YEAR_ALT = lambda self, year: (
        (year % 4 == 0) * (year % 128 != 0) == 1)
    MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    HR = lambda self, x: x / 24
    MN = lambda self, x: x / 24 / 60
    SEC = lambda self, x: x / 24 / 60 / 60

    TEST_DATES = (
        (-4712, 1, 1.5),
        (1, 1, 1),
        (1, 3, 20),
        (2, 1, 1), (3, 1, 1), (4, 1, 1),
        (4, 12, 31), (5, 1, 1),
        (6, 1, 1), (8, 1, 1), (9, 1, 1),
        # The 29th should be wrong for standard leap year
        (100, 2, 28), (100, 2, 29), (100, 3, 1),
        # The 29th should be wrong for alternative leap year
        (128, 2, 28), (128, 2, 29), (128, 3, 1),
        # The 29th should be wrong for standard leap year
        (200, 2, 28), (200, 2, 29), (200, 3, 1),
        # The 29th should be wrong for alternative leap year
        (256, 2, 28), (256, 2, 29), (256, 3, 1),
        # The 29th should be wrong for standard leap year
        (300, 2, 28), (300, 2, 29), (300, 3, 1),
        # The 29th should be wrong for alternative leap year
        (384, 2, 28), (384, 2, 29), (384, 3, 1),
        # The 29th should be wrong for standard leap year
        (400, 2, 28), (400, 2, 29), (400, 3, 1),
        (800, 1, 1), (800, 12, 31),
        (1200, 1, 1), (1200, 12, 31),
        (1300, 1, 1), (1300, 12, 31),
        (1400, 1, 1), (1400, 12, 31),
        (1500, 1, 1), (1500, 12, 31),
        (1582, 10, 4), (1582, 10, 5), (1582, 10, 6), (1582, 10, 7),
        (1582, 10, 8), (1582, 10, 9), (1582, 10, 10), (1582, 10, 11),
        (1582, 10, 12), (1582, 10, 13), (1582, 10, 14), (1582, 10, 15),
        (1844, 3, 20),
        (1957, 10, 4.81),
        (2020, 12, 7), (2020, 12, 7.1), (2020, 12, 7.25), (2020, 12, 7.49),
        (2020, 12, 8),
        (2024, 3, 20),
        (3004, 12, 31),
        # Off by one issue
        ## (100, 12, 30), (100, 12, 31),
        ## (101, 1, 1), (101, 1, 2), (101, 12, 30), (101, 12, 31),
        ## (102, 1, 1), (102, 1, 2), (102, 12, 30), (102, 12, 31),
        ## (104, 1, 1), (108, 1, 1),
        ## (200, 12, 30), (200, 12, 31),
        ## (201, 1, 1), (201, 1, 2),
        ## (300, 12, 30), (300, 12, 31),
        ## (301, 1, 1), (301, 1, 2),
        ## (400, 12, 30), (400, 12, 31),
        ## (401, 1, 1), (401, 1, 2),
        ## # The 29th should be wrong for standard leap year
        ## (500, 2, 28), (500, 2, 29), (500, 3, 1),
        ## (500, 12, 30), (500, 12, 31),
        ## (501, 1, 1), (501, 7, 1),
        ## (600, 12, 30), (600, 12, 31),
        ## (601, 1, 1),
        ## # The 29th should be wrong for standard leap year
        ## (700, 2, 28), (700, 2, 29), (700, 3, 1),
        ## (700, 12, 30), (700, 12, 31),
        ## (701, 1, 1),
        ## (800, 12, 30), (800, 12, 31),
        ## (801, 1, 1), (801, 1, 2),
        ## # The 29th should be wrong for standard leap year
        ## (900, 2, 28), (900, 2, 29), (900, 3, 1),
        ## (900, 12, 31), (901, 1, 1), (901, 1, 2),
        ## # The 29th should be wrong for standard leap year
        ## (1000, 2, 28), (1000, 2, 29), (1000, 3, 1),
        ## (1000, 12, 31), (1001, 1, 1),
        ## # The 29th should be wrong for standard leap year
        ## (1100, 2, 28), (1100, 2, 29), (1100, 3, 1),
        ## (1100, 12, 31), (1101, 1, 1), #(1100, 1, 2),
        ## (1200, 12, 31), (1201, 1, 1), (1201, 1, 2),
        ## (2024, 1, 1), (2024, 1, 2), (2024, 1, 3),
        ## # The 29th should be wrong for standard leap year
        ## (1300, 2, 28), (1300, 2, 29), (1300, 3, 1),
        ## (1300, 12, 31), (1301, 1, 1),
        ## (1400, 12, 31), (1401, 1, 1),
        ## (1500, 12, 31), (1501, 1, 1),
        ## # The 29th should be wrong for standard leap year
        ## (1500, 2, 28), (1500, 2, 29), (1500, 3, 1),
        ## (1600, 12, 31), (1601, 1, 1),
        ## (1700, 12, 31), (1701, 1, 1), # 11
        ## (1800, 12, 31), (1801, 1, 1), # 12
        ## (1900, 12, 31), (1901, 1, 1), # 13
        ## (2000, 1, 1, 12),
        ## (2000, 12, 31), (2001, 1, 1), # 13
        ## (2100, 12, 31), (2101, 1, 1), # 14
        ## (2200, 12, 31), (2201, 1, 1), # 15
        ## (2300, 12, 31), (2301, 1, 1), # 16
        ## (2400, 12, 31), (2401, 1, 1), # 16
        ## (2500, 12, 31), (2501, 1, 1), # 17
        ## (2600, 12, 31), (2601, 1, 1), # 18
        ## (2700, 12, 31), (2701, 1, 1), # 19
        ## (2800, 12, 31), (2801, 1, 1), # 19
        ## (2900, 12, 31), (2901, 1, 1), # 20
        ## (3000, 12, 31), (3001, 1, 1), # 21
        ## (3100, 12, 31), (3101, 1, 1), # 22
        ## (3200, 12, 31), (3201, 1, 1),
        )

    def __init__(self):
        self._bc = BahaiCalendar()
        self._gc = GregorianCalendar()

    def analyze(self, options):
        """
        Check that Gregorian dates can be converted to a Julian Period
        day then back to a Gregorian dates correctly.

        -a with optional -A for alternate leap year calculation.
        """
        GLY = (self.GREGORIAN_LEAP_YEAR_ALT if options.alt_leap
               else self.GREGORIAN_LEAP_YEAR)
        data = []

        for date in self.TEST_DATES:
            date = date if isinstance(date, tuple) else (date, 1, 1)
            leap = GLY(date[0])
            jd0 = self.jd_from_gregorian_date_0(date)
            jd1 = self.jd_from_gregorian_date_1(date, alt=options.alt_leap)

            if not options.alt_leap and date[1:] == (2, 29) and not leap:
                gd0 = 'INVALID'
            else:
                gd0 = self.gregorian_date_from_jd_0(jd0)

            if date[1:] == (2, 29) and not leap:
                gd1 = 'INVALID'
            else:
                gd1 = self.gregorian_date_from_jd_1(jd1, alt=options.alt_leap)
            #                        Meeus     Mine
            data.append((date, leap, jd0, gd0, jd1, gd1))

        return data

    def analyze_1(self, options):
        """
        Compare Meeus' and my algorithms showing differences.

        -1 with optional -A for alternate leap year calculation.
        -S and -E are mandatory.

        If the last column shows anything other than 0.0 then there are
        inconsistencies.

        This test will display, to stderr a progress counter indicating
        every 500 years.
        """
        GLY = (self.GREGORIAN_LEAP_YEAR_ALT if options.alt_leap
               else self.GREGORIAN_LEAP_YEAR)
        data = []

        for year in range(options.start, options.end):
            leap = GLY(year)

            for month, days in enumerate(self.MONTHS, start=1):
                if month == 2 and not leap:
                    max_days = days - 1
                else:
                    max_days = days

                for day in range(1, max_days + 1):
                    date = (year, month, day)
                    jd0 = self.jd_from_gregorian_date_0(date)
                    gd0 = self.gregorian_date_from_jd_0(jd0)
                    jd1 = self.jd_from_gregorian_date_1(
                        date, alt=options.alt_leap)
                    gd1 = self.gregorian_date_from_jd_1(
                        jd1, alt=options.alt_leap)
                    data.append((date, leap, jd0, gd0, jd1, gd1))

                    if year % 500 == 0 and month == 1 and day == 1:
                        print(date, file=sys.stderr)

        return data

    def analyze_2(self, options):
        """
        Check that any Gregorian date can be converted to a Julian Period
        day and then back again to a Gregorian date correctly.
        This only tests my algorithm.

        If there is any data returned except the heading then there are
        errors in the conversion.

        -2 with optional -A for alternate leap year calculation.
        -S and -E are mandatory.

        This test will display, to stderr a progress counter indicating
        every 500 years.
        """
        GLY = (self.GREGORIAN_LEAP_YEAR_ALT if options.alt_leap
               else self.GREGORIAN_LEAP_YEAR)
        data = []

        for year in range(options.start, options.end):
            leap = GLY(year)

            for month, days in enumerate(self.MONTHS, start=1):
                if month == 2 and not leap:
                    max_days = days - 1
                else:
                    max_days = days

                for day in range(1, max_days + 1):
                    date = (year, month, day)
                    jd1 = self.jd_from_gregorian_date_1(
                        date, alt=options.alt_leap)
                    gd1 = self.gregorian_date_from_jd_1(
                        jd1, alt=options.alt_leap)

                    if date != gd1:
                        data.append((date, gd1, jd1, leap))

                    if year % 500 == 0 and month == 1 and day == 1:
                        print(date, file=sys.stderr)

        return data

    def compare_leap_year_algorithms(self, year=None):
        """
        Compare the two leap year algorithms.

        If -c == 0 then all dates from 1 to 3004 are processed.
        If -c == any year, then only that year is processed.

        The table below shows the differences between the 4, 10, 400 and
        4, 128 algorithms. Notice that the dates up to 1482-10-15 are not
        in sync with each other.

        +------+------------+--------+-----+-----+
        | Year | 4, 10, 400 | 4, 128 | std | alt |
        +======+============+========+=====+=====+
        | 100  | 0          | 1      | 1   |     |
        +------+------------+--------+-----+-----+
        | 128  | 1          | 0      |     | 1   |
        +------+------------+--------+-----+-----+
        | 200  | 0          | 1      | 2   |     |
        +------+------------+--------+-----+-----+
        | 256  | 1          | 0      |     | 2   |
        +------+------------+--------+-----+-----+
        | 300  | 0          | 1      | 3   |     |
        +------+------------+--------+-----+-----+
        | 384  | 1          | 0      |     | 3   |
        +------+------------+--------+-----+-----+
        | 500  | 0          | 1      | 4   |     |
        +------+------------+--------+-----+-----+
        | 512  | 1          | 0      |     | 4   |
        +------+------------+--------+-----+-----+
        | 600  | 0          | 1      | 5   |     |
        +------+------------+--------+-----+-----+
        | 640  | 1          | 0      |     | 5   |
        +------+------------+--------+-----+-----+
        | 700  | 0          | 1      | 6   |     |
        +------+------------+--------+-----+-----+
        | 768  | 1          | 0      |     | 6   |
        +------+------------+--------+-----+-----+
        | 896  | 1          | 0      |     | 7   |
        +------+------------+--------+-----+-----+
        | 900  | 0          | 1      | 7   |     |
        +------+------------+--------+-----+-----+
        | 1000 | 0          | 1      | 8   |     |
        +------+------------+--------+-----+-----+
        | 1024 | 1          | 0      |     | 8   |
        +------+------------+--------+-----+-----+
        | 1100 | 0          | 1      | 9   |     |
        +------+------------+--------+-----+-----+
        | 1152 | 1          | 0      |     | 9   |
        +------+------------+--------+-----+-----+
        | 1280 | 1          | 0      |     | 10  |
        +------+------------+--------+-----+-----+
        | 1300 | 0          | 1      | 10  |     |
        +------+------------+--------+-----+-----+
        | 1400 | 0          | 1      | 11  |     |
        +------+------------+--------+-----+-----+
        | 1408 | 1          | 0      |     | 11  |
        +------+------------+--------+-----+-----+
        | 1500 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 1536 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 1664 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 1700 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 1792 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 1800 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 1900 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 1920 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2048 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2100 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 2176 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2200 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 2300 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 2304 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2432 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2500 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 2560 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2600 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 2688 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2700 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 2816 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 2900 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        | 2944 | 1          | 0      |     |     |
        +------+------------+--------+-----+-----+
        | 3000 | 0          | 1      |     |     |
        +------+------------+--------+-----+-----+
        """
        data = []

        if year <= 0:
            for y in range(1, 3005):
                y0 = self.GREGORIAN_LEAP_YEAR(y)
                y1 = self.GREGORIAN_LEAP_YEAR_ALT(y)

                if y0 != y1:
                    data.append((y, y0, y1))
        else:
            y0 = self.GREGORIAN_LEAP_YEAR(year)
            y1 = self.GREGORIAN_LEAP_YEAR_ALT(year)
            data.append((year, y0, y1))

        return data

    def consecutive_days(self, options):
        """
        Test that the Julian Period days are consecutive. i.e. no skipping
        or doubling up.
        Should produce no output if working correctly.

        -k With optional -A for alternate leap year calculation.
        -S and -E are mandatory.
        If -J is used then the test is for consecutive Julian Period days.

        If -JM is used the test is for consecutive Julian Period days using
        Meeus' algorithm.

        If -G is used the test is for consecutive Julian Period days using
        my algorithm.

        Some tests will display, to stderr a progress counter indicating
        every 500 years.
        """
        GLY = (self.GREGORIAN_LEAP_YEAR_ALT if options.alt_leap
               else self.GREGORIAN_LEAP_YEAR)
        LY = self.JULIAN_LEAP_YEAR if options.meeus else GLY
        #sys.stderr.write(f" LY: {str(LY)}\nGLY: {str(GLY)}\n")

        method = (self.jd_from_gregorian_date_0 if options.meeus else
                  self.jd_from_gregorian_date_1)
        data = []
        last_jd = 0

        if options.julian:
            for year in range(options.start, options.end):
                for month, days in enumerate(self.MONTHS, start=1):
                    LY = LY if (year, month) < (1582, 10) else GLY

                    if month == 2 and not LY(year):
                        max_days = days - 1
                    else:
                        max_days = days

                    for day in range(1, max_days + 1):
                        date = (year, month, day)
                        jd = method(date, alt=options.alt_leap)

                        if last_jd != 0 and last_jd == jd:
                            data.append((date, last_jd, jd))

                        last_jd = jd  # Save the jd
        elif options.g_date:
            items = []
            last_date = ()
            GLY = (self.GREGORIAN_LEAP_YEAR_ALT if options.alt_leap
                   else self.GREGORIAN_LEAP_YEAR)
            month_days = list(self.MONTHS)

            for year in range(options.start, options.end):
                month_days[1] = 29 if GLY(year) else 28

                for month, days in enumerate(month_days, start=1):
                    for day in range(1, days+1):
                        items.append((year, month, day))

            for item in items:
                # My algorithms
                jd = self.jd_from_gregorian_date_1(item, alt=options.alt_leap)
                date = self.gregorian_date_from_jd_1(jd, alt=options.alt_leap)

                if last_jd != 0 and (last_jd == jd or item != date):
                    data.append((last_date, last_jd, item, jd, date))

                last_jd = jd
                last_date = date

                if item[0] % 500 == 0 and item[1] == 1 and item[2] == 1:
                    print(item, file=sys.stderr)

        #[print(item) for item in items]
        return data

    def julian_day_with_sunset(self, options):
        """
        Generate a list of Julian days with sunset data. This must be done
        in the historically correct Julian day count or the sunsets will
        not be correct.

        This is used in some Badí tests.

        -j with -S and -E which are mandatory.
        """
        data = []
        alt_lat_lon = {
            1970: (51.4769, 0, 0)
            }
        hm = {
            (1, 3, 20):    (18, 16),
            (1, 4, 8):     (18, 30),
            (2, 2, 24):    (17, 56),
            (2, 2, 25):    (17, 57),
            (2, 2, 26):    (17, 58),
            (2, 3, 2):     (18, 1),
            (2, 3, 6):     (18, 5),
            (1583, 3, 21): (18, 16),
            (1844, 3, 20): (18, 16),
            (1845, 3, 20): (18, 16),
            (1863, 3, 21): (18, 16),
            (2015, 3, 21): (18, 16),
            (2022, 2, 25): (17, 56),
            (2022, 3, 1):  (17, 59),
            (2022, 3, 2):  (18, 0),
            (2024, 3, 20): (18, 16),
            (2024, 4, 20): (18, 42),
            (2024, 5, 14): (19, 2),
            (2024, 7, 17): (19, 19),
            }

        for year in range(options.start, options.end):
            leap = (self.JULIAN_LEAP_YEAR(year) if year < 1583
                    else self.GREGORIAN_LEAP_YEAR(year))
            location = (alt_lat_lon[year] if year in alt_lat_lon
                        else self._bc._BAHAI_LOCATION[:3])

            for month, days in enumerate(self.MONTHS, start=1):
                if month == 2 and not leap:
                    max_days = days - 1
                else:
                    max_days = days

                for day in range(1, max_days + 1):
                    date = (year, month, day)
                    date += hm[date] if date in hm else ()
                    jd = self._gc.jd_from_gregorian_date(date)
                    jdss = self._gc._sun_setting(jd, *location)
                    # Convert to my jd count.
                    ejd = self._gc.jd_from_gregorian_date(date, exact=True)
                    ss = math.floor(ejd) + jdss % 1
                    data.append((date, ss))

        return data

    def jd_from_gregorian_date_0(self, g_date, alt=False):
        """
        Meeus algorithm
        The alt keyword does nothing.
        https://aa.usno.navy.mil/data/JulianDate

        There are 12 years that skip a day. Pope Gregory only compensated for
        10 of them.

        +----------------+-----------------+----------------+-------------+
        | Previous Valid | JD for Previous | Next Valid Day | JD for Next |
        | Day            | Valid Day       |                | Valid Day   |
        +================+=================+================+=============+
        |  (100, 2, 28)  | 1757640.5       |  (100, 3, 1)   | 1757642.5   |
        +----------------+-----------------+----------------+-------------+
        |  (200, 2, 28)  | 1794165.5       |  (200, 3, 1)   | 1794167.5   |
        +----------------+-----------------+----------------+-------------+
        |  (300, 2, 28)  | 1830690.5       |  (300, 3, 1)   | 1830692.5   |
        +----------------+-----------------+----------------+-------------+
        |  (500, 2, 28)  | 1903740.5       |  (500, 3, 1)   | 1903742.5   |
        +----------------+-----------------+----------------+-------------+
        |  (600, 2, 28)  | 1940265.5       |  (600, 3, 1)   | 1940267.5   |
        +----------------+-----------------+----------------+-------------+
        |  (700, 2, 28)  | 1976790.5       |  (700, 3, 1)   | 1976792.5   |
        +----------------+-----------------+----------------+-------------+
        |  (900, 2, 28)  | 2049840.5       |  (900, 3, 1)   | 2049842.5   |
        +----------------+-----------------+----------------+-------------+
        | (1000, 2, 28)  | 2086365.5       | (1000, 3, 1)   | 2086367.5   |
        +----------------+-----------------+----------------+-------------+
        | (1100, 2, 28)  | 2122890.5       | (1100, 3, 1)   | 2122892.5   |
        +----------------+-----------------+----------------+-------------+
        | (1300, 2, 28)  | 2195940.5       | (1300, 3, 1)   | 2195942.5   |
        +----------------+-----------------+----------------+-------------+
        | (1400, 2, 28)  | 2232465.5       | (1400, 3, 1)   | 2232467.5   |
        +----------------+-----------------+----------------+-------------+
        | (1500, 2, 28)  | 2268990.5       | (1500, 3, 1)   | 2268992.5   |
        +----------------+-----------------+----------------+-------------+
        """
        year, month, day = self.date_from_ymdhms(g_date)

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
        My algorithm
        """
        GLY = self.GREGORIAN_LEAP_YEAR_ALT if alt else self.GREGORIAN_LEAP_YEAR
        year, month, day = self.date_from_ymdhms(g_date)
        td = self._days_in_years(year-1, alt=alt)
        days = td + (self.GREGORIAN_EPOCH - 1)  # 37
        month_days = list(self.MONTHS)
        month_days[1] = 29 if GLY(year) else 28
        days += sum(month_days[:month-1]) + day
        #print(f"date: {str(g_date):<16} td: {td:<8} "
        #      f"days: {days:<10} "
        #      f"sum: {sum(month_days[:month-1]):<10}\n", file=sys.stderr)
        return days

    def gregorian_date_from_jd_0(self, jd):
        """
        Convert Julian day to Gregorian date using the Meeus algorithm.
        """
        j_day = jd + 0.5
        z = math.floor(j_day)
        f = j_day % 1

        if z >= 2299161:  # 1582-10-15 Julian and Gregorian crossover.
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
        """
        My algorithm
        """
        GLY = (self.GREGORIAN_LEAP_YEAR_ALT if alt
               else self.GREGORIAN_LEAP_YEAR)
        # Get the number of days since the Gregorian epoch.
        md = jd - (self.GREGORIAN_EPOCH - 1)
        year = math.floor(abs(md / self.MEAN_TROPICAL_YEAR)) + 1
        year *= -1 if md < (self.GREGORIAN_EPOCH - 1) else 1
        # A refined number of days since epoch for the date.
        td = self._days_in_years(year, alt=alt)
        days = md - td

        while days > 365:
            year += 1
            td = self._days_in_years(year, alt=alt)
            days = md - td

        if days == 0:
            days = 365 if year < 0 and year % 4 != 0 else 366
        else:
            year += 1

        month_days = list(self.MONTHS)
        month_days[1] = 29 if GLY(year) else 28
        d = day = 0

        for month, ds in enumerate(month_days, start=1):
            d += ds
            if days > d: continue
            day = math.ceil(days - (d - ds))
            break

        f = jd % 1
        day += f - (1.5 if f > 0.5 else 0.5)

        if day < 1:
            print("TEST--day: {day:<8} month: {month:<2}", file=sys.stderr)
            month -= 1
            day += 28

        return (year, month, round(day, 6))

    def _days_in_years(self, y, alt=False):
        n_4 = y // 4

        if alt:
            n_128 = y // 128
            n_leap_years = n_4 - n_128
        else:
            n_100 = y // 100
            n_400 = y // 400
            n_leap_years = n_4 - n_100 + n_400

        a = y - n_leap_years  # Non-leap years
        b = y - a             # Leap years
        return a * 365 + b * 366

    def date_from_ymdhms(self, date: tuple) -> tuple:
        """
        Convert (year, month, day, hour, minute, second) into a
        (year, month, day.partial) date.
        """
        #self._check_valid_gregorian_month_day(date)
        t_len = len(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hour = date[3] if t_len > 3 and date[3] is not None else 0
        minute = date[4] if t_len > 4 and date[4] is not None else 0
        second = date[5] if t_len > 5 and date[5] is not None else 0
        day += round(self.HR(hour) + self.MN(minute) + self.SEC(second),
                     self.ROUNDING_PLACES)
        return (year, month, day)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test Julian Period methods."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help=("Generate a list of Julian Period days for both Meeus and "
              "my formulas."))
    parser.add_argument(
        '-1', '--analyze-1', action='store_true', default=False,
        dest='analyze_1',
        help=("Generate a list of Julian Period days for different start "
              "and end year."))
    parser.add_argument(
        '-2', '--analyze-2', action='store_true', default=False,
        dest='analyze_2',
        help=("Check that any Gregorian date can be converted to a Julian "
              "Period day and then back again to a Gregorian date correctly."))
    parser.add_argument(
        '-c', '--compare', type=int, default=None, dest='compare',
        help=("Compare two algorithms for determining the leap year. "
              "A value <= 0 processes dates from 1 to 3004"))
    parser.add_argument(
        '-k', '--consecutive', action='store_true', default=False,
        dest='consecutive', help="Test for non consecutive days.")
    parser.add_argument(
        '-j', '--julian-day', action='store_true', default=False,
        dest='julian_day', help=("Generate list of Julian days with sunset "
                                 "data days."))
    parser.add_argument(
        '-A', '--alt-leap', action='store_true', default=False,
        dest='alt_leap', help="Use the 4|128 rule instead of the 4|100|400 "
        "rule.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start year of sequence.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End year of sequence.")
    parser.add_argument(
        '-M', '--meeus', action='store_true', default=False,
        dest='meeus', help="Use Meeus' algorithm.")
    parser.add_argument(
        '-J', '--julian', action='store_true', default=False,
        dest='julian', help="Test for consecutive Julian Period days.")
    parser.add_argument(
        '-G', '--g-date', action='store_true', default=False, dest='g_date',
        help=("Test for consecutive Gregorian dates."))
    parser.add_argument(
        '-D', '--debug', action='store_true', default=False, dest='debug',
        help="Run in debug mode.")
    options = parser.parse_args()

    jpt = JulianPeriodTests()
    ret = 0

    if options.debug:
        sys.stderr.write("DEBUG--options: {}\n".format(options))

    if options.analyze:  # -a
        data = [f"{idx:>02} "
                f"{str(date):<17} "
                f"{str(leap):<6} "
                #f"{jd0:<10} "       # Meeus
                #f"{str(gd0):<17} "  # Meeus
                f"{jd1:<10} "        # Mine
                f"{str(gd1):<17} "   # Mine
                #f"{jd1 - jd0:<4}"
                for (
                    idx,
                    (date,  # Initial Gregorian date
                     leap,  # Is leap year
                     jd0,   # Meeus
                     gd0,   # Meeus
                     jd1,   # Mine
                     gd1)   # Mine
                    ) in enumerate(jpt.analyze(options), start=1)]
        print("ID "
              "Start Date"
              "        Leap"
              #"   Meeus"
              #"      Meeus"
              "   Mine"
              "       Mine"
              #"              Diff"
              )
        [print(item) for item in data]
    elif options.analyze_1:  # -1
        if options.start is None or options.end is None:
            print("If option -1 is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = [f"{str(date):<17} "  # Initial Gregorian date
                    f"{str(leap):<6} "   # Is leap year
                    f"{jd0:<10} "        # Meeus
                    f"{str(gd0):<17} "   # Meeus
                    f"{jd1:<10} "        # Mine
                    f"{str(gd1):<17} "   # Mine
                    f"{jd1 - jd0:<5}"    # Mine - Meeus (diff)
                    for (date,
                         leap,
                         jd0,
                         gd0,
                         jd1,
                         gd1) in jpt.analyze_1(options)]
            print("Start Date        "
                  "Leap   "
                  "Meeus      "
                  "Meeus             "
                  "Mine       "
                  "Mine              "
                  "Diff"
                  )
            [print(item) for item in data]
    elif options.analyze_2:  # -2
        if options.start is None or options.end is None:
            print("If option -2 is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = [f"{str(date):<16} "  # Initial Gregorian date
                    f"{str(gd1):<16} "   # Mine
                    f"{jd1:<10} "        # Mine
                    f"{str(leap):<6} "   # Is leap year
                    for (date,
                         gd1,
                         jd1,
                         leap) in jpt.analyze_2(options)]
            print("Date             "
                  "gd1              "
                  "jd1        "
                  "Leap   "
                  )
            [print(item) for item in data]
    elif isinstance(options.compare, int):  # -c
        data = [
            f"Year {year:>4} "
            f"GLY_STD {gly_std:<1} "
            f"GLY_ALT {gly_alt:<1}"
            for year, gly_std, gly_alt in jpt.compare_leap_year_algorithms(
                options.compare)]
        [print(item) for item in data]
    elif options.consecutive:  # -k
        if options.start is None or options.end is None:
            print("If option -k is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        elif options.julian:
            data = [f"date: {str(date):<12} "
                    f"d: {d:<9} "
                    f"jd: {jd:<9}"
                    for date, d, jd in jpt.consecutive_days(options)]
        elif options.g_date:
            data = [f"last_date: {str(last_date):<15} "
                    f"last_jd: {d:<9} "
                    f"item: {str(item):<15} "
                    f"jd: {jd:<9} "
                    f"date: {str(date):<15}"
                    for last_date, d, item, jd, date in jpt.consecutive_days(
                        options)]
        else:
            print("You must choose one of -[JG].", file=sys.stderr)
            ret = 1

        if ret == 0: [print(item) for item in data]
    elif options.julian_day:  # -j
        if options.start is None or options.end is None:
            print("If option -j is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = [
                f"{str(date):<23} "
                f"{jdss:<14}"
                for date, jdss in jpt.julian_day_with_sunset(options)]
            [print(item) for item in data]
    else:
        parser.print_help()

    sys.exit(ret)
