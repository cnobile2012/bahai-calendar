#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/posix_time.py
#

import importlib
import os
import sys
import math
import datetime as dtime

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar
datetime = importlib.import_module('badidatetime.datetime')


class PosixTests:
    """
    | POSIX converter: https://www.unixtimestamp.com/
    | Julian Period Converter: https://aa.usno.navy.mil/data/JulianDate
    | Sunset: https://gml.noaa.gov/grad/solcalc/

    The Python datetime package seems to always give local time from
    timestamps not UTC time. For Example:

    | In [18]: dtime.datetime.fromtimestamp(18000) This -5 hours from UTC time.
    | Out[19]: datetime.datetime(1970, 1, 1, 0, 0)
    """
    #BADI_COORD = BahaiCalendar._BAHAI_LOCATION[:3]
    # Force standard time in US/Eastern (America/New_York)
    # so test works all year.
    LOCAL_COORD = (35.5894, -78.7792, -5.0)

    UTC_US_E_TZ = dtime.timezone(dtime.timedelta(hours=-5))
    BADI_US_E_TZ = datetime.timezone(datetime.timedelta(hours=-5))

    TEST_TS = (
        ((1970, 1, 1), (126, 16, 2, None, None, 8, 0, 0), BADI_US_E_TZ,
         18000),
        ((1970, 1, 1), (126, 16, 2, None, None, 8, 0, 0), None, 18000),
        #((2024, 11, 24), (), BADI_US_E_TZ, 0),
        )

    _TEST_DATES = (  # The timestamps are for my local area.
        (1, -1842, (1, 44)),  # Sunset 17:16
        (2, -1841, (1, 44)),  # Sunset 17:16
        (3, -1840, (1, 44)),  # Sunset 17:16
        (4, -1839, (1, 43)),  # Sunset 17:17
        (1964, 121, (1, 48)),
        (1965, 122, (1, 48)),
        (1966, 123, (1, 48)),
        (1967, 124, (1, 48)),
        (1968, 125, (1, 48)),
        # 1969-12-31T19:00:00-05:00 - 1969-12-31T17:12:00-05:00
        (1969, 126, (1, 48)),
        # 1970-12-31T19:00:00-05:00 - 1970-12-31T17:12:00-05:00
        (1970, 127, (1, 48)),
        # 1971-12-31T19:00:00-05:00 - 1971-12-31T17:12:00-05:00
        (1971, 128, (1, 48)),
        (1972, 129, (1, 48)),
        (1973, 130, (1, 48)),
        (1974, 131, (1, 48)),
        (1975, 132, (1, 48)),
        (1976, 133, (1, 48)),
        (1977, 134, (1, 48)),
        (1978, 135, (1, 48)),
        (1979, 136, (1, 48)),
        # 1980-12-31T19:00:00-05:00 - 1980-12-31T17:12:00-05:00
        (1980, 137, (1, 48)),
        (1981, 138, (1, 48)),
        (1982, 139, (1, 48)),
        (1983, 140, (1, 48)),
        # 2014 - 2065
        (2014, 171, (1, 48)),
        (2015, 172, (1, 48)),
        (2016, 173, (1, 48)),
        (2017, 174, (1, 48)),
        (2018, 175, (1, 48)),
        (2019, 176, (1, 48)),
        (2020, 177, (1, 48)),
        (2021, 178, (1, 48)),
        (2022, 179, (1, 48)),
        (2023, 180, (1, 48)),
        (2024, 181, (1, 48)),
        (2025, 182, (1, 48)),
        (2026, 183, (1, 48)),
        (2027, 184, (1, 48)),
        (2028, 185, (1, 48)),
        (2029, 186, (1, 48)),
        (2030, 187, (1, 48)),
        (2031, 188, (1, 48)),
        (2032, 189, (1, 48)),
        (2033, 190, (1, 48)),
        (2034, 191, (1, 48)),
        (2035, 192, (1, 48)),
        (2036, 193, (1, 48)),
        (2037, 194, (1, 48)),
        (2038, 195, (1, 48)),
        (2039, 196, (1, 48)),
        (2040, 197, (1, 47)),  # Sunset 17:13
        (2041, 198, (1, 48)),
        (2042, 199, (1, 48)),
        (2043, 200, (1, 48)),
        (2044, 201, (1, 47)),  # Sunset 17:13
        (2045, 202, (1, 48)),
        (2046, 203, (1, 48)),
        (2047, 204, (1, 48)),
        (2048, 205, (1, 47)),  # Sunset 17:13
        (2049, 206, (1, 48)),
        (2050, 207, (1, 48)),
        (2051, 208, (1, 48)),
        (2052, 209, (1, 47)),  # Sunset 17:13
        (2053, 210, (1, 48)),
        (2054, 211, (1, 48)),
        (2055, 212, (1, 48)),
        (2056, 213, (1, 47)),  # Sunset 17:13
        (2057, 214, (1, 48)),
        (2058, 215, (1, 48)),
        (2059, 216, (1, 48)),
        (2060, 217, (1, 47)),  # Sunset 17:13
        (2061, 218, (1, 48)),
        (2062, 219, (1, 48)),
        (2063, 220, (1, 48)),
        (2064, 221, (1, 47)),  # Sunset 17:13
        (2065, 222, (1, 48)),
        (3000, 1157, (1, 50)),  # Sunset 17:10
        (3001, 1158, (1, 50)),
        (3002, 1159, (1, 50)),
        (3003, 1160, (1, 50)),
        (3004, 1161, (1, 50)),
        )

    def __init__(self):
        self._bc = BahaiCalendar()
        self._gc = GregorianCalendar()

    def analize0(self, options):
        """
        Analize the POSIX date and time for accuracy.

        -a
        """
        data = []

        for g_date, b_date, tb_tz, expected_result, in self.TEST_TS:
            gdt = dtime.datetime(*g_date)
            gt = gdt.timestamp()
            bdt = datetime.datetime(*b_date, tzinfo=tb_tz)
            bt = bdt.timestamp()
            #            Greg    Greg TS Badi    Badi TZ  Badi TS
            data.append((g_date, gt,     b_date, tb_tz,   bt))

        return data

    def test_mktime(self, options):
        """
        Test that the _mktime method produces the correct local time.

        -m
        """
        data = []

        for g_year, b_year, old_time in self._TEST_DATES:
            result = []
            result.append(g_year)
            date = (b_year, 16, 2, None, None)
            date += self._get_badi_hms(b_year)[-1]
            dt = datetime.datetime(*date)
            b_date = date[:3] + date[5:]
            result.append(b_date)
            result.append(self._mktime(dt))
            result.append(int(dtime.datetime(g_year, 12, 31, 19).timestamp()))
            data.append(result)

        return data

    def _mktime(self, dt) -> float:
        """
        This is the inverse function of localtime(). Its argument is the
        struct_time or full 9-tuple (since the dst flag is needed; use -1 as
        the dst flag if it is unknown) which expresses the time in local time,
        not UTC. It returns a floating-point number, for compatibility with
        time(). If the input value cannot be represented as a valid time,
        either OverflowError or ValueError will be raised (which depends on
        whether the invalid value is caught by Python or the underlying C
        libraries).

        .. code:: python

           In [1]: t = 1739498576.793511
           In [2]: jd = t / 86400 + 2440585.5  # The more exact formula.
           In [3]: jd
           Out[3]: 2460718.5853795544

        | Historically correct JD value: 2460720.5853795544
        | As per the POSIX converter above.
        | GMT Gregorian Date: 2025-02-14T02:02:56+00:00
        | EST Gregorian Date: 2025-02-13T21:02:56-05:00

        .. code:: python

           In [4]: bc.badi_date_from_gregorian_date((2025, 2, 14, 2, 2, 56),
                                                    short=True, trim=True)
           Out[4]: (181, 18, 9, 8, 21, 17.4816)
           In [5]: bc.posix_timestamp(t, *datetime.GMT_COORD, short=True,
                                      trim=True)
           Out[5]: (181, 18, 9, 8, 58, 23.3184)

        Is the above Badí' date correct? Lets figure it out long-hand. We
        actually need the UTC day before because the JD is >= 2460718.5 and
        in UTC time is the next day.

        .. code:: python

           In [6]: bc.utc_sunset((181, 18, 9), *datetime.GMT_COORD)
           Out[6]: (17, 15, 31.7952)
           In [7]: gc.jd_from_gregorian_date((2025, 2, 13, 17, 15, 31.7952),
                                             exact=True)
           Out[7]: 2460718.219118  # Exact value

        The NOAA says it's 17:12 where we get 17:15:31.7952.

        The beginning of a Badí' day starts at sunset:

        1. If the UTC time is after midnight then subtract the previous sunset
           time from midnight then add the time past midnight. Then convert
           the results to hours, minutes, and seconds.

           0.3662615544 = 2460718.5 - 2460718.219118 + 0.0853795544

           .. code:: python

              In [8]: bc._hms_from_decimal_day(0.3662615544)
              Out[8]: (8, 47, 24.9972)

           The Badí' date should then be (181, 18, 9, 8, 47, 24.9972), so it
           looks like the original calculation above was pretty close.

        2. If the UTC time is equal to or before midnight subtract the sunset
           time from the UTC time. *** TODO *** Give an example for this
           situation.

        3. If UTC time is before sunset on previous Julian day.

        """
        def local(u):
            date = self._bc.posix_timestamp(u, *self.LOCAL_COORD, short=True,
                us=True)
            return (datetime.datetime(*date[:3], None, None, *date[3:6]) -
                    epoch) // datetime.timedelta(0, 1)

        # The datetime package is more than 2 hours behind the actual time.
        # datetime(126, 16, 2, None, None, 5, 46, 8.9472, tzinfo=timezone.utc)
        # NOAA GMT sunset on POSIX epoch: 16:01
        # NOAA GMT sunset the day before: 16:00 (4 pm)
        # The Badi time was: 08:00:00+00:00
        epoch = datetime.datetime(126, 16, 2, None, None, 1, 48)
        date = self._short_from_long_form(dt, time=dt.b_time)
        t = (datetime.datetime(*date) - epoch) // datetime.timedelta(0, 1)
        a = local(t) - t
        u1 = t - a
        t1 = local(u1)
        return t1

    def sunset(self, options):
        """
        Find the sunset for given years.

        -s
        """
        data = []

        for year in range(self._bc.MINYEAR, self._bc.MAXYEAR+1):
            ss, g_hms, b_hms = self._get_badi_hms(year, rd=True)
            data.append((year, ss, g_hms, b_hms))

        return data

    def _short_from_long_form(self, dt, time: tuple=()) -> tuple:
        if dt.is_short:
            date = (*dt.b_date, None, None, *time)
        else:
            b_date = self._bc.short_date_from_long_date(dt.b_date + time)
            date = (*b_date[:3], None, None, *b_date[3:])

        return date

    def _get_badi_hms(self, year, rd=False):
        jd = self._bc.jd_from_badi_date((year, 16, 2), *self.LOCAL_COORD)
        mjd = jd + self._bc._meeus_from_exact(jd)
        ss = self._bc._sun_setting(mjd, *self.LOCAL_COORD)
        f_ss = math.floor(ss) + round(ss % 1 * 1440) / 1440 if rd else ss
        hms = self._bc._hms_from_decimal_day(f_ss + 0.5)
        # Where 24 is hours in a day and 5 is the negative offset from GMT.
        b_time = (((24 - 5) / 24) - (f_ss + 0.5)) % 1
        return ss, hms, self._bc._hms_from_decimal_day(b_time)[:2]


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test POSIX dates and times."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze0',
        help="Analyze the POSIX date and time for accuracy.")
    parser.add_argument(
        '-m', '--mktime', action='store_true', default=False, dest='mktime',
        help="Test the _mktime() method for accuracy.")
    parser.add_argument(
        '-s', '--sunset', action='store_true', default=False, dest='sunset',
        help="Find the sunset for given years.")
    options = parser.parse_args()
    pt = PosixTests()
    ret = 0
    basename = os.path.basename(__file__)

    if options.analyze0:  # -a
        [print(f"{str(g_date):13} "
               f"{gt:10} "
               f"{str(b_date):13} "
               f"{str(tb_tz):9} "
               f"{bt:10.4f}"
               ) for g_date, gt, b_date, tb_tz, bt in pt.analize0(options)]
    elif options.mktime:  # -m
        print(f"./contrib/misc/{basename} -m")
        print("Badí' Date", " "*19, "t1", " "*9,
              "ts", " "*9, "t1-ts", " "*1, "Badí'",
              "Gregorian")
        print(" "*64, "Leap", " "*0, "Year", "Leap")
        [print(f"{str(date):30} "
               f"{t1:12} "
               f"{ts:12} "
               f"{t1-ts:7} "
               f"{str(pt._bc._is_leap_year(date[0])):5} "
               f"{g_year:4} "
               f"{str(pt._gc._GREGORIAN_LEAP_YEAR(g_year)):5}"
               ) for g_year, date, t1, ts, in pt.test_mktime(options)]
    elif options.sunset:  # -s
        print(f"./contrib/misc/{basename} -s")
        print("Year  Sunset JD", " "*4, "UTC HMS", " "*9, "Badí' HMS")
        [print(f"{year:>5} "
               f"{ss:14.6f} "
               f"{str(g_hms):17} "
               f"{str(b_hms):17}"
               ) for year, ss, g_hms, b_hms in pt.sunset(options)]
    else:
        parser.print_help()

    sys.exit(ret)
