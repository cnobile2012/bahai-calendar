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


class PosixTests(BahaiCalendar):
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
    GMT_COORD = (51.477928, -0.001545, 0)
    DEFAULT_COORD = None

    # Gregorian offset to the year before the Bahi epoch.
    TRAN_COFF = 1843

    UTC_US_E_TZ = dtime.timezone(dtime.timedelta(hours=-5))
    BADI_US_E_TZ = datetime.timezone(datetime.timedelta(hours=-5))

    TEST_TS = (
        ((1970, 1, 1), (126, 16, 2, None, None, 8, 0, 0), BADI_US_E_TZ,
         18000),
        ((1970, 1, 1), (126, 16, 2, None, None, 8, 0, 0), None, 18000),
        #((2024, 11, 24), (), BADI_US_E_TZ, 0),
        )

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()

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

    def sunset(self, options):
        """
        Find the sunset for given years.

        -s
        """
        self.DEFAULT_COORD = options.coord
        data = []

        for year in range(self.MINYEAR, self.MAXYEAR+1):
            ss, g_hms, b_hms = self._get_badi_hms(year, rd=True)
            data.append((year, ss, g_hms, b_hms))

        return data

    def test_mktime(self, options):
        """
        Test that the _mktime method produces the correct local time.

        -m with -C, -S, and -E

        .. warning::

           *** TODO *** -C does not work correctly yet, just run without -C
           which causes the default coordinates to be used which do work
           properly.

        Total range -1842 through 1161
        """
        start = options.start
        end = options.end
        self.DEFAULT_COORD = options.coord
        data = []

        for b_year in range(start, end):
            g_year = b_year + self.TRAN_COFF
            result = []
            result.append(g_year)
            date = (b_year, 16, 2, None, None)
            date += self._get_badi_hms(b_year, rd=True)[-1]
            dt = datetime.datetime(*date)
            b_date = date[:3] + date[5:]
            result.append(b_date)
            utc_ts = int(dtime.datetime(g_year, 12, 31, 19).timestamp())
            t1 = self._mktime(dt)

            if not options.kill_coeff:
                t1 += self._get_coeff(date[0])

            result.append(t1)
            result.append(utc_ts)
            diff = t1 - utc_ts
            result.append(diff)
            data.append(result)

        return data

    def find_coefficents_precursor(self, options):
        """
        -p or --precursor
        -S and -E must be Badí' dates.
        """
        data = []

        for y in range(options.start, options.end):
            year = options.end - y
            data.append((y + self.TRAN_COFF, y, year % 4))

        return data

    def find_coefficents(self, options):
        """
        -q or --coeff and -S and -E
        """
        start = options.start
        end = options.end
        self.DEFAULT_COORD = options.coord
        cp = {by: n
              for gy, by, n in self.find_coefficents_precursor(options)}
        items = []

        for b_year in range(start, end):
            month, day = 16, 2
            h, m = self._get_badi_hms(b_year, rd=True)[-1]
            dt = datetime.datetime(b_year, month, day, None, None, h, m)
            t1 = self._mktime(dt)
            msg = (f"{b_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                   f"00 {t1:12} ")
            g_year = b_year + self.TRAN_COFF
            utc_ts = int(dtime.datetime(g_year, 12, 31, 19).timestamp())
            msg += (f"{g_year:4} {utc_ts:12} ")
            diff = t1 - utc_ts
            msg += f"{diff:7} "
            j = cp.get(b_year)
            msg += f"{j}"
            items.append(msg)

        return items

    # Support methods

    def _get_coeff(self, year):
        def process_segment(y, a=0, onoff0=(), b=0, onoff1=(), c=0, onoff2=()):
            func = lambda y, onoff: 0 < y < 100 and y % 4 in onoff
            coeff = 0

            if a and func(y, onoff0):    # Whatever is passed in onoff0.
                coeff = a
            elif b and func(y, onoff1):  # Whatever is passed in onoff1.
                coeff = b
            elif c and func(y, onoff2):  # Whatever is passed in onoff1.
                coeff = c

            return coeff

        def process_segments(year, pn, a=0, onoff0=(), b=0, onoff1=(),
                             c=0, onoff2=()):
            coeff = 0

            for start, end in pn:
                if year in range(start, end):
                    # start to end (range -S start -E end)
                    coeff0 = process_segment(end - year, a=a, onoff0=onoff0)
                    coeff1 = process_segment(end - year, b=b, onoff1=onoff1)
                    coeff2 = process_segment(end - year, c=c, onoff2=onoff2)

                    if coeff0 != 0:
                        coeff = coeff0
                    elif coeff1 != 0:
                        coeff = coeff1
                    elif coeff2 != 0:
                        coeff = coeff2

            return coeff

        pn1 = ((-1842, -1822),)
        pn2 = ((-1822, -1813),)
        coeff = 0

        if not coeff:
            coeff = process_segments(year, pn1, 86282, (0, 1), -118, (2,),
                                     -58, (3,))

        if not coeff:
            coeff = process_segments(year, pn2, 86282, (1, 2), -58, (0, 3))

        return coeff

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
            date = self.posix_timestamp(u, *self.DEFAULT_COORD, short=True,
                us=True)
            return (datetime.datetime(*date[:3], None, None, *date[3:6]) -
                    epoch) // datetime.timedelta(0, 1)

        epoch = self._epoch
        date = self._short_from_long_form(dt, time=dt.b_time)
        t = (datetime.datetime(*date) - epoch) // datetime.timedelta(0, 1)
        a = local(t) - t
        u1 = t - a
        t1 = local(u1)
        return t1

    def _short_from_long_form(self, dt, time: tuple=()) -> tuple:
        if dt.is_short:
            date = (*dt.b_date, None, None, *time)
        else:
            b_date = self.short_date_from_long_date(dt.b_date + time)
            date = (*b_date[:3], None, None, *b_date[3:])

        return date

    def _get_badi_hms(self, year, rd=False):
        MIN = 1440
        jd = self.jd_from_badi_date((year, 16, 2), *self.DEFAULT_COORD)
        mjd = jd + self._meeus_from_exact(jd)
        ss = self._sun_setting(mjd, *self.DEFAULT_COORD)
        f_ss = math.floor(ss) + round(ss % 1 * MIN) / MIN if rd else ss
        hms = self._hms_from_decimal_day(f_ss + 0.5)
        # Where 24 is hours in a day and 5 is the negative offset from GMT.
        b_time = (((24 - 5) / 24) - (f_ss + 0.5)) % 1
        #print(year, ss, b_time, file=sys.stderr)
        return ss, hms, self._hms_from_decimal_day(b_time)[:2]

    @property
    def _epoch(self):
        """
        Find the POSIX epoch based on the local timezone.
        """
        hms = self._get_badi_hms(126, rd=True)[-1]
        return datetime.datetime(126, 16, 2, None, None, *hms)


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
        '-p', '--precursor', action='store_true', default=False,
        dest='precursor',
        help="Dump data for determining the precursors to the coefficients.")
    parser.add_argument(
        '-q', '--coeff', action='store_true', default=False, dest='coeff',
        help="Dump data for determining coefficients.")
    parser.add_argument(
        '-s', '--sunset', action='store_true', default=False, dest='sunset',
        help="Find the sunset for given years.")
    parser.add_argument(
        '-C', '--coord', type=float, nargs=3, metavar=('lat', 'lon', 'offset'),
        default=(35.5894, -78.7792, -5.0), dest='coord',
        help="The coordinates with GMT offset in hours.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End Badí' year of sequence.")
    parser.add_argument(
        '-K', '--kill-coeff', action='store_true', default=False,
        dest='kill_coeff',
        help="Turn off all coefficients during an analysis.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start Badí' year of sequence.")
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
        if options.start is None or options.end is None:
            print("If option -m is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print(f"./contrib/misc/{basename} -mC {options.coord} "
                  f"-S{options.start} -E{options.end}")
            print("Badí' Date", " "*10, "t1", " "*9,
                "ts", " "*9, "t1-ts", " "*1, "Badí'",
                "Gregorian")
            print(" "*55, "Leap", " "*0, "Year", "Leap")
            [print(f"{str(date):21} "
                   f"{t1:12} "
                   f"{ts:12} "
                   f"{diff:7} "
                   f"{str(pt._is_leap_year(date[0])):5} "
                   f"{g_year:4} "
                   f"{str(pt.gc._GREGORIAN_LEAP_YEAR(g_year)):5}"
                   ) for g_year, date, t1, ts, diff in pt.test_mktime(options)]
    elif options.precursor:  # -p
        if options.start is None or options.end is None:
            print("If option -p is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print(f"./contrib/misc/{basename} -pS{options.start} "
                  f"-E{options.end}")
            data = pt.find_coefficents_precursor(options)
            [print(f"{gy:> 5} {by:> 5}, {n:<1} {a:>2}")
             for gy, by, n, a in data]
            print(f"Total years: {len(data)}")
    elif options.coeff:  # -q
        if options.start is None or options.end is None:
            print("If option -q is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print(f"./contrib/misc/{basename} -qS{options.start} "
                  f"-E{options.end}")
            [print(item) for item in pt.find_coefficents(options)]
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
