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
from badidatetime._coefficients import Coefficients
datetime = importlib.import_module('badidatetime.datetime')


class PosixTests(BahaiCalendar, Coefficients):
    """
    | POSIX converter: https://www.unixtimestamp.com/
    | Julian Period Converter: https://aa.usno.navy.mil/data/JulianDate
    | Sunset: https://gml.noaa.gov/grad/solcalc/

    The Python datetime package seems to always give local time from
    timestamps not UTC time. For Example:

    | In [18]: dtime.datetime.fromtimestamp(18000) This -5 hours from UTC time.
    | Out[19]: datetime.datetime(1970, 1, 1, 0, 0)
    """
    #BADI_COORD = (35.682376, 51.285817, 3.5)
    #FQ_COORD = (35.5894, -78.7792, -5.0)
    GMT_COORD = (51.477928, -0.001545, 0)

    # Modula for determining coefficients.
    _MODULA = 4

    # Gregorian offset to the year before the Bahi epoch.
    TRAN_COFF = 1843

    def __init__(self):
        super().__init__()
        self.LOCAL_COORD = None
        self.gc = GregorianCalendar()

    def mktime(self, options) -> list:
        """
        Test that the _mktime method produces the correct local time.

        -m or --mktime with -C, -S, and -E

        Total range -1842 through 1161
        """
        start = options.start
        end = options.end
        self.LOCAL_COORD = options.coord
        data = []
        offset = int(self.LOCAL_COORD[-1] * 3600)

        for b_year in range(start, end):
            g_year = b_year + self.TRAN_COFF
            date = (b_year, 16, 2, None, None) + self._get_badi_hms(b_year)[-1]
            dt = datetime.datetime(*date)
            b_date = date[:3] + date[5:]
            gmt_ts = int(dtime.datetime(g_year + 1, 1, 1,
                tzinfo=dtime.timezone.utc).timestamp()) + offset
            t1 = self._mktime(dt)

            if not options.kill_coeff:
                if options.coeff1:
                    t1 -= self._get_ts_coeff1(date[0])
                else:
                    t1 -= self._get_ts_coeff0(date[0])

            g_leap = self.gc._GREGORIAN_LEAP_YEAR(g_year)
            b_leap = self._is_leap_year(b_date[0])
            t1 += offset
            data.append(
                (b_date, t1, gmt_ts, g_leap, g_year, b_leap, gmt_ts-t1))

        return data

    def find_coefficents_modula(self, options) -> list:
        """
        -p or --precursor
        -S and -E must be Badí' dates.
        """
        data = []

        for y in range(options.start, options.end):
            year = options.end - y
            data.append((y + self.TRAN_COFF, y, year % self._MODULA))

        return data

    def find_coefficents(self, options) -> list:
        """
        -q or --coeff and -S and -E
        """
        start = options.start
        end = options.end
        self.LOCAL_COORD = options.coord
        cp = {by: m for gy, by, m in self.find_coefficents_modula(options)}
        items = []

        for b_year in range(start, end):
            g_year = b_year + self.TRAN_COFF
            month, day = 16, 2
            h, m = self._get_badi_hms(b_year)[-1]
            dt = datetime.datetime(b_year, month, day, None, None, h, m)
            t1 = self._mktime(dt)
            msg = (f"{b_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                   f"00 {t1:12} ")
            gmt_ts = int(dtime.datetime(g_year + 1, 1, 1,
                tzinfo=dtime.timezone.utc).timestamp())
            msg += (f"{g_year:4} {gmt_ts:12} ")
            diff = gmt_ts - t1
            msg += f"{diff:7} "
            msg += f"{cp.get(b_year)}"
            items.append(msg)

        return items

    def sunset(self, options) -> list:
        """
        Find the sunset for given years.

        -s or --sunset with -S and -E
        """
        self.LOCAL_COORD = options.coord
        start = options.start
        end = options.end
        data = []

        for year in range(start, end):
            ss, g_hms, b_hms = self._get_badi_hms(year)
            data.append((year, ss, g_hms, b_hms))

        return data

    # Supporting methods

    def _get_ts_coeff0(self, year: int) -> int:
        """
        Determine the coefficients needed to adjust the POSIX timestamp of
        the Badí' dates to the Gregorian dates.
        """
        def years(pn_all):
            data = []

            for pn in pn_all:
                if isinstance(pn, int):
                    data.append(pn)
                elif isinstance(pn, tuple):
                    start, end = pn
                    data += range(start, end+1)
                else:  # pragma: no cover
                    assert False, ("The 'pn' argument can only be an int or "
                                   f"tuple, found: {type(pn)}")

            return data

        if year in self._PN01:
            coeff = 86520
        elif year in self._PN02:
            coeff = 86400
        elif year in self._PN03:
            coeff = 86160
        elif year in self._PN04:
            coeff = 86040
        elif year in self._PN05:
            coeff = 85860
        elif year in self._PN06:
            coeff = 85800
        elif year in self._PN07:
            coeff = 85620
        elif year in self._PN08:
            coeff = 180
        elif year in self._PN09:
            coeff = 121
        elif year in years(self._PN10):
            coeff = 120
        elif year in self._PN11:
            coeff = 119
        elif year in self._PN12:
            coeff = -1
        elif year in self._PN13:
            coeff = -60
        elif year in years(self._PN14):
            coeff = -180
        elif year in self._PN15:
            coeff = -181
        elif year in years(self._PN16):
            coeff = -240
        elif year in self._PN17:
            coeff = -359
        elif year in years(self._PN18):
            coeff = -360
        elif year in self._PN19:
            coeff = -419
        elif year in years(self._PN20):
            coeff = -420
        elif year in years(self._PN21):
            coeff = -540
        elif year in years(self._PN22):
            coeff = -600
        elif year in years(self._PN23):
            coeff = -720
        elif year in self._PN24:
            coeff = -721
        elif year in self._PN25:
            coeff = -780
        elif year in self._PN26:
            coeff = -86220
        elif year in self._PN27:
            coeff = -86279
        elif year in years(self._PN28):
            coeff = -86280
        elif year in self._PN29:
            coeff = -86281
        elif year in years(self._PN30):
            coeff = -86400
        elif year in self._PN31:
            coeff = -86401
        elif year in years(self._PN32):
            coeff = -86460
        elif year in years(self._PN33):
            coeff = -86580
        elif year in self._PN34:
            coeff = -86581
        elif year in self._PN35:
            coeff = -86640
        elif year in self._PN36:
            coeff = -86759
        elif year in years(self._PN37):
            coeff = -86760
        elif year in self._PN38:
            coeff = -86819
        elif year in self._PN39:
            coeff = -86820
        elif year in years(self._PN40):
            coeff = -86940
        elif year in self._PN41:
            coeff = -87000
        elif year in self._PN42:
            coeff = -87120
        elif year in self._PN43:
            coeff = -87121
        elif year in self._PN44:
            coeff = -172620
        elif year in self._PN45:
            coeff = -172679
        elif year in self._PN46:
            coeff = -172680
        elif year in self._PN47:
            coeff = -172800
        elif year in self._PN48:
            coeff = -172980
        elif year in self._PN49:
            coeff = -173160
        elif year in self._PN50:
            coeff = -173340
        else:
            coeff = 0

        return coeff

    def _get_ts_coeff1(self, year: int) -> int:
        """
        An alternative algorithm for applying coefficients to the timestamp.
        There is a much shorter list of years, but the code itself is much
        more complicated, so it's not used. The total coefficients were not
        completely derived.
        """
        def years(pn_all):
            data = []

            for pn in pn_all:
                data += range(*pn)

            return data

        def process_segment(y, a=0, onoff0=(), b=0, onoff1=(), c=0, onoff2=(),
                            d=0, onoff3=()):
            func = lambda y, onoff: 0 < y < 100 and y % self._MODULA in onoff
            coeff = 0

            if a and func(y, onoff0):    # Whatever is passed in onoff0.
                coeff = a
            elif b and func(y, onoff1):  # Whatever is passed in onoff1.
                coeff = b
            elif c and func(y, onoff2):  # Whatever is passed in onoff2.
                coeff = c
            elif d and func(y, onoff3):  # Whatever is passed in onoff3.
                coeff = d

            return coeff

        def process_segments(year, pn, a=0, onoff0=(), b=0, onoff1=(),
                             c=0, onoff2=(), d=0, onoff3=()):
            coeff = 0

            for start, end in pn:
                if year in range(start, end):
                    # start to end (range -S start -E end)
                    coeff0 = process_segment(end - year, a=a, onoff0=onoff0)
                    coeff1 = process_segment(end - year, b=b, onoff1=onoff1)
                    coeff2 = process_segment(end - year, c=c, onoff2=onoff2)
                    coeff3 = process_segment(end - year, d=d, onoff3=onoff3)
                    coeff = [
                        coeff for coeff in (coeff0, coeff1, coeff2, coeff3)
                        if coeff != 0]
                    assert len(coeff) == 1, f"Invalid len of {coeff} must be 1"
                    coeff = coeff[0]

            return coeff

        pn1 = ((-1842, -1814),)  # 86400, 60, 60, 86400
        pn2 = ((-1814, -1782),)  # 86400, 86400, 60, 86400
        pn3 = ((-1782, -1742),)  # 86400, 86400, 86400, 86460
        pn4 = ((-1742, -1715),)  # 180, 60, 60, 86460
        pn5 = ((-1715, -1683),)  # 86460, 86460, 180, 60
        pn6 = ((-1683, -1651),)  # 86460, 86460, 86460, 180
        pn7 = ((-1651, -1643),)  # 86460, 86460, 86460, 86460
        pn8 = ((-1643, -1615), (-1519, -1483))  # 180, 180, 180, 180
        pn9 = ((-1615, -1583), (-1483, -1451),)  # 86580, 180, 180, 180
        pn10 = ((-1583, -1551), (-1451, -1415),)  # 86580, 86580, 180, 180
        pn11 = ((-1551, -1543), (-1415, -1383), )  # 86580, 86580, 86580, 180
        pn12 = ((-1543, -1519),)  # 180, 180, 180, -86160
        pn13 = ((-1383, -1343),)  # 86580, 86580, 86580, 86580
        pn14 = ((-1343, -1315),)  # 86580, 240, 240, 240
        pn15 = ((-1315, -1283),)  # 86640, 86580, 240, 240
        pn16 = ((-1283, -1251),)  # 86640, 86640, 86580, 240
        pn17 = ((-1251, -1243),)  # 86640, 86640, 86580, 86580
        pn18 = ((-1243, -1215), (-1119, -1083),)  # 360, 360, 360, 360
        pn19 = ((-1215, -1183),)  # 86640, 360, 360, 360
        pn20 = ((-1183, -1151),)  # 86760, 86640, , 360, 360
        pn21 = ((-1151, -1143),)  # 86760, 86640, 86640, 360
        pn22 = ((-1143, -1119),)  # 360, 360, 360, -86040
        pn23 = ((-1083, -1051),)  # 86760, 360, 360, 360
        pn24 = ((-1051, -1019),)  # 86760, 86760, 360, 360
        pn25 = ((-1019, -983),)  # 86760, 86760, 86760, 360
        pn26 = ((-983, -942),)  # 86760, 86760, 86760, 86760
        pn27 = ((-942, -914),)  # 420, 420, 420, 86760
        pn28 = ((-914, -882),)  # 86760, 420, 420, 86820
        pn29 = ((-882, -850),)  # 86760, 86760, 420, 86820
        pn30 = ((-850, -843),)  # 86820, 86760, 86760, 86820
        pn31 = ((-843, -815),)  # 540, 540, 540, 420
        pn32 = ((-815, -783),)  # 86820, 540, 540, 540
        pn33 = ((-783, -751),)  # 86940, 86820, 540, 540
        pn34 = ((-751, -743),)  # 86940, 86820, 86820, 540
        pn35 = ((-743, -719),)  # 540, 540, 540, -85800
        pn36 = ((-719, -683),)  # 540, 540, 540, 540
        pn37 = ((-683, -651),)  # 86940, 540, 540, 540
        pn38 = ((-651, -619),)  # 86940, 86940, 540, 540
        pn39 = ((-619, -583),)  # 86940, 86940, 86940, 540
        pn40 = ((-583, -543),)  # 86940, 86940, 86940, 86940
        pn41 = ((-543, -515),)  # 86940, 720, 600, 600
        # pn42 = ((),)  #
        # pn43 = ((),)  #
        # pn44 = ((),)  #
        # pn45 = ((),)  #
        # pn46 = ((),)  #
        # pn47 = ((),)  #
        # pn48 = ((),)  #
        # pn49 = ((),)  #
        # pn50 = ((),)  #

        if year in years(pn1):
            coeff = process_segments(year, pn1, -86400, (0, 1), -60, (2, 3))
        elif year in years(pn2):
            coeff = process_segments(year, pn2, -86400, (0, 1, 3), -60, (2,))
        elif year in years(pn3):
            coeff = process_segments(year, pn3, -86400, (0, 2, 3),
                -86460, (1,))
        elif year in years(pn4):
            coeff = process_segments(year, pn4, -86460, (0,), -60, (1, 2),
                -180, (3,))
        elif year in years(pn5):
            coeff = process_segments(year, pn5, -86460, (0, 3), -180, (2,),
                -60, (1,))
        elif year in years(pn6):
            coeff = process_segments(year, pn6, -86460, (0, 2, 3), -180, (1,))
        elif year in years(pn7):
            coeff = process_segments(year, pn7, -86460, (0, 1, 2, 3))
        elif year in years(pn8):
            coeff = process_segments(year, pn8, -180, (0, 1, 2, 3))
        elif year in years(pn9):
            coeff = process_segments(year, pn9, -86580, (0,), -180, (1, 2, 3))
        elif year in years(pn10):
            coeff = process_segments(year, pn10, -86580, (0, 3), -180, (1, 2))
        elif year in years(pn11):
            coeff = process_segments(year, pn11, -86580, (0, 2, 3), -180, (1,))
        elif year in years(pn12):
            coeff = process_segments(year, pn12, -180, (0, 2, 3), 86160, (1,))
        elif year in years(pn13):
            coeff = process_segments(year, pn13, -86580, (0, 1, 2, 3))
        elif year in years(pn14):
            coeff = process_segments(year, pn14, -86580, (0,), -240, (1, 2, 3))
        elif year in years(pn15):
            coeff = process_segments(year, pn15, -86640, (0,), -240, (1, 2),
                -86580, (3,))
        elif year in years(pn16):
            coeff = process_segments(year, pn16, -86640, (0, 3), -86640, (2,),
                -240, (1,))
        elif year in years(pn17):
            coeff = process_segments(year, pn17, -86640, (0, 3),
                -86580, (1, 2))
        elif year in years(pn18):
            coeff = process_segments(year, pn18, -360, (0, 1, 2, 3))
        elif year in years(pn19):
            coeff = process_segments(year, pn19, -86640, (0,), -360, (1, 2, 3))
        elif year in years(pn20):
            coeff = process_segments(year, pn20, -86760, (0,), -360, (1, 2),
                -86640, (3,))
        elif year in years(pn21):
            coeff = process_segments(year, pn21, -86760, (0,), -360, (1,),
                -86640, (2, 3))
        elif year in years(pn22):
            coeff = process_segments(year, pn22, -360, (0, 2, 3), 86040, (1,))
        elif year in years(pn23):
            coeff = process_segments(year, pn23, -86760, (0,), -360, (1, 2, 3))
        elif year in years(pn24):
            coeff = process_segments(year, pn24, -86760, (0, 3), -360, (1, 2))
        elif year in years(pn25):
            coeff = process_segments(year, pn25, -86760, (0, 2, 3), -360, (1,))
        elif year in years(pn26):
            coeff = process_segments(year, pn26, -86760, (0, 1, 2, 3))
        elif year in years(pn27):
            coeff = process_segments(year, pn27, -86760, (1,), -420, (0, 2, 3))
        elif year in years(pn28):
            coeff = process_segments(year, pn28, -86760, (0,), -86820, (1,),
                -420, (2, 3))
        elif year in years(pn29):
            coeff = process_segments(year, pn29, -86760, (0, 3), -86820, (1,),
                -420, (2,))
        elif year in years(pn30):
            coeff = process_segments(year, pn30, -86820, (0, 3), -86760,
                (1, 2))
        elif year in years(pn31):
            coeff = process_segments(year, pn31, -540, (0, 2, 3), -420, (1,))
        elif year in years(pn32):
            coeff = process_segments(year, pn32, -86820, (0,), -540, (1, 2, 3))
        elif year in years(pn33):
            coeff = process_segments(year, pn33, -86940, (0,), -540, (1, 2),
                -86820, (3,))
        elif year in years(pn34):
            coeff = process_segments(year, pn34, -86940, (0,), -540, (1,),
                -86820, (2, 3))
        elif year in years(pn35):
            coeff = process_segments(year, pn35, -540, (0, 2, 3), 85800, (1,))
        elif year in years(pn36):
            coeff = process_segments(year, pn36, -540, (0, 1, 2, 3))
        elif year in years(pn37):
            coeff = process_segments(year, pn37, -86940, (0,), -540, (1, 2, 3))
        elif year in years(pn38):
            coeff = process_segments(year, pn38, -86940, (0, 3), -540, (1, 2))
        elif year in years(pn39):
            coeff = process_segments(year, pn39, -86940, (0, 2, 3), -540, (1,))
        elif year in years(pn40):
            coeff = process_segments(year, pn40, -86940, (0, 1, 2, 3))
        elif year in years(pn41):
            coeff = process_segments(year, pn41, -86940, (0,), -600, (1, 2),
                -720, (3,))
        # elif year in years(pn42):
        #     coeff = process_segments(year, pn42, )
        # elif year in years(pn43):
        #     coeff = process_segments(year, pn43, )
        # elif year in years(pn44):
        #     coeff = process_segments(year, pn44, )
        # elif year in years(pn45):
        #     coeff = process_segments(year, pn45, )
        # elif year in years(pn46):
        #     coeff = process_segments(year, pn46, )
        # elif year in years(pn47):
        #     coeff = process_segments(year, pn47, )
        # elif year in years(pn48):
        #     coeff = process_segments(year, pn48, )
        # elif year in years(pn49):
        #     coeff = process_segments(year, pn49, )
        # elif year in years(pn50):
        #     coeff = process_segments(year, pn50, )
        else:
            coeff = 0

        if year in (-1822, -1790, -1617, -1580, -1572, -1529, -1513, -1511,
                    -1510, -1507, -1494, -1484, -1475, -1473, -1462, -1457,
                    -1438, -1427, -1424, -1401, -1400, -1398, -1380, -1376,
                    -1363, -1354, ):
            coeff += -1
        elif year in (-1819, -1807, -1803, -1799, -1795, -1791, -1787, -1783,
                      -1766, -1762, -1758, -1754, -1750, -1746, -1491, -1487,
                      -1454, -1335, -1323, -1319, -1286, -1245, -870, -866,
                      -862, -858, -854, -691, -687, -527, -523, -519, ):
            coeff += -60
        elif year in (-1729, -1725, -1721, -1717, -1688, -1187, -820, -816):
            coeff += -120
        elif year in (-1747, ):
            coeff += -86340
        elif year in (-1347, -947, -547, ):
            coeff += -86400
        elif year in ( -1281, -1273, -1269, -1265, -1261, -1257, -1253, -740,
                      -736):
            coeff += 60
        elif year in (-1277,):
            coeff += 59
        elif year in (-1242, -1050, -887, -864):
            coeff += 1
        elif year in (-1240, -1236, -1232, -542, -538, ):
            coeff += 120
        elif year in (-783, ):
            coeff += 121

        return coeff

    def _mktime(self, dt: datetime.datetime) -> float:
        """
        This is the inverse function of localtime(). Its argument is the
        struct_time or full 9-tuple (since the dst flag is needed; use -1 as
        the dst flag if it is unknown) which expresses the time in local time,
        not UTC. It returns a floating-point number, for compatibility with
        time(). If the input value cannot be represented as a valid time,
        either OverflowError or ValueError will be raised (which depends on
        whether the invalid value is caught by Python or the underlying C
        libraries).
        """
        def gmt(u):
            date = self.posix_timestamp(u, *self.GMT_COORD, short=True,
                us=True)
            return (datetime.datetime(*date[:3], None, None, *date[3:6]) -
                    epoch) // datetime.timedelta(0, 1)

        hms = self._get_badi_hms(126)[-1]
        epoch = datetime.datetime(126, 16, 2, None, None, *hms)
        date = self._short_from_long_form(dt, time=dt.b_time)
        t = (datetime.datetime(*date) - epoch) // datetime.timedelta(0, 1)
        a = gmt(t) - t
        u1 = t - a
        t1 = gmt(u1)
        return t1

    def _short_from_long_form(self, dt: datetime.datetime,
                              time: tuple=()) -> tuple:
        """
        Convert the long form Badí' date to the short form Badí' date.
        """
        if dt.is_short:
            date = (*dt.b_date, None, None, *time)
        else:
            b_date = self.short_date_from_long_date(dt.b_date + time)
            date = (*b_date[:3], None, None, *b_date[3:])

        return date

    def _get_badi_hms(self, year: int) -> tuple:
        """
        Find the correct hour and minute of the day based on the coordinents.

        This is the last value returned, the first two are only for testing.
        """
        jd = self.jd_from_badi_date((year, 16, 2), *self.LOCAL_COORD)
        mjd = jd + self._meeus_from_exact(jd)
        ss = self._sun_setting(mjd, *self.LOCAL_COORD)
        f_ss = math.floor(ss) + round(ss % 1 * 1440) / 1440
        hms = self._hms_from_decimal_day(f_ss + 0.5)
        # Where 24 is hours in a day and the GMT offset.
        b_time = (((24 + self.LOCAL_COORD[-1]) / 24) - (f_ss + 0.5)) % 1
        #print(year, f_ss, b_time, file=sys.stderr)
        return ss, hms, self._hms_from_decimal_day(b_time)[:2]  # hh & mm only


def _group_sequences(lst: list) -> list:
    """
    This function combines all years into a tuple. If there is a spread of
    years they the first and last year is placed in a tuple which is then
    placed in the outer tupple.

    Thanks to ChatGPT for deriving this algorithm.
    """
    grouped = []
    start = lst[0]  # Start of a potential sequence
    prev = lst[0]   # Previous number in sequence

    for i in range(1, len(lst)):
        # Check if the sequence continues
        if lst[i] == prev + 1:
            prev = lst[i]
        else:
            # If the sequence is 3 or more, add as a tuple (first, last)
            if prev - start >= 2:
                grouped.append((start, prev))
            else:
                # Otherwise, add numbers individually
                grouped.extend(range(start, prev + 1))

            # Reset start and prev for new sequence
            start = lst[i]
            prev = lst[i]

    # Handle the last sequence or number
    if prev - start >= 2:
        grouped.append((start, prev))
    else:
        grouped.extend(range(start, prev + 1))

    return grouped


if __name__ == "__main__":
    import time
    import argparse
    import pprint

    def convert_to_float(value):
        values = value.split()

        if len(values) != 3:
            raise argparse.ArgumentError("Must have three arguments, found "
                                         f"{len(values)}.")

        return tuple(map(float, values))

    parser = argparse.ArgumentParser(
        description=("Test POSIX dates and times."))
    parser.add_argument(
        '-m', '--mktime', action='store_true', default=False, dest='mktime',
        help="Test the _mktime() method for accuracy.")
    parser.add_argument(
        '-p', '--modula', action='store_true', default=False, dest='modula',
        help="Find the modula for determining coefficients (NOT USED).")
    parser.add_argument(
        '-q', '--coeff', action='store_true', default=False, dest='coeff',
        help="Dump data for determining coefficients (NOT USED).")
    parser.add_argument(
        '-s', '--sunset', action='store_true', default=False, dest='sunset',
        help="Find the sunset for given years.")
    parser.add_argument(
        '-C', '--coord', type=convert_to_float, action='store',
        default=(51.477928, -0.001545, 0.0), dest='coord',
        help="The coordinates with GMT offset in hours. Defalult is GMT.")
    parser.add_argument(
        '-D', '--debug', action='store_true', default=False, dest='debug',
        help="Run in debug mode.")
    parser.add_argument(
        '-1', '--coeff1',action='store_true', default=False, dest='coeff1',
        help=("Test the _mktime() method for accuracy using coefficients 1 "
              "(NOT USED).)"))
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

    if options.debug:
        sys.stderr.write("DEBUG--options: {}\n".format(options))

    if options.mktime:  # -m
        if options.start is None or options.end is None:
            print("If option -m is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            start_time = time.time()
            data = pt.mktime(options)
            k = '-K ' if options.kill_coeff else ''
            coord = ' '.join([str(coord) for coord in options.coord])
            alt = '1' if options.coeff1 else ''
            print(f"./contrib/misc/{basename} -m{alt}C \"{coord}\" {k}"
                  f"-S{options.start} -E{options.end}")
            print("Badí'", " "*29, "Gregorian ", " "*12, "Badí'", "diff")
            print("Date", " "*17, "timestamp", " "*2, "timestamp", " "*2,
                  "Leap", " "*0, "Year", "Leap")
            [print(f"{str(b_date):22} "
                   f"{t1:12} "
                   f"{gmt_ts:12} "
                   f"{str(g_leap):5} "
                   f"{g_year:4} "
                   f"{str(b_leap):5} "
                   f"{diff:7} ")
             for b_date, t1, gmt_ts, g_leap, g_year, b_leap, diff in data]
            diffs = []  # [(diff, year)...]
            p = n = 0

            for item in data:
                if item[6] != 0:
                    year = item[0][0]
                    diffs.append((item[6], year))

                if item[6] > 0:
                    p += 1
                elif item[6] < 0:
                    n += 1

            print(f"Total Years Analyzed: {len(data)}\nPositive Errors: {p}\n"
                  f"Negative Errors: {n}\n   Total Errors: {len(diffs)}")
            set_items = set([diff for diff, year in diffs])
            items = []

            if set_items:
                min_max = set([abs(v) for v in set_items])
                print(f"Maximum deviation: {max(min_max)}\n"
                      f"Minimum deviation: {min(min_max)}")

            for diff in sorted(set_items):
                years = [y for d, y in diffs if d == diff]
                years = _group_sequences(years)
                items.append((diff, len(years), years))

            print(f"There is/are {len(set_items)} sequence(s) in items within "
                  f"years {options.start} to {options.end-1} range:")
            pprint.pp(items, width=70, compact=True)

            end_time = time.time()
            days, hours, minutes, seconds = pt._dhms_from_seconds(
                end_time - start_time)
            print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
                f"{round(seconds, 6):02.6} seconds.")
    elif options.modula:  # -p
        if options.start is None or options.end is None:
            print("If option -p is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print(f"./contrib/misc/{basename} -pS{options.start} "
                  f"-E{options.end}")
            data = pt.find_coefficents_modula(options)
            [print(f"{gy:> 5} {by:> 5}, {n:<1}") for gy, by, n in data]
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
        if options.start is None or options.end is None:
            print("If option -s is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
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
