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


class BadiDateFromJD(BahaiCalendar):
    GMT_COORDS = (51.477928, -0.001545, 0)
    BADI_COORDS = BahaiCalendar._BAHAI_LOCATION[:3]
    LOCAL_COORDS = (35.5894, -78.7792, -5.0)
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.gc = GregorianCalendar()

    def analyze(self, options):
        """
        Analyze the validity of the badi_date_from_jd method.

        -a with -s and -E
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
                    b_date = self._badi_date_from_jd(
                        jd, lat, lon, zone, short=True, _chk_on=False)
                    g_date = self.gc.gregorian_date_from_jd(jd, hms=True,
                                                            exact=True)
                    diff = self._subtract_tuples(date, b_date[:3])
                    data.append((g_date, date, jd, b_date, diff))

        return data

    def _badi_date_from_jd(self, jd: float, lat: float=None, lon: float=None,
                           zone: float=None, *, us: bool=False,
                           short: bool=False, fraction: bool=False,
                           trim: bool=False, rtd: bool=False,
                           _chk_on: bool=True) -> tuple:
        assert self._xor_boolean((fraction, us, rtd)), (
            "Cannot set more than one of fraction, us, or rtd to True.")

        def get_leap_year_info(year, _chk_on):
            leap = self._is_leap_year(year, _chk_on=_chk_on)
            return leap, 4 + leap

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self._BAHAI_LOCATION[:3]

        md = jd - self._BADI_EPOCH
        # This is only needed for the last two days of Badi year 1161
        # and the day before the epoch.
        y = 1 if md < 424046 and md != 0 else 0
        # Find the year
        year = math.floor(md / self._MEAN_TROPICAL_YEAR) + y
        leap, ld = get_leap_year_info(year, _chk_on)
        # Get 1st day of year so we can find the number of days to the JD.
        fdoy = (year, 1, 1)
        fjdoy = self.jd_from_badi_date(fdoy, lat, lon, zone, _chk_on=_chk_on)
        # Fix day if needed.
        yr = year - 1 if (math.floor(fjdoy) - math.floor(jd)) > 0 else year

        if yr != year:
            year = yr
            fdoy = (year, 1, 1)
            fjdoy = self.jd_from_badi_date(fdoy, lat, lon, zone,
                                           _chk_on=_chk_on)
            leap, ld = get_leap_year_info(year, _chk_on)

        days = math.floor(jd) - math.floor(fjdoy) + 1

        if days <= 342:                 # Month 1 - 18
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= 366:  # Month 19
            day = days - (342 + ld)
        else:                           # Ayyam-i-Ha
            day = days % 342

        month_days = list(self._BADI_MONTH_NUM_DAYS)
        month_days[18] = (0, ld)  # Fix Ayyám-i-Há days

        for month, ds in month_days:
            if days <= ds: break
            days -= ds

        self._debug_print(
            "Stage 0--jd: {}, date: {}, days: {}, fjdoy: {}, md: {}, y: {}, "
            "leap: {}", (jd, (year, month, day), days, fjdoy, md, y, leap))
        year, month, day, frac = self.__adjust_date(jd, year, month, day,
                                                    lat, lon, zone)

        if fraction:
            b_date = year, month, round(day + frac, 6)
        elif rtd:
            day = round(day + frac)
            b_date = year, month, day

            if not short:
                b_date = self.long_date_from_short_date(b_date, trim=trim,
                                                        _chk_on=_chk_on)
            assert frac < 1.0, (
                "If this assertion fires send this entire message to the "
                f"developer: jd: {jd}, day: {day}, frac: {frac}, "
                f"b_date: {b_date}")
        else:
            trim = trim if us else True
            date = year, month, day, *self._hms_from_decimal_day(frac)
            l_date = self.long_date_from_short_date(date, trim=True,
                                                    _chk_on=_chk_on)
            b_date = self.kvymdhms_from_b_date(l_date, us=us, short=short,
                                               trim=trim, _chk_on=_chk_on)

        return b_date

    def __adjust_date(self, jd, year, month, day, lat, lon, zone):
        """
        Adjust the date based on the latitude, longitude, and zone. The JD
        value is in local time so all JDs that are compared to it must also
        be in local time.

        :param float jd:
        :param int year:
        :param int, month:
        :param int day:
        :param float lat:
        :param float lon:
        :param float zone:
        :returns: The year, month, day, frag in a tuple.
        :rtype: tuple
        """
        def day_before(year, month, day):
            if day <= 0:
                if 2 <= month <= 18:  # Months 2 - 18 -> to previous month
                    month -= 1
                    day = 19
                elif month == 19:  # Month 19 -> Ayyám-i-Há
                    month = 0
                    day = 4 + self._is_leap_year(year)
                elif month == 0:  # Ayyám-i-Há
                    month = 18
                    day = 19
                else:  # Month 1 -> Month 19 & year to previous year
                    year -= 1
                    month = 19
                    day = 19

            return year, month, day

        def day_after(year, month, day):
            dim = 4 + self._is_leap_year(year) if month == 0 else 19

            if day > dim:
                if 1 <= month <= 17:
                    month += 1
                    day = 1
                elif month == 18:
                    month = 0
                    day = 1
                elif month == 0:
                    month = 19
                    day = 1
                else:  # Month 19
                    year += 1
                    month = 1
                    day = 1

            return year, month, day

        jd0, jd_frac = divmod(jd, 1)
        jd1 = self._meeus_from_exact(jd0)
        ss0 = self._sun_setting(jd1, lat, lon)
        ss_frac = round(self._local_zone_correction(ss0, zone),
                        self._ROUNDING_PLACES)

        if (jd_frac + 0.5) % 1 < (ss_frac + 0.5) % 1:
            ss1 = self._sun_setting(jd1 - 1, lat, lon)
            ss_frac1 = round(self._local_zone_correction(ss1, zone),
                             self._ROUNDING_PLACES)
            # Calculate the time between the previous sunset and the
            # following midnight of the JD day then add the results to
            # the JD day fraction to get the Badi time.
            frac = (1 - ss_frac1 + jd_frac) % 1
            # Get the original GMT by reversing the time zone value.
            o_jd = self._local_zone_correction(jd, zone, inverse=True,
                                               mod_jd=True)
            oi_jd, o_jd_f = divmod(o_jd, 1)
            ni_jd, n_jd_f = divmod(jd, 1)

            if not (oi_jd == ni_jd and (o_jd_f < 0.5 and n_jd_f >= 0.5) or
                    (o_jd_f == n_jd_f)):
                day -= 1
                self._debug_print(
                    "Stage 1--jd: {}, jd1: {}, date: {}, ss1: {}, "
                    "jd_frac: {}, ss_frac: {}, ss_frac1: {}, frac: {}",
                    (jd, jd1, (year, month, day), ss1, jd_frac, ss_frac,
                     ss_frac1, frac))
            else:
                self._debug_print(
                    "Stage 2--jd: {}, jd1: {}, date: {}, ss1: {}, "
                    "jd_frac: {}, ss_frac: {}, ss_frac1: {}, frac: {}",
                    (jd, jd1, (year, month, day), ss1, jd_frac, ss_frac,
                     ss_frac1, frac))
        else:
            diff = ss_frac - jd_frac
            dl = self._day_length(jd - 1, lat, lon, decimal=True)
            frac = round(dl - diff, self._ROUNDING_PLACES) % 1
            self._debug_print(
                "Stage 3--jd: {}, jd0: {}, date: {}, ss0: {}, jd_frac: {}, "
                "ss_frac: {}, diff: {}, frac: {}",
                (jd, jd0, (year, month, day), ss0, jd_frac, ss_frac, diff,
                 frac))

        year, month, day = day_before(year, month, day)
        year, month, day = day_after(year, month, day)
        return year, month, day, frac

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


if __name__ == "__main__":
    import argparse
    import time

    parser = argparse.ArgumentParser(
        description=("Test sunset before (-261, 11, 19, 0, 0, 20.9952) "
                     "Badi--(1582, 10, 15, 17, 27) Gregorian."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Analyze the validity of the badi_date_from_jd method.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End Badí' year of sequence.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start Badí' year of sequence.")
    options = parser.parse_args()

    bdfjd = BadiDateFromJD()
    basename = os.path.basename(__file__)
    ret = 0

    if options.analyze:  # -a
        if None in (options.start, options.end):
            print("If option -a is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            start_time = time.time()
            data = bdfjd.analyze(options)
            print(f"./contrib/misc/{basename} -aS {options.start} "
                  f"-E {options.end}")
            underline_length = 113
            print('-' * underline_length)
            print("Gregorian Date", ' ' * 16, "Orig Badi Date  JD", ' ' * 15,
                  "Badi Date", ' ' * 22, "Difference")
            print('-' * underline_length)
            [print(f"{str(g_date):31} "
                   f"{str(date):15} "
                   f"{fmt_float(jd, 7, 10)} "
                   f"{str(b_date):32} "
                   f"{str(diff):13}"
                   )
             for g_date, date, jd, b_date, diff in data]
            print('-' * underline_length)
            print(f"Total years analysed: {options.end - options.start}")
            errors = []

            for g_date, date, jd, b_date, diff in data:
                if diff != (0, 0, 0):
                    errors.append((date, jd, b_date[:3], diff))

            if errors:
                print("\nOriginal Date   JD                 Badi Date       "
                      "Difference")
                print('-' * 64)
                [print(f"{str(date):15} "
                       f"{jd:<18} "
                       f"{str(b_date):15} "
                       f"{str(diff):13}"
                       )
                 for o_date, jd, b_date, diff in errors]
                print('-' * 64)

            end_time = time.time()
            days, hours, minutes, seconds = bdfjd._dhms_from_seconds(
                end_time - start_time)
            print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
                f"{round(seconds, 6):02.6} seconds.")
    else:
        parser.print_help()

    sys.exit(ret)
