#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# odds_and_ends.py
#

import os
import sys
from datetime import datetime as dtime
from zoneinfo import ZoneInfo

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar, datetime
from badidatetime._timedateutils import _td_utils


class OddsAndEnds:
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)
    LOCAL_COORDS = (35.5894, -78.7792, -5.0)
    BADI_COORDS = BahaiCalendar.BAHAI_LOCATION[:3]
    tz0 = ZoneInfo('Asia/Tehran')
    tz1 = ZoneInfo('US/Eastern')
    GREG_POSIX_DATES = (
        ((1844, 3, 19, 18, 16, 36, 710400), BADI_COORDS, tz0),
        ((1970, 1, 1), LOCAL_COORDS, tz1), # POSIX epoch.
        ((2024, 12, 23, 20, 28, 24), LOCAL_COORDS, tz1),
        )

    def __init__(self):
        self._bc = BahaiCalendar()
        self._gc = GregorianCalendar()

    def find_year_week_day_of_week(self, options):
        """
        -1842, 1162
        1, 182
        """
        for year in range(173, 182):
            is_leap = self._bc._is_leap_year(year)

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + is_leap

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    y, w, d, var = self._year_week_day(*date)

                    if var == 3:
                        print(date, is_leap, y, w, d)

    def find_posix_time(self, options):
        """

        """
        data = []

        for date, coords, tz in self.GREG_POSIX_DATES:
            m_jd = self._gc.jd_from_gregorian_date(date)
            db_m_jd = m_jd - 1 # We need the day before.
            ss = self._gc._sun_setting(db_m_jd, *coords)
            ss -= self._bc._exact_from_meeus(ss) # Convert to exact JD
            b_date = self._bc.badi_date_from_jd(ss, *coords, short=True,
                                                trim=True)
            # Get the fractional day between sunset and UTC 12 a.m. on the
            # following day
            bm_frac = 0.5 - ss % 1
            # Add the before midnight fraction to the fractional part of
            # the Gregorian day minus 0.5.
            total_frac = bm_frac + (m_jd % 1 - 0.5)
            hms = self._bc.hms_from_decimal_day(total_frac)
            full_date0 = b_date[:3] + hms
            full_date1 = b_date[:3] + (None, None) + hms
            greg_ts = dtime(*date, tzinfo=tz).timestamp()

            #print(full_date0, full_date1, file=sys.stderr)

            badi_ts = datetime.datetime(*full_date1, tzinfo=tz).timestamp()
            diff = round(badi_ts - greg_ts, 6)
            data.append((date, b_date, hms, full_date0, greg_ts, badi_ts, diff))

        return data

    def _year_week_day(self, year:int, month:int, day:int,
                       week0:bool=False) -> tuple:
        """
        This method has only one purpose and that is to find the years that
        are in the 53rd week of the given year and is considered to be in
        the 1 week of the following year.

        Return the year, week, and day of the week.
        """
        week1jalal = _td_utils._isoweek1jalal(year)
        today = _td_utils._ymd2ord(year, month, day)
        # Internally, week and day have origin 0
        week, day = divmod(today - week1jalal, 7)
        var = 0

        if not week0:
            if week < 0:
                year -= 1
                week1jalal = _td_utils._isoweek1jalal(year)
                week, day = divmod(today - week1jalal, 7)
                var = 1
            elif week >= 52:
                var = 2

                if today >= _td_utils._isoweek1jalal(year+1):
                    year += 1
                    week = 0
                    var = 3

        return year, week+1, day+1, var


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Some odds and ends."))
    parser.add_argument(
        '-a', '--analyze0', action='store_true', default=False,
        dest='analyze0', help="Find the dates that are >= the 52 week.")
    parser.add_argument(
        '-b', '--analyze1', action='store_true', default=False,
        dest='analyze1', help="Reconcile POSIX timestamps with Badi Dates.")

    options = parser.parse_args()
    oae = OddsAndEnds()
    ret = 0

    if options.analyze0: # -a
        oae.find_year_week_day_of_week(options)
    elif options.analyze1: # -b
        print("Gregorian Date                    Badi Date                  "
              "   HMS               Combined                     Greg TS"
              "          Badi TS          Diff")
        print('-'*155)
        [print(f"{str(date):33} "
               f"{str(b_date):29} "
               f"{str(hms):17} "
               f"{str(comb):28} "
               f"{greg_ts:>+16.4f} "
               f"{badi_ts:>+16.4f} "
               f"{diff:>+10.4f}"
               ) for (date, b_date, hms, comb, greg_ts,
                      badi_ts, diff) in oae.find_posix_time(options)]
    else:
        parser.print_help()

    sys.exit(ret)