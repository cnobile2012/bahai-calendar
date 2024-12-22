#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# year_week_day.py
#
# This script has only one purpose and that is to find the years that
# are in the 53rd week of the given year and is considered to be in
# the 1 week of the following year.
#

import os
import sys

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar
from badidatetime._timedateutils import _td_utils


def _year_week_day(year:int, month:int, day:int, week0:bool=False) -> tuple:
    """
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
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)

    bc = BahaiCalendar()

    # -1842, 1162
    # 1, 182

    for year in range(173, 182):
        is_leap = bc._is_leap_year(year)

        for month in MONTHS:
            dm = 19 if month != 0 else 4 + is_leap

            for day in range(1, dm + 1):
                date = (year, month, day)
                y, w, d, var = _year_week_day(*date)

                if var == 3:
                    print(date, is_leap, y, w, d)
