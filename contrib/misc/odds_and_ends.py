#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/odds_and_ends.py
#

import os
import sys
from zoneinfo import ZoneInfo

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar
from badidatetime._timedateutils import _td_utils


class OddsAndEnds(BahaiCalendar):
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)
    LOCAL_COORDS = (35.5894, -78.7792, -5.0)
    BADI_COORDS = BahaiCalendar._BAHAI_LOCATION[:3]
    tz0 = ZoneInfo('Asia/Tehran')
    tz1 = ZoneInfo('US/Eastern')
    GREG_POSIX_DATES = (
        ((1844, 3, 19, 18, 16, 36, 710400), BADI_COORDS, tz0),
        ((1970, 1, 1), LOCAL_COORDS, tz1),  # POSIX epoch.
        ((2024, 12, 23, 20, 28, 24), LOCAL_COORDS, tz1),
        )

    def __init__(self):
        super().__init__()
        self._gc = GregorianCalendar()

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

    # Support methods

    def _year_week_day(self, year: int, month: int, day: int,
                       week0: bool=False) -> tuple:
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
    import time
    import argparse

    parser = argparse.ArgumentParser(
        description=("Some odds and ends."))
    parser.add_argument(
        '-o', '--ordinal', action='store_true', default=False,
        dest='ordinal', help="Test that _ymd2ord and _ord2ymd produce "
        "the correct dates.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help=f"End Badi year {BahaiCalendar.MAXYEAR} of sequence.")
    parser.add_argument(
        '-P', '--previous', action='store_true', default=False,
        dest='previous', help="Dump the previous and current ordinals.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help=f"Start Badi year {BahaiCalendar.MINYEAR} of sequence.")
    options = parser.parse_args()
    oae = OddsAndEnds()
    ret = 0
    basename = os.path.basename(__file__)

    if options.ordinal:  # -o
        if options.start is None or options.end is None:
            print("If option -o is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        elif options.previous:  # with -P
            start_time = time.time()
            data = oae.test_ordinals(options)
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
            days, hours, minutes, seconds = oae._dhms_from_seconds(
                end_time - start_time)
            print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
                f"{round(seconds, 6):02.6} seconds.")
        else:
            start_time = time.time()
            data = oae.test_ordinals(options)
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
            print(f"    Total Years Tested: {options.end-options.start}")
            errors = [l[4] is True for l in data].count(True)
            print(f"Total Number of Errors: {errors}")
            end_time = time.time()
            days, hours, minutes, seconds = oae._dhms_from_seconds(
                end_time - start_time)
            print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
                f"{round(seconds, 6):02.6} seconds.")
    else:
        parser.print_help()

    sys.exit(ret)
