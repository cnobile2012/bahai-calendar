#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/posix_time.py
#

import os
import sys
import math
import pprint
import time
import datetime as dtime

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar, datetime


class PosixTests:
    """
    https://www.unixtimestamp.com/

    The Python datetime package seems to always give local time from
    timestamps not UTC time. For Example:
    In [18]: dtime.datetime.fromtimestamp(18000) This -5 hours from UTC time.
    Out[19]: datetime.datetime(1970, 1, 1, 0, 0)
    """
    GMT_COORD = datetime.GMT_COORD
    BADI_COORD = BahaiCalendar.BAHAI_LOCATION[:3]
    # Force standard time in US/Eastern (America/New_York)
    # so test works all year.
    LOCAL_COORD = datetime.LOCAL_COORD[:2] + (-5,)

    UTC_US_E_TZ = dtime.timezone(dtime.timedelta(hours=-5))
    BADI_US_E_TZ = datetime.timezone(datetime.timedelta(hours=-5))

    TEST_TS = (
        ((1970, 1, 1), (126, 16, 2, None, None, 7, 58, 31.4976), BADI_US_E_TZ,
         18000),
        ((1970, 1, 1), (126, 16, 2, None, None, 7, 58, 31.4976), None, 18000),
        #((2024, 11, 24), (), BADI_US_E_TZ, 0),
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
            #            Greg    Greg TS  Badi    Badi TS
            data.append((g_date, gt,      b_date, bt))

        return data


if __name__ == "__main__":
    #import datetime
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test POSIX dates and times."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze0',
        help="Analyze the POSIX date and time for accuracy.")

    options = parser.parse_args()
    pt = PosixTests()
    ret = 0

    if options.analyze0: # -a
        #print(pt.analize0(options))

        [print(f"{str(g_date):13} "
               f"{gt:10} "
               f"{str(b_date):13} "
               f"{bt:10} "
               ) for g_date, gt, b_date, bt in pt.analize0(options)]
    else:
        parser.print_help()

    sys.exit(ret)
