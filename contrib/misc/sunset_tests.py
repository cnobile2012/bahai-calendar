#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import sys
import importlib
import time
from zoneinfo import ZoneInfo
from unittest.mock import patch

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar
import badidatetime
badidt = importlib.import_module('badidatetime.datetime')


class SunsetTests(BahaiCalendar):

    TEST_DATES = (
        (1, 3, 20, 23, 59),      # 0
        (100, 2, 28, 23, 59),    # 0
        (100, 3, 1, 23, 59),     # -1
        (200, 2, 28, 17, 57),    # -1
        (200, 3, 1, 17, 57),     # -2
        (300, 2, 28, 17, 56),    # -2
        (300, 3, 1, 17, 57),     # -3
        (500, 2, 28, 17, 57),    # -3
        (500, 3, 1, 17, 58),     # -4
        (600, 2, 28, 17, 57),    # -4
        (600, 3, 1, 17, 57),     # -5
        (700, 2, 28, 17, 56),    # -5
        (700, 3, 1, 17, 57),     # -6
        (900, 2, 28, 17, 57),    # -6
        (900, 3, 1, 17, 57),     # -7
        (1000, 2, 28, 17, 56),   # -7
        (1000, 3, 1, 17, 57),    # -8
        (1100, 2, 28, 17, 56),   # -8
        (1100, 3, 1, 17, 57),    # -9
        (1300, 2, 28, 17, 56),   # -9
        (1300, 3, 1, 17, 57),    # -10
        (1400, 2, 28, 17, 56),   # -10
        (1400, 3, 1, 17, 57),    # -11
        (1500, 2, 28, 17, 56),   # -11
        (1500, 3, 1, 17, 56),    # -12
        (1582, 10, 14, 17, 28),  # -12
        (1582, 10, 15, 17, 27),  # -2
        )

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()

    def analyze(self, options):
        """
        Test the last valid day before the next day is invalid because the
        Julian calendar recorded too many leap years.

        -a
        """
        data = []

        for g_date in self.TEST_DATES:
            g_jd0 = self.gc.jd_from_gregorian_date(g_date, exact=False)
            g_jd1 = self.gc.jd_from_gregorian_date(g_date, exact=True)
            b_date = self.badi_date_from_gregorian_date(g_date, short=True)
            b_jd = self.jd_from_badi_date(b_date)
            data.append((g_date, g_jd0, g_jd1, b_jd, b_date))

        return data

    @patch.object(badidt, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def record_sunset_flip(self, options):
        """
        Create a table in one second intervals from just before to just after
        sunset.

        -f
        """
        data = []
        tz = ZoneInfo('US/Eastern')
        tz_info = badidatetime.TZWithCoords.fromzoneinfo(
            tz, 35.5894, -78.7792, -5.0)
        exit = False

        while not exit:
            now = badidatetime.datetime.now(tz=tz_info)
            start_day = now.day

            if now.hour == 23:
                while not exit:
                    b_now = badidatetime.datetime.now(tz=tz_info)

                    if now.minute == 59 and now.second >= 30:
                        b_today = badidatetime.date.today()
                        data.append((b_now, b_today))

                    if (now.day > start_day and now.minute >= 1
                        and now.second > 30):
                        exit = True

                    time.sleep(1.0)

            time.sleep(20)  # Test every 20 seconds

        return data


def find_elapse_time(start_time):
    end_time = time.time()
    days, hours, minutes, seconds = st._dhms_from_seconds(
        end_time - start_time)
    print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
          f"{round(seconds, 6):02.6} seconds.")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test various data points concerning sunset."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Analyze the sunset between my and Meeus' algorithms.")
    parser.add_argument(
        '-f', '--flip', action='store_true', default=False, dest='flip',
        help="Record the day and time flip when sunset happens..")
    options = parser.parse_args()

    st = SunsetTests()
    ret = 0
    basename = os.path.basename(__file__)

    if options.analyze:  # -a
        start_time = time.time()
        data = st.analyze(options)
        print(f"./contrib/misc/{basename} -a")
        print("g_date                  "
              "g_jd0           "
              "g_jd1           "
              "b_jd            "
              "calculated date                  "
              "g_jd0-b_jd  "
              "g_jd1-b_jd"
              )
        [print(f"{str(g_date):<22}  "
               f"{g_jd0:<14}  "
               f"{g_jd1:<14}  "
               f"{b_jd:<14}  "
               f"{str(b_date):<14}  "
               f"{round(g_jd0 - b_jd, 6):<9}   "
               f"{round(g_jd1 - b_jd, 6):<9}"
               )
         for g_date, g_jd0, g_jd1, b_jd, b_date in data]
        find_elapse_time(start_time)
    elif options.flip:  # -f
        data = st.record_sunset_flip(options)
        print(data)

    else:
        parser.print_help()

    sys.exit(ret)
