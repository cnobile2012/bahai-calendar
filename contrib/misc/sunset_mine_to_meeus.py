#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import sys

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar


class SunsetMineToMeeus(BahaiCalendar):

    TEST_DATES = (
        ((1, 3, 20, 18, 13),     (-1842, 1, 1)),   # 0
        ((100, 2, 28, 17, 57),   (-1744, 0, 4)),   # 0
        ((100, 3, 1, 17, 58),    (-1744, 19, 1)),  # -1
        ((200, 2, 28, 17, 57),   (-1644, 0, 5)),   # -1
        ((200, 3, 1, 17, 57),    (-1644, 19, 1)),  # -2
        ((300, 2, 28, 17, 56),   (-1544, 0, 4)),   # -2
        ((300, 3, 1, 17, 57),    (-1544, 19, 1)),  # -3
        ((500, 2, 28, 17, 57),   (-1344, 0, 4)),   # -3
        ((500, 3, 1, 17, 58),    (-1344, 19, 1)),  # -4
        ((600, 2, 28, 17, 57),   (-1244, 0, 5)),   # -4
        ((600, 3, 1, 17, 57),    (-1244, 19, 1)),  # -5
        ((700, 2, 28, 17, 56),   (-1144, 0, 4)),   # -5
        ((700, 3, 1, 17, 57),    (-1144, 19, 1)),  # -6
        ((900, 2, 28, 17, 57),   (-944, 0, 4)),    # -6
        ((900, 3, 1, 17, 57),    (-944, 19, 1)),   # -7
        ((1000, 2, 28, 17, 56),  (-844, 0, 5)),    # -7
        ((1000, 3, 1, 17, 57),   (-844, 19, 1)),   # -8
        ((1100, 2, 28, 17, 56),  (-744, 0, 4)),    # -8
        ((1100, 3, 1, 17, 57),   (-744, 19, 1)),   # -9
        ((1300, 2, 28, 17, 56),  (-544, 0, 4)),    # -9
        ((1300, 3, 1, 17, 57),   (-544, 19, 1)),   # -10
        ((1400, 2, 28, 17, 56),  (-444, 0, 5)),    # -10
        ((1400, 3, 1, 17, 57),   (-444, 19, 1)),   # -11
        ((1500, 2, 28, 17, 56),  (-344, 0, 4)),    # -11
        ((1500, 3, 1, 17, 56),   (-344, 19, 1)),   # -12
        ((1582, 10, 14, 17, 28), (-261, 11, 18, 0, 0, 0.2592)),  # -12
        ((1582, 10, 15, 17, 27), (-261, 11, 19, 0, 0, 20.9952)), # -2
        )

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()

    def analyze(self, options):
        data = []

        for g_date, b_date in self.TEST_DATES:
            g_jd0 = self.gc.jd_from_gregorian_date(g_date, exact=False)
            g_jd1 = self.gc.jd_from_gregorian_date(g_date, exact=True)
            b_jd = self.jd_from_badi_date(b_date)
            date = self.badi_date_from_jd(b_jd, short=True)
            data.append((g_date, g_jd0, g_jd1, b_date, date, b_jd))

        return data


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test sunset before (-261, 11, 19, 0, 0, 20.9952) "
                     "Badi--(1582, 10, 15, 17, 27) Gregorian."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Analyze the sunset between my and Meeus' algorithms.")
    options = parser.parse_args()

    smtm = SunsetMineToMeeus()
    ret = 0

    if options.analyze:
        data = smtm.analyze(options)
        print("g_date                 "
              "g_jd0          "
              "g_jd1          "
              "expected b_date               "
              "calculated date                 "
              "b_jd           "
              "g_jd0-b_jd "
              "g_jd1-b_jd"
              )
        [print(f"{str(g_date):<22} "
               f"{g_jd0:<14} "
               f"{g_jd1:<14} "
               f"{str(b_date):<29} "
               f"{str(date):<31} "
               f"{b_jd:<14} "
               f"{round(g_jd0 - b_jd, 6):<9}  "
               f"{round(g_jd1 - b_jd, 6):<9}"
               )
         for g_date, g_jd0, g_jd1, b_date, date, b_jd in data]
    else:
        parser.print_help()

    sys.exit(ret)
