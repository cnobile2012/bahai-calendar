#!/usr/bin/env python

import os
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from bahai_calendar import BahaiCalendar, GregorianCalendar


class DumpjulianPeriodFromJulianDay(BahaiCalendar):

    CORRECT = lambda self, x: 0.25 if x % 1 in (0.25, 0.75) else 0

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()

    def dump(self):
        # Leap years: 2712 and 2716
        data = (
            #                                   RESULT
            # Epoch -4712-01-01T12:00:00
            (0.0, (1, 1, 1, 1.0)),            # correct
            (0.5, (1, 1, 1, 1.5)),            # correct
            (1.0, (1, 1, 1, 2.0)),            # correct
            (365.0, (1, 1, 12, 31.0)),        # 32.0  high 1.0
            (365.5, (1, 1, 12, 31.5)),        # 32.5  high 0.5

            (366.0, (1, 2, 1, 1.0)),          # 1.5  high 0.5
            (366.5, (1, 2, 1, 1.5)),          # 2.0  high 0.5

            (1721055.0, (1, 4712, 12, 29.0)), # correct
            (1721055.5, (1, 4712, 12, 29.5)), # correct
            ## (1721056.0, (1, 4712, 12, 30.0)), # 29.0  low  1.0
            ## (1721056.5, (1, 4712, 12, 30.5)), # correct
            ## (1721057.0, (1, 4712, 12, 31.0)), # correct
            ## (1721057.5, (1, 4712, 12, 31.5)), # correct
            ## (1721058.0, (1, 4713, 1, 1.0)),   # correct

            ## (1721420.0, (1, 4713, 12, 28.0)), # correct
            ## (1721420.5, (1, 4713, 12, 28.5)), # 29.5  high 1.0
            ## (1721421.0, (1, 4713, 12, 29.0)), # 30.0  high 1.0
            ## (1721421.5, (1, 4713, 12, 29.5)), # 30.5  high 1.0
            ## (1721422.0, (1, 4713, 12, 30.0)), # 31.0  high 1.0
            ## (1721422.5, (1, 4713, 12, 30.5)), # 31.5  high 1.0
            ## (1721423.0, (1, 4713, 12, 31.0)), # 32.0  high 1.0
            ## (1721423.5, (1, 4713, 12, 31.5)), # 1.00  high 1.0
            ## (1721424.0, (1, 4714, 1, 1.0)),   # 1.50  high 0.5
            ## (1721424.5, (1, 4714, 1, 1.5)),   # 2.00  high 0.5
            ## (1721425.0, (1, 4714, 1, 2.0)),   # 2.50  high 0.5

            ## (1721788.0, (1, 4714, 12, 31.0)), # 30.50 high 0.5
            ## (1721788.5, (1, 4714, 12, 31.5)), # 1.00  high 0.5
            ## (1721789.0, (1, 4715, 1, 1.0)),   # 1.50  high 0.5
            ## (1721789.5, (1, 4715, 1, 1.5)),   # 2.00  high 0.5
            )
        result = []

        for days, date in data:
            derived_date = self.julian_period_from_julian_day(days)
            result.append((days, date, derived_date))

        return result


if __name__ == "__main__":
    djpjd = DumpjulianPeriodFromJulianDay()
    data = djpjd.dump()
    pprint.pprint(data)
