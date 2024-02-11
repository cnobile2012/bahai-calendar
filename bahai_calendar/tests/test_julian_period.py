# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_julian_period.py
#
__docformat__ = "restructuredtext en"

import os
import unittest
#import datetime


from ..julian_period import JulianPeriod
from ..gregorian_calendar import GregorianCalendar


class TestJulianPeriod(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._jp = JulianPeriod()
        self._gc = GregorianCalendar()

    #@unittest.skip("Temporarily skipped")
    def test_julian_period_from_julian_day(self):
        """
        Test that the julian_period_from_julian_day method converts to
        the correct Julian date from the julian period.

        See https://aa.usno.navy.mil/data/JulianDate
        """
        data = (
            # Monday   4713 B.C. January 1  12:00:00.0        0.0
            (0, (1, 1, 1, 1)),
            # Tuesday  4713 B.C. January 2  12:00:00.0        1.0
            (1, (1, 1, 1, 2)),
            # Saturday    1 A.D. January 1  00:00:00.0  1721423.5
            (1721423.5, (1, 4714, 1, 1)),
            # Thursday 1582 A.D. October 4  00:00:00.0  2299159.5
            (2299159.5, (1, 6295, 10, 4.0)),
            # Friday   1582 A.D. October 15 00:00:00.0  2299160.5
            (2299160.5, (1, 6295, 10, 5.0)),
            # Thursday 2019 A.D. October 3  00:00:00.0  2458759.5
            (2458759.5, (1, 6732, 9, 19.75)),
            )
        msg = "Expected {}, found {}"

        for day, expected_result in data:
            result = self._jp.julian_period_from_julian_day(day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_julian_period_date(self):
        """
        Test that the jd_from_julian_period_date method converts to
        the correct Julian Period day.
        """
        data = (
            # Monday   4713 B.C. January 1  12:00:00.0        0.0
            ((1, 1, 1, 1), 0),
            # Tuesday  4713 B.C. January 2  12:00:00.0        1.0
            ((1, 1, 1, 2), 1),
            # Saturday    1 A.D. January 1  00:00:00.0  1721423.5
            ((1, 4714, 1, 1), 1721423.5), # ** TODO ** Looks weard
            # Thursday 1582 A.D. October 4  00:00:00.0  2299159.5
            ((1, 6295, 10, 4.0), 2299159.5),
            # Friday   1582 A.D. October 15 00:00:00.0  2299160.5
            ((1, 6295, 10, 5.0), 2299160.5),
            # Thursday 2019 A.D. October 3  00:00:00.0  2458759.5
            ((1, 6732, 9, 19.75), 2458759.5),
            )
        msg = "Expected {}, found {}"

        for date, expected_result in data:
            result = self._jp.jd_from_julian_period_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))
