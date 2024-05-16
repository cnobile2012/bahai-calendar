# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_julian_period.py
#
__docformat__ = "restructuredtext en"

import os
import unittest
#import datetime

from ..base_calendar import BaseCalendar
from ..julian_period import JulianPeriod
from ..gregorian_calendar import GregorianCalendar


class TestJulianPeriod(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._jp = JulianPeriod()
        self._gc = GregorianCalendar()

    #@unittest.skip("Temporarily skipped")
    def test_julian_centuries(self):
        """
        Test that the julian_centuries method returns the Julian
        century in dynamical time from a Julian moment.
        """
        data = (
            (2394646.5, -1.5577960301163587),
            (self._jp.J2000, 0.0),
            )
        msg = "Expected {} for tee {}, found {}"

        for tee, expected_result in data:
            result = self._jp.julian_centuries(tee)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, tee, result))

    @unittest.skip("Temporarily skipped")
    def test_julian_millennia(self):
        """
        """

    #@unittest.skip("Temporarily skipped")
    def test_julian_leap_year(self):
        """
        Test that the julian_leap_year method returns true or false
        for the Julian leap year.
        """
        data = (
            (0, True),
            (1, False),
            (4, True)
            )
        msg = "Expected {} for {}, found {}"

        for year, expected_result in data:
            result = self._jp.julian_leap_year(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))
