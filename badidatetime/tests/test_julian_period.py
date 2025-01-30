# -*- coding: utf-8 -*-
#
# badidatetime/test/test_julian_period.py
#
__docformat__ = "restructuredtext en"

import unittest

from ..julian_period import JulianPeriod
from ..gregorian_calendar import GregorianCalendar


class TestJulianPeriod(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._jp = JulianPeriod()
        self._gc = GregorianCalendar()

    #@unittest.skip("Temporarily skipped")
    def test__julian_centuries(self):
        """
        Test that the _julian_centuries method returns the Julian
        century in dynamical time from a Julian moment.
        """
        data = (
            (2394646.5, -1.5577960301163587),  # Badi epoch
            (1721425.5, -19.98958247775496),
            (self._jp._J2000, 0.0),
            )
        msg = "Expected {} for jd {}, found {}"

        for jd, expected_result in data:
            result = self._jp._julian_centuries(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test__julian_millennia(self):
        """
        Test that the julian_millennis method returns the Julian
        millennia in dynamical time from a Julian moment.
        """
        data = (
            (2394646.5, -0.15577960301163587),
            (1721425.5, -1.9989582477754961),
            (self._jp._J2000, 0.0),
            )
        msg = "Expected {} for jd {}, found {}"

        for jd, expected_result in data:
            result = self._jp._julian_millennia(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))
