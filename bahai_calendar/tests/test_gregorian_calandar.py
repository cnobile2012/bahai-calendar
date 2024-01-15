# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_gregorian_calandar.py
#
__docformat__ = "restructuredtext en"

import os
import unittest

from ..gregorian_calendar import GregorianCalendar


class TestGregorianCalandar(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._gc = GregorianCalendar()

    def test_GREGORIAN_LEAP_YEAR(self):
        """
        Test that the lambda GREGORIAN_LEAP_YEAR function correctly
        determines the Gregorian leap year.
        """
        years = ((1844, True), (1951, False), (2064, True), (2100, False))
        msg = "Expected {} for year {}, found {}"

        for year, value in years:
            result = self._gc.GREGORIAN_LEAP_YEAR(year)
            self.assertEqual(value, result, msg.format(value, year, result))


    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_gregorian(self):
        """
        Test that the fixed_from_gregorian method returns the proper
        fixed date from a gregorian date.

        March 21, 1844   = 673222 (Baha'i Epoch)
        January, 1, 1970 = 719163 (UNIX Epoch)
        July 6, 622      = 227015 (Islamic Epoch)
        """
        date_reps = (
            ((1844, 3, 21), 673222),
            ((1970, 1, 1), 719163),
            ((622, 7, 19), 227015)
            )
        msg = "Expected result {} for year {}, found {}."

        for date_rep, expected_result in date_reps:
            result = self._gc.fixed_from_gregorian(date_rep)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date_rep[0], result))



