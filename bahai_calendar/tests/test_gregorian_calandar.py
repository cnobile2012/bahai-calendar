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

        November 12, 1945 (Gregorian) = 710347
        """
        date_rep = (2094, 4, 4)
        result = self._gc.fixed_from_gregorian(date_rep)
        expected_result = 764652
        msg = f"Expected result {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)



