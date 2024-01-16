# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_gregorian_calandar.py
#
__docformat__ = "restructuredtext en"

import os
import math
import unittest

from ..gregorian_calendar import GregorianCalendar


class TestGregorianCalandar(unittest.TestCase):
    """
    This test class provides unittests for the GregorianCalendar class.
    Many tests use the Gregorian dates and their cooesponding fixed dates
    below.

    March 21, 1844   = 673222 (Baha'i Epoch)
    January, 1, 1970 = 719163 (UNIX Epoch)
    July 6, 622      = 227015 (Islamic Epoch)
    """

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

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_year_from_fixed(self):
        """
        Test that the gregorian_year_from_fixed method returns a
        Gregorian year corresponding to the fixed date
        """
        dates = ((673222, 1844), (719163, 1970), (227015, 622))
        msg = "Expected result {} for year {}, found {}."

        for date, expected_result in dates:
            result = self._gc.gregorian_year_from_fixed(date)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_date_difference(self):
        """
        Test that the gregorian_date_difference method returns the
        number of days from Gregorian date g_date1 until g_date2.

        C(n, k) = n!/k!*(n−k)!
        """
        date_reps = (
            (1844, 3, 21),
            (1970, 1, 1),
            (622, 7, 19)
            )
        expected_result = (446207, 492148, -446207)
        num = len(date_reps)
        combinations = []
        msg = "Expected result {} year {} - {}, found {}."

        for k in range(1, num + 1):
            j = math.comb(num, k)
            combinations.append((date_reps[k-1], date_reps[j-1]))

        for idx, (g_date1, g_date2) in enumerate(combinations):
            result = self._gc.gregorian_date_difference(g_date1, g_date2)
            er = expected_result[idx]
            self.assertEqual(er, result,
                msg.format(er, g_date1[0], g_date2[0], result))






