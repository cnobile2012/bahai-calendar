# -*- coding: utf-8 -*-
#
# badidatetime/test/test_badidatetime.py
#
__docformat__ = "restructuredtext en"

import os
import sys
import unittest

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)
print(BASE_DIR)

from badidatetime import datetime
from ..badi_calendar import BahaiCalendar


class TestBadiDatetime(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._bc = BahaiCalendar()

    #@unittest.skip("Temporarily skipped")
    def test__days_before_year(self):
        """
        Test that the _days_before_year function returns the correct
        number of days before the specified year.
        """
        data = (
            (-1842, 0),
            (-1841, 366), # Year -1842 was a leap year so must have 366 days
            (181, 738887),
            )
        msg = "Expected {} with year {}, found {}."

        for year, expected_result in data:
            result = datetime._days_before_year(self._bc, year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test__days_in_month(self):
        """
        Test that the _days_in_month function returns the correct days
        in the specified month.
        """
        data = (
            (181, 1, 19),
            (181, 2, 19),
            (181, 3, 19),
            (181, 4, 19),
            (181, 5, 19),
            (181, 6, 19),
            (181, 7, 19),
            (181, 8, 19),
            (181, 9, 19),
            (181, 10, 19),
            (181, 11, 19),
            (181, 12, 19),
            (181, 13, 19),
            (181, 14, 19),
            (181, 15, 19),
            (181, 16, 19),
            (181, 17, 19),
            (181, 18, 19),
            (181, 0, 4),
            (181, 19, 19),
            (182, 0, 5),
            )
        msg = "Expected {} with year {} and month {}, found {}."

        for year, month, expected_result in data:
            result = datetime._days_in_month(self._bc, year, month)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, month, result))

    #@unittest.skip("Temporarily skipped")
    def test__days_before_month(self):
        """
        Test that the _days_before_month function returns the correct
        number in days in year preceeding the first day in the currect
        month.
        """
        data = (
            (181, 1, 365),
            (181, 2, 19),
            (181, 3, 38),
            (181, 4, 57),
            (181, 5, 76),
            (181, 6, 95),
            (181, 7, 114),
            (181, 8, 133),
            (181, 9, 152),
            (181, 10, 171),
            (181, 11, 190),
            (181, 12, 209),
            (181, 13, 228),
            (181, 14, 247),
            (181, 15, 266),
            (181, 16, 285),
            (181, 17, 304),
            (181, 18, 323),
            (181, 0, 342),
            (181, 19, 346),
            (183, 1, 366),
            )
        msg = "Expected {} with year {} and month {}, found {}."

        for year, month, expected_result in data:
            result = datetime._days_before_month(self._bc, year, month)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, month, result))

            # *** TODO *** Test assert

    #@unittest.skip("Temporarily skipped")
    def test__ymd2ord(self):
        """
        Test that the _ymd2ord function returns the correct number of days
        since Badi year -1842.
        """
        data = (
            ((-1842, 1, 1), 0),
            ((-1841, 1, 1), 0),
            ((181, 1, 1), 739253),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = datetime._ymd2ord(self._bc, *date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

            # *** TODO *** Test assert
