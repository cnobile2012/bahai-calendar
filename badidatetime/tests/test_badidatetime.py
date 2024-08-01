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
            (-1841, 366), # Year -1842 was a leap year
            (-1840, 731),
            (181, 738886),
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
        err_msg = "Month must be in range of 0..19"
        data = (
            (181, 1, False, 0),
            (181, 2, False, 19),
            (181, 3, False, 38),
            (181, 4, False, 57),
            (181, 5, False, 76),
            (181, 6, False, 95),
            (181, 7, False, 114),
            (181, 8, False, 133),
            (181, 9, False, 152),
            (181, 10, False, 171),
            (181, 11, False, 190),
            (181, 12, False, 209),
            (181, 13, False, 228),
            (181, 14, False, 247),
            (181, 15, False, 266),
            (181, 16, False, 285),
            (181, 17, False, 304),
            (181, 18, False, 323),
            (181, 0, False, 342),
            (181, 19, False, 346),
            (183, 1, False, 0), # 182 is a leap year
            (181, 20, True, err_msg),
            )
        msg = "Expected {} with year {} and month {}, found {}."

        for year, month, validity, expected_result in data:
            if validity:
                with self.assertRaises(AssertionError) as cm:
                    datetime._days_before_month(self._bc, year, month)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                result = datetime._days_before_month(self._bc, year, month)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, year, month, result))

    #@unittest.skip("Temporarily skipped")
    def test__ymd2ord(self):
        """
        Test that the _ymd2ord function returns the correct number of days
        since Badi year -1842 including the current day.
        """
        err_msg_0 = "Month must be in range of 0..19"
        err_msg_1 = "Day for month {} must be in range of 1..{}"
        data = (
            ((-1842, 1, 1), False, 1),
            ((-1841, 1, 1), False, 367),
            ((181, 1, 1), False, 738887),
            ((181, 20, 1), True, err_msg_0),
            ((181, 19, 20), True, err_msg_1.format(19, 19)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, validity, expected_result in data:
            if validity:
                with self.assertRaises(AssertionError) as cm:
                    datetime._ymd2ord(self._bc, *date)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                result = datetime._ymd2ord(self._bc, *date)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__ord2ymd(self):
        """
        Test that the _ord2ymd function returns the year, month, and day
        from the Badi year -1842.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test__check_date_fields(self):
        """
        Test that the _check_date_fields function correctly raises
        assertion exceptions.

        More complete test are in badidatetime/tests/test_badi_calendar.py.
        """
        msg0 = ("The number of Váḥids in a Kull-i-Shay’ should be >= 1 or "
                "<= 19, found {}")
        msg1 = ("The number of years in a Váḥid should be >= 1 or <= 19, "
                "found {}")
        msg2 = "Invalid month '{}', should be 0 - 19."
        msg3 = ("Invalid day '{}' for month '{}' and year '{}' "
                "should be from 1 to <= {{}}.")
        msg4 = "Invalid hour '{}' it must be 0 <= {} < 24"
        msg5 = "Invalid minute '{}' should be 0 <= {} < 60."
        ## msg6 = ("If there is a part day then there can be no hours, "
        ##         "minutes, or seconds.")
        ## msg7 = ("If there is a part hour then there can be no minutes or "
        ##         "seconds.")
        ## msg8 = "If there is a part minute then there can be no seconds."
        data = (
            # Invalid Váḥid
            ((1, 0, 1, 1, 1), msg0.format(0)),
            ((1, 20, 1, 1, 1), msg0.format(20)),
            # Invalid year
            ((1, 10, 0, 1, 1), msg1.format(0)),
            ((1, 10, 20, 1, 1), msg1.format(20)),
            # Invalid month
            ((1, 10, 10, -1, 1), msg2.format(-1)),
            ((1, 10, 10, 20, 1), msg2.format(20)),
            # Invalid Ayyám-i-Há day
            ((1, 10, 3, 0, 0), msg3.format(0, 0, 3)),
            ((1, 10, 3, 0, 6), msg3.format(6, 0, 3)),
            # Invalid normal day
            ((1, 10, 3, 2, 0), msg3.format(0, 2, 3)),
            ((1, 10, 3, 2, 20), msg3.format(20, 2, 3)),
            )

        for date, err_msg in data:
            kull_i_shay, vahid, year, month, day = date[:5]

            try:
                with self.assertRaises(AssertionError) as cm:
                    datetime._check_date_fields(self._bc, *date)
            except AssertionError as e:
                # Raise an error when an AssertionError is not raised.
                raise AssertionError(f"Váḥid {vahid}, year {year}, "
                                     f"month {month}, day {day}, {e}")
            else:
                cycle = 4 + self._bc._is_leap_year(date)
                num_days = cycle if month == 0 else 19
                message = str(cm.exception)
                self.assertEqual(err_msg.format(num_days), message)
