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
        data = (
            (1, False, (-5, 18, 1, 1, 1)),
            (1, True, (-1842, 1, 1)),
            (367, True, (-1841, 1, 1)),
            (738887, True, (181, 1, 1)),
            )
        msg = "Expected {} with ordinal {} and short {}, found {}."

        for ordinal, short, expected_result in data:
            result = datetime._ord2ymd(self._bc, ordinal, short=short)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, ordinal, short, result))

    #@unittest.skip("Temporarily skipped")
    def test__isoweek_to_badi(self):
        """
        Test that the _isoweek_to_badi function
        """
        err_msg_0 = "Invalid week: {}"
        err_msg_1 = "Invalid weekday: {} (range is 1..7)"
        data = (
            ((-1842, 1, 1), False, False, (-5, 18, 1, 1, 3, 23, 56, 51.9072)),
            ((-1842, 1, 1), True, False, (-1842, 1, 3, 23, 56, 51.9072)),
            ((181, 1, 1), True, False, (181, 1, 2, 23, 57, 15.3216)),
            ((182, 1, 1), True, False, (182, 1, 1, 23, 58, 20.3808)),
            ((183, 1, 1), True, False, (182, 19, 18, 23, 59, 25.6128)),
            ((181, 1, 7), True, False, (181, 1, 8, 23, 52, 2.9856)),
            ((181, 20, 7), True, False, (181, 8, 8, 23, 12, 18.0)),
            ((182, 53, 1), True, False, (182, 19, 18, 23, 59, 25.6128)),
            ((181, 53, 1), False, True, err_msg_0.format(53)),
            ((181, 54, 1), False, True, err_msg_0.format(54)),
            ((181, 20, 10), False, True, err_msg_1.format(10)),
            )
        msg = "Expected {} with (year, week, day) {}, found {}."

        for item, short, validity, expected_result in data:
            if validity:
                with self.assertRaises(AssertionError) as cm:
                    datetime._isoweek_to_badi(self._bc, *item)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                result = datetime._isoweek_to_badi(
                    self._bc, *item, short=short)
                self.assertEqual(expected_result, result,
                             msg.format(expected_result, item, result))

    #@unittest.skip("Temporarily skipped")
    def test__isoweek1jalal(self):
        """
        Test that the _isoweek1jalal function returns the day number of
        the first week with more than 3 days in it.
        """
        data = (
            (1, 673145),   # 1844-03-22
            (181, 738889), # 2024-03-23
            (182, 739253), # 2025-03-22
            (183, 739617), # 2026-03-20
            )
        msg = "Expected {} with year {}, found {}."

        for year, expected_result in data:
            result = datetime._isoweek1jalal(self._bc, year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_date(self):
        """
        Test that the _parse_isoformat_date function parses the date
        correctly from ISO standard formats.
        """
        data = (
            ('0181-01', (181, 1, 1)),    # (181, 1, 2, 23, 57, 15.3216)
            ('01810101', (181, 1, 1)),   # (181, 1, 2, 23, 57, 15.3216)
            ('0181-01-01', (181, 1, 1)), # (181, 1, 2, 23, 57, 15.3216)
            ('0181W01', (181, 1, 2)),    # (181, 1, 3, 23, 56, 23.0496)
            ('0181-W01', (181, 1, 2)),   # (181, 1, 3, 23, 56, 23.0496)
            ('0181W017', (181, 1, 8)),   # (181, 1, 8, 23, 52, 2.9856)
            ('0181-W01-7', (181, 1, 8)), # (181, 1, 8, 23, 52, 2.9856)
            ('0181W207', (181, 8, 8)),   # (181, 8, 8, 23, 12, 18.0)
            ('0181001', (181, 1, 1)),
            ('0181019', (181, 1, 19)),
            ('0181324', (181, 18, 1)),
            ('0181342', (181, 18, 19)),
            ('0181343', (181, 0, 1)),
            ('0181346', (181, 0, 4)),
            ('0181347', (181, 19, 1)),
            ('0181365', (181, 19, 19)),
            ('0181-001', (181, 1, 1)),
            ('0181-019', (181, 1, 19)),
            ('0182-324', (182, 18, 1)),
            ('0182-342', (182, 18, 19)),
            ('0182-343', (182, 0, 1)),
            ('0182-347', (182, 0, 5)),
            ('0182-348', (182, 19, 1)),
            ('0182-366', (182, 19, 19)),
            )
        msg = "Expected {} with ISO date {}, found {}."

        for date, expected_result in data:
            result = datetime._parse_isoformat_date(self._bc, date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_time(self):
        """
        Test that the _parse_isoformat_time function parses the time
        correctly from ISO standard formats.
        """
        err_msg_0 = ("Cannot have both a 'T' and a space or more than one "
                     "of either to indicate time.")
        err_msg_1 = "Invalid number of colons (:), can be 0 - 2, found {}"
        err_msg_2 = "Invalid number of dots (.), can be 0 - 1, found {}"
        err_msg_3 = "Invalid time string, found {}"
        data = (
            ('', False, ()),
            ('T14', False, (14, 0, 0)),
            ('T14.2', False, (14, 12, 0)),
            ('T1412', False, (14, 12, 0)),
            ('T1412.45', False, (14, 12, 27.0)),
            ('T141232', False, (14, 12, 32)),
            ('T141232.029', False, (14, 12, 32.029)),
            ('T14:12', False, (14, 12, 0)),
            ('T14:12.45', False, (14, 12, 27.0)),
            ('T14:12:32', False, (14, 12, 32)),
            ('T14:12:32.029', False, (14, 12, 32.029)),
            ('T ', True, err_msg_0),
            ('TT', True, err_msg_0),
            ('  ', True, err_msg_0),
            (':::', True, err_msg_1.format(3)),
            ('..', True, err_msg_2.format(2)),
            #('T014.2', True, err_msg_3.format('T014.2')),
            )
        msg = "Expected {} with ISO time {}, found {}."

        for time, validity, expected_result in data:
            if validity:
                with (self.assertRaises(AssertionError) or
                      self.assertRaises(ValueError)) as cm:
                    datetime._parse_isoformat_time(self._bc, time)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                result = datetime._parse_isoformat_time(self._bc, time)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, time, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_date_fields(self):
        """
        Test that the _check_date_fields function correctly raises
        assertion exceptions.

        A more complete test is in badidatetime/tests/test_badi_calendar.py.
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
            # Test short form date.
            ((181, 20, 1), msg2.format(20)),
            )

        for date, err_msg in data:
            if len(date) == 5:
                kull_i_shay, vahid, year, month, day = date[:5]
            else:
                year, month, day = date[:3]

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
