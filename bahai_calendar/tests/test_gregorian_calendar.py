# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import os
import math
from datetime import datetime
import unittest

from ..gregorian_calendar import GregorianCalendar


class TestGregorianCalendar(unittest.TestCase):
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

    #@unittest.skip("Temporarily skipped")
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
    def test_parse_datetime(self):
        """
        Test that the parse_datetime method sets the date and time properly.

        We test using the Baha'i epoch on 1844-03-20T18:16:00
        """
        data = (
            (datetime(year=1844, month=3, day=20, hour=18, minute=16),
             (1844, 3, 20, 18, 16, 0)),
            )

        msg = "Expected result {}, found {}."

        for date, expected_result in data:
            self._gc.parse_datetime(date)
            result = self._gc.date_representation
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_gregorian_date(self):
        """
        Test that the jd_from_gregorian_date method returns a
        Julian day from a Gregorian date.
        """
        data = (
            # -4712-Jan-01 12:00:00
            ((-4712, 1, 1.5), True, 0.0),
            # -4712-Jan-02 00:00:00
            ((-4712, 1, 2.0), True, 0.5),
            # Meeus AA ch 7 p61 ex7.b
            ((333, 1, 27, 12), True, 1842713.0),
            # Meeus AA ch 7 p61 ex7.a
            ((1957, 10, 4.81), True, 2436116.31),
            # 1844-Mar-21 00:00:00
            ((1844, 3, 21), True, 2394646.5),
            # 2451545.0 as per https://aa.usno.navy.mil/data/JulianDate
            ((2000, 1, 1.5), True, 2451545.0),
            ((1582, 10, 10), False, 0)
            )
        msg = "Expected '{}' for g_date '{}', found '{}'"

        for g_date, validity, expected_result in data:
            if validity:
                result = self._gc.jd_from_gregorian_date(g_date)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, g_date, result))
            else:
                with self.assertRaises(AssertionError) as cm:
                    self._gc.jd_from_gregorian_date(g_date)

                message = str(cm.exception)
                year, month, day = self._gc.date_from_ymdhms(g_date)
                err_msg = ("The days 5-14 in 1582-10 are invalid, found "
                           f"day '{day}'.")
                self.assertEqual(err_msg, message)

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_date_from_jd(self):
        """
        Test that the gregorian_date_from_jd method returns a
        Gregorian date from a Julian day.
        """
        data = (
            # -4712-Jan-01 12:00:00
            (0.0, (-4712, 1, 1.5)),
            # -4712-Jan-02 00:00:00
            (0.5, (-4712, 1, 2.0)),
            # Meeus AA ch 7 p64 ex7.d
            (2418781.5, (1910, 4, 20)),
            # Meeus AA ch 7 p64 ex7.c
            (2436116.31, (1957, 10, 4.81)),
            # Meeus AA ch 7 p64 ex7.d
            (2446470.5, (1986, 2, 9)),
            # 1844-Mar-21 00:00:00
            (2394646.5, (1844, 3, 21)),
            (2451544.5, (2000, 1, 1)),
            (2451545.0, (2000, 1, 1.5)),
            )
        msg = "Expected '{}' for j_day '{}', found '{}'"

        for j_day, expected_result in data:
            result = self._gc.gregorian_date_from_jd(j_day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, j_day, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_year_from_jd(self):
        """
        Test that the gregorian_year_from_jd method returns a
        Gregorian year from a Julian day.
        """
        data = (
            (2394646.5, 1844),
            (2451544.5, 2000), # Start of day 12 noon
            (2451545.0, 2000), # Middle of day 12 midnight
            )
        msg = "Expected {} for Julian day {}, found {}"

        for j_day, expected_result in data:
            result = self._gc.gregorian_year_from_jd(j_day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, j_day, result))

    #@unittest.skip("Temporarily skipped")
    def test_date_from_ymdhms(self):
        """
        Test that the date_from_ymdhms method returns a
        (year, month, day.partial) from a
        (year, month, day, hour, minute, second).
        """
        data = (
            ((2024, 2, 15, 12, 45, 15), (2024, 2, 15.531424) ),
            # Badi Calendar epoch
            ((1844, 3, 20, 18, 16), (1844, 3, 20.761111)),
            )
        msg = "Expected result {} for ymdhms {}, found {}."

        for ymdhms, expected_result in data:
            result = self._gc.date_from_ymdhms(ymdhms)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, ymdhms, result))

    #@unittest.skip("Temporarily skipped")
    def test_ymdhms_from_date(self):
        """
        Test that the ymdhms_from_date method returns a
        (year, month, day, hour, minute, second) from a
        (year, month, day.partial).
        """
        data = (
            ((2024, 2, 15.531424), (2024, 2, 15, 12, 45, 15.0336)),
            # Badi Calendar epoch
            ((1844, 3, 20.761111), (1844, 3, 20, 18, 15, 59.9904)),
            )
        msg = "Expected result {} for date {}, found {}."

        for date, expected_result in data:
            result = self._gc.ymdhms_from_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_valid_gregorian_month_day(self):
        """
        Check that the year, month, day, hour, minute, and second in a
        gregorian date are in bounds. Also check that if a decimal number
        is used there are no succeeding number at all.
        """
        msg1 = ("If there is a part day then there can be no hours, minutes, "
                "or seconds.")
        msg2 = ("If there is a part hour then there can be no minutes or "
                "seconds.")
        msg3 = "If there is a part minute then there can be no seconds."
        data = (
            ((1, 1, 1), True, ''),
            ((2024, 1, 1), True, ''),
            ((2024, 1, 1.5, 1, 0, 0), False, msg1),
            ((2024, 1, 1.5, 0, 1, 0), False, msg1),
            ((2024, 1, 1.5, 0, 0, 1), False, msg1),
            ((2024, 1, 1, 1.5, 1, 0), False, msg2),
            ((2024, 1, 1, 1.5, 0, 1), False, msg2),
            ((2024, 1, 1, 0, 1.5, 1), False, msg3),
            )

        for g_date, validity, err_msg in data:
            year = g_date[0]
            month = g_date[1]
            day = g_date[2]
            #print(g_date)

            if validity:
                # Test correct dates
                for m in range(1, 13):
                    for days in range(self._gc._MONTHS[m - 1]):
                        if m == 2: # Subtract 0 or 1 from Febuary if leap year.
                            days -= (0 if self._gc.GREGORIAN_LEAP_YEAR(year)
                                     else 1)

                        for d in range(1, days + 1):
                            date = (year, m, d)
                            self._gc._check_valid_gregorian_month_day(date)
            else:
                with self.assertRaises(AssertionError) as cm:
                    self._gc._check_valid_gregorian_month_day(g_date)

                message = str(cm.exception)
                self.assertEqual(err_msg, message)

        # Test invalid month
        month = 14
        date = (year, month, 30)
        msg = f"Invalid month '{month}', should be 1 - 12."

        with self.assertRaises(AssertionError) as cm:
            self._gc._check_valid_gregorian_month_day(date)

        message = str(cm.exception)
        self.assertEqual(msg, message)
