# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import os
import math
import datetime
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

        We test using the Baha'i epoch on 1844-03-21T18:11:00
        """
        bahai_g_epech = datetime.datetime(year=1844, month=3, day=21,
                                          hour=18, minute=11, second=0)
        expected_result = ((1844, 3, 21), (18, 11, 0))
        # Test date
        self._gc.parse_datetime(bahai_g_epech)
        result = self._gc.date_representation
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result[0], result, msg)
        # Test time
        result = self._gc.time_representation
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result[1], result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_gregorian(self):
        """
        Test that the fixed_from_gregorian method returns the proper
        fixed date from a gregorian date.
        """
        date_reps = (
            ((1844, 3, 21), 673222),
            ((1970, 1, 1), 719163),
            ((622, 7, 19), 227015),
            # Vernal Equinox
            ((2000, 3, 20.31711805555555555556), 730199.3171180555),
            # Vernal Equinox
            ((2000, 3, 20, 7, 36, 19), 730199.316886574),
            )
        msg = "Expected result {} for year {}, found {}."

        for date_rep, expected_result in date_reps:
            result = self._gc.fixed_from_gregorian(date_rep)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date_rep[0], result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_new_year(self):
        """
        Test that the gregorian_new_year method returns the fixed date
        of January 1 in g_year.
        """
        g_year = 2024
        result = self._gc.gregorian_new_year(g_year)
        expected_result = 738886
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

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
    def test_gregorian_from_fixed(self):
        """
        Test that the gregorian_from_fixed method returns the Gregorian
        (year month day) corresponding to fixed date.
        """
        dates = (
            (719163, (1970, 1 ,1)),  #
            (673222, (1844, 3, 21)), # Leap Year
            (227015, (622, 7, 19)),
            )
        msg = "Expected result {}, found {}."

        for date, expected_result in dates:
            result = self._gc.gregorian_from_fixed(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_date_difference(self):
        """
        Test that the gregorian_date_difference method returns the
        number of days from Gregorian date g_date1 until g_date2.

        C(n, k) = n!/k!*(n−k)!
        """
        date_reps = (
            (1970, 1, 1),  #
            (1844, 3, 21), # Leap Year
            (622, 7, 19),
            )
        expected_result = (492148, 446207, -492148)
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

    #@unittest.skip("Temporarily skipped")
    def test_alt_fixed_from_gregorian(self):
        """
        Test that the alt_fixed_from_gregorian method returns the
        alternative calculation of fixed date equivalent to the Gregorian
        date g-date.
        """
        date_reps = (
            #((2094, 4, 4), 764652),
            ((1844, 3, 21), 673222),
            ((1970, 1, 1), 719163),
            ((622, 7, 19), 227015)
            )
        msg = "Expected result {} for year {}, found {}."

        for date_rep, expected_result in date_reps:
            result = self._gc.alt_fixed_from_gregorian(date_rep)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date_rep[0], result))

    #@unittest.skip("Temporarily skipped")
    def test_alt_gregorian_from_fixed(self):
        """
        Test that the gregorian_from_fixed method returns the Gregorian
        (year month day) corresponding to fixed date.
        """
        dates = (
            #(764652, (2094, 4, 4)),  # Easter
            (719163, (1970, 1 ,1)),  #
            (673222, (1844, 3, 21)), # Leap Year
            (227015, (622, 7, 19)),
            )
        msg = "Expected result {}, found {}."

        for date, expected_result in dates:
            result = self._gc.alt_gregorian_from_fixed(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_alt_gregorian_year_from_fixed(self):
        """
        Test that the gregorian_year_from_fixed method returns a
        Gregorian year corresponding to the fixed date
        """
        dates = ((673222, 1844), (719163, 1970), (227015, 622))
        msg = "Expected result {} for year {}, found {}."

        for date, expected_result in dates:
            result = self._gc.alt_gregorian_year_from_fixed(date)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_gregorian_date(self):
        """
        Test that the jd_from_gregorian_date method returns a
        Julian day from a Gregorian date.
        """
        data = (
            # -4712-Jan-01 12:00:00
            ((-4712, 1, 1.5), 0.0),
            # -4712-Jan-02 00:00:00
            ((-4712, 1, 2.0), 0.5),
            # Meeus AA ch 7 p61 ex7.b
            ((333, 1, 27, 12), 1842713.0),
            # Meeus AA ch 7 p61 ex7.a
            ((1957, 10, 4.81), 2436116.31),
            # 1844-Mar-21 00:00:00
            ((1844, 3, 21), 2394646.5),
            # 2451545.0 as per https://aa.usno.navy.mil/data/JulianDate
            ((2000, 1, 1.5), 2451545.0)
            )
        msg = "Expected {} for g_date {}, found {}"

        for g_date, expected_result in data:
            result = self._gc.jd_from_gregorian_date(g_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

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
            (2436116.31, (1957, 10, 4.810000000055879)),
            # Meeus AA ch 7 p64 ex7.d
            (2446470.5, (1986, 2, 9)),
            # 1844-Mar-21 00:00:00
            (2394646.5, (1844, 3, 21)),
            (2451544.5, (2000, 1, 1)),
            (2451545.0, (2000, 1, 1.5)),
            )
        msg = "Expected {} for j_day {}, found {}"

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
            ((2024, 2, 15, 12, 45, 15), (2024, 2, 15.53142361111111) ),
            # Badi Calendar epoch
            ((1844, 3, 20, 18, 11, 6), (1844, 3, 20.757708333333333)),
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
            ((2024, 2, 15.53142361111111),
             (2024, 2, 15, 12, 45, 14.999999999935199)),
            # Badi Calendar epoch
            ((1844, 3, 20.757708333333333),
             (1844, 3, 20, 18, 11, 6.0000000000081855)),
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
        data = (
            ((1, 1, 1), True),
            ((2024, 1, 1), True),
            ((2024, 1, 1.1, 1), False),
            ((2024, 1, 1.1, 0, 1), False),
            ((2024, 1, 1.1, 0, 0, 1), False),
            )

        for g_date, validity in data:
            year = g_date[0]
            month = g_date[1]
            day = g_date[2]

            if validity:
                msg = ("Invalid day '{}' for month '{}' and year '{}' "
                       "should be 1 - {}.")

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
                t_len = len(g_date)
                msg = "If there is a part day then there can be no {}."
                hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
                mint = g_date[4] if t_len > 4 and g_date[4] is not None else 0
                secs = g_date[5] if t_len > 5 and g_date[5] is not None else 0

                if secs > 0:
                    err_msg = msg.format('seconds')
                elif mint > 0:
                    err_msg = msg.format('minutes')
                elif hour > 0:
                    err_msg = msg.format('hours')

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
