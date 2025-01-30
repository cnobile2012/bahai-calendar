# -*- coding: utf-8 -*-
#
# badidatetime/test/test_gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

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
    def test__GREGORIAN_LEAP_YEAR(self):
        """
        Test that the lambda _GREGORIAN_LEAP_YEAR function correctly
        determines the Gregorian leap year.
        """
        years = ((1844, True), (1951, False), (2064, True), (2100, False))
        msg = "Expected {} for year {}, found {}"

        for year, value in years:
            result = self._gc._GREGORIAN_LEAP_YEAR(year)
            self.assertEqual(value, result, msg.format(value, year, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_gregorian_date(self):
        """
        Test that the jd_from_gregorian_date method returns a
        Julian day from a Gregorian date.
        """
        data = (
            # -4712-Jan-01 12:00:00
            ((-4712, 1, 1.5), False, False, True, 0.0),
            #((-4712, 1, 1.5), True, False, True, 0.0),
            #((-4712, 1, 1.5), True, True, True, 0.0),
            # -4712-Jan-02 00:00:00
            ((-4712, 1, 2.0), False, False, True, 0.5),
            #((-4712, 1, 2.0), True, False, True, 0.5),
            #((-4712, 1, 2.0), True, True, True, 0.5),
            # Last day of December on leap years which gave me a head ache.
            ((4, 12, 31), False, False, True, 1722883.5),
            ((4, 12, 31), True, False, True, 1722883.5),
            ((4, 12, 31), True, True, True, 1722883.5),
            # 1st year divisable by 100
            ((100, 2, 28), False, False, True, 1757640.5),
            # Meeus AA ch 7 p61 ex7.b
            ((333, 1, 27, 12), False, False, True, 1842713.0),
            ((333, 1, 27, 12), True, False, True, 1842710.0),
            #((333, 1, 27, 12), True, True, True, 1842713.0),
            # 1844-Mar-21 00:00:00
            ((1844, 3, 21), False, False, True, 2394646.5),
            ((1844, 3, 21), True, False, True, 2394644.5),
            #((1844, 3, 21), True, True, True, 2394658.5),
            # Meeus AA ch 7 p61 ex7.a
            ((1957, 10, 4.81), False, False, True, 2436116.31),
            ((1957, 10, 4.81), True, False, True, 2436114.31),
            #((1957, 10, 4.81), True, True, True, 2436129.31),
            # 2451545.0 as per https://aa.usno.navy.mil/data/JulianDate
            ((2000, 1, 1.5), False, False, True, 2451545.0),
            ((2000, 1, 1.5), True, False, True, 2451543.0),
            #((2000, 1, 1.5), True, True, True, 2451558.0),
            # Tests for dates when the Julian Calendar was changed to
            # the Gregorian Calendar.
            ((1582, 10, 10), False, False, False, 0),
            ((1582, 10, 10), True, False, True, 2299153.5),
            #((1582, 10, 10), True, True, True, 2299165.5),
            # Test for Julian Period day jumps a day after December 31st.
            ((1700, 12, 31), True, False, True, 2342334.5),
            ((1701, 1, 1), True, False, True, 2342335.5),
            )
        msg = "Expected '{}' for g_date '{}', exact '{}', alt '{}', found '{}'"

        for g_date, exact, alt, validity, expected_result in data:
            if validity:
                result = self._gc.jd_from_gregorian_date(g_date, exact=exact,
                                                         alt=alt)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, g_date, exact,
                                            alt, result))
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
            (0.0, False, False, (-4712, 1, 1.5)),
            #(0.0, True, False, (-4712, 1, 1.5)),
            # -4712-Jan-02 00:00:00
            (0.5, False, False, (-4712, 1, 2.0)),
            #(0.5, True, False, (-4712, 1, 2.0)),
            (363.5, True, False, (-4712, 11, 24)),
            (364.5, True, False, (-4712, 11, 25)),
            (1172462.5, True, False, (-1503, 12, 30.0)),
            (1172463.5, True, False, (-1503, 12, 31)),
            (1312715.5, True, False, (-1119, 12, 30)),
            (1312716.5, True, False, (-1119, 12, 31)),
            (1314176.5, True, False, (-1115, 12, 30)),
            (1314177.5, True, False, (-1115, 12, 31)),
            (1721482.5, True, False, (1, 3, 1)),
            # Leap years with special correct
            (1867519.5, False, False, (400, 12, 28.0)),
            (1867519.5, True, False, (400, 12, 31.0)),
            (1867519.5, True, True, (400, 12, 31.0)),
            # Meeus AA ch 7 p64 ex7.d
            (2418781.5, False, False, (1910, 4, 20)),
            (2418781.5, True, False, (1910, 4, 22)),
            (2418781.5, True, True, (1910, 4, 21)),
            # Meeus AA ch 7 p64 ex7.c
            (2436116.31, False, False, (1957, 10, 4.81)),
            (2436116.31, True, False, (1957, 10, 6.81)),
            (2436116.31, True, True, (1957, 10, 6.81)),
            # Meeus AA ch 7 p64 ex7.d
            (2446470.5, False, False, (1986, 2, 9)),
            (2446470.5, True, False, (1986, 2, 11)),
            (2446470.5, True, True, (1986, 2, 11)),
            # 1844-Mar-21 00:00:00
            (2394646.5, False, False, (1844, 3, 21)),
            (2394646.5, True, False, (1844, 3, 23)),
            (2394646.5, True, True, (1844, 3, 23)),
            (2451544.5, False, False, (2000, 1, 1)),
            (2451544.5, True, False, (2000, 1, 3)),
            (2451544.5, True, True, (2000, 1, 3)),
            (2451545.0, False, False, (2000, 1, 1.5)),
            (2451545.0, True, False, (2000, 1, 3.5)),
            (2451545.0, True, True, (2000, 1, 3.5)),
            # Fractional days other than 0 or 0.5
            (2459188.6, False, False, (2020, 12, 5.1)),
            (2459188.6, True, False, (2020, 12, 7.1)),
            (2459188.75, False, False, (2020, 12, 5.25)),
            (2459188.75, True, False, (2020, 12, 7.25)),
            (2459188.99, False, False, (2020, 12, 5.49)),
            (2459188.99, True, False, (2020, 12, 7.49)),
            (2459189.31, False, False, (2020, 12, 5.81)),
            (2459189.31, True, False, (2020, 12, 7.81)),
            # Tests the 0.nnn day issue.
            (2460732.5, True, False, (2025, 2, 28)),
            (2460733.0, True, False, (2025, 2, 28.5)),
            (2460733.250321, True, False, (2025, 2, 28.750321)),
            (2440585.20942, True, False, (1969, 12, 31.70942)),
            )
        msg = "Expected '{}' for jd '{}', with exact '{}', alt '{}', found '{}'"

        for jd, exact, alt, expected_result in data:
            result = self._gc.gregorian_date_from_jd(jd, exact=exact, alt=alt)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, exact,
                                        alt, result))

    #@unittest.skip("Temporarily skipped")
    def test_posix_timestamp(self):
        """
        Test that the posix_timestamp method returns the year, month, day,
        hours, minutes, and seconds for a POSIX timestamp.

        *** TODO *** This method is giving wrong results.
        """
        data = (
            # POSIX epoch -> 1970-01-01T00:00:00
            (0, 0, False, (1970, 1, 1, 0, 0, 0)),
            (1, 0, False, (1970, 1, 1, 0, 0, 1)),
            (31536000, 0, False, (1971, 1, 1, 0, 0, 0)),
            (63072000, 0, False, (1972, 1, 1, 0, 0, 0)),
            # One second before epoch.
            #(-1, 0, False, (1969, 12, 31, 23, 59, 59)),
            # One year before epoch
            #(-31536000, 0, False, (1969, 1, 1, 0, 0, 0)),
            # One year and a day before epoch
            #(-31622400, 0, False, (1968, 12, 31, 0, 0, 0)),
            # Two years before epoch
            #(-63158400, 0, False, (1968, 1, 1, 0, 0, 0)),
            # Ten years before epoch
            #(-315619200, 0, False, (1960, 1, 1, 0, 0, 0)),
            # Tehran Iran Friday, August 23, 2024 (GMT+3:30)
            (1724362982.984497, 3.5, False, (2024, 8, 23, 1, 13, 2.984497)),
            # Tehran Iran Friday, August 23, 2024 3:35 (UTC+3:30)
            (1724371535.5798125, 3.5, False, (2024, 8, 23, 3, 35, 35.579813)),
            # Greenwich UK Friday, August 23, 2024 0:05 (UTC+0:00)
            (1724371535.5798125, 0, False, (2024, 8, 23, 0, 5, 35.579813)),
            # Raleigh NC Thursday, August 22, 2024 20:05 (UTC-4:00)
            (1724371535.5798125, -4, False, (2024, 8, 22, 20, 5, 35.579813)),
            # Parts of Australia Friday, August 23 2024 8:50 (UTC+8:45)
            (1724371535.5798125, 8.75, False, (2024, 8, 23, 8, 50, 35.579813)),
            # (2024, 11, 30, 20, 24, 13, 327577)
            (1733016253.327577, -5, False, (2024, 11, 30, 20, 24, 13.327577)),
            (1733016253.327577, 0, False, (2024, 12, 1, 1, 24, 13.327577)),
            (1733016253.327577, 0, True, (2024, 12, 1, 1, 24, 13, 327577)),
            (1730433600, 0, False, (2024, 11, 1, 4, 0, 0)),
            )
        msg = "Expected {} with t {}, and zone {}, found {}"

        for t, zone, us, expected_result in data:
            result = self._gc.posix_timestamp(t, zone=zone, us=us)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, t, zone, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_year_from_jd(self):
        """
        Test that the gregorian_year_from_jd method returns a
        Gregorian year from a Julian day.
        """
        data = (
            (2394646.5, 1844),
            (2451544.5, 2000), # Middle of Julian Period day 12 midnight
            (2451545.0, 2000), # Start of Julian Period day 12 noon
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
            ((2024, 2, 15, 12, 45, 15), (2024, 2, 15.531424)),
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
            ((2024, 2, 15.531424), False, (2024, 2, 15, 12, 45, 15.0336)),
            # Badi Calendar epoch
            ((1844, 3, 20.761111), False, (1844, 3, 20, 18, 15, 59.9904)),
            ((1844, 3, 20.761111), True, (1844, 3, 20, 18, 15, 59, 990400)),
            )
        msg = "Expected result {} for date {} and us {}, found {}."

        for date, us, expected_result in data:
            result = self._gc.ymdhms_from_date(date, us=us)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, us, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_valid_gregorian_month_day(self):
        """
        Check that the year, month, day, hour, minute, and second in a
        gregorian date are in bounds. Also check that if a decimal number
        is used there are no succeeding number at all.
        """
        msg1 = "Invalid month '{}', should be 1 - 12."
        msg2 = ("Invalid day '{}' for month '{}' and year '{}' "
                "should be 1 - {}.")
        msg3 = ("If there is a part day then there can be no hours, minutes, "
                "or seconds.")
        msg4 = ("If there is a part hour then there can be no minutes or "
                "seconds.")
        msg5 = "If there is a part minute then there can be no seconds."
        data = (
            ((1, 1, 1), True, ''),
            ((100, 2, 29), False, msg2.format(29, 2, 100, 28)),
            ((200, 2, 29), False, msg2.format(29, 2, 200, 28)),
            ((2024, 1, 1), True, ''),
            ((2024, 0, 1), False, msg1.format(0)),
            ((2024, 1, 1.5, 1, 0, 0), False, msg3),
            ((2024, 1, 1.5, 0, 1, 0), False, msg3),
            ((2024, 1, 1.5, 0, 0, 1), False, msg3),
            ((2024, 1, 1, 1.5, 1, 0), False, msg4),
            ((2024, 1, 1, 1.5, 0, 1), False, msg4),
            ((2024, 1, 1, 0, 1.5, 1), False, msg5),
            )

        for g_date, validity, err_msg in data:
            year = g_date[0]
            month = g_date[1]

            if validity:
                # Test correct dates
                for m in range(1, 13):
                    for days in range(self._gc._MONTHS[m - 1]):
                        if m == 2: # Subtract 0 or 1 from Febuary if leap year.
                            days -= (0 if self._gc._GREGORIAN_LEAP_YEAR(year)
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
