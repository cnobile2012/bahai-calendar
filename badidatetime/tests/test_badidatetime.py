# -*- coding: utf-8 -*-
#
# badidatetime/test/test_badidatetime.py
#
__docformat__ = "restructuredtext en"

import re
import os
import sys
import time
import unittest

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)
print(BASE_DIR)

from badidatetime import datetime
from ..badi_calendar import BahaiCalendar


class TestBadiDatetimeFunctions(unittest.TestCase):

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

    @unittest.skip("Temporarily skipped")
    def test__build_struct_time(self):
        """
        Test that the _build_struct_time function returns a struct of the
        date and time.
        """
        data = (
            ((1, 1, 1, 0, 0, 0, 0),
             time.struct_time((1, 1, 1, 0, 0, 0, 1, 1, 0))),
            ((181, 19, 19, 0, 0, 0, 0),
             time.struct_time((181, 19, 19, 0, 0, 0, 1, 1, 0))),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = datetime._build_struct_time(self._bc, *date)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__isoweek_to_badi(self):
        """
        Test that the _isoweek_to_badi function
        """
        err_msg_0 = "Invalid week: {}"
        err_msg_1 = "Invalid weekday: {} (range is 1..7)"
        data = (
            ((-1842, 1, 1), False, False, (-5, 18, 1, 1, 4, 23, 57, 45.3888)),
            ((-1842, 1, 1), True, False, (-1842, 1, 4, 23, 57, 45.3888)),
            ((181, 1, 1), True, False, (181, 1, 3, 23, 55, 9.696)),
            ((182, 1, 1), True, False, (182, 1, 2, 23, 56, 11.6448)),
            ((183, 1, 1), True, False, (183, 1, 1)),
            ((181, 1, 7), True, False, (181, 1, 9, 23, 50, 11.7024)),
            ((181, 20, 7), True, False, (181, 8, 9, 23, 15, 32.0544)),
            ((182, 53, 1), True, False, (183, 1, 1)),
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
    def test__day_of_week(self):
        """
        Test that the _day_of_week function returns the correct day of
        the week for a given year, month, and day.
        """
        data = (
            ((181, 9, 9), 4),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = datetime._day_of_week(self._bc, *date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_date_time(self):
        """
        Test trhat the _parse_isoformat_date_time function returns a
        parsed date and time ISO string.
        """
        data = (
            ('-18420101T120000', (-1842, 1, 1, 12, 0, 0.0)),
            ('-1842-01-01T12:00:00', (-1842, 1, 1, 12, 0, 0.0)),
            ('11610101T120000', (1161, 1, 1, 12, 0, 0.0)),
            ('1161-01-01T12:00:00', (1161, 1, 1, 12, 0, 0.0)),
            ('0181-W20T12:00:00', (181, 8, 3, 12, 0, 0.0)),
            ('0181-W20-5T12:00:00', (181, 8, 7, 12, 0, 0.0)),
            )
        msg = "Expected {} with dtstr {}, found {}."

        for dtstr, expected_result in data:
            result = datetime._parse_isoformat_date_time(self._bc, dtstr)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, dtstr, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_date(self):
        """
        Test that the _parse_isoformat_date function parses the date
        correctly from ISO standard formats.
        """
        err_msg_0 = "Year is out of range: {}, min {}, max {}."
        err_msg_1 = ("Invalid format, there must be between 0 to 2 hyphens "
                     "(-) in the date format or there can be one uppercase "
                     "(W) week identifier and between 0 and 2 hyphens (-) "
                     "used.")
        err_msg_2 = "invalid literal for int() with base 10: {}"
        err_msg_3 = "Invalid ISO string {}."
        data = (
            ('', False, ()),
            ('0181-01', False, (181, 1, 1)),
            ('01810101', False, (181, 1, 1)),
            ('0181-01-01', False, (181, 1, 1)),
            ('0181W01', False, (181, 1, 3)),
            ('0181-W01', False, (181, 1, 3)),
            ('0181W017', False, (181, 1, 9)),
            ('0181-W01-7', False, (181, 1, 9)),
            ('0181W207', False, (181, 8, 9)),
            ('0181001', False, (181, 1, 1)),
            ('0181019', False, (181, 1, 19)),
            ('0181324', False, (181, 18, 1)),
            ('0181342', False, (181, 18, 19)),
            ('0181343', False, (181, 0, 1)),
            ('0181346', False, (181, 0, 4)),
            ('0181347', False, (181, 19, 1)),
            ('0181365', False, (181, 19, 19)),
            ('0181-001', False, (181, 1, 1)),
            ('0181-019', False, (181, 1, 19)),
            ('0182-324', False, (182, 18, 1)),
            ('0182-342', False, (182, 18, 19)),
            ('0182-343', False, (182, 0, 1)),
            ('0182-347', False, (182, 0, 5)),
            ('0182-348', False, (182, 19, 1)),
            ('0182-366', False, (182, 19, 19)),
            ('-1843-01', True, err_msg_0.format(-1843, datetime.MINYEAR,
                                                datetime.MAXYEAR)),
            ('1162-01', True, err_msg_0.format(1162, datetime.MINYEAR,
                                               datetime.MAXYEAR)),
            ('0181-01-01-', True, err_msg_1),
            ('0181-W10-1-', True, err_msg_1),
            ('015s', True, err_msg_2.format("'015s'")),
            ('0181-W101', True, err_msg_3.format('0181-W101')),
            )
        msg = "Expected {} with ISO date {}, found {}."

        for date, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._parse_isoformat_date(self._bc, date)
                except AssertionError as e:
                    self.assertEqual(expected_result, str(e))
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                except IndexError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
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
            (' 14', False, (14, 0, 0)),
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
            ('T014.2', True, err_msg_3.format('T014.2')),
            )
        msg = "Expected {} with ISO time {}, found {}."

        for time, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._parse_isoformat_time(self._bc, time)
                except AssertionError as e:
                    self.assertEqual(expected_result, str(e))
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
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

    @unittest.skip("Temporarily skipped")
    def test__wrap_strftime(self):
        """
        Test that the _wrap_strftime function returns a formatted time
        string.
        """
        data = (
            (datetime.date(181, 1, 1), '%d/%m/%Y, %H:%M:%S', (), False, ''),
            )

        for date, fmt, tt, validate, err_msg in data:
            if validate:
                try:
                    with self.assertRaises(AssertionError) as cm:
                        datetime._wrap_strftime(date, fmt, tt)
                except AssertionError as e:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(f"Date {date}, format {fmt}, "
                                             f"timetuple {tt}, {e}")
                else:
                    message = str(cm.exception)
                    self.assertEqual(err_msg.format(num_days), message)
            else:
                result = datetime._wrap_strftime(date, fmt, tt)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, time, result))




class TestBadiDatetime_timedalta(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)




class TestBadiDatetime_date(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test_fromtimestamp(self):
        """
        Test that the fromtimestamp class method creates an instance of
        date from a POSIX timestamp.
        """
        data = (
            (0, True, 'badidatetime.datetime.date(126, 16, 1)'),
            (1723057467.0619307, False,
             'badidatetime.datetime.date(1, 10, 10, 8, 8)'),
            (1723057467.0619307, True,
             'badidatetime.datetime.date(181, 8, 8)'),
            )
        msg = "Expected {} with timestamp {}, found {}."

        for ts, short, expected_result in data:
            result = datetime.date.fromtimestamp(ts, short=short)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, ts, result))

    #@unittest.skip("Temporarily skipped")
    def test_today(self):
        """
        Test that the today class method creates an instance of date
        for today.
        """
        dt_reg = r'badidatetime\.datetime\.date\((?P<date>.+)\)'
        data = (
            (False, 5),
            (True, 3),
            )
        msg = "Expected {}, found {}."

        for short, num in data:
            result = datetime.date.today(short=short)
            date_str = re.search(dt_reg, str(result))

            if date_str:
                date = [int(num.strip())
                        for num in date_str.group('date').split(',')]
                self.assertEqual(len(date), num, msg.format(num, len(date)))
            else:
                self.assertIsNone(date_str, (
                    f"For short {short} and num {num}, could not get a "
                    "value from the regex."))

    #@unittest.skip("Temporarily skipped")
    def test_fromordinal(self):
        """
        Test that the fromordinal class method creates a date instance
        from a date ordinal number.
        """
        data = (
            (1, False, 'badidatetime.datetime.date(-5, 18, 1, 1, 1)'),
            (1, True, 'badidatetime.datetime.date(-1842, 1, 1)'),
            (367, True, 'badidatetime.datetime.date(-1841, 1, 1)'),
            (738887, True, 'badidatetime.datetime.date(181, 1, 1)'),
            )
        msg = "Expected {} with ordinal {}, found {}."

        for n, short, expected_result in data:
            result = datetime.date.fromordinal(n, short=short)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, n, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisoformat(self):
        """
        Test that the fromisoformat class method creates a date instance
        from an ISO formatted string.
        """
        err_msg_0 = "fromisoformat: argument must be a string."
        err_msg_1 = ("A time indicator was found, this is invalid for date "
                     "parsing, isoformat string: {}.")
        err_msg_2 = "Invalid isoformat string: {}."
        err_msg_3 = (f"Year is out of range: {{}}, min {datetime.MINYEAR}, "
                     f"max {datetime.MAXYEAR}.")
        data = (
            ('0181-01', False, False,
             'badidatetime.datetime.date(1, 10, 10, 1, 1)'),
            ('01810101', False, False,
             'badidatetime.datetime.date(1, 10, 10, 1, 1)'),
            ('0181-01-01', False, False,
             'badidatetime.datetime.date(1, 10, 10, 1, 1)'),
            ('0181-01-01', True, False,
             'badidatetime.datetime.date(181, 1, 1)'),
            # Test error messages.
            (10, False, True, err_msg_0),
            ('0181-01-01T00:00:00', False, True,
             err_msg_1.format("'0181-01-01T00:00:00'")),
            ('', False, True, err_msg_2.format("''")),
            # We only test one error that propigated up from
            # the _parse_isoformat_date function.
            ('-2000-01-01', False, True, err_msg_3.format(-2000)),
            )
        msg = "Expected {} with iso {} and short {}, found {}."

        for iso, short, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.date.fromisoformat(iso, short=short)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {iso} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.date.fromisoformat(iso, short=short)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, iso, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisocalendar(self):
        """
        Test that the fromisocalendar class method creates a date instance
        from an ISO calendar date.
        """
        err_msg = "Invalid weekday: {} (range is 1..7)"
        data = (
            ((181, 1, 1), False, False,
             'badidatetime.datetime.date(1, 10, 10, 1, 3)'),
            ((181, 1, 1), True, False,
             'badidatetime.datetime.date(181, 1, 3)'),
            ((181, 24, 7), True, False,
             'badidatetime.datetime.date(181, 9, 18)'),
            ((181, 1, 10), False, True, err_msg.format(10)),
            )
        msg = "Expected {} with iso {} and short {}, found {}."

        for date, short, validity, expected_result in data:
            if validity:
                with self.assertRaises(AssertionError) as cm:
                    datetime.date.fromisocalendar(*date, short=short)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                result = datetime.date.fromisocalendar(*date, short=short)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_ctime(self):
        """
        Test that the ctime method creates a string indicating the date.

        All days before 1752-09-14 in the Gregorian Calendar will be wrong
        when compaired to the Badi Calendar in UK and the US. This is when
        The Gregorian Calendar was adopted and compinsated 11 days.
        """
        data = (
            # 0001-03-20 Saturday (Fiḍāl -> Tuesday)
            ((-1842, 1, 1), 'Fiḍāl Bahá  1 00:00:00 -1842'),
            # 1582-10-15 Monday (Istijlāl -> Thursday)
            ((-261, 11, 18), 'Istijlāl Mashíyyat 18 00:00:00 -0261'),
            # 1582-10-04 Thursday (Jamál -> Sunday)
            ((-261, 11, 7), 'Jamál Mashíyyat  7 00:00:00 -0261'),
            # 1700-03-20 Wednesday (Jalál -> Saturday)
            ((-143, 1, 1), 'Jalál Bahá  1 00:00:00 -0143'),
            # 1752-09-02 Wednesday (Jalál -> Saturday)
            ((-91, 9, 15), "Jalál Asmá' 15 00:00:00 -0091"),
            # 1752-09-14 Thursday
            ((-91, 10, 8), "Istijlāl 'Izzat  8 00:00:00 -0091"),
            # 1800-03-21 Friday
            ((-43, 1, 1), 'Istiqlāl Bahá  1 00:00:00 -0043'),
            # 1825-03-21 Monday
            ((-18, 1, 1), 'Kamál Bahá  1 00:00:00 -0018'),
            # 1843-03-21 Tuesday
            ((0, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0000'),
            # 1844-03-20 Wednesday
            ((1, 1, 1), '`Idāl Bahá  1 00:00:00 0001'),
            # 1862-03-21 Friday
            ((19, 1, 1), 'Istiqlāl Bahá  1 00:00:00 0019'),
            # 1881-03-20 Sunday
            ((38, 1, 1), 'Jamál Bahá  1 00:00:00 0038'),
            # 1900-03-21 Wednesday
            ((57, 1, 1), '`Idāl Bahá  1 00:00:00 0057'),
            # 2014-03-21 Friday
            ((171, 1, 1), 'Istiqlāl Bahá  1 00:00:00 0171'),
            # 2024-03-20 Wednesday
            ((181, 1, 1), '`Idāl Bahá  1 00:00:00 0181'),
            # 2024-08-14 Wednesday
            ((181, 8, 15), '`Idāl Kamál 15 00:00:00 0181'),
            # 2024-08-14 Wednesday
            ((1, 10, 10, 8, 15), '`Idāl Kamál 15 00:00:00 0181'),
            # 2024-08-15 Thursday
            ((1, 10, 10, 8, 16) , 'Istijlāl Kamál 16 00:00:00 0181'),
            # 2033-03-20 Sunday
            ((190, 1, 1), 'Jamál Bahá  1 00:00:00 0190'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.ctime()
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))


    @unittest.skip("Temporarily skipped")
    def test_strftime(self):
        """
        Test that the strftime method returns a formatted date time string.
        """
        pass




    #@unittest.skip("Temporarily skipped")
    def test_toordinal(self):
        """
        Test that the toordinal method returns a proleptic Badi ordinal.
        """
        data = (
            ((-1842, 1, 1), 1),
            ((1, 1, 1), 673143),
            ((181, 1, 1), 738887),
            ((181, 8, 15), 739034),
            ((1, 10, 10, 8, 15), 739034),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.toordinal()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))





class TestBadiDatetime_tzinfo(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)




class TestBadiDatetime_IsoCalendarDate(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)




class TestBadiDatetime_time(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)




class TestBadiDatetime_datetime(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)




class TestBadiDatetime_timezone(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)
