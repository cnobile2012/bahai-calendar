# -*- coding: utf-8 -*-
#
# badidatetime/test/test_datetime.py
#
__docformat__ = "restructuredtext en"

import re
import os
import sys
import locale
import time
import pickle
import unittest

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import datetime
from ..badi_calendar import BahaiCalendar


class TestBadiDatetimeFunctions(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    @classmethod
    def setUpClass(cls):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    def setUp(self):
        self._bc = BahaiCalendar()

    #@unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns the the correct value for the
        caparison.
        """
        data = (
            (10, 10, 0),
            (10, 9, 1),
            (10, 11, -1),
            )
        msg = "Expected {} with x {} and y {}, found {}."

        for x, y, expected_result in data:
            result = datetime._cmp(x, y)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, x, y, result))

    #@unittest.skip("Temporarily skipped")
    def test__divide_and_round(self):
        """
        Test that the _divide_and_round function returns the correct result.
        """
        data = (
            ((7, 3), 2),
            ((99, 4), 25),
            ((99, 3), 33),
            )
        msg = "Expected {} with values {}, found {}."

        for values, expected_result in data:
            result = datetime._divide_and_round(*values)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, values, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_offset(self):
        """
        Test that the _check_utc_offset function returns the correct result.
        """
        err_msg0 = ("Invalid name argument '{}' must be one of "
                    "('utcoffset', 'badioffset', 'dst').")
        err_msg1 = "tzinfo.{}() must return None or timedelta, not {}"
        err_msg2 = ("{}()={}, must be strictly between -timedelta(hours=24) "
                    "and timedelta(hours=24)")
        data = (
            ('utcoffset', datetime.timedelta(hours=10), False, None),
            ('dst', datetime.timedelta(hours=1), False, None),
            ('utcoffset', None, False, None),
            ('junk', None, True, err_msg0.format('junk')),
            ('utcoffset', 10, True, err_msg1.format('utcoffset', type(10))),
            ('utcoffset', datetime.timedelta(hours=24), True,
             err_msg2.format('utcoffset', '1 day, 0:00:00')),
            )
        msg = "Expected {} with name {} and offset {}, found {}."

        for name, offset, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._check_offset(name, offset)
                except (AssertionError, TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(
                        f"With {name} and {offset} an error is "
                        f"not raised, with result {result}.")
            else:
                result = datetime._check_offset(name, offset)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, name, offset, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_tzinfo_arg(self):
        """
        Test that the _check_tzinfo_arg function returns the correct result.
        """
        err_msg0 = ("tzinfo argument must be None or of a tzinfo subclass, "
                    "found {}")
        data = (
            (datetime.BADI, False, None),
            ('JUNK', True, err_msg0.format("'JUNK'")),
            )
        msg = "Expected {} with tz {}, found {}."

        for tz, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._check_tzinfo_arg(tz)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(
                        f"With {name} and {offset} an error is "
                        f"not raised, with result {result}.")
            else:
                result = datetime._check_tzinfo_arg(tz)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, tz, result))

    #@unittest.skip("Temporarily skipped")
    def test__cmperror(self):
        """
        Test that the _cmperror
        """
        err_msg0 = "Cannot compare {} to {}"
        data = (
            ((181, 1, 1), (181, 1, 1),
             err_msg0.format("'date'", "'datetime'")),
            )
        msg = "date0 {} and date1 {}."

        for date0, date1, expected_result in data:
            d = datetime.date(*date0)
            dt = datetime.datetime(*date1)

            try:
                datetime._cmperror(d, dt)
            except TypeError as e:
                self.assertEqual(expected_result, str(e),
                                 msg.format(date0, date1))

    #@unittest.skip("Temporarily skipped")
    def test__format_time(self):
        """
        Test that the _format_time function returns the correct result.
        """
        specs = ('auto', 'hours', 'minutes', 'seconds', 'milliseconds',
                 'microseconds')
        err_msg0 = f"Invalid timespec '{{}}', must be one of {specs}."
        data = (
            ((12, 30, 30, 0), 'auto', False, '12:30:30'),
            ((12, 30, 30, 1000), 'auto', False, '12:30:30.001000'),
            ((12, 30, 30, 1000), 'milliseconds', False, '12:30:30.001'),
            ((12, 30, 30, 0), 'hours', False, '12'),
            ((12, 30, 30, 0), 'minutes', False, '12:30'),
            ((12, 30, 30, 0), 'seconds', False, '12:30:30'),
            ((12, 30, 30, 500000), 'microseconds', False, '12:30:30.500000'),
            ((12, 30, 30, 0), 'junk', True, err_msg0.format('junk')),
            )
        msg = "Expected {} with hhmmssus {} and ts {}, found {}."

        for hhmmssus, ts, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._format_time(*hhmmssus, timespec=ts)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(
                        f"With {hhmmssus} and {ts} an error is "
                        f"not raised, with result {result}.")
            else:
                result = datetime._format_time(*hhmmssus, timespec=ts)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, hhmmssus, ts, result))

    #@unittest.skip("Temporarily skipped")
    def test__format_offset(self):
        """
        Test that the _format_offset returns the correct result.
        """
        err_msg0 = "The off value '{}', must be a timedelta object or None."
        data = (
            (None, False, ''),
            (datetime.timedelta(hours=2), False, '+02:00'),
            (datetime.timedelta(hours=2, minutes=5), False, '+02:05'),
            (datetime.timedelta(hours=2, minutes=5, seconds=30), False,
             '+02:05:30'),
            (datetime.timedelta(
                hours=2, minutes=5, seconds=30, microseconds=5000), False,
             '+02:05:30.005000'),
            (datetime.timedelta(days=-1, hours=2), False, '-22:00'),
            (100, True, err_msg0.format(100)),
            )
        msg = "Expected {} with off {}, found {}."

        for off, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._format_offset(off)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {off} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime._format_offset(off)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, off, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_tzname(self):
        """
        Test that the _check_tzname function raises a TypeError is the name
        argument is not None or a string.
        """
        err_msg0 = "tzinfo.tzname() must return None or string, not {}"
        data = (
            ('', False, ''),
            (None, False, ''),
            (100, True, err_msg0.format(int)),
            )
        msg = "Expected {} with name {}, found {}."

        for name, validity, expected_result in data:
            if validity:
                try:
                    datetime._check_tzname(name)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    raise AssertionError(
                        f"With {name} an error is not raised.")
            else:
                datetime._check_tzname(name)

    @unittest.skip("Temporarily skipped")
    def test__local_tz_utc_offset_seconds(self):
        """
        Test that the _local_tz_utc_offset_seconds function returns the
        timezone offset in seconds.
        """


    @unittest.skip("Temporarily skipped")
    def test__module_name(self):
        """
        Test that the _module_name function returns the module path without
        the base directory.
        """


class TestBadiDatetime_timedelta(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method instantiates a timedelta object.
        """
        err_msg99 = "timedelta # of days is too large: {}"
        data = (
            ((1,), False, '1 day, 0:00:00'),
            ((1, 2), False, '1 day, 0:00:02'),
            ((1, 2, 3), False, '1 day, 0:00:02.000003'),
            ((1, 2, 3, 4), False, '1 day, 0:00:02.004003'),
            ((1, 2, 3, 4, 5), False, '1 day, 0:05:02.004003'),
            ((1, 2, 3, 4, 5, 6), False, '1 day, 6:05:02.004003'),
            ((1, 2, 3, 4, 5, 6, 7), False, '50 days, 6:05:02.004003'),
            ((1.5,), False, '1 day, 12:00:00'),
            ((1.5, 2.5), False, '1 day, 12:00:02.500000'),
            ((1.5, 2.5, 3.5), False, '1 day, 12:00:02.500004'),
            ((1000000000,), True, err_msg99.format(1000000000)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.timedelta(*date)
                except (AssertionError, OverflowError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {date} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.timedelta(*date)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ method returns the correctly formatted string.
        """
        data = (
            ((1, 1, 1), 'datetime.timedelta('
             'days=1, seconds=1, microseconds=1)'),
            ((0, 0, 0), 'datetime.timedelta(0)'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.timedelta(*date)
            result = repr(d)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___str__(self):
        """
        Test that the __str__ method returns the correctly formatted string.
        """
        data = (
            ((1, 1, 1), '1 day, 0:00:01.000001'),
            ((0, 0, 0), '0:00:00'),
            ((2, 10, 15, 20, 5, 1), '2 days, 1:05:10.020015'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = datetime.timedelta(*date)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_total_seconds(self):
        """
        Test that the total_seconds functions returns the provided date
        data in total seconds.
        """
        data = (
            ((1, 1, 1), 86401.000001),
            ((0, 0, 0), 0),
            ((10, 10, 10), 864010.00001),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td = datetime.timedelta(*date)
            result = td.total_seconds()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_days(self):
        """
        Test that the days property returns the correct number of days.
        """
        data = (
            ((1, 1, 1), 1),
            ((0, 0, 0), 0),
            ((10, 9, 8), 10),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td = datetime.timedelta(*date)
            result = td.days
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_seconds(self):
        """
        Test that the seconds property returns the correct number of seconds.
        """
        data = (
            ((1, 1, 1), 1),
            ((0, 0, 0), 0),
            ((10, 9, 8), 9),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td = datetime.timedelta(*date)
            result = td.seconds
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_microseconds(self):
        """
        Test that the microseconds property returns the correct number of
        microseconds.
        """
        data = (
            ((1, 1, 1), 1),
            ((0, 0, 0), 0),
            ((10, 9, 8), 8),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td = datetime.timedelta(*date)
            result = td.microseconds
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test___add__(self):
        """
        Test that the __add__ method returns an added timedelta object
        with the days, seconds, and microseconds.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), False, (2, 2, 2)),
            ((0, 0, 0), (1, 1, 1), False, (1, 1, 1)),
            ((10, 9, 8), (11, 1, 1), False, (21, 10, 9)),
            ((1, 1, 1), (1, 1, 1), True, (1, 1, 2)), # NotImplemented
            )
        msg = "Expected {} with date0 {}, and date1 {}, found {}."

        for date0, date1, validity, expected_result in data:
            td0 = datetime.timedelta(*date0)

            if validity:
                d0 = datetime.date(*date1)
                d1 = td0 + d0
                result = (d1._year, d1._month, d1._day)
            else:
                td1 = datetime.timedelta(*date1)
                td = td0 + td1
                result = (td.days, td.seconds, td.microseconds)

            self.assertEqual(expected_result, result, msg.format(
                expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___sub__(self):
        """
        Test that the __sub__ method returns a subtracted timedelta object
        with the days, seconds, and microseconds.
        """
        err_msg0 = "unsupported operand type(s) for -: 'timedelta' and '{}'"
        data = (
            ((1, 1, 1), (1, 1, 1), False, (0, 0, 0)),
            ((0, 0, 0), (1, 1, 1), False, (-2, 86398, 999999)),
            ((10, 9, 8), (11, 1, 1), False, (-1, 8, 7)),
            # NotImplemented
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            )
        msg = "Expected {} with date0 {}, and date1 {}, found {}."

        for date0, date1, validity, expected_result in data:
            td0 = datetime.timedelta(*date0)

            if validity:
                try:
                    result = td0 - date1
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date0}' an error is not "
                                         f"raised, with result {result}.")
            else:
                td1 = datetime.timedelta(*date1)
                td = td0 - td1
                result = (td.days, td.seconds, td.microseconds)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, result))

    @unittest.skip("Temporarily skipped")
    def test___rsub__(self):
        """
        Test that the __rsub__ method returns the inverse subtracted
        timedelta object with the days, seconds, and microseconds.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), (0, 0, 0)),
            ((0, 0, 0), (0, 0, 0), (-2, 86398, 999999)),
            ((10, 9, 8), (1, 1, 1), (-1, 8, 7)),
            )
        msg = "Expected {} with date {}, and dividend {}, found {}."

        for date, dividend, expected_result in data:
            td0 = datetime.timedelta(*date)
            td1 = td0 - dividend
            result = (td1.days, td1.seconds, td1.microseconds)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, dividend, result))

    #@unittest.skip("Temporarily skipped")
    def test___neg__(self):
        """
        Test that the __neg__ returns a negative version of the
        tiemdelta object.
        """
        data = (
            ((1, 1, 1), (-2, 86398, 999999)),
            ((0, 0, 0), (0, 0, 0)),
            ((10, 9, 8), (-11, 86390, 999992)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td0 = datetime.timedelta(*date)
            td1 = -td0
            result = (td1.days, td1.seconds, td1.microseconds)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___pos__(self):
        """
        Test that the __pos__ returns a positive version of the
        tiemdelta object.
        """
        data = (
            ((1, 1, 1), (1, 1, 1)),
            ((0, 0, 0), (0, 0, 0)),
            ((10, 9, 8), (10, 9, 8)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td0 = datetime.timedelta(*date)
            td1 = +td0
            result = (td1.days, td1.seconds, td1.microseconds)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___abs__(self):
        """
        Test that the __abs__ returns the absolute value version of the
        tiemdelta object.
        """
        data = (
            ((1, 1, 1), (1, 1, 1)),
            ((0, 0, 0), (0, 0, 0)),
            ((-1 , -1, -1), (1, 1, 1)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td0 = datetime.timedelta(*date)
            td1 = abs(td0)
            result = (td1.days, td1.seconds, td1.microseconds)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___mul__(self):
        """
        Test that the __mul__ method returns the product of the
        datetime.timedelta object and an interger or float.
        """
        err_msg0 = ("unsupported operand type(s) for *: 'timedelta' "
                    "and '{}'")
        data = (
            ((10, 50, 5000), 2, False, (20, 100, 10000)),
            ((10, 50, 5000), 2.5, False, (25, 125, 12500)),
            ((10, 50, 5000), None, True, err_msg0.format('NoneType')),
            )
        msg = "Expected {} with date {} and multiplyer {}, found {}."

        for date, multiplyer, validity, expected_result in data:
            td0 = datetime.timedelta(*date)

            if validity:
                try:
                    result = td0 * multiplyer
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                td1 = td0 * multiplyer
                result = (td1.days, td1.seconds, td1.microseconds)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, multiplyer, result))

    #@unittest.skip("Temporarily skipped")
    def test__to_microseconds(self):
        """
        Test that the _to_microseconds method returns the total
        microseconds of the datetime.timedelta object.
        """
        data = (
            ((1, 1, 1), 86401000001),
            ((0, 0, 0), 0),
            ((10, 9, 8), 864009000008),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            td = datetime.timedelta(*date)
            result = td._to_microseconds()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___floordiv__(self):
        """
        Test that the __floordiv__ returns the floor after divided using
        the // operator.
        """
        err_msg0 = "unsupported operand type(s) for //: 'timedelta' and '{}'"
        data = (
            ((1, 1, 1), (1, 1, 1), False, 1),
            ((10, 9, 8), (5, 4, 4), False, 2),
            ((1, 1, 1), 2, False, (0, 43200, 500000)),
            ((2, 2, 2), 5, False, (0, 34560, 400000)),
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            )
        msg = "Expected {} with date {} and value {}, found {}."

        for date, value, validity, expected_result in data:
            td0 = datetime.timedelta(*date)

            if validity:
                try:
                    result = td0 // value
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                if isinstance(value, tuple):
                    diviser = datetime.timedelta(*value)
                else:
                    diviser = value

                result = td0 // diviser

                if isinstance(result, datetime.timedelta):
                    result = (result.days, result.seconds, result.microseconds)

                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, value, result))

    #@unittest.skip("Temporarily skipped")
    def test___truediv__(self):
        """
        Test that the __truediv__ method returns the result of an
        datetime.timedelta divided by a datetime.timedelta, interger,
        or float.
        """
        err_msg0 = "unsupported operand type(s) for /: 'timedelta' and '{}'"
        data = (
            ((1, 1, 1), (1, 1, 1), False, 1),
            ((10, 9, 8), (5, 4, 4), False, 2.0000023147933814),
            ((1, 1, 1), 2, False, (0, 43200, 500000)),
            ((2, 2, 2), 5, False, (0, 34560, 400000)),
            ((1, 1, 1), 2.5, False, (0, 34560, 400000)),
            ((10, 10, 10), 4.5, False, (2, 19202, 222224)),
            ((1, 1, 1), None, True, err_msg0.format('NoneType'))
            )
        msg = "Expected {} with date {} and value {}, found {}."

        for date, value, validity, expected_result in data:
            td0 = datetime.timedelta(*date)

            if validity:
                try:
                    result = td0 / value
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                if isinstance(value, tuple):
                    diviser = datetime.timedelta(*value)
                else:
                    diviser = value

                result = td0 / diviser

                if isinstance(result, datetime.timedelta):
                    result = (result.days, result.seconds, result.microseconds)

                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, value, result))

    #@unittest.skip("Temporarily skipped")
    def test___mod__(self):
        """
        Test that the __mod__ method returns the results of the mod (%)
        operator.
        """
        err_msg0 = "unsupported operand type(s) for %: 'timedelta' and '{}'"
        data = (
            ((1, 1, 1), (1, 1, 1), False, (0, 0, 0)),
            ((10, 9, 8), (2, 4, 4), False, (1, 86392, 999992)),
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, validity, expected_result in data:
            td0 = datetime.timedelta(*date0)

            if validity:
                try:
                    result = td0 % date1
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date0}' an error is not "
                                         f"raised, with result {result}.")
            else:
                td1 = datetime.timedelta(*date1)
                td = td0 % td1
                result = (td.days, td.seconds, td.microseconds)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___divmod__(self):
        """
        Test that the __divmod__ method returns the results of the divmod
        buildin function.
        """
        err_msg0 = ("unsupported operand type(s) for divmod(): 'timedelta' "
                    "and '{}'")
        data = (
            ((1, 1, 1), (1, 1, 1), False, (1, (0, 0, 0))),
            ((10, 9, 8), (2, 4, 4), False, (4, (1, 86392, 999992))),
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, validity, expected_result in data:
            td0 = datetime.timedelta(*date0)

            if validity:
                try:
                    result = divmod(td0, date1)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                td1 = datetime.timedelta(*date1)
                q, r = divmod(td0, td1)
                result = (q, (r.days, r.seconds, r.microseconds))
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns  True if equal and False if
        not equal.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), True),
            ((1, 1, 1), (1, 1, 0), False),
            ((1, 1, 1), (1, 1, 2), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.timedelta(*date0)
            d1 = datetime.timedelta(*date1)
            result = d0 == d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___le__(self):
        """
        Test that the __le__ method returns  True if less than or equal and
        False if not less than or equal.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), True),
            ((1, 1, 1), (1, 1, 0), False),
            ((1, 1, 1), (1, 1, 2), True),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.timedelta(*date0)
            d1 = datetime.timedelta(*date1)
            result = d0 <= d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___lt__(self):
        """
        Test that the __lt__ method returns True if less than and False
        if not less than.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), False),
            ((1, 1, 1), (1, 1, 0), False),
            ((1, 1, 1), (1, 1, 2), True),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.timedelta(*date0)
            d1 = datetime.timedelta(*date1)
            result = d0 < d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___ge__(self):
        """
        Test that the __ge__ method returns True if greater than or equal
        and False if not greater than or equal.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), True),
            ((1, 1, 1), (1, 1, 0), True),
            ((1, 1, 1), (1, 1, 2), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.timedelta(*date0)
            d1 = datetime.timedelta(*date1)
            result = d0 >= d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___gt__(self):
        """
        Test that the __gt__ method returns True if greater than and False
        if not greater than.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), False),
            ((1, 1, 1), (1, 1, 0), True),
            ((1, 1, 1), (1, 1, 2), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.timedelta(*date0)
            d1 = datetime.timedelta(*date1)
            result = d0 > d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns 1 if the two dates are equal, +1
        if the current date is greater than the test date, and -1 if the
        inverse.
        """
        data = (
            ((1, 1, 1), (1, 1, 1), 0),
            ((1, 2, 1), (1, 1, 1), 1),
            ((1, 1, 1), (1, 2, 1), -1),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            td0 = datetime.timedelta(*date0)
            td1 = datetime.timedelta(*date1)
            result = td0._cmp(td1)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns a valid hash.
        """
        data = (
            (1, 1, 1),
            (10, 10, 10),
            )
        msg = "date {}, found {}."

        for date in data:
            d = datetime.timedelta(*date)
            result = hash(d)
            self.assertTrue(len(str(result)) > 15, msg.format(date, result))

    #@unittest.skip("Temporarily skipped")
    def test___bool__(self):
        """
        Test that the __bool__ method returns the correct boolean for the
        given datetime.timedelta object.
        """
        data = (
            ((0, 0, 0), False),
            ((1, 1, 1), True),
            )
        msg = "date {}, found {}."

        for date, expected_result in data:
            d = datetime.timedelta(*date)
            result = bool(d)
            self.assertEqual(expected_result, result, msg.format(date, result))

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the state of the class.
        """
        data = (
            ((1, 1, 1), (1, 1, 1)),
            ((-1, 1, 1), (-1, 1, 1)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.timedelta(*date)
            result = d._getstate()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce__(self):
        """
        Test that the __reduce__ method sets the year properly.
        """
        data = (
            (1, 1, 1),
            (-1, 1, 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date in data:
            td0 = datetime.timedelta(*date)
            obj = pickle.dumps(td0)
            td1 = pickle.loads(obj)
            td0_result = (td0._days, td0._seconds, td0._microseconds)
            td1_result = (td1._days, td1._seconds, td1._microseconds)
            self.assertEqual(td0_result, td1_result, msg.format(
                td0_result, date, td1_result))


class TestBadiDatetime_date(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    @classmethod
    def setUpClass(cls):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        MIN = datetime.date.KULL_I_SHAY_MIN
        MAX = datetime.date.KULL_I_SHAY_MAX
        err_msg0 = ("Invalid kull-i-shay {}, it must be in the range "
                    "of [-5, 4].")
        err_msg1 = "Invalid string {} had length of {} for pickle."
        err_msg2 = ("A full short or long form Badi date must be used, found "
                    "{} fields.")
        err_msg3 = ("Failed to encode latin1 string when unpickling a date or "
                    "datetime object. pickle.load(data, encoding='latin1') is "
                    "assumed.")
        data = (
            ((1, 1, 1), False, '0001-01-01'),
            ((1, 1, 1, 1, 1), False, '01-01-01-01-01'),
            ((b'\x073\x01\x01',), False, '0001-01-01'),
            ((b'\x14\x01\x01\x01\x01',), False, '01-01-01-01-01'),
            ((b'\x073\x01\x01\x01',), True, err_msg0.format(-12)),
            ((b'\x14\x01\x01\x01\x01\x01',), True, err_msg1.format(
                b'\x14\x01\x01\x01\x01\x01', 6)),
            ((100,), True, err_msg2.format(1)),
            (('\u2190\x01\x01\x01',), True, err_msg3),
            )
        msg = "Expected {} with value {}, found {}."

        for value, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.date(*value)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.date(*value)
                self.assertEqual(expected_result, str(result),
                                 msg.format(expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test_is_short(self):
        """
        Test that the is_short property properly indicates if the Badi
        date is in the short or long form.
        """
        data = (
            ((181, 9, 16), True),
            ((1, 10, 10, 9, 16), False),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.is_short
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromtimestamp(self):
        """
        Test that the fromtimestamp class method creates an instance of
        date from a POSIX timestamp.
        """
        data = (
            (0, True, '0126-16-02'),
            (1723057467.0619307, False, '01-10-10-08-09'),
            (1723057467.0619307, True, '0181-08-09'),
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
            (79, False, '-05-18-01-01-01'),
            (79, True, '-1842-01-01'),
            (445, True, '-1841-01-01'),
            (738965, True, '0181-01-01'),
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
            ('0181-01', False, False, '01-10-10-01-01'),
            ('01810101', False, False, '01-10-10-01-01'),
            ('0181-01-01', False, False, '01-10-10-01-01'),
            ('0181-01-01', True, False, '0181-01-01'),
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
                except (TypeError, ValueError) as e:
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
            # year, week, day in week
            ((181,   1,    1), False, False, '01-10-10-01-04'),
            ((181,   1,    1), True,  False, '0181-01-04'),
            ((181,  24,    7), True,  False, '0181-09-19'),
            ((181,   1,   10), False, True, err_msg.format(10)),
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
    def test___repr__(self):
        """
        Test that the __repr__ returns the expected formatted text.
        """
        data = (
            ((181, 9, 16), 'datetime.date(181, 9, 16)'),
            ((1, 10, 10, 9, 16), 'datetime.date(1, 10, 10, 9, 16)'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = repr(d)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__short_from_long_form(self):
        """
        Test that the _short_from_long_form method returns the short form
        Badi date.
        """
        data = (
            ((1, 1, 1, 1, 1), (1, 1, 1, 0, 0, 0, 0)),
            ((1, 10, 10, 10, 12), (181, 10, 12, 0, 0, 0, 0)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d._short_from_long_form()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_ctime(self):
        """
        Test that the ctime method creates a string indicating the date.

        All days before 1752-09-14 in the Gregorian Calendar will be wrong
        when compaired to the Badi Calendar in UK and the US. This is when
        The Gregorian Calendar was adopted and compinsated 11 days.
        """
        data = (
            # 0001-03-20 Sunday (Fiḍāl -> Tuesday)
            ((-1842, 1, 1), 'Fiḍāl Bahá  1 00:00:00 -1842'),
            # 1582-10-04 Thursday (Jamál -> Sunday)
            ((-261, 11, 7), 'Jamál Mashíyyat  7 00:00:00 -0261'),
            # 1582-10-15 Monday (Istijlāl -> Thursday)
            ((-261, 11, 18), 'Istijlāl Mashíyyat 18 00:00:00 -0261'),
            # 1700-03-20 Wednesday (Jalál -> Saturday)
            ((-143, 1, 1), 'Jalál Bahá  1 00:00:00 -0143'),
            # 1752-09-02 Wednesday (Jalál -> Saturday)
            ((-91, 9, 15), "Jalál Asmá' 15 00:00:00 -0091"),
            # 1752-09-14 Thursday
            ((-91, 10, 8), "Istijlāl 'Izzat  8 00:00:00 -0091"),
            # 1800-03-21 Friday
            ((-43, 1, 1), 'Istiqlāl Bahá  1 00:00:00 -0043'),
            # 1817-11-12 Wednesday Birthday of Bahá’u’lláh
            ((-26, 13, 9), '`Idāl Qudrat  9 00:00:00 -0026'),
            # 1825-03-21 Monday
            ((-18, 1, 1), 'Kamál Bahá  1 00:00:00 -0018'),
            # 1843-03-21 Tuesday
            ((0, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0000'),
            # 1844-03-20 Thursday
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

    #@unittest.skip("Temporarily skipped")
    def test_strftime(self):
        """
        Test that the strftime method returns a formatted date time string.
        """
        data = (
            ((181, 19, 1), '%D', '19/01/81'),
            ((1, 10, 10, 19, 1), '%D', '19/01/81'),
            ((1, 1, 1), '%x', '01/01/0001'),
            ((181, 11, 17), '%c', 'Jal Mas  17 00:00:00 0181'),
            )
        msg = "Expected {} with date {} and format {}, found {}."

        for date, fmt, expected_result in data:
            d = datetime.date(*date)
            result = d.strftime(fmt)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test___format__(self):
        """
        Test that the __format__ method returns a correctly formatted string.
        """
        data = (
            ((181, 11, 17), '', '0181-11-17'),
            ((181, 11, 17), '%D', '11/17/81'),
            )
        msg = "Expected {} with date {} and format {}, found {}."

        for date, fmt, expected_result in data:
            d = datetime.date(*date)
            result = f"{d:{fmt}}"
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoformat(self):
        """
        Test that the isoformat method return the ISO formated version of
        the date represented by this class.
        """
        data = (
            ((181, 1, 1), '0181-01-01'),
            ((1, 10, 10, 8, 15), '0181-08-15'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.isoformat()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___str__(self):
        """
        Test that the __str__ method returns the ISO formated version of
        the date represented by the class object.
        """
        data = (
            ((181, 1, 1), '0181-01-01'),
            ((1, 10, 10, 8, 15), '01-10-10-08-15'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = datetime.date(*date)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_kull_i_shay(self):
        """
        Test that the kull_i_shay property returns the kull_i_shay value.
        """
        data = (
            ((1, 10, 10, 1, 1), 1),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.kull_i_shay
            self.assertEqual(expected_result, d.kull_i_shay,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_vahid(self):
        """
        Test that the vahid property returns the vahid value.
        """
        data = (
            ((1, 10, 10, 1, 1), 10),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.vahid
            self.assertEqual(expected_result, d.vahid,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_year(self):
        """
        Test that the year property returns the year value.
        """
        data = (
            ((181, 1, 1), 181),
            ((1, 10, 10, 1, 1), 10),
             )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.year
            self.assertEqual(expected_result, d.year,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_month(self):
        """
        Test that the month property returns the month value.
        """
        data = (
            ((181, 1, 1), 1),
            ((1, 10, 10, 1, 1), 1),
             )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.month
            self.assertEqual(expected_result, d.month,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_day(self):
        """
        Test that the day property returns the day value.
        """
        data = (
            ((181, 1, 1), 1),
            ((1, 10, 10, 1, 1), 1),
             )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.day
            self.assertEqual(expected_result, d.day,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_timetuple(self):
        """
        Test that the timetuple returns the correct long or short form object.
        """
        data = (
            ((181, 9, 6),
             "structures.ShortFormStruct(tm_year=181, tm_mon=9, tm_mday=6, "
             "tm_hour=0, tm_min=0, tm_sec=0, tm_wday=0, tm_yday=158, "
             "tm_isdst=-1)"),
            ((1, 10, 10, 9, 6),
             "structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, "
             "tm_year=10, tm_mon=9, tm_mday=6, tm_hour=0, tm_min=0, tm_sec=0, "
             "tm_wday=0, tm_yday=158, tm_isdst=-1)")
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.timetuple()
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_toordinal(self):
        """
        Test that the toordinal method returns a proleptic Badi ordinal.
        """
        data = (
            ((-1842, 1, 1), 79),
            ((1, 1, 1), 673221),
            ((181, 1, 1), 738965),
            ((181, 8, 15), 739112),
            ((1, 10, 10, 8, 15), 739112),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.toordinal()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_replace(self):
        """
        Test that the replace method returns a new date object with the
        replaced values.
        """
        def execute_replace(date2):
            if short:
                year, month, day = date2
                result = d.replace(year=year, month=month, day=day)
            else:
                kull_i_shay, vahid, year, month, day = date2
                result = d.replace(kull_i_shay=kull_i_shay, vahid=vahid,
                                   year=year, month=month, day=day)

            return result

        err_msg0 = "Cannot convert from a short to a long form date."
        err_msg1 = ("Cannot convert from a long to a short form date. The "
                    "value {} is not valid for long form dates.")
        data = (
            # Normal replace for a short date
            ((181, 1, 1), (182, None, None), True, False, '0182-01-01'),
            ((181, 1, 1), (None, 9, 12), True, False, '0181-09-12'),
            # Normal replace for a long date
            ((1, 10, 10, 1, 1), (None, None, 11, None, None), False, False,
             '01-10-11-01-01'),
            ((1, 10, 10, 1, 1), (None, 9, None, None, None), False, False,
             '01-09-10-01-01'),
            ((1, 10, 10, 1, 1), (None, 9, 10, None, None), False, False,
             '01-09-10-01-01'),
            # Error conditions.
            ((181, 1, 1), (1, 10, None, None, None), False, True, err_msg0),
            ((1, 10, 10, 1, 1), (181, 1, None), True, True,
             err_msg1.format(181)),
            )
        msg = "Expected {} with date1 {}, date2 {}, and short {}, found {}."

        for date1, date2, short, validity, expected_result in data:
            d = datetime.date(*date1)

            if validity:
                try:
                    result = execute_replace(date2)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date1}' an error is not "
                                         f"raised, with result {result}.")
            else:
                result = execute_replace(date2)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date1, date2, short, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns True if equal and False if
        not equal.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), True),
            ((181, 9, 14), (181, 9, 13), False),
            ((181, 9, 14), (181, 9, 15), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 == d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___le__(self):
        """
        Test that the __le__ method returns True if less than or equal and
        False if not less than or equal.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), True),
            ((181, 9, 14), (181, 9, 13), False),
            ((181, 9, 14), (181, 9, 15), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), True),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 <= d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___lt__(self):
        """
        Test that the __lt__ method returns True if less than and False
        if not less than.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), False),
            ((181, 9, 14), (181, 9, 13), False),
            ((181, 9, 14), (181, 9, 15), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), True),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 < d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___ge__(self):
        """
        Test that the __ge__ method returns True if greater than or equal
        and False if not greater than or equal.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), True),
            ((181, 9, 14), (181, 9, 13), True),
            ((181, 9, 14), (181, 9, 15), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 >= d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___gt__(self):
        """
        Test that the __gt__ method returns True if greater than and False
        if not greater than.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), False),
            ((181, 9, 14), (181, 9, 13), True),
            ((181, 9, 14), (181, 9, 15), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 > d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns 1 if the two dates are equal, +1
        if the current date is greater than the test date, and -1 if the
        inverse.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), 0),
            ((181, 9, 14), (181, 9, 13), 1),
            ((181, 9, 14), (181, 9, 15), -1),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), 0),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), 1),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), -1),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0._cmp(d1)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns a valid hash for both short
        and long form dates.
        """
        data = (
            (datetime.MINYEAR, 1, 1),
            (-5, 18, 1, 1, 1),
            (1, 1, 1),
            (1, 1, 1, 1, 1),
            )
        msg = "date {}, found {}."

        for date in data:
            d = datetime.date(*date)
            result = hash(d)
            self.assertTrue(len(str(result)) > 15, msg.format(date, result))

    #@unittest.skip("Temporarily skipped")
    def test___add__(self):
        """
        Test that the __add__ method can correctly add a date to a timedelta.
        """
        err_msg0 = "unsupported operand type(s) for +: 'date' and '{}'"
        err_msg1 = "Result out of range."
        data = (
            ((1, 1, 1), (1,), False, (1, 1, 2)),
            ((1, 1, 1), (366,), False, (2, 1, 1)),     # Leap year
            ((181, 1, 1), (365,), False, (182, 1, 1)), # Non leap year
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            ((-1842, 1, 1), (-1,), True, err_msg1),
            ((1161, 19, 19), (1,), True, err_msg1),
            )
        msg = "Expected {} with date {} and timedelta {}, found {}"

        for date, td, validity, expected_result in data:
            d0 = datetime.date(*date)

            if validity:
                if isinstance(td, tuple):
                    td0 = datetime.timedelta(*td)
                else:
                    td0 = td

                try:
                    result = d0 + td0
                except (OverflowError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                td0 = datetime.timedelta(*td)
                d1 = d0 + td0
                result = (d1._year, d1._month, d1._day)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, td, result))

    #@unittest.skip("Temporarily skipped")
    def test___sub__(self):
        """
        Test that the __sub__ method returns the correct results of a
        timedelta object subtracted from a date object.
        """
        err_msg0 = "unsupported operand type(s) for -: 'date' and '{}'"
        data = (
            ((1, 1, 1), 1, False, (0, 19, 19)),
            ((2, 1, 1), 366, False, (1, 1, 1)),     # Leap year
            ((181, 1, 1), 365, False, (180, 1, 1)), # Non Leap year
            ((182, 1, 1), (181, 1, 1), False, (365, 0, 0)),
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            )
        msg = "Expected {} with date {} and value {}, found {}"

        for date, value, validity, expected_result in data:
            d0 = datetime.date(*date)

            if validity:
                try:
                    result = d0 - value
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                if isinstance(value, int):
                    dt = datetime.timedelta(value)
                    d1 = d0 - dt
                    result = (d1._year, d1._month, d1._day)
                else:
                    d1 = datetime.date(*value)
                    dt = d0 - d1
                    result = (dt._days, dt._seconds, dt._microseconds)

                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, value, result))

    #@unittest.skip("Temporarily skipped")
    def test_weekday(self):
        """
        Test that the weekday method returns the correct weekday number.
        """
        data = (
            ((181, 1, 1), 4),
            ((181, 10, 8), 0),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.weekday()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoweekday(self):
        """
        Test that the weekday method returns the correct weekday number.
        """
        data = (
            ((181, 1, 1), 5),
            ((181, 10, 8), 1),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.isoweekday()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_isocalendar(self):
        """
        Test that the isocalendar method the correct ISO Calendar tuple.
        """
        data = (
            ((181, 1, 1), (180, 0, 5)),       # Short form
            ((1, 10, 10, 1, 1), (180, 0, 5)), # Long form
            ((181, 0, 1), (181, 49, 4)),      # 0 < week < 53
            ((181, 19, 19), (181, 52, 5)),    # 0 < week < 53
            ((182, 1, 1), (181, 0, 6)),    # Week < 0 starts in previous year
            ((183, 19, 19), (184, 1, 1)),  # Week >= 52 starts in previous year
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = tuple(d.isocalendar())
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__is_pickle_data(self):
        """
        Test that the _is_pickle_data classmethod returns the correct results
        depending on the incoming data.
        """
        err_msg0 = "Invalid string {} had length of {} for pickle."
        err_msg1 = ("Failed to encode latin1 string when unpickling a date or "
                    "datetime object. pickle.load(data, encoding='latin1') is "
                    "assumed.")
        data = (
            ((b'\x073\x01\x01', None), False, True),
            ((b'\x14\x01\x01\x01\x01', None), False, False),
            ((181, 10), False, None),
            ((b'\x073\x20\x01', None), False, None),
            ((b'\x14\x01\x01\x14\x01', None), False, None),
            ((b'\x14\x01\x01\x01\x01\x01', None), True, err_msg0.format(
                b'\x14\x01\x01\x01\x01\x01', 6)),
            (('\u2190\x01\x01\x01', None), True, err_msg1),
            )
        msg = "Expected {} with value {}, found {}."

        for value, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.date._is_pickle_data(*value)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.date._is_pickle_data(*value)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the state of the class.
        """
        data = (
            ((datetime.MINYEAR, 1, 1), (b'\x00\x00\x01\x01',)),
            ((-5, 18, 1, 1, 1), (b'\x0e\x12\x01\x01\x01',)),
            ((1, 1, 1), (b'\x073\x01\x01',)),
            ((1, 1, 1, 1, 1), (b'\x14\x01\x01\x01\x01',)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d._getstate()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___setstate(self):
        """
        Test that the __setstate method sets the year properly.
        """
        data = (
            ((datetime.MINYEAR, 1, 1), b'\x00\x00\x01\x01'),
            ((-5, 18, 1, 1, 1), b'\x0e\x12\x01\x01\x01'),
            ((1, 1, 1), b'\x073\x01\x01'),
            ((1, 1, 1, 1, 1), b'\x14\x01\x01\x01\x01'),
            )
        msg = "Expected {} with bytes_str {}, found {}."

        for date, bytes_str in data:
            d = datetime.date(*date)
            d._date__setstate(bytes_str)

            if len(date) == 3:
                result = (d._year, d._month, d._day)
            else:
                result = (d._kull_i_shay, d._vahid, d._year, d._month, d._day)

            self.assertEqual(date, result, msg.format(date, bytes_str, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce__(self):
        """
        Test that the __reduce__ method works for both short and long
        form Badi dates.
        """
        data = (
            (datetime.MINYEAR, 1, 1),
            (1, 1, 1),
            (1, 1, 1, 1, 1),
            (datetime.MAXYEAR, 1, 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date in data:
            date0 = datetime.date(*date)
            obj = pickle.dumps(date0)
            date1 = pickle.loads(obj)

            if len(date) == 3:
                b_date0 = (date0._year, date0._month, date0._day)
                b_date1 = (date1._year, date1._month, date1._day)
            else:
                b_date0 = (date0._kull_i_shay, date0._vahid,
                           date0._year, date0._month, date0._day)
                b_date1 = (date1._kull_i_shay, date1._vahid,
                           date1._year, date1._month, date1._day)

            self.assertEqual(b_date0, b_date1, msg.format(
                b_date0, date, b_date1))


class TestBadiDatetime_tzinfo(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test_frombadi(self):
        """
        Test that the frombadi method converts BADI time to local time.
        """
        err_msg0 = "frombadi() requires a datetime argument, found '{}'."
        err_msg1 = "dt.tzinfo is not self"
        err_msg2 = "frombadi() requires a non-None utcoffset() result"
        err_msg3 = "frombadi() requires a non-None dst() result"
        err_msg4 = ("frombadi(): dt.dst gave inconsistent results; "
                    "cannot convert")
        data = (
            #((181, 1, 1), datetime.BADI, False, ''),

            (None, None, True, err_msg0.format("<class 'NoneType'>")),
            ((181, 1, 1), datetime.BADI, True, err_msg1),
            #(),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, tz, validity, expected_result in data:
            tzinfo = datetime.tzinfo()

            if date is None:
                dt = date
            else:
                dt = datetime.datetime(*date)

            if validity:
                try:
                    result = tzinfo.frombadi(dt)
                except (TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = tzinfo.frombadi(dt)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, value, result))


class TestBadiDatetime__IsoCalendarDate(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test_year(self):
        """
        Test that the year property returns the year.
        """
        data = (
            # year, week, weekday
            ((1,    1,    1), 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = d.year
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_week(self):
        """
        Test that the week property returns the week.
        """
        data = (
            # year, week, weekday
            ((1,    1,    1), 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = d.week
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_weekday(self):
        """
        Test that the weekday property returns the weekday.
        """
        data = (
            # year, week, weekday
            ((1,    1,    1), 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = d.weekday
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce__(self):
        """
        Test that the __reduce__ method works for both short and long
        form Badi dates.
        """
        data = (
            (datetime.MINYEAR, 1, 1),
            (1, 1, 1),
            (datetime.MAXYEAR, 1, 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date in data:
            date0 = datetime._IsoCalendarDate(*date)
            obj = pickle.dumps(date0)
            date1 = pickle.loads(obj)
            b_date0 = (date0.year, date0.week, date0.weekday)
            b_date1 = (date1[0], date1[1], date1[2])
            self.assertEqual(b_date0, b_date1, msg.format(
                b_date0, date, b_date1))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ returns the expected formatted text.
        """
        data = (
            ((181, 9, 16), '_IsoCalendarDate(year=181, week=9, weekday=16)'),
            ((1, 1, 1), '_IsoCalendarDate(year=1, week=1, weekday=1)'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = repr(d)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))


class TestBadiDatetime_time(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    @classmethod
    def setUpClass(cls):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        data = (
            ((12, 30), None, 0, False, '12:30:00'),
            ((12, 30, 30), None, 0, False, '12:30:30'),
            ((12, 30, 30, 10), None, 0, False, '12:30:30.000010'),
            ((12, 30, 30, 50000), datetime.BADI, 0, False,
             '12:30:30.050000+03:30'),
            ((12, 30, 30, 50000), None, 1, False, '12:30:30.050000'),
            # Test errors when picking
            )
        msg = "Expected {} with value {}, found {}."

        for value, tz, fold, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.time(*value, tzinfo=tz, fold=fold)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.time(*value, tzinfo=tz, fold=fold)
                self.assertEqual(expected_result, str(result),
                                 msg.format(expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test_hour(self):
        """
        Test that the hour property returns the correct value.
        """
        data = (
            ((12, 30), 12),
            ((24, 0), 24),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.hour
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_minute(self):
        """
        Test that the minute returns the correct value.
        """
        data = (
            ((12, 30), 30),
            ((24, 10), 10),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.minute
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_second(self):
        """
        Test that the second returns the correct value.
        """
        data = (
            ((12, 30, 30), 30),
            ((24, 10, 20), 20),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.second
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_microsecond(self):
        """
        Test that the microsecond returns the correct value.
        """
        data = (
            ((12, 30, 30, 1), 1),
            ((24, 10, 20, 999999), 999999),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.microsecond
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_tzinfo(self):
        """
        Test that the tzinfo returns the correct value.
        """
        data = (
            ((12, 30, 30), datetime.BADI, 'UTC+03:30'),
            ((24, 10, 20), datetime.UTC, 'UTC'),
            )
        msg = "Expected {} with time {}, found {}."

        for time, tz, expected_result in data:
            td = datetime.time(*time, tzinfo=tz)
            result = td.tzinfo
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_fold(self):
        """
        Test that the fold returns the correct value.
        """
        data = (
            ((12, 30, 30, 1), 0, 0),
            ((24, 10, 20, 999999), 1, 1),
            )
        msg = "Expected {} with time {}, found {}."

        for time, fold, expected_result in data:
            td = datetime.time(*time, fold=fold)
            result = td.fold
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns  True if equal and False if
        not equal.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), True),
            ((12, 30, 30), (12, 30, 29), False),
            ((12, 30, 30), (12, 30, 31), False),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 == t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___le__(self):
        """
        Test that the __le__ method returns  True if less than or equal and
        False if not less than or equal.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), True),
            ((12, 30, 30), (12, 30, 29), False),
            ((12, 30, 30), (12, 30, 31), True),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 <= t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___lt__(self):
        """
        Test that the __lt__ method returns True if less than and False
        if not less than.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), False),
            ((12, 30, 30), (12, 30, 29), False),
            ((12, 30, 30), (12, 30, 31), True),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 < t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___ge__(self):
        """
        Test that the __ge__ method returns True if greater than or equal
        and False if not greater than or equal.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), True),
            ((12, 30, 30), (12, 30, 29), True),
            ((12, 30, 30), (12, 30, 31), False),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 >= t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___gt__(self):
        """
        Test that the __gt__ method returns True if greater than and False
        if not greater than.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), False),
            ((12, 30, 30), (12, 30, 29), True),
            ((12, 30, 30), (12, 30, 31), False),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 > t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns 1 if the two times are equal, +1
        if the current time is greater than the test time, and -1 if the
        inverse.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), 0),
            ((12, 30, 30), (12, 30, 29), 1),
            ((12, 30, 30), (12, 30, 31), -1),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0._cmp(t1)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns the proper hash of the class.
        """
        data = (
            ((1, 30, 30), None, 0),
            ((1, 30, 30), None, 1),
            ((1, 30, 30, 500000), None, 0),
            ((1, 30, 30), datetime.BADI, 0),
            # *** TODO *** Test utcoffset not zero or None
            )
        msg = "time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = str(hash(t))
            self.assertTrue(len(result) > 15, msg.format(
                time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test__tzstr(self):
        """
        Test that the _tzstr method returns a formatted timezone offset.
        """
        data = (
            ((1, 30, 30, 500000), datetime.BADI, 0, '+03:30'),
            # *** TODO *** No timezone yet
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t._tzstr()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ method returns the correct string.
        """
        data = (
            ((0, 0, 0, 0), None, 0, 'datetime.time(0, 0)'),
            ((1, 30), None, 0, 'datetime.time(1, 30)'),
            ((1, 30, 30), None, 0, 'datetime.time(1, 30, 30)'),
            ((1, 30, 30, 50000), None, 0, 'datetime.time(1, 30, 30, 50000)'),
            ((1, 30, 30, 50000), datetime.BADI, 0,
             'datetime.time(1, 30, 30, 50000, tzinfo=UTC+03:30)'),
            ((1, 30, 30, 50000), datetime.BADI, 1,
             'datetime.time(1, 30, 30, 50000, tzinfo=UTC+03:30, fold=1)'),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            d = datetime.time(*time, tzinfo=tz, fold=fold)
            result = repr(d)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoformat(self):
        """
        Test that the isoformat method an ISO formatted string.
        """
        data = (
            ((1, 30, 30), None, 0, '01:30:30'),
            ((1, 30, 30, 500000), None, 0, '01:30:30.500000'),
            # *** TODO *** Needs to be tested with a timezone.
            #((1, 30, 30, 500000), datetime.BADI, 0, '01:30:30.500000'),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.isoformat()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisoformat(self):
        """
        Test that the fromisoformat classmethod returns a correctly
        formatted ISO time string.
        """
        err_msg0 = ("Invalid isoformat string: {}, Invalid character {} in "
                    "incoming time string.")
        err_msg1 = ("Invalid isoformat string: {}, Cannot have both a 'T' "
                    "and a space or more than one of either to indicate time.")
        err_msg2 = ("Invalid isoformat string: {}, Invalid time string, 1st "
                    "character must be one of ( T), found {}")
        err_msg3 = ("Invalid isoformat string: {}, Invalid number of colons "
                    "(:), can be 0 - 2, found {}")
        err_msg4 = ("Invalid isoformat string: {}, Invalid number of dots "
                    "(.), can be 0 - 1, found {}")
        err_msg5 = "Invalid isoformat string: {}, Invalid time string, found {}"
        err_msg6 = "fromisoformat: argument must be str"
        data = (
            ('T12', False, '12:00:00'),
            ('T12.5', False, '12:30:00'),
            ('T12:30', False, '12:30:00'),
            ('T12:30.5', False, '12:30:30'),
            ('T1230', False, '12:30:00'),
            ('T1230.5', False, '12:30:30'),
            (' 12:30', False, '12:30:00'),
            (' 12:30.5', False, '12:30:30'),
            (' 1230', False, '12:30:00'),
            (' 1230.5', False, '12:30:30'),
            ('T12:30:30', False, '12:30:30'),
            ('T12:30:30.5', False, '12:30:30.5'),
            ('T123030', False, '12:30:30'),
            ('T123030.5', False, '12:30:30.5'),
            # Error conditions
            ('abcdefg', True, err_msg0.format("'abcdefg'", "'a'")),
            (' T', True, err_msg1.format("' T'")),
            ('1230.5', True, err_msg2.format("'1230.5'", "'1230.5'")),
            ('T:::', True, err_msg3.format("'T:::'", 3)),
            ('T..', True, err_msg4.format("'T..'", 2)),
            ('T014.2', True, err_msg5.format("'T014.2'", "'T014.2'")),
            (10, True, err_msg6),
            )
        msg = "Expected {} with format {}, found {}."

        for iso, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.time.fromisoformat(iso)
                except (ValueError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {iso} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.time.fromisoformat(iso)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, iso, result))

    #@unittest.skip("Temporarily skipped")
    def test_strftime(self):
        """
        Test that the strftime method returns a correctly formatting string.
        """
        data = (
            ((1, 30, 30), '%X', '01:30:30'),
            ((1, 30, 30), '%r', '01:30:30 AM'),
            ((1, 30, 30), '%c', 'Jal Bah  1 01:30:30 0001'),
            ((1, 30, 30, 500000), '%T.%f', '01:30:30.500000'),
            ((1, 30, 30, 500000), 'T%H:%M:%S.%f','T01:30:30.500000'),
            )
        msg = "Expected {} with time {} and format {}, found {}."

        for time, fmt, expected_result in data:
            result = datetime.time(*time).strftime(fmt)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test___format__(self):
        """
        Test that the __format__ method returns a correctly formatting string.
        """
        err_msg0 = "Must be a str, not {}"
        data = (
            ((1, 30, 30), '', False, '01:30:30'),
            ((1, 30, 30), '%X', False, '01:30:30'),
            ((1, 30, 30, 500000), '%T.%f', False, '01:30:30.500000'),
            ((1, 30, 30, 500000), 'T%H:%M:%S.%f', False, 'T01:30:30.500000'),
            ((1, 30, 30, 500000), 10, True, err_msg0.format('int')),
            )
        msg = "Expected {} with time {} and format {}, found {}."

        for time, fmt, validity, expected_result in data:
            t = datetime.time(*time)

            if validity:
                try:
                    result = t.__format__(fmt)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = t.__format__(fmt)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, time, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test_utcoffset(self):
        """
        Test that the utcoffset method returns the correct timezone offset.
        """
        data = (
            ((1, 30, 30, 500000), None, 0, None),
            #((1, 30, 30, 500000), datetime.BADI, 0, ''),
            # *** TODO *** Needs to be tested with a timezone.
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.utcoffset()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_tzname(self):
        """
        Test that the tzname method returns the timezone name.
        """
        data = (
            ((12, 30, 30), None, 0, None),
            # *** TODO *** Needs to be tested with a timezone.
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.tzname()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_dst(self):
        """
        Test that the dst method returns either 0 or 1 if DST is in effect.
        """
        data = (
            ((12, 30, 30), None, 0, None),
            # *** TODO *** Needs to be tested with a timezone.
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.dst()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_replace(self):
        """
        Test that the replace
        """
        data = (
            ((12, 30, 30, 500000), None, 0,
             (6, None), None, None, '06:30:30.500000'),
            ((12, 30, 30, 500000), None, 0,
             (None, 15), None, None, '12:15:30.500000'),
            ((12, 30, 30, 500000), None, 0,
             (None, None, 15), None, None, '12:30:15.500000'),
            ((12, 30, 30, 500000), None, 0,
             (None, None, None, 999999), None, None, '12:30:30.999999'),
            ((12, 30, 30, 500000), None, 0,
             (), datetime.BADI, None, '12:30:30.500000+03:30'),
            ((12, 30, 30, 500000), None, 0, (), None, 1, '12:30:30.500000'),
            )
        msg = "Expected {} with time0 {}, and time1 {}, found {} "

        for time0, tz0, fold0, time1, tz1, fold1, expected_result in data:
            t = datetime.time(*time0, tzinfo=tz0, fold=fold0)
            result = t.replace(*time1, tzinfo=tz1, fold=fold1)
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, time0, time1, str(result)))

            if tz0 != tz1:
                self.assertEqual(tz1, result.tzinfo, msg.format(
                    expected_result, time0, time1, result.tzinfo))

            if fold1 is not None and fold0 != fold1:
                self.assertEqual(fold1, result.fold, msg.format(
                    expected_result, time0, time1, result.fold))

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the state of the class.
        """
        data = (
            ((12, 30, 30, 500000), None, 0, r"(b'\x0c\x1e\x1e\x07\xa1 ',)"),
            ((24, 30, 30, 500000), None, 0, r"(b'\x18\x1e\x1e\x07\xa1 ',)"),
            ((12, 30, 30, 500000), datetime.BADI, 0,
             r"(b'\x0c\x1e\x1e\x07\xa1 ', datetime.timezone.badi)"),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t._getstate()
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test___setstate(self):
        """
        Test that the __setstate method sets the correct state for pickeling.
        """
        err_msg0 = ("tzinfo argument must be None or of a tzinfo subclass, "
                    "found {}")
        data = (
            ((12, 30, 30, 500000), None, 0, b'\x0c\x1e\x1e\x07\xa1 ',
             False, ''),
            ((24, 30, 30, 500000), None, 0, b'\x18\x1e\x1e\x07\xa1 ',
             False, ''),
            ((0, 30, 30, 500000), None, 1, b'\x80\x1e\x1e\x07\xa1 ',
             False, ''),
            ((12, 30, 30, 500000), None, 0, b'', True, err_msg0.format("''")),
            )
        msg = ("Expected {} with time {}, tz {}, fold {}, "
               "and bytes_str {}, found {}.")

        for time, tz, fold, bytes_str, validity, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)

            if validity:
                try:
                    result = t._time__setstate(bytes_str, '')
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                t._time__setstate(bytes_str, tz)
                result = (t.hour, t.minute, t.second,
                          t.microsecond, t.tzinfo, t.fold)
                expected_result = time + (tz, fold)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, time, tz, fold, bytes_str, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce_ex__(self):
        """
        Test that the __reduce_ex__ method creates the correct pickle value
        for protocol 3.
        """
        data = (
            ((12, 30, 30, 500000), datetime.BADI, 0),
            ((12, 30, 30, 500000), datetime.UTC, 1),
            )
        msg = "Expected {}, with time {}, found {}"

        for time, tz, fold in data:
            t0 = datetime.time(*time, tzinfo=tz, fold=fold)
            obj = pickle.dumps(t0)
            t1 = pickle.loads(obj)
            t0_result = (t0.hour, t0.minute, t0.second, t0.microsecond,
                         t0.tzinfo, t0.fold)
            t1_result = (t1.hour, t1.minute, t1.second, t1.microsecond,
                         t1.tzinfo, t1.fold)
            self.assertEqual(t0_result, t1_result, msg.format(
                t0_result, time, tz, fold, t1_result))


class TestBadiDatetime_datetime(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)
        self._time_fields = ('hour', 'minute', 'second', 'microsecond')

    def _get_time(self, time):
        t_len = len(time)
        hh = time[0] if t_len > 0 else 0
        mm = time[1] if t_len > 1 else 0
        ss = time[2] if t_len > 2 else 0
        us = time[3] if t_len > 3 else 0
        return hh, mm, ss, us

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        err_msg0 = ("A full short or long form Badi date must be used, found "
                    "{} fields.")
        data = (
            ((1, 1, 1), (12, 30, 30), None, 0, False, '0001-01-01T12:30:30'),
            ((1, 1, 1), (12, 30, 30, 500000), None, 0, False,
             '0001-01-01T12:30:30.500000'),
            ((1, 1, 1, 1, 1), (), None, 0, False, '01-01-01-01-01T00:00:00'),
            # Short form
            ((b'\x00\x00\x01\x01\x0c\x1e\x1e\x07\xa1 ',), (), None, 0,
             False, '-1842-01-01T12:30:30.500000'),
            # Long form
            ((b'\x0e\x12\x01\x01\x01\x0c\x1e\x1e\x07\xa1 ',), (), None, 0,
             False, '-05-18-01-01-01T12:30:30.500000'),
            # Short form
            ((b'\x073\x01\x01\x00\x00\x00\x00\x00\x00',), (),
             datetime.timezone.badi, 0, False, '0001-01-01T00:00:00+03:30'),
            # Long form
            ((b'\x14\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00',), (),
             datetime.timezone.badi, 0, False, '01-01-01-01-01T00:00:00+03:30'),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, "
               "and fold {}, found {}.")

        for date, time, tz, fold, validity, expected_result in data:
            hh, mm, ss, us = self._get_time(time)

            if validity:
                try:
                    result = datetime.datetime(*date)
                except AssertionError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {date} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.datetime(*date, hour=hh, minute=mm,
                                           second=ss, microsecond=us,
                                           tzinfo=tz, fold=fold)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_hour(self):
        """
        Test that the hour property returns the correct value.
        """
        data = (
            ((1, 1, 1), (12, 30, 30), None, 0, 12),
            ((1, 1, 1, 1, 1), (12, 30, 30), None, 0, 12)
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, "
               "and fold {}, found {}.")

        for date, time, tz, fold, expected_result in data:
            kwargs = {self._time_fields[i] : time[i]
                      for i, v in enumerate(time)}
            kwargs['tzinfo'] = tz
            kwargs['fold'] = fold
            dt = datetime.datetime(*date, **kwargs)
            result = dt.hour
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, time, tz, fold, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_minute(self):
        """
        Test that the minute returns the correct value.
        """
        data = (
            ((1, 1, 1), (12, 30, 30), None, 0, 30),
            ((1, 1, 1, 1, 1), (12, 30, 30), None, 0, 30)
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, "
               "and fold {}, found {}.")

        for date, time, tz, fold, expected_result in data:
            kwargs = {self._time_fields[i] : time[i]
                      for i, v in enumerate(time)}
            kwargs['tzinfo'] = tz
            kwargs['fold'] = fold
            dt = datetime.datetime(*date, **kwargs)
            result = dt.minute
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, time, tz, fold, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_second(self):
        """
        Test that the second returns the correct value.
        """
        data = (
            ((1, 1, 1), (12, 30, 30), None, 0, 30),
            ((1, 1, 1, 1, 1), (12, 30, 30), None, 0, 30)
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, "
               "and fold {}, found {}.")

        for date, time, tz, fold, expected_result in data:
            kwargs = {self._time_fields[i] : time[i]
                      for i, v in enumerate(time)}
            kwargs['tzinfo'] = tz
            kwargs['fold'] = fold
            dt = datetime.datetime(*date, **kwargs)
            result = dt.second
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, time, tz, fold, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_microsecond(self):
        """
        Test that the microsecond returns the correct value.
        """
        data = (
            ((1, 1, 1), (12, 30, 30, 500000), None, 0, 500000),
            ((1, 1, 1, 1, 1), (12, 30, 30, 999999), None, 0, 999999)
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, "
               "and fold {}, found {}.")

        for date, time, tz, fold, expected_result in data:
            kwargs = {self._time_fields[i] : time[i]
                      for i, v in enumerate(time)}
            kwargs['tzinfo'] = tz
            kwargs['fold'] = fold
            dt = datetime.datetime(*date, **kwargs)
            result = dt.microsecond
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, time, tz, fold, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_tzinfo(self):
        """
        Test that the tzinfo returns the correct value.
        """
        data = (
            ((1, 1, 1), (12, 30, 30), None, 0, None),
            ((1, 1, 1, 1, 1), (12, 30, 30), datetime.BADI, 0, datetime.BADI)
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, "
               "and fold {}, found {}.")

        for date, time, tz, fold, expected_result in data:
            kwargs = {self._time_fields[i] : time[i]
                      for i, v in enumerate(time)}
            kwargs['tzinfo'] = tz
            kwargs['fold'] = fold
            dt = datetime.datetime(*date, **kwargs)
            result = dt.tzinfo
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_fold(self):
        """
        Test that the fold returns the correct value.
        """
        data = (
            ((1, 1, 1), (12, 30, 30), None, 0, 0),
            ((1, 1, 1, 1, 1), (12, 30, 30), None, 1, 1)
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, "
               "and fold {}, found {}.")

        for date, time, tz, fold, expected_result in data:
            kwargs = {self._time_fields[i] : time[i]
                      for i, v in enumerate(time)}
            kwargs['tzinfo'] = tz
            kwargs['fold'] = fold
            dt = datetime.datetime(*date, **kwargs)
            result = dt.fold
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, time, tz, fold, str(result)))

    @unittest.skip("Temporarily skipped")
    def test__fromtimestamp(self):
        """
        Test that the _fromtimestamp classmethod creates an instance
        of datetime.
        """
        data = (
            (0, False, datetime.UTC, True, ''),
            (0, False, None, True, '0126-16-01 00:00:00'),
            #(1, False, datetime.BADI, True, ''),
            )
        msg = ("Expected {} with timestamp {}, badi {}, timezone {}, "
               "and short {}, found {}.")

        for t, badi, tz, short, expected_result in data:
            result = datetime.datetime._fromtimestamp(t, badi, tz, short=short)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, t, badi, tz, short, result))

    @unittest.skip("Temporarily skipped")
    def test_fromtimestamp(self):
        """
        Test that the fromtimestamp classmethod creates an instance
        of datetime.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_utcfromtimestamp(self):
        """
        Test that the utcfromtimestamp classmethod creates an instance
        of datetime.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_now(self):
        """
        Test that the now classmethod creates an instance
        of datetime.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_utcnow(self):
        """
        Test that the utcnow classmethod creates an instance
        of datetime.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_combine(self):
        """
        Test that the combine classmethod creates an instance of datetime
        from an instance of a date and time object.
        """
        err_msg0 = "date argument must be a date instance, found {}."
        err_msg1 = "time argument must be a time instance, found {}."
        data = (
            ((181, 1, 1), (12, 30, 30), True, False, '0181-01-01T12:30:30'),
            ((1, 10, 10, 1, 1), (12, 30, 30), True, False,
             '01-10-10-01-01T12:30:30'),
            ((181, 13, 3), (12, 30, 30, 500000), True, False,
             '0181-13-03T12:30:30.500000'),
            ((181, 1, 1), (12, 30, 30), datetime.BADI, False,
             '0181-01-01T12:30:30+03:30'),
            )
        msg = "Expected {} with date {}, time {}, and timezone {}, found {}."

        for date, time, tz, validity, expected_result in data:
            d = datetime.date(*date)
            t = datetime.time(*time, tzinfo=tz if tz is not True else None)

            if validity:
                try:
                    result = datetime.datetime.combine(d, t, tzinfo=t.tzinfo)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.datetime.combine(d, t, tzinfo=t.tzinfo)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisoformat(self):
        """
        Test that the fromisoformat classmethod creates an instance
        of datetime.
        """
        data =(
            ('0181-01-01T12:30:30.500000', '0181-01-01T12:30:30.5'),
            ('0001-01-01T00:00:00.0+03:30', '0001-01-01T00:00:00+03:30'),
            ('-1842-01-01T00:00:00+03:30', '-1842-01-01T00:00:00+03:30'),
            ('1161-19-19T+03:30', '1161-19-19T00:00:00+03:30'),
            ('0181-13-09B', '0181-13-09T00:00:00+03:30'),
            ('0181-13-09Z', '0181-13-09T00:00:00+00:00'),
            )
        msg = "Expected {} with date and time {}, "

        for dt, expected_result in data:
            result = datetime.datetime.fromisoformat(dt)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, dt, result))

    #@unittest.skip("Temporarily skipped")
    def test_timetuple(self):
        """
        Test that the timetuple method returns either a short or long form
        timetuple.
        """
        # *** TODO *** Update the two test below that have timezone objects.
        data = (
            ((181, 13, 9), (12, 30, 30), None, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=13, tm_mday=9, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=2, tm_yday=237, '
             'tm_isdst=-1)'),
            ((181, 13, 9), (12, 30, 30), datetime.BADI, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=13, tm_mday=9, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=2, tm_yday=237, '
             'tm_isdst=-1)'),
            ((1, 10, 10, 13, 9), (12, 30, 30), None, 0,
             'structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, '
             'tm_year=10, tm_mon=13, tm_mday=9, tm_hour=12, tm_min=30, '
             'tm_sec=30, tm_wday=2, tm_yday=237, tm_isdst=-1)'),
            ((1, 10, 10, 13, 9), (12, 30, 30), datetime.BADI, 0,
             'structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, '
             'tm_year=10, tm_mon=13, tm_mday=9, tm_hour=12, tm_min=30, '
             'tm_sec=30, tm_wday=2, tm_yday=237, tm_isdst=-1)'),
            )
        msg = "Expected {} with date {}, time {}, and timezone {}, found {}."

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2])
            result = dt.timetuple()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, result))

    @unittest.skip("Temporarily skipped")
    def test__mktime(self):
        """
        Test that the _mktime method 
        """
        data = (
            (),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")



    @unittest.skip("Temporarily skipped")
    def test_timestamp(self):
        """
        Test that thetimestamp method 
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_utctimetuple(self):
        """
        Test that the utctimetuple method returns a timetuple object.
        """
        data = (
            ((181, 1, 1), (12, 30, 30), None, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=1, tm_mday=1, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=4, tm_yday=1, '
             'tm_isdst=0)'),
            ((181, 1, 1), (12, 30, 30), datetime.UTC, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=1, tm_mday=1, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=4, tm_yday=1, '
             'tm_isdst=0)'),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt.utctimetuple()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))

    @unittest.skip("Temporarily skipped")
    def test_baditimetuple(self):
        """
        Test that the baditimetuple method returns a timetuple object.
        """
        data = (
            ((), (), None, 0, ''),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt.baditimetuple()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))

    @unittest.skip("Temporarily skipped")
    def test__timetuple(self):
        """
        Test that the _timetuple method returns a timetuple object.
        """
        data = (
            ((), (), None, 0, ''),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt._timetuple(offset)
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_date(self):
        """
        Test that thedate method returns a date object with the same date as
        the originating datetime object.
        """
        data = (
            ((181, 1, 1), (12, 30, 30), None, 0, '0181-01-01'),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt.date()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_time(self):
        """
        Test that the time method a time object with the same time as
        the originating datetime object.
        """
        data = (
            ((181, 1, 1), (12, 30, 30), None, 0, '12:30:30'),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt.time()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_timetz(self):
        """
        Test that the timetz method a time object with the same time and
        tzinfo as the originating datetime object.
        """
        data = (
            ((181, 1, 1), (12, 30, 30), datetime.BADI, 0, '12:30:30+03:30'),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt.timetz()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_replace(self):
        """
        Test that the replace method a new datetime object with the
        replaced values.
        """
        err_msg0 = "Cannot convert from a short to a long form date."
        err_msg1 = ("Cannot convert from a long to a short form date. The "
                    "value {} is not valid for long form dates.")
        data = (
            # Normal replace for a short date
            ((181, 1, 1, None, None), None, 0, (None, None, 182, None, None),
             None, 0, False, '0182-01-01T00:00:00'),
            ((181, 1, 1, None, None), None, 0, (None, None, None, 9, 12),
             None, 0, False, '0181-09-12T00:00:00'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, 23, None, None),
             None, 0, False, '0181-01-01T23:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, 15, 15),
             None, 0, False, '0181-01-01T12:15:15'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             datetime.BADI, 0, False, '0181-01-01T12:30:30+03:30'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             None, 1, False, '0181-01-01T12:30:30'),
            # Normal replace for a long date
            ((1, 10, 10, 1, 1), None, 0, (None, None, 11, None, None), None, 0,
             False, '01-10-11-01-01T00:00:00'),
            ((1, 10, 10, 1, 1), None, 0, (None, 9, None, None, None), None, 0,
             False, '01-09-10-01-01T00:00:00'),
            ((1, 10, 10, 1, 1), None, 0, (None, 9, 10, None, None), None, 0,
             False, '01-09-10-01-01T00:00:00'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, 23, None, None), None, 0,
             False, '01-10-10-01-01T23:30:30'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, 15, 15), None, 0,
             False, '01-10-10-01-01T12:15:15'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             datetime.BADI, 0, False, '01-10-10-01-01T12:30:30+03:30'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             None, 1, False, '01-10-10-01-01T12:30:30'),
            # Error conditions.
            ((181, 1, 1, None, None), None, 0, (1, 10, None, None, None),
             None, 0, True, err_msg0),
            ((1, 10, 10, 1, 1), None, 0, (None, None, 181, 1, None),
             None, 0, True, err_msg1.format(181)),
            )
        msg = "Expected {} with date1 {} and date2 {}, found {}."

        for (date1, tz0, fold0, date2, tz1, fold1,
             validity, expected_result) in data:
            dt = datetime.datetime(*date1, tzinfo=tz0, fold=fold0)

            if validity:
                try:
                    result = dt.replace(*date2, tzinfo=tz1, fold=fold1)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date1}' an error is not "
                                         f"raised, with result {result}.")
            else:
                result = dt.replace(*date2, tzinfo=tz1, fold=fold1)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date1, date2, str(result)))
                self.assertEqual(fold1, result.fold,
                                 f"Expected fold {fold1}, found {result.fold}")

    @unittest.skip("Temporarily skipped")
    def test__local_timezone(self):
        """
        Test that the _local_timezone method 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_asutctimezone(self):
        """
        Test that the asutctimezone method 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_asbaditimezone(self):
        """
        Test that the asbaditimezone method 
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_ctime(self):
        """
        Test that the ctime method creates a string indicating the date
        and time.

        All days before 1752-09-14 in the Gregorian Calendar will be wrong
        when compaired to the Badi Calendar in UK and the US. This is when
        The Gregorian Calendar was adopted and compinsated 11 days.
        """
        data = (
            # 0001-03-20 Sunday (Fiḍāl -> Tuesday)
            ((-1842, 1, 1), 'Fiḍāl Bahá  1 00:00:00 -1842'),
            ((-1842, 1, 1, None, None, 12, 30, 30),
             'Fiḍāl Bahá  1 12:30:30 -1842'),
            # 1582-10-04 Thursday (Jamál -> Sunday)
            ((-261, 11, 7), 'Jamál Mashíyyat  7 00:00:00 -0261'),
            ((-261, 11, 7, None, None, 6, 15, 15),
             'Jamál Mashíyyat  7 06:15:15 -0261'),
            # 1843-03-21 Tuesday
            ((0, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0000'),
            ((0, 1, 1, None, None, 12, 30, 30), 'Fiḍāl Bahá  1 12:30:30 0000'),
            # 1844-03-20 Thursday
            ((1, 1, 1), '`Idāl Bahá  1 00:00:00 0001'),
            ((1, 1, 1, None, None, 12, 30, 30), '`Idāl Bahá  1 12:30:30 0001'),
            # 2024-03-20 Wednesday
            ((181, 1, 1), '`Idāl Bahá  1 00:00:00 0181'),
            ((181, 1, 1, None, None, 12, 30, 30),
             '`Idāl Bahá  1 12:30:30 0181'),
            # 2024-08-14 Wednesday
            ((181, 8, 15, None, None, 12, 30, 30),
             '`Idāl Kamál 15 12:30:30 0181'),
            ((181, 8, 15), '`Idāl Kamál 15 00:00:00 0181'),
            ((181, 8, 15, None, None, 12, 30, 30),
             '`Idāl Kamál 15 12:30:30 0181'),
            # 2024-08-14 Wednesday
            ((1, 10, 10, 8, 15), '`Idāl Kamál 15 00:00:00 0181'),
            ((1, 10, 10, 8, 15, 12, 30, 30), '`Idāl Kamál 15 12:30:30 0181'),
            # 2024-08-15 Thursday
            ((1, 10, 10, 8, 16) , 'Istijlāl Kamál 16 00:00:00 0181'),
            ((1, 10, 10, 8, 16, 12, 30, 30) ,
             'Istijlāl Kamál 16 12:30:30 0181'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            dt = datetime.datetime(*date)
            result = dt.ctime()
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoformat(self):
        """
        Test that the isoformat method returns an ISO formatted date and time.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), 'T', 'auto',
             '0181-01-01T12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30), ' ', 'auto',
             '0181-01-01 12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'auto',
             '0181-01-01T12:30:30.500000'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'hours',
             '0181-01-01T12'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'minutes',
             '0181-01-01T12:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'seconds',
             '0181-01-01T12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'milliseconds',
             '0181-01-01T12:30:30.500'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'microseconds',
             '0181-01-01T12:30:30.500000'),
            )
        msg = "Expected {} with date {}, sep {}, and timespec {}, found {}."

        for date, sep, ts, expected_result in data:
            dt = datetime.datetime(*date)
            result = dt.isoformat(sep, ts)
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, sep, ts, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ method returns a string of the object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30), None, 0,
             'datetime.datetime(181, 1, 1, 12, 30)'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             'datetime.datetime(181, 1, 1, 12, 30, 30)'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), None, 0,
             'datetime.datetime(181, 1, 1, 12, 30, 30, 500000)'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             'datetime.datetime(181, 1, 1, 12, 30, 30, '
             'tzinfo=datetime.timezone.badi)'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 1,
             'datetime.datetime(181, 1, 1, 12, 30, 30, '
             'tzinfo=datetime.timezone.badi, fold=1)'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = repr(dt)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test__dt_str_conversion(self):
        """
        Test that the _dt_str_conversion method returns a string of the object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30), None, 0, '0181-01-01T12:30:00'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             '0181-01-01T12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), None, 0,
             '0181-01-01T12:30:30.500000'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             '0181-01-01T12:30:30+03:30'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 1,
             '0181-01-01T12:30:30+03:30'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = str(dt)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    @unittest.skip("Temporarily skipped")
    def test_strptime(self):
        """
        Test that the strptime classmethod 
        """


    #@unittest.skip("Temporarily skipped")
    def test_utcoffset(self):
        """
        Test that the utcoffset method returns a timedelta object.
        """
        data = (
            ((181, 1, 1), (12, 30, 30), datetime.UTC, 0, '0:00:00'),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt.utcoffset()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))
            self.assertTrue(isinstance(result, datetime.timedelta), msg.format(
                    expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_badioffset(self):
        """
        Test that the badioffset method returns a timedelta object.
        """
        data = (
            ((181, 1, 1), (12, 30, 30), datetime.BADI, 0, '3:30:00'),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, hour=time[0], minute=time[1],
                                   second=time[2], tzinfo=tz, fold=fold)
            result = dt.badioffset()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, fold, result))
            self.assertTrue(isinstance(result, datetime.timedelta), msg.format(
                    expected_result, date, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_tzname(self):
        """
        Test that the tzname method returns the timezone associated with
        the datetime object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             'UTC+03:30'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.UTC, 0, 'UTC'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.tzname()
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    @unittest.skip("Temporarily skipped")
    def test_dst(self):
        """
        Test that the dst method returns the daylight savings time
        associated with the datetime object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0, None),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.UTC, 1, ''),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.dst()
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns True if equal and False if
        not equal.
        """
        data = (
            ((181, 9, 14, None, None, 12, 30, 30), None, 0,
             (181, 9, 14, None, None, 12, 30, 30), None, 0, True),
            ((181, 9, 14, None, None, 12, 30, 30), None, 0,
             (181, 9, 14, None, None, 12, 30, 29), None, 0, False),
            ((181, 9, 14, None, None, 12, 30, 30), None, 0,
             (181, 9, 14, None, None, 12, 30, 31), None, 0, False),
            ((181, 9, 14, None, None, 12, 30, 30), datetime.UTC, 0,
             (181, 9, 14, None, None, 12, 30, 30), None, 0, False),
            #((181, 9, 14, None, None, 12, 30, 30), None, 0,
            # (181, 9, 14, None, None, 12, 30, 30), None, 1, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None, 0,
             (1, 10, 10, 9, 14, 12, 30, 30), None, 0, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None, 0,
             (1, 10, 10, 9, 14, 12, 30, 31), None, 0, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None, 0,
             (1, 10, 10, 9, 14, 12, 30, 31), None, 0, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), datetime.UTC, 0,
             (1, 10, 10, 9, 14, 12, 30, 31), None, 0, False),
            )
        msg = "Expected {} with date0 {} date1 {}, found {}."

        for date0, tz0, fold0, date1, tz1, fold1, expected_result in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz0, fold=fold0)
            dt1 = datetime.datetime(*date1, tzinfo=tz1, fold=fold1)
            result = dt0 == dt1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date0, date1, result))

    @unittest.skip("Temporarily skipped")
    def test___le__(self):
        """
        Test that the __le__ method returns True if less than or equal and
        False if not less than or equal.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___lt__(self):
        """
        Test that the __lt__ method returns True if less than and False
        if not less than.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___ge__(self):
        """
        Test that the __ge__ method returns True if greater than or equal
        and False if not greater than or equal.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___gt__(self):
        """
        Test that the __gt__ method returns True if greater than and False
        if not greater than.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns 1 if the two dates are equal, +1
        if the current date is greater than the test date, and -1 if the
        inverse.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___add__(self):
        """
        Test that the __add__ method 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___sub__(self):
        """
        Test that the __sub__ method 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method 
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the correct state for pickling.
        """
        data = (
            ((datetime.MINYEAR, 1, 1), (12, 30, 30, 500000), None, 0,
             (b'\x00\x00\x01\x01\x0c\x1e\x1e\x07\xa1 ',)),
            ((-5, 18, 1, 1, 1), (12, 30, 30, 500000), None, 0,
             (b'\x0e\x12\x01\x01\x01\x0c\x1e\x1e\x07\xa1 ',)),
            ((1, 1, 1), (), datetime.BADI, 0,
             (b'\x073\x01\x01\x00\x00\x00\x00\x00\x00',
              datetime.timezone.badi)),
            ((1, 1, 1, 1, 1), (), datetime.BADI, 0,
             (b'\x14\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00',
              datetime.timezone.badi)),
            )
        msg = ("Expected {} with date {}, time {}, timezone {}, and fold {}, "
               "found {}.")

        for date, time, tz, fold, expected_result in data:
            hh, mm, ss, us = self._get_time(time)
            dt = datetime.datetime(*date, hour=hh, minute=mm, second=ss,
                                   microsecond=us, tzinfo=tz, fold=fold)
            result = dt._getstate()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, time, tz, fold, result))

    @unittest.skip("Temporarily skipped")
    def test___setstate(self):
        """
        Test that the __setstate method 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___reduce_ex__(self):
        """
        Test that the __reduce_ex__ method 
        """
        pass


class TestBadiDatetime_timezone(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        err_msg0 = "offset must be a timedelta"
        err_msg1 = "name must be a string"
        err_msg2 = ("offset must be a timedelta strictly between "
                    "-timedelta(hours=24) and timedelta(hours=24).")
        td = datetime.timedelta(hours=datetime.BADI_TZ[0])
        data = (
            (td, 'Asia/Terhan', False, 'Asia/Terhan'),
            (td, datetime.timezone._Omitted, False, 'UTC+03:30'),
            (td, '', False, ''),
            (object, 'Asia/Terhan', True, err_msg0),
            (td, object, True, err_msg1),
            (datetime.timedelta(hours=25), '', True, err_msg2),
            )
        msg = "Expected {} with offset {} and name {}, found {}."

        for offset, name, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.timezone(offset, name)
                except (TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.timezone(offset, name)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, name, result))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns  True if equal and False if
        not equal.
        """
        td0 = datetime.timedelta(hours=datetime.BADI_TZ[0])
        td1 = datetime.timedelta(seconds=18000)
        data = (
            (td0, 'Asia/Terhan', td0, 'Asia/Terhan', True),
            (td0, 'Asia/Terhan', td1, 'US/Eastern', False),
            )
        msg = "Expected {} with td0 {} and td1 {}, found {}."

        for offset0, name0, offset1, name1, expected_result in data:
            tz0 = datetime.timezone(offset0, name0)
            tz1 = datetime.timezone(offset1, name1)
            result = tz0 == tz1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, td0, td1, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ method returns the correctly formatted string.
        """
        td0 = datetime.timedelta(hours=datetime.BADI_TZ[0])
        td1 = datetime.timedelta(0)
        td2 = datetime.timedelta()
        data = (
            (td0, 'Asia/Terhan', "datetime.timezone("
             "datetime.timedelta(seconds=12600), 'Asia/Terhan')"),
            (td1, 'UTC', "datetime.timezone(datetime.timedelta(0), 'UTC')"),
            (td2, '', "datetime.timezone(datetime.timedelta(0), '')"),
            (td2, None, "datetime.timezone(datetime.timedelta(0))"),
            ('UTC', 'UTC', 'datetime.timezone.utc'),
            ('BADI', 'Asia/Terhan', 'datetime.timezone.badi'),
            )
        msg = "Expected {} with offset {} and name {}, found {}."

        for offset, name, expected_result in data:
            if name is None:
                tz = datetime.timezone._create(offset, name)
            elif offset == 'UTC':
                tz = datetime.UTC
            elif offset == 'BADI':
                tz = datetime.BADI
            else:
                tz = datetime.timezone(offset, name)

            result = repr(tz)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, offset, name, result))

    #@unittest.skip("Temporarily skipped")
    def test___str__(self):
        """
        Test that the __str__ method returns the correctly formatted string.
        """
        td0 = datetime.timedelta(hours=datetime.BADI_TZ[0])
        td1 = datetime.timedelta(seconds=18000)
        data = (
            (td0, 'Asia/Terhan', 'Asia/Terhan'),
            (td1, 'US/Eastern', 'US/Eastern'),
            )
        msg = "Expected {} with offset {}, found {}."

        for offset, name, expected_result in data:
            result = datetime.timezone(offset, name)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, offset, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_utcoffset(self):
        """
        Test that the utcoffset method returns the correct timezone offset.
        """
        err_msg0 = "utcoffset() argument must be a datetime instance or None"
        td = datetime.timedelta(hours=datetime.BADI_TZ[0])
        data = (
            (td, 'Asia/Terhan', (181, 1, 1), datetime.UTC, False, '3:30:00'),
            (td, 'Asia/Terhan', (12, 30, 30), datetime.UTC, True, err_msg0),
            )
        msg = ("Expected {} with offset {}, name {}, date {}, and "
               "timezone {}, found {}.")

        for offset, name, date, tz0, validity, expected_result in data:
            tz1 = datetime.timezone(offset, name)

            if validity:
                try:
                    t = datetime.time(*date)
                    result = tz1.utcoffset(t)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {tz1} an error is not "
                                         f"raised, with result {result}.")
            else:
                dt = datetime.datetime(*date, tzinfo=tz0)
                result = tz1.utcoffset(dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, name, date, tz1, result))

    #@unittest.skip("Temporarily skipped")
    def test_tzname(self):
        """
        Test that the tzname method returns the timezone name.
        """
        err_msg0 = "tzname() argument must be a datetime instance or None"
        td = datetime.timedelta(hours=datetime.BADI_TZ[0])
        data = (
            (td, 'Asia/Terhan', (181, 1, 1), None, False, 'Asia/Terhan'),
            (td, 'Asia/Terhan', None, None, False, 'Asia/Terhan'),
            (td, None, None, None, False, 'UTC+03:30'),
            (td, 'Asia/Terhan', False, None, True, err_msg0),
            )
        msg = ("Expected {} with offset {}, name {}, date {}, and "
               "timezone {}, found {}.")

        for offset, name, date, tz0, validity, expected_result in data:
            if name is None:
                tz1 = datetime.timezone._create(offset, name)
            else:
                tz1 = datetime.timezone(offset, name)

            if date:
                dt = datetime.datetime(*date)
            elif date is False:
                dt = datetime.time()
            else:
                dt = date

            if validity:
                try:
                    result = tz1.tzname(dt)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {tz1} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = tz1.tzname(dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, name, date, tz1, result))

    #@unittest.skip("Temporarily skipped")
    def test_dst(self):
        """
        Test that the dst method always returns None.
        """
        err_msg0 = "dst() argument must be a datetime instance or None"
        td = datetime.timedelta(hours=datetime.BADI_TZ[0])
        data = (
            (td, 'Asia/Terhan', (181, 1, 1), None, False, None),
            (td, 'Asia/Terhan', None, None, False, None),
            (td, 'Asia/Terhan', False, None, True, err_msg0),
            )
        msg = ("Expected {} with offset {}, name {}, date {}, and "
               "timezone {}, found {}.")

        for offset, name, date, tz0, validity, expected_result in data:
            tz1 = datetime.timezone(offset, name)

            if date:
                dt = datetime.datetime(*date)
            elif date is False:
                dt = datetime.time()
            else:
                dt = date

            if validity:
                try:
                    result = tz1.dst(dt)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {tz1} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = tz1.dst(dt)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, offset, name, date, tz1, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromutc(self):
        """
        Test that the fromutc method returns a timezone object from a UTC
        timezone.
        """
        err_msg0 = "fromutc: dt.tzinfo is not self"
        err_msg1 = "fromutc() argument must be a datetime instance or None"
        td = datetime.timedelta(hours=datetime.BADI_TZ[0])
        data = (
            (td, 'Asia/Terhan', (181, 1, 1), False,
             "01-10-10-01-01T03:30:00+03:30"),
            (td, 'Asia/Terhan', (181, 1, 1), True, err_msg0),
            (td, 'Asia/Terhan', None, True, err_msg1),
            )
        msg = "Expected {} with offset {}, name {}, and date {} found {}."

        for offset, name, date, validity, expected_result in data:
            tz1 = datetime.timezone(offset, name)

            if not validity and date:
                dt = datetime.datetime(*date, tzinfo=tz1)
            else:
                dt = datetime.time()

            if validity:
                if date:
                    dt = datetime.datetime(*date)

                try:
                    result = tz1.fromutc(dt)
                except (TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {tz1} an error is not "
                                         f"raised, with result {result}.")
            else:
                dt = datetime.datetime(*date, tzinfo=tz1)
                result = tz1.fromutc(dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, name, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__name_from_offset(self):
        """
        Test that the _name_from_offset returns a string indicating the
        UTC offset.
        """
        td0 = datetime.timedelta(0)
        td1 = datetime.timedelta(hours=datetime.BADI_TZ[0])
        td2 = datetime.timedelta(-1)
        td3 = datetime.timedelta(microseconds=500000)
        td4 = datetime.timedelta(seconds=50)
        data = (
            (td0, 'UTC'),
            (td1, "UTC+03:30"),
            (td2, 'UTC-24:00'),
            (td3, 'UTC+00:00:00.500000'),
            (td4, 'UTC+00:00:50'),
            )
        msg = "Expected {} with offset {} found {}."

        for offset, expected_result in data:
            tz = datetime.timezone(td0, 'UTC')
            result = tz._name_from_offset(offset)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, offset, result))

    @unittest.skip("Temporarily skipped")
    def test____getinitargs__(self):
        """
        Test that the __getinitargs__ method 
        """

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns a valid hash.
        """
        td0 = datetime.timedelta(0)
        data = (
            (td0, 'UTC'),
            )
        msg = "offset {} with name {}, found {}."

        for offset, name in data:
            tz = datetime.timezone(offset, name)
            result = hash(tz)
            self.assertTrue(len(str(result)) > 15, msg.format(
                offset, name, result))
