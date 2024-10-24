# -*- coding: utf-8 -*-
#
# badidatetime/test/test_datetime.py
#
__docformat__ = "restructuredtext en"

import re
import os
import sys
import time
import pickle
import unittest

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import datetime
from ..badi_calendar import BahaiCalendar
from .._timedateutils import _td_utils

class TestBadiDatetimeFunctions(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

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
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, x, y, result))

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
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, values, result))


class TestBadiDatetime_timedelta(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method 
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
            ((1, 1, 1), 'badidatetime.datetime.timedelta('
             'days=1, seconds=1, microseconds=1)'),
            ((0, 0, 0), 'badidatetime.datetime.timedelta(0)'),
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
        err_msg3 = ("Failed to encode latin1 string when unpickling a date "
                    "object. pickle.load(data, encoding='latin1') is assumed.")
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
            (0, True, '0126-16-01'),
            (1723057467.0619307, False, '01-10-10-08-08'),
            (1723057467.0619307, True, '0181-08-08'),
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
        err_msg_3 = (f"Year is out of range: {{}}, min {_td_utils.MINYEAR}, "
                     f"max {_td_utils.MAXYEAR}.")
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
            ((181, 9, 16), 'badidatetime.datetime.date(181, 9, 16)'),
            ((1, 10, 10, 9, 16),
             'badidatetime.datetime.date(1, 10, 10, 9, 16)'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = repr(d)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___short_from_long_form(self):
        """
        Test that the __short_from_long_form method returns the short form
        Badi date.
        """
        data = (
            ((1, 1, 1, 1, 1), (1, 1, 1)),
            ((1, 10, 10, 10, 12), (181, 10, 12)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d._date__short_from_long_form()
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
        Test that the __format__ method 
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
            (_td_utils.MINYEAR, 1, 1),
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
    def test_is_pickle_data(self):
        """
        Test that the is_pickle_data classmethod returns the correct results
        depending on the incoming data.
        """
        err_msg0 = "Invalid string {} had length of {} for pickle."
        err_msg1 = ("Failed to encode latin1 string when unpickling a date "
                    "object. pickle.load(data, encoding='latin1') is assumed.")
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
                    result = datetime.date.is_pickle_data(*value)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.date.is_pickle_data(*value)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the state of the class.
        """
        data = (
            ((_td_utils.MINYEAR, 1, 1), (b'\x00\x00\x01\x01',)),
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
            ((_td_utils.MINYEAR, 1, 1), b'\x00\x00\x01\x01'),
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
            (_td_utils.MINYEAR, 1, 1),
            (1, 1, 1),
            (1, 1, 1, 1, 1),
            (_td_utils.MAXYEAR, 1, 1),
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
    def test_tzname(self):
        """
        Test that the tzname method raises an exception is not overridden.
        """
        err_msg0 = "tzinfo subclass must override tzname()"
        data = (
            ((181, 1, 1), err_msg0),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            dt = datetime.date(*date)
            tz = datetime.tzinfo()

            try:
                result = tz.tzname(dt)
            except NotImplementedError as e:
                self.assertEqual(expected_result, str(e))
            else:
                result = result if result else None
                raise AssertionError(f"With {value} an error is not "
                                     f"raised, with result {result}.")

    #@unittest.skip("Temporarily skipped")
    def test_utcoffset(self):
        """
        Test that the utcoffset method raises an exception is not overridden.
        """
        err_msg0 = "tzinfo subclass must override utcoffset()"
        data = (
            ((181, 1, 1), err_msg0),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            dt = datetime.date(*date) # *** TODO *** Use datetime() later on.
            tz = datetime.tzinfo()

            try:
                result = tz.utcoffset(dt)
            except NotImplementedError as e:
                self.assertEqual(expected_result, str(e))
            else:
                result = result if result else None
                raise AssertionError(f"With {value} an error is not "
                                     f"raised, with result {result}.")

    #@unittest.skip("Temporarily skipped")
    def test_dst(self):
        """
        Test that the dst method raises an exception is not overridden.
        """
        err_msg0 = "tzinfo subclass must override dst()"
        data = (
            ((181, 1, 1), err_msg0),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            dt = datetime.date(*date) # *** TODO *** Use datetime() later on.
            tz = datetime.tzinfo()

            try:
                result = tz.dst(dt)
            except NotImplementedError as e:
                self.assertEqual(expected_result, str(e))
            else:
                result = result if result else None
                raise AssertionError(f"With {value} an error is not "
                                     f"raised, with result {result}.")

    @unittest.skip("Temporarily skipped")
    def test_fromutc(self):
        """
        Test that the fromutc method 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test___reduce__(self):
        """
        Test that the __reduce__ method 
        """
        pass


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
            (_td_utils.MINYEAR, 1, 1),
            (1, 1, 1),
            (_td_utils.MAXYEAR, 1, 1),
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




class TestBadiDatetime_datetime(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)




class TestBadiDatetime_timezone(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)
