# -*- coding: utf-8 -*-
#
# badidatetime/test/test_timedateutils.py
#
__docformat__ = "restructuredtext en"

import os
import sys
import locale
import unittest

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from .._timedateutils import _td_utils
from ..datetime import _build_struct_time
from ..badi_calendar import BahaiCalendar


class TestTimeDateUtils(unittest.TestCase):
    """
    Notes on running these tests. If you live outside of the USA you may need
    to install the USA local info on your computer or some tests will fail.
    There is no way to test for changing local information when most tests
    are hard coded.
    """

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test__order_format(self):
        """
        Test that the _order_format returns a correctly parsed date or
        time format.
        """
        data = (
            ('%m/%d/%y', '%m/%d/%y', ['/', 'm', 'd', 'y']),
            ('%H:%M:%S', '%H:%M:%S', [':', 'H', 'M', 'S']),
            )
        msg = "Expected {}, with format {} and default {}, found {}."

        for fmt, default, expected_result in data:
            result = _td_utils._order_format(fmt, default)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, fmt, default, result))

    #@unittest.skip("Temporarily skipped")
    def test__find_time_order(self):
        """
        """
        data = (
            '%I:%M:%S',
            )
        msg = "Expected {}, found {}."

        for expected_result in data:
            result = _td_utils._find_time_order()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_locale(self):
        """
        Test that the locale property is set correctly.
        """
        data = (
            "{}.{}".format(*locale.getlocale()),
            )
        msg = "Expected {}, found {}."

        for expected_result in data:
            result = _td_utils.locale
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_am(self):
        """
        Test that the am property is set correctly
        """
        data = (
            locale.nl_langinfo(locale.AM_STR),
            )
        msg = "Expected {}, found {}."

        for expected_result in data:
            result = _td_utils.am
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_pm(self):
        """
        Test that the pm property is set correctly
        """
        data = (
            locale.nl_langinfo(locale.PM_STR),
            )
        msg = "Expected {}, found {}."

        for expected_result in data:
            result = _td_utils.pm
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_date_format(self):
        """
        Test that the date_format property is set correctly
        """
        data = (
            ['/', 'm', 'd', 'Y'],
            )
        msg = "Expected {}, found {}."

        for expected_result in data:
            result = _td_utils.date_format
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_time_format(self):
        """
        Test that the time_format property is set correctly
        """
        data = (
            [':', 'I', 'M', 'S'],
            )
        msg = "Expected {}, found {}."

        for expected_result in data:
            result = _td_utils.time_format
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test__checktm(self):
        """
        Test that the _checktm method does not raise an exception with
        an invalid tupple type.
        """
        bad_t, ttup_l, ttup_s, ttup_tl, ttup_ts = 0, 1, 2, 3, 4
        MIN_K = _td_utils.KULL_I_SHAY_MIN
        MAX_K = _td_utils.KULL_I_SHAY_MAX
        MIN_Y = _td_utils.MINYEAR
        MAX_Y = _td_utils.MAXYEAR
        err_msg0 = ("Invalid kull-i-shay {}, it must be in the range "
                    f"of [{MIN_K}, {MAX_K}].")
        err_msg1 = ("Invalid Váḥids '{}' in a Kull-i-Shay’, it must be in "
                    "the range of [1, 19].")
        err_msg2 = ("Invalid year '{}' in a Váḥid, it must be in the "
                    "range of [1, 19].")
        err_msg3 = ("Invalid year '{}' it must be in the range of ["
                    f"{MIN_Y}, {MAX_Y}].")
        err_msg4 = "Invalid month '{}', it must be in the range of [0, 19]."
        err_msg5 = ("Invalid day '{}' for month '{}', it must be in the "
                    "range of [1, {}].")
        err_msg6 = "Invalid hour '{}', it must be in the range of [0, 24]."
        err_msg7 = "Invalid minute '{}', it must be in the range of [0, 60]."
        err_msg8 = "Invalid second '{}', it must be in the range of [0, 61]."
        err_msg9 = "Invalid week day '{}', it must be inthe range of [0, 6]."
        err_msg10 = ("Invalid day '{}' in year, it must be in the range of "
                     "[1, 366].")
        err_msg11 = "Invalid isdst '{}', it must be in the range of [-1, 1]."
        err_msg12 = "The ttup argument {} is not a proper tuple."
        data = (
            ### Valid tuples
            ((MIN_K, 1, 1, 1, 1, 1, 1, 1), -1, ttup_l, False, ''),
            ((1, 10, 10, 9, 6, 8, 45, 1), -1, ttup_l, False, ''),
            ((MAX_K, 5, 2, 19, 19, 1, 1, 1), -1, ttup_l, False, ''),
            ((MIN_Y, 1, 1, 0, 0, 0), -1, ttup_s, False, ''),
            ((181, 9, 6, 8, 45, 1), -1, ttup_s, False, ''),
            ((MAX_Y, 19, 19, 0, 0, 0), -1, ttup_s, False, ''),
            ((1, 10, 10, 1, 1, 0, 0, 0, 4, 1), -1, ttup_tl, False, ''),
            ((181, 1, 1, 0, 0, 0, 4, 1), -1, ttup_ts, False, ''),
            ### Invalid tuples
            # Long form NamedTuple errors
            ((-6, 1, 1, 1, 1, 1, 1, 1), -1, ttup_l, True, err_msg0.format(-6)),
            ((5, 1, 1, 1, 1, 1, 1, 1), -1, ttup_l, True, err_msg0.format(5)),
            ((1, 0, 1, 1, 1, 1, 1, 1), -1, ttup_l, True, err_msg1.format(0)),
            ((1, 20, 1, 1, 1, 1, 1, 1), -1, ttup_l, True, err_msg1.format(20)),
            ((1, 1, 0, 1, 1, 1, 1, 1), -1, ttup_l, True, err_msg2.format(0)),
            ((1, 1, 20, 1, 1, 1, 1, 1), -1, ttup_l, True, err_msg2.format(20)),
            # Long form standard tuple errors
            ((-6, 1, 1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, True,
             err_msg0.format(-6)),
            ((5, 1, 1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, True,
             err_msg0.format(5)),
            ((1, 0, 1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, True,
             err_msg1.format(0)),
            ((1, 20, 1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, True,
             err_msg1.format(20)),
            ((1, 1, 0, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, True,
             err_msg2.format(0)),
            ((1, 1, 20, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, True,
             err_msg2.format(20)),
            # Short for NamedTuple errors
            ((-1843, 1, 1, 0, 0, 0), -1, ttup_s, True, err_msg3.format(-1843)),
            ((1162, 1, 1, 0, 0, 0), -1, ttup_s, True, err_msg3.format(1162)),
            # Short for standard tuple errors
            ((-1843, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, True,
             err_msg3.format(-1843)),
            ((1162, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, True,
             err_msg3.format(1162)),
            # All tuple types use the same code for the month, day, hour,
            # minute, second, wday, yday, and isdst fields.
            # Month
            ((1, -1, 1, 0, 0, 0), -1, ttup_s, True, err_msg4.format(-1)),
            ((1, 20, 1, 0, 0, 0), -1, ttup_s, True, err_msg4.format(20)),
            # Day
            ((1, 1, 1, 1, -1, 0 ,0, 0, 1, 1), -1, ttup_tl, True,
             err_msg5.format(-1, 1, 19)),
            ((1, 1, 1, 1, 20, 0 ,0, 0, 1, 1), -1, ttup_tl, True,
             err_msg5.format(20, 1, 19)),
            # Day, leap year
            ((1, 0, 0, 0, 0, 0, 1, 1), -1, ttup_ts, True,
             err_msg5.format(0, 0, 5)),
            ((1, 0, 6, 0, 0, 0, 1, 1), -1, ttup_ts, True,
             err_msg5.format(6, 0, 5)),
            # Hour
            ((1, 1, 1, 1, 1, -1, 1, 1), -1, ttup_l, True, err_msg6.format(-1)),
            ((1, 1, 1, 1, 1, 25, 1, 1), -1, ttup_l, True, err_msg6.format(25)),
            # Minute
            ((1, 1, 1, 1, -1, 0), -1, ttup_s, True, err_msg7.format(-1)),
            ((1, 1, 1, 1, 60, 0), -1, ttup_s, True, err_msg7.format(60)),
            # Second
            ((1, 1, 1, 1, 1, 1, 1, -1, 1, 1), -1, ttup_tl, True,
             err_msg8.format(-1)),
            ((1, 1, 1, 1, 1, 1, 1, 62, 1, 1), -1, ttup_tl, True,
             err_msg8.format(62)),
            # Week day
            ((1, 1, 1, 0, 0, 0, -1, 1), -1, ttup_ts, True, err_msg9.format(-1)),
            ((1, 1, 1, 0, 0, 0, 7, 1), -1, ttup_ts, True, err_msg9.format(7)),
            # Day in year
            ((1, 1, 1, 1, 1, 1, 1, 1, 1, -1), -1, ttup_tl, True,
             err_msg10.format(-1)),
            ((1, 1, 1, 1, 1, 1, 1, 1, 1, 367), -1, ttup_tl, True,
             err_msg10.format(367)),
            # isdst (Daylight savings time)
            ((1, 1, 1, 1, 1, 1, 1, 1), -2, ttup_ts, True, err_msg11.format(-2)),
            ((1, 1, 1, 1, 1, 1, 1, 1), 2, ttup_ts, True, err_msg11.format(2)),
            # Proper tuple
            ([1, 1, 1, 1, 1, 1], -1, bad_t, True,
             err_msg12.format("<class 'list'>")),
            )
        msg = "Expected {}, with date {}. found {}."

        for date, dstflag, t_type, validity, expected_result in data:
            if t_type == bad_t:
                ttup = date
            elif t_type == ttup_l:
                ttup = _build_struct_time(_td_utils, date, dstflag)
            elif t_type == ttup_s:
                ttup = _build_struct_time(_td_utils, date, dstflag,
                                          short_in=True)
            else: # ttup_tl and ttup_ts
                ttup = date + (dstflag,)

            if validity: # Invalid tests
                try:
                    _td_utils._checktm(ttup)
                except (AssertionError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(
                        f"With date {date} an error was not raised, {e}")
            else: # Valid tests (Nothing to assert)
                _td_utils._checktm(ttup)

    #@unittest.skip("Temporarily skipped")
    def test_strftime(self):
        """
        Test that the strftime method returns the correct string.
        """
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')
        ttup_l, ttup_s, ttup_tl, ttup_ts = 1, 2, 3, 4
        data = (
            ('%a', (1, 1, 1, 1, 1, 0, 0, 0), -1, ttup_l, 'Idā'),
            ('%A', (1, 1, 1, 0, 0, 0), -1, ttup_s, '`Idāl'),
            ('%b', (1, 1, 1, 1, 1, 0, 0, 0, 1, 1), -1, ttup_tl, 'Bah'),
            ('%B', (1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_ts, 'Bahá'),
            ('%c', (1, 1, 1, 1, 1, 3, 1, 1), -1, ttup_l,
             'Idā Bah  1 03:01:01 1 01 01'),
            ('%c', (1, 1, 1, 3, 1, 1), -1, ttup_s, 'Idā Bah  1 03:01:01 0001'),
            


            )
        msg = "Expected {}, with format {} and date {}. found {}."

        for fmt, date, dstflag, t_type, expected_result in data:
            if t_type == ttup_l:
                ttup = _build_struct_time(_td_utils, date, dstflag)
            elif t_type == ttup_s:
                ttup = _build_struct_time(_td_utils, date, dstflag,
                                          short_in=True)
            else: # ttup_tl and ttup_ts
                ttup = date + (dstflag,)

            result = _td_utils.strftime(fmt, ttup)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, fmt, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_format(self):
        """
        Test that the _check_format method does not raise an exception
        with an invalid format.
        """
        err_msg0 = "Invalid format character '{}'"
        err_msg1 = "Found an empty format string."
        data = (
            ('%c', False, None),
            ('%X', False, None),
            ('%P', True, err_msg0.format('%P')),
            ('%-P', True, err_msg0.format('%-P')),
            ('%:P', True, err_msg0.format('%:P')),
            ('', True, err_msg1),
            )
        msg = "Expected {}, with {}. found {}."

        for fmt, validity, expected_result in data:
            if validity:
                try:
                    result = _td_utils._check_format(fmt)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{fmt}' an error is not "
                                         f"raised, with result {result}.")
            else:
                result = _td_utils._check_format(fmt)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, fmt, result))
