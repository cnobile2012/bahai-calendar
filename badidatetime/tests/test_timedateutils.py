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

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._bc = BahaiCalendar()

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

    @unittest.skip("Temporarily skipped")
    def test_strftime(self):
        """
        Test that the 
        """

    #@unittest.skip("Temporarily skipped")
    def test__check_format(self):
        """
        Test that the _check_format method does not raise an exception
        with an invalid format.
        """
        err_msg0 = "Illegal format character '{}'"
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

    #@unittest.skip("Temporarily skipped")
    def test__checktm(self):
        """
        Test that the _checktm method does not raise an exception with
        an invalid tupple type.
        """
        ttup_l, ttup_s, ttup_tl, ttup_ts = 1, 2, 3, 4
        data = (
            # Valid tuples
            ((1, 10, 10, 9, 6, 8, 45, 1), -1, ttup_l, False, ''),
            ((181, 9, 6, 8, 45, 1), -1, ttup_s, False, ''),
            )
        msg = "Expected {}, with date {}. found {}."

        for date, dstflag, t_type, validity, expected_result in data:
            if t_type == ttup_l:
                ttup = _build_struct_time(self._bc, date, dstflag)
            elif t_type == ttup_s:
                ttup = _build_struct_time(self._bc, date, dstflag,
                                          short_in=True)
            else: # ttup_tl and ttup_ts
                ttup = date + (dstflag,)

            if validity: # Invalid tests
                try:
                    with self.assertRaises(AssertionError) as cm:
                        _td_utils._checktm(ttup)
                except AssertionError as e:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(
                        f"With date {date} and error was not raised, {e}")
                else:
                    message = str(cm.exception)
                    self.assertEqual(err_msg, message)
            else: # Valid tests
                _td_utils._checktm(ttup)
