# -*- coding: utf-8 -*-
#
# badidatetime/test/test_timedateutils.py
#
__docformat__ = "restructuredtext en"

import os
import sys
import locale
import importlib
import unittest
from unittest.mock import patch, PropertyMock

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from .._timedateutils import _td_utils

datetime = importlib.import_module('badidatetime.datetime')


class TestTimeDateUtils(unittest.TestCase):
    """
    Notes on running these tests. If you live outside of the USA you may need
    to install the USA local info on your computer or some tests will fail.
    There is no way to test for changing local information when most tests
    are hard coded.
    """

    def __init__(self, name):
        super().__init__(name)

    @classmethod
    def setUpClass(cls):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    #@unittest.skip("Temporarily skipped")
    def test__order_format(self):
        """
        Test that the _order_format returns a correctly parsed date or
        time format.
        """
        data = (
            ('%m/%d/%y', '%m/%d/%y', ['/', 'm', 'd', 'y']),
            ('%H:%M:%S', '%H:%M:%S', [':', 'H', 'M', 'S']),
            ('', '%m/%d/%y', ['/', 'm', 'd', 'y']),
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
    @patch('badidatetime._timedateutils.TimeDateUtils.locale',
           new_callable=PropertyMock)
    def test_locale(self, mock_property):
        """
        Test that the locale property is set correctly.
        """
        mock_property.return_value = 'en_US.UTF-8'
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
    @patch('badidatetime._timedateutils.TimeDateUtils.date_format',
           new_callable=PropertyMock)
    def test_date_format(self, mock_property):
        """
        Test that the date_format property is set correctly
        """
        mock_property.return_value = ['/', 'm', 'd', 'Y']
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

        def make_ttup(t_type):
            if t_type == bad_t:
                ttup = date
            elif t_type == ttup_l:
                ttup = _td_utils._build_struct_time(date, dstflag)
            elif t_type == ttup_s:
                ttup = _td_utils._build_struct_time(date, dstflag,
                                                    short_in=True)
            else: # ttup_tl and ttup_ts
                ttup = date + (dstflag,)

            return ttup

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
        err_msg7 = "Invalid minute '{}', it must be in the range of [0, 59]."
        err_msg8 = "Invalid second '{}', it must be in the range of [0, 61]."
        err_msg9 = "Invalid week day '{}', it must be in the range of [0, 6]."
        err_msg10 = ("Invalid day '{}' in year, it must be in the range of "
                     "[1, 366].")
        err_msg11 = "Invalid isdst '{}', it must be in the range of [-1, 1]."
        err_msg12 = "The ttup argument {} is not a proper tuple."
        err_msg13 = "Invalid timetuple, found length {}."
        data = (
            ### Valid tuples
            ((MIN_K, 18, 1, 1, 1, 1, 1, 1), -1, ttup_l, False, ''),
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
            ((1, 1, 1, 1, -1, 0, 0, 0, 1, 1), -1, ttup_tl, True,
             err_msg5.format(-1, 1, 19)),
            ((1, 1, 1, 1, 20, 0, 0, 0, 1, 1), -1, ttup_tl, True,
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
            ((1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, True,
             err_msg13.format(13)),
            )

        for date, dstflag, t_type, validity, expected_result in data:
            if validity: # Invalid tests
                try:
                    ttup = make_ttup(t_type)
                    _td_utils._checktm(ttup)
                except (AssertionError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(
                        f"With date {date} an error was not raised,")
            else: # Valid tests (Nothing to assert)
                ttup = make_ttup(t_type)
                _td_utils._checktm(ttup)

    #@unittest.skip("Temporarily skipped")
    @patch('badidatetime._timedateutils.TimeDateUtils.date_format',
           new_callable=PropertyMock)
    def test_strftime(self, mock_property):
        """
        Test that the strftime method returns the correct string.
        """
        mock_property.return_value = ['/', 'm', 'd', 'Y']
        ttup_l, ttup_s, ttup_tl, ttup_ts = 1, 2, 3, 4
        data = (
            ('%a', (1, 1, 1, 1, 1, 0, 0, 0), -1, ttup_l, None, 'Fiḍ'),
            ('%A', (1, 1, 1, 0, 0, 0), -1, ttup_s, None, 'Fiḍāl'),
            ('%b', (1, 1, 1, 1, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, 'Bah'),
            ('%B', (1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_ts, None, 'Bahá'),
            ('%c', (1, 1, 1, 1, 1, 3, 1, 1), -1, ttup_l, None,
             'Fiḍ Bah  1 03:01:01 1 01 01'),
            ('%c', (1, 1, 1, 3, 1, 1), -1, ttup_s, None,
             'Fiḍ Bah  1 03:01:01 0001'),
            ('%C', (1, 10, 10, 1, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, '01'),
            ('%C', (181, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '01'),
            ('%d', (1, 1, 1, 1, 8, 0, 0, 0), -1, ttup_l, None, '08'),
            ('%-d', (1, 1, 8, 0, 0, 0), -1, ttup_s, None, '8'),
            ('%D', (1, 10, 10, 19, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None,
             '19/01/81'),
            ('%e', (181, 1, 9, 0, 0, 0, 1, 1), -1, ttup_ts, None, ' 9'),
            ('%f', (1, 10, 10, 1, 1, 0, 0, 60.025), -1, ttup_l, None, '025000'),
            ('%G', (181, 1, 1, 0, 0, 0), -1, ttup_s, None, '0181'),
            ('%G', (_td_utils.MINYEAR, 1, 1, 0, 0, 0), -1, ttup_s, None,
             '-1842'),
            ('%h', (1, 10, 10, 2, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, 'Jal'),
            ('%H', (181, 1, 1, 3, 0, 0, 1, 1), -1, ttup_ts, None, '03'),
            ('%-H', (1, 10, 10, 1, 1, 3, 0, 0), -1, ttup_l, None, '3'),
            ('%I', (181, 1, 1, 13, 0, 0), -1, ttup_s, None, '01'),
            ('%j', (1, 10, 10, 2, 1, 0, 0, 0), -1, ttup_l, None, '020'),
            ('%-j', (1, 10, 10, 2, 1, 0, 0, 0), -1, ttup_l, None, '20'),
            ('%k', (181, 1, 1, 3, 0, 0, 1, 1), -1, ttup_ts, None, '03'),
            ('%:K', (1, 10, 10, 1, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, '1'),
            ('%:K', (181, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '1'),
            ('%l', (1, 10, 10, 1, 1, 13, 0, 0), -1, ttup_l, None, ' 1'),
            ('%-l', (181, 1, 1, 13, 0, 0), -1, ttup_s, None, '1'),
            ('%m', (1, 1, 1, 9, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, '09'),
            ('%-m', (1, 9, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '9'),
            ('%M', (1, 10, 10, 1, 1, 0, 9, 0), -1, ttup_l, None, '09'),
            ('%-M', (181, 1, 1, 0, 9, 0), -1, ttup_s, None, '9'),
            ('%n', (1, 1, 1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, None, '\n'),
            ('%p', (181, 1, 1, 9, 0, 0, 1, 1), -1, ttup_ts, None, 'AM'),
            ('%p', (181, 1, 1, 19, 0, 0, 1, 1), -1, ttup_ts, None, 'PM'),
            ('%r', (1, 1, 1, 1, 1, 13, 5, 2), -1, ttup_l, None, '01:05:02 PM'),
            ('%S', (1, 1, 1, 0, 0, 5), -1, ttup_s, None, '05'),
            ('%-S', (1, 1, 1, 1, 1, 0, 0, 5, 1, 1), -1, ttup_tl, None, '5'),
            ('%T', (181, 1, 1, 3, 10, 5, 1, 1), -1, ttup_ts, None, '03:10:05'),
            ('%u', (1, 10, 10, 1, 1, 0, 0, 0), -1, ttup_l, None, '4'),
            ('%U', (1, 1, 1, 0, 0, 0), -1, ttup_s, None, '01'),
            ('%U', (181, 1, 10, 0, 0, 0), -1, ttup_s, None, '02'),
            ('%V', (1, 10, 10, 2, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, '04'),
            ('%:V', (1, 10, 10, 1, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, '10'),
            ('%:V', (181, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '10'),
            ('%W', (1, 10, 10, 1, 1, 0, 0, 0), -1, ttup_l, None, '01'),
            ('%x', (181, 1, 1, 0, 0, 0), -1, ttup_s, None, '01/01/0181'),
            ('%X', (1, 1, 1, 1, 1, 1, 1, 1, 1, 1), -1, ttup_tl, None,
             '01:01:01'),
            ('%y', (1, 10, 10, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '81'),
            ('%y', (181, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '81'),
            ('%-y', (1, 1, 1, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '1'),
            ('%-y', (1, 1, 1, 0, 0, 0, 1, 1), -1, ttup_ts, None, '1'),
            ('%Y', (181, 11, 17, 0, 0, 0), -1, ttup_s, None, '0181'),
            ('%z', (1, 1, 1, 1, 1, 13, 5, 2, 1, 1), -1, ttup_tl, None, ''),
            ('%z', (1, 1, 1, 1, 1, 13, 5, 2), -1, ttup_l, datetime.BADI,
             '+0350'),
            ('%z', (1, 1, 1, 13, 5, 2), -1, ttup_s, datetime.BADI, '+0350'),
            ('%:z', (181, 1, 1, 13, 5, 2), -1, ttup_s, datetime.BADI, '+03:50'),
            ('%Z', (1, 10, 10, 2, 1, 0, 0, 0, 1, 1), -1, ttup_tl, None, ''),
            ('%Z', (1, 10, 10, 2, 1, 0, 0, 0), -1, ttup_l, datetime.BADI,
             'UTC+03:30'),
            ('%%', (181, 1, 1, 13, 5, 2, 1, 1), -1, ttup_ts, None, '%'),
            # Some composit formats
            ('%d/%m/%Y, %H:%M:%S', (1, 10, 10, 1, 1, 12, 30, 30), -1, ttup_l,
             None, '01/01/0181, 12:30:30'),
            ('%B %A %r', (181, 11, 16, 18, 40, 59), -1, ttup_s, None,
             'Mashíyyat Istijlāl 06:40:59 PM'),
            )
        msg = "Expected {}, with format {} and date {}. found {}."

        for fmt, date, dstflag, t_type, tz, expected_result in data:
            if t_type == ttup_l:
                ttup = _td_utils._build_struct_time(date, dstflag, tzinfo=tz)
            elif t_type == ttup_s:
                ttup = _td_utils._build_struct_time(date, dstflag, tzinfo=tz,
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

    #@unittest.skip("Temporarily skipped")
    def test__find_midday(self):
        """
        Test that the _find_midday method returns the correct midday fraction
        of the day with either long or short form dates.
        """
        ttup_l, ttup_s = 1, 2
        data = (
            ((1, 10, 10, 11, 17, 9, 30, 30), -1, ttup_l, 0.49919999996200204),
            ((182, 1, 1, 18, 30, 30), -1, ttup_s, 0.5005864999257028),
            )
        msg = "Expected {}, with {}. found {}."

        for date, dstflag, t_type, expected_result in data:
            if t_type == ttup_l:
                ttup = _td_utils._build_struct_time(date, dstflag)
            else: # t_type == ttup_s
                ttup = _td_utils._build_struct_time(date, dstflag,
                                                    short_in=True)

            result = _td_utils._find_midday(ttup)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__get_year(self):
        """
        Test that the _get_year returns the year converted from a long
        form date or the year from a short date.
        """
        ttup_l, ttup_s = 1, 2
        data = (
            ((1, 10, 10, 11, 17, 9, 30, 30), -1, ttup_l, 181),
            ((182, 1, 1, 18, 30, 30), -1, ttup_s, 182),
            )
        msg = "Expected {}, with {}. found {}."

        for date, dstflag, t_type, expected_result in data:
            if t_type == ttup_l:
                ttup = _td_utils._build_struct_time(date, dstflag)
            else: # t_type == ttup_s
                ttup = _td_utils._build_struct_time(date, dstflag,
                                                    short_in=True)

            result = _td_utils._get_year(ttup)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__year_week_day(self):
        """
        Test that the _year_week_day return the year, week, and day of
        the week.
        """
        data = (
            # year, mon, day -> year, week, day of week
            ((181, 11, 17),    (181, 30, 7)),
            ((182, 1, 1),      (181, 53, 5)),
            ((183, 19, 19),    (183, 52, 7)),
            # Last week of a leap year where the beginning of the year
            # started on Fiḍāl.
            ((6, 19, 19),      (6, 53, 4)),
            ((12, 19, 19),     (12, 53, 4)),
            # Year where the first day of the year starts on Kamál and
            # is a leap year.
            ((174, 19, 17),    (175, 1, 1)),
            ((174, 19, 18),    (175, 1, 2)),
            ((174, 19, 19),    (175, 1, 3)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = _td_utils._year_week_day(*date)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

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
            result = _td_utils._days_before_year(year)
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
            result = _td_utils._days_in_month(year, month)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, month, result))

    #@unittest.skip("Temporarily skipped")
    def test__days_before_month(self):
        """
        Test that the _days_before_month function returns the correct
        number of days in the year proceedings the first day in the correct
        month.
        """
        data = (
            (181, 1, 0),
            (181, 2, 19),
            (181, 3, 38),
            (181, 4, 57),
            (181, 5, 76),
            (181, 6, 95),
            (181, 7, 114),
            (181, 8, 133),
            (181, 9, 152),
            (181, 10, 171),
            (181, 11, 190),
            (181, 12, 209),
            (181, 13, 228),
            (181, 14, 247),
            (181, 15, 266),
            (181, 16, 285),
            (181, 17, 304),
            (181, 18, 323),
            (181, 0, 342),
            (181, 19, 346),
            (183, 1, 0), # 182 is a leap year
            )
        msg = "Expected {} with year {} and month {}, found {}."

        for year, month, expected_result in data:
            result = _td_utils._days_before_month(year, month)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, year, month, result))

    #@unittest.skip("Temporarily skipped")
    def test__day_of_week(self):
        """
        Test that the _day_of_week function returns the correct day of
        the week (0 - 6) for a given year, month, and day.

        All days before 1752-09-14 (-91, 10, 8, 5, 46, 17.1264) in the
        Gregorian Calendar will seem wrong when compared to the Badi
        Calendar in UK and the US. This is when the Gregorian Calendar was
        adopted and compensated 11 days.
        """
        data = (
            ((-1842, 1, 1), 2), # 0001-03-19 (Saturday -> Jamál)
            ((-91, 9, 15), 6),  # 1752-09-02 (Wednesday -> `Idāl)
            ((-91, 10, 8), 4),  # 1752-09-14 (Thursday -> Istijlāl)
            ((1, 1, 1), 3),     # 1844-03-19 (Tuesday -> Fiḍāl)
            ((181, 9, 9), 2),   # 2024-08-26 (Monday -> Kamál)
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = _td_utils._day_of_week(*date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__ymd2ord(self):
        """
        Test that the _ymd2ord function returns the correct number of days
        since Badi year -1842 including the current day.
        """
        data = (
            ((-1842, 1, 1), 78),
            ((-1841, 1, 1), 444),
            ((181, 1, 1), 738964),
            # 1st week of 181
            ((180, 19, 17), 738961),
            ((180, 19, 18), 738962),
            ((180, 19, 19), 738963),
            ((181, 1, 1), 738964),
            ((181, 1, 2), 738965),
            ((181, 1, 3), 738966),
            ((181, 1, 4), 738967),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = _td_utils._ymd2ord(*date)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__ord2ymd(self):
        """
        Test that the _ord2ymd function returns the year, month, and day
        from the Badi year -1842.
        """
        data = (
            (78, False, (-5, 18, 1, 1, 1)),
            (78, True, (-1842, 1, 1)),
            (444, True, (-1841, 1, 1)),
            (673219, True, (0, 19, 19)),
            (673220, True, (1, 1, 1)),
            (738964, True, (181, 1, 1)),
            # 1st week of 181
            (738961, True, (180, 19, 17)),
            (738962, True, (180, 19, 18)),
            (738963, True, (180, 19, 19)),
            (738964, True, (181, 1, 1)),
            (738965, True, (181, 1, 2)),
            (738966, True, (181, 1, 3)),
            (738967, True, (181, 1, 4)),
            (739100, True, (181, 8, 4)),
            )
        msg = "Expected {} with ordinal {} and short {}, found {}."

        for ordinal, short, expected_result in data:
            result = _td_utils._ord2ymd(ordinal, short=short)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, ordinal, short, result))

    #@unittest.skip("Temporarily skipped")
    def test__build_struct_time(self):
        """
        Test that the _build_struct_time correctly builds a timetuple object.
        """
        data = (
            ((181, 9, 6, 8, 45, 1), -1, None, True,
             ("structures.ShortFormStruct(tm_year=181, tm_mon=9, tm_mday=6, "
              "tm_hour=8, tm_min=45, tm_sec=1, tm_wday=6, tm_yday=158, "
              "tm_isdst=-1)", None, None)),
            ((1, 1, 1, 0, 0, 0), -1, datetime.BADI, True,
             ("structures.ShortFormStruct(tm_year=1, tm_mon=1, tm_mday=1, "
              "tm_hour=0, tm_min=0, tm_sec=0, tm_wday=3, tm_yday=1, "
              "tm_isdst=-1)", 'UTC+03:30', 12600.0)),
            ((1, 10, 10, 9, 6, 8, 45, 1), -1, None, False,
             ("structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, "
              "tm_year=10, tm_mon=9, tm_mday=6, tm_hour=8, tm_min=45, "
              "tm_sec=1, tm_wday=6, tm_yday=158, tm_isdst=-1)", None, None)),
            ((1, 1, 1, 0, 0, 0), -1, datetime.BADI, True,
             ('structures.ShortFormStruct(tm_year=1, tm_mon=1, tm_mday=1, '
              'tm_hour=0, tm_min=0, tm_sec=0, tm_wday=3, tm_yday=1, '
              'tm_isdst=-1)', 'UTC+03:30', 12600.0)),
            )
        msg0 = "Expected {} with dt {}, dst {}, and timezone {}, found {}."
        msg1 = "Expected {}, found {}."

        for dt, dst, tz, short_in, expected_result in data:
            result = _td_utils._build_struct_time(dt, dst, tzinfo=tz,
                                                  short_in=short_in)
            self.assertEqual(expected_result[0], str(result), msg0.format(
                expected_result, dt, dst, tz, result))
            self.assertEqual(expected_result[1], result.tm_zone,
                             msg1.format(expected_result[1], result.tm_zone))
            self.assertEqual(expected_result[2], result.tm_gmtoff,
                             msg1.format(expected_result[2], result.tm_gmtoff))

    #@unittest.skip("Temporarily skipped")
    def test__isoweek_to_badi(self):
        """
        Test that the _isoweek_to_badi function returns the year, month,
        and day for the beginning of the week either before or after
        depending on what day in the week the day falls.
        """
        err_msg0 = "Invalid week: {}"
        err_msg1 = "Invalid weekday: {} (range is [1, 7])"
        data = (
            # year  week day
            ((-1842,  1,  1), False, False, (-5, 17, 19, 19, 18)),
            ((-1842,  1,  1), True,  False, (-1843, 19, 18)),
            ((    1, 52,  1), True, False, (1, 19, 15)),
            ((   46, 52,  1), True, False, (46, 19, 15)),
            ((  175, 52,  1), True,  False, (175, 19, 16)),
            ((  176, 52,  1), True,  False, (176, 19, 15)),
            ((  181,  1,  1), True,  False, (180, 19, 17)),
            ((  181,  1,  7), True,  False, (181, 1, 4)),
            ((  181, 20,  7), True,  False, (181, 8, 4)),
            ((  181, 52,  1), True,  False, (181, 19, 16)),
            ((  181, 52,  7), True,  False, (182, 1, 3)),
            ((  182, 52,  1), True,  False, (182, 19, 14)),
            ((  183,  1,  1), True,  False, (183, 1, 2)),
            ((  187, 52,  1), True, False, (187, 19, 15)),
            ((  192, 52,  1), True, False, (192, 19, 16)),
            ((  193, 52,  1), True, False, (193, 19, 15)),
            ((  194, 52,  1), True, False, (194, 19, 14)),
            ((  198, 52,  1), True, False, (198, 19, 16)),
            ((  199, 52,  1), True, False, (199, 19, 14)),
            ((  203, 52,  1), True, False, (203, 19, 16)),
            ((  204, 52,  1), True, False, (204, 19, 15)),
            ((  209, 52,  1), True, False, (209, 19, 16)),
            ((  210, 52,  1), True, False, (210, 19, 15)),
            ((  215, 52,  1), True, False, (215, 19, 16)),
            ((  216, 52,  1), True, False, (216, 19, 14)),
            ((  181, 53,  1), True, True, err_msg0.format(53)),
            ((  181, 20, 10), False, True, err_msg1.format(10)),
            )
        msg = "Expected {} with (year, week, day) {}, found {}."

        for item, short, validity, expected_result in data:
            if validity:
                with self.assertRaises(AssertionError) as cm:
                    _td_utils._isoweek_to_badi(*item)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                result = _td_utils._isoweek_to_badi(*item, short=short)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, item, result))

    #@unittest.skip("Temporarily skipped")
    def test__isoweek1jalal(self):
        """
        Test that the _isoweek1jalal function returns the ordinal day number
        of the first week with more than 3 days in it.
        """
        data = (
            (  1, 673217), # 1844-03-16
            (181, 738961), # 2024-03-16
            (182, 739332), # 2025-03-22
            (183, 739696), # 2026-03-21
            )
        msg = "Expected {} with year {}, found {}."

        for year, expected_result in data:
            result = _td_utils._isoweek1jalal(year)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_date_time_timezone(self):
        """
        Test that the _parse_isoformat_date_time_timezone function returns a
        parsed date and time ISO string.
        """
        data = (
            ('-18420101T120000', (-1842, 1, 1, 12, 0, 0), 'None'),
            ('-1842-01-01T12:00:00', (-1842, 1, 1, 12, 0, 0), 'None'),
            ('11610101T120000', (1161, 1, 1, 12, 0, 0), 'None'),
            ('1161-01-01T12:00:00', (1161, 1, 1, 12, 0, 0), 'None'),
            #('0181-W20T12:00:00', (181, 8, 17, 12, 0, 0), 'None'),
            ('0181-W20-5T12:00:00', (181, 8, 2, 12, 0, 0), 'None'),
            ('0001-01-01B', (1, 1, 1), 'UTC+03:30'),
            ('0001-01-01T00:00:00.0+03:30', (1, 1, 1, 0, 0, 0), 'UTC+03:30'),
            ('-0126-16-02T07:58:31.4976Z', (-126, 16, 2, 7, 58, 31.4976),
             'UTC'),
            ('0181-13-09+02', (181, 13, 9), 'UTC+02:00'),
            ('0181-13-09-05', (181, 13, 9), 'UTC-05:00'),
            )
        msg = "Expected {} with dtstr {}, found {}."

        for dtstr, expected_result0, expected_result1 in data:
            date, time, tz = _td_utils._parse_isoformat_date_time_timezone(
                dtstr)
            self.assertEqual(expected_result0, date + time, msg.format(
                expected_result0, dtstr, date + time))
            self.assertEqual(expected_result1, str(tz), msg.format(
                expected_result1, dtstr, tz))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_date(self):
        """
        Test that the _parse_isoformat_date function parses the date
        correctly from ISO standard formats.
        """
        err_msg0 = "Invalid character {} in incoming date string."
        err_msg1 = "Year is out of range: {}, min {}, max {}."
        err_msg2 = ("Invalid format, there must be between 0 to 2 hyphens "
                    "(-) in the date format or there can be one uppercase "
                    "(W) week identifier and between 0 and 2 hyphens (-) "
                    "used.")
        err_msg3 = "Invalid ISO string {}."
        data = (
            ('', False, ()),
            ('0181-01', False, (181, 1, 1)),
            ('01810101', False, (181, 1, 1)),
            ('0181-01-01', False, (181, 1, 1)),
            ('0181W01', False, (180, 19, 17)),
            ('0181-W01', False, (180, 19, 17)),
            ('0181-W01-1', False, (180, 19, 17)),
            ('0181-W01-2', False, (180, 19, 18)),
            ('0181-W01-3', False, (180, 19, 19)),
            ('0181-W01-4', False, (181, 1, 1)),
            ('0181-W01-5', False, (181, 1, 2)),
            ('0181-W01-6', False, (181, 1, 3)),
            ('0181-W01-7', False, (181, 1, 4)),
            ('0181W017', False, (181, 1, 4)),
            ('0181W20', False, (181, 7, 17)),
            ('0181W207', False, (181, 8, 4)),
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
            ('0001/01/01', True, err_msg0.format("'/'")),
            ('015s', True, err_msg0.format("'s'")),
            ('-1843-01', True, err_msg1.format(-1843, _td_utils.MINYEAR,
                                               _td_utils.MAXYEAR)),
            ('1162-01', True, err_msg1.format(1162, _td_utils.MINYEAR,
                                              _td_utils.MAXYEAR)),
            ('0181-01-01-', True, err_msg2),
            ('0181-W10-1-', True, err_msg2),
            ('0181-W101', True, err_msg3.format('0181-W101')),
            )
        msg = "Expected {} with ISO date {}, found {}."

        for date, validity, expected_result in data:
            if validity:
                try:
                    result = _td_utils._parse_isoformat_date(date)
                except (AssertionError, ValueError, IndexError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                result = _td_utils._parse_isoformat_date(date)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_time(self):
        """
        Test that the _parse_isoformat_time function parses the time
        correctly from ISO standard formats.
        """
        err_msg0 = "Invalid character {} in incoming time string."
        err_msg1 = ("Cannot have both a 'T' and a space or more than one "
                    "of either to indicate time.")
        err_msg2 = ("Invalid time string, 1st character must be one of ( T), "
                    "found {}")
        err_msg3 = "Invalid number of colons (:), can be 0 - 2, found {}"
        err_msg4 = "Invalid number of dots (.), can be 0 - 1, found {}"
        err_msg5 = "Invalid time string, found {}"
        data = (
            ('', False, ()),
            ('T14', False, (14, 0, 0)),
            (' 14', False, (14, 0, 0)),
            ('T14.2', False, (14, 12, 0)),
            ('T1412', False, (14, 12, 0)),
            ('T1412.45', False, (14, 12, 27)),
            ('T141232', False, (14, 12, 32)),
            ('T141232.029', False, (14, 12, 32.029)),
            ('T14:12', False, (14, 12, 0)),
            ('T14:12.45', False, (14, 12, 27)),
            ('T14:12:32', False, (14, 12, 32)),
            ('T14:12:32.029', False, (14, 12, 32.029)),
            ('S01:01:01', True, err_msg0.format("'S'")),
            ('T ', True, err_msg1),
            ('TT', True, err_msg1),
            ('  ', True, err_msg1),
            ('01:01:01', True, err_msg2.format("'01:01:01'")),
            ('T:::', True, err_msg3.format(3)),
            ('T..', True, err_msg4.format(2)),
            ('T014.2', True, err_msg5.format("'T014.2'")),
            )
        msg = "Expected {} with ISO time {}, found {}."

        for time, validity, expected_result in data:
            if validity:
                try:
                    result = _td_utils._parse_isoformat_time(time)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = _td_utils._parse_isoformat_time(time)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, time, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_isoformat_timezone(self):
        """
        Test that the _parse_isoformat_timezone
        """
        err_msg0 = "Invalid character {} in incoming timezone string."
        err_msg1 = ("Can only have one of (-+Z) and no more than one of (-+Z) "
                    "to indicate a timezone.")
        err_msg2 = ("Invalid timezone string, 1st character must be one of "
                    "(-+Z), found {}")
        err_msg3 = "Invalid number of colons (:), can be 0 - 1, found {}"
        data = (
            ('', False, 'None'),
            ('Z', False, 'UTC'),
            ('B', False, 'UTC+03:30'),
            ('+05', False, 'UTC+05:00'),
            ('-05', False, 'UTC-05:00'),
            ('A', True, err_msg0.format("'A'")),
            ('-+0', True, err_msg1),
            ('--0', True, err_msg1),
            ('03:30', True, err_msg2.format("'03:30'")),
            ('-05:30:30', True, err_msg3.format(2)),
            )
        msg = "Expected {} with ISO timezone {}, found {}."

        for timezone, validity, expected_result in data:
            if validity:
                try:
                    result = _td_utils._parse_isoformat_timezone(timezone)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {timezone} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = _td_utils._parse_isoformat_timezone(timezone)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, timezone, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_date_fields(self):
        """
        Test that the _check_date_fields function correctly raises
        assertion exceptions.

        A more complete test is in badidatetime/tests/test_badi_calendar.py.
        """
        err_msg0 = ("Invalid Váḥids '{}' in a Kull-i-Shay’, it must be in "
                    "the range of [1, 19].")
        err_msg1 = ("Invalid year '{}' in a Váḥid, it must be in the range "
                    "of [1, 19].")
        err_msg2 = "Invalid month '{}', it must be in the range of [0, 19]."
        err_msg3 = ("Invalid day '{}' for month '{}', it must be in the "
                    "range of [1, {}].")
        data = (
            # Valid short form Badi dates
            ((_td_utils.MINYEAR, 1, 1), False, ''),
            ((_td_utils.MAXYEAR, 1, 1), False, ''),
            # Invalid Váḥid
            ((1, 0, 1, 1, 1), True, err_msg0.format(0)),
            ((1, 20, 1, 1, 1), True, err_msg0.format(20)),
            # Invalid year
            ((1, 10, 0, 1, 1), True, err_msg1.format(0)),
            ((1, 10, 20, 1, 1), True, err_msg1.format(20)),
            # Invalid month
            ((1, 10, 10, -1, 1), True, err_msg2.format(-1)),
            ((1, 10, 10, 20, 1), True, err_msg2.format(20)),
            # Invalid Ayyám-i-Há day
            ((1, 10, 3, 0, 0), True, err_msg3.format(0, 0, 5)),
            ((1, 10, 3, 0, 6), True, err_msg3.format(6, 0, 5)),
            # Invalid normal day
            ((1, 10, 3, 2, 0), True, err_msg3.format(0, 2, 19)),
            ((1, 10, 3, 2, 20), True, err_msg3.format(20, 2, 19)),
            # Test short form date.
            ((181, 20, 1), True, err_msg2.format(20)),
            )

        for date, validity, err_msg in data:
            short_in = False if len(date) == 5 else True

            if validity:
                try:
                    _td_utils._check_date_fields(*date, short_in=short_in)
                except AssertionError as e:
                    self.assertEqual(err_msg, str(e))
                else:
                    raise AssertionError(f"date {date}")
            else:
                _td_utils._check_date_fields(*date, short_in=short_in)

    #@unittest.skip("Temporarily skipped")
    def test__check_time_fields(self):
        """
        Test that the _check_time_fields function correctly raises
        assertion exceptions.
        """
        err_msg0 = "Invalid hour '{}', it must be in the range of [0, 24]."
        err_msg1 = "Invalid minute '{}', it must be in the range of [0, 59]."
        err_msg2 = "Invalid second '{}', it must be in the range of [0, 61]."
        err_msg3 = ("Invalid microseconds '{}', it must be in the range of "
                    "[0, 999999].")
        err_msg4 = "The fold argument '{}' must be either 0 or 1."
        data = (
            ((0, 0, 0, 0, 0), False, ''),
            ((24, 59, 59, 999999, 1), False, ''),
            ((-1, 0, 0, 0, 0), True, err_msg0.format(-1)),
            ((25, 0, 0, 0, 0), True, err_msg0.format(25)),
            ((0, -1, 0, 0, 0), True, err_msg1.format(-1)),
            ((0, 60, 0, 0, 0), True, err_msg1.format(60)),
            ((0, 0, -1, 0, 0), True, err_msg2.format(-1)),
            ((0, 0, 61, 0, 0), True, err_msg2.format(61)),
            ((0, 0, 0, -1, 0), True, err_msg3.format(-1)),
            ((0, 0, 0, 1000000, 0), True, err_msg3.format(1000000)),
            ((0, 0, 0, 0, -1), True, err_msg4.format(-1)),
            ((0, 0, 0, 0, 2), True, err_msg4.format(2)),
            )
        for time, validity, err_msg in data:
            if validity:
                try:
                    _td_utils._check_time_fields(*time)
                except AssertionError as e:
                    self.assertEqual(err_msg, str(e))
                else:
                    raise AssertionError(f"time {time}")
            else:
                _td_utils._check_time_fields(*time)

    #@unittest.skip("Temporarily skipped")
    def test__wrap_strftime(self):
        """
        Test that the _wrap_strftime function returns a formatted time
        string.
        """
        d_t, dt_t = 1, 2
        data = (
            ((181, 1, 1), '%d/%m/%Y, %H:%M:%S', d_t, '01/01/0181, 00:00:00'),
            ((1, 1, 8), '%-d', d_t, '8'),
            #((181, 1, 1, 0, 9, 0), dt_t, '%-M', ''),
            )
        msg = "Expected {} with date {} and format {}, found {}."

        for date, fmt, obj_t, expected_result in data:
            if obj_t == d_t:
                d = datetime.datetime(*date)
            else:
                d = datetime(*date)

            tt = d.timetuple()
            result = _td_utils._wrap_strftime(date, fmt, tt)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, format, result))
