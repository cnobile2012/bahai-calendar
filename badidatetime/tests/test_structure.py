# -*- coding: utf-8 -*-
#
# badidatetime/test/test_structures.py
#
__docformat__ = "restructuredtext en"

import unittest
import importlib

from .._structures import struct_time
#from badidatetime import datetime
from zoneinfo import ZoneInfo

datetime = importlib.import_module('badidatetime.datetime')


class TestStructures(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test_struct_time(self):
        """
        Test that the struct_time class can properly store short form
        Badi dates and times.
        """
        err_msg0 = "struct_time() takes a 9 or 11-sequence ({}-sequence given)"
        err_msg1 = "Invalid isdst '{}', it must be in the range of [-1, 1]."
        tz0 = ZoneInfo('US/Eastern')
        data = (
            ((181, 9, 6, 8, 45, 1, 0, 0, -1), None, False,
             ("structures.ShortFormStruct(tm_year=181, tm_mon=9, tm_mday=6, "
              "tm_hour=8, tm_min=45, tm_sec=1, tm_wday=0, tm_yday=0, "
              "tm_isdst=-1)", None, None)),
            ((1, 10, 10, 9, 6, 8, 45, 1, 0, 0, -1), None, False,
             ("structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, "
              "tm_year=10, tm_mon=9, tm_mday=6, tm_hour=8, tm_min=45, "
              "tm_sec=1, tm_wday=0, tm_yday=0, tm_isdst=-1)", None, None)),
            ((1, 1, 1, 0, 0, 0, 0, 0, -1), datetime.BADI, False,
             ('structures.ShortFormStruct(tm_year=1, tm_mon=1, tm_mday=1, '
              'tm_hour=0, tm_min=0, tm_sec=0, tm_wday=0, tm_yday=0, '
              'tm_isdst=-1)', 'UTC+03:30', 12600.0)),
            ((181, 9, 6, 8, 45, 1, 0, 0, -1), tz0, False,
             ('structures.ShortFormStruct(tm_year=181, tm_mon=9, tm_mday=6, '
              'tm_hour=8, tm_min=45, tm_sec=1, tm_wday=0, tm_yday=0, '
              'tm_isdst=1)', 'US/Eastern', -14400)),
            ((181, 15, 13, 0, 0, 0, 0, 0, -1), tz0, False,
             ('structures.ShortFormStruct(tm_year=181, tm_mon=15, tm_mday=13, '
              'tm_hour=0, tm_min=0, tm_sec=0, tm_wday=0, tm_yday=0, '
              'tm_isdst=0)', 'US/Eastern', -18000)),
            # Errors
            ((181, 9, 6, 8, 45, 1, 0, 0, -1, 999), None, True,
             err_msg0.format(10)),
            ((1, 1, 1, 1, 1, 1, 1, 1, -2), None, True, err_msg1.format(-2)),
            ((1, 1, 1, 1, 1, 1, 1, 1, 2), None, True, err_msg1.format(2)),
            )
        msg0 = "Expected {}, with dt {}, found {}."
        msg1 = "Expected {}, found {}."

        for dt, tz, validity, expected_result in data:
            if validity:
                try:
                    result = struct_time(dt, tzinfo=tz)
                except (AssertionError, TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {dt} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = struct_time(dt, tzinfo=tz)
                self.assertEqual(expected_result[0], str(result), msg0.format(
                    expected_result[0], dt, result))
                self.assertEqual(expected_result[1], result.tm_zone,
                                 msg1.format(expected_result[1],
                                             result.tm_zone))
                self.assertEqual(expected_result[2], result.tm_gmtoff,
                                 msg1.format(expected_result[2],
                                             result.tm_gmtoff))

    #@unittest.skip("Temporarily skipped")
    def test_short(self):
        """
        Test that the short property returns the correct boolean.
        """
        data = (
            ((181, 9, 6, 8, 45, 1, 0, 0, -1), True),
            ((1, 10, 10, 9, 6, 8, 45, 1, 1, 0, -1), False),
            )
        msg = "Expected {}, with date {}, found {}."

        for date, expected_result in data:
            result = struct_time(date).short
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))
