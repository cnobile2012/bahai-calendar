# -*- coding: utf-8 -*-
#
# badidatetime/test/test_structures.py
#
__docformat__ = "restructuredtext en"

import unittest

from .._structures import struct_time
from ..datetime import _build_struct_time


class TestStructures(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test_struct_time(self):
        """
        Test that the struct_time class can properly store short form
        Badi dates and times.
        """
        err_msg0 = ("Invalid value for tm_isdst, found {}, should be "
                    "one of (-1, 0, 1).")
        err_msg1 = "struct_time() takes a 9 or 11-sequence ({}-sequence given)"
        err_msg2 = "Invalid Váḥid must be 1 to 19, found {}."
        err_msg3 = "Invalid year must be 1 to 19, found {}."
        err_msg4 = "Invalid month must be 0 to 19, found {}"
        data = (
            ((181, 9, 6, 8, 45, 1, 0, 0, -1), False,
             ("structures.ShortFormStruct(tm_year=181, tm_mon=9, tm_mday=6, "
              "tm_hour=8, tm_min=45, tm_sec=1, tm_wday=0, tm_yday=0, "
              "tm_isdst=-1)", None, None)),
            ((1, 10, 10, 9, 6, 8, 45, 1, 0, 0, -1), False,
             ("structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, "
              "tm_year=10, tm_mon=9, tm_mday=6, tm_hour=8, tm_min=45, "
              "tm_sec=1, tm_wday=0, tm_yday=0, tm_isdst=-1)", None, None)),
            ((181, 9, 6, 8, 45, 1, 0, 0, 10), True, err_msg0.format(10)),
            ((181, 9, 6, 8, 45, 1, 0, 0, -1, 999), True, err_msg1.format(10)),
            ((1, 0, 10, 9, 6, 8, 45, 1, 0, 0, -1), True, err_msg2.format(0)),
            ((1, 10, 0, 9, 6, 8, 45, 1, 0, 0, -1), True, err_msg3.format(0)),
            ((181, 20, 6, 8, 45, 1, 0, 0, -1), True, err_msg4.format(20)),
            ((1, 10, 10, 20, 6, 8, 45, 1, 0, 0, -1), True, err_msg4.format(20)),
            )
        msg0 = "Expected {}, with dt {}, found {}."
        msg1 = "Expected {}, fount {}."

        for dt, validity, expected_result in data:
            short = True if len(dt) == 7 else False

            if validity:
                try:
                    result = struct_time(dt)(dt)
                except AssertionError as e:
                    self.assertEqual(expected_result, str(e))
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = struct_time(dt)
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
