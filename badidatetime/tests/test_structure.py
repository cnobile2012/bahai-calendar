# -*- coding: utf-8 -*-
#
# badidatetime/test/test_structures.py
#
__docformat__ = "restructuredtext en"

import unittest

from ..structures import struct_time


class TestStructures(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        pass

    #@unittest.skip("Temporarily skipped")
    def test_struct_time_short(self):
        """
        Test that the struct_time class can properly store short form
        Badi dates and times.
        """
        data = (
            ((181, 9, 6, 8, 45, 1, 1, 158, -1),
             ("ShortFormStruct(tm_year=181, tm_month=9, tm_day=6, tm_hour=8, "
              "tm_min=45, tm_sec=1, tm_wday=1, tm_yday=158, tm_isdst=1)",
              'EDT', -14400)),
            ((1, 10, 10, 9, 6, 8, 45, 1, 1, 158, -1),
             ("LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, tm_year=10, "
              "tm_month=9, tm_day=6, tm_hour=8, tm_min=45, tm_sec=1, "
              "tm_wday=1, tm_yday=158, tm_isdst=1)",
              'EDT', -14400)),
            )
        msg0 = "Expected {}, with dt {}, found {}."
        msg1 = "Expected {}, fount {}."

        for dt, expected_result in data:
            result = struct_time(dt)
            self.assertEqual(expected_result[0], str(result),
                             msg0.format(expected_result[0], dt, result))
            self.assertEqual(expected_result[1], result.tm_zone,
                             msg1.format(expected_result[1], result.tm_zone))
            self.assertEqual(expected_result[2], result.tm_gmtoff,
                             msg1.format(expected_result[2], result.tm_gmtoff))


