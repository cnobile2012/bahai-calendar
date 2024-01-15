# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_badi_calandar.py
#
__docformat__ = "restructuredtext en"

import os
import unittest
import datetime

from ..badi_calendar import BahaiCalendar


class TestBadiCalandar(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._bc = BahaiCalendar()

    #@unittest.skip("Temporarily skipped")
    def test_parse_datetime(self):
        """
        Test that the parse_datetime method creates the baha'i date
        representation.

        Thus, for example, Monday, April 21, 1930 would be called “Kamāl
        (Monday), the day of Qudrat (the thirteenth), of the month of Jalāl,
        of the year Bahhāj (the eleventh), of the fifth  Vāhid, of the first
        Kull-i-Shay, of the Bahá’í Era. [major, cycle, year, month, day]
        """
        dt = datetime.datetime(1930, 4, 21)
        self._bc.parse_datetime(dt)
        result = self._bc.date_representation
        expected_result = (1, 5, 11, 2, 3)
        msg = f"Expected {expected_result}, found {result}"
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_bahai_sunset(self):
        """
        Test that the bahai_sunset method returns the universal time of
        sunset on fixed date in Bahai-Location.

        Baha'i epoc in fixed date is 673222.
        """
        fixed_date = 673222
        result = self._bc.bahai_sunset(fixed_date)
        expected_result = 673222.6070645516
        msg = f"Expected {expected_result}, found {result}"
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_astro_bahai_new_year_on_or_before(self):
        """
        Test that the astro_bahai_new_year_on_or_before method returns a
        fixed date of astronomical Bahai New Year on or before fixed date.

        Baha'i epoc in fixed date is 673222.
        """
        fixed_date = 673222
        result = self._bc.astro_bahai_new_year_on_or_before(fixed_date)
        expected_result = 673024
        msg = f"Expected {expected_result}, found {result}"
        self.assertEqual(expected_result, result, msg)

    @unittest.skip("Temporarily skipped")
    def test_fixed_from_astro_bahai(self):
        """
        Test that the fixed_from_astro_bahai method returns a fixed date
        of Baha’i date.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_astro_bahai_from_fixed(self):
        """
        Test that the astro_bahai_from_fixed method returns the
        astronomical Baha’i date corresponding to fixed date.

        Baha'i epic date of 1844, March, 21 = 673222 in fixed date.
        """
        fixed_date = 673222
        result = self._bc.astro_bahai_from_fixed(fixed_date)
        expected_result = (1, 1, 1, 1, 1)
        msg = f"Expected {expected_result}, found {result}"
        self.assertEqual(expected_result, result, msg)


