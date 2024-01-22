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
    """
    Some sunrise and sunset calculations done with my SunriseSunset package.

    In [1]: from sunrisesunset import SunriseSunset
    In [2]: import pytz
    In [3]: dt = datetime.datetime(1844, 3, 20)
    In [4]: zone = pytz.timezone('Asia/Tehran')
    In [5]: dt = dt.astimezone(zone)
    In [6]: ss = SunriseSunset(dt, 35.696111, 51.423056, 'official')
    In [7]: ss.sun_rise_set
    Out[7]:
    (datetime.datetime(1844, 3, 20, 6, 5, 51, 164827, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>),
     datetime.datetime(1844, 3, 20, 18, 11, 6, 983600, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>))
    In [8]: ss = SunriseSunset(dt, 35.696111, 51.423056, 'astronomical')
    In [9]: ss.sun_rise_set
    Out[9]:
    (datetime.datetime(1844, 3, 20, 4, 40, 42, 865539, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>),
     datetime.datetime(1844, 3, 20, 19, 36, 26, 469878, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>))
    In [10]: ss = SunriseSunset(dt, 35.6892523, 51.3896004, 'astronomical')
    In [11]: ss.sun_rise_set
    Out[11]:
    (datetime.datetime(1844, 3, 20, 4, 40, 51, 342324, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>),
     datetime.datetime(1844, 3, 20, 19, 36, 34, 35553, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>))
    """

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
        (the second) of the year Bahhāj (the eleventh), of the fifth  Vāhid,
        of the first Kull-i-Shay, of the Bahá’í Era.
        [major, cycle, year, month, day]
        """
        dt = datetime.datetime(1930, 4, 21)
        self._bc.parse_datetime(dt)
        result = self._bc.date_representation
        expected_result = (1, 5, 11, 2, 13)
        msg = f"Expected {expected_result}, found {result}"
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_bahai_sunset(self):
        """
        Test that the bahai_sunset method returns the universal time of
        sunset on fixed date in Bahai-Location.

        Baha'i epoc in fixed date is 673221 (1844-03-20) at 6:11 pm.
        (GMT+3:25:44) Sunset in Tehran, Tehran Province, Iran
        """
        fixed_date = 673221
        result = self._bc.bahai_sunset(fixed_date)
        expected_result = 673221.6100380344
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
        expected_result = 673222
        msg = f"Expected {expected_result}, found {result}"
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_astro_bahai(self):
        """
        Test that the fixed_from_astro_bahai method returns a fixed date
        of Baha’i date.
        """
        b_dates = (
            ((1, 10, 9, 19, 1), 738945),
            ((1, 1, 1, 1, 1), 673222),
            ((1, 10, 9, 17, 2), 0),
            )
        msg = "Expected result {}, found {}."

        for b_date, expected_result in b_dates:
            result = self._bc.fixed_from_astro_bahai(b_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_astro_bahai_from_fixed(self):
        """
        Test that the astro_bahai_from_fixed method returns the
        astronomical Baha’i date corresponding to fixed date.

        Baha'i epic date of 1844, March, 21 = 673222 in fixed date.
        """
        fixed_dates = (
            (673222, (1, 1, 1, 1, 1)),
            )
        msg = "Expected {}, found {}"

        for fixed_date, expected_result in fixed_dates:
            result = self._bc.astro_bahai_from_fixed(fixed_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))


