# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_badi_calandar.py
#
__docformat__ = "restructuredtext en"

import os
import unittest
import datetime

from ..badi_calendar import BahaiCalendar, GregorianCalendar


class TestBadiCalandar(unittest.TestCase):
    """
    Some sunrise and sunset calculations done with my SunriseSunset package.
    Sunrise and Sunset for 1844-03

    The vernal equinox in Tehran was at 15:19 on Wednesday, March 20, 1844.
    This means the Badi epoch was sunset at 18:11 Wednesday, March 20, 1844.
    https://www.timeanddate.com/sun/@112931?month=3&year=1844

    In [1]: from sunrisesunset import SunriseSunset
    In [2]:import datetime
    In [3]: import pytz
    In [4]: dt = datetime.datetime(1844, 3, 20)
    In [5]: zone = pytz.timezone('Asia/Tehran')
    In [6]: dt = dt.astimezone(zone)
    In [7]: ss = SunriseSunset(dt, 35.696111, 51.423056, 'official')
    In [8]: ss.sun_rise_set
    Out[8]:
    (datetime.datetime(1844, 3, 20, 6, 5, 51, 164827, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>),
     datetime.datetime(1844, 3, 20, 18, 11, 6, 983600, tzinfo=<DstTzInfo 'Asia/Tehran' LMT+3:26:00 STD>))
    """

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._bc = BahaiCalendar()
        self._gc = GregorianCalendar()

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
        data = (
            ((1844, 3, 21), (1, 1, 1, 1, 1)),
            ((1930, 4, 21), (1, 5, 11, 2, 13)),
            #((), ()),
            )
        msg = "Expected {}, found {}"

        for g_date, expected_result in data:
            dt = datetime.datetime(*g_date)
            self._bc.parse_datetime(dt)
            result = self._bc.date_representation
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    @unittest.skip("Temporarily skipped")
    def test_date_representation(self):
        """
        Test that both the setter and getter properties set and get
        the correct location data.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_bahai_sunset(self):
        """
        Test that the bahai_sunset method returns the universal time of
        sunset on fixed date. This results in the UTC time of sunset.

        Baha'i epoc in fixed date is 673221 (1844-03-20) at:
            Official: 6:11 pm (18.1833333333) -> 18.14182542 (6:08:30)
        Astronomical: 7:37 pm -> Does not seem to be used.
        (GMT+3:25:44) Sunset in Tehran, Tehran Province, Iran

        18hr + 11min * 1hr / 60min +0s * 1hr / 3600s
        =18hr + 0.1833hr + 0hr
        =18.18333333333333333333 hrs

        673221 + 1 * 18.1833hr / 24
        =673221.75763888888888888889
        =1844-03-20T18:11:00
        """
        # All dates are sunset on the beginning of the Baha'i year.
        data = (
            # (1844, 3, 20, 18, 11) -> (1844, 3, 20, 14, 38, 25.571062490344048)
            (673221, 673221.6100181836),
            # (2024, 3, 19, 18, 16) -> (2024, 3, 19, 14, 38, 22.21245065331459)
            (738964, 738964.6099793108),
            # (2064, 3, 19, 18, 15) -> (2064, 3, 19, 14, 38, 19.865490943193436)
            (753574, 753574.6099521469),
            )
        msg = "Expected {}, found {}"

        for date, expected_result in data:
            result = self._bc.bahai_sunset(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    @unittest.skip("Temporarily skipped")
    def test_astro_bahai_new_year_on_or_before(self):
        """
        Test that the astro_bahai_new_year_on_or_before method returns a
        fixed date of astronomical Bahai New Year on or before fixed date.

        Baha'i epoc fixed date is 673222.
        """
        data = (
            (673222, 673222),
            )
        msg = "Expected result {} for date {}, found {}."

        for date, expected_result in data:
            result = self._bc.astro_bahai_new_year_on_or_before(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_astro_bahai(self):
        """
        Test that the fixed_from_astro_bahai method returns a fixed date
        of Baha’i date.
        """
        b_dates = (
            # (2024, 2, 25) 1st day of Ayyām-i-Hā
            ((1, 10, 9, 0, 1), 738941),
            # (2024, 2, 29) 4th day of Ayyām-i-Hā
            ((1, 10, 9, 0, 4), 738944),
            # (2024, 3, 1) 1st day of last month of year 180
            ((1, 10, 9, 19, 1), 738946),
            # (2022, 2, 25) 1st day of Ayyām-i-Hā of year 178
            ((1, 10, 7, 0, 1), 738211),
            # (2022, 3, 1) 5th day of Ayyām-i-Hā of year 178
            ((1, 10, 7, 0, 5), 738215),
            # (2022, 3, 2) 1st day of Baha of year 178
            ((1, 10, 7, 19, 1), 738216), # This fails but shouldn't.
            # (2024, 1, 22) 4th day of 17th month of year 180
            ((1, 10, 9, 17, 4), 738907),
            )
        msg = "Expected result {} for b_date {}, found {}."

        for b_date, expected_result in b_dates:
            result = self._bc.fixed_from_astro_bahai(b_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, b_date, result))

    #@unittest.skip("Temporarily skipped")
    def test_astro_bahai_from_fixed(self):
        """
        Test that the astro_bahai_from_fixed method returns the
        astronomical Baha’i date corresponding to fixed date.

        Baha'i epic date of 1844, March, 21 = 673222 in fixed date.
        """
        fixed_dates = (
            # (2024, 2, 25) 1st day of Ayyām-i-Hā
            (738941, (1, 10, 9, 0, 1)),
            # (2024, 2, 29) 4th day of Ayyām-i-Hā
            (738944, (1, 10, 9, 0, 4)),
            # (2024, 3, 1) 1st day of last month of year 180
            (738946, (1, 10, 9, 19, 1)),
            # (2022, 2, 25) 1st day of Ayyām-i-Hā year 178
            (738211, (1, 10, 7, 0, 1)),
            # (2022, 3, 1) 5th day of Ayyām-i-Hā year 178
            (738215, (1, 10, 7, 0, 5)),
            # (2022, 3, 2) 1st day of Baha of year 178
            (738216, (1, 10, 7, 19, 1)),
            # (2024, 1, 22) 4th day of 17th month of year 180
            (738907, (1, 10, 9, 17, 4)),
            )
        msg = "Expected {} for fixed_day {}, found {}"

        for fixed_day, expected_result in fixed_dates:
            result = self._bc.astro_bahai_from_fixed(fixed_day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, fixed_day, result))

    #@unittest.skip("Temporarily skipped")
    def test_nam_ruz(self):
        """
        Test that the nam_ruz method returns the correct Badi date.
        """
        data = (
            (1, (1, 1, 1, 1, 1)),
            )
        msg = "Expected {} for date {}, found {}"

        for year, expected_result in data:
            result = self._bc.nam_ruz(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test_nam_ruz_from_gregorian_year(self):
        """
        Test that the nam_ruz_from_gregorian_year method returns the
        correct Gregorian dates.
        """
        data = (
            (1844, (1844, 3, 20)), # (1844, 3, 20, 18, 31)
            )
        msg = "Expected {} for date {}, found {}"

        for year, date in data:
            expected_result = self._gc.fixed_from_gregorian(date)
            result = self._bc.nam_ruz_from_gregorian_year(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

