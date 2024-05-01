# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_badi_calandar.py
#
__docformat__ = "restructuredtext en"

import unittest
import datetime

from ..badi_calendar import BahaiCalendar, GregorianCalendar


class TestBadiCalendar(unittest.TestCase):
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
            # Badi epoch (Vernal Equinox 1844-03-20T11:53:51 DT)
            ((1844, 3, 20), (1, 1, 1, 1, 1)),
            # CC ch#16 p271 First day of Riḍván
            ((1930, 4, 21), (1, 5, 11, 2, 13)),
            # B.E. 100 (Vernal Equinox 1943-03-21T12:03:04 DT)
            ((1943, 3, 21), (1, 6, 5, 1, 1)),
            # World Centre update (Vernal Equinox 2015-03-20T22:46:16 DT)
            ((2015, 3, 21), (1, 10, 1, 1, 1)),
            )
        msg = "Expected {} with g_date {}, found {}"

        for g_date, expected_result in data:
            dt = datetime.datetime(*g_date)
            self._bc.parse_datetime(dt)
            result = self._bc.date_representation
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    @unittest.skip("Temporarily skipped")
    def test_date_representation(self):
        """
        Test that both the setter and getter properties set and get
        the correct location data.
        """
        pass

    @unittest.skip("Temporarily skipped")
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
        lat, lon, alt, zone = self._bc.BAHAI_LOCATION
        data = (
            # (1844, 3, 20, 18, 11) -> (1844, 3, 20, 14, 33, 18.275475203990936)
            ((1, 1, 1, 1, 1), lat, lon, zone, 0),
            # (2024, 3, 19, 18, 16) -> (2024, 3, 19, 14, 33, 14.83555220067501)
            #((), 0),
            # (2064, 3, 19, 18, 15) -> (2064, 3, 19, 14, 33, 14.920333474874496)
            #((), 0),
            )
        msg = "Expected {}, found {}"

        for date, lat, lon, zone, expected_result in data:
            result = self._bc.bahai_sunset(date, lat, lon, zone)
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
    def test_naw_ruz(self):
        """
        Test that the nam_ruz method returns the correct Badi date.
        """
        data = (
            (1, (1, 1, 1, 1, 1)),
            )
        msg = "Expected {} for date {}, found {}"

        for year, expected_result in data:
            result = self._bc.naw_ruz(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test_naw_ruz_from_gregorian_year(self):
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
            result = self._bc.naw_ruz_from_gregorian_year(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test__is_leap_year(self):
        """
        Test that the _is_leap_year method returns the correct boolean
        for the given year.
        """
        data = (
            (736044, False), # (2016, 3, 21) start 173
            (736407, False), # (2017, 3, 19) end 173
            (736408, True),  # (2017, 3, 20) start 174
            (736773, True),  # (2018, 3, 20) end 174
            (736774, False), # (2018, 3, 21) start 175
            (737138, False), # (2019, 3, 20) end 175
            )
        msg = "Expected {} for day {}, found {}"

        for day, expected_result in data:
            result = self._bc._is_leap_year(day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, day, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_badi_date(self):
        """
        Test that the jd_from_badi_date method returns the correct jd day.
        """
        data = (
            ((1, 1, 1), self._bc.BADI_EPOCH), # 1844-03-20T00:00:00
            #((19, 19, 19), 2395374.5), # 2401583.5
            ((180, 19, 19), 2460387.5), # 2460388.5
            ((181, 3, 2), 2460428.5),         # 2024-04-28T00:00:00
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.jd_from_badi_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_jd(self):
        """
        Test that the jd_from_badi_date method returns the correct jd day.
        """
        data = (
            #(self._bc.BADI_EPOCH, (1, 1, 1, 1 , 1)), # 1844-03-20T00:00:00
            #(2395374.5, (19, 19, 19)),
            #(2460387.5, (180, 19, 19)),
            (2460428.5, (1, 10, 10, 3, 4)),          # 2024-04-29T00:00:00
            )
        msg = "Expected {} for jd {}, found {}"

        for jd, expected_result in data:
            result = self._bc.badi_date_from_jd(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test_date_from_b_date(self):
        """
        Test that the date_from_b_date method returns the correct
        (year, month, day) date.
        """
        data = (
            # 1844-03-20T00:00:00
            ((1, 1, 1, 1, 1), (1, 1, 1, 0, 0, 0)),
            # 2024-04-20T20:17:45
            ((1, 10, 10, 2, 14, 20, 17, 45), (181, 2, 14, 20, 17, 45)),
            # 1844-03-19T00:00:00 Before the Badi epoch
            ((0, 19, 19, 19, 19), (0, 19, 19, 0, 0, 0)),
            # 1484-03-11T00:00:00 Before the Badi epoch
            ((0, 1, 1, 1 ,1), (-360, 1, 1, 0, 0, 0)),
            # 1843-03-21T00:00:00
            ((0, 19, 18, 1, 1), (-1, 1, 1, 0, 0, 0)),
            # 1444-05-17T00:00:00
            ((-1, 17, 18, 4, 3), (-400, 4, 3, 0, 0, 0)),
            # 1483-03-12T00:00:00
            ((-1, 19, 19, 1, 1), (-361, 1, 1, 0, 0, 0)),
            ((-2, 19, 19, 1, 1), (-722, 1, 1, 0, 0, 0)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.date_from_b_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_b_date_from_date(self):
        """
        """
        data = (
            # 1844-03-20T00:00:00
            ((1, 1, 1), (1, 1, 1, 1, 1, 0, 0, 0)),
            # 2024-04-20T20:17:45
            ((181, 2, 14, 20, 17, 45), (1, 10, 10, 2, 14, 20, 17, 45)),
            # 1844-03-19T00:00:00 Day before the Badi epoch
            ((0, 19, 19), (0, 19, 19, 19, 19, 0, 0, 0)),
            ((0, 10, 10), (0, 19, 19, 10, 10, 0, 0, 0)),
            ((0, 1, 1), (0, 19, 19, 1, 1 , 0, 0, 0)),
            # 1843-03-21T00:00:00
            ((-1, 1, 1), (0, 19, 18, 1, 1, 0, 0, 0)),
            # 1444-05-17T00:00:00
            ((-400, 4, 3), (-1, 17, 18, 4, 3, 0, 0, 0)),
            # 1483-03-12T00:00:00
            ((-361, 1, 1), (-1, 19, 19, 1, 1, 0, 0, 0)),
            ((-722, 1, 1), (-2, 19, 19, 1, 1, 0, 0, 0)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.b_date_from_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_valid_badi_month_day(self):
        """
        Test that the _check_valid_badi_month_day method returns the
        correct boolean for valid and invalid dates.
        """
        msg0 = ("The number of Váḥids in a Kull-i-Shay’ should be >= 1 or "
                "<= 19, found {}")
        msg1 = ("The number of years in a Váḥid should be >= 1 or <= 19, "
                "found {}")
        msg2 = "Invalid month '{}', should be 0 - 19."
        msg3 = ("Invalid day '{}' for month '{}' and year '{}' "
                "should be 1 - 19.")
        msg4 = "Invalid hour '{}' it must be 0 <= {} < 24"
        msg5 = "Invalid minute '{}' should be 0 <= {} < 60."
        msg6 = ("If there is a part day then there can be no hours, "
                "minutes, or seconds.")
        msg7 = ("If there is a part hour then there can be no minutes or "
                "seconds.")
        msg8 = "If there is a part minute then there can be no seconds."
        data = (
            ((1, 1, 1, 1, 1), True, ''),  # Non leap year
            ((1, 10, 3, 1, 1), True, ''), # Known leap year
            ((0, 1, 1, 1, 1), True, ''),  # Before Badi epoch
            ((-1, 1, 1, 1, 1), True, ''), # Before Badi epoch
            # Invalid Váḥid
            ((1, 0, 1, 1, 1, 1, 1, 1), False, msg0.format(0)),
            ((1, 20, 1, 1, 1, 1, 1, 1), False, msg0.format(20)),
            # Invalid year
            ((1, 10, 0, 1, 1, 1, 0, 0 ), False, msg1.format(0)),
            ((1, 10, 20, 1, 1, 1, 0, 0 ), False, msg1.format(20)),
            # Invalid month
            ((1, 10, 10, -1, 1, 1, 0, 0 ), False, msg2.format(-1)),
            ((1, 10, 10, 20, 1, 1, 0, 0 ), False, msg2.format(20)),
            # Invalid Ayyám-i-Há day
            ## ((1, 10, 3, 0, 0, 1, 1, 1), False, msg3.format(0, 0, 3)),
            ## ((1, 10, 3, 0, 4, 1, 1, 1), False, msg3.format(4, 0, 3)),
            ## ((1, 10, 3, 0, 6, 1, 1, 1), False, msg3.format(6, 0, 3)),
            # Invalid normal day
            ((1, 10, 3, 2, 0, 1, 1, 1), False, msg3.format(0, 2, 3)),
            ((1, 10, 3, 2, 20, 1, 1, 1), False, msg3.format(20, 2, 3)),
            # Invalid hour
            ((1, 10, 3, 2, 1, -1, 1, 1), False, msg4.format(-1, -1)),
            ((1, 10, 3, 2, 1, 24, 1, 1), False, msg4.format(24, 24)),
            # Invalid minute
            ((1, 10, 3, 2, 1, 1, -1, 1), False, msg5.format(-1, -1)),
            ((1, 10, 3, 2, 1, 1, 60, 1), False, msg5.format(60, 60)),
            # Invalid partial day
            ((1, 10, 3, 2, 1.5, 1, 0, 0), False, msg6),
            ((1, 10, 3, 2, 1.5, 0, 1, 0), False, msg6),
            ((1, 10, 3, 2, 1.5, 0, 0, 1), False, msg6),
            # Invalid partial hour
            ((1, 10, 3, 2, 1, 1.5, 1, 0), False, msg7),
            ((1, 10, 3, 2, 1, 1.5, 0, 1), False, msg7),
            # Invalid partial minute
            ((1, 10, 3, 2, 1, 1, 1.5, 1), False, msg8),
            )

        for b_date, validity, err_msg in data:
            t_len = len(b_date)
            kull_i_shay = b_date[0]
            vahid = b_date[1]
            year = b_date[2]
            month = b_date[3]
            day = b_date[4]
            hour = b_date[5] if t_len > 5 and b_date[5] is not None else 0
            minute = b_date[6] if t_len > 6 and b_date[6] is not None else 0
            second = b_date[7] if t_len > 7 and b_date[7] is not None else 0

            if validity:
                # Test correct dates
                for m in range(20):
                    if m == 0:
                        pass # *** TODO *** Test for leap years

                    for d in range(1, 20):
                        date = (kull_i_shay, vahid, year, m, d)
                        self._bc._check_valid_badi_month_day(date)
            else:
                try:
                    with self.assertRaises(AssertionError) as cm:
                        self._bc._check_valid_badi_month_day(b_date)
                except AssertionError as e:
                    raise AssertionError(
                        f"Váḥid {vahid}, year {year}, month {month}, "
                        f"day {day}, hour {hour}, minute {minute}, "
                        f"second {second}, {e}")
                else:
                    message = str(cm.exception)
                    self.assertEqual(err_msg, message)
