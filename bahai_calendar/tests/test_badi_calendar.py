# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_badi_calendar.py
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
    def test_parse_gregorian_datetime(self):
        """
        Test that the parse_gregorian_datetime method creates the badi
        long form date representation.

        Thus, for example, Monday, April 21, 1930 would be called “Kamāl
        (Monday), the day of Qudrat (the thirteenth), of the month of Jalāl,
        (the second) of the year Bahhāj (the eleventh), of the fifth  Vāhid,
        of the first Kull-i-Shay, of the Bahá’í Era.
        [major, cycle, year, month, day]
        """
        data = (
            # Badi epoch (Sunset 1844-03-20T18:53:00)
            ((1844, 3, 20, 18, 16), (1, 1, 1, 1, 1, 18, 15, 59.9904)),
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
            self._bc.parse_gregorian_datetime(dt)
            result = self._bc.date_representation
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    #@unittest.skip("Temporarily skipped")
    def test_sunset(self):
        """
        Test that the sunset method returns the universal time of
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
            # Should be 1844-03-20T18:16:00
            ((1, 1, 1), lat, lon, zone, (1, 1, 1, 1, 1, 18, 15, 33.0336)),
            # Should be 2024-03-19T18:16:00
            ((180, 19, 19), lat, lon, zone, (1, 10, 10, 1, 1, 18, 14, 53.1168)),
            # Should be 2064-03-19T18:15:00
            ((221, 1, 1), lat, lon, zone, (1, 12, 12, 1, 2, 18, 15, 57.312)),
            )
        msg = "Expected {}, date {}, found {}"

        for date, lat, lon, zone, expected_result in data:
            result = self._bc.sunset(date, lat, lon, zone)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_naw_ruz(self):
        """
        Test that the nam_ruz method returns the correct Badi date.
        """
        data = (
            (1, False, (1, 1, 1, 1, 1, 18, 15, 57.744)),
            (1, True, (1, 1, 1, 18, 15, 57.744))
            )
        msg = "Expected {} for date {} and short {}, found {}"

        for year, short, expected_result in data:
            result = self._bc.naw_ruz(year, short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, short, result))

    #@unittest.skip("Temporarily skipped")
    def test__is_leap_year(self):
        """
        Test that the _is_leap_year method returns the correct boolean
        for the given year or long Badi date.
        """
        data = (
            # Start of years
            (173, False),                 # 2016
            ((1, 10, 2, 1, 1), False),    # (2016, 3, 20)
            (174, True),                  # 2017
            ((1, 10, 3, 1, 1), True),     # (2017, 3, 20)
            (175, False),                 # 2018
            ((1, 10, 4, 1, 1), False),    # (2018, 3, 21)
            # End of years
            ((1, 10, 2, 19, 19), False), # (2017, 3, 19)
            ((1, 10, 3, 19, 19), True),  # (2017, 3, 19)
            ((1, 10, 4, 19, 19), False), # (2018, 3, 20)
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

        See: https://aa.usno.navy.mil/data/RS_OneYear
        """
        data = (
            ((1, 1, 1), 2394645.5),                 # 1844-03-20T00:00:00
            # Real epoch at sunset 01-01-01T18:16:00 B.E.
            ((1, 1, 1, 18, 16), 2394646.261111),    # 1844-03-20T18:16:00
            ((19, 19, 19), 2401584.5),              # 1863-03-20T00:00:00
            ((180, 19, 19), 2460388.5),             # 2024-03-19T00:00:00
            ((181, 3, 2), 2460428.5),               # 2024-04-28T00:00:00
            ((-260, 1, 1, 18, 16), 2299318.261111), # 1583-03-21T18:16:00
            # A day in Ayyám-i-Há 2022-02-25T00:00:00
            ((178, 0, 1), 2459635.5),
            # 2024-05-14T20:00
            ((181, 3, 19, 20), 2460446.333333),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.jd_from_badi_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_jd(self):
        """
        Test that the badi_date_from_jd method returns the correct jd day.
        """
        data = (
            (2394645.5, True, (1, 1, 1)),     # 1844-03-20T00:00:00
            # Real epoch at sunset 01-01-01T18:16:00 B.E. 1844-03-20T18:16:00
            (2394646.261111, True, (1, 1, 1, 18, 15, 59.9904)),
            (2401584.5, True, (19, 19, 19)),  # 1863-03-20T00:00:00
            (2460388.5, True, (180, 19, 19)), # 2024-03-19T00:00:00
            (2460428.5, True, (181, 3, 2)),   # 2024-04-28T00:00:00
            # 1583-03-21T18:16:00
            (2299318.261111, True, (-260, 1, 1, 18, 15, 59.9904)),
            # A day in Ayyám-i-Há 2022-02-25T00:00:00
            (2459635.5, True, (178, 0, 1)),   # 2022-02-25T00:00:00
            # 2024-05-14T19:59:59.9712 - long form
            (2460445.333333, False, (1, 10, 10, 3, 19, 19, 59, 59.9712)),
            # 2024-05-14T19:59:59.9712 - short form
            (2460445.333333, True, (181, 3, 19, 19, 59, 59.9712)),
            )
        msg = "Expected {} for jd {}, found {}"

        for jd, short, expected_result in data:
            result = self._bc.badi_date_from_jd(jd, short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test_short_date_from_long_date(self):
        """
        Test that the short_date_from_long_date method returns the correct
        (year, month, day) date.
        """
        data = (
            # 1844-03-20T00:00:00
            ((1, 1, 1, 1, 1), (1, 1, 1)),
            # 2024-04-20T20:17:45
            ((1, 10, 10, 2, 14, 20, 17, 45), (181, 2, 14, 20, 17, 45)),
            # 1844-03-19T00:00:00 Before the Badi epoch
            ((0, 19, 19, 19, 19), (0, 19, 19)),
            # 1484-03-11T00:00:00 Before the Badi epoch
            ((0, 1, 1, 1 ,1), (-360, 1, 1)),
            # 1843-03-21T00:00:00
            ((0, 19, 18, 1, 1), (-1, 1, 1)),
            # 1444-05-17T00:00:00
            ((-1, 17, 18, 4, 3), (-400, 4, 3)),
            # 1483-03-12T00:00:00
            ((-1, 19, 19, 1, 1), (-361, 1, 1)),
            ((-2, 19, 19, 1, 1), (-722, 1, 1)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.short_date_from_long_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_long_date_from_short_date(self):
        """
        """
        data = (
            # 1844-03-20T00:00:00
            ((1, 1, 1), (1, 1, 1, 1, 1)),
            # 2024-04-20T20:17:45
            ((181, 2, 14, 20, 17, 45), (1, 10, 10, 2, 14, 20, 17, 45)),
            # 1844-03-19T00:00:00 Day before the Badi epoch
            ((0, 19, 19), (0, 19, 19, 19, 19)),
            # 
            ((0, 10, 10), (0, 19, 19, 10, 10)),
            # 
            ((0, 1, 1), (0, 19, 19, 1, 1)),
            # 1843-03-21T00:00:00
            ((-1, 1, 1), (0, 19, 18, 1, 1)),
            # 1444-05-17T00:00:00
            ((-400, 4, 3), (-1, 17, 18, 4, 3)),
            # 1483-03-12T00:00:00
            ((-361, 1, 1), (-1, 19, 19, 1, 1)),
            # 
            ((-722, 1, 1), (-2, 19, 19, 1, 1)),
            # (2024, 5, 14, 20)
            ((181, 3, 19), (1, 10, 10, 3, 19)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.long_date_from_short_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_date_from_kvymdhms(self):
        """
        Test that the date_from_kvymdhms method correctly converts
        ((Kull-i-Shay, Váḥid, year, month, day.partial) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second).
        """
        data = (
            ((1, 1, 1, 1, 1, 18, 16), False, (1, 1, 1, 1, 1.761111)),
            ((1, 1, 1, 1, 1, 18, 16), True, (1, 1, 1.761111)),
            ((0, 6, 6, 1, 1), True, (-260, 1, 1)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, short, expected_result in data:
            result = self._bc.date_from_kvymdhms(date, short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_kvymdhms_from_b_date(self):
        """
        Test that the kvymdhms_from_b_date method correctly converts
        ((Kull-i-Shay, Váḥid, year, month, day.partial) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second).
        """
        data = (
            ((1, 1, 1, 1, 1.761111), False, (1, 1, 1, 1, 1, 18, 15, 59.9904)),
            ((1, 1, 1, 1, 1.761111), True, (1, 1, 1, 18, 15, 59.9904)),
            ((0, 6, 6, 1, 1, 1), True, (-260, 1, 1)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, short, expected_result in data:
            result = self._bc.kvymdhms_from_b_date(date, short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_valid_badi_month_day(self):
        """
        Test that the _check_valid_badi_month_day method returns the
        correct boolean for valid and invalid dates.

        Note: The boolean below in the data statments determines whether
              or not the data is valid or invalid.
        """
        msg0 = ("The number of Váḥids in a Kull-i-Shay’ should be >= 1 or "
                "<= 19, found {}")
        msg1 = ("The number of years in a Váḥid should be >= 1 or <= 19, "
                "found {}")
        msg2 = "Invalid month '{}', should be 0 - 19."
        msg3 = ("Invalid day '{}' for month '{}' and year '{}' "
                "should be 1 - < {{}}.")
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
            ((1, 10, 2, 0, 1), True, ''), # During Ayyám-i-Há non leap year
            ((1, 10, 3, 0, 1), True, ''), # During Ayyám-i-Há leap year
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
            ((1, 10, 3, 0, 0, 1, 1, 1), False, msg3.format(0, 0, 3)),
            ((1, 10, 3, 0, 6, 1, 1, 1), False, msg3.format(6, 0, 3)),
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
            kull_i_shay, vahid, year, month, day = b_date[:5]
            hour, minute, second = self._bc._get_hms(b_date)
            cycle = 5 if self._bc._is_leap_year(b_date) else 4

            if validity: # Test for correct dates
                if month == 0: # Ayyám-i-Há

                    for d in range(1, cycle+1):
                        date = (kull_i_shay, vahid, year, month, d)
                        self._bc._check_valid_badi_month_day(date)

                for m in range(1, 20):
                    for d in range(1, 20):
                        date = (kull_i_shay, vahid, year, m, d)
                        self._bc._check_valid_badi_month_day(date)
            else: # Test for invalid dates
                try:
                    with self.assertRaises(AssertionError) as cm:
                        self._bc._check_valid_badi_month_day(b_date)
                except AssertionError as e:
                    raise AssertionError(
                        f"Váḥid {vahid}, year {year}, month {month}, "
                        f"day {day}, hour {hour}, minute {minute}, "
                        f"second {second}, {e}")
                else:
                    num_days = cycle if month == 0 else 19
                    message = str(cm.exception)
                    self.assertEqual(err_msg.format(num_days), message)

    #@unittest.skip("Temporarily skipped")
    def test__get_hms(self):
        """
        Test that the _get_hms method parses the hours, minutes, and
        seconds correctly for either the short or long form Badi date.
        Test both the long and short for od the Badi Date.
        """
        data = (
            ((1, 1, 1, 1, 1, 18, 16), False, (18, 16, 0)),
            ((1, 1, 1, 18, 16), True, (18, 16, 0)),
            )
        msg = "Expected {} for date {} amd short {}, found {}"

        for date, short, expected_result in data:
            result = self._bc._get_hms(date, short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_gregorian_date(self):
        """
        Test that the badi_date_from_gregorian_date method returns the
        correct Badi date.
        """
        data = (
            ((1844, 3, 20, 18, 16), False, (1, 1, 1, 1, 1, 18, 15, 59.9904)),
            ((1844, 3, 20, 18, 16), True, (1, 1, 1, 18, 15, 59.9904)),
            ((2024, 5, 14, 20), False, (1, 10, 10, 3, 19, 19, 59, 59.9712)),
            ((2024, 5, 14, 20), True, (181, 3, 19, 19, 59, 59.9712)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, short, expected_result in data:
            result = self._bc.badi_date_from_gregorian_date(date, short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_date_from_badi_date(self):
        """
        Test that the gregorian_date_from_badi_date method returns the
        correct Gregorian date.
        """
        data = (
            ((1, 1, 1, 18, 16), (1844, 3, 20.761111)),
            ((181, 3, 18, 20), (2024, 5, 14.833333)),
            ((181, 3, 19, 20), (2024, 5, 15.833333)),
            ((181, 4, 1, 17), (2024, 5, 16.708333)),
            ((181, 4, 1, 20), (2024, 5, 16.833333)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.gregorian_date_from_badi_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))
