# -*- coding: utf-8 -*-
#
# badidatetime/test/test_badi_calendar.py
#
__docformat__ = "restructuredtext en"

import unittest

from ..badi_calendar import BahaiCalendar
from ..gregorian_calendar import GregorianCalendar


class TestBadiCalendar(unittest.TestCase):
    """
    Some sunrise and sunset calculations done with my SunriseSunset package.
    Sunrise and Sunset for 1844-03

    The vernal equinox in Tehran was at 15:19 on Wednesday, March 20, 1844.
    This means the Badi epoch was sunset at 18:11 Wednesday, March 20, 1844.
    https://www.timeanddate.com/sun/@112931?month=3&year=1844

    Alternative latitude and longitude coordinates can be found at:
    https://latitude.to/map/us/united-states/cities/fuquay-varina
    """

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._bc = BahaiCalendar()
        self._gc = GregorianCalendar()

    #@unittest.skip("Temporarily skipped")
    def test_utc_sunset(self):
        """
        Test that the utc_sunset method returns the universal time of
        sunset on fixed date. This results in the UTC time of sunset.
        See: https://gml.noaa.gov/grad/solcalc/
        """
        lat, lon, zone = self._bc._BAHAI_LOCATION[:3]
        data = (
            # Should be 1844-03-19T18:16:00
            ((1, 1, 1), None, None, None, (18, 18, 15.6456)),
            # Should be 2024-03-19T18:13:00
            ((180, 19, 19), lat, lon, zone, (18, 16, 47.0352)),
            # Should be 2064-03-19T18:16:00
            ((221, 1, 1), lat, lon, zone, (18, 17, 51.3384)),
            # Should be 2024-04-20T19:53:00 DST in Raleigh NC
            ((181, 2, 13), 35.7796, -78.6382, -4, (19, 53, 28.122)),
            # Should be 2024-07-22T20:26:00 DST in Raleigh NC
            ((181, 7, 11), 35.7796, -78.6382, -4, (20, 25, 47.3628)),
            )
        msg = "Expected {}, date {}, found {}"

        for date, lat, lon, zone, expected_result in data:
            result = self._bc.utc_sunset(date, lat, lon, zone)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_naw_ruz_g_date(self):
        """
        Test that the naw_ruz_g_date method returns the correct Badi date.
        """
        lat, lon, zone = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 1844-03-19T18:16:00+03:30
            (1, lat, lon, zone, False, True, (1844, 3, 19.762681067921)),
            # 1844-03-19T18:16:00:00+03:30
            (1, lat, lon, zone, True, True, (1844, 3, 19, 18, 18, 15.6456)),
            # 2024-03-19T18:26:00-05:00
            (181, 35.7796, -78.6382, -5, True, True,
             (2024, 3, 19, 18, 27, 38.5848)),
            # 2025-03-19T18:16:00+03:30
            (182, lat, lon, zone, False, True, (2025, 3, 19.762679148)),
            # 2026-03-20T18:16:00+03:30
            (183, lat, lon, zone, False, True, (2026, 3, 20.762539365795)),
            # The following years are the ones that had errors.
            # 2021-03-19T18:16:00+03:30
            (178, lat, lon, zone, False, True, (2021, 3, 19.762659180444)),
            # 2030-03-20T18:16:00:+03:30
            (187, lat, lon, zone, False, True, (2030, 3, 19.762557186186)),
            # 2034-03-19T18:16:00+03:30
            (191, lat, lon, zone, False, True, (2034, 3, 19.762569336686)),
            # 2038-03-19T18:16:00+03:30
            (195, lat, lon, zone, False, True, (2038, 3, 19.762581459247)),
            # 2054-03-19T18:16:00+03:30
            (211, lat, lon, zone, False, True, (2054, 3, 19.762064724695)),
            # 2059-03-20T18:16:21.590400
            (216, lat, lon, zone, False, True, (2059, 3, 19.761936932337)),
            # 2063-03-19T18:16:23.318400
            (220, lat, lon, zone, False, True, (2063, 3, 19.761957366485)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 3, 19.762681067921)),
            # 1993-03-19T18:16:24.960000 Test sunset before Vernal Equinox
            (150, lat, lon, zone, False, True, (1993, 3, 19.762555095367)),
            )
        msg = "Expected {} for date {}, found {}"

        for year, lat, lon, zone, hms, use_coords, expected_result in data:
            if use_coords:
                result = self._bc.naw_ruz_g_date(year, lat, lon, zone, hms=hms)
            else:
                result = self._bc.naw_ruz_g_date(year, hms=hms)

            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test_first_day_of_ridvan_g_date(self):
        """
        Test that the first_day_of_ridvan_g_date method returns Jalál 13th
        in any year.
        """
        lat, lon, zone = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 0001-02-13T00:00:00 -> 1844-04-20T18:42:00
            (1, lat, lon, zone, False, True, (1844, 4, 19.780365471262)),
            (1, lat, lon, zone, True, True, (1844, 4, 19, 18, 43, 43.5756)),
            # 0181-02-13T19:41:00-04:00 DST at Raleigh NC, USA
            (181, 35.7796, -78.6382, -4, False, True,
             (2024, 4, 19.828797717579)),
            (181, 35.7796, -78.6382, -4, True, True,
             (2024, 4, 19, 19, 53, 28.122)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 4, 19.780365471262)),
            )
        msg = "Expected {} for hms {}, found {}"

        for year, lat, lon, zone, hms, use_cor, expected_result in data:
            if use_cor:
                result = self._bc.first_day_of_ridvan_g_date(
                    year, lat, lon, zone, hms=hms)
            else:
                result = self._bc.first_day_of_ridvan_g_date(year, hms=hms)

            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, hms, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_badi_date(self):
        """
        Test that the jd_from_badi_date method returns the correct jd day.

        For a more complete test run: ./contrib/misc/badi_jd_tests.py -aX

        See: https://aa.usno.navy.mil/data/RS_OneYear
        """
        data = (
            # Real epoch at sunset 01-01-01T00:00:00 B.E.
            # 1844-03-19T18:16:00+03:30
            ((1, 1, 1), 2394643.262681068),
            # 1844-03-19T12:00:00+03:30
            ((0, 19, 19, 17, 44), 2394643.2615279276),
            # 1863-03-18T18:14:00+03:30
            ((19, 19, 19), 2401582.261748015),
            # 2024-03-18T18:15:00+03:30
            ((180, 19, 19), 2460386.2616554843),
            # 2024-04-27T18:48:00+03:30
            ((181, 3, 2), 2460426.2853480815),
            # 1583-03-20T18:17:00+03:30
            ((-260, 1, 1), 2299315.2623346695),
            # A day in Ayyám-i-Há 2022-02-24T17:57:55.152000
            ((178, 0, 1), 2459633.2485395027),
            # 2024-05-15T15:04:51.427199
            ((181, 3, 19, 20), 2460444.2950505884),
            # Test one date for each coefficient.
            ((-1842, 1, 1), 1721501.2611352967),
            ((-1841, 1, 1), 1721867.2610094287),
            ((-1813, 1, 1), 1732093.2611315844),
            ((-1801, 1, 1), 1736476.2611916426),
            ((-1797, 1, 1), 1737937.2606832578),
            ((-1781, 1, 1), 1743781.2607598111),
            ((-1715, 1, 1), 1767887.261852178),
            ((-1699, 1, 1), 1773731.261398498),
            ((-1683, 1, 1), 1779575.2614695076),
            ((-1551, 1, 1), 1827787.262602002),
            ((-497, 1, 1), 2212753.26650604),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.jd_from_badi_date(date)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_jd(self):
        """
        Test that the badi_date_from_jd method returns the correct Badi date.

        Run: ./contrib/misc/gregorian_jd_tests.py -jS<start_date> -E<end_date>
        to find the jd values below. It's best to do a year at a time.

        .. note::

           All JDs are in UT time so we can check if the code changes it to
           the given time zone correctly.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_coords = (35.5894, -78.7792, -5.0)
        data = (
            # 0001-03-20T18:12:00+00:00 -> 1721502.258333 (1721502.2574517454)
            # 0001-03-20T18:16:00+03:30 -> 1721502.2606140166
            (1721502.2574517454, *epoch_coords, False, True, True, False,
             False, (-1842, 1, 2, 0, 2, 20.6808)),
            # 0001-04-08T18:42:00+00:00
            (1721521.2783162275, *epoch_coords, False, True, True, False,
             False, (-1842, 2, 2, 0, 2, 19.0752)),
            # 0002-02-24T17:33:00+00:00
            (1721843.2297538682, *epoch_coords, False, True, True, False,
             False, (-1842, 0, 1, 0, 2, 34.1628)),
            # 0002-02-25T17:34:00+00:00
            (1721844.2309374965, *epoch_coords, False, True, True, False,
             False, (-1842, 0, 2, 0, 2, 33.4644)),
            # 0002-02-26T17:36:00+00:00
            (1721845.2321173307, *epoch_coords, False, True, True, False,
             False, (-1842, 0, 3, 0, 2, 32.7696)),
            # 0002-03-02T17:43:00+00:00
            (1721849.236798515, *epoch_coords, False, True, True, False, False,
             (-1842, 19, 2, 0, 2, 30.03)),
            # 0002-03-06T17:49:00+00:00
            (1721853.24141933, *epoch_coords, False, True, True, False, False,
             (-1842, 19, 6, 0, 2, 27.4416)),
            # 1583-03-20T18:12:00+00:00
            (2299315.2596576316, *epoch_coords, False, True, True, False,
             False, (-260, 1, 1, 0, 2, 29.3028)),
            # 1844-03-19T18:12:00+00:00
            (2394643.2592724077, *epoch_coords, False, True, True, False,
             False, (1, 1, 1, 0, 2, 30.8328)),
            # 1845-03-20T18:13:00+00:00
            (2395009.2601561244, *epoch_coords, False, True, True,  False,
             False, (2, 1, 1, 0, 2, 30.624)),
            # 1863-03-20T18:12:00+00:00
            (2401583.2597260755, *epoch_coords, False, True, True, False,
             False, (20, 1, 1, 0, 2, 30.822)),
            # Three consecutive days that gave me trouble
            (2394641.5, *epoch_coords, False, True, True, False, True,
             (0, 19, 19)),
            (2394642.5, *epoch_coords, False, True, True, False, True,
             (1, 1, 1)),
            (2394643.262113, *epoch_coords, False, True, True, False, True,
             (1, 1, 2)),
            # 1969-12-31T16:00:00+00:00
            (2440584.1669850457, *local_coords, True, True, True, False, False,
             (126, 16, 1, 0, 1, 43, 579200)),
            # 1970-01-01T:00:00:00Z -> 2440585.5
            (self._bc._POSIX_EPOCH, 51.477928, -0.001545, 0, False, True, True,
             False, False, (126, 16, 2, 8, 0, 36.2052)),
            # 2015-03-20T18:12:00+00:00
            (2457100.2598390104, *epoch_coords, False, True, True, False,
             False, (172, 1, 1, 0, 2, 31.596)),
            # 2024-03-19T18:12:00+00:00
            (2460387.2596184844, *epoch_coords, False, True, True, False,
             False, (181, 1, 1, 0, 2, 31.7364)),
            # 2024-04-20T19:06:00+00:00
            (2460419.296837184, *epoch_coords, False, True, True,  False,
             False, (181, 2, 13, 0, 0, 50.3136)),
            # 1st day of Ayyám-i-Há -> 2022-02-24T17:31:00+00:00
            (2459633.231312508, *epoch_coords, False, True, True, False, False,
             (178, 0, 1, 0, 2, 43.0404)),
            # 5th day of Ayyám-i-Há -> 2022-03-01T17:40:00+00:00
            (2459638.2374685165, *epoch_coords, False, True, True, False,
             False, (178, 19, 1, 0, 2, 40.0596)),  # TODO Off by a day
            # 2022-03-02T17:42::00+00:00
            (2459639.238690004, *epoch_coords, False, True, True, False, False,
             (178, 19, 2, 0, 2, 39.48)),
            # Badi short form -> 2024-05-12T19:41:00+00:00
            (2460441.3215382686, *epoch_coords, False, True, True, False,
             False, (181, 3, 16, 0, 0, 48.3624)),
            # Badi long form -> 2024-05-14T19:45:00+00:00
            (2460443.32363071, *epoch_coords, False, False, True, False, False,
             (1, 10, 10, 3, 18, 0, 0, 47.7)),
            (2460443.32363071, *epoch_coords, True, False, True, False, False,
             (1, 10, 10, 3, 18, 0, 0, 47, 700000)),
            # 2024-07-17T19:19:00+00:00
            (2460507.3382339347, *epoch_coords, False, True, True, False,
             False, (181, 7, 6, 23, 58, 17.4756)),
            # 2024-07-17T19:19:00+00:00 Test fractional day.
            (2460507.3382339347, *epoch_coords, False, True, True, True, False,
             (181, 7, 6.998813)),
            # (2024, 3, 19, 18, 17, 37, 968000) -> (181, 1, 1)
            (2460387.2596184844, *epoch_coords, False, True, True, False,
             False, (181, 1, 1, 0, 2, 31.7364)),
            )
        msg = "Expected {} for jd {} for lat {}, lon {}, and zone {}, found {}"

        for (jd, lat, lon, zone, us, short, trim,
             fraction, rtd, expected_result) in data:
            result = self._bc.badi_date_from_jd(
                jd, lat, lon, zone, us=us, short=short, trim=trim,
                fraction=fraction, rtd=rtd, _chk_on=False)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, jd, lat, lon, zone, result))

    #@unittest.skip("Temporarily skipped")
    def test_short_date_from_long_date(self):
        """
        Test that the short_date_from_long_date method returns the correct
        (year, month, day, hour, minute, seconds, us) date.
        """
        data = (
            # 1844-03-20T00:00:00
            ((1, 1, 1, 1, 1), False, True, (1, 1, 1, 0, 0, 0, 0)),
            ((1, 1, 1, 1, 1), True, True, (1, 1, 1)),
            # 2024-04-20T20:17:45
            ((1, 10, 10, 2, 14, 20, 17, 45), False, True,
             (181, 2, 14, 20, 17, 45, 0)),
            # 1844-03-19T00:00:00 Before the Badi epoch
            ((0, 19, 19, 19, 19), False, True, (0, 19, 19, 0, 0, 0, 0)),
            # 1484-03-11T00:00:00 Before the Badi epoch
            ((0, 1, 1, 1, 1), False, True, (-360, 1, 1, 0, 0, 0, 0)),
            # 1843-03-21T00:00:00
            ((0, 19, 18, 1, 1), False, True, (-1, 1, 1, 0, 0, 0, 0)),
            # 1444-05-17T00:00:00
            ((-1, 17, 18, 4, 3), False, True, (-400, 4, 3, 0, 0, 0, 0)),
            # 1483-03-12T00:00:00
            ((-1, 19, 19, 1, 1), False, True, (-361, 1, 1, 0, 0, 0, 0)),
            ((-2, 19, 19, 1, 1), False, True, (-722, 1, 1, 0, 0, 0, 0)),
            # 2024-08-27T13:37:58.651870-4:00
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799), False, True,
             (181, 9, 8, 19, 1, 3, 532799)),
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799), True, True,
             (181, 9, 8, 19, 1, 3, 532799)),
            )
        msg = "Expected {} for date {}, trim {}, and _chk_on {}, found {}"

        for date, trim, _chk_on, expected_result in data:
            result = self._bc.short_date_from_long_date(date, trim=trim,
                                                        _chk_on=_chk_on)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, trim, _chk_on, result))

    #@unittest.skip("Temporarily skipped")
    def test_long_date_from_short_date(self):
        """
        Test that the long_date_from_short_date method returns the correct
        long form Badi date.
        """
        data = (
            # 1121-03-21T18:23:39.8112
            ((-722, 1, 1), False, (-2, 19, 19, 1, 1, 0, 0, 0, 0)),
            # 1444-05-17T00:00:00
            ((-400, 4, 3), False, (-1, 17, 18, 4, 3, 0, 0, 0, 0)),
            # 1481-03-20T18:25:09.2352
            ((-362, 1, 1), False, (-1, 19, 18, 1, 1, 0, 0, 0, 0)),
            # 1482-03-21T18:24:57.6576
            ((-361, 1, 1), False, (-1, 19, 19, 1, 1, 0, 0, 0, 0)),
            # 1483-09-08T18:08:00.2112
            ((-360, 10, 1), False, (0, 1, 1, 10, 1, 0, 0, 0, 0)),
            # 1843-09-17T18:08:24.8352
            ((0, 10, 10), False, (0, 19, 19, 10, 10, 0, 0, 0, 0)),
            # 1843-03-21T21:18:14
            ((0, 1, 1), False, (0, 19, 19, 1, 1, 0, 0, 0, 0)),
            # 1844-03-19T00:00:00 Day before the Badi epoch
            ((0, 19, 19), False, (0, 19, 19, 19, 19, 0, 0, 0, 0)),
            # 1844-03-20T00:00:00
            ((1, 1, 1), False, (1, 1, 1, 1, 1, 0, 0, 0, 0)),
            ((1, 1, 1), True, (1, 1, 1, 1, 1)),
            # 1842-03-21T00:00:00
            ((-1, 1, 1), False, (0, 19, 18, 1, 1, 0, 0, 0, 0)),
            # 2024-04-20T20:17:45
            ((181, 2, 14, 20, 17, 45), False,
             (1, 10, 10, 2, 14, 20, 17, 45, 0)),
            ((181, 2, 14, 20, 17, 45), True, (1, 10, 10, 2, 14, 20, 17, 45)),
            # (2024, 5, 14, 20)
            ((181, 3, 19), False, (1, 10, 10, 3, 19, 0, 0, 0, 0)),
            ((181, 3, 19), True, (1, 10, 10, 3, 19)),
            # During Ayyám-i-Há leap year
            ((174, 0, 1), True, (1, 10, 3, 0, 1)),
            # 2204-09-08T18:20:56.3424
            ((361, 10, 1), False, (1, 19, 19, 10, 1, 0, 0, 0, 0)),
            # 2205-09-08T18:21:17.5104
            ((362, 10, 1), False, (2, 1, 1, 10, 1, 0, 0, 0, 0)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, trim, expected_result in data:
            result = self._bc.long_date_from_short_date(date, trim=trim)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, trim, result))

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
            result = self._bc.date_from_kvymdhms(date, short=short)
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
            ((1, 1, 1, 1, 1.761111), False, False, False,
             (1, 1, 1, 1, 1, 18, 15, 59.9904)),
            ((1, 1, 1, 1, 1.761111), False, False, True,
             (1, 1, 1, 1, 1, 18, 15, 59.9904)),
            ((1, 1, 1, 1, 1.761111), False, True, False,
             (1, 1, 1, 18, 15, 59.9904, 0)),
            ((1, 1, 1, 1, 1.761111), False, True, True,
             (1, 1, 1, 18, 15, 59.9904)),
            ((0, 6, 6, 1, 1, 1), False, True, False, (-260, 1, 1, 1, 0, 0, 0)),
            ((1, 10, 10, 9, 8, 19, 1, 3.5328), True, False, False,
             (1, 10, 10, 9, 8, 19, 1, 3, 532800)),
            ((1, 10, 10, 9, 8, 19, 1, 3.5328, 0), True, True, False,
             (181, 9, 8, 19, 1, 3, 532800)),
            )
        msg = ("Expected {} for date {} with us {}, short {}, and "
               "trim {}, found {}")

        for date, us, short, trim, expected_result in data:
            result = self._bc.kvymdhms_from_b_date(
                date, us=us, short=short, trim=trim)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, us, short, trim, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_gregorian_date(self):
        """
        Test that the badi_date_from_gregorian_date method returns the
        correct Badi date.

        .. note::

           1. https://www.timeanddate.com/sun/iran/tehran?month=3&year=1844
           2. https://gml.noaa.gov/grad/solcalc/
           3. https://www.calculatorsoup.com/calculators/time/sunrise_sunset.php
        """
        data = (
            # 1. shows 18:10:00, 2. 18:16:00, 3. 18:15
            ((1844, 3, 19, 18, 11, 40.057), False, True, True,
             (1, 1, 1, 1, 1, 0, 0, 49.752)),
            ((1844, 3, 19, 18, 11, 40.057), True, True, True,
             (1, 1, 1, 0, 0, 49.752)),
            # 1. shows 19:01, 2. 19:02, 3. 19:02, (19:41.6628 works but wrong)
            # *** TODO *** Look into this
            ((2024, 5, 14, 19, 2), False, True, True,
             (1, 10, 10, 3, 18, 23, 16, 46.0092)),
            ((2024, 5, 14, 19, 2), True, True, True,
             (181, 3, 18, 23, 16, 46.0092)),
            # 1. shows 18:15, 2. 18:16, 3. 18:15
            ((2024, 3, 19, 18, 12, 9.485), True, True, True,
             (181, 1, 1, 0, 0, 50.184)),
            # The next tests may show the wrong month and day if
            # _exact=False is used.
            # The _exact=False condition is generally used in testing.
            ((1844, 3, 19, 18, 16), True, True, False,
             (1, 1, 3, 0, 1, 47.4384)),
            ((2024, 5, 14, 20), True, True, False,
             (181, 4, 2, 0, 11, 47.8572)),
            )
        msg = "Expected {} for date {}, short {} and exact {}, found {}"

        for date, short, trim, exact, expected_result in data:
            result = self._bc.badi_date_from_gregorian_date(
                date, short=short, trim=trim, _exact=exact)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, short, exact, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_date_from_badi_date(self):
        """
        Test that the gregorian_date_from_badi_date method returns the
        correct Gregorian date.
        """
        lat, lon, zone = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 1844-03-19T18:16:36.710400
            ((1, 1, 1), lat, lon, zone, True,
             (1844, 3, 19, 18, 18, 15, 645600)),
            ((126, 16, 1), lat, lon, zone, True,
             (1969, 12, 30, 17, 1, 58, 872000)),
            ((181, 3, 18, 20), lat, lon, zone, True,
             (2024, 5, 14, 19, 4, 5, 37600)),
            ((181, 3, 19, 20), lat, lon, zone, True,
             (2024, 5, 15, 19, 4, 52, 370400)),
            ((181, 4, 1, 17), lat, lon, zone, True,
             (2024, 5, 16, 19, 5, 39, 300000)),
            ((181, 4, 1, 20), lat, lon, zone, True,
             (2024, 5, 16, 19, 5, 39, 300000)),
            # The next tests show the wrong day if exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1, 1, 1), lat, lon, zone, False,
             (1844, 3, 17, 18, 18, 15, 645600)),
            ((181, 3, 18, 20), lat, lon, zone, False,
             (2024, 5, 12, 19, 4, 5, 37600)),
            )
        msg = "Expected {} for date {} and exact {}, found {}"

        for date, lat, lon, zone, exact, expected_result in data:
            result = self._bc.gregorian_date_from_badi_date(
                date, lat=lat, lon=lon, zone=zone, _exact=exact)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, exact, result))

    #@unittest.skip("Temporarily skipped")
    def test_posix_timestamp(self):
        """
        Test that the posix_timestamp method returns the correct Badi
        date with a POSIX timestamp as input.

        The lat and lon at GMT is:
        https://www.latlong.net/place/prime-meridian-greenwich-30835.html#:~:text=Prime%20Meridian%20(Greenwich)%20Lat%20Long,%C2%B0%200'%205.5620''%20W.
        https://www.unixtimestamp.com/
        https://www.calculator.net/time-calculator.html
        """
        utc_coords = (51.477928, -0.001545, 0)
        local_coords = (35.5894, -78.7792, -5.0)
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 1970-01-01T00:00:00 -> UNIX Epoch at UTC
            # Sunset the day before 16:00 lat=51.477928, lon=-0.001545, zone=0
            #                       UTC 12am == (126, 16, 2, 8, 0, 0)
            (0, *utc_coords, False, True, True, (126, 16, 2, 8, 0, 36.2052)),
            #                       UTC 12am == (1, 7, 12, 16, 2, 8, 0, 0)
            (0, *utc_coords, False, False, True,
             (1, 7, 12, 16, 2, 8, 0, 36.2052)),
            # 1969-12-31T23:59:59 This is one second before the POSIX epoch UTC
            (-1, *utc_coords, False, True, True, (126, 16, 2, 8, 0, 35.2044)),
            (-1, *utc_coords, True, True, True,
             (126, 16, 2, 8, 0, 35, 204400)),
            # Tue Oct 28 2025 22:23:00 GMT+0000 Sunset
            (1761690180, *utc_coords, False, True, True,
             (182, 12, 15, 5, 42, 59.3172)),
            (1761690179, *utc_coords, False, True, True,
             (182, 12, 15, 5, 42, 58.3164)),
            # 2024-08-21T14:33:46.246101 -- Sunset 18:56:00-05:00
            # Sunset 18:56:00 - 14:33:46.246101 == 04:22:13.753899
            # 24 - 04:22:13.753899 = 19:37:46.246101 Badi time
            # Should be about 0181-09-03T19:37:46.246101
            # The h, m, & s are counted from the beginning of the Badi day
            # which would be sunset on the previous Gregorian day.
            (1724265226.246101, *local_coords, False, True, True,
             (181, 9, 3, 23, 23, 15.2988)),
            # Just after sunset US/Eastern on (2024, 1, 12, 17, 34, 1, 290939)
            (1736721241.2909386, *local_coords, True, True, False,
             (181, 16, 15, 6, 18, 45, 360000)),
            # Test with zone 3.5 (Tehran Iran) 2024-08-28T00:59:58.549010+3:30
            (1724794198.5490103, *epoch_coords, False, True, True,
             (181, 9, 10, 2, 32, 12.4008)),
            # 2024-08-07T14:04:24-0500 sunset 19:13 EST
            (1723057467.0619307, *local_coords, False, True, True,
             (181, 8, 8, 23, 26, 36.2364)),
            )
        msg = "Expected {} for timestamp {}, found {}"

        for t, lat, lon, zone, us, short, trim, expected_result in data:
            result = self._bc.posix_timestamp(t, lat, lon, zone, us=us,
                                              short=short, trim=trim)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, t, result))

    #@unittest.skip("Temporarily skipped")
    def test_midday(self):
        """
        Test that the midday method returns the middle of the Badi day in
        hours, minutes, and seconds or as a decimal value.
        """
        data = (
            ((1, 1, 1), False, 0.5005841173697263),
            ((1, 1, 1), True, (12, 0, 50.4684)),
            ((181, 11, 4), False, 0.49920915765687823),
            ((181, 11, 4), True, (11, 58, 51.672)),
            ((1, 1, 1, 1, 1), False, 0.5005841173697263),
            ((1, 1, 1, 1, 1), True, (12, 0, 50.4684)),
            )
        msg = "Expected {} for date {} and hms {}, found {}"

        for date, hms, expected_result in data:
            result = self._bc.midday(date, hms=hms)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, hms, result))

    #@unittest.skip("Temporarily skipped")
    def test__trim_hms(self):
        """
        Test that the _trim_hms method correctly trims the secons and
        minutes if zero values.
        """
        data = (
            ((12, 30, 15), (12, 30, 15)),
            ((12, 30, 0), (12, 30)),
            ((12, 0, 0), (12,)),
            ((12, 0, 15), (12, 0, 15)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc._trim_hms(date)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_valid_badi_date(self):
        """
        Test that the _check_valid_badi_date method returns the
        correct Boolean for valid and invalid dates.

        Note: The Boolean below in the data statements determines whether
              or not the data is valid or invalid.
        """
        MIN_K = self._bc.KULL_I_SHAY_MIN
        MAX_K = self._bc.KULL_I_SHAY_MAX
        MIN_Y = self._bc.MINYEAR
        MAX_Y = self._bc.MAXYEAR
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
        err_msg8 = "Invalid second '{}', it must be in the range of [0, 60]."
        err_msg9 = ("Invalid microseconds '{}', it must be in the range of "
                    "[0, 999999].")
        err_msg10 = ("If there is a part day then there can be no hours, "
                     "minutes, or seconds.")
        err_msg11 = ("If there is a part hour then there can be no minutes or "
                     "seconds.")
        err_msg12 = "If there is a part minute then there can be no seconds."
        data = (
            ((1, 1, 1, 1, 1), False, False, ''),  # Non leap year
            ((1, 1, 1), True, False, ''),
            ((1, 10, 3, 1, 1), False, False, ''), # Known leap year
            ((174, 1, 1), True, False, ''),
            ((0, 1, 1, 1, 1), False, False, ''),  # Before Badi epoch
            ((-1, 1, 1, 1, 1), False, False, ''), # Before Badi epoch
            # During Ayyám-i-Há non leap year
            ((1, 10, 2, 0, 1), False, False, ''),
            ((1, 10, 3, 0, 1), False, False, ''), # During Ayyám-i-Há leap year
            #((174, 0, 1), True, False, ''), # During Ayyám-i-Há leap year
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799), False, False, ''),
            # Invalid kull-i-shay
            ((MIN_K-1, 2, 19, 19, 19), False, True, err_msg0.format(MIN_K-1)),
            ((MAX_K+1, 1, 1, 1, 1), False, True, err_msg0.format(MAX_K+1)),
            # Invalid Váḥid
            ((1, 0, 1, 1, 1, 1, 1, 1), False, True, err_msg1.format(0)),
            ((1, 20, 1, 1, 1, 1, 1, 1), False, True, err_msg1.format(20)),
            # Invalid year in Váḥid
            ((1, 10, 0, 1, 1, 1, 0, 0), False, True, err_msg2.format(0)),
            ((1, 10, 20, 1, 1, 1, 0, 0), False, True, err_msg2.format(20)),
            # Invalid short date
            ((MIN_Y-1, 1, 1), True, True, err_msg3.format(MIN_Y-1)),
            ((MAX_Y+1, 1, 1), True, True, err_msg3.format(MAX_Y+1)),
            # Invalid month
            ((1, 10, 10, -1, 1, 1, 0, 0), False, True, err_msg4.format(-1)),
            ((1, 10, 10, 20, 1, 1, 0, 0), False, True, err_msg4.format(20)),
            # Invalid Ayyám-i-Há day
            ((1, 10, 3, 0, 0, 1, 1, 1), False, True, err_msg5.format(0, 0, 5)),
            ((1, 10, 3, 0, 6, 1, 1, 1), False, True, err_msg5.format(6, 0, 5)),
            # Invalid normal day
            ((1, 10, 3, 2, 0, 1, 1, 1), False, True,
             err_msg5.format(0, 2, 19)),
            ((1, 10, 3, 2, 20, 1, 1, 1), False, True,
             err_msg5.format(20, 2, 19)),
            # Invalid hour
            ((1, 10, 3, 2, 1, -1, 1, 1), False, True, err_msg6.format(-1)),
            ((1, 10, 3, 2, 1, 25, 1, 1), False, True, err_msg6.format(25)),
            # Invalid minute
            ((1, 10, 3, 2, 1, 1, -1, 1), False, True, err_msg7.format(-1)),
            ((1, 10, 3, 2, 1, 1, 60, 1), False, True, err_msg7.format(60)),
            # Invalid second
            ((1, 1, 1, 1, 1, -1), True, True, err_msg8.format(-1)),
            ((1, 1, 1, 1, 1, 60), True, True, err_msg8.format(60)),
            # Invalid microsecond
            ((1, 1, 1, 0, 0, 0, -1), True, True, err_msg9.format(-1)),
            ((1, 10, 10, 9, 8, 19, 1, 3, 1532799), False, True,
             err_msg9.format(1532799)),
            # Invalid partial day
            ((1, 10, 3, 2, 1.5, 1, 0, 0), False, True, err_msg10),
            ((1, 10, 3, 2, 1.5, 0, 1, 0), False, True, err_msg10),
            ((1, 10, 3, 2, 1.5, 0, 0, 1), False, True, err_msg10),
            # Invalid partial hour
            ((1, 10, 3, 2, 1, 1.5, 1, 0), False, True, err_msg11),
            ((1, 10, 3, 2, 1, 1.5, 0, 1), False, True, err_msg11),
            # Invalid partial minute
            ((1, 10, 3, 2, 1, 1, 1.5, 1), False, True, err_msg12),
            )

        for b_date, short_in, validity, err_msg in data:
            if validity: # Test for invalid dates
                try:
                    with self.assertRaises(AssertionError) as cm:
                        self._bc._check_valid_badi_date(
                            b_date, short_in=short_in)
                except AssertionError as e:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(
                        f"With date {b_date} and error was not raised, {e}")
                else:
                    message = str(cm.exception)
                    self.assertEqual(err_msg, message)
            elif short_in: # Test for valid short dates
                year, month, day = b_date[:3]
                hour, minute, second, us = self._bc._get_hms(
                    b_date, short_in=short_in)
                cycle = 4 + self._bc._is_leap_year(year) if month == 0 else 19

                for d in range(1, cycle + 1):
                    date = (year, month, d)
                    self._bc._check_valid_badi_date(date, short_in=short_in)
            else: # Test for valid long dates
                kull_i_shay, vahid, year, month, day = b_date[:5]
                hour, minute, second, us = self._bc._get_hms(b_date)
                ly = ((kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year)
                cycle = 4 + self._bc._is_leap_year(ly) if month == 0 else 19

                for d in range(1, cycle + 1):
                    date = (kull_i_shay, vahid, year, month, d)
                    self._bc._check_valid_badi_date(date, short_in=short_in)

    #@unittest.skip("Temporarily skipped")
    def test__is_leap_year(self):
        """
        Test that the _is_leap_year method returns the correct Boolean
        for the given year or long Badi date.
        """
        data = (
            # Start of years
            (173, True, False),                 # 2016
            ((1, 10, 2, 1, 1), True, False),    # (2016, 3, 20)
            (174, True, True),                  # 2017
            ((1, 10, 3, 1, 1), True, True),     # (2017, 3, 20)
            (175, True, False),                 # 2018
            ((1, 10, 4, 1, 1), True, False),    # (2018, 3, 21)
            # End of years
            ((1, 10, 2, 19, 19), True, False), # (2017, 3, 19)
            ((1, 10, 3, 19, 19), True, True),  # (2017, 3, 19)
            ((1, 10, 4, 19, 19), True, False), # (2018, 3, 20)
            )
        msg = "Expected {} for day {}, found {}"

        for date, validity, expected_result in data:
            if isinstance(date, int):
                year = date
            else:
                year = ((date[0] - 1) * 361 + (date[1] - 1) * 19 + date[2])

            if validity:
                result = self._bc._is_leap_year(year)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, date, result))
            else:
                with self.assertRaises(AssertionError) as cm:
                    self._bc._is_leap_year(year)

                message = str(cm.exception)
                self.assertEqual(expected_result.format(date), message)

    #@unittest.skip("Temporarily skipped")
    def test__days_in_year(self):
        """
        Test that the _days_in_year method returns the correct number of
        days in the current year.
        """
        data = (
            (1, 366),
            (181, 365),
            )
        msg = "Expected {} for year {}, found {}"

        for year, expected_result in data:
            result = self._bc._days_in_year(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test__get_hms(self):
        """
        Test that the _get_hms method parses the hours, minutes, and
        seconds correctly for either the short or long form Badi date.
        Test both the long and short for od the Badi Date.
        """
        data = (
            ((1, 1, 1, 1, 1, 18, 16), False, (18, 16, 0, 0)),
            ((1, 1, 1, 18, 16), True, (18, 16, 0, 0)),
            )
        msg = "Expected {} for date {} amd short_in {}, found {}"

        for date, short_in, expected_result in data:
            result = self._bc._get_hms(date, short_in=short_in)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, short_in, result))

    #@unittest.skip("Temporarily skipped")
    def test__adjust_date(self):
        """
        Test that the _adjust_date method returns the corrected date based
        on the JD and the original date.
        """
        err_msg0 = "Cannot set more than one of fraction, us, or rtd to True."
        local_coords = (35.5894, -78.7792, -5.0)
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        data = (
            # Test where the JD is < the sunset.
            # Stage 1 -- 2024-03-19T16:48:00 sunset = 18:26:00 EST
            # Should be about 19.932014
            (2460387.2, (181, 1, 1), *local_coords, True, False, False, False,
             (180, 19, 19.942137)),
            # Stage 2 -- 2024-04-07T16:48:00 sunset = 18:42 EST
            # Should be about 19.0.919403
            (2460406.2, (181, 2, 1), *local_coords, True, False, False, False,
             (181, 1, 19.919974)),
            # Stage 3
            (2460733.2, (181, 19, 1), *local_coords, True, False, False, False,
             (181, 0, 4.965281)),
            # Stage 4
            (2460729.2, (181, 0, 1), *local_coords, True, False, False, False,
             (181, 18, 19.970238)),
            # Stage 5 -- 2025-06-28T19:12:00 sunset = 19:35 ?
            (2460853.3, (182, 6, 7), *epoch_coords, False, False, True,
             False, (182, 6, 7)),
            # Stage 6 -- 2025-02-28T18:11:34.5 after sunset = 18:10 EST
            (2460733.2580386614, (181, 19, 1), *local_coords, False, False,
             False, False, (181, 19, 1, 0, 33, 34.524)),
            (2460387.3, (181, 1, 1), *local_coords, False, False, False, False,
             (181, 1, 1, 1, 0, 40.5144)),
            # Test fraction -- Stage 6
            (2460733.3, (181, 19, 1), *local_coords, True, False, False, False,
             (181, 19, 2.065278)),
            # Test 1st day of the year for a few years
            # 1845-03-20T18:16:24.4416 -> 2395009.261394 Stage 6
            (2395009.261972, (2, 1, 1), *epoch_coords, False, False, False,
             False, (2, 1, 1, 0, 5, 7.5156)),
            # 1863-03-20T18:16:05.6928 -> 2401583.261177 Stage 6
            (2401583.261756, (20, 1, 1), *epoch_coords, False, False, False,
             False, (20, 1, 1, 0, 5, 26.2068)),
            # 2015-03-20T18, 16, 7.0104 -> 2457102.262356 Stage 6
            (2457102.262356, (172, 1, 1), *epoch_coords, False, False,
             False, False, (172, 1, 1, 0, 2, 46.0392)),
            # 2024-03-19T18:15, 57.2688 -> 2460387.26108 Stage 6
            (2460389.262825, (181, 1, 1), *epoch_coords, False, False, False,
             False, (181, 1, 1, 0, 3, 45.6012)),
            # Test with microseconds
            # JD from jd_from_badi_date() Stage 6
            (2460387.262245, (181, 1, 1), *epoch_coords, False, True, False,
             False, (181, 1, 1, 0, 6, 18, 666000)),
            # JD from jd_from_gregorian_date() Stage 6
            (2460387.262234, (181, 1, 1), *epoch_coords, False, True, False,
             False, (181, 1, 1, 0, 6, 17, 715600)),
            # 1844-03-18T00:00:00 should be next day Stage 6
            (2394641.5, (0, 19, 18), *epoch_coords, False, False, True, False,
             (0, 19, 19)),
            # 1844-03-19T00:00:00 should be next day Stage 6
            (2394642.5, (0, 19, 19), *epoch_coords, False, False, True, False,
             (1, 1, 1)),
            # 1844-03-20T00:00:00 should be next day Stage 6
            (2394643.5, (1, 1, 1), *epoch_coords, False, False, True, False,
             (1, 1, 2)),
            # 2024-03-20T00:00:00 sunset 18:17:00 JD is exactly UTC midnight.
            # should be about 05:43:00
            (2460387.5, (181, 1, 1), *epoch_coords, False, True, False,
             False, (181, 1, 1, 5, 48, 40, 698000)),
            # 2024-03-20T02:24:00 sunset 18:17:00 JD is after UTC midnight.
            # should be about 10:08:00
            (2460387.6, (181, 1, 1), *epoch_coords, False, True, False,
             False, (181, 1, 1, 8, 12, 40, 698000)),
            # Error conditions
            (2460733.2, (181, 19, 1), *local_coords, True, True, False, True,
             err_msg0),
            )
        msg = "Expected {} for jd {} and date {}, found {}"

        for (jd, date, lat, lon, zone, fraction, us,
             rtd, validity, expected_result) in data:
            if validity:
                try:
                    result = self._bc._adjust_date(jd, date, lat, lon, zone,
                                                   fraction=fraction, us=us)
                except AssertionError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(
                        f"With {jd} and {date} an error is "
                        f"not raised, with result {result}.")
            else:
                result = self._bc._adjust_date(jd, date, lat, lon, zone,
                                               fraction=fraction, us=us,
                                               rtd=rtd)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, jd, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__day_length(self):
        """
        Test that the _day_length method returns the hours, minutes, and
        seconds of a day.
        """
        local_coords = (35.5894, -78.7792)
        epoch_coords = self._bc._BAHAI_LOCATION[:2]
        utc_coords = (51.477928, -0.001545) # Greenwich
        data = (
            # BADI epoch location
            ((-1842, 1, 1), epoch_coords, (24, 0, 45.198)),
            ((-1842, 19, 19), epoch_coords, (24, 0, 45.2448)),
            ((181, 1, 1), epoch_coords, (24, 0, 50.3064)),
            ((181, 19, 19), epoch_coords, (24, 0, 50.4864)),
            # Arbitrary location
            ((-1842, 1, 1), local_coords, (24, 0, 45.0432)),
            ((-1842, 19, 19), local_coords, (24, 0, 45.0936)),
            # Earth seasons for Gregorian year 2024 zone -05:00.
            # Perihelion (2024, 1, 2, 19, 38)
            ((180, 16, 4, 2, 34, 36.336), local_coords, (24, 0, 48.8304)),
            # Vernal Equinox (2024, 3, 19, 22, 6)
            ((181, 1, 1, 3, 49, 12.288), local_coords, (24, 0, 50.0004)),
            # Summer Solstice (2024, 6, 20, 15, 51)
            ((181, 5, 18, 20, 26, 54.0096), local_coords, (24, 0, 10.4904)),
            # Aphelion (2024, 7, 5, 0, 6)
            ((181, 6, 13, 4, 41, 49.1712), local_coords, (23, 59, 47.058)),
            # Fall Equinox (2024, 9, 22, 7, 44)
            ((181, 10, 16, 13, 42, 37.3824), local_coords, (23, 58, 32.2248)),
            # Winter Solstice (2024, 12, 21, 4, 20)
            ((181, 15, 11, 11, 24, 40.6944), local_coords, (24, 0, 31.5864)),
            # Earth seasons for Gregorian year 2024 zone -00:00.
            # Perihelion (2024, 1, 3, 0, 38)
            ((180, 16, 4, 7, 34, 36.3072), utc_coords, (24, 1, 8.6196)),
            # Vernal Equinox (2024, 3, 20, 3, 6)
            ((181, 1, 1, 8, 49, 12.3456), utc_coords, (24, 1, 41.412)),
            # Summer Solstice (2024, 6, 20, 20, 51)
            ((181, 5, 18, 1, 26, 53.9808), utc_coords, (24, 0, 11.592)),
            # Aphelion (2024, 7, 5, 5, 6)
            ((181, 6, 13, 9, 41, 49.1424), utc_coords, (23, 59, 25.2204)),
            # Fall Equinox (2024, 9, 22, 12, 44)
            ((181, 10, 17, 18, 44, 5.4816), utc_coords, (23, 57, 42.0912)),
            # Winter Solstice (2024, 12, 21, 9, 20)
            ((181, 15, 11, 16, 24, 40.6656), utc_coords, (24, 0, 35.6652)),
            )
        msg = "Expected {} for date {}, and location {}, found {}"

        for (date, location, expected_result) in data:
            jd = self._bc.jd_from_badi_date(date, *location)
            result = self._bc._day_length(jd, *location)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, jd, location, result))
