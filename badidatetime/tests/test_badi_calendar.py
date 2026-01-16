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
            ((1, 1, 1), None, None, None, (18, 15, 45.9792)),
            # Should be 2024-03-19T18:13:00
            ((180, 19, 19), lat, lon, zone, (18, 15, 5.9976)),
            # Should be 2064-03-19T18:16:00
            ((221, 1, 1), lat, lon, zone, (18, 16, 10.452)),
            # Should be 2024-04-20T19:53:00 DST in Raleigh NC
            ((181, 2, 13), 35.7796, -78.6382, -4, (19, 51, 47.2032)),
            # Should be 2024-07-22T20:26:00 DST in Raleigh NC
            ((181, 7, 11), 35.7796, -78.6382, -4, (20, 27, 9.054)),
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
            (1, lat, lon, zone, False, True, (1844, 3, 19.760948839597)),
            # 1844-03-19T18:16:00+03:30
            (1, lat, lon, zone, True, True, (1844, 3, 19, 18, 15, 45.9792)),
            # 2024-03-19T18:26:00-05:00
            (181, 35.7796, -78.6382, -5, True, True,
             (2024, 3, 19, 18, 25, 57.5004)),
            # 2025-03-19T18:16:00+03:30
            (182, lat, lon, zone, False, True, (2025, 3, 19.760931049474)),
            # 2026-03-20T18:16:00+03:30
            (183, lat, lon, zone, False, True, (2026, 3, 20.761374630965)),
            # The following years are the ones that had errors.
            # 2021-03-19T18:16:00+03:30
            (178, lat, lon, zone, False, True, (2021, 3, 19.760911472142)),
            # 2030-03-20T18:16:00:+03:30
            (187, lat, lon, zone, False, True, (2030, 3, 19.760807621758)),
            # 2034-03-19T18:16:00+03:30
            (191, lat, lon, zone, False, True, (2034, 3, 19.760819753632)),
            # 2038-03-19T18:16:00+03:30
            (195, lat, lon, zone, False, True, (2038, 3, 19.760831517633)),
            # 2054-03-19T18:16:00+03:30
            (211, lat, lon, zone, False, True, (2054, 3, 19.76089622872)),
            # 2059-03-20T18:16:21.590400
            (216, lat, lon, zone, False, True, (2059, 3, 19.760767274071)),
            # 2063-03-19T18:16:23.318400
            (220, lat, lon, zone, False, True, (2063, 3, 19.760787492618)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 3, 19.760948839597)),
            # 1993-03-19T18:16:24.960000 Test sunset before Vernal Equinox
            (150, lat, lon, zone, False, True, (1993, 3, 19.760808784515)),
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
            (1, lat, lon, zone, False, True, (1844, 4, 19.778637580574)),
            (1, lat, lon, zone, True, True, (1844, 4, 19, 18, 41, 14.2872)),
            # 0181-02-13T19:41:00-04:00 DST at Raleigh NC, USA
            (181, 35.7796, -78.6382, -4, False, True,
             (2024, 4, 19.827629667241)),
            (181, 35.7796, -78.6382, -4, True, True,
             (2024, 4, 19, 19, 51, 47.2032)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 4, 19.778637580574)),
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
        BADI_COORDS = (35.69435, 51.288701, 3.5)
        GMT_COORDS = (51.477928, -0.001545, 0.0)
        data = (
            # Real epoch at sunset 01-01-01T00:00:00 B.E.
            # 1583-03-20T18:16:00+03:30
            ((-260, 1, 1), BADI_COORDS, 2299315.261195329),
            # 1844-03-19T12:00:00+03:30
            ((0, 19, 19, 17, 43.399901), BADI_COORDS, 2394642.9988401216),
            # 1844-03-19T18:16:00+03:30
            ((1, 1, 1), BADI_COORDS, 2394643.2609488396),
            # 1863-03-18T18:14:00+03:30
            ((19, 19, 19), BADI_COORDS, 2401582.2605884),
            # POSIX Timestamp epoch
            ((126, 16, 2, 8, 0, 30.6684), GMT_COORDS, 2440585.5006733374),
            # A day in Ayyám-i-Há 2022-02-24T17:57:55.152000
            ((178, 0, 1), BADI_COORDS, 2459633.2465897757),
            # 2024-03-18T18:15:00+03:30
            ((180, 19, 19), BADI_COORDS, 2460386.2604860975),
            # 2024-04-27T18:48:00+03:30
            ((181, 3, 2), BADI_COORDS, 2460426.2835964654),
            # 2024-05-15T20:03:00+03:30
            # Sunset = 20:03
            ((181, 4, 1), BADI_COORDS, 2460444.293950542),
            # Test one date for each coefficient.
            ((-1842, 1, 1), BADI_COORDS, 1721501.259565802),
            ((-1815, 1, 1), BADI_COORDS, 1731363.2598133588),
            ((-1799, 1, 1), BADI_COORDS, 1737207.2598888942),
            ((-1783, 1, 1), BADI_COORDS, 1743051.2599653169),
            ((-1747, 1, 1), BADI_COORDS, 1756199.2596033823),
            ((-1699, 1, 1), BADI_COORDS, 1773731.260346055),
            ((-1519, 1, 1), BADI_COORDS, 1839475.2616925763),
            )
        msg = "Expected {} for date {}, found {}"

        for date, coords, expected_result in data:
            result = self._bc.jd_from_badi_date(date, *coords)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_jd(self):
        """
        Test that the badi_date_from_jd method returns the correct Badi date.

        Run: ./contrib/misc/gregorian_jd_tests.py -jS<start_date> -E<end_date>
        to find the jd values below. It's best to do a year at a time.

        .. note::

           1. All JDs are in UT1 (within 0.9 seconds of UTC) time. We check
              if the code changes it to the given time zone correctly.
           2. Since all JDs are in UT1 time they need to be changed to the
              time zone that the inverse method jd_from_badi_date() would
              return.
           3. Unless otherwise noted all test are Stage 1.
        """
        err_msg0 = "Cannot set more than one of fraction, us, or rtd to True."
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_coords = (35.5894, -78.7792, -5.0)
        gmt_coords = (51.477928, -0.001545, 0)
        data = (
            # Stage 1
            # 1844-03-19T00:00:00Z -> 1844-03-19T03:30:00.0+03:30
            # Sunset day before = 18:15 -> 24:00 - 18:15 = 05:45 ->
            # 05:45 + 03:30 = Badi time 09:15 -> 0000-19-19T09:15:00
            (2394643.5, *epoch_coords, False, True, True, False, False,
             (0, 19, 19, 9, 15, 4.2084)),
            # 1844-03-19T00:00:00Z -> 1844-03-19T03:30:00.0+03:30
            # Sunset day before = 18:15 -> 24:00 - 18:15 = 05:45 ->
            # 05:45 + 03:30 = Badi time 09:15
            (2394642.5, *epoch_coords, False, True, True, False, True,
             (0, 19, 18)),
            # 1844-03-18T00:00:00Z -> 1844-03-18T03:30:00.0+03:30
            # Sunset day before = 18:14 -> 24:00 - 18:14 = 05:46 ->
            # 05:46 + 03:30 = Badi time 09:16 -> 0000-19-17T09:16:00
            (2394641.5, *epoch_coords, False, True, True, False, True,
             (0, 19, 17)),
            # 1969-12-30T16:00:27.5Z -> 1969-12-30T11:00:27.5-05:00
            # Sunset day before = 17:15 -> 24:00 - 17:15 = 06:15 ->
            # 06:15 + 11:00:27.5 = Badi time 17:45:27.5
            (2440584.1669850457, *local_coords, True, True, True, False, False,
             (126, 15, 18, 17, 50, 32, 928000)),
            # Stage 2
            # 2024-08-21T19:33:46.246101Z -> 2024-08-21T14:33:46.246101-05:00
            # Sunset day before = 18:58 -> 24:00 - 18:58 + 14:33:46.246101
            # Badi time = 19:35:46.246101
            (2460542.2734519225, *local_coords, True, True, True, False, False,
             (181, 9, 3, 18, 36, 11, 898000)),
            # 2440585.5 -- 1970-01-01T:00:00:00Z
            # Sunset day before = 16:00 -> 24:00 - 16:00 = 08:00
            # 08:00 + 00:00 = Badi time = 08:00:00
            (self._bc._POSIX_EPOCH, *gmt_coords, False, True, True, False,
             False, (126, 16, 2, 8, 0, 30.672)),
            (self._bc._POSIX_EPOCH, *local_coords, False, True, True, False,
             False, (126, 16, 2, 1, 48, 41.256)),
            # 1849-03-19T18:14:39.7716Z
            # Sunset day before = 18:11 -> 18:14 - 18:11 = 00:03
            (2396469.5, *gmt_coords, False, True, True, False, False,
             (5, 19, 19, 5, 50, 23.5824)),
            # Stage 3
            # 2025-11-30T22:00:00.0Z -> 2025-12-01T01:30.00.0+03:30
            # Sunset day before = 16:52 -> 24:00 - 16:52 = 07:08 ->
            # 07:08 + 01:30 = Badi date & time = 0182-14-10T08:38:00
            (2461008.416667, *epoch_coords, True, True, True, False, False,
             (182, 14, 10, 8, 38, 11, 133600)),
            # 0001-03-20T18:10:43.8Z -> 0001-03-20T21:40:43.8+03:30
            # Sunset = 18:17 -> 21:40:43.8 - 18:17 = Badi time 03:23:43.8
            (1721502.2574517454, *epoch_coords, False, True, True, False,
             False, (-1842, 1, 2, 3, 26, 57.3468)),
            # 0001-04-08T18:40:46.5Z -> 0001-04-08T22:10:46.5+03:30
            # Sunset = 18:27 Tehran -> Badi time = 03:43:46.5
            (1721521.2783162275, *epoch_coords, False, True, True, False,
             False, (-1842, 2, 2, 3, 42, 55.026)),
            # 0002-02-24T17:30:50.7Z -> 0002-02-24T21:00:50.7+03:30
            # Sunset = 17:57 Tehran -> Badi time = 03:03:50.7
            (1721843.2297538682, *epoch_coords, False, True, True, False,
             False, (-1842, 0, 1, 3, 6, 46.4148)),
            # 0002-02-25T17:32:33.0Z -> 0002-02-25T21:02:33.0+03:30
            # Sunset = 17:58 -> Badi time = 03:04:33
            (1721844.2309374965, *epoch_coords, False, True, True, False,
             False, (-1842, 0, 2, 3, 7, 35.9688)),
            # 0002-02-26T17:34:14.9Z -> 0002-02-26T21:04:14.9+03:30
            # Sunset = 17:59 -> Badi time = 03:05:14.9
            (1721845.2321173307, *epoch_coords, False, True, True, False,
             False, (-1842, 0, 3, 3, 8, 25.5804)),
            # 0002-03-02T17:40:59.4Z -> 0002-03-02T21:10:59.4+03:30
            # Sunset = 18:02 -> Badi time = 03:08:59.4
            (1721849.236798515, *epoch_coords, False, True, True, False, False,
             (-1842, 19, 2, 3, 11, 44.4768)),
            # 0002-03-06T17:47:38.6Z -> 0002-03-06T21:17:38.2+03:30
            # Sunset = 18:05 -> Badi time = 03:12:38.2
            (1721853.24141933, *epoch_coords, False, True, True, False, False,
             (-1842, 19, 6, 3, 15, 3.942)),
            # 1583-03-20T18:13:54.4Z -> 1583-03-20T21:43:54.4+03:30
            # Sunset = 18:16 -> Badi time = 03:27:54.4
            (2299315.2596576316, *epoch_coords, False, True, True, False,
             False, (-260, 1, 1, 3, 28, 36.588)),
            # 1844-03-19T18:13:21.1Z -> 1844-03-19T21:43:21.1+03:30
            # Sunset = 18:16 -> Badi time = 03:27:21.1
            (2394643.2592724077, *epoch_coords, False, True, True, False,
             False, (1, 1, 1, 3, 28, 25.3452)),
            # 1844-03-19T18:11:40.057Z -> 1844-03-19T21:41:40.5+03:30
            # Sunset = 18:16 -> Badi time = 03:25:40.5
            (2394643.2581025115, *epoch_coords, False, True, True, False,
             False, (1, 1, 1, 3, 26, 44.2644)),
            # 1845-03-20T18:14:37.5Z -> 1845-03-20T21:44:37.5+03:30
            # Sunset = 18:17 -> Badi time = 03:27:37.5
            (2395009.2601561244, *epoch_coords, False, True, True,  False,
             False, (2, 1, 1, 3, 29, 3.75)),
            # 1863-03-20T18:14:00.3Z -> 1863-03-20T21:44:00.0+03:30
            # Sunset = 18:16 -> Badi time = 03:28:00
            (2401583.2597260755, *epoch_coords, False, True, True, False,
             False, (20, 1, 1, 3, 28, 45.4944)),
            # 1844-03-19T18:17:26.6Z -> 1844-03-19T21:47:26.6+03:30
            # Sunset = 18:16 -> 21:47:26.6 - 18:16 = Badi time = 03:31:26.6
            (2394643.262113, *epoch_coords, False, True, True, False, True,
             (1, 1, 1)),
            # Previous day test
            # 2025-12-01T00:00:00.0Z -> 2025-11:30T19:00:00.0-05:00
            # Sunset = 17:02 -> Badi date & time 0182-14-10T01:58:00
            (2461008.5, *local_coords, True, True, True, False, False,
             (182, 14, 10, 1, 57, 43, 866000)),
            # 2015-03-20T18:14:10.1Z -> 2015-03-20T21:44:10.1+03:30
            # Sunset = 18:16 -> Badi time = 03:28:10.1
            (2457100.2598390104, *epoch_coords, False, True, True, False,
             False, (172, 1, 1, 3, 28, 54.2928)),
            # 2024-03-19T18:13:51.0Z -> 2024-03-19T21:43:51.0+03:30
            # Sunset = 18:16 -> Badi time = 03:27:51
            (2460387.2596184844, *epoch_coords, False, True, True, False,
             False, (181, 1, 1, 3, 28, 45.0372)),
            # 2024-04-20T19:07:26.7Z -> 2024-04-20T22:37:26.7+03:30
            #Sunset = 18:42 -> Badi time = 03:55:26.7
            (2460419.296837184, *epoch_coords, False, True, True,  False,
             False, (181, 2, 14, 3, 55, 46.848)),
            # 2022-02-24T17:33:05.4Z -> 2022-02-24T21:03:05.4+03:30
            # Sunset = 17:55 -> Badi time = 03:08:05.4
            (2459633.231312508, *epoch_coords, False, True, True, False, False,
             (178, 0, 1, 3, 8, 56.868)),
            # 2022-03-01T17:41:57.3Z -> 2022-03-01T21:11:57.3+03:30
            # Sunset = 18:00 -> Badi time = 03:11:57.3
            (2459638.2374685165, *epoch_coords, False, True, True, False,
             False, (178, 19, 1, 3, 13, 7.9752)),
            # 2022-03-02T17:43:42.8Z -> 2022-03-02T21:13:42.8+03:30
            # Sunset = 18:01 -> Badi time = 03:12:42.8
            (2459639.238690004, *epoch_coords, False, True, True, False, False,
             (178, 19, 2, 3, 13, 58.3392)),
            # 2024-05-12T19:43:00.9Z -> 2024-05-12T23:13:00.9+03:30
            # Sunset = 19:01 -> Badi time = 04:12:00.9
            (2460441.3215382686, *epoch_coords, False, True, True, False,
             False, (181, 3, 17, 4, 12, 57.582)),
            # 2024-05-14T19:46:01.7Z -> 2024-05-14T23:16:01.7+03:30
            # Sunset = 19:02 -> Badi time = 04:14:01.7
            (2460443.32363071, *epoch_coords, False, False, True, False, False,
             (1, 10, 10, 3, 19, 4, 14, 20.7888)),
            # 2024-05-14T19:46:01.7Z -> 2024-05-14T23:16:01.7+03:30
            # Sunset = 19:02 -> Badi time = 04:14:01.7
            (2460443.32363071, *epoch_coords, True, False, True, False, False,
             (1, 10, 10, 3, 19, 4, 14, 20, 788800)),
            # 2024-07-17T20:07:03.4Z -> 2024-07-17T23:37:03.4+03:30
            # Sunset = 19:20 -> Badi time = 04:17:1.7
            (2460507.3382339347, *epoch_coords, False, True, True, False,
             False, (181, 7, 7, 4, 16, 42.4812)),
            # 2024-07-17T20:07:03.4Z -> 2024-07-17T23:37:03.4+03:30
            # Sunset = 19:20 -> Badi date & time = 0181-07-07.178492
            (2460507.3382339347, *epoch_coords, False, True, True, True, False,
             (181, 7, 7.178269)),
            # 2024-03-19T18:13:51.0Z -> 2024-03-19T21:43:51.0+03:30
            # Sunset 18:16 -> Badi time = 03:27:51
            (2460387.2596184844, *epoch_coords, False, True, True, False,
             False, (181, 1, 1, 3, 28, 45.0372)),
            # rtd and long format
            (2394985.2486724695, *epoch_coords, False, False, True, False,
             True, (1, 1, 1, 0, 1)),
            # Use default coordinents
            (2394985.2486724695, None, None, None, False, False, True, False,
             True, (1, 1, 1, 0, 1)),
            # us and fraction
            (0, 0, 0, 0, True, True, False, True, False, err_msg0),
            # us and rtd
            (0, 0, 0, 0, True, True, False, False, True, err_msg0),
            # fraction and rtd
            (0, 0, 0, 0, False, True, False, True, True, err_msg0),
            )
        msg = "Expected {} for jd {} for lat {}, lon {}, and zone {}, found {}"

        for (jd, lat, lon, zone, us, short, trim,
             fraction, rtd, expected_result) in data:
            if isinstance(expected_result, str):
                with self.assertRaises(AssertionError) as cm:
                    self._bc.badi_date_from_jd(
                        jd, lat, lon, zone, us=us, short=short, trim=trim,
                        fraction=fraction, rtd=rtd)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                if zone is None:
                    jd0 = jd
                else:
                    jd0 = self._bc._local_zone_correction(jd, zone,
                                                          mod_jd=True)

                result = self._bc.badi_date_from_jd(
                    jd0, lat, lon, zone, us=us, short=short, trim=trim,
                    fraction=fraction, rtd=rtd)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, jd, lat, lon, zone, result))

    #@unittest.skip("Temporarily skipped")
    def test__adjust_date(self):
        """
        Test that the _adjust_date method correctly fixes the date and time.
        The date parameters below can be found in the 'Stage 0' print
        statement from the badi_date_from_jd test above.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_coords = (35.5894, -78.7792, -5.0)
        gmt_coords = (51.477928, -0.001545, 0)
        data = (
            # Stage 1
            # 1844-03-19T00:00:00Z -> 1844-03-19T03:30:00.0+03:30
            # Sunset day before = 18:15 -> 24:00 - 18:15 = 05:45 ->
            # 05:45 + 03:30 = Badi time 09:15
            (2394642.5, (0, 19, 19), *epoch_coords,
             (0, 19, 18, 9, 15, 54.5616)),
            # 1844-03-18T00:00:00Z -> 1844-03-18T03:30:00.0+03:30
            # Sunset day before = 18:14 -> 24:00 - 18:14 = 05:46 ->
            # 05:46 + 03:30 = Badi time 09:16 -> 0000-19-17T09:16:00
            (2394641.5, (0, 19, 18), *epoch_coords,
             (0, 19, 17, 9, 16, 45.0912)),
            # The previous JD when adding -5.0
            # 1969-12-30T16:00:27.5Z -> 1969-12-30T11:00:27.5-05:00
            # Sunset day before = 17:11 -> 24:00 - 17:11 = 06:49 ->
            # 06:49 + 11:00:27.5 = Badi time 17:49:27.5
            (2440584.1669850457, (126, 15, 19), *local_coords,
             (126, 15, 18, 17, 50, 32.928)),
            # 2024-08-21T18:33:46.2Z -> 2024-08-21T13:33:46.2-05:00
            # Sunset day before = 18:58 -> 24:00 - 18:58 = 05:02
            # 05:02 + 13:33:46.2 = Badi date & time 0181-09-03T18:35:46.2
            (2460542.2734519225, (181, 9, 4), *local_coords,
             (181, 9, 3, 18, 36, 11.898)),
            # Stage 1a
            # 1844-03-19T14:46:00Z -> 1844-03-19T18:16:00.0+03:30
            # Sunset = 18:16 -> 18:16 - 18:16 = 00:00:00
            (2394643.115278, (1, 1, 1), *epoch_coords,
             (1, 1, 1, 0, 1, 4.2276)),
            # Stage 2
            # 2440585.5 -- 1970-01-01T:00:00:00Z
            # Sunset day before = 16:00 -> 24:00 - 16:00 = 08:00
            # 08:00 + 00:00 = Badi time = 08:00:00
            (self._bc._POSIX_EPOCH, (126, 16, 2), 51.477928, -0.001545, 0,
             (126, 16, 2, 8, 0, 30.672)),
            # 2025-11-30T22:00:00.0Z -> 2025-12-01T01:30.00.0+03:30
            # Sunset day before = 16:52 -> 24:00 - 16:52 = 07:08 ->
            # 07:08 + 01:30 = Badi date & time = 0182-14-10T08:38:00
            (2461008.416667, (182, 14, 10), *epoch_coords,
             (182, 14, 10, 8, 38, 11.1336)),
            # 1849-03-19T18:14:39.7716Z
            # Sunset day before = 18:11 -> 18:14 - 18:11 = 00:03
            (2396469.5, (5, 19, 19), *gmt_coords, (5, 19, 19, 5, 50, 23.5824)),
            # Stage 3
            # 0001-03-20T18:10:43.8Z -> 0001-03-20T21:40:43.8+03:30
            # Sunset = 18:17 -> 21:40:43.8 - 18:17 = Badi time 03:23:43.8
            (1721502.2574517454, (-1842, 1, 2), *epoch_coords,
             (-1842, 1, 2, 3, 26, 57.3468)),
            # 2025-01-12T22:34:01 -> 2025-01-12T17:34:01-05:00
            # Sunset 17:22 -> 17:34:01 - 17:22 = Badi time 01:00:12.01
            (2460686.440292719197, (181, 16, 15), *local_coords,
             (181, 16, 15, 0, 12, 8.8128)),
            # 1844-03-19T18:18:15.6Z -> 1844-03-19T21:48:15.6+03:30
            # Sunset = 18:16 -> 21:48:15.6 - 18:16 = 03:32:15.6
            (2394643.262681068, (0, 19, 20), *epoch_coords,
             (1, 1, 1, 3, 33, 19.8504)),
            # Tests for the day_before() closure.
            # 2 <= month <= 18
            (1721805.6458333333, (-1842, 17, 1), *epoch_coords,
             (-1842, 16, 19, 13, 42, 27.2628)),
            # month == 19
            (1721848.6458333333, (-1842, 19, 1), *epoch_coords,
             (-1842, 0, 5, 13, 1, 35.9184)),
            # month == 0
            (1721843.6458333333, (-1842, 0, 1), *epoch_coords,
             (-1842, 18, 19, 13, 5, 55.68)),
            # Month 1 -> Month 19
            (1721867.6458333333, (-1841, 1, 1), *epoch_coords,
             (-1842, 19, 19, 12, 46, 24.5316)),
            # Tests for the day_after() closure.
            # 1 <= month <= 17
            (2394662.272353196, (1, 1, 20), *epoch_coords,
             (1, 2, 1, 3, 31, 38.0928)),
            # month == 18
            (2394985.2473844443, (1, 18, 20), *epoch_coords,
             (1, 0, 1, 3, 31, 52.6404)),
            # month == 0
            (2394990.2505755094, (1, 0, 20), *epoch_coords,
             (1, 19, 1, 3, 31, 49.2852)),
            # Month 19
            (2394643.3, (0, 19, 20), *epoch_coords,
             (1, 1, 1, 4, 27, 4.2084)),
            )
        msg = "Expected {} for jd {}, date {}, found {}"

        for jd, date, lat, lon, zone, expected_result in data:
            jd0 = self._bc._local_zone_correction(jd, zone, mod_jd=True)
            y, m, d, f = self._bc._adjust_date(jd0, *date, lat, lon, zone)
            result = (y, m, d, *self._bc._hms_from_decimal_day(f))
            self.assertEqual(expected_result, result, msg.format(
                expected_result, jd, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_short_date_from_long_date(self):
        """
        Test that the short_date_from_long_date method returns the correct
        (year, month, day, hour, minute, seconds, us) date.
        """
        data = (
            # 1844-03-20T00:00:00
            ((1, 1, 1, 1, 1), False, (1, 1, 1, 0, 0, 0, 0)),
            ((1, 1, 1, 1, 1), True, (1, 1, 1)),
            # 2024-04-20T20:17:45
            ((1, 10, 10, 2, 14, 20, 17, 45), False,
             (181, 2, 14, 20, 17, 45, 0)),
            # 1844-03-19T00:00:00 Before the Badi epoch
            ((0, 19, 19, 19, 19), False, (0, 19, 19, 0, 0, 0, 0)),
            # 1484-03-11T00:00:00 Before the Badi epoch
            ((0, 1, 1, 1, 1), False, (-360, 1, 1, 0, 0, 0, 0)),
            # 1843-03-21T00:00:00
            ((0, 19, 18, 1, 1), False, (-1, 1, 1, 0, 0, 0, 0)),
            # 1444-05-17T00:00:00
            ((-1, 17, 18, 4, 3), False, (-400, 4, 3, 0, 0, 0, 0)),
            # 1483-03-12T00:00:00
            ((-1, 19, 19, 1, 1), False, (-361, 1, 1, 0, 0, 0, 0)),
            ((-2, 19, 19, 1, 1), False, (-722, 1, 1, 0, 0, 0, 0)),
            # 2024-08-27T13:37:58.651870-4:00
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799), False,
             (181, 9, 8, 19, 1, 3, 532799)),
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799), True,
             (181, 9, 8, 19, 1, 3, 532799)),
            # Overage on any date part.
            ((1, 10, 10, 9, 8, 19, 1, 60.5), True, (181, 9, 8, 19, 2, 59.5)),
            ((1, 10, 10, 9, 8, 19, 59, 60.5), True, (181, 9, 8, 20, 59, 59.5)),
            ((1, 10, 10, 9, 8, 23, 59, 60.5), True, (181, 9, 9, 23, 59, 59.5)),
            ((1, 10, 10, 9, 19, 23, 59, 60.5), True,
             (181, 10, 19, 23, 59, 59.5)),
            # Normal month 1 - 19
            ((1, 10, 10, 19, 19, 23, 59, 60.5), True,
             (182, 1, 19, 23, 59, 59.5)),
            # Month 0
            ((1, 10, 10, 0, 4, 23, 59, 60.5), True,
             (181, 19, 1, 23, 59, 59.5)),  # Not leap year
            ((1, 10, 11, 0, 5, 23, 59, 60.5), True,
             (182, 19, 1, 23, 59, 59.5)),  # Leap year
            )
        msg = "Expected {} for date {}, and trim {}, found {}"

        for date, trim, expected_result in data:
            result = self._bc.short_date_from_long_date(date, trim=trim)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, trim, result))

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

           1. All Gregorian dates are assumed to be in UT time. They get
              converted to the lat, lon, and zone arguments or the default.
              See first example below.
           2. https://www.timeanddate.com/sun/iran/tehran?month=3&year=1844
           3. https://gml.noaa.gov/grad/solcalc/
           4. https://www.calculatorsoup.com/calculators/time/sunrise_sunset.php
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        # local_coords = (35.5894, -78.7792, -5.0)
        data = (
            # 1844-03-19T18:16:36.0048 -> 0001-01-01T00:00:00+03:30
            ((1844, 3, 19, 18, 16, 36.0048), epoch_coords, False, False, True,
             True, (1, 1, 1, 1, 1, 0, 45, 54.234)),
            # 1844-03-19T21:41:40.05+03:30
            # Sunset = 18:16 -> 21:41:40.05 - 18:16 = 03:25:40.05
            ((1844, 3, 19, 21, 41, 40.05), epoch_coords, False, False, True,
             True, (1, 1, 1, 1, 1, 4, 10, 8.2452)),
            ((1844, 3, 19, 21, 41, 40.05), epoch_coords, False, True, True,
             True, (1, 1, 1, 4, 10, 8.2452)),
            # 2024-05-14T19:05:00+03:30
            # Sunset = 19:02 -> 19:05 - 19:02 = 00:03
            ((2024, 5, 14, 19, 5), epoch_coords, False, False, True, True,
             (1, 10, 10, 3, 19, 0, 0, 49.824)),
            ((2024, 5, 14, 19, 5), epoch_coords, False, True, True, True,
             (181, 3, 19, 0, 0, 49.824)),
            # 2024-03-19T18:12:09.485+03:30
            # Sunset day before = 18:15 -> 24:00 - 18:15 = 05:45
            # 05:45 + 12:09.485 = 23:57:09.485
            ((2024, 3, 19, 18, 12, 9.485), epoch_coords, False, True, True,
             True, (181, 1, 1, 0, 41, 6.8964)),
            # The next tests may show the wrong month and day if
            # _exact=False is used.
            # The _exact=False condition is generally used in testing.
            ((1844, 3, 19, 18, 16), epoch_coords, False, True, True, False,
             (1, 1, 3, 0, 41, 58.092)),
            ((2024, 5, 14, 20), epoch_coords, False, True, True, False,
             (181, 4, 2, 0, 52, 37.6392)),
            )
        msg = "Expected {} for date {}, short {} and exact {}, found {}"

        for date, coords, us, short, trim, exact, expected_result in data:
            result = self._bc.badi_date_from_gregorian_date(
                date, *coords, us=us, short=short, trim=trim, _exact=exact)
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
            # 0001-01-01T00:00:00+03:30 -> 1844-03-19T18:16:36.0048
            ((1, 1, 1), lat, lon, zone, True, True,
             (1844, 3, 19, 17, 31, 31, 958400)),
            ((126, 16, 1), lat, lon, zone, True, True,
             (1969, 12, 30, 15, 1, 0, 321600)),
            ((181, 3, 18, 20), lat, lon, zone, True, True,
             (2024, 5, 14, 15, 4, 10, 178400)),
            ((181, 3, 19, 20), lat, lon, zone, True, True,
             (2024, 5, 15, 15, 5, 46, 600800)),
            ((181, 4, 1, 17), lat, lon, zone, True, True,
             (2024, 5, 16, 12, 7, 22, 360800)),
            ((181, 4, 1, 20), lat, lon, zone, True, True,
             (2024, 5, 16, 15, 7, 22, 360800)),
            # The next tests show the wrong day if exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1, 1, 1), lat, lon, zone, True, False,
             (1844, 3, 17, 17, 31, 31, 958400)),
            ((181, 3, 18, 20), lat, lon, zone, True, False,
             (2024, 5, 12, 15, 4, 10, 178400)),
            )
        msg = "Expected {} for date {} and exact {}, found {}"

        for date, lat, lon, zone, us, exact, expected_result in data:
            result = self._bc.gregorian_date_from_badi_date(
                date, lat, lon, zone, us=us, _exact=exact)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, exact, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_timestamp(self):
        """
        Test that the badi_date_from_timestamp method returns the correct Badi
        date with a POSIX timestamp as input.

        The lat and lon at GMT is:
        https://www.latlong.net/place/prime-meridian-greenwich-30835.html#:~:text=Prime%20Meridian%20(Greenwich)%20Lat%20Long,%C2%B0%200'%205.5620''%20W.
        https://www.unixtimestamp.com/
        https://timestamp.online/
        https://www.calculator.net/time-calculator.html

        Check the timestamp with the standard Python datetime class.
        datetime.datetime.fromtimestamp(1761690180, datetime.UTC).isoformat()
        Julian Day == Unix timestamp / 86400 + 2440587.5
        (- 2 after 1582 for Astronomically correct JD)
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_coords = (35.5894, -78.7792, -5.0)
        gmt_coords = (51.477928, -0.001545, 0)
        data = (
            # 1970-01-01T00:00:00Z -> UNIX Epoch at UTC
            # Sunset day before 16:00 -> UTC 12am == (126, 16, 2, 8, 0, 0)
            (0, gmt_coords, False, True, True, (126, 16, 2, 8, 0, 30.672)),
            #  UTC 12am == (1, 7, 12, 16, 2, 8, 0, 0)
            (0, gmt_coords, False, False, True,
             (1, 7, 12, 16, 2, 8, 0, 30.672)),
            # 1969-12-31T23:59:59Z One second before the POSIX epoch UTC.
            (-1, gmt_coords, False, True, True, (126, 16, 2, 8, 0, 29.6712)),
            (-1, gmt_coords, True, True, True,
             (126, 16, 2, 8, 0, 29, 671200)),
            # 1969-12-31T19:00:00Z -> 24:00 - 05:00 = 19:00
            # Sunset = 16:00 -> 19:00 - 16:00 = 03:00
            (-18000, gmt_coords, False, True, True,
             (126, 16, 2, 3, 0, 30.672)),
            # 1970-01-01T05:00:00Z -> 1970-01-01T00:00:00-05:00
            # Sunset day before 17:12 -> 24:00 - 17:12 = 06:48
            (18000, local_coords, False, True, True,
             (126, 16, 1, 6, 48, 41.256)),
            # 1969-12-31T19:00:00Z -> 1969-12-31T14:00:00-05:00
            # Sunset day before (1969-12-30) = 17:11 -> 24:00 - 17:11 = 06:49
            # 06:49 + 14:00 = Badi time 20:49
            (-18000, local_coords, False, True, True,
             (126, 16, 1, 20, 48, 41.256)),
            # 2025-10-28T22:23:00+03:30 -> Sunset = 16:40
            # Badi time 05:43:00
            (1761690180, gmt_coords, False, True, True,
             (182, 12, 15, 5, 41, 2.0148)),
            # One second less than the timestamp above.
            (1761690179, gmt_coords, False, True, True,
             (182, 12, 15, 5, 41, 1.014)),
            # 2024-08-21T18:33:46.2Z -> 2024-08-21T13:33:46.2-05:00
            # Sunset day before = 18:58 -> 24:00 - 18:58 = 05:02
            # 05:02 + 13:33:46.2 = Badi date & time 0181-09-03T18:35:46.2
            (1724265226.246101, local_coords, False, True, True,
             (181, 9, 3, 18, 36, 11.898)),
            # 2025-01-12T22:34:01Z -> 2025-01-12T17:34:01-05:00
            # Sunset 17:22 -> 17:34:01 - 17:22 = Badi time 00:12.01
            (1736721241.2909386, local_coords, True, True, False,
             (181, 16, 15, 0, 12, 8, 812800)),
            # 1844-03-19T18:15:45.9792+03:30
            # Sunset = 18:15:459792 -> 18:15:45.9792 - 18:15:45.9792 = 00:00:00
            (-3969422293, epoch_coords, False, True, True,
             (0, 19, 19, 23, 56, 51.2088)),
            # 2024-08-27T21:29:58Z -> 2024-08-28T00:59:58+3:30
            # Sunset 18:48 -> 24:00 - 18:48 = 05:12 ->
            # 05:12 + 00:59:58 = 06:11:58
            (1724794198.5490103, epoch_coords, False, True, True,
             (181, 9, 10, 6, 19, 54.0084)),
            # 1969:12:31T20:30:00Z -> 1970-01-01T00:00:00+03:30
            # Sunset day before = 17:01 -> 24:00 - 17:01 = 06:59
            (-12600, epoch_coords, False, True, True,
             (126, 16, 2, 6, 59, 29.8392)),
            # 2024-08-07T20:04:27.0Z -> 2024-08-07T15:04:27.0-05:00
            # Sunset day before = 19:14 EST -> 24:00 - 19:14 = 04:46 ->
            # 04:46 + 15:04:27 = 19:50:27
            (1723057467.0619307, local_coords, False, True, True,
             (181, 8, 8, 18, 50, 43.5048)),
            )
        msg = "Expected {} for timestamp {} and coords {}, found {}"

        for t, coords, us, short, trim, expected_result in data:
            result = self._bc.badi_date_from_timestamp(
                t, *coords, us=us, short=short, trim=trim)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, t, coords, result))

    #@unittest.skip("Temporarily skipped")
    def test_timestamp_from_badi_date(self):
        """
        Test that the timestamp_from_badi_date method correctly converts
        a Badi date to a timestamp.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_coords = (35.5894, -78.7792, -5.0)
        gmt_coords = (51.477928, -0.001545, 0)
        data = (
            # 1970-01-01T00:00:00Z -> approximately 0.0
            ((126, 16, 2, 8), gmt_coords, 27.507957816124),
            # 1969:12:31T20:30:00Z -> 1970-01-01T00:00:00+03:30
            # Sunset day before = 17:01 -> 24:00 - 17:01 = 06:59
            # approximately -12600
            ((126, 16, 2, 6, 59), epoch_coords, -12586.141186952591),
            # 1970-01-01T00:00:00Z -> 1970-01-01T03:30:00+03:30
            # Sunset day before = 17:01 -> 24:00 - 17:01 = 06:59 ->
            # 06:59 + 03:30 = 10:29 -> approximately 0.0
            ((126, 16, 2, 10, 29), epoch_coords, 13.858826458454),
            # 1970-01-01T00:00:00Z -> 1969-12-31T19:00:00-05:00 ->
            # Sunset day before = 17:12 -> 24:00 - 17:12 = 06:48 ->
            # 06:48 + 19:00 = 01T01:48:00 -> approximately 0.0
            ((126, 16, 2, 1, 48), local_coords, 2.858282625675),
            )
        msg = "Expected {} for date {} and coords {}, found {}."

        for date, coords, expected_result in data:
            result = self._bc.timestamp_from_badi_date(date, *coords)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, coords, result))

    #@unittest.skip("Temporarily skipped")
    def test_midday(self):
        """
        Test that the midday method returns the middle of the Badi day in
        hours, minutes, and seconds or as a decimal value.
        """
        data = (
            ((1, 1, 1), False, True, 0.500584995141),
            ((1, 1, 1), True, True, (12, 0, 50.544)),
            ((181, 11, 4), False, True, 0.499207360437),
            ((181, 11, 4), True, True, (11, 58, 51.5172)),
            ((1, 1, 1, 1, 1), False, False, 0.500584995141),
            ((1, 1, 1, 1, 1), True, False, (12, 0, 50.544)),
            )
        msg = "Expected {} for date {} hms {}, and _short {}, found {}"

        for date, hms, _short, expected_result in data:
            result = self._bc.midday(date, hms=hms, _short=_short)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, hms, _short, result))

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
        MIN_K = self._bc.KULLISHAY_MIN
        MAX_K = self._bc.KULLISHAY_MAX
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
            ((174, 0, 1), True, False, ''), # During Ayyám-i-Há leap year
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799), False, False, ''),
            # Invalid kull-i-shay
            ((MIN_K-2, 2, 19, 19, 19), False, True, err_msg0.format(MIN_K-2)),
            ((MAX_K+2, 1, 1, 1, 1), False, True, err_msg0.format(MAX_K+2)),
            # Invalid Váḥid
            ((1, 0, 1, 1, 1, 1, 1, 1), False, True, err_msg1.format(0)),
            ((1, 20, 1, 1, 1, 1, 1, 1), False, True, err_msg1.format(20)),
            # Invalid year in Váḥid
            ((1, 10, 0, 1, 1, 1, 0, 0), False, True, err_msg2.format(0)),
            ((1, 10, 20, 1, 1, 1, 0, 0), False, True, err_msg2.format(20)),
            # Invalid short date
            ((MIN_Y-2, 1, 1), True, True, err_msg3.format(MIN_Y-2)),
            ((MAX_Y+2, 1, 1), True, True, err_msg3.format(MAX_Y+2)),
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
            ((-1842, 1, 1), epoch_coords, (24, 0, 45.3672)),
            ((-1842, 19, 19), epoch_coords, (24, 0, 45.4212)),
            ((181, 1, 1), epoch_coords, (24, 0, 50.4432)),
            ((181, 19, 19), epoch_coords, (24, 0, 50.6376)),
            # Arbitrary location
            ((-1842, 1, 1), local_coords, (24, 0, 45.0432)),
            ((-1842, 19, 19), local_coords, (24, 0, 45.0972)),
            # Earth seasons for Gregorian year 2024 zone -05:00.
            # Perihelion (2024, 1, 2, 19, 38)
            ((180, 16, 4, 2, 34, 36.336), local_coords, (24, 0, 47.6748)),
            # Vernal Equinox (2024, 3, 19, 22, 6)
            ((181, 1, 1, 3, 49, 12.288), local_coords, (24, 0, 50.13)),
            # Summer Solstice (2024, 6, 20, 15, 51)
            ((181, 5, 18, 20, 26, 54.0096), local_coords, (24, 0, 10.4904)),
            # Aphelion (2024, 7, 5, 0, 6)
            ((181, 6, 13, 4, 41, 49.1712), local_coords, (23, 59, 48.7824)),
            # Fall Equinox (2024, 9, 22, 7, 44)
            ((181, 10, 16, 13, 42, 37.3824), local_coords, (23, 58, 32.1168)),
            # Winter Solstice (2024, 12, 21, 4, 20)
            ((181, 15, 11, 11, 24, 40.6944), local_coords, (24, 0, 29.9052)),
            # Earth seasons for Gregorian year 2024 zone -00:00.
            # Perihelion (2024, 1, 3, 0, 38)
            ((180, 16, 4, 7, 34, 36.3072), utc_coords, (24, 1, 6.0276)),
            # Vernal Equinox (2024, 3, 20, 3, 6)
            ((181, 1, 1, 8, 49, 12.3456), utc_coords, (24, 1, 41.5524)),
            # Summer Solstice (2024, 6, 20, 20, 51)
            ((181, 5, 18, 1, 26, 53.9808), utc_coords, (24, 0, 11.592)),
            # Aphelion (2024, 7, 5, 5, 6)
            ((181, 6, 13, 9, 41, 49.1424), utc_coords, (23, 59, 28.2192)),
            # Fall Equinox (2024, 9, 22, 12, 44)
            ((181, 10, 17, 18, 44, 5.4816), utc_coords, (23, 57, 41.9328)),
            # Winter Solstice (2024, 12, 21, 9, 20)
            ((181, 15, 11, 16, 24, 40.6656), utc_coords, (24, 0, 29.106)),
            )
        msg = "Expected {} for date {}, and location {}, found {}"

        for date, location, expected_result in data:
            jd = self._bc.jd_from_badi_date(date, *location)
            result = self._bc._day_length(jd, *location)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, location, result))

    #@unittest.skip("Temporarily skipped")
    def test__utc_to_badi_time(self):
        """
        Test that the _utc_to_badi_time method converts a UTC time to a
        Badi time.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        gmt_coords = (51.477928, -0.001545, 0.0)
        data = (
            # _POSIX_EPOCH = 2440585.5 -> 1970-01-01T00:00:00Z
            (self._bc._POSIX_EPOCH, epoch_coords, 2440585.581957507),
            (2461118.261957752, epoch_coords, 2461118.292249788),
            (2461118.1, gmt_coords, 2461118.3421236128),
            # Test min and max margins for both the SAME-DAY and
            # PREVIOUS-DAY branchs.
            (2394642.9692, epoch_coords, 2394643.500498699),   # Below min
            (2394642.9693, epoch_coords, 2394643.0000178274),  # Above min
            (2394643.8541, epoch_coords, 2394643.884238762),   # Below max
            (2394643.8542, epoch_coords, 2394644.3849178273),  # Above max
            )
        msg = "Expected {} for JD {} and coordinates {}, found {}."

        for jd, coords, expected_results in data:
            result = self._bc._utc_to_badi_time(jd, *coords)
            self.assertEqual(result, expected_results, msg.format(
                expected_results, jd, coords, result))

    #@unittest.skip("Temporarily skipped")
    def test__badi_to_utc_time(self):
        """
        Test that the _badi_to_utc_time method converts a Badi time to a
        UTC time.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        gmt_coords = (51.477928, -0.001545, 0.0)
        data = (
            # _POSIX_EPOCH = 2440585.5
            (2440585.581957507, epoch_coords, 2440585.5),
            # Result should be = 2461118.261957752
            (2461118.292249788, epoch_coords, 2461118.261957752),
            # 0183-01-01T01:45:1.9188 -> 2026-03-20T14:24:00+00:00
            # Result should be = 2461118.1  (2461118.6011763965 wrong)
            (2461118.3421236128, gmt_coords, 2461118.1),
            # Test min and max margins for both the SAME-DAY and
            # PREVIOUS-DAY branchs.
            # *** TODO *** Fix _badi_to_utc_time
            #(2394643.500498699, epoch_coords, 2394642.9692),
            (2394643.0000178274, epoch_coords, 2394642.9693),
            (2394643.884238762, epoch_coords, 2394643.8541),
            (2394644.3849178273, epoch_coords, 2394643.8542),
            )
        msg = "Expected {} for JD {} and coordinantes {}, found {}."

        for jd, coords, expected_results in data:
            result = self._bc._badi_to_utc_time(jd, *coords)
            self.assertEqual(result, expected_results, msg.format(
                expected_results, jd, coords, result))
