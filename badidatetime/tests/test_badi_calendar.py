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
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_dst_coords = (35.7796, -78.6382, -4)
        data = (
            # Should be 1844-03-19T18:16:00
            ((1, 1, 1), epoch_coords, (18, 15, 45.9792)),
            ((1, 1, 1), (None, None, None), (18, 15, 45.9792)),
            # Should be 2024-03-19T18:13:00
            ((180, 19, 19), epoch_coords, (18, 15, 6.0012)),
            # Should be 2064-03-19T18:16:00
            ((221, 1, 1), epoch_coords, (18, 16, 10.452)),
            # Should be 2024-04-20T19:53:00 DST in Raleigh NC
            ((181, 2, 13), local_dst_coords, (19, 51, 47.2032)),
            # Should be 2024-07-22T20:26:00 DST in Raleigh NC
            ((181, 7, 11), local_dst_coords, (20, 27, 9.054)),
            )
        msg = "Expected {}, date {}, found {}"

        for date, coords, expected_result in data:
            result = self._bc.utc_sunset(date, *coords)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_naw_ruz_g_date(self):
        """
        Test that the naw_ruz_g_date method returns the correct Badi date.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_coords = (35.7796, -78.6382, -5)
        data = (
            # 1844-03-19T18:16:00+03:30
            (1, epoch_coords, False, True, (1844, 3, 19.760948823765)),
            # 1844-03-19T18:16:00+03:30
            (1, epoch_coords, True, True, (1844, 3, 19, 18, 15, 45.9792)),
            # 2024-03-19T18:26:00-05:00
            (181, local_coords, True, True, (2024, 3, 19, 18, 25, 57.5004)),
            # 2025-03-19T18:16:00+03:30
            (182, epoch_coords, False, True, (2025, 3, 19.760931045748)),
            # 2026-03-20T18:16:00+03:30
            (183, epoch_coords, False, True, (2026, 3, 20.761374616064)),
            # The following years are the ones that had errors.
            # 2021-03-19T18:16:00+03:30
            (178, epoch_coords, False, True, (2021, 3, 19.760911469348)),
            # 2030-03-20T18:16:00:+03:30
            (187, epoch_coords, False, True, (2030, 3, 19.760807605926)),
            # 2034-03-19T18:16:00+03:30
            (191, epoch_coords, False, True, (2034, 3, 19.760819741059)),
            # 2038-03-19T18:16:00+03:30
            (195, epoch_coords, False, True, (2038, 3, 19.760831510182)),
            # 2054-03-19T18:16:00+03:30
            (211, epoch_coords, False, True, (2054, 3, 19.760896231048)),
            # 2059-03-20T18:16:21.590400
            (216, epoch_coords, False, True, (2059, 3, 19.760767276399)),
            # 2063-03-19T18:16:23.318400
            (220, epoch_coords, False, True, (2063, 3, 19.760787493549)),
            # Test default latitude, longitude, and zone.
            (1, epoch_coords, False, False, (1844, 3, 19.760948823765)),
            # 1993-03-19T18:16:24.960000 Test sunset before Vernal Equinox
            (150, epoch_coords, False, True, (1993, 3, 19.76080877753)),
            )
        msg = "Expected {} for date {}, found {}"

        for year, coords, hms, use_coords, expected_result in data:
            if use_coords:
                result = self._bc.naw_ruz_g_date(year, *coords, hms=hms)
            else:
                result = self._bc.naw_ruz_g_date(year, hms=hms)

            self.assertEqual(expected_result, result, msg.format(
                expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test_first_day_of_ridvan_g_date(self):
        """
        Test that the first_day_of_ridvan_g_date method returns Jalál 13th
        in any year.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_dst_coords = (35.7796, -78.6382, -4)
        data = (
            # 0001-02-13T00:00:00 -> 1844-04-20T18:42:00
            (1, epoch_coords, False, True, (1844, 4, 19.77863757778)),
            (1, epoch_coords, True, True, (1844, 4, 19, 18, 41, 14.2872)),
            # 0181-02-13T19:41:00-04:00 DST at Raleigh NC, USA
            (181, local_dst_coords, False, True, (2024, 4, 19.827629679348)),
            (181, local_dst_coords, True, True, (2024, 4, 19, 19, 51, 47.2032)),
            # Test default latitude, longitude, and zone.
            (1, epoch_coords, False, False, (1844, 4, 19.77863757778)),
            )
        msg = "Expected {} for hms {}, found {}"

        for year, coords, hms, use_cor, expected_result in data:
            if use_cor:
                result = self._bc.first_day_of_ridvan_g_date(
                    year, *coords, hms=hms)
            else:
                result = self._bc.first_day_of_ridvan_g_date(year, hms=hms)

            self.assertEqual(expected_result, result, msg.format(
                expected_result, year, hms, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_badi_date(self):
        """
        Test that the jd_from_badi_date method returns the correct jd day in
        UT time.

        For a more complete test run: ./contrib/misc/badi_jd_tests.py -aX

        See: https://aa.usno.navy.mil/data/RS_OneYear
        """
        BADI_COORDS = (35.69435, 51.288701, 3.5)
        GMT_COORDS = (51.477928, -0.001545, 0.0)
        data = (
            # Real epoch at sunset 01-01-01T00:00:00 B.E.
            # 1583-03-20T18:16:00+03:30
            ((-260, 1, 1), BADI_COORDS, 2299315.115361992),
            # 1844-03-19T12:00:00+03:30
            ((0, 19, 19, 17, 43.399901), BADI_COORDS, 2394642.853006773),
            # 1844-03-19T18:16:00+03:30
            ((1, 1, 1), BADI_COORDS, 2394643.1151154903),
            # 1863-03-18T18:14:00+03:30
            ((19, 19, 19), BADI_COORDS, 2401582.114755051),
            # POSIX Timestamp epoch
            ((126, 16, 2, 8, 0, 30.6684), GMT_COORDS, 2440585.5006733374),
            # A day in Ayyám-i-Há 2022-02-24T17:57:55.152000
            ((178, 0, 1), BADI_COORDS, 2459633.100756449),
            # 2024-03-18T18:15:00+03:30
            ((180, 19, 19), BADI_COORDS, 2460386.114652771),
            # 2024-04-27T18:48:00+03:30
            ((181, 3, 2), BADI_COORDS, 2460426.137763141),
            # 2024-05-15T20:03:00+03:30
            # Sunset = 20:03
            ((181, 4, 1), BADI_COORDS, 2460444.148117212),
            # Test one date for each coefficient.
            ((-1842, 1, 1), BADI_COORDS, 1721501.113732438),
            ((-1815, 1, 1), BADI_COORDS, 1731363.113980028),
            ((-1799, 1, 1), BADI_COORDS, 1737207.1140555586),
            ((-1783, 1, 1), BADI_COORDS, 1743051.1141319883),
            ((-1747, 1, 1), BADI_COORDS, 1756199.1137700714),
            ((-1699, 1, 1), BADI_COORDS, 1773731.1145127406),
            ((-1519, 1, 1), BADI_COORDS, 1839475.1158592436),
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

           All JDs are in UT1 (within 0.9 seconds of UTC) time. We check
           if the code changes it to the given time zone correctly.
        """
        err_msg0 = "Cannot set more than one of fraction, us, or rtd to True."
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        local_coords = (35.5894, -78.7792, -5.0)
        gmt_coords = (51.477928, -0.001545, 0)
        data = (
            # 1844-03-19T15:45:45.9792Z -> 1844-03-19T19:15:45.9792+03:30
            # Sunset = 19:15:45.9792 -> 19:15:45.9792 - 19:15:45.9792 = 00:00
            (self._bc._BADI_EPOCH, epoch_coords, False, True, True, False,
             False, (1, 1, 1)),
            # 1844-03-20T00:00:00Z -> 1844-03-20T03:30:00.0+03:30
            # Sunset day before = 18:16 -> 24:00 - 18:16 = 05:44 ->
            # 05:44 + 03:30 = Badi time 09:14 -> 0001-01-01T09:14:00
            (2394643.5, epoch_coords, False, True, True, False, False,
             (1, 1, 1, 9, 14, 14.0208)),
            # 1844-03-19T00:00:00Z -> 1844-03-19T03:30:00.0+03:30
            # Sunset day before = 18:15 -> 24:00 - 18:15 = 05:45 ->
            # 05:45 + 03:30 = Badi time 09:15
            (2394642.5, epoch_coords, False, True, True, False, True,
             (0, 19, 19)),
            # 1844-03-18T00:00:00Z -> 1844-03-18T03:30:00.0+03:30
            # Sunset day before = 18:14 -> 24:00 - 18:14 = 05:46 ->
            # 05:46 + 03:30 = Badi time 09:16 -> 0000-19-18T09:16:00
            (2394641.5, epoch_coords, False, True, True, False, True,
             (0, 19, 18)),
            # 1969-12-30T16:00:00Z -> 1969-12-30T11:00:00-05:00
            # Sunset day before--1969-12-29 = 17:11 -> 24:00 - 17:11 = 06:49 ->
            # 06:49 + 11:00 = Badi date & time  0126-15-19T17:49
            (2440584.166667, local_coords, True, True, True, False, False,
             (126, 15, 19, 17, 49, 24, 60000)),
            # 2024-08-21T19:33:46Z -> 2024-08-21T14:33:46-05:00
            # Sunset day before = 18:58 -> 24:00 - 18:58 = 05:02 ->
            # 05:02 + 14:33:46 = 0181-09-03T19:35:46
            (2460542.315116, local_coords, True, True, True, False, False,
             (181, 9, 3, 19, 36, 11, 671200)),
            # 2440585.5 -- 1970-01-01T:00:00:00Z
            # Sunset day before = 16:00 -> 24:00 - 16:00 = 08:00
            # 08:00 + 00:00 = Badi time = 08:00:00
            (self._bc._POSIX_EPOCH, gmt_coords, False, True, True, False,
             False, (126, 16, 2, 7, 59, 32.4924)),
            (self._bc._POSIX_EPOCH, local_coords, False, True, True, False,
             False, (126, 16, 2, 1, 47, 57.1416)),
            # 1849-03-20T00:00:00Z
            # Sunset day before = 18:11 -> 24:00 - 18:11 = 05:49 ->
            # 05:49 + 00:00 = 05:49
            (2396469.5, gmt_coords, False, True, True, False, False,
             (5, 19, 19, 5, 48, 42.3036)),
            # 2025-11-30T22:00:00.0Z -> 2025-12-01T01:30.00.0+03:30
            # Sunset day before = 16:52 -> 24:00 - 16:52 = 07:08 ->
            # 07:08 + 01:30 = Badi date & time = 0182-14-10T08:38:00
            (2461008.416667, epoch_coords, True, True, True, False, False,
             (182, 14, 10, 8, 38, 22, 632000)),
            # 0001-03-20T18:10:00Z -> 0001-03-20T21:40:00+03:30
            # Sunset = 18:17 -> 21:40 - 18:17 = Badi time 03:23:00
            (1721502.256944, epoch_coords, False, True, True, False, False,
             (-1842, 1, 2, 3, 25, 28.11)),
            # 0001-04-08T18:40:46Z -> 0001-04-08T22:10:46+03:30
            # Sunset = 18:27 Tehran -> Badi time = 03:43:46
            (1721521.278310, epoch_coords, False, True, True, False, False,
             (-1842, 2, 2, 3, 42, 10.2636)),
            # 0002-02-24T17:30:50Z -> 0002-02-24T21:00:50+03:30
            # Sunset = 17:57 -> 21:00:50- 17:57 = 03:03:50
            (1721843.229745, epoch_coords, False, True, True, False, False,
             (-1842, 0, 1, 3, 5, 52.9368)),
            # 0002-02-25T17:32:33Z -> 0002-02-25T21:02:33+03:30
            # Sunset = 17:58 -> 21:02:33 - 17:58 = 03:04:33
            (1721844.230938, epoch_coords, False, True, True, False, False,
             (-1842, 0, 2, 3, 6, 43.6824)),
            # 0002-02-26T17:34:14Z -> 0002-02-26T21:04:14+03:30
            # Sunset = 17:59 -> 21:04:14 - 17:59 = 03:05:14
            (1721845.232106, epoch_coords, False, True, True, False, False,
             (-1842, 0, 3, 3, 7, 32.6496)),
            # 0002-03-02T17:40:59Z -> 0002-03-02T21:10:59+03:30
            # Sunset = 18:02 -> Badi time = 03:08:59
            (1721849.236794, epoch_coords, False, True, True, False, False,
             (-1842, 19, 2, 3, 10, 53.6124)),
            # 0002-03-06T17:47:38Z -> 0002-03-06T21:17:38+03:30
            # Sunset = 18:05 -> Badi time = 03:12:38
            (1721853.241412, epoch_coords, False, True, True, False, False,
             (-1842, 19, 6, 3, 14, 14.226)),
            # 1583-03-20T18:13:54Z -> 1583-03-20T21:43:54+03:30
            # Sunset = 18:16 -> 21:43:54 - 18:16 = 03:27:54
            (2299315.259653, epoch_coords, False, True, True, False, False,
             (-260, 1, 1, 3, 27, 46.7424)),
            # 1844-03-19T18:13:21Z -> 1844-03-19T21:43:21+03:30
            # Sunset = 18:16 -> Badi time = 03:27:21
            (2394643.259271, epoch_coords, False, True, True, False, False,
             (1, 1, 1, 3, 27, 35.0352)),
            # 1844-03-19T18:11:40Z -> 1844-03-19T21:41:40+03:30
            # Sunset = 18:16 -> 21:41:40 - 18:16 = 03:25:40
            (2394643.258102, epoch_coords, False, True, True, False, False,
             (1, 1, 1, 3, 25, 54.0336)),
            # 1845-03-20T18:14:37Z -> 1845-03-20T21:44:37+03:30
            # Sunset = 18:17 -> 21:44:37 - 18:17 = 03:27:37
            (2395009.260150, epoch_coords, False, True, True,  False, False,
             (2, 1, 1, 3, 28, 13.1592)),
            # 1863-03-20T18:14:00Z -> 1863-03-20T21:44:00+03:30
            # Sunset = 18:16 -> 21:44 - 18:16 = 03:28
            (2401583.259722, epoch_coords, False, True, True, False, False,
             (20, 1, 1, 3, 27, 54.972)),
            # 1844-03-19T18:17:26Z -> 1844-03-19T21:47:26+03:30
            # Sunset = 18:16 -> 21:47:26 - 18:16 = Badi time = 03:31:26
            (2394643.262106, epoch_coords, False, True, True, False, True,
             (1, 1, 1)),
            # Previous day test
            # 2025-12-01T00:00:00Z -> 2025-11-30T19:00:00-05:00
            # Sunset day before = 17:02 -> 24:00 - 17:02 = 06:58
            (2461008.5, local_coords, True, True, True, False, False,
             (182, 14, 10, 1, 57, 54, 507600)),
            # 2015-03-20T18:14:10.1Z -> 2015-03-20T21:44:10.1+03:30
            # Sunset = 18:16 -> Badi time = 03:28:10.1
            (2457100.2598390104, epoch_coords, False, True, True, False,
             False, (172, 1, 1, 3, 28, 3.756)),
            # 2024-03-19T18:13:51.0Z -> 2024-03-19T21:43:51.0+03:30
            # Sunset = 18:16 -> Badi time = 03:27:51
            (2460387.2596184844, epoch_coords, False, True, True, False,
             False, (181, 1, 1, 3, 27, 54.4464)),
            # 2024-04-20T19:07:26.7Z -> 2024-04-20T22:37:26.7+03:30
            #Sunset = 18:42 -> Badi time = 03:55:26.7
            (2460419.296837184, epoch_coords, False, True, True,  False,
             False, (181, 2, 14, 3, 54, 56.6388)),
            # 2022-02-24T17:33:05.4Z -> 2022-02-24T21:03:05.4+03:30
            # Sunset = 17:55 -> Badi time = 03:08:05.4
            (2459633.231312508, epoch_coords, False, True, True, False, False,
             (178, 0, 1, 3, 8, 0.042)),
            # 2022-03-01T17:41:57.3Z -> 2022-03-01T21:11:57.3+03:30
            # Sunset = 18:00 -> Badi time = 03:11:57.3
            (2459638.2374685165, epoch_coords, False, True, True, False,
             False, (178, 19, 1, 3, 12, 12.8016)),
            # 2022-03-02T17:43:42.8Z -> 2022-03-02T21:13:42.8+03:30
            # Sunset = 18:01 -> Badi time = 03:12:42.8
            (2459639.238690004, epoch_coords, False, True, True, False, False,
             (178, 19, 2, 3, 13, 3.4788)),
            # 2024-05-12T19:43:00.9Z -> 2024-05-12T23:13:00.9+03:30
            # Sunset = 19:01 -> Badi time = 04:12:00.9
            (2460441.3215382686, epoch_coords, False, True, True, False,
             False, (181, 3, 17, 4, 12, 8.658)),
            # 2024-05-14T19:46:01.7Z -> 2024-05-14T23:16:01.7+03:30
            # Sunset = 19:02 -> Badi time = 04:14:01.7
            (2460443.32363071, epoch_coords, False, False, True, False, False,
             (1, 10, 10, 3, 19, 4, 13, 32.4192)),
            # 2024-05-14T19:46:01.7Z -> 2024-05-14T23:16:01.7+03:30
            # Sunset = 19:02 -> 23:16:01.7 - 19:02 = 04:14:01.7
            (2460443.32363071, epoch_coords, True, False, True, False, False,
             (1, 10, 10, 3, 19, 4, 13, 32, 419200)),
            # 2024-07-17T20:07:03.4Z -> 2024-07-17T23:37:03.4+03:30
            # Sunset = 19:20 -> Badi time = 04:17:1.7
            (2460507.3382339347, epoch_coords, False, True, True, False,
             False, (181, 7, 7, 4, 17, 13.7004)),
            # 2024-07-17T20:07:03.4Z -> 2024-07-17T23:37:03.4+03:30
            # Sunset = 19:20 -> Badi date & time = 0181-07-07.178492
            (2460507.3382339347, epoch_coords, False, True, True, True, False,
             (181, 7, 7.178631)),
            # 2024-03-19T18:13:51.0Z -> 2024-03-19T21:43:51.0+03:30
            # Sunset 18:16 -> Badi time = 03:27:51
            (2460387.2596184844, epoch_coords, False, True, True, False,
             False, (181, 1, 1, 3, 27, 54.4464)),
            # rtd and long format
            (2394985.2486724695, epoch_coords, False, False, True, False,
             True, (1, 1, 1, 0, 1)),
            # Use default coordinents
            (2394985.2486724695, None, False, False, True, False,
             True, (1, 1, 1, 0, 1)),
            # Minute after, of, and before, each should be one minute apart.
            # 1 minute = 0.00069444440305233 of a JD
            (2394643.2587968805, gmt_coords, False, True, False, False, False,
             (1, 1, 1, 0, 1, 0.0012)),
            (2394643.258102436, gmt_coords, False, True, False, False, False,
             (1, 1, 1)),
            # 1844-03-19T18:10:40.1Z -> (1844-03-19T21:40:40.1+03:30)
            # Sunset = 18:10 -> 18:10:40.1 - 18:10 = 00:00:40.1
            (2394643.257409, gmt_coords, False, True, False, False, False,
             (0, 19, 19, 0, 0, 41.3352)),
            # us and fraction
            (0, gmt_coords, True, True, False, True, False, err_msg0),
            # us and rtd
            (0, gmt_coords, True, True, False, False, True, err_msg0),
            # fraction and rtd
            (0, gmt_coords, False, True, False, True, True, err_msg0),
            )
        msg = "Expected {} for jd {} for coords {}, found {}"

        for jd, coords, us, short, trim, frac, rtd, expected_result in data:
            if isinstance(expected_result, str):
                with self.assertRaises(AssertionError) as cm:
                    self._bc.badi_date_from_jd(jd, *coords, us=us, short=short,
                                               trim=trim, fraction=frac,
                                               rtd=rtd)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                if coords is None:
                    coords = (None, None, None)

                result = self._bc.badi_date_from_jd(
                    jd, *coords, us=us, short=short, trim=trim, fraction=frac,
                    rtd=rtd)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, jd, coords, result))

    #@unittest.skip("Temporarily skipped")
    def test__badi_year_from_rd(self):
        """
        Tear that the _badi_year_from_rd method returns the correct year
        given a Rata Die (rd).
        """
        err_msg0 = ("Invalid Rata Die value {} it must be between "
                    f"[{self._bc._RD_START}, {self._bc._RD_END}].")
        data = (
            (self._bc._RD_START, False, -1843),
            (self._bc._RD_END, False, 1161),
            (-1000, True, err_msg0.format(-1000)),
            (self._bc._RD_START-1, True, err_msg0.format(self._bc._RD_START-1)),
            (self._bc._RD_END+1, True, err_msg0.format(self._bc._RD_END+1)),
            )
        msg = "Expected {} for rd {}, found {}"

        for rd, validity, expected_result in data:
            if validity:
                try:
                    with self.assertRaises(AssertionError) as cm:
                        self._bc._badi_year_from_rd(rd)
                except AssertionError as e:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(
                        f"With rd {rd} and error was not raised, {e}")
                else:
                    message = str(cm.exception)
                    self.assertEqual(expected_result, message)
            else:
                result = self._bc._badi_year_from_rd(rd)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, rd, result))

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
            # 1844-03-19T18:16:36.0048+03:33 -> 1844-03-19T14:46:36.0048Z
            # Since jd_from_gregorian_date() takes UT time we need to convert
            # The time from the time zone to GMT + UT date and time.
            ((1844, 3, 19, 14, 46), epoch_coords, False, False, True, True,
             (1, 1, 1, 1, 1, 0, 0, 14.022)),
            # 1844-03-19T18:16:00+03:30 -> 1844-01-01T14:46:00Z
            # Sunset = 18:16 -> 18:11:40.05 - 18:16 = 03:25:40.05
            ((1844, 3, 19, 14, 46), epoch_coords, False, True, True, True,
             (1, 1, 1, 0, 0, 14.022)),
            ((1844, 3, 19, 21, 41, 40.05), epoch_coords, False, True, True,
             True, (1, 1, 1, 6, 55, 54.0732)),
            # 2024-05-14T19:05:00+03:30 -> 2024-05-14T15:35:00Z
            # Sunset = 19:02 -> 19:05 - 19:02 = 00:03
            ((2024, 5, 14, 15, 35), epoch_coords, False, False, True, True,
             (1, 10, 10, 3, 19, 0, 2, 30.7248)),
            ((2024, 5, 14, 19, 5), epoch_coords, False, True, True, True,
             (181, 3, 19, 3, 32, 30.7248)),
            # 2024-03-19T18:12:00+03:30 -> 2025-03-19T14:42:00Z
            # Sunset day before = 18:15 -> 24:00 - 18:15 = 05:45
            # 05:45 + 14:42 = 20:27
            ((2024, 3, 19, 14, 42), epoch_coords, False, True, True, True,
             (180, 19, 19, 23, 56, 53.9988)),
            # The next tests may show the wrong month and day if
            # _exact=False is used.
            # The _exact=False condition is generally used in testing.
            ((1844, 3, 19, 18, 16), epoch_coords, False, True, True, False,
             (1, 1, 3, 3, 28, 34.104)),
            ((2024, 5, 14, 20), epoch_coords, False, True, True, False,
             (181, 4, 2, 4, 25, 54.966)),
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
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 0001-01-01T00:00:00+03:30 -> 1844-03-19T18:16:36.0048
            ((1, 1, 1), epoch_coords, True, True,
             (1844, 3, 19, 18, 15, 45, 979200)),
            ((1, 1, 1), (None, None, None), True, True,
             (1844, 3, 19, 18, 15, 45, 979200)),
            # 1970-01-01T00:00:00Z -> 1970-01-01T03:30:00+03:30
            # Sunset day before = 16:00 -> 24:00 - 16:00 = 08:00 ->
            # 08:00 + 03:30 = 11:30
            ((126, 16, 2, 11, 30), epoch_coords, True, True,
             (1970, 1, 1, 4, 31, 13, 857600)),
            # 2024-05-14T
            ((181, 3, 18, 20), epoch_coords, True, True,
             (2024, 5, 14, 15, 1, 40, 904400)),
            ((181, 3, 19, 20), epoch_coords, True, True,
             (2024, 5, 15, 15, 2, 29, 274000)),
            ((181, 4, 1, 17), epoch_coords, True, True,
             (2024, 5, 16, 12, 3, 17, 326800)),
            ((181, 4, 1, 20), epoch_coords, True, True,
             (2024, 5, 16, 15, 3, 17, 326800)),
            # The next tests show the wrong day if exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1, 1, 1), epoch_coords, True, False,
             (1844, 3, 17, 18, 15, 45, 979200)),
            ((181, 3, 18, 20), epoch_coords, True, False,
             (2024, 5, 12, 15, 1, 40, 904400)),
            )
        msg = "Expected {} for date {} and exact {}, found {}"

        for date, coords, us, exact, expected_result in data:
            result = self._bc.gregorian_date_from_badi_date(
                date, *coords, us=us, _exact=exact)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, exact, result))

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
        gmt_coords = (51.477928, -0.001545, 0.0)
        data = (
            # 1970-01-01T00:00:00Z -> UNIX Epoch at UTC
            # Sunset day before 16:00 -> UTC 12am == (126, 16, 2, 8, 0, 0)
            (0, gmt_coords, False, True, True, (126, 16, 2, 7, 59, 32.4924)),
            #  UTC 12am == (1, 7, 12, 16, 2, 8, 0, 0)
            (0, gmt_coords, False, False, True,
             (1, 7, 12, 16, 2, 7, 59, 32.4924)),
            # 1969-12-31T23:59:59Z One second before the POSIX epoch UTC.
            (-1, gmt_coords, False, True, True, (126, 16, 2, 7, 59, 31.4916)),
            (-1, gmt_coords, True, True, True,
             (126, 16, 2, 7, 59, 31, 491600)),
            # 1970-01-01T00:00:00Z -> 1969-12-31T19:00:00-05:00
            # Sunset 17:12 -> 19:00 - 17:12 = 01:48
            (0, local_coords, False, True, True, (126, 16, 2, 1, 47, 57.1416)),
            # 1969-12-31T19:00:00Z -> 1969-12-31T14:00:00-05:00
            # Sunset day before = 16:00 -> 24:00 - 16:00 = 08:00 ->
            # 08:00 + 14:00 = 22:00
            (-18000, gmt_coords, False, True, True,
             (126, 16, 2, 2, 59, 32.4924)),
            # 1970-01-01T05:00:00Z -> 1970-01-01T00:00:00-05:00
            # Sunset day before 17:12 -> 24:00 - 17:12 = 06:48
            # 06:48 + 00:00 = 06:48
            (18000, local_coords, False, True, True,
             (126, 16, 2, 6, 47, 57.1416)),
            # 1969-12-31T19:00:00Z -> 1969-12-31T14:00:00-05:00
            # Sunset day before (1969-12-30) = 17:11 -> 24:00 - 17:11 = 06:49
            # 06:49 + 14:00 = Badi time 20:49
            (-18000, local_coords, False, True, True,
             (126, 16, 1, 20, 48, 41.256)),
            # 1969-12-31T16:00:00Z
            # Sunset 16:00 -> 24:00 - 16:00 = 08:00 -> -28800
            (-28800, gmt_coords, False, True, True,
             (126, 16, 1, 0, 0, 30.672)),
            # 1970-01-01T08:00:00Z == jd 2440585.8333333335
            # Sunset day before 16:00 -> 24:00 - 16:00 = 08:00 ->
            # 08:00 + 08:00 = 16:00 -> 28800
            (28800, gmt_coords, False, True, True,
             (126, 16, 2, 15, 59, 32.4924)),
            # 2025-10-28T22:23:00+03:30 -> Sunset = 16:40
            # Badi time 05:43:00
            (1761690180, gmt_coords, False, True, True,
             (182, 12, 15, 5, 42, 56.4228)),
            # One second less than the timestamp above.
            (1761690179, gmt_coords, False, True, True,
             (182, 12, 15, 5, 42, 55.422)),
            # 2024-08-21T18:33:46.2Z -> 2024-08-21T13:33:46.2-05:00
            # Sunset day before = 18:58 -> 24:00 - 18:58 = 05:02
            # 05:02 + 13:33:46.2 = Badi date & time 0181-09-03T18:35:46.2
            (1724265226.246101, local_coords, False, True, True,
             (181, 9, 3, 18, 36, 11.8944)),
            # 2025-01-12T22:34:01Z -> 2025-01-12T17:34:01-05:00
            # Sunset 17:22 -> 17:34:01 - 17:22 = Badi time 00:12.01
            (1736721241.2909386, local_coords, True, True, False,
             (181, 16, 15, 0, 11, 11, 979600)),
            # 1844-03-19T14:45:45.9792 -> 1844-03-19T18:15:45.9792+03:30
            # Sunset = 18:15:459792 -> 18:15:45.9792 - 18:15:45.9792 = 00:00:00
            (-3969404293, epoch_coords, False, True, True,
             (1, 1, 1, 4, 56, 1.0212)),
            # 2024-08-27T21:29:58Z -> 2024-08-28T00:59:58+3:30
            # Sunset day before 18:39 -> 24:00 - 18:39 = 05:21 ->
            # 05:21 + 00:59:58 = 06:20:58
            (1724794198.5490103, epoch_coords, False, True, True,
             (181, 9, 10, 6, 21, 14.7924)),
            # 1969:12:31T20:30:00Z -> 1970-01-01T00:00:00+03:30
            # Sunset day before = 17:01 -> 24:00 - 17:01 = 06:59
            (-12600, epoch_coords, False, True, True,
             (126, 16, 2, 6, 58, 46.1424)),
            # 2024-08-07T20:04:27.0Z -> 2024-08-07T14:04:27.0-05:00
            # Sunset day before = 19:14 -> 24:00 - 19:14 = 04:46 ->
            # 04:46 + 14:04:27 = 18:50:27 (Should be 0181-08-08...)
            (1723057467.0619307, local_coords, False, True, True,
             (181, 8, 8, 18, 50, 43.5012)),
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
            # 1970-01-01T00:00:00Z -> UNIX Epoch at UTC
            # Sunset day before 16:00 -> UTC 12am == (126, 16, 2, 8, 0, 0)
            ((126, 16, 2, 7, 59, 32.4924), gmt_coords, 0.000362098217),
            # 1969-12-31T23:59:59Z One second before the POSIX epoch UTC.
            ((126, 16, 2, 7, 59, 31.4916), gmt_coords, -1.000437140465),
            ((126, 16, 2, 7, 59, 31, 491600), gmt_coords, -1.000437140465),
            # 1970-01-01T00:00:00Z -> 1969-12-31T19:00:00-05:00
            # Sunset 17:12 -> 19:00 - 17:12 = 01:48 -> 18000
            ((126, 16, 2, 1, 47, 57.1416), local_coords, -0.000804662704),
            # 1970-01-01T00:00:00Z -> approximately 0.0
            ((126, 16, 2, 8), gmt_coords, 27.507957816124),
            # 1969:12:31T20:30:00Z -> 1970-01-01T00:00:00+03:30
            # Sunset day before = 17:01 -> 24:00 - 17:01 = 06:59 ->
            # 06:59 + 00:00 = 06:59 -> -25200
            ((126, 16, 2, 6, 58, 46.1424), epoch_coords, -12599.999088048935),
            # 1970-01-01T00:00:00Z -> 1970-01-01T03:30:00+03:30
            # Sunset day before = 17:01 -> 24:00 - 17:01 = 06:59 ->
            # 06:59 + 03:30 = 10:29 -> approximately -12600
            ((126, 16, 2, 10, 29), epoch_coords, 13.858504593372),
            # 1970-01-01T00:00:00Z -> 1969-12-31T19:00:00-05:00
            # Sunset day before = 17:12 -> 19:00 - 17:12 = 01:48 ->
            # approximately 18000
            ((126, 16, 2, 1, 48), local_coords, 2.857598662376),
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
            ((1, 10, 2, 19, 19), True, False),  # (2017, 3, 19)
            ((1, 10, 3, 19, 19), True, True),   # (2017, 3, 19)
            ((1, 10, 4, 19, 19), True, False),  # (2018, 3, 20)
            # First and last years
            (-1842, True, True),                # 0001
            (1061, True, True),                 # 3004
            )
        msg = "Expected {} for day {}, found {}"

        for date, valid, expected_result in data:
            if isinstance(date, int):
                year = date
            else:
                year = ((date[0] - 1) * 361 + (date[1] - 1) * 19 + date[2])

            if valid:
                result = self._bc._is_leap_year(year)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, date, result))
            else:
                with self.assertRaises(AssertionError) as cm:
                    self._bc._is_leap_year(year)

                message = str(cm.exception)
                self.assertEqual(expected_result.format(date), message)

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
            ((-1842, 1, 1), epoch_coords, False, (24, 0, 45.3672)),
            ((-1842, 19, 19), epoch_coords, False, (24, 0, 45.4212)),
            ((181, 1, 1), epoch_coords, False, (24, 0, 50.4432)),
            ((181, 19, 19), epoch_coords, False, (24, 0, 50.6376)),
            # Arbitrary location
            ((-1842, 1, 1), local_coords, False, (24, 0, 45.0432)),
            ((-1842, 19, 19), local_coords, False, (24, 0, 45.0936)),
            # Earth seasons for Gregorian year 2024 zone -05:00.
            # Perihelion (2024, 1, 2, 19, 38)
            ((180, 16, 4, 2, 34, 36.336), local_coords, False,
             (24, 0, 47.6748)),
            # Vernal Equinox (2024, 3, 19, 22, 6)
            ((181, 1, 1, 3, 49, 12.288), local_coords, False, (24, 0, 50.13)),
            # Summer Solstice (2024, 6, 20, 15, 51)
            ((181, 5, 18, 20, 26, 54.0096), local_coords, False,
             (24, 0, 10.4868)),
            # Aphelion (2024, 7, 5, 0, 6)
            ((181, 6, 13, 4, 41, 49.1712), local_coords, False,
             (23, 59, 48.786)),
            # Fall Equinox (2024, 9, 22, 7, 44)
            ((181, 10, 16, 13, 42, 37.3824), local_coords, False,
             (23, 58, 32.1132)),
            # Winter Solstice (2024, 12, 21, 4, 20)
            ((181, 15, 11, 11, 24, 40.6944), local_coords, False,
             (24, 0, 29.9052)),
            # Earth seasons for Gregorian year 2024 zone -00:00.
            # Perihelion (2024, 1, 3, 0, 38)
            ((180, 16, 4, 7, 34, 36.3072), utc_coords, False, (24, 1, 6.0276)),
            # Vernal Equinox (2024, 3, 20, 3, 6)
            ((181, 1, 1, 8, 49, 12.3456), utc_coords, False, (24, 1, 41.5524)),
            # Summer Solstice (2024, 6, 20, 20, 51)
            ((181, 5, 18, 1, 26, 53.9808), utc_coords, False, (24, 0, 11.592)),
            # Aphelion (2024, 7, 5, 5, 6)
            ((181, 6, 13, 9, 41, 49.1424), utc_coords, False,
             (23, 59, 28.2192)),
            # Fall Equinox (2024, 9, 22, 12, 44)
            ((181, 10, 17, 18, 44, 5.4816), utc_coords, False,
             (23, 57, 41.8068)),
            # Winter Solstice (2024, 12, 21, 9, 20)
            ((181, 15, 11, 16, 24, 40.6656), utc_coords, False,
             (24, 0, 29.106)),
            # Test decimal
            ((181, 15, 11, 16, 24, 40.6656), utc_coords, True, 1.000336857978),
            )
        msg = "Expected {} for date {}, and location {}, found {}"

        for date, location, decimal, expected_result in data:
            jd = self._bc.jd_from_badi_date(date, *location)
            result = self._bc._day_length(jd, *location, decimal=decimal)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, location, result))

    #@unittest.skip("Temporarily skipped")
    def test__utc_to_badi_time(self):
        """
        Test that the _utc_to_badi_time method converts a UTC time to a
        Badi time. All seed values (the JD being tested) must be in UT time
        without zone corrections. The resultant date and time are now
        authoritative.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:2]
        gmt_coords = (51.477928, -0.001545)
        local_coords = (35.5894, -78.7792)
        data = (
            # _POSIX_EPOCH = 2440585.5 -> 1970-01-01T00:00:00Z ->
            # 1970-01-01T03:30:00+03:30 -> Sunset day before = 17:01 ->
            # 24:00 - 17:01 = 06:59 -> 06:59 + 03:30 = 10:29 ->
            # 2440585.4368055555
            (self._bc._POSIX_EPOCH, epoch_coords, (2440585.0633548438,
                                                   0.4366451562382281)),
             # _POSIX_EPOCH = 2440585.5 -> 1970-01-01T00:00:00Z ->
            # 1939-12-31T19:00:00-05:00 -> Sunset day before = 17:12 ->
            # 24:00 - 17:12 = 06:48 -> 06:48 + 19:00 = 01T01:48 ->
            # 2440585.074966918
            (self._bc._POSIX_EPOCH, local_coords, (2440585.425033074,
                                                   0.0749669261276722)),
            # 2026-03-20T18:17:13.1Z -> 2026-03-20T21:38:13.1+03:30
            # Sunset = 18:16 -> 21:38:13.1 - 18:16 = 03:22:13.1 ->
            # 2461118.1404293982
            (2461120.261263, epoch_coords, (2461120.1167060486,
                                            0.14455695124343038)),
            # 2026-03-20T14:24:00Z -> Sunset day before  = 18:11
            # 24:00 - 18:11 = 05:49 -> 05:49 + 14:24 = 20:13 ->
            # 2461117.8423611111
            (2461118.1, gmt_coords, (2461117.2578763873, 0.8421236127614975)),
            # Test min and max margins for both the SAME-DAY and
            # PREVIOUS-DAY branchs.
            # 1844-03-19T14:50:00Z -> 1844-03-19T18:20:00+03:30 ->
            # Sunset = 18:16 -> 18:20 - 18:16 = 00:04 -> 2394643.0027777778
            (2394643.118056, epoch_coords, (2394643.1151154903,
                                            0.002940509933978319)), # Above min
            # 1844-03-19T14:42:00Z -> 1844-03-19T18:12:00+03:30 ->
            # Sunset day before = 18:15 -> 24:00 - 18:15 = 05:45 ->
            # 05:45 + 18:12 = 23:57 -> 2394642.9979166666
            (2394643.112500, epoch_coords, (2394642.1145346197,
                                            0.9979653800837696)),  # Below min
            # 1844-03-19T20:26:00Z -> 1844-03-19T23:56:00+03:30 ->
            # Sunset = 18:16 -> 23:56 - 18:16 = 05:40 -> 2394643.2361111111
            (2394643.351389, epoch_coords, (2394643.1151154903,
                                            0.2362735099159181)),  # Below max
            # 1844-03-19T20:34:00Z -> 1844-03-20T00:04:00+03:30 ->
            # Sunset day before = 18:16 -> 24:00 - 18:16 = 05:44 ->
            # 05:44 + 00:04 = 05:48 -> 2394642.24166666666
            (2394643.356944, epoch_coords, (2394643.1151154903,
                                            0.241828509606421)),  # Above max
            # 1844-03-19T18:11:40.1Z ->
            # Sunset day before = 18:12 -> 24:00 - 18:12 = 05:48 ->
            # 05:48 + 18:11:40.1 = 23:59:40.1 -> 2394642.9997696759
            (2394643.258103, gmt_coords, (2394643.258102436,
                                          5.639158189296722e-07)),
            # 1844-03-19T18:14:00Z ->
            # Sunset = 18:12 -> 18:14 - 18:12 = 00:02
            (2394643.259722, gmt_coords, (2394643.258102436,
                                          0.0016195639036595821)),
            # 1844-03-19T23:28:00Z -> 1844-03-19T18:25:00-05:00
            # Sunset = 18:26 -> 18:26 - 18:25 = 00:01
            (2394643.477778, local_coords, (2394643.4766224716,
                                            0.0011555282399058342)),
            )
        msg = "Expected {} for JD {} and coordinates {}, found {}."

        for jd, coords, expected_results in data:
            result = self._bc._utc_to_badi_time(jd, *coords)
            self.assertEqual(expected_results, result, msg.format(
                expected_results, jd, coords, result))
