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
    #MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
    #          12, 13, 14, 15, 16, 17, 18, 0, 19)

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
            # Should be 1844-03-20T18:14:00
            ((1, 1, 1, 2), None, None, None, True, (18, 17, 26.5632)),
            # Should be 2024-03-19T18:13:00
            ((180, 19, 19), lat, lon, zone, True, (18, 16, 47.6832)),
            # Should be 2064-03-19T18:13:00
            ((221, 1, 1), lat, lon, zone, True, (18, 17, 1.5936)),
            # Should be 2024-04-20T19:53:00 DST in Raleigh NC
            ((181, 2, 13), 35.7796, -78.6382, -4, True, (19, 53, 28.1472)),
            # Should be 2024-07-22T20:26:00 DST in Raleigh NC
            ((181, 7, 11), 35.7796, -78.6382, -4, True, (20, 25, 47.3952)),
            )
        msg = "Expected {}, date {}, found {}"

        for date, lat, lon, zone, use_cor, expected_result in data:
            if use_cor:
                result = self._bc.utc_sunset(date, lat, lon, zone)
            else:
                result = self._bc.utc_sunset(date)

            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_naw_ruz_g_date(self):
        """
        Test that the naw_ruz_g_date method returns the correct Badi date.
        """
        lat, lon, zone = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 1844-03-19T18:16:36.710400
            (1, lat, lon, zone, False, True, (1844, 3, 19.762113)),
            # 1844-03-19T18:16:36.710400
            (1, lat, lon, zone, True, True, (1844, 3, 19, 18, 17, 26.5632)),
            # 2024-03-19T18:15:57.312000
            (181, 35.7796, -78.6382, -5, True, True,
             (2024, 3, 19, 18, 27, 38.6208)),
            # 2025-03-19T18:16:34.587200
            (182, lat, lon, zone, False, True, (2025, 3, 19.762105)),
            # 2026-03-20T18:17:13.776000
            (183, lat, lon, zone, False, True, (2026, 3, 20.761965)),
            # The following years are the ones that had errors.
            # 2021-03-19T18:16:33.859200
            (178, lat, lon, zone, False, True, (2021, 3, 19.762085)),
            # 2030-03-19T18:16:24.960000
            (187, lat, lon, zone, False, True, (2030, 3, 19.761983)),
            # 2034-03-19T18:16:25.996800
            (191, lat, lon, zone, False, True, (2034, 3, 19.761995)),
            # 2038-03-19T18:16:27.033600
            (195, lat, lon, zone, False, True, (2038, 3, 19.762007)),
            # 2054-03-19T18:16:32.6496
            (211, lat, lon, zone, False, True, (2054, 3, 19.761489)),
            # 2059-03-19T18:16:21.590400
            (216, lat, lon, zone, False, True, (2059, 3, 19.761944)),
            # 2063-03-19T18:16:23.318400
            (220, lat, lon, zone, False, True, (2063, 3, 19.761965)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 3, 19.762113)),
            # 1993-03-19T18:16:24.960000 Test sunset before Vernal Equinox
            (150, lat, lon, zone, False, True, (1993, 3, 19.761982)),
            )
        msg = "Expected {} for date {}, found {}"

        for year, lat, lon, zone, hms, use_cor, expected_result in data:
            if use_cor:
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
            (1, lat, lon, zone, False, True, (1844, 4, 19.780362)),
            (1, lat, lon, zone, True, True, (1844, 4, 19, 18, 43, 43.2768)),
            # 0181-02-13T19:41:00-04:00 DST at Raleigh NC, USA
            (181, 35.7796, -78.6382, -4, False, True, (2024, 4, 19.828798)),
            (181, 35.7796, -78.6382, -4, True, True,
             (2024, 4, 19, 19, 53, 28.1472)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 4, 19.780362)),
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
            ((1, 1, 1), 2394643.262113),            # 1844-03-19T18:17:26.563199
            ((1, 1, 1, 18, 14), 2394644.02241),     # 1844-03-20T12:32:16.224000
            ((19, 19, 19), 2401582.261756),         # 1863-03-19T18:16:55.718400
            ((180, 19, 19), 2460386.261663),        # 2024-03-18T18:16:47.683199
            ((181, 3, 2), 2460426.285342),          # 2024-04-27T18:50:53.548800
            ((-260, 1, 1, 18, 16), 2299316.023453), # 1583-03-21T12:33:46.339199
            # A day in Ayyám-i-Há 2022-02-24T17:57:55.152000
            ((178, 0, 1), 2459633.248555),
            ((181, 3, 19, 20), 2460444.128373),     # 2024-05-15T15:04:51.427199
            # Test one date for each coefficient.
            ((-1842, 1, 1), 1721501.261143),
            ((-1800, 1, 1), 1736842.261073),
            ((-1750, 1, 1), 1755104.261046),
            ((-1720, 1, 1), 1766061.261438),
            ((-1700, 1, 1), 1773366.261532),
            ((-1690, 1, 1), 1777018.26132),
            ((-1620, 1, 1), 1802585.261905),
            ((-1590, 1, 1), 1813543.261772),
            ((-1580, 1, 1), 1817195.261561),
            ((-1520, 1, 1), 1839110.262357),
            ((-1480, 1, 1), 1853719.262008),
            ((-1450, 1, 1), 1864676.262416),
            ((-1420, 1, 1), 1875634.262282),
            ((-1400, 1, 1), 1882939.262375),
            ((-1350, 1, 1), 1901201.262329),
            ((-1320, 1, 1), 1912158.26327),
            ((-1300, 1, 1), 1919463.263359),
            ((-1290, 1, 1), 1923115.262601),
            ((-1220, 1, 1), 1948682.263714),
            ((-1190, 1, 1), 1959640.264123),
            ((-1160, 1, 1), 1970597.263985),
            ((-1120, 1, 1), 1985207.264704),
            ((-1080, 1, 1), 1999816.26435),
            ((-1060, 1, 1), 2007121.264439),
            ((-1020, 1, 1), 2021731.264615),
            ((-1000, 1, 1), 2029036.264703),
            ((-950, 1, 1), 2047298.26466),
            ((-920, 1, 1), 2058255.265056),
            ((-900, 1, 1), 2065560.265146),
            ((-890, 1, 1), 2069212.264924),
            ((-820, 1, 1), 2094779.265507),
            ((-790, 1, 1), 2105737.265904),
            ((-770, 1, 1), 2113041.265994),
            ((-720, 1, 1), 2131304.26649),
            ((-700, 1, 1), 2138608.266577),
            ((-660, 1, 1), 2153218.266206),
            ((-620, 1, 1), 2167828.266385),
            ((-600, 1, 1), 2175133.266475),
            ((-550, 1, 1), 2193395.266415),
            ((-520, 1, 1), 2204352.266829),
            ((-500, 1, 1), 2211657.266915),
            ((-490, 1, 1), 2215309.266679),
            ((-420, 1, 1), 2240876.267259),
            ((-390, 1, 1), 2251834.267675),
            ((-360, 1, 1), 2262791.267523),
            ((-320, 1, 1), 2277401.268258),
            ((-280, 1, 1), 2292010.267877),
            ((-250, 1, 1), 2302968.262091),
            ((-220, 1, 1), 2313925.261935),
            ((-200, 1, 1), 2321230.262014),
            ((-150, 1, 1), 2339492.261935),
            ((-120, 1, 1), 2350449.261762),
            ((-100, 1, 1), 2357754.261844),
            ((-90, 1, 1), 2361406.261602),
            ((-20, 1, 1), 2386973.261596),
            ((10, 1, 1), 2397931.261997),
            ((40, 1, 1), 2408888.261834),
            ((80, 1, 1), 2423498.26199),
            ((110, 1, 1), 2434455.261823),
            ((150, 1, 1), 2449064.261982),
            ((180, 1, 1), 2460022.261804),
            ((200, 1, 1), 2467327.261884),
            ((250, 1, 1), 2485589.261786),
            ((280, 1, 1), 2496546.261618),
            ((300, 1, 1), 2503851.261695),
            ((310, 1, 1), 2507503.261435),
            ((380, 1, 1), 2533070.261412),
            ((410, 1, 1), 2544028.261829),
            ((440, 1, 1), 2554985.261644),
            ((480, 1, 1), 2569594.261801),
            ((520, 1, 1), 2584204.261366),
            ((550, 1, 1), 2595161.26177),
            ((580, 1, 1), 2606119.261593),
            ((600, 1, 1), 2613424.261668),
            ((650, 1, 1), 2631686.261563),
            ((680, 1, 1), 2642643.261371),
            ((700, 1, 1), 2649948.261447),
            ((710, 1, 1), 2653600.261192),
            ((780, 1, 1), 2679167.261155),
            ((810, 1, 1), 2690124.261561),
            ((840, 1, 1), 2701082.261379),
            ((880, 1, 1), 2715691.261525),
            ((910, 1, 1), 2726649.261338),
            ((940, 1, 1), 2737606.261141),
            ((980, 1, 1), 2752216.261291),
            ((1000, 1, 1), 2759521.261366),
            ((1040, 1, 1), 2774130.26091),
            ((1080, 1, 1), 2788740.261058),
            ((1100, 1, 1), 2796045.261131),
            ((1110, 1, 1), 2799697.260857),
            ((1160, 1, 1), 2817959.260736),
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

        NOTE: We use the JD derived from the jd_from_badi_date method not the
              jd_from_gregorian_date method. The former will always be on the
              correct day, but are sumtimes not as accurate as the latter.
        """
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 0001-03-19T18:14:32.5536 -> 1721501.260099
            (1721501.261143, *epoch_coords, False, True, True, False, False,
             (-1842, 1, 1, 0, 1, 30.2016)),
            # 0001-04-08T18:30:00 -> 1721521.270833
            (1721521.271398, *epoch_coords, False, True, True, False, False,
             (-1842, 2, 2, 0, 1, 28.6464)),
            # 0002-02-24T17:56:00 -> 1721843.247222
            (1721843.248313, *epoch_coords, False, True, True, False, False,
             (-1842, 0, 1, 0, 1, 43.4208)),
            # 0002-02-25T17:58:00 -> 1721844.248611
            (1721844.248905, *epoch_coords, False, True, True, False, False,
             (-1842, 0, 2, 0, 1, 42.6432)),
            # 0002-02-26T17:59:00 -> 1721845.249306
            (1721845.249493, *epoch_coords, False, True, True, False, False,
             (-1842, 0, 3, 0, 1, 41.952)),
            # 0002-03-02T18:02:00 -> 1721849.251389
            (1721849.251804, *epoch_coords, False, True, True, False, False,
             (-1842, 19, 2, 0, 1, 39.1008)),
            # 0002-03-06T18:05:00 -> 1721853.253472
            (1721853.254053, *epoch_coords, False, True, True, False, False,
             (-1842, 19, 6, 0, 1, 36.5088)),
            # 1583-03-20T18:16:57.1872 -> 2299315.261773
            (2299315.261773, *epoch_coords, False, True, True, False, False,
             (-260, 1, 1, 0, 3, 17.9424)),
            # 1844-03-19T18:16:36.7104 -> 2394643.261536
            (2394643.262113, *epoch_coords, False, True, True, False, False,
             (1, 1, 1, 0, 4, 10.9056)),
            # 1845-03-20T18:16:24.4416 -> 2395009.261394
            (2395009.261972, *epoch_coords, False, True, True,  False, False,
             (2, 1, 1, 0, 3, 20.448)),
            # 1863-03-20T18:16:05.6928 -> 2401583.261177
            (2401583.261756, *epoch_coords, False, True, True, False, False,
             (20, 1, 1, 0, 3, 20.9664)),
            # Three consecutive days that gave me trouble
            (2394641.5, *epoch_coords, False, True, True, False, True,
             (0, 19, 18)),
            (2394642.5, *epoch_coords, False, True, True, False, True,
             (0, 19, 19)),
            (2394643.5, *epoch_coords, False, True, True, False, True,
             (1, 1, 1)),
            # 1970-01-01T:00:00:00Z -> 2440585.5
            (self._bc._POSIX_EPOCH, 51.477928, -0.001545, 0, False, True, True,
             False, False, (126, 16, 2, 8, 3, 7.7184)),
            # 2015-03-20T18:16:06.9888 -> 2457100.261192
            (2457100.261775, *epoch_coords, False, True, True, False, False,
             (172, 1, 1, 0, 3, 22.3488)),
            # 2024-03-19T18:15:57.312 -> 2460387.26108
            (2460387.262245, *epoch_coords, False, True, True, False, False,
             (181, 1, 1, 0, 4, 12.8928)),
            # 2024-04-20T18:42:00 -> 2460419.27975
            (2460419.281258, *epoch_coords, False, True, True,  False, False,
             (181, 2, 14, 0, 5, 1.1904)),
            # 1st day of Ayyám-i-Há -> 2022-02-24T17:57:55.152000
            (2459633.248555, *epoch_coords, False, True, True, False, False,
             (178, 0, 1, 0, 5, 39.8112)),
            # 5th day of Ayyám-i-Há -> 2022-03-28T18:01:35.40000
            (2459637.2511, *epoch_coords, False, True, True, False, False,
             (178, 0, 5, 0, 5, 31.8624)),
            # 2022-03-01T18:02:29.299200 -> 2459638.251728
            (2459638.251728, *epoch_coords, False, True, True, False, False,
             (178, 19, 1, 0, 5, 29.9616)),
            # Badi short form -> 2024-05-12T19:02:00 -> 2460441.293056
            (2460443.29504, *epoch_coords, False, True, True, False, False,
             (181, 3, 19, 0, 4, 48.9216)),
            # 2024-05-12T19:02:00 -> 2460441.293056
            (2460441.29504, *epoch_coords, True, True, True, False, False,
             (181, 3, 17, 0, 6, 27, 417600)),
            # Badi long form -> 2024-05-14T19:02:00 -> 2460443.293056
            (2460443.293056, *epoch_coords, False, False, True, False, False,
             (1, 10, 10, 3, 19, 0, 1, 57.504)),
            (2460443.293056, *epoch_coords, True, False, True, False, False,
             (1, 10, 10, 3, 19, 0, 1, 57, 504000)),
            # 2024-07-17T19:19:00
            (2460507.304861, *epoch_coords, False, True, True, False, False,
             (181, 7, 6, 11, 57, 16.6176)),
            # 2024-07-17T19:19:00 Test fractional day. -> 2460507.304861
            (2460507.304861, *epoch_coords, False, True, True, True, False,
             (181, 7, 6.498109)),
            # Error with hours
            # (2024, 3, 19, 18, 15, 57.312) -> (181, 1, 1)
            (2460387.26108, *epoch_coords, False, True, True, False, False,
             (181, 1, 1, 0, 2, 32.2368)),
            )
        msg = "Expected {} for jd {} for lat {}, lon {}, and zone {}, found {}"

        for (jd, lat, lon, zone, us, short, trim,
             fraction, rtd, expected_result) in data:
            result = self._bc.badi_date_from_jd(
                jd, lat, lon, zone, us=us, short=short, trim=trim,
                fraction=fraction, rtd=rtd)
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

        *** TODO *** In some cases Badi dates are off by almost a day.
        (2024, 3, 19, 18, 15, 57.312) -> (181, 1, 1, 23, 59, 9.6288, 0)
        """
        data = (
            ((1844, 3, 19, 18, 16, 36.7104), False, True, True,
             (1, 1, 1, 1, 1, 0, 3, 21.0528)),
            ((1844, 3, 19, 18, 16, 36.7104), True, True, True,
             (1, 1, 1, 0, 3, 21.0528)),
            ((2024, 5, 14, 20), False, True, True,
             (1, 10, 10, 3, 19, 0, 59, 57.4368)),
            ((2024, 5, 14, 20), True, True, True,
             (181, 3, 19, 0, 59, 57.4368)),
            ((2024, 3, 19, 18, 15, 57.312), True, True, True,
             (181, 1, 1, 0, 2, 32.2368)),
            # The next tests may show the wrong month and day if
            # _exact=False is used.
            # The _exact=False condition is generally used in testing.
            ((1844, 3, 19, 18, 16, 36.7104), True, True, False,
             (1, 1, 3, 0, 1, 40.224)),
            ((2024, 5, 14, 20), True, True, False,
             (181, 4, 2, 0, 58, 19.8912)),
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
             (1844, 3, 19, 18, 17, 26, 563200)),
            ((126, 16, 1), lat, lon, zone, True,
             (1969, 12, 30, 17, 2, 1, 449600)),
            ((181, 3, 18, 20), lat, lon, zone, True,
             (2024, 5, 14, 15, 4, 4, 80000)),
            ((181, 3, 19, 20), lat, lon, zone, True,
             (2024, 5, 15, 15, 4, 51, 427200)),
            ((181, 4, 1, 17), lat, lon, zone, True,
             (2024, 5, 16, 12, 5, 38, 342400)),
            ((181, 4, 1, 20), lat, lon, zone, True,
             (2024, 5, 16, 15, 5, 38, 342400)),
            # The next tests show the wrong day if exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1, 1, 1), lat, lon, zone, False,
             (1844, 3, 17, 18, 17, 26, 563200)),
            ((181, 3, 18, 20), lat, lon, zone, False,
             (2024, 5, 12, 15, 4, 4, 80000)),
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
        """
        local_coords = (35.5894, -78.7792, -5.0)
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        data = (
            # 1970-01-01T00:00:00 -> UNIX Epoch at UTC
            # sunset the day before 16:01 lat=51.4769, lon=0, zone=0
            #                      UTC 12am == (126, 16, 2, 8, 0, 0)
            (0, 51.477928, -0.001545, 0, False, True, True,
             (126, 16, 2, 8, 3, 7.7184)),
            #                      UTC 12am == (1, 7, 12, 16, 2, 8, 0, 0)
            (0, 51.477928, -0.001545, 0, False, False, True,
             (1, 7, 12, 16, 2, 8, 3, 7.7184)),
            # 1969-12-31T23:59:59 This is one second before the POSIX epoch
            (-1, 51.477928, -0.001545, 0, False, True, True,
             (126, 16, 2, 8, 3, 6.7176)),
            (-1, 51.477928, -0.001545, 0, True, True, True,
             (126, 16, 2, 8, 3, 6, 717600)),
            # 2024-08-21T14:33:46.246101 -- Locality in NC USA
            # The h, m, & s are counted from the beginning of the Badi day
            # which would be the previous Gregorian day.
            (1724265226.246101, *local_coords, False, True, True,
             (181, 9, 3, 11, 31, 14.9304)),
            # Just after sunset US/Eastern on (2024, 1, 12, 17, 34, 1, 290939)
            (1736721241.2909386, *local_coords, True, True, False,
             (181, 16, 15, 5, 14, 54, 790800)),
            # Test with zone 3.5 (Tehran Iran) 2024-08-28T00:59:58.549010+3:30
            (1724794198.5490103, *epoch_coords, False, True, True,
             (181, 9, 10, 2, 47, 14.7156)),
            # 2024-08-07T14:04:24-0500
            (1723057467.0619307, *local_coords, False, True, True,
             (181, 8, 8, 11, 46, 47.2512)),
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
            ((1, 1, 1), False, 0.5005840000230819),
            ((1, 1, 1), True, (12, 0, 50.4576)),
            ((181, 11, 4), False, 0.49920950015075505),
            ((181, 11, 4), True, (11, 58, 51.7008)),
            ((1, 1, 1, 1, 1), False, 0.5005840000230819),
            ((1, 1, 1, 1, 1), True, (12, 0, 50.4576)),
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
            # Stage 1
            (2460387.2, (181, 1, 1), *local_coords, True, False, False, False,
             (180, 19, 19.434516)),
            # Stage 2
            (2460406.2, (181, 2, 1), *local_coords, True, False, False, False,
             (181, 1, 19.423559)),
            # Stage 3
            (2460733.2, (181, 19, 1), *local_coords, True, False, False, False,
             (181, 0, 4.446448)),
            # Stage 4
            (2460729.2, (181, 0, 1), *local_coords, True, False, False, False,
             (181, 18, 19.449096)),
            # Test where the JD is >= the sunset. Stage 5
            (2460733.3, (181, 19, 1), *local_coords, False, False, False, False,
             (181, 19, 1, 1, 5, 56.7744)),
            # Test fraction -- Stage 5
            (2460733.3, (181, 19, 1), *local_coords, True, False, False, False,
             (181, 19, 1.045796)),
            # Stage 5
            (2394641.5, (0, 19, 18), *epoch_coords, False, False, True, False,
             (0, 19, 18)),
            (2394642.5, (0, 19, 19), *epoch_coords, False, False, True, False,
             (0, 19, 19)),
            (2394643.5, (1, 1, 1), *epoch_coords, False, False, True, False,
             (1, 1, 1)),
            (2460387.3, (181, 1, 1), *local_coords, False, False, False, False,
             (181, 1, 1, 0, 48, 51.2064)),
            # Test 1st day of the year for a few years
            # 1845-03-20T18:16:24.4416 -> 2395009.261394 Stage 5
            (2395009.261972, (2, 1, 1), *epoch_coords, False, False, False,
             False, (2, 1, 1, 0, 3, 20.448)),
            # 1863-03-20T18:16:05.6928 -> 2401583.261177 Stage 5
            (2401583.261756, (20, 1, 1), *epoch_coords, False, False, False,
             False, (20, 1, 1, 0, 3, 20.9664)),
            # 2015-03-20T18:16:06.9888 -> 2457100.261192 Stage 5
            (2457100.261775, (172, 1, 1), *epoch_coords, False, False, False,
             False, (172, 1, 1, 0, 3, 22.3488)),
            # 2024-03-19T18:15:57.312 -> 2460387.26108 Stage 5
            (2460387.262245, (181, 1, 1), *epoch_coords, False, False, False,
             False, (181, 1, 1, 0, 4, 12.8928)),
            # Test with microseconds
            # JD from jd_from_badi_date() Stage 5
            (2460387.262245, (181, 1, 1), *epoch_coords, False, True, False,
             False, (181, 1, 1, 0, 4, 12, 892800)),
            # JD from jd_from_gregorian_date() Stage 5
            (2460387.26108, (181, 1, 1), *epoch_coords, False, True, False,
             False, (181, 1, 1, 0, 2, 32, 236800)),
            # Error conditions
            (2460733.2, (181, 19, 1), *local_coords, True, True, False, True,
             err_msg0),
            )
        msg = "Expected {} for jd {} and date {}, found {}"

        for (jd, date, lat, lon, zone, fraction,
             us, rtd, validity, expected_result) in data:
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
    def test__day_Length(self):
        """
        Test that the _day_Length method returns the hours, minutes, and
        seconds of a day.
        """
        local_coords = (35.5894, -78.7792, -5.0)
        epoch_coords = self._bc._BAHAI_LOCATION[:3]
        utc_coords = (51.477928, -0.001545, 0) # Greenwich
        data = (
            # BADI epoch location
            ((-1842, 1, 1), epoch_coords, (24, 0, 45.1872)),
            ((-1842, 19, 19), epoch_coords, (24, 0, 45.1872)),
            ((181, 1, 1), epoch_coords, (24, 0, 0.0)),
            ((181, 19, 19), epoch_coords, (24, 0, 50.4576)),
            # Arbitrary location
            ((-1842, 1, 1), local_coords, (24, 0, 45.1008)),
            ((-1842, 19, 19), local_coords, (24, 0, 45.1008)),
            # Earth seasons for Gregorian year 2024 zone -05:00.
            # Perihelion (2024, 1, 2, 19, 38)
            ((180, 16, 4, 2, 34, 36.336), local_coords, (24, 0, 47.6928)),
            # Vernal Equinox (2024, 3, 19, 22, 6)
            ((181, 1, 1, 3, 49, 12.288), local_coords, (24, 0, 50.112)),
            # Summer Solstice (2024, 6, 20, 15, 51)
            ((181, 5, 18, 20, 26, 54.0096), local_coords, (24, 0, 10.5408)),
            # Aphelion (2024, 7, 5, 0, 6)
            ((181, 6, 13, 4, 41, 49.1712), local_coords, (23, 59, 48.8544)),
            # Fall Equinox (2024, 9, 22, 7, 44)
            ((181, 10, 16, 13, 42, 37.3824), local_coords, (23, 58, 32.0448)),
            # Winter Solstice (2024, 12, 21, 4, 20)
            ((181, 15, 11, 11, 24, 40.6944), local_coords, (24, 0, 29.8944)),
            # Earth seasons for Gregorian year 2024 zone -00:00.
            # Perihelion (2024, 1, 3, 0, 38)
            ((180, 16, 4, 7, 34, 36.3072), utc_coords, (24, 1, 6.096)),
            # Vernal Equinox (2024, 3, 20, 3, 6)
            ((181, 1, 1, 8, 49, 12.3456), utc_coords, (24, 1, 41.6064)),
            # Summer Solstice (2024, 6, 20, 20, 51)
            ((181, 5, 18, 1, 26, 53.9808), utc_coords, (24, 0, 11.5776)),
            # Aphelion (2024, 7, 5, 5, 6)
            ((181, 6, 13, 9, 41, 49.1424), utc_coords, (23, 59, 28.2048)),
            # Fall Equinox (2024, 9, 22, 12, 44)
            ((181, 10, 17, 18, 44, 5.4816), utc_coords, (23, 57, 42.0192)),
            # Winter Solstice (2024, 12, 21, 9, 20)
            ((181, 15, 11, 16, 24, 40.6656), utc_coords, (24, 0, 32.4)),
            )
        msg = "Expected {} for date {}, and location {}, found {}"

        for (date, location, expected_result) in data:
            jd = self._bc.jd_from_badi_date(date, *location)
            result = self._bc._day_Length(jd, *location)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, jd, location, result))
