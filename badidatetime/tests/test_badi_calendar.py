# -*- coding: utf-8 -*-
#
# badidatetime/test/test_badi_calendar.py
#
__docformat__ = "restructuredtext en"

import unittest
import datetime

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
            # Badi epoch (Sunset 1844-03-20T18:16:58.7424)
            ((1844, 3, 20, 18, 16, 58.7424), False,
             (1, 1, 1, 1, 1, 0, 0, 49.0752)),
            # CC ch#16 p271 First day of Riḍván
            ((1930, 4, 21, 18, 41), False, (1, 5, 11, 2, 12, 23, 58, 22.8864)),
            # B.E. 100 (Vernal Equinox 1943-03-21T12:03:04 DT)
            ((1943, 3, 21, 18, 16), False, (1, 6, 5, 1, 1)),
            # World Centre update (Vernal Equinox 2015-03-20T22:46:16 DT)
            ((2015, 3, 21, 18, 16), True, (172, 1, 1)),
            )
        msg = "Expected {} with g_date {}, found {}"

        for g_date, short, expected_result in data:
            if len(g_date) == 6:
                sm = self._bc._sec_microsec_from_seconds(g_date[-1])
            else:
                sm = ()

            date = g_date[:5] + sm if sm else g_date
            dt = datetime.datetime(*date)
            self._bc.parse_gregorian_datetime(dt, short=short)
            result = self._bc.date_representation
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    #@unittest.skip("Temporarily skipped")
    def test_utc_sunset(self):
        """
        Test that the utc_sunset method returns the universal time of
        sunset on fixed date. This results in the UTC time of sunset.
        See: https://gml.noaa.gov/grad/solcalc/
        """
        lat, lon, zone = self._bc.BAHAI_LOCATION[:3]
        data = (
            # Should be 1844-03-20T18:14:00
            ((1, 1, 1, 2), None, None, None, True, (18, 16, 58.7424)),
            # Should be 2024-03-19T18:13:00
            ((180, 19, 19), lat, lon, zone, True, (18, 16, 19.8624)),
            # Should be 2064-03-19T18:13:00
            ((221, 1, 1), lat, lon, zone, True, (18, 16, 33.7728)),
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
        lat, lon, zone = self._bc.BAHAI_LOCATION[:3]
        data = (
            # 1844-03-20T18:14:00
            (1, lat, lon, zone, False, True, (1844, 3, 20.761791)),
            # 1844-03-20T18:14:00
            (1, lat, lon, zone, True, True, (1844, 3, 20, 18, 16, 58.7424)),
            # 2024-03-20T18:26:48.336
            (181, 35.7796, -78.6382, -5, True, True,
             (2024, 3, 20, 18, 27, 38.6208)),
            # 2025-03-20T18:14:00
            (182, lat, lon, zone, False, True, (2025, 3, 20.761783)),
            # 2026-03-21T18:14:00
            (183, lat, lon, zone, False, True, (2026, 3, 21.761643)),
            # The following years are the ones that had errors.
            # 2021-03-20T18:14:00
            (178, lat, lon, zone, False, True, (2021, 3, 20.761763)),
            # 2030-03-20T18:14:00
            (187, lat, lon, zone, False, True, (2030, 3, 20.761661)),
            # 2034-03-20T18:14:00
            (191, lat, lon, zone, False, True, (2034, 3, 20.761673)),
            # 2038-03-20T18:14:00
            (195, lat, lon, zone, False, True, (2038, 3, 20.761685)),
            # 2054-03-20T18:14:00
            (211, lat, lon, zone, False, True, (2054, 3, 20.761167)),
            # 2059-03-20T18:14:00
            (216, lat, lon, zone, False, True, (2059, 3, 20.761622)),
            # 2063-03-20T18:14:00
            (220, lat, lon, zone, False, True, (2063, 3, 20.761643)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 3, 20.761791)),
            # 1993-03-21T18:14:00 Test sunset before Vernal Equinox
            (150, lat, lon, zone, False, True, (1993, 3, 21.76166)),
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
        lat, lon, zone = self._bc.BAHAI_LOCATION[:3]
        data = (
            # 0001-02-13T00:00:00 -> 1844-04-20T18:42:00
            (1, lat, lon, zone, False, True, (1844, 4, 20.780039)),
            (1, lat, lon, zone, True, True, (1844, 4, 20, 18, 43, 15.3696)),
            # 0181-02-13T19:41:00-04:00 DST at Raleigh NC, USA
            (181, 35.7796, -78.6382, -4, False, True, (2024, 4, 20.828798)),
            (181, 35.7796, -78.6382, -4, True, True,
             (2024, 4, 20, 19, 53, 28.1472)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 4, 20.780039)),
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
            ((1, 1, 1), 2394644.261791),            # 1844-03-20T18:13:47.0208
            ((1, 1, 1, 18, 14), 2394645.022088),    # 1844-03-21T12:28:39.1008
            ((19, 19, 19), 2401583.261434),         # 1863-03-20T18:13:14.7072
            ((180, 19, 19), 2460387.261341),        # 2024-03-19T18:13:06.4128
            ((181, 3, 2), 2460427.285018),          # 2024-04-28T18:48:41.5296
            ((-260, 1, 1, 18, 16), 2299317.023131), # 1583-03-22T12:30:8.5248
            # A day in Ayyám-i-Há 2022-02-25T17:53:20.2272
            ((178, 0, 1), 2459634.248233),
            ((181, 3, 19, 20), 2460445.128049),     # 2024-05-16T15:03:13.7088
            # Test one date for each coefficient.
            ((-1842, 1, 1), 1721502.260821),
            ((-1800, 1, 1), 1736843.260751),
            ((-1750, 1, 1), 1755105.260724),
            ((-1720, 1, 1), 1766062.261116),
            ((-1700, 1, 1), 1773367.26121),
            ((-1690, 1, 1), 1777019.260998),
            ((-1620, 1, 1), 1802586.261583),
            ((-1590, 1, 1), 1813544.26145),
            ((-1580, 1, 1), 1817196.261239),
            ((-1520, 1, 1), 1839111.262035),
            ((-1480, 1, 1), 1853720.261686),
            ((-1450, 1, 1), 1864678.262094),
            ((-1420, 1, 1), 1875635.26196),
            ((-1400, 1, 1), 1882940.262052),
            ((-1350, 1, 1), 1901202.262007),
            ((-1320, 1, 1), 1912159.262948),
            ((-1300, 1, 1), 1919464.263037),
            ((-1290, 1, 1), 1923116.262279),
            ((-1220, 1, 1), 1948683.263392),
            ((-1190, 1, 1), 1959641.263801),
            ((-1160, 1, 1), 1970598.263663),
            ((-1120, 1, 1), 1985208.264381),
            ((-1080, 1, 1), 1999817.264028),
            ((-1060, 1, 1), 2007122.264117),
            ((-1020, 1, 1), 2021732.264293),
            ((-1000, 1, 1), 2029037.264381),
            ((-950, 1, 1), 2047299.264338),
            ((-920, 1, 1), 2058256.264734),
            ((-900, 1, 1), 2065561.264823),
            ((-890, 1, 1), 2069213.264601),
            ((-820, 1, 1), 2094780.265185),
            ((-790, 1, 1), 2105738.265582),
            ((-770, 1, 1), 2113042.265672),
            ((-720, 1, 1), 2131305.266167),
            ((-700, 1, 1), 2138609.266255),
            ((-660, 1, 1), 2153219.265884),
            ((-620, 1, 1), 2167829.266063),
            ((-600, 1, 1), 2175134.266152),
            ((-550, 1, 1), 2193396.266092),
            ((-520, 1, 1), 2204353.266507),
            ((-500, 1, 1), 2211658.266593),
            ((-490, 1, 1), 2215310.266357),
            ((-420, 1, 1), 2240877.266937),
            ((-390, 1, 1), 2251835.267353),
            ((-360, 1, 1), 2262792.2672),
            ((-320, 1, 1), 2277402.267936),
            ((-280, 1, 1), 2292011.267555),
            ((-250, 1, 1), 2302969.261769),
            ((-220, 1, 1), 2313926.261613),
            ((-200, 1, 1), 2321231.261692),
            ((-150, 1, 1), 2339493.261613),
            ((-120, 1, 1), 2350450.26144),
            ((-100, 1, 1), 2357755.261522),
            ((-90, 1, 1), 2361407.26128),
            ((-20, 1, 1), 2386974.261274),
            ((10, 1, 1), 2397932.261675),
            ((40, 1, 1), 2408889.261512),
            ((80, 1, 1), 2423499.261668),
            ((110, 1, 1), 2434456.261501),
            ((150, 1, 1), 2449066.26166),
            ((180, 1, 1), 2460023.261482),
            ((200, 1, 1), 2467328.261562),
            ((250, 1, 1), 2485590.261464),
            ((280, 1, 1), 2496547.261296),
            ((300, 1, 1), 2503852.261373),
            ((310, 1, 1), 2507504.261113),
            ((380, 1, 1), 2533071.26109),
            ((410, 1, 1), 2544029.261507),
            ((440, 1, 1), 2554986.261322),
            ((480, 1, 1), 2569596.261479),
            ((520, 1, 1), 2584205.261044),
            ((550, 1, 1), 2595163.261447),
            ((580, 1, 1), 2606120.261271),
            ((600, 1, 1), 2613425.261346),
            ((650, 1, 1), 2631687.261241),
            ((680, 1, 1), 2642644.261049),
            ((700, 1, 1), 2649949.261125),
            ((710, 1, 1), 2653601.26087),
            ((780, 1, 1), 2679168.260833),
            ((810, 1, 1), 2690126.261239),
            ((840, 1, 1), 2701083.261057),
            ((880, 1, 1), 2715693.261203),
            ((910, 1, 1), 2726650.261016),
            ((940, 1, 1), 2737607.260819),
            ((980, 1, 1), 2752217.260969),
            ((1000, 1, 1), 2759522.261044),
            ((1040, 1, 1), 2774131.260588),
            ((1080, 1, 1), 2788741.260736),
            ((1100, 1, 1), 2796046.260809),
            ((1110, 1, 1), 2799698.260535),
            ((1160, 1, 1), 2817960.260414),
            ((1162, 1, 1), 2818691.260725), # This 1 beyond what we guarantee.
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc.jd_from_badi_date(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_jd(self):
        """
        Test that the badi_date_from_jd method returns the correct Badi date.

        Run: ./contrib/misc/gregorian_jd_tests.py -jS<start_date> -E<end_date>
        to find the jd values below. It's best to do a year at a time.
        """
        lat, lon, zone = self._bc.BAHAI_LOCATION[:3]
        data = (
            # 0001-03-20T18:16:00
            (1721502.2603, lat, lon, zone, True, (-1842, 1, 1)),
            # 0001-04-08T18:30:00
            (1721521.270049, lat, lon, zone, True, (-1842, 2, 1)),
            # 0002-02-24T17:56:00
            (1721843.246795, lat, lon, zone, True, (-1842, 18, 19)),
            # 0002-02-25T17:57:00
            (1721844.247395, lat, lon, zone, True, (-1842, 0, 1)),
            # 0002-02-26T17:58:00
            (1721845.247992, lat, lon, zone, True, (-1842, 0, 2)),
            # 0002-03-02T18:01:00
            (1721849.250336, lat, lon, zone, True, (-1842, 19, 1)),
            # 0002-03-06T18:05:00
            (1721853.252614, lat, lon, zone, True, (-1842, 19, 5)),
            # 1583-03-21T18:16:00
            (2299316.26202, lat, lon, zone, True, (-260, 1, 1, 0, 0,49.1616)),
            # 1844-03-20T18:16:00
            (2394644.261791, lat, lon, zone, True, (1, 1, 1, 0, 0, 49.8528)),
            # 1845-03-20T18:16:00
            (2395009.26165, lat, lon, zone, True, (1, 19, 19)),
            # 1863-03-21T18:16:00
            (2401584.26201, lat, lon, zone, True, (20, 1, 1, 0, 0, 49.7664)),
            # 1970-01-01T:00:00:00Z
            (self._bc.POSIX_EPOCH, 51.477928, -0.001545, 0, True,
             (126, 16, 1, 7, 59, 32.496)),
            # 2015-03-21T18:16:00
            (2457101.262034, lat, lon, zone, True, (172, 1, 1, 0, 0, 50.1984)),
            # 2024-03-20T18:16:00
            (2460388.261923, lat, lon, zone, True, (181, 1, 1, 0, 0, 50.2848)),
            # 2024-04-20T18:42:00
            (2460419.27977, lat, lon, zone, True, (181, 2, 13)),
            # 1st day of Ayyám-i-Há -> 2022-02-25T17:56:00
            (2459634.247588, lat, lon, zone, True, (178, 0, 1)),
            # 2022-03-01T17:59:00
            (2459638.250148, lat, lon, zone, True, (178, 0, 5)),
            # 2022-03-02T18:00:00
            (2459639.250779, lat, lon, zone, True, (178, 19, 1)),
            # Badi short form -> 2024-05-14T19:02:00
            (2460443.293616, lat, lon, zone, True, (181, 3, 18)),
            # Badi long form -> 2024-05-14T19:02:00
            (2460443.293616, lat, lon, zone, False, (1, 10, 10, 3, 18)),
            # 2024-07-17T19:19:00
            (2460507.30472, lat, lon, zone, True, (181, 7, 6)),
            )
        msg = "Expected {} for jd {} for lat {}, lon {}, and zone {}, found {}"

        for jd, lat, lon, zone, short, expected_result in data:
            result = self._bc.badi_date_from_jd(
                jd, lat, lon, zone, short=short)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, jd, lat, lon, zone, result))

    #@unittest.skip("Temporarily skipped")
    def test_short_date_from_long_date(self):
        """
        Test that the short_date_from_long_date method returns the correct
        (year, month, day, hour, minute, seconds, ms) date.
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
            # 2024-08-27T13:37:58.651870-4:00
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799),
             (181, 9, 8, 19, 1, 3, 532799)),
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
            # 1843-03-21T21:18:14
            ((0, 1, 1), (0, 19, 19, 1, 1)),
            # 1842-03-21T00:00:00
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
            ((1, 1, 1, 1, 1.761111), False, False,
             (1, 1, 1, 1, 1, 18, 15, 59.9904)),
            ((1, 1, 1, 1, 1.761111), False, True, (1, 1, 1, 18, 15, 59.9904)),
            ((0, 6, 6, 1, 1, 1), False, True, (-260, 1, 1, 1)),
            ((1, 10, 10, 9, 8, 19, 1, 3.5328), True, False,
             (1, 10, 10, 9, 8, 19, 1, 3, 532799)),
            ((1, 10, 10, 9, 8, 19, 1, 3.5328), True, True,
             (181, 9, 8, 19, 1, 3, 532799)),
            )
        msg = "Expected {} for date {} with ms {} and short {}, found {}"

        for date, ms, short, expected_result in data:
            result = self._bc.kvymdhms_from_b_date(date, ms=ms, short=short)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, ms, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_gregorian_date(self):
        """
        Test that the badi_date_from_gregorian_date method returns the
        correct Badi date.
        """
        data = (
            ((1844, 3, 20, 18, 16), False, True, (1, 1, 1, 1, 1)),
            ((1844, 3, 20, 18, 16), True, True, (1, 1, 1)),
            ((2024, 5, 14, 20), False, True,
             (1, 10, 10, 3, 18, 0, 57, 11.5488)),
            ((2024, 5, 14, 20), True, True, (181, 3, 18, 0, 57, 11.5488)),
            # The next tests may show the wrong month and day if
            # exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1844, 3, 20, 18, 16), True, False, (1, 1, 2, 23, 57, 21.9744)),
            ((2024, 5, 14, 20), True, False, (181, 4, 1, 0, 55, 36.5088)),
            )
        msg = "Expected {} for date {}, short {} and exact {}, found {}"

        for date, short, exact, expected_result in data:
            result = self._bc.badi_date_from_gregorian_date(
                date, short=short, _exact=exact)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, short, exact, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_date_from_badi_date(self):
        """
        Test that the gregorian_date_from_badi_date method returns the
        correct Gregorian date.
        """
        lat, lon, zone = self._bc.BAHAI_LOCATION[:3]
        data = (
            # 1844-03-20T18:14:00
            ((1, 1, 1), lat, lon, zone, True, (1844, 3, 20, 18, 16, 58.7424)),
            ((126, 16, 1), lat, lon, zone, True, (1969, 12, 31, 17, 1, 33.888)),
            ((181, 3, 18, 20), lat, lon, zone, True,
             (2024, 5, 15, 15, 3, 36.0864)),
            ((181, 3, 19, 20), lat, lon, zone, True,
             (2024, 5, 16, 15, 4, 23.4336)),
            ((181, 4, 1, 17), lat, lon, zone, True,
             (2024, 5, 17, 12, 5, 10.3488)),
            ((181, 4, 1, 20), lat, lon, zone, True,
             (2024, 5, 17, 15, 5, 10.3488)),
            # The next tests show the wrong day if exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1, 1, 1), lat, lon, zone, False, (1844, 3, 18, 18, 16, 58.7424)),
            ((181, 3, 18, 20), lat, lon, zone, False,
             (2024, 5, 13, 15, 3, 36.0864)),
            )
        msg = "Expected {} for date {} and exact {}, found {}"

        for date, lat, lon, zone, exact, expected_result in data:
            g_date = self._bc.gregorian_date_from_badi_date(
                date, lat=lat, lon=lon, zone=zone, _exact=exact)
            result = self._gc.ymdhms_from_date(g_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, exact, result))

    #@unittest.skip("Temporarily skipped")
    def test_posix_timestamp(self):
        """
        Test that the posix_timestamp method returns the correct Badi
        date with a POSIX timestamp as input.

        The lat and lon at GMT is:
        https://www.latlong.net/place/prime-meridian-greenwich-30835.html#:~:text=Prime%20Meridian%20(Greenwich)%20Lat%20Long,%C2%B0%200'%205.5620''%20W.
        """
        data = (
            # 1970-01-01T00:00:00 -> UNIX Epoch at UTC
            # sunset the day before 16:01 lat=51.4769, lon=0, zone=0
            #                      UTC 12am == (126, 16, 1, 8, 0, 0)
            (0, 51.477928, -0.001545, 0, True, (126, 16, 1, 7, 59, 32.496)),
            #                      UTC 12am == (1, 7, 12, 16, 1, 8, 0, 0)
            (0, 51.477928, -0.001545, 0, False,
             (1, 7, 12, 16, 1, 7, 59, 32.496)),
            # 1969-12-31T23:59:59 This is one second before the POSIX epoch
            (-1, 51.477928, -0.001545, 0, True, (126, 16, 1, 7, 59, 31.4592)),
            # 2024-08-24T14:33:46.24610090255737 -- Raleigh, NC USA
            # The h, m, & s are counted from the beginning of the Badi day
            # which would be the previous Gregorian day.
            (1724265226.246101, 35.7796, -78.6382, -4, True,
             (181, 9, 2, 22, 37, 46.992)),
            # Test with zone 3.5 (Tehran Iran) 2024-08-28T01:00:00+3:30
            (1724794198.5490103, None, None, None, True,
             (181, 9, 9, 2, 53, 4.1568)),
            )
        msg = "Expected {} for timestamp {}, found {}"

        for t, lat, lon, zone, short, expected_result in data:
            result = self._bc.posix_timestamp(t, lat, lon, zone, short=short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, t, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_valid_badi_date(self):
        """
        Test that the _check_valid_badi_date method returns the
        correct Boolean for valid and invalid dates.

        Note: The Boolean below in the data statements determines whether
              or not the data is valid or invalid.
        """
        msg0 = ("The number of Váḥids in a Kull-i-Shay’ should be >= 1 or "
                "<= 19, found {}")
        msg1 = ("The number of years in a Váḥid should be >= 1 or <= 19, "
                "found {}")
        msg2 = "Invalid month '{}', should be 0 - 19."
        msg3 = ("Invalid day '{}' for month '{}' and year '{}' "
                "should be from 1 to <= {{}}.")
        msg4 = "Invalid hour '{}' it must be 0 <= {} < 24"
        msg5 = "Invalid minute '{}' should be 0 <= {} < 60."
        msg6 = ("If there is a part day then there can be no hours, "
                "minutes, or seconds.")
        msg7 = ("If there is a part hour then there can be no minutes or "
                "seconds.")
        msg8 = "If there is a part minute then there can be no seconds."
        msg9 = "Microsecond value {} > 1000000."
        data = (
            ((1, 1, 1, 1, 1), True, ''),  # Non leap year
            ((1, 10, 3, 1, 1), True, ''), # Known leap year
            ((0, 1, 1, 1, 1), True, ''),  # Before Badi epoch
            ((-1, 1, 1, 1, 1), True, ''), # Before Badi epoch
            ((1, 10, 2, 0, 1), True, ''), # During Ayyám-i-Há non leap year
            ((1, 10, 3, 0, 1), True, ''), # During Ayyám-i-Há leap year
            ((1, 10, 10, 9, 8, 19, 1, 3, 532799), True, ''),
            # Invalid Váḥid
            ((1, 0, 1, 1, 1, 1, 1, 1), False, msg0.format(0)),
            ((1, 20, 1, 1, 1, 1, 1, 1), False, msg0.format(20)),
            # Invalid year
            ((1, 10, 0, 1, 1, 1, 0, 0), False, msg1.format(0)),
            ((1, 10, 20, 1, 1, 1, 0, 0), False, msg1.format(20)),
            # Invalid month
            ((1, 10, 10, -1, 1, 1, 0, 0), False, msg2.format(-1)),
            ((1, 10, 10, 20, 1, 1, 0, 0), False, msg2.format(20)),
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
            # Invalid microsecond
            ((1, 10, 10, 9, 8, 19, 1, 3, 1532799), False, msg9.format(1532799)),
            )

        for b_date, validity, err_msg in data:
            kull_i_shay, vahid, year, month, day = b_date[:5]
            hour, minute, second, ms = self._bc._get_hms(b_date)
            cycle = 4 + self._bc._is_leap_year(b_date)

            if validity: # Test for correct dates
                if month == 0: # Ayyám-i-Há

                    for d in range(1, cycle+1):
                        date = (kull_i_shay, vahid, year, month, d)
                        self._bc._check_valid_badi_date(date)

                for m in range(1, 20):
                    for d in range(1, 20):
                        date = (kull_i_shay, vahid, year, m, d)
                        self._bc._check_valid_badi_date(date)
            else: # Test for invalid dates
                try:
                    with self.assertRaises(AssertionError) as cm:
                        self._bc._check_valid_badi_date(b_date)
                except AssertionError as e:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(
                        f"Váḥid {vahid}, year {year}, month {month}, "
                        f"day {day}, hour {hour}, minute {minute}, "
                        f"second {second}, {e}")
                else:
                    num_days = cycle if month == 0 else 19
                    message = str(cm.exception)
                    self.assertEqual(err_msg.format(num_days), message)

    #@unittest.skip("Temporarily skipped")
    def test__is_leap_year(self):
        """
        Test that the _is_leap_year method returns the correct Boolean
        for the given year or long Badi date.
        """
        err_msg = ("If a tuple it must be at least the Kull-i-Shay', Váḥid, "
                   "and year, found {}")
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
            ((181, 1), False, err_msg),        # Test assert error.
            )
        msg = "Expected {} for day {}, found {}"

        for date, validity, expected_result in data:
            if validity:
                result = self._bc._is_leap_year(date)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, date, result))
            else:
                with self.assertRaises(AssertionError) as cm:
                    self._bc._is_leap_year(date)

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
        msg = "Expected {} for date {} amd short {}, found {}"

        for date, short, expected_result in data:
            result = self._bc._get_hms(date, short=short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, short, result))

    #@unittest.skip("Temporarily skipped")
    def test__meeus_algorithm_jd_compensation(self):
        """
        Test that the _meeus_algorithm_jd_compensation method returns
        the correct difference needed to compensate for the differences
        in mime and Meesus' algorithms.
        """
        data = (
            (1757641.5, 0),
            (1757642.5, 1),
            (1794164.5, 1),
            (1794165.5, 2),
            (1830688.5, 2),
            (1830689.5, 3),
            (1903737.5, 3),
            (1903738.5, 4),
            (1940261.5, 4),
            (1940262.5, 5),
            (1976785.5, 5),
            (1976786.5, 6),
            (2049834.5, 6),
            (2049835.5, 7),
            (2086358.5, 7),
            (2086359.5, 8),
            (2122882.5, 8),
            (2122883.5, 9),
            (2195931.5, 9),
            (2195932.5, 10),
            (2232455.5, 10),
            (2232456.5, 11),
            (2268979.5, 11),
            (2268980.5, 12),
            (2299157.5, 12),
            (2299158.5, 2),
            (2460388.26032, 2),
            )
        msg = "Expected {} for jd {}, found {}"

        for jd, expected_result in data:
            result = self._bc._meeus_algorithm_jd_compensation(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test__adjust_day_for_24_hours(self):
        """
        Test that the _adjust_day_for_24_hours method returns the
        correct length of day between two sunsets.
        """
        lat, lon, zone = self._bc.BAHAI_LOCATION[:3]
        data = (
            # Test hms mode
            # 2024-03-20 Vernal Equinox
            (2460388.261923, lat, lon, zone, None, True, (0, 0, 50.2848)),
            # 2024-06-20 Summer Solstice
            (2460479.5, lat, lon, zone, None, True, (0, 0, 12.6144)),
            # 2024-09-22T19:44:00+3:30 Fall Equinox
            (2460574.322222, lat, lon, zone, None, True, (23, 58, 32.1312)),
            # 2024-12-21 Winter Solstice
            (2460663.5, lat, lon, zone, None, True, (0, 0, 31.0176)),
            # 1844-03-23 (1844, 3, 23.7603)
            (2394647.2603,  lat, lon, zone, None, True, (0, 0, 49.3344)),
            # 2024-03-04 (2024, 3, 23.7603)
            (2460391.2603,  lat, lon, zone, None, True, (0, 0, 49.8528)),
            # Test > 24 day mode
            # 1844-03-20 (1844, 3, 20.761791)
            (2394644.261791, lat, lon, zone, 1, False, 1.261214000172913),
            # 1844-03-23 (1844, 3, 23.7603) (1, 1, 4)
            (2394647.2603, lat, lon, zone, 4, False, 4.259729000274092),
            # 2024-03-04 (2024, 3, 23.7603) (181, 1, 4)
            (2460391.2603, lat, lon, zone, 4, False, 4.259723000228405),
            # Test < 24 day mode
            # 2024-09-22T19:44:00+3:30 Fall Equinox (181, 10, 16.073624)
            (2460574.322222, lat, lon, zone, 15, False, 16.073624),
            )
        msg = "Expected {} for value {}, day {}, and hms {}, found {}"

        for value, lat, lon, zone, day, hms, expected_result in data:
            result = self._bc._adjust_day_for_24_hours(value, lat, lon, zone,
                                                       day=day, hms=hms)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, value, day, hms, result))
