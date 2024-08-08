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
            # Badi epoch (Sunset 1844-03-20T18:14:00)
            ((1844, 3, 20, 18, 14), False, (1, 1, 1, 1, 1)),
            # CC ch#16 p271 First day of Riḍván
            ((1930, 4, 21, 18, 41), False, (1, 5, 11, 2, 12, 23, 59, 33.648)),
            # B.E. 100 (Vernal Equinox 1943-03-21T12:03:04 DT)
            ((1943, 3, 21, 18, 14), False, (1, 6, 5, 1, 1)),
            # World Centre update (Vernal Equinox 2015-03-20T22:46:16 DT)
            ((2015, 3, 21, 18, 14), True, (172, 1, 1)),
            )
        msg = "Expected {} with g_date {}, found {}"

        for g_date, short, expected_result in data:
            dt = datetime.datetime(*g_date)
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
            ((1, 1, 1, 2), None, None, None, True, (18, 14, 39.12)),
            # Should be 2024-03-19T18:13:00
            ((180, 19, 19), lat, lon, zone, True, (18, 13, 59.1168)),
            # Should be 2064-03-19T18:13:00
            ((221, 1, 1), lat, lon, zone, True, (18, 14, 13.8912)),
            # Should be 2024-04-20T19:53:00 DST in Raleigh NC
            ((181, 2, 13), 35.7796, -78.6382, -4, True, (19, 53, 28.4064)),
            # Should be 2024-07-22T20:26:00 DST in Raleigh NC
            ((181, 7, 11), 35.7796, -78.6382, -4, True, (20, 25, 47.568)),
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
            (1, lat, lon, zone, False, True, (1844, 3, 20.760175)),
            # 1844-03-20T18:14:00
            (1, lat, lon, zone, True, True, (1844, 3, 20, 18, 14, 39.12)),
            # 2024-03-20T18:26:48.336
            (181, 35.7796, -78.6382, -5, True, True,
             (2024, 3, 20, 18, 27, 38.88)),
            # 2025-03-20T18:14:00
            (182, lat, lon, zone, False, True, (2025, 3, 20.760174)),
            # 2026-03-21T18:14:00
            (183, lat, lon, zone, False, True, (2026, 3, 21.760028)),
            # The following years are the ones that had errors.
            # 2021-03-20T18:14:00
            (178, lat, lon, zone, False, True, (2021, 3, 20.760153)),
            # 2030-03-20T18:14:00
            (187, lat, lon, zone, False, True, (2030, 3, 20.760046)),
            # 2034-03-20T18:14:00
            (191, lat, lon, zone, False, True, (2034, 3, 20.760059)),
            # 2038-03-20T18:14:00
            (195, lat, lon, zone, False, True, (2038, 3, 20.760072)),
            # 2054-03-20T18:14:00
            (211, lat, lon, zone, False, True, (2054, 3, 20.759532)),
            # 2059-03-20T18:14:00
            (216, lat, lon, zone, False, True, (2059, 3, 20.760008)),
            # 2063-03-20T18:14:00
            (220, lat, lon, zone, False, True, (2063, 3, 20.76003)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 3, 20.760175)),
            # 1993-03-21T18:14:00 Test sunset before Vernal Equinox
            (150, lat, lon, zone, False, True, (1993, 3, 21.760044)),
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
            # 0001-02-13T18:40:00 (1844-04-20T18:40:00)
            (1, lat, lon, zone, False, True, (1844, 4, 20.779239)),
            (1, lat, lon, zone, True, True, (1844, 4, 20, 18, 42, 6.2496)),
            # 0181-02-13T19:41:00-04:00 DST at Raleigh NC, USA
            (181, 35.7796, -78.6382, -4, False, True, (2024, 4, 20.828801)),
            (181, 35.7796, -78.6382, -4, True, True,
             (2024, 4, 20, 19, 53, 28.4064)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 4, 20.779239)),
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
            ((1, 1, 1), 2394644.260175),            # 1844-03-20T18:13:47.0208
            ((1, 1, 1, 18, 14), 2394645.020499),    # 1844-03-21T12:28:39.1008
            ((19, 19, 19), 2401583.259803),         # 1863-03-20T18:13:14.7072
            ((180, 19, 19), 2460387.259712),        # 2024-03-19T18:13:06.4128
            ((181, 3, 2), 2460427.284421),          # 2024-04-28T18:48:41.5296
            ((-260, 1, 1, 18, 16), 2299317.021527), # 1583-03-22T12:30:8.5248
            # A day in Ayyám-i-Há 2022-02-25T17:53:20.2272
            ((178, 0, 1), 2459634.246044),
            ((181, 3, 19, 20), 2460445.127808),     # 2024-05-16T15:03:13.7088
            # Test one date for each coefficent.
            ((-1842, 1, 1), 1721502.259568),
            ((-1800, 1, 1), 1736843.259477),
            ((-1750, 1, 1), 1755105.25943),
            ((-1720, 1, 1), 1766062.25983),
            ((-1700, 1, 1), 1773367.25992),
            ((-1690, 1, 1), 1777019.259693),
            ((-1620, 1, 1), 1802586.260281),
            ((-1590, 1, 1), 1813544.26013),
            ((-1580, 1, 1), 1817196.260457),
            ((-1520, 1, 1), 1839111.260716),
            ((-1480, 1, 1), 1853720.260335),
            ((-1450, 1, 1), 1864678.26075),
            ((-1420, 1, 1), 1875635.260598),
            ((-1400, 1, 1), 1882940.260687),
            ((-1350, 1, 1), 1901202.26062),
            ((-1320, 1, 1), 1912159.261595),
            ((-1300, 1, 1), 1919464.26168),
            ((-1290, 1, 1), 1923116.260882),
            ((-1220, 1, 1), 1948683.262022),
            ((-1190, 1, 1), 1959641.26244),
            ((-1160, 1, 1), 1970598.262284),
            ((-1120, 1, 1), 1985208.263023),
            ((-1080, 1, 1), 1999817.262639),
            ((-1060, 1, 1), 2007122.262725),
            ((-1020, 1, 1), 2021732.262897),
            ((-1000, 1, 1), 2029037.262983),
            ((-950, 1, 1), 2047299.262923),
            ((-920, 1, 1), 2058256.26333),
            ((-900, 1, 1), 2065561.263419),
            ((-890, 1, 1), 2069213.263184),
            ((-820, 1, 1), 2094780.263779),
            ((-790, 1, 1), 2105738.264189),
            ((-770, 1, 1), 2113042.264279),
            ((-720, 1, 1), 2131305.26479),
            ((-700, 1, 1), 2138609.264879),
            ((-660, 1, 1), 2153219.264485),
            ((-620, 1, 1), 2167829.264667),
            ((-600, 1, 1), 2175134.264759),
            ((-550, 1, 1), 2193396.264691),
            ((-520, 1, 1), 2204353.265123),
            ((-500, 1, 1), 2211658.265212),
            ((-490, 1, 1), 2215310.264963),
            ((-420, 1, 1), 2240877.265567),
            ((-390, 1, 1), 2251835.266),
            ((-360, 1, 1), 2262792.265839),
            ((-320, 1, 1), 2277402.266607),
            ((-280, 1, 1), 2292011.266207),
            ((-250, 1, 1), 2302969.260154),
            ((-220, 1, 1), 2313926.259989),
            ((-200, 1, 1), 2321231.260072),
            ((-150, 1, 1), 2339493.259987),
            ((-120, 1, 1), 2350450.259808),
            ((-100, 1, 1), 2357755.259893),
            ((-90, 1, 1), 2361407.259641),
            ((-20, 1, 1), 2386974.259635),
            ((10, 1, 1), 2397932.260055),
            ((40, 1, 1), 2408889.259885),
            ((80, 1, 1), 2423499.26005),
            ((110, 1, 1), 2434456.259876),
            ((150, 1, 1), 2449066.260044),
            ((180, 1, 1), 2460023.259859),
            ((200, 1, 1), 2467328.259944),
            ((250, 1, 1), 2485590.259847),
            ((280, 1, 1), 2496547.259674),
            ((300, 1, 1), 2503852.259757),
            ((310, 1, 1), 2507504.259487),
            ((380, 1, 1), 2533071.259472),
            ((410, 1, 1), 2544029.259911),
            ((440, 1, 1), 2554986.259721),
            ((480, 1, 1), 2569596.259892),
            ((520, 1, 1), 2584205.259443),
            ((550, 1, 1), 2595163.25987),
            ((580, 1, 1), 2606120.259691),
            ((600, 1, 1), 2613425.259773),
            ((650, 1, 1), 2631687.259673),
            ((680, 1, 1), 2642644.259479),
            ((700, 1, 1), 2649949.259563),
            ((710, 1, 1), 2653601.259298),
            ((780, 1, 1), 2679168.259276),
            ((810, 1, 1), 2690126.259706),
            ((840, 1, 1), 2701083.259524),
            ((880, 1, 1), 2715693.259687),
            ((910, 1, 1), 2726650.2595),
            ((940, 1, 1), 2737607.259303),
            ((980, 1, 1), 2752217.25947),
            ((1000, 1, 1), 2759522.259554),
            ((1040, 1, 1), 2774131.259091),
            ((1080, 1, 1), 2788741.259257),
            ((1100, 1, 1), 2796046.259339),
            ((1110, 1, 1), 2799698.259057),
            ((1160, 1, 1), 2817960.259583),
            ((1162, 1, 1), 2818691.259272), # This 1 beyond what we guarantee.
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
        """
        data = (
            # 0001-03-20T18:13:00
            (1721502.259568, True, (-1842, 1, 1, 0, 0, 47.2608)),
            ## # 0001-04-08T18:24:00
            (1721520.269258, True, (-1842, 1, 19, 0, 0, 46.4832)),
            ## # 0001-
            (1721843.245527, True, (-1842, 18, 19, 0, 0, 54.0)),
            (1721844.246149, True, (-1842, 0, 1, 0, 0, 53.7408)),
            (1721845.246766, True, (-1842, 0, 2, 0, 0, 53.3088)),
            ## # 0001-
            (1721849.249193, True, (-1842, 19, 1, 0, 0, 51.9264)),
            ## # 0001-
            (1721853.251557, True, (-1842, 19, 5, 0, 0, 50.544)),
            ## # 1583-03-19T18:11:33.5328
            (2299316.259821, True, (-260, 1, 1)),
            # 1844-03-20T18:12:2.3904
            (2394644.259572, True, (1, 1, 1)),
            (2395009.260028, True, (1, 19, 19)),
            # 1863-03-21T18:11:29.7312
            (2401584.259803, True, (20, 1, 1)),
            # 2015-03-21T18:14:00
            (2457101.259829, True, (172, 1, 1)),
            (2457101.259722, True, (172, 1, 1)),
            # 2024-03-20T18:12:2.3904
            (2460388.259712, True, (181, 1, 1)),
            # 2024-04-20T18:39:5.1552
            (2460419.278959, True, (181, 2, 13)),
            # 1st day of Ayyám-i-Há -> 2022-02-25T17:50:24.2304
            (2459634.245373, True, (178, 0, 1)),
            # 2022-03-01T17:54:18.2016
            (2459638.248036, True, (178, 0, 5)),
            # 2022-03-02T17:55:15.9168
            (2459639.248693, True, (178, 19, 1)),
            # Badi short form -> 2024-05-14T18:59:54.3264
            (2460443.293338, True, (181, 3, 18)),
            # Badi long form -> 2024-05-14T18:59:54.3264
            (2460443.293338, False, (1, 10, 10, 3, 18)),
            (2460507.450424, True, (181, 7, 6, 3, 29, 59.9712)),
            )
        msg = "Expected {} for jd {}, found {}"

        for jd, short, expected_result in data:
            result = self._bc.badi_date_from_jd(jd, short=short)
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
            ((1, 1, 1, 1, 1.761111), False, (1, 1, 1, 1, 1, 18, 15, 59.9904)),
            ((1, 1, 1, 1, 1.761111), True, (1, 1, 1, 18, 15, 59.9904)),
            ((0, 6, 6, 1, 1, 1), True, (-260, 1, 1)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, short, expected_result in data:
            result = self._bc.kvymdhms_from_b_date(date, short=short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_badi_date_from_gregorian_date(self):
        """
        Test that the badi_date_from_gregorian_date method returns the
        correct Badi date.
        """
        data = (
            ((1844, 3, 20, 18, 14), False, True, (1, 1, 1, 1, 1)),
            ((1844, 3, 20, 18, 14), True, True, (1, 1, 1)),
            ((2024, 5, 14, 20), False, True, (1, 10, 10, 3, 18, 0, 57, 35.568)),
            ((2024, 5, 14, 20), True, True, (181, 3, 18, 0, 57, 35.568)),
            # The next tests may show the wrong month and day if
            # exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1844, 3, 20, 18, 14), True, False, (1, 1, 2, 23, 57, 37.008)),
            ((2024, 5, 14, 20), True, False, (181, 4, 1, 0, 55, 57.3312)),
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
            ((1, 1, 1), lat, lon, zone, True, (1844, 3, 20, 18, 14, 39.12)),
            ((181, 3, 18, 20), lat, lon, zone, True,
             (2024, 5, 15, 15, 3, 13.7088)),
            ((181, 3, 19, 20), lat, lon, zone, True,
             (2024, 5, 16, 15, 4, 2.6112)),
            ((181, 4, 1, 17), lat, lon, zone, True,
             (2024, 5, 17, 12, 4, 51.168)),
            ((181, 4, 1, 20), lat, lon, zone, True,
             (2024, 5, 17, 15, 4, 51.168)),
            # The next tests show the wrong day if exact=False is used.
            # The exact=False condition is generally used in testing.
            ((1, 1, 1), lat, lon, zone, False, (1844, 3, 18, 18, 14, 39.12)),
            ((181, 3, 18, 20), lat, lon, zone, False,
             (2024, 5, 13, 15, 3, 13.7088)),
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
        Test that the posix_timestamp method returnds the correct Badi
        date with a POSIX timestamp as input.
        """
        data = (
            # 1970-01-01 -> UNIX Eppoch (1970, 1, 1.049148)
            (1, False, (1, 7, 12, 16, 1, 7, 3, 13.9104)),
            (1, True, (126, 16, 1, 7, 3, 13.9104)),
            # 2024-07-24T06:55:08.688 *** TODO *** This was wrong was 4 AM
            (1722067088.6303926, True, (181, 7, 15, 12, 45, 53.136))
            )
        msg = "Expected {} for timestamp {}, found {}"

        for t, short, expected_result in data:
            result = self._bc.posix_timestamp(t, short=short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, t, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_valid_badi_month_day(self):
        """
        Test that the _check_valid_badi_month_day method returns the
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
            )

        for b_date, validity, err_msg in data:
            kull_i_shay, vahid, year, month, day = b_date[:5]
            hour, minute, second = self._bc._get_hms(b_date)
            cycle = 4 + self._bc._is_leap_year(b_date)

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
            ((1, 1, 1, 1, 1, 18, 16), False, (18, 16, 0)),
            ((1, 1, 1, 18, 16), True, (18, 16, 0)),
            )
        msg = "Expected {} for date {} amd short {}, found {}"

        for date, short, expected_result in data:
            result = self._bc._get_hms(date, short=short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, short, result))

    #@unittest.skip("Temporarily skipped")
    def test__meeus_algorithm_date_compensation(self):
        """
        Test that the _meeus_algorithm_date_compensation method returns
        the correct difference needed to compensate for the differences
        in mime and Meesus' algorithms.
        """
        data = (
            ((-1842, 1, 1), 0),
            ((-1744, 0, 4, 6, 2, 44.5055), 0),
            ((-1744, 0, 4, 6, 2, 44.5056), 1),
            ((-1644, 0, 4, 6, 3, 0.9215), 1),
            ((-1644, 0, 4, 6, 3, 0.9216), 2),
            ((-1544, 0, 3, 6, 3, 17.1647), 2),
            ((-1544, 0, 3, 6, 3, 17.1648), 3),
            ((-1344, 0, 4, 6, 2, 59.711), 3),
            ((-1344, 0, 4, 6, 2, 59.712), 4),
            ((-1244, 0, 4, 6, 3, 18.1151), 4),
            ((-1244, 0, 4, 6, 3, 18.1152), 5),
            ((-1144, 0, 3, 6, 3, 37.9007), 5),
            ((-1144, 0, 3, 6, 3, 37.9008), 6),
            ((-944, 0, 4, 6, 3, 22.0031), 6),
            ((-944, 0, 4, 6, 3, 22.0032), 7),
            ((-844, 0, 4, 6, 3, 42.047), 7),
            ((-844, 0, 4, 6, 3, 42.048), 8),
            ((-744, 0, 3, 6, 4, 1.3151), 8),
            ((-744, 0, 3, 6, 4, 1.3152), 9),
            ((-544, 0, 4, 6, 3, 46.1087), 9),
            ((-544, 0, 4, 6, 3, 46.1088), 10),
            ((-444, 0, 4, 6, 4, 6.23), 10),
            ((-444, 0, 4, 6, 4, 6.24), 11),
            ((-344, 0, 3, 6, 4, 27.6671), 11),
            ((-344, 0, 3, 6, 4, 27.6672), 12),
            ((-261, 11, 18, 6, 32, 0.23), 12),
            ((-261, 11, 18, 6, 32, 0.24), 2),
            ((1, 1, 1), 2),
            ((181, 1, 1), 2),
            )
        msg = "Expected {} for date {}, found {}"

        for date, expected_result in data:
            result = self._bc._meeus_algorithm_date_compensation(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

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
