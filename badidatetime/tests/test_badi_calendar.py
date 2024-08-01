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
            ((1844, 3, 20, 18, 14), False, (1, 1, 1, 1, 1, 0, 0, 12.96)),
            # CC ch#16 p271 First day of Riḍván
            ((1930, 4, 21, 18, 41), False, (1, 5, 11, 2, 13, 0, 0, 25.7472)),
            # B.E. 100 (Vernal Equinox 1943-03-21T12:03:04 DT)
            ((1943, 3, 21, 18, 14), False, (1, 6, 5, 1, 1, 0, 0, 16.9344)),
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
            ((1, 1, 1, 2), lat, lon, zone, True, (18, 13, 47.0208)),
            # Should be 2024-03-19T18:13:00
            ((180, 19, 19), lat, lon, zone, True, (18, 13, 6.4128)),
            # Should be 2064-03-19T18:13:00
            ((221, 1, 1), lat, lon, zone, True, (18, 14, 13.8912)),
            # Should be 2024-04-20T19:53:00 DST in Raleigh NC
            ((181, 2, 13), 35.7796, -78.6382, -4, True, (19, 52, 37.8624)),
            # Should be 2024-07-22T20:26:00 DST in Raleigh NC
            ((181, 7, 11), 35.7796, -78.6382, -4, True, (20, 26, 29.1264)),
            # Test default latitude, longitude, and zone.
            ((1, 1, 1, 2), lat, lon, zone, False, (18, 13, 47.0208)),
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
            (1, lat, lon, zone, False, True, (1844, 3, 20.759572)),
            # 1844-03-20T18:14:00
            (1, lat, lon, zone, True, True, (1844, 3, 20, 18, 13, 47.0208)),
            # 2024-03-20T18:26:48.336
            (181, 35.7796, -78.6382, -5, True, True,
             (2024, 3, 20, 18, 26, 48.336)),
            # 2025-03-20T18:14:00
            (182, lat, lon, zone, False, True, (2025, 3, 20.759566)),
            # 2026-03-21T18:14:00
            (183, lat, lon, zone, False, True, (2026, 3, 21.760028)),
            # The following years are the ones that had errors.
            # 2021-03-20T18:14:00
            (178, lat, lon, zone, False, True, (2021, 3, 20.759545)),
            # 2030-03-20T18:14:00
            (187, lat, lon, zone, False, True, (2030, 3, 20.760046)),
            # 2034-03-20T18:14:00
            (191, lat, lon, zone, False, True, (2034, 3, 20.760059)),
            # 2038-03-20T18:14:00
            (195, lat, lon, zone, False, True, (2038, 3, 20.760072)),
            # 2054-03-20T18:14:00
            (211, lat, lon, zone, False, True, (2054, 3, 20.759532)),
            # 2059-03-20T18:14:00
            (216, lat, lon, zone, False, True, (2059, 3, 20.759398)),
            # 2063-03-20T18:14:00
            (220, lat, lon, zone, False, True, (2063, 3, 20.75942)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 3, 20.759572)),
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
            (1, lat, lon, zone, False, True, (1844, 4, 20.778639)),
            (1, lat, lon, zone, True, True, (1844, 4, 20, 18, 41, 14.4096)),
            # 0181-02-13T19:41:00-04:00 DST at Raleigh NC, USA
            (181, 35.7796, -78.6382, -4, False, True, (2024, 4, 20.828216)),
            (181, 35.7796, -78.6382, -4, True, True,
             (2024, 4, 20, 19, 52, 37.8624)),
            # Test default latitude, longitude, and zone.
            (1, lat, lon, zone, False, False, (1844, 4, 20.778639)),
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
            ((1, 1, 1), 2394644.259572),            # 1844-03-20T18:13:47.0208
            ((1, 1, 1, 18, 14), 2394645.019897),    # 1844-03-21T12:28:39.1008
            ((19, 19, 19), 2401583.259198),         # 1863-03-20T18:13:14.7072
            ((180, 19, 19), 2460387.259102),        # 2024-03-19T18:13:06.4128
            ((181, 3, 2), 2460427.283814),          # 2024-04-28T18:48:41.5296
            ((-260, 1, 1, 18, 16), 2299317.020932), # 1583-03-22T12:30:8.5248
            # A day in Ayyám-i-Há 2022-02-25T17:53:20.2272
            ((178, 0, 1), 2459634.245373),
            ((181, 3, 19, 20), 2460445.127242),     # 2024-05-16T15:03:13.7088
            # Test one date for each coefficent.
            ((-1842, 1, 1), 1721502.259568),
            ((-1800, 1, 1), 1736843.259477),
            ((-1750, 1, 1), 1755105.25943),
            ((-1720, 1, 1), 1766062.25928),
            ((-1700, 1, 1), 1773367.259369),
            ((-1690, 1, 1), 1777019.259142),
            ((-1620, 1, 1), 1802586.259176),
            ((-1590, 1, 1), 1813544.259576),
            ((-1580, 1, 1), 1817196.25935),
            ((-1520, 1, 1), 1839111.259607),
            ((-1480, 1, 1), 1853720.259221),
            ((-1450, 1, 1), 1864678.259638),
            ((-1420, 1, 1), 1875635.259482),
            ((-1400, 1, 1), 1882940.25957),
            ((-1350, 1, 1), 1901202.2595),
            ((-1320, 1, 1), 1912159.259355),
            ((-1300, 1, 1), 1919464.259439),
            ((-1290, 1, 1), 1923116.259195),
            ((-1220, 1, 1), 1948683.259208),
            ((-1190, 1, 1), 1959641.259626),
            ((-1160, 1, 1), 1970598.259463),
            ((-1120, 1, 1), 1985208.259637),
            ((-1080, 1, 1), 1999817.259239),
            ((-1060, 1, 1), 2007122.259323),
            ((-1020, 1, 1), 2021732.25949),
            ((-1000, 1, 1), 2029037.259573),
            ((-950, 1, 1), 2047299.259503),
            ((-920, 1, 1), 2058256.259334),
            ((-900, 1, 1), 2065561.25942),
            ((-890, 1, 1), 2069213.259179),
            ((-820, 1, 1), 2094780.259189),
            ((-790, 1, 1), 2105738.259599),
            ((-770, 1, 1), 2113042.259685),
            ((-720, 1, 1), 2131305.259611),
            ((-700, 1, 1), 2138609.259695),
            ((-660, 1, 1), 2153219.259282),
            ((-620, 1, 1), 2167829.259456),
            ((-600, 1, 1), 2175134.259544),
            ((-550, 1, 1), 2193396.259462),
            ((-520, 1, 1), 2204353.259304),
            ((-500, 1, 1), 2211658.259388),
            ((-490, 1, 1), 2215310.259131),
            ((-420, 1, 1), 2240877.259133),
            ((-390, 1, 1), 2251835.259564),
            ((-360, 1, 1), 2262792.259389),
            ((-320, 1, 1), 2277402.259563),
            ((-280, 1, 1), 2292011.25914),
            ((-250, 1, 1), 2302969.260154),
            ((-220, 1, 1), 2313926.259989),
            ((-200, 1, 1), 2321231.260072),
            ((-150, 1, 1), 2339493.259987),
            ((-120, 1, 1), 2350450.259807),
            ((-100, 1, 1), 2357755.259893),
            ((-90, 1, 1), 2361407.25964),
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
            ((700, 1, 1), 2649949.259562),
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
            ((1160, 1, 1), 2817960.258948),
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
            (1721502.259568, True, (-1842, 1, 1)),
            ## # 0001-04-08T18:24:00
            (1721520.269258, True, (-1842, 1, 19)),
            ## # 0001-
            (1721843.245527, True, (-1842, 18, 19)),
            (1721844.246149, True, (-1842, 0, 1)),
            (1721845.246766, True, (-1842, 0, 2)),
            ## # 0001-
            (1721849.249193, True, (-1842, 19, 1)),
            ## # 0001-
            (1721853.251557, True, (-1842, 19, 5)),
            ## # 1583-03-19T18:11:33.5328
            (2299316.259821, True, (-260, 1, 1)),
            # 1844-03-20T18:12:2.3904
            (2394644.259572, True, (1, 1, 1)),
            (2395009.260028, True, (1, 19, 19, 0, 0, 52.0992)),
            # 1863-03-21T18:11:29.7312
            (2401584.259803, True, (20, 1, 1)),
            # 2015-03-21T18:14:00
            (2457101.259829, True, (172, 1, 1)),
            (2457101.259722, True, (172, 1, 1)),
            # 2024-03-20T18:12:2.3904
            (2460388.259712, True, (181, 1, 1)),
            # 2024-04-20T18:39:5.1552
            (2460419.278959, True, (181, 2, 13, 0, 0, 52.3584)),
            # 1st day of Ayyám-i-Há -> 2022-02-25T17:50:24.2304
            (2459634.245373, True, (178, 0, 1, 0, 0, 58.32)),
            # 2022-03-01T17:54:18.2016
            (2459638.248036, True, (178, 0, 5, 0, 0, 57.024)),
            # 2022-03-02T17:55:15.9168
            (2459639.248693, True, (178, 19, 1, 0, 0, 56.7648)),
            # Badi short form -> 2024-05-14T18:59:54.3264
            (2460443.293338, True, (181, 3, 18, 0, 0, 49.68)),
            # Badi long form -> 2024-05-14T18:59:54.3264
            (2460443.293338, False, (1, 10, 10, 3, 18, 0, 0, 49.68)),
            (2460507.450424, True, (181, 7, 6, 3, 29, 25.7568)),
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

        # *** TODO *** Test for the assert error.

    @unittest.skip("Temporarily skipped")
    def test__days_in_year(self):
        """
        Test that the _days_in_year method returns the correct number of
        days in the current year.
        """
        # *** TODO *** Write the test.
        pass



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
    def test_badi_date_from_gregorian_date(self):
        """
        Test that the badi_date_from_gregorian_date method returns the
        correct Badi date.
        """
        data = (
            ((1844, 3, 20, 18, 14), False, (1, 1, 1, 1, 1, 0, 0, 12.96)),
            ((1844, 3, 20, 18, 14), True, (1, 1, 1, 0, 0, 12.96)),
            ((2024, 5, 14, 20), False, (1, 10, 10, 3, 18, 0, 58, 25.248)),
            ((2024, 5, 14, 20), True, (181, 3, 18, 0, 58, 25.248)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, short, expected_result in data:
            result = self._bc.badi_date_from_gregorian_date(date, short=short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_gregorian_date_from_badi_date(self):
        """
        Test that the gregorian_date_from_badi_date method returns the
        correct Gregorian date.
        """
        lat, lon, zone = self._bc.BAHAI_LOCATION[:3]
        data = (
            ((1, 1, 1), lat, lon, zone, (1844, 3, 18.759572)),
            ((181, 3, 18, 20), lat, lon, zone, (2024, 5, 13.626671)),
            ((181, 3, 19, 20), lat, lon, zone, (2024, 5, 14.627242)),
            ((181, 4, 1, 17), lat, lon, zone, (2024, 5, 15.502808)),
            ((181, 4, 1, 20), lat, lon, zone, (2024, 5, 15.627808)),
            )
        msg = "Expected {} for date {}, found {}"

        for date, lat, lon, zone, expected_result in data:
            result = self._bc.gregorian_date_from_badi_date(date, lat=lat,
                                                            lon=lon, zone=zone)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_posix_timestamp(self):
        """
        Test that the posix_timestamp method returnds the correct Badi
        date with a POSIX timestamp as input.
        """
        data = (
            # 1970-01-01 -> UNIX Eppoch (1970, 1, 1.049148)
            (1, False, (1, 7, 12, 16, 1, 7, 3, 59.5296)),
            (1, True, (126, 16, 1, 7, 3, 59.5296)),
            # 2024-07-24T06:55:08.688 *** TODO *** This was wrong was 4 AM
            (1722067088.6303926, True, (181, 7, 15, 12, 45, 4.4928))
            )
        msg = "Expected {} for timestamp {}, found {}"

        for t, short, expected_result in data:
            result = self._bc.posix_timestamp(t, short=short)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, t, result))
