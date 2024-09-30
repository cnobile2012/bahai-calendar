# -*- coding: utf-8 -*-
#
# badidatetime/test/test_base_calendar.py
#
__docformat__ = "restructuredtext en"

import os
import math
import unittest
import datetime

from ..base_calendar import BaseCalendar
from ..gregorian_calendar import GregorianCalendar


class TestBaseCalendar(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        class FakeParent(BaseCalendar):
            # Location of Tehran for astronomical Baha’i calendar.
            # Can be change for individual tests using the location
            # method below.
            latitude = 36.569336
            longitude = 52.0050234
            zone = 3.5
            elevation = 0

            def location(self, location):
                if location[0] is not None:
                    self.latitude = location[0]

                if location[1] is not None:
                    self.longitude = location[1]

                if location[2] is not None:
                    self.elevation = location[2]

                if location[3] is not None:
                    self.zone = location[3]
            location = property(None, location)

        self.bc = FakeParent()
        self.gc = GregorianCalendar()

    #@unittest.skip("Temporarily skipped")
    def test_HR(self):
        """
        Test the HR (hour) lambda.
        """
        value = 6
        hr = self.bc.HR(value)
        expected_hr = 0.25
        msg = f"HR should be {expected_hr}, found {hr}."
        self.assertEqual(expected_hr, hr, msg)

    #@unittest.skip("Temporarily skipped")
    def test_MN(self):
        """
        Test the MN (minute) lambda.
        """
        value = 6
        mn = self.bc.MN(value)
        expected_mn = 0.00416666666666666667
        msg = f"MN should be {expected_mn}, found {mn}."
        self.assertEqual(expected_mn, mn, msg)

    #@unittest.skip("Temporarily skipped")
    def test_SEC(self):
        """
        Test the sec (second) lambda.
        """
        value = 6
        sec = self.bc.SEC(value)
        expected_sec = 6.94444444444444444444e-5
        msg = f"SEC should be {expected_sec}, found {sec}."
        self.assertEqual(expected_sec, sec, msg)

    #@unittest.skip("Temporarily skipped")
    def test_MINS(self):
        """
        Test the MINS (minutes) lambda.
        """
        value = 6
        mins = self.bc.MINS(value)
        expected_mins = 0.1
        msg = f"MINS should be {expected_mins}, found {mins}."
        self.assertEqual(expected_mins, mins, msg)

    #@unittest.skip("Temporarily skipped")
    def test_SECS(self):
        """
        Test the SECS (seconds) lambda.
        """
        value = 6
        secs = self.bc.SECS(value)
        expected_secs = 0.00166666666666666667
        msg = f"SECS should be {expected_secs}, found {secs}."
        self.assertEqual(expected_secs, secs, msg)

    #@unittest.skip("Temporarily skipped")
    def test_ANGLE(self):
        """
        Test the ANGLE (angle) lambda.
        """
        d, m, s = (23, 26, 21.448)
        angle = self.bc.ANGLE(d, m ,s)
        expected_angle = 23.43929111111111
        msg = f"ANGLE should be {expected_angle}, found {angle}."
        self.assertEqual(expected_angle, angle, msg)

    #@unittest.skip("Temporarily skipped")
    def test_AMOD(self):
        """
        Test the AMOD lambda.
        """
        data = ((2, 2, 2), (5, 2, 1), (5, -2, -1))
        msg = "Expected {} with args '{}, {}', found {}"

        for x, y, expected_result in data:
            result = self.bc.AMOD(x, y)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, x, y, result))

    #@unittest.skip("Temporarily skipped")
    def test_MOD3(self):
        """
        Test the MOD3 (modular three) lambda.
        """
        # Test a == b
        x, a, b = (25, 25, 100)
        mod3 = self.bc.MOD3(x, a ,b)
        expected_mod3 = x if a == b else a + (x - a) % (b - a)
        msg = f"MOD3 should be {expected_mod3}, found {mod3}."
        self.assertEqual(expected_mod3, mod3, msg)
        # Test a != b
        x, a, b = (25, 10, 100)
        mod3 = self.bc.MOD3(x, a ,b)
        expected_mod3 = x if a == b else a + (x - a) % (b - a)
        msg = f"MOD3 should be {expected_mod3}, found {mod3}."
        self.assertEqual(expected_mod3, mod3, msg)

    #@unittest.skip("Temporarily skipped")
    def test_QUOTIENT(self):
        """
        Test the QUOTIENT (divid, floor, int) lambda,
        """
        m = 19
        n = 2
        quotient = self.bc.QUOTIENT(m, n)
        expected_quotient = int(math.floor(m / n))
        msg = f"QUOTIENT should be {expected_quotient}, found {quotient}."
        self.assertEqual(expected_quotient, quotient, msg)

    #@unittest.skip("Temporarily skipped")
    def test_parse_datetime(self):
        """
        Test that the parse_datetime method creates an instance object.
        """
        now = datetime.datetime.now()
        self.bc.parse_datetime(now)
        expected_dt = (now.hour, now.minute, now.second)
        msg = f"Expected {expected_dt}, found {self.bc._time}"
        self.assertTrue(
            all([i == j for i, j in zip(expected_dt,
                                        self.bc.time_representation)]), msg)

    #@unittest.skip("Temporarily skipped")
    def test_delta_t(self):
        """
        Test that the delta_t method returns the correct delta T value
        for converting between UT and DT time.

        To check for valid values see: https://planetcalc.com/9200/
        """
        data = (
            ((-600, 1, 1), True, 18719.834672222227), # OK
            ((0, 1, 1), True, 10583.17733503136),     # OK
            ((900, 1, 1), True, 2199.998764705048),   # OK
            ((900, 3, 1), True, 2198.842046841268),   # OK
            ((905, 1, 1), True, 2165.4555994434895),  # OK
            ((1650, 1, 1), True, 50.13316097312479),  # OK
            ((1750, 1, 1), True, 13.375979008768493), # OK
            ((1830, 1, 1), True, 7.6550062338922995), # OK
            ((1885, 1, 1), True, -5.65258999265227),  # OK
            ((1910, 1, 1), True, 10.445380968083992), # OK
            ((1935, 1, 1), True, 23.81634451255787),  # OK
            ((1951, 1, 1), True, 29.48974515233175),  # OK
            # 2443192.6511574076 JD DT -- 48 AA Ex.10.a
            ((1977, 2, 1), True, 47.686642722506434), # OK
            # 2447240.5 JD DT -- +56 AA Ex.15.a
            ((1988, 3, 20), True, 55.8792447106014),
            ((2000, 1, 1), True, 63.873832810959236), # OK
            ((2010, 1, 1), True, 66.71869095312503),
            ((2010, 1, 1), False, 0.0007722070712167249),
            ((2020, 1, 1), True, 71.62174845312504),  # OK
            ((2100, 1, 1), True, 202.8381222222219),  # -93 WRONG
            ((2200, 1, 1), True, 442.18133888888855), # 103 WRONG
            )
        msg = "Expected {}, with seconds {}, found {}."

        for g_date, seconds, expected_result in data:
            jde = self.gc.jd_from_gregorian_date(g_date)
            result = self.bc.delta_t(jde, seconds=seconds)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, seconds, result))

    #@unittest.skip("Temporarily skipped")
    def test__mean_sidereal_time_greenwich(self):
        """
        Test that the _mean_sidereal_time_greenwich method returns the
        correct mean sidereal time.
        """
        data = (
            # 2446895.5 -- 197.693195 -- AA ch.12 p.88 Ex.12.a
            ((1987, 4, 10), 197.693195090862),
            # 128.73788708333333
            ((1987, 4, 10, 19, 21), 128.73787324433215),
            )
        msg = "Expected {}, for date {}, found {}."

        for g_date, expected_result in data:
            jde = self.gc.jd_from_gregorian_date(g_date)
            tc = self.bc.julian_centuries(jde)
            result = self.gc._mean_sidereal_time_greenwich(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    #@unittest.skip("Temporarily skipped")
    def test__apparent_sidereal_time_greenwich(self):
        """
        Test that the apparent_sidereal_time_greenwich method returns
        the correct apparent sidereal time.
        """
        data = (
            # 197.69222958 -- AA ch.12 p.88 Ex.12.a
            ((1987, 4, 10), 197.69222977202386),
            ((1987, 4, 10, 19, 21), 128.73688780647933), # 128.73690333333334
            ((2000, 1, 1), 99.96424623619367),           # 99.96424875
            ((2024, 3, 20), 178.01765584602592),         # 178.0176425
            )
        msg = "Expected {}, for date {}, found {}."

        for g_date, expected_result in data:
            jde = self.gc.jd_from_gregorian_date(g_date)
            tc = self.bc.julian_centuries(jde)
            result = self.gc._apparent_sidereal_time_greenwich(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    @unittest.skip("Temporarily skipped")
    def test__altitude(self):
        """
        Test that the _altitude method returns the correct altitude in degrees.
        """
        func = lambda m: m + 1 if m <= 0 else m - 1 if m >= 1 else m

        def inter_ra(tc, n):
            a0 = self.bc._sun_apparent_right_ascension(tc - (1 / 36525))
            a1 = self.bc._sun_apparent_right_ascension(tc)
            a2 = self.bc._sun_apparent_right_ascension(tc + (1 / 36525))
            return self.bc.interpolation_from_three(a0, a1, a2, n)

        def inter_de(tc, n):
            d0 = self.bc._sun_apparent_declination(tc - (1 / 36525))
            d1 = self.bc._sun_apparent_declination(tc)
            d2 = self.bc._sun_apparent_declination(tc + (1 / 36525))
            return self.bc.interpolation_from_three(d0, d1, d2, n)

        data = (
            # 1987-04-10T19:21:00 -- 2446896.30625, lat, lon
            # AA Ex.13.b alpha=347.3193, delta=  +15.1249
            ((1987, 4, 10, 19, 21), 38.921388, -77.065416, 0),
            )
        msg = "Expected {}, for date {}, with lat {} and lon {}, found {}."

        for g_date, lat, lon, expected_result in data:
            jd = self.gc.jd_from_gregorian_date(g_date)
            tc = self.bc.julian_centuries(jd)
            ast = self.bc._apparent_sidereal_time_greenwich(tc)
            dt = self.gc.delta_t(jd)
            jde = jd + dt
            # Find alpha
            ara = self.bc._sun_apparent_right_ascension(tc)
            ma = func((ara - lon - ast) / 360)
            alpha = inter_ra(tc, ma * dt / 86400)
            # Find delta
            de = self.bc._sun_apparent_declination(tc)
            md = func((de - lon - ast) / 360)
            delta = inter_de(tc, ma * dt / 86400)
            # Find the local hour angle
            #srt = ast + 360.98564736629 *
            h = self.bc._local_hour_angle(ast, lon, alpha)
            result = self.bc._altitude(delta, lat, h)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, g_date, lat, lon, result))

    #@unittest.skip("Temporarily skipped")
    def test__approx_local_hour_angle(self):
        """
        Test that the _approx_local_hour_angle method returns the
        correct angle based on the jde.

        The variable 'self.bc.latitude' is the latitude in Tehran Iran.
        """
        offset = self.bc.SUN_OFFSET
        data = (
            ((1844, 3, 20), self.bc.latitude, offset, 90.89234108635455),
            ((1988, 3, 20), 42.3333, offset, 90.98306896131454), # AA Ex15.a
            ((2024, 6, 20), self.bc.latitude, offset, 109.9589991005709),
            # Test for combinations of latitude and degrees that cause the
            # cos_h0 value to be less than -1.
            ((1844, 4, 10.5), 90, offset, 90.0),
            # cos_h0 value to be greater than 1.
            ((1844, 4, 13.5), -90, offset, 60.00000000000001),
            )
        msg = "Expected {}, for date {}, found {}."

        for g_date, lat, offset, expected_result in data:
            jde = self.gc.jd_from_gregorian_date(g_date)
            tc = self.bc.julian_centuries(jde)
            result = self.bc._approx_local_hour_angle(tc, lat, offset)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_rising(self):
        """
        """
        data = (
            # 1844-03-20T12:00:00 -> 1844-03-21T06:05:57.0912 (06:06)
            (2394646.0, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2394646.754133),
            # 2024-03-19T12:00:00 -> 2024-03-20T06:05:17.1744 (06:07)
            (2460389.0, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2460389.753669),
            # 2024-03-20T00:00:00 -> 2024-03-20T06:05:17.1744 (06:05)
            (2460389.5, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2460389.753669),
            # 2024-03-20T02:00:00 -> 2024-03-20T06:05:17.1744 (06:06)
            (2460389.583333, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2460389.753669),
            )
        msg = "Expected {}, for jd {}, found {}."

        for jd, lat, lon, zone, expected_result in data:
            result = self.bc._sun_rising(jd, lat, lon, zone)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_transit(self):
        """
        Test that the _sun_transit method returns the correct transition
        based on the jde.
        """
        data = (
            # 1988-03-20T00:00:00 -- 0.5354166666666667 = 12:51 pm Alt 48 deg
            # AA Ex.15.a 0.8198 at Boston, US
            # JD        Longitude
            (2447240.5, -71.0833, -5.0, False, 0.49423327921649307),
            # 2024-03-20T00:00:00 -- 0.5048611111111111 = 12:07 pm, Alt 39 deg
            # Transit in Greenwich UK with 51.477928 (lat) and -0.001545 (lon)
            (2460389.5, -0.001545, 0, False, 0.5050828012152296),
            # 2024-03-20T00:00:00 -- 0.5076388888888889 = 12:11 pm, Alt 54 deg
            # Transit in Tehran Iran with 35.696111 (lat) and 51.423056 (lon)
            (2460389.5, 51.423056, 3.5, False, 0.5080994320882255),
            # 2024-03-20T00:00:00 -- 0.5051123582525348 = 12:07:0.36179588
            # Transit in Tehran Iran with 35.696111 (lat) and 51.423056 (lon)
            (2460389.5, 51.423056, 0, True, 0.5051079209771144),
            )
        msg = "Expected {}, for jd {}, zone {}, and exact {}, found {}."

        for jd, lon, zone, exact, expected_result in data:
            result = self.bc._sun_transit(jd, lon, zone=zone, exact=exact)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, jd, zone, exact, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_setting(self):
        """
        Test that the _sun_setting method returns the correct sunset for a
        given date represented by a Julian Period day.
        """
        data = (
            # 1844-03-20T12:00:00 -> 1844-03-20T18:13:47.0208
            (2394646.0, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2394646.259572),
            # 2024-03-19T12:00:00 -> 2024-03-19T18:13:59.1168
            (2460389.0, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2460389.259709),
            # 2024-03-20T00:00:00 -> 2024-03-20T18:13:59.1168
            (2460389.5, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2460390.259709),
            # 2024-03-20T02:00:00 -> 2024-03-20T18:13:59.1168
            (2460389.583333, self.bc.latitude, self.bc.longitude, self.bc.zone,
             2460390.259709),
            # 2024-04-20T00:00:00 -> 2024-04-20T19:52:378624 DST (19:53)
            # In Raligh NC, USA
            (2460420.5, 35.7796, -78.6382, -4, 2460421.328213)
            )
        msg = "Expected {}, for jd {}, found {}."

        for jd, lat, lon, zone, expected_result in data:
            result = self.bc._sun_setting(jd, lat, lon, zone)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test__rising_setting(self):
        """
        https://www.timeanddate.com/sun/usa/boston?month=3&year=1988
        rise=5.47 am, set=5.56 pm
        lat 42.364506, lon -71.038887
        (2447240.5, -71.0833, -5.0, 0),
        """
        data = (
            # 1988-03-20T00:00:00 -- 0.51766, 0.1213
            # AA Ex.15.a  at Boston, US
            # JD        Latitude Longitude zone  exact
            (2447240.5, 42.3333, -71.0833, -5.0, False,
             self.bc.STARS_PLANET_OFFSET,
             (0.24210617024415584, 0.7468650828479303)),
            # 2024-03-20T00:00:00 -- (0.250694 = 6:01 am, 0.759027 = 6:13 pm)
          # https://www.timeanddate.com/sun/uk/greenwich-city?month=3&year=2024
            # In Greenwich UK with 51.477928 (lat) and -0.001545 (lon)
            (2460389.5, 51.477928, -0.001545, 0, False, self.bc.SUN_OFFSET,
             (0.25124811609282555, 0.759618623423145)),
            # 2024-03-20T00:00:00 -- (0.254861 = 6:07 am, 0.761 = 6:16 pm)
            # https://www.timeanddate.com/sun/@112931?month=3&year=2024
            # Transit in Tehran Iran with 35.696111 (lat) and 51.423056 (lon)
            (2460389.5, 35.696111, 51.423056, 3.5, False, self.bc.SUN_OFFSET,
             (0.2553156470420874, 0.7612822588083131)),
            # 2024-03-20T00:00:00 -- (0.254861 = 6:07 am, 0.761 = 6:16 pm)
            # Transit in Tehran Iran with 35.696111 (lat) and 51.423056 (lon)
            (2460389.5, 35.696111, 51.423056, 0, True, self.bc.SUN_OFFSET,
             (0.25232413593097636, 0.7582907476972021)),
            )
        msg = "Expected {}, for jd {}, zone {}, exact {}, found {}."

        for jd, lat, lon, zone, exact, offset, expected_result in data:
            result0 = self.bc._rising_setting(
                jd, lat, lon, zone=zone, exact=exact, offset=offset,
                sr_ss='rise')
            result1 = self.bc._rising_setting(
                jd, lat, lon, zone=zone, exact=exact, offset=offset,
                sr_ss='set')
            result = (result0, result1)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, jd, zone, exact, result))

    #@unittest.skip("Temporarily skipped")
    def test__nutation_longitude(self):
        """
        Test that the _nutation_longitude method returns the
        correct values.
        """
        data = (
            # 1987-04-10T00:00:00 UT -- 0.001052... AA ch.22 p.148 Ex.22.a
            (2446895.5, -0.001052173258783292),
            (2394646.5, 0.004685955021135532),  # 1844-03-21
            (2451544.5, -0.003867549921252321), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self.bc.julian_centuries(jde)
            result = self.bc._nutation_longitude(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__nutation_obliquity(self):
        """
        Test that the _nutation_obliquity method returns the
        correct values.
        """
        data = (
            # 1987-04-10T00:00:00 UT -- 0.00262305... AA ch.22 p.148 Ex.22.a
            (2446895.5, 0.002622907065077194),
            (2394646.5, -0.0003313196399792),   # 1844-03-21
            (2451544.5, -0.001601110054347824), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self.bc.julian_centuries(jde)
            result = self.bc._nutation_obliquity(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__nutation_obliquity_longitude(self):
        """
        Test that the _nutation_obliquity_longitude method returns the
        correct values.
        """
        data = (
            # 1987-04-10T00:00:00 UT -- AA ch.22 p.148 Ex.22.a
            # 0.001052..., 0.00262305...
            (2446895.5, False, (-0.001052173258783292, 0.002622907065077194)),
            # 1844-03-21
            (2394646.5, True, (0.26848544569920246, 359.9810167829594)),
            # 2000-01-01
            (2451544.5, True, (359.7784057124561, 359.9082631513499)),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, degrees, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._nutation_obliquity_longitude(t, degrees=degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__moon_mean_anomaly(self):
        """
        Test that the _moon_mean_anomaly method returns the correct values.
        """
        data = (
            # 1987-04-10T00:00:00
            (2446895.5, 229.27841269605415), # 229.2784 AA p.148 Ex.22.a
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._moon_mean_anomaly(t)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__moon_latitude(self):
        """
        Test that the _moon_latitude method returns the correct values.
        """
        data = (
            # 1987-04-10T00:00:00
            (2446895.5, 143.4079066405975), # 143.4079 AA p.148 Ex.22.a
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._moon_latitude(t)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_earth_mean_anomaly(self):
        """
        Test that the _sun_earth_mean_anomaly method returns the
        correct values.
        """
        data = (
            # 1844-03-21
            (2394646.5, 78.34963598562899),
            # 1987-04-10T00:00:00 -- 94.9792 AA p.148 Ex.22.a
            (2446895.5, 94.9792011648251),
            # 1992-10-13T00:00:00 -- 278.99397 AA p.165 Ex.25.a
            (2448908.5, 278.9925727892901),
            # 2000-01-01
            (2451544.5, 357.03491985845307),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._sun_earth_mean_anomaly(t)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__mean_moon_elongation(self):
        """
        Test that the _mean_moon_elongation method returns the correct values.
        """
        data = (
            # 1987-04-10T00:00:00
            (2446895.5, 136.96231182464544), # 136.9623 AA p.148 Ex.22.a
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._mean_moon_elongation(t)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__moon_ascending_node_longitude(self):
        """
        Test that the _moon_ascending_node_longitude method returns the
        correct values.
        """
        data = (
            # 1844-03-21
            (2394646.5, 258.039325958443),
            # 1987-04-10T00:00:00
            (2446895.5, 11.253083202875985), # 11.2531 AA p.148 Ex.22.a
            # 2000-01-01
            (2451544.5, 125.07099688242339),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._moon_ascending_node_longitude(t)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__true_obliquity_of_ecliptic(self):
        """
        Test that the _true_obliquity_of_ecliptic method returns the
        correct true obliquity of the ecliptic.
        """
        data = (
            # 2446895.5 -- 23.4435694... AA p.148 Ex.22.a
            ((1987, 4, 10), 23.4435691980224),
            ((1992, 10, 13), 23.440144135213007), # 23.44023 AA Ex.25.a
            ((2000, 1, 1.5), 23.437687275568372), # 23.4392911 AA p92
            ((1950, 1, 1.5), 23.44809805233669),  # 23.4457889 AA p92
            )
        msg = "Expected {}, for date {}, found {}."

        for g_date, expected_result in data:
            jde = self.gc.jd_from_gregorian_date(g_date)
            tc = self.bc.julian_centuries(jde)
            result = self.gc._true_obliquity_of_ecliptic(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_mean_longitude(self):
        """
        Test that the _sun_mean_longitude returns the proper angle
        from the sun.
        """
        data = (
            # 1992-10-13T00:00:00 TD -- 201.80720 AA Ex.25.a
            (2448908.5, 201.80719650670744),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self.bc.julian_centuries(jde)
            result = self.bc._sun_mean_longitude(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__eccentricity_earth_orbit(self):
        """
        Test that the _eccentricity_earth_orbit method returns the
        correct values.
        """
        data = (
            # 1992-10-13T00:00:00 -- 0.016711668 AA p.165 Ex.25.a
            (2448908.5, 0.01671166771493543),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._eccentricity_earth_orbit(t)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_apparent_longitude(self):
        """
        Test that the _sun_apparent_longitude returns the proper
        """
        data = (
            # 1992-10-13T00:00:00 TD -- 199.9060605... AA Ex.25.b
            (2448908.5, 199.90893644078398),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self.bc.julian_centuries(jde)
            result = self.bc._sun_apparent_longitude(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_equation_of_center(self):
        """
        Test that the _sun_equation_of_center method returns the
        correct values.
        """
        data = (
            # 1992-10-13T00:00:00 TD -- -1.89732 AA Ex.25.a
            (2448908.5, -1.8973292982987633),
            (2394646.5, 1.8902065487119648),   # 1844-03-21
            (2451544.5, -0.10114766650438385), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            t = self.bc.julian_centuries(jde)
            result = self.bc._sun_equation_of_center(t)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_true_longitude(self):
        """
        Test that the _sun_true_longitude returns the proper
        """
        data = (
            # 1992-10-13T00:00:00 TD -- 199.907347 AA Ex.25.b
            (2448908.5, 199.90986720840868),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self.bc.julian_centuries(jde)
            result = self.bc._sun_true_longitude(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_apparent_right_ascension(self):
        """
        Test that the _sun_apparent_right_ascension method returns the
        correct angle based on the jde.
        """
        data = (
            # 1988-03-19T00:00:00 -- 40.68021 AA Ex.15.a
            #(2447239.5, 358.7231216620047),
            # 1988-03-20T00:00:00 -- 41.73129 AA Ex.15.a
            #(2447240.5, 359.6349480462671),
            # 1988-03-21T00:00:00 -- 42.78204 AA Ex.15.a
            #(2447241.5, 0.5462272546171956),
            # 1992-10-13T00:00:00 TD -- 198.37817916... AA Ex.25.b
            (2448908.5, 198.38083123459006),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self.bc.julian_centuries(jde)
            result = self.bc._sun_apparent_right_ascension(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__sun_apparent_declination(self):
        """
        Test that the _apparent_declination method returns the proper
        apparent declination measured (from 0° to 90°) from the equator,
        positive to the north, negative to the south.
        """
        data = (
            # 1988-03-19T00:00:00 -- 18.04761 AA Ex.15.a
            #(2447239.5, 359.4684977578814),
            # 1988-03-20T00:00:00 -- 18.44092 AA Ex.15.a
            #(2447240.5, 359.8480298854421),
            # 1988-03-21T00:00:00 -- 18.82742 AA Ex.15.a
            #(2447241.5, 0.2273912943419768),
            # 1992-10-13T00:00:00 -- -7.783872 -- AA Ex.25.b
            (2448908.5, -7.78504080260712),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jd, expected_result in data:
            tc = self.bc.julian_centuries(jd)
            result = self.bc._sun_apparent_declination(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test__heliocentric_ecliptical_longitude(self):
        """
        Test that the _heliocentric_ecliptical_longitude method returns
        the correct values.
        """
        data = (
            (2394646.5, True, 180.50142), # 1844-03-21
            # 1992-10-13T00:00:00 -- 19.907372 AA Ex.25.b
            (2448908.5, True, 19.907372),
            (2451544.5, True, 99.868072), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, degrees, expected_result in data:
            tm = self.bc.julian_millennia(jde)
            result = self.bc._heliocentric_ecliptical_longitude(tm, degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__heliocentric_ecliptical_latitude(self):
        """
        Test that the _heliocentric_ecliptical_latitude method returns
        the correct values.
        """
        data = (
            (2394646.5, True, 359.999859), # 1844-03-21
            # 1992-10-13T00:00:00 -- -0.000179 AA Ex.25.b
            (2448908.5, True, 359.999821),
            (2451544.5, True, 359.99981), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, degrees, expected_result in data:
            tm = self.bc.julian_millennia(jde)
            result = self.bc._heliocentric_ecliptical_latitude(tm, degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__radius_vector(self):
        """
        Test that the _radius_vector method returns the correct values.
        """
        data = (
            (2394646.5, True, 57.116216),  # 1844-03-21
            # 1992-10-13T00:00:00 -- 0.99760775 (57.15871368454215) AA Ex.25.b
            (2448908.5, True, 57.158714),
            (2451544.5, True, 56.340762),  # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, degrees, expected_result in data:
            tm = self.bc.julian_millennia(jde)
            result = self.bc._radius_vector(tm, degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test_apparent_solar_longitude(self):
        """
        Test that the apparent_solar_longitude method returns the
        longitude of sun at moment tee.

        Equinox & Solstice Calculator
        https://stellafane.org/misc/equinox.html

        Solar Position Calculator
        https://gml.noaa.gov/grad/solcalc/
        Greenwich lat: 51, 29, 36.24 N (51.4934)
                  lon: 00, 00, 00.00 E (0.0)

        Data from the book pages 225, 446 and 452
        ===============================================================
                                  Solar     Approximate      Season
        Name
                                  longitude date             length
        ---------------------------------------------------------------
        Vernal (spring) equinox     0◦      March 20         92.76 days
        Summer solstice            90◦      June 21          93.65 days
        Autumnal (fall) equinox   180◦      September 22−23  89.84 days
        Winter solstice           270◦      December 21−22   88.99 days
        """
        data = (
            # (2024, 3, 20) Vernal equinox 2024-03-20T03:06:04 UTC
            (2460389.759722, 359.740947), # 0
            # (2024, 6, 20) Summer solstice 2024-06-20T20:50:23 UTC
            (2460483.238888, 90.449786),  # 90
            # (2024, 9, 22) Autumnal equinox 2024-09-22T12:43:12 UTC
            (2460576.561111, 180.161496), # 180
            # (2024, 12, 21) Winter solstice 2024-12-21T09:19:54 UTC
            (2460666.279166, 270.074089), # 270
            # (1800, 3, 20) Vernal equinox 1800-03-20T20:11:48 UTC
            (2378575.181945, 359.401715), # 0
            # (1800, 6, 21) Summer solstice 1800-06-21T17:51:29 UTC
            (2378667.9875, 89.320445),    # 90
            # (1800, 9, 23) Autumnal equinox 1800-09-23T07:25:31 UTC
            (2378761.118055, 178.907454), # 180
            # (1800, 12, 22) Winter solstice 1800-12-22T00:16:24
            (2378850.522222, 268.615081), # 270
            # (2073, 3, 20) Vernal equinox 2073-03-20T00:12:24 UTC
            (2478286.520833, 359.553774), # 0
            # (2073, 6, 20) Summer solstice 2073-06-20T17:06:18 UTC
            (2478379.927777, 90.197347),  # 90
            # (2073, 9, 22) Autumnal equinox 2073-09-22T09:14:10 UTC
            (2478473.273611, 179.871075), # 180
            # (2073, 12, 21) Winter solstice 2073-12-21T06:49:41 UTC
            (2478563.072222, 269.779984), # 270
            # All the dates below are from the CC Appendix C Sample Data p452
            # (-586, 7, 24)
            (1507231.5, 118.207136),      # 118.98911336371384
            # (576, 5, 20)
            (1931579.5, 58.475114),       # 59.119741
            # (1288, 4, 2)
            (2191584.5, 12.816201),        # 13.498220
            # (1553, 9, 19)
            (2288542.5, 175.003305),      # 176.059431
            # (1768, 6, 19)
            (2366978.5, 88.027608),       # 88.567428
            # (1941, 9, 29)
            (2430266.5, 185.09022),      # 185.945867
            # (2038, 11, 10)
            (2465737.5, 227.070777),      # 228.184879
            # (2094, 7, 18)
            (2486076.5, 115.381223),      # 116.439352
            # 1992-10-13T00:00:00 DT -- 199.9060605... AA Ex.25.b
            (2448908.5, 199.835144),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            result = self.bc.apparent_solar_longitude(jde)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test_apparent_solar_latitude(self):
        """
        Test that the apparent_solar_latitudemethod returns the correct values.
        """
        data = (
            # 1992-10-13T00:00:00 DT -- 0.000172 AA Ex.25.b
            (2448908.5, 0.000167),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            result = self.bc.apparent_solar_latitude(jde)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__aberration(self):
        """
        Test that the _aberration method returns the correct aberration
        of the given date in degrees.
        """
        data = (
            # (2024, 3, 20) Vernal equinox 2024-03-20T03:06:04 UTC
            (389.759722222, True, -0.005708),
            (389.759722222, False, -0.005708),
            )
        msg = "Expected {}, for jd {}, found {}."

        for jd, fixed, expected_result in data:
            tm = self.bc.julian_millennia(jd)
            result = self.bc._aberration(tm, fixed=fixed)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test_approx_julian_day_for_equinoxes_or_solstices(self):
        """
        Test that the approx_julian_day_for_equinoxes_or_solstices method
        returns a Julian day of the equinoxes or solstices.
        """
        data = (
            (500, self.bc.SPRING, 1903760.376019375),
            (500, self.bc.SUMMER, 1903854.104661875),
            (500, self.bc.AUTUMN, 1903946.9228224999),
            (500, self.bc.WINTER, 1904035.8380625),
            (2000, self.bc.SPRING, 2451623.80984),
            (2000, self.bc.SUMMER, 2451716.56767),
            (2000, self.bc.AUTUMN, 2451810.21715),
            (2000, self.bc.WINTER, 2451900.05952),
            )
        msg = "Expected {}, for year {} at angle {}, found {}."

        for year, season, expected_result in data:
            result = self.bc.approx_julian_day_for_equinoxes_or_solstices(
                year, lam=season)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, season, result))

    #@unittest.skip("Temporarily skipped")
    def test_find_moment_of_equinoxes_or_solstices(self):
        """
        Test that the find_moment_of_equinoxes_or_solstices method returns
        the correct equinoxe and solstice R.D. moments for the given years.
        """
        SP = self.bc.SPRING
        SM = self.bc.SUMMER
        AU = self.bc.AUTUMN
        WN = self.bc.WINTER
        seasons = {SP: 'SPRING', SM: 'SUMMER', AU: 'AUTUMN', WN: 'WINTER'}
        data = (
            # Vernal Equinoxe
            ((900, 3, 1), SP, (900, 3, 15, 18, 19, 15.6864)),
            ((1788, 3, 19, 22, 16), SP, (1788, 3, 19, 22, 16, 44.9472)),
            ((1844, 3, 20, 11, 53), SP, (1844, 3, 20, 11, 53, 50.208)),
            ((1951, 3, 21, 10, 26), SP, (1951, 3, 21, 10, 26, 44.4768)),
            # AA p.180 Ex 27.a
            ((1962, 6, 1), SM, (1962, 6, 21, 21, 25, 6.0384)),
            # AA p.182 Table 27.e
            ((2000, 3, 20, 7, 35), SP, (2000, 3, 20, 7, 36, 37.872)),
            # Should be 2018-03-20T16:16:36 DT
            ((2018, 3, 20), SP, (2018, 3, 20, 16, 16, 23.952)),
            # Should be 2022-03-20T15:34:33 DT
            ((2022, 3, 20), SP, (2022, 3, 20, 15, 34, 40.08)),
            ((2024, 3, 20, 3, 6), SP, (2024, 3, 20, 3, 7, 49.152)),
            # Should be 2026-03-20T14:47:05 DT
            ((2026, 3, 20), SP, (2026, 3, 20, 14, 46, 50.8224)),
            ((2038, 3, 20, 12, 40), SP, (2038, 3, 20, 12, 42, 2.2752)),
            # Should be 2043-03-20T17:28:58 DT
            ((2043, 3, 20), SP, (2043, 3, 20, 17, 29, 7.4112)),
            # Should be 2047-03-20T16:53:53 DT
            ((2047, 3, 20), SP, (2047, 3, 20, 16, 54, 14.0256)),
            # Should be 2051-03-20T16:00:28 DT
            ((2051, 3, 20), SP, (2051, 3, 20, 16, 0, 23.2704)),
            # Should be 2055-03-20T15:30:03 DT
            ((2055, 3, 20), SM, (2055, 6, 21, 8, 40, 54.5952)),
            ((2064, 3, 19, 19, 37), SP, (2064, 3, 19, 19, 40, 13.6992)),
            ((2100, 3, 20, 13, 3), SP, (2100, 3, 20, 13, 6, 30.2112)),
            ((2150, 3, 20, 16, 1), SP, (2150, 3, 20, 16, 6, 35.9136)),
            ((2200, 3, 20, 18, 40), SP, (2200, 3, 20, 18, 48, 6.624)),
            ((2211, 3, 21, 10, 38), SP, (2211, 3, 21, 10, 45, 29.9232)),
            # Summer Solstice
            ((900, 6, 1), SM, (900, 6, 17, 6, 32, 44.2176)),
            ((2000, 6, 1), SM, (2000, 6, 21, 1, 48, 45.36)),
            # Autumn Equinox
            ((900, 9, 1), AU, (900, 9, 18, 8, 35, 1.4784)),
            ((2000, 9, 1), AU, (2000, 9, 22, 17, 28, 41.664)),
            # Winter Solstice
            ((900, 12, 1), WN, (900, 12, 16, 11, 38, 22.272)),
            ((2000, 12, 1), WN, (2000, 12, 21, 13, 38, 47.1264)),
            )
        msg = "Expected '{}' during the {}, found '{}'"

        for date, season, expected_result in data:
            jde = self.gc.jd_from_gregorian_date(date)
            result = self.bc.find_moment_of_equinoxes_or_solstices(
                jde, lam=season)
            result = self.gc.gregorian_date_from_jd(result)
            result = self.gc.ymdhms_from_date(result)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, seasons[season], result))

    #@unittest.skip("Temporarily skipped")
    def test_decimal_from_dms(self):
        """
        Test that the method decimal_from_dms correctly converts degrees,
        minutes, and seconds into a decimal number.

        https://warble.com/blog/2017/11/05/virtually-hovering-over-holy-places/
        The Shrine of Baha’u’llah: 32°56’36.86″N, 35°5’30.38″E
        The Shrine of The Bab: 32°48’52.49″N, 34°59’13.91″E
        The Guardian’s Resting Place: 51°37’21.85″N, 0°08’35.57″W
        """
        data = (
            # Statue of Liberty latitude (US)
            ((40, 41, 21.29, 'N'), 40.68924722222222),
            # Statue of Liberty longitude (US)
            ((74, 2, 40.29, 'W'), -74.044525),
            # The Shrine of Baha’u’llah latitude (IL)
            ((32, 56, 36.86, 'N'), 32.943572222222215),
            # The Shrine of Baha’u’llah longitude (IL)
            ((35, 5, 30.37, 'E'), 35.091769444444445),
            # Sydney Opera House latitude (AU)
            ((-33, 51, 24.37, 'S'), -33.856769444444446),
            # Sydney Opera House longitude (AU)
            ((151, 12, 54.43, 'E'), 151.21511944444444),
            )
        msg = "Expected {} with '{}', found {}."

        for args, expected_result in data:
            result = self.bc.decimal_from_dms(*args)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))

    #@unittest.skip("Temporarily skipped")
    def test_dms_from_decimal(self):
        """
        Test that the dms_from_degrees method correctly converts a decimal
        representation of a latitude or longitude into degrees, minutes,
        and seconds.
        """
        data = (
            # Statue of Liberty latitude (US)
            ((40.68924722222222, 'lat'), (40, 41, 21.28999999999559, 'N')),
            # Statue of Liberty longitude (US)
            ((-74.044525, 'lon'), (74, 2, 40.28999999997495, 'W')),
            # The Shrine of Baha’u’llah latitude (IL)
            ((32.943572222222215, 'lat'), (32, 56, 36.85999999997529, 'N')),
            # The Shrine of Baha’u’llah longitude (IL)
            ((35.091769444444445, 'lon'), (35, 5, 30.37000000000207, 'E')),
            # Sydney Opera House latitude (AU)
            ((-33.856769444444446, 'lat'), (33, 51, 24.370000000004175, 'S')),
            # Sydney Opera House longitude (AU)
            ((151.21511944444444, 'lon'), (151, 12, 54.42999999998388, 'E')),
            )
        msg = "Expected {} with '{}', found {}."

        for args, expected_result in data:
            result = self.bc.dms_from_decimal(*args)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))

    #@unittest.skip("Temporarily skipped")
    def test_degrees_from_hms(self):
        """
        Test that the degrees_from_hms method converts hours, minutes,
        and seconds into degrees properly.
        """
        data = (
            ((1, 0, 0), 15.0),
            ((2, 0, 0), 30.0),
            ((24, 0, 0), 360.0),
            )
        msg = "Expected {} with h,m,s {}, found {}."

        for args, expected_result in data:
            result = self.bc.degrees_from_hms(*args)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))

    #@unittest.skip("Temporarily skipped")
    def test_hms_from_degrees(self):
        """
        Test that the hms_from_degrees method converts degrees to hours,
        minutes, and seconds properly.
        """
        data = (
            (15., (1, 0, 0)),
            (30., (2, 0, 0)),
            (360, (24, 0, 0)),
            )
        msg = "Expected {} with degrees {}, found {}."

        for degrees, expected_result in data:
            result = self.bc.hms_from_degrees(degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, degrees, result))

    #@unittest.skip("Temporarily skipped")
    def test_seconds_from_dhms(self):
        """
        Test that the seconds_from_dhms method converts hours minutes,
        and seconds to seconds.
        """
        data = (
            ((0, 10, 2, 5), 36125),
            ((1, 0, 0, 0), 86400),
            )
        msg = "Expected {} with h,m,s {}, found {}."

        for args, expected_result in data:
            result = self.bc.seconds_from_dhms(*args)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))

    #@unittest.skip("Temporarily skipped")
    def test_dhms_from_seconds(self):
        """
        Test that the dhms_from_seconds method converts seconds into
        hours, minutes, and seconds properly.
        """
        data = (
            (36125, (0, 10, 2, 5)),
            (86400, (1, 0, 0, 0)),
            )
        msg = "Expected {} with seconds {}, found {}."

        for seconds, expected_result in data:
            result = self.bc.dhms_from_seconds(seconds)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, seconds, result))

    #@unittest.skip("Temporarily skipped")
    def test_tz_decimal_from_dhms(self):
        """
        Test that the tz_decimal_from_dhms method converts hours, minutes,
        and seconds of a time zone to a decimal number representing degrees.
        """
        data = (
            ((0, 10, 2, 5), 0.41811342592592593),
            ((1, 0, 0, 0), 1.0),
            )
        msg = "Expected {} with h,m,s {}, found {}."

        for args, expected_result in data:
            result = self.bc.tz_decimal_from_dhms(*args)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))

    #@unittest.skip("Temporarily skipped")
    def test_tz_dhms_from_decimal(self):
        """
        Test that the tz_dhms_from_decimal method returns converts a
        decimal number to hours, minutes, and seconds.
        """
        data = (
            (0.41811342592592593, (0, 10, 2, 5)),
            (1.0, (1, 0, 0, 0)),
            )
        msg = "Expected {} with decimal {}, found {}."

        for dec, expected_result in data:
            result = self.bc.tz_dhms_from_decimal(dec)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, dec, result))

    #@unittest.skip("Temporarily skipped")
    def test_hms_from_decimal_day(self):
        """
        Test that the hms_from_decimal_day method returns hours, minutes,
        and seconds deom a decimal number.
        """
        data = (
            (0.5, (12, 0, 0.0)),
            (0.25, (6, 0, 0.0)),
            (0.1, (2, 24, 0.0)),
            (.1774306, (4, 15, 30.0384))
            )
        msg = "Expected {} with decimal {}, found {}."

        for dec, expected_result in data:
            result = self.bc.hms_from_decimal_day(dec)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, dec, result))

    #@unittest.skip("Temporarily skipped")
    def test_decimal_day_from_hms(self):
        """
        Test that the decimal_day_from_hms method returns the correct
        decimal part of the day.
        """
        data = (
            ((12, 0, 0.0), 0.5),
            ((6, 0, 0.0), 0.25),
            ((2, 24, 0.0), 0.1),
            ((4, 15, 30), 0.17743055555555556),
            )
        msg = "Expected {} with hms {}, found {}."

        for hms, expected_result in data:
            result = self.bc.decimal_day_from_hms(*hms)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, hms, result))

    #@unittest.skip("Temporarily skipped")
    def test__sec_microsec_from_seconds(self):
        """
        Test that the _sec_microsec_from_seconds method returns the seconds
        and microseconds from a second + partian as in 10.75 seconds.
        """
        data = (
            (-18000, (-18000, 0)),
            (-18000.5, (-18000, 500000)),
            (100.125, (100, 125000)),
            )
        msg = "Expected {} with second {}, found {}."

        for second, expected_result in data:
            result = self.bc._sec_microsec_from_seconds(second)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, second, result))

    #@unittest.skip("Temporarily skipped")
    def test__sin_deg(self):
        """
        Test that the _sin_deg method returns the sine of theta
        (given in degrees).
        """
        theta = 90.0
        result = self.bc._sin_deg(theta)
        expected_result = 1.0
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test__cos_deg(self):
        """
        Test that the _cos_deg method returns the cosine of theta
        (given in degrees).
        """
        theta = 90.0
        result = self.bc._cos_deg(theta)
        expected_result = 6.123233995736766e-17 # is 0?
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test__sigma(self):
        """
        Test that the sigma method returns a sum of the provided lists
        wuth the given function (condition).
        """
        lists = ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                 (1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
                 (2, 3, 4, 5, 6, 7, 8, 9, 0, 1))
        func = lambda x, y, z, : x * y * z
        result = self.bc._sigma(lists, func)
        expected_result = 1260
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test__poly(self):
        """
        Test the poly (polynomial) lambda.
        """
        # Test not 'x'
        x = 10
        a = ()
        poly = self.bc._poly(x, a)
        expected_poly = 0
        msg = f"POLY should be {expected_poly}, found {poly}."
        self.assertEqual(expected_poly, poly, msg)
        # Test 'a' has a value
        x = 0.1
        a = (1, 2, 3, 4)
        poly = self.bc._poly(x, a)
        expected_poly = 1.234
        msg = f"POLY should be {expected_poly}, found {poly}."
        self.assertEqual(expected_poly, poly, msg)

    #@unittest.skip("Temporarily skipped")
    def test__days_in_years(self):
        """
        Test that the _days_in_years method returns the number of days
        including year one (Gregorian) to the given year.
        """
        data = (
            (1, False, 365),
            (1, True, 365),
            (2024, False, 739251),
            (2024, True, 739251),
            (3000, False, 1095727),
            (3000, True, 1095727),
            )
        msg = "Expected {} with year {} and alt {}, found {}."

        for year, alt, expected_result in data:
            result = self.bc._days_in_years(year, alt=alt)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, alt, result))

    #@unittest.skip("Temporarily skipped")
    def test__coterminal_angle(self):
        """
        Test that the _coterminal_angle method converts degrees greater
        or less that 360 degrees to a number between 0 and 360 degrees.
        """
        data = (
            (1000, 280.0),
            (-361, 359.0),
            )
        msg = "Expected {} with degrees {}, found {}."

        for degrees, expected_result in data:
            result = self.bc._coterminal_angle(degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, degrees, result))

    #@unittest.skip("Temporarily skipped")
    def test__interpolation_from_three(self):
        """
        Test that the _interpolation_from_three method interpolates from
        three values an estimate of a forth value.

        See AA Example 3.a and 3.b
        """
        data = (
            ((0.884226, 0.884226, 0.870531, 0.18125), False,
             0.8827599395507812),
            ((0.884226, 0.884226, 0.870531, 0.18125), True,
             39.42104118955078),
            ((1.3814294, 1.3812213, 1.3812453, 0.39660), False,
             1.381203046655538),
            ((1.3814294, 1.3812213, 1.3812453, 0.39660), True,
             44.45672224665554),
            )
        msg = "Expected {} with {}, found {}."

        for args, normalize, expected_result in data:
            result = self.bc._interpolation_from_three(
                *args, normalize=normalize)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))

    #@unittest.skip("Temporarily skipped")
    def test__truncate_decimal(self):
        """
        Test that the _truncate_decimal method correctly truncates a
        decimal value.
        """
        data = (
            ((10.1212123456789, 6), 10.121212),
            ((0.01234, 3), 0.012),
            ((1.0123456789, 12), 1.0123456789 ),
            ((1.123, 20), 1.123),
            )
        msg = "Expected {} with {}, found {}."

        for args, expected_result in data:
            result = self.bc._truncate_decimal(*args)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))
