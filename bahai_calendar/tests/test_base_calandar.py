# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_base_calandar.py
#
__docformat__ = "restructuredtext en"

import os
import math
import unittest
import datetime

from ..base_calendar import BaseCalendar
from ..gregorian_calendar import GregorianCalendar


class TestBaseCalandar(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        class FakeParent(BaseCalendar):
            # Location of Tehran for astronomical Baha’i calendar.
            # Can be change for individual tests using the location
            # method below.
            latitude = 35.696111
            longitude = 51.423056
            elevation = 0
            zone = 3.5

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

        self._bc = FakeParent()
        self._gc = GregorianCalendar()

    #@unittest.skip("Temporarily skipped")
    def test_HR(self):
        """
        Test the HR (hour) lambda.
        """
        value = 6
        hr = self._bc.HR(value)
        expected_hr = 0.25
        msg = f"HR should be {expected_hr}, found {hr}."
        self.assertEqual(expected_hr, hr, msg)

    #@unittest.skip("Temporarily skipped")
    def test_MN(self):
        """
        Test the MN (minute) lambda.
        """
        value = 6
        mn = self._bc.MN(value)
        expected_mn = 0.00416666666666666667
        msg = f"MN should be {expected_mn}, found {mn}."
        self.assertEqual(expected_mn, mn, msg)

    #@unittest.skip("Temporarily skipped")
    def test_SEC(self):
        """
        Test the sec (second) lambda.
        """
        value = 6
        sec = self._bc.SEC(value)
        expected_sec = 6.94444444444444444444e-5
        msg = f"SEC should be {expected_sec}, found {sec}."
        self.assertEqual(expected_sec, sec, msg)

    #@unittest.skip("Temporarily skipped")
    def test_MINS(self):
        """
        Test the MINS (minutes) lambda.
        """
        value = 6
        mins = self._bc.MINS(value)
        expected_mins = 0.1
        msg = f"MINS should be {expected_mins}, found {mins}."
        self.assertEqual(expected_mins, mins, msg)

    #@unittest.skip("Temporarily skipped")
    def test_SECS(self):
        """
        Test the SECS (seconds) lambda.
        """
        value = 6
        secs = self._bc.SECS(value)
        expected_secs = 0.00166666666666666667
        msg = f"SECS should be {expected_secs}, found {secs}."
        self.assertEqual(expected_secs, secs, msg)

    #@unittest.skip("Temporarily skipped")
    def test_ANGLE(self):
        """
        Test the ANGLE (angle) lambda.
        """
        d, m, s = (23, 26, 21.448)
        angle = self._bc.ANGLE(d, m ,s)
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
            result = self._bc.AMOD(x, y)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, x, y, result))

    #@unittest.skip("Temporarily skipped")
    def test_MOD3(self):
        """
        Test the MOD3 (modular three) lambda.
        """
        # Test a == b
        x, a, b = (25, 25, 100)
        mod3 = self._bc.MOD3(x, a ,b)
        expected_mod3 = x if a == b else a + (x - a) % (b - a)
        msg = f"MOD3 should be {expected_mod3}, found {mod3}."
        self.assertEqual(expected_mod3, mod3, msg)
        # Test a != b
        x, a, b = (25, 10, 100)
        mod3 = self._bc.MOD3(x, a ,b)
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
        quotient = self._bc.QUOTIENT(m, n)
        expected_quotient = int(math.floor(m / n))
        msg = f"QUOTIENT should be {expected_quotient}, found {quotient}."
        self.assertEqual(expected_quotient, quotient, msg)

    #@unittest.skip("Temporarily skipped")
    def test_parse_datetime(self):
        """
        Test that the parse_datetime method creates an instance object.
        """
        now = datetime.datetime.now()
        self._bc.parse_datetime(now)
        expected_dt = (now.hour, now.minute, now.second)
        msg = f"Expected {expected_dt}, found {self._bc._time}"
        self.assertTrue(
            all([i == j for i, j in zip(expected_dt,
                                        self._bc.time_representation)]), msg)

    #@unittest.skip("Temporarily skipped")
    def test_delta_t(self):
        """
        Test that the delta_t method returns the correct delta T value
        for converting between UT and DT time.

        To check for valid values see: https://planetcalc.com/9200/
        """
        data = (
            ((-600, 1, 1), 18719.834672222227), # OK
            ((0, 1, 1), 10583.17733503136),     # OK
            ((900, 1, 1), 2199.998764705048),   # OK
            ((900, 3, 1), 2198.842046841268),   # OK
            ((905, 1, 1), 2165.4555994434895),  # OK
            ((1650, 1, 1), 50.13316097312479),  # OK
            ((1750, 1, 1), 13.375979008768493), # OK
            ((1830, 1, 1), 7.6550062338922995), # OK
            ((1885, 1, 1), -5.65258999265227),  # OK
            ((1910, 1, 1), 10.445380968083992), # OK
            ((1935, 1, 1), 23.81634451255787),  # OK
            ((1951, 1, 1), 29.48974515233175),  # OK
            # 2443192.6511574076 JD DT -- 48 AA Ex.10.a
            ((1977, 2, 1), 47.686642722506434), # OK
            # 2447240.5 JD DT -- +56 AA Ex.15.a
            ((1988, 3, 20), 55.8792447106014),
            ((2000, 1, 1), 63.873832810959236), # OK
            ((2020, 1, 1), 71.62174845312504),  # OK
            ((2100, 1, 1), 202.8381222222219),  # -93 WRONG
            ((2200, 1, 1), 442.18133888888855), # 103 WRONG
            )
        msg = "Expected {}, found {}."

        for g_date, expected_result in data:
            jde = self._gc.jd_from_gregorian_date(g_date)
            result = self._bc.delta_t(jde)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

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
            jde = self._gc.jd_from_gregorian_date(g_date)
            tc = self._bc.julian_centuries(jde)
            result = self._gc._mean_sidereal_time_greenwich(tc)
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
            jde = self._gc.jd_from_gregorian_date(g_date)
            tc = self._bc.julian_centuries(jde)
            result = self._gc._apparent_sidereal_time_greenwich(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    ## @unittest.skip("Temporarily skipped")
    ## def test_ecliptic_longitude(self):
    ##     """
    ##     Test that the ecliptic_longitude method returns
    ##     the correct ecliptic latitude.
    ##     """
    ##     data = (
    ##         (self._bc.J2000, 214.15716590411967),
    ##         )
    ##     msg = "Expected {}, for jde {}, found {}."

    ##     for jde, expected_result in data:
    ##         result = self._bc.ecliptic_longitude(jde)
    ##         self.assertEqual(expected_result, result,
    ##                          msg.format(expected_result, jde, result))

    ## @unittest.skip("Temporarily skipped")
    ## def test_ecliptic_latitude(self):
    ##     """
    ##     Test that the ecliptic_latitude method returns
    ##     the correct ecliptic latitude.
    ##     """
    ##     data = (
    ##         (),
    ##         )
    ##     msg = "Expected {}, for jde {}, found {}."

    ##     for jde, expected_result in data:
    ##         result = self._bc.ecliptic_latitude(jde)
    ##         self.assertEqual(expected_result, result,
    ##                          msg.format(expected_result, jde, result))

    ## @unittest.skip("Temporarily skipped")
    ## def test_equatorial_right_ascension(self):
    ##     """
    ##     Test that the equatorial_right_ascension method returns
    ##     the correct equatorial right ascension.
    ##     """
    ##     data = (
    ##         (),
    ##         )
    ##     msg = "Expected {}, for jde {}, found {}."

    ##     for jde, expected_result in data:
    ##         result = self._bc.equatorial_right_ascension(jde)
    ##         self.assertEqual(expected_result, result,
    ##                          msg.format(expected_result, jde, result))

    ## @unittest.skip("Temporarily skipped")
    ## def test_equatorial_declination(self):
    ##     """
    ##     Test that the equatorial_declination method returns
    ##     the correct equatorial declination.
    ##     """
    ##     data = (
    ##         (),
    ##         )
    ##     msg = "Expected {}, for jde {}, found {}."

    ##     for jde, expected_result in data:
    ##         result = self._bc.equatorial_declination(jde)
    ##         self.assertEqual(expected_result, result,
    ##                          msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test_altitude(self):
        """
        """
        func = lambda m: m + 1 if m <= 0 else m - 1 if m >= 1 else m

        def inter_ra(tc, n):
            a0 = self._bc._sun_apparent_right_ascension(tc - (1 / 36525))
            a1 = self._bc._sun_apparent_right_ascension(tc)
            a2 = self._bc._sun_apparent_right_ascension(tc + (1 / 36525))
            return self._bc.interpolation_from_three(a0, a1, a2, n)

        def inter_de(tc, n):
            d0 = self._bc._sun_apparent_declination(tc - (1 / 36525))
            d1 = self._bc._sun_apparent_declination(tc)
            d2 = self._bc._sun_apparent_declination(tc + (1 / 36525))
            return self._bc.interpolation_from_three(d0, d1, d2, n)

        data = (
            # 1987-04-10T19:21:00 -- 2446896.30625, lat, lon
            # AA Ex.13.b alpha=347.3193, delta=  +15.1249
            ((1987, 4, 10, 19, 21), 38.921388888888885, -77.06555555555555, 0),
            )
        msg = "Expected {}, for date {}, with lat {} and lon {}, found {}."

        for g_date, lat, lon, expected_result in data:
            jd = self._gc.jd_from_gregorian_date(g_date)
            tc = self._bc.julian_centuries(jd)
            app_srt = self._bc._apparent_sidereal_time_greenwich(tc)
            dt = self._gc.delta_t(jd)
            jde = jd + dt
            # Find alpha
            ra = self._bc._sun_apparent_right_ascension(tc)
            ma = func((ra - lon - app_srt) / 360)
            alpha = inter_ra(tc, ma * dt / 86400)
            # Find delta
            de = self._bc._sun_apparent_declination(tc)
            md = func((de - lon - app_srt) / 360)
            delta = inter_de(tc, ma * dt / 86400)
            # Find the local hour angle
            h = self._bc.local_hour_angle(app_srt, lon, alpha)
            print(alpha, delta, app_srt, h)
            result = self._bc.altitude(alpha, delta, h)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, g_date, lat, lon, result))

    #@unittest.skip("Temporarily skipped")
    def test__approx_local_hour_angle(self):
        """
        Test that the _approx_local_hour_angle method returns the
        correct angle based on the jde.

        The variable 'self._bc.latitude' is the latitude in Tehran Iran.
        """
        offset = self._bc.SUN_OFFSET
        data = (
            ((1844, 3, 20), self._bc.latitude, offset, 90.8842501111108),
            ((1988, 3, 20), 42.3333, offset, 90.98885152188359), # AA Ex15.a
            ((2024, 6, 20), self._bc.latitude, offset, 108.44454881189583),
            )
        msg = "Expected {}, for date {}, found {}."

        for g_date, lat, offset, expected_result in data:
            jde = self._gc.jd_from_gregorian_date(g_date)
            tc = self._bc.julian_centuries(jde)
            result = self._bc._approx_local_hour_angle(tc, lat, offset)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    @unittest.skip("Temporarily skipped")
    def test_sunrise(self):
        """
        """
        data = (
            # 2447240.5 -- 12h25m 186.25 AA Ex.15.a
            ((1988, 3, 20), 42.3333, 71.0833, 0),
            )
        msg = "Expected {}, for g_date {}, found {}."

        for g_date, lat, lon, expected_result in data:
            jde = self._gc.jd_from_gregorian_date(g_date)
            result = self._bc.rising(jde, lat, lon)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    @unittest.skip("Temporarily skipped")
    def test_transit(self):
        """
        Test that the test_transit method returns the correct transition
        based on the jde.
        """
        data = (
            # 2447240.5 -- 19h41m 295.26 AA Ex.15.a
            ((1988, 3, 20), 42.3333, 71.0833, 0),
            )
        msg = "Expected {}, for g_date {}, found {}."

        for g_date, lat, lon, expected_result in data:
            jde = self._gc.jd_from_gregorian_date(g_date)
            result = self._bc.transit(jde, lat, lon)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    @unittest.skip("Temporarily skipped")
    def test_sunset(self):
        """
        """
        data = (
            # 2447240.5 -- 2h55m 43.75 AA Ex.15.a
            ((1988, 3, 20), 42.3333, 71.0833, 0),
            )
        msg = "Expected {}, for g_date {}, found {}."

        for g_date, lat, lon, expected_result in data:
            jde = self._gc.jd_from_gregorian_date(g_date)
            result = self._bc.setting(jde, lat, lon)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, g_date, result))

    #@unittest.skip("Temporarily skipped")
    def test__transit_rising_setting(self):
        """
        """
        data = (
            # 1988-03-20T00:00:00 -- AA Ex.15.a 186.25, 295.25, 43.75
            # deltaT=55.8792447106014, T=-0.1178507871321013,
            # Theta0=177.74206503597088, TD=-0.11785080483916544,
            # alpha=359.63435850814983, m=0.5054770344517455,
            # DeltaM=-0.00010244990650423662
            # JDE       Latitude Longitude
            (2447240.5, 42.3333, -71.0833,
             (7.101063701185129, 0.7005540373620093, 13.899552781347815)),
            )
        msg = "Expected {}, for jd {}, found {}."

        for jd, lat, lon, expected_result in data:
            result = self._bc._transit_rising_setting(jd, lat, lon)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

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
            tc = self._bc.julian_centuries(jde)
            result = self._bc._nutation_longitude(tc)
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
            tc = self._bc.julian_centuries(jde)
            result = self._bc._nutation_obliquity(tc)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._nutation_obliquity_longitude(t, degrees=degrees)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._moon_mean_anomaly(t)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._moon_latitude(t)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._sun_earth_mean_anomaly(t)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._mean_moon_elongation(t)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._moon_ascending_node_longitude(t)
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
            jde = self._gc.jd_from_gregorian_date(g_date)
            tc = self._bc.julian_centuries(jde)
            result = self._gc._true_obliquity_of_ecliptic(tc)
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
            tc = self._bc.julian_centuries(jde)
            result = self._bc._sun_mean_longitude(tc)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._eccentricity_earth_orbit(t)
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
            tc = self._bc.julian_centuries(jde)
            result = self._bc._sun_apparent_longitude(tc)
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
            t = self._bc.julian_centuries(jde)
            result = self._bc._sun_equation_of_center(t)
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
            tc = self._bc.julian_centuries(jde)
            result = self._bc._sun_true_longitude(tc)
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
            #(2447239.5, 358.713720047607),
            # 1988-03-20T00:00:00 -- 41.73129 AA Ex.15.a
            #(2447240.5, 359.63225968399684),
            # 1988-03-21T00:00:00 -- 42.78204 AA Ex.15.a
            #(2447241.5, 0.5502491977150128),
            # 1992-10-13T00:00:00 TD -- 198.37817916... AA Ex.25.b
            (2448908.5, 198.36836226034777),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self._bc.julian_centuries(jde)
            result = self._bc._sun_apparent_right_ascension(tc)
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
            (2448908.5, -7.8152755943930545),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tc = self._bc.julian_centuries(jde)
            result = self._bc._sun_apparent_declination(tc)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__heliocentric_ecliptical_longitude(self):
        """
        Test that the _heliocentric_ecliptical_longitude method returns
        the correct values.
        """
        data = (
            (2394646.5, True, 180.5014202623206), # 1844-03-21
            # 1992-10-13T00:00:00 -- 19.907372 AA Ex.25.b
            (2448908.5, True, 19.907371990723732),
            (2451544.5, True, 99.8680724573845), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, degrees, expected_result in data:
            tm = self._bc.julian_millennia(jde)
            result = self._bc._heliocentric_ecliptical_longitude(tm, degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__heliocentric_ecliptical_latitude(self):
        """
        Test that the _heliocentric_ecliptical_latitude method returns
        the correct values.
        """
        data = (
            (2394646.5, True, 359.9998594594096), # 1844-03-21
            # 1992-10-13T00:00:00 -- -0.000179 AA Ex.25.b
            (2448908.5, True, 359.9998209874959),
            (2451544.5, True, 359.9998100279164), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, degrees, expected_result in data:
            tm = self._bc.julian_millennia(jde)
            result = self._bc._heliocentric_ecliptical_latitude(tm, degrees)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__radius_vector(self):
        """
        Test that the _radius_vector method returns the correct values.
        """
        data = (
            (2394646.5, True, 57.116215635928334),  # 1844-03-21
            # 1992-10-13T00:00:00 -- 0.99760775 (57.15871368454215) AA Ex.25.b
            (2448908.5, True, 57.158713656750486),
            (2451544.5, True, 56.34076223912779),  # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, degrees, expected_result in data:
            tm = self._bc.julian_millennia(jde)
            result = self._bc._radius_vector(tm, degrees)
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
            (2460389.759722222, 359.7409390107532),   # 0
            # (2024, 6, 20) Summer solstice 2024-06-20T20:50:23 UTC
            (2460483.2388888886, 90.44982248640918),  # 90
            # (2024, 9, 22) Autumnal equinox 2024-09-22T12:43:12 UTC
            (2460576.561111111, 180.1614972548632),  # 180
            # (2024, 12, 21) Winter solstice 2024-12-21T09:19:54 UTC
            (2460666.279166667, 270.0741201525616),  # 270
            # (1800, 3, 20) Vernal equinox 1800-03-20T20:11:48 UTC
            (2378575.181944445, 359.4017532804719),   # 0
            # (1800, 6, 21) Summer solstice 1800-06-21T17:51:29 UTC
            (2378667.9875, 89.3204124514159),        # 90
            # (1800, 9, 23) Autumnal equinox 1800-09-23T07:25:31 UTC
            (2378761.118055556, 178.90744905435713),  # 180
            # (1800, 12, 22) Winter solstice 1800-12-22T00:16:24
            (2378850.5222222223, 268.61508652403427),  # 270
            # (2073, 3, 20) Vernal equinox 2073-03-20T00:12:24 UTC
            (2478286.520833333, 359.553824087463),  # 0
            # (2073, 6, 20) Summer solstice 2073-06-20T17:06:18 UTC
            (2478379.927777778, 90.19736693700906),   # 90
            # (2073, 9, 22) Autumnal equinox 2073-09-22T09:14:10 UTC
            (2478473.2736111116, 179.8710632478178), # 180
            # (2073, 12, 21) Winter solstice 2073-12-21T06:49:41 UTC
            (2478563.072222222, 269.7800085199342),  # 270
            # All the dates below are from the CC Appendix C Sample Data p452
            # (-586, 7, 24)
            (1507231.5, 118.20715430541895), # 118.98911336371384
            # (576, 5, 20)
            (1931579.5, 58.475085406855214),  # 59.119741
            # (1288, 4, 2)
            (2191584.5, 12.81621395246475),  # 13.498220
            # (1553, 9, 19)
            (2288542.5, 175.0033258572803), # 176.059431
            # (1768, 6, 19)
            (2366978.5, 88.02759592511575),  # 88.567428
            # (1941, 9, 29)
            (2430266.5, 185.0902244497338),  # 185.945867
            # (2038, 11, 10)
            (2465737.5, 227.07078344428737), # 228.184879
            # (2094, 7, 18)
            (2486076.5, 115.381213164379), # 116.439352
            # 1992-10-13T00:00:00 DT -- 199.9060605... AA Ex.25.b
            (2448908.5, 199.8351339376054),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            result = self._bc.apparent_solar_longitude(jde)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test_apparent_solar_latitude(self):
        """
        Test that the apparent_solar_latitudemethod returns the correct values.
        """
        data = (
            # 1992-10-13T00:00:00 DT -- 0.000172 AA Ex.25.b
            (2448908.5, 0.0001739059352758293),
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            result = self._bc.apparent_solar_latitude(jde)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test__aberration(self):
        """
        Test that the _aberration method returns the correct values.
        """
        data = (
            # 1992-10-13T00:00:00 TD (-0.005705277777777778)
            (2448908.5, -0.005679205843346786),
            (2394646.5, -0.005675213832842919), # 1844-03-21
            (2451544.5, -0.005600867033197131), # 2000-01-01
            )
        msg = "Expected {}, for jde {}, found {}."

        for jde, expected_result in data:
            tm = self._bc.julian_millennia(jde)
            result = self._bc._aberration(tm)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jde, result))

    #@unittest.skip("Temporarily skipped")
    def test_approx_julian_day_for_equinoxes_or_solstices(self):
        """
        Test that the approx_julian_day_for_equinoxes_or_solstices method
        returns a Julian day of the equinoxes or solstices.
        """
        data = (
            (500, self._bc.SPRING, 1903760.376019375),
            (500, self._bc.SUMMER, 1903854.104661875),
            (500, self._bc.AUTUMN, 1903946.9228224999),
            (500, self._bc.WINTER, 1904035.8380625),
            (2000, self._bc.SPRING, 2451623.80984),
            (2000, self._bc.SUMMER, 2451716.56767),
            (2000, self._bc.AUTUMN, 2451810.21715),
            (2000, self._bc.WINTER, 2451900.05952),
            )
        msg = "Expected {}, for year {} at angle {}, found {}."

        for year, season, expected_result in data:
            result = self._bc.approx_julian_day_for_equinoxes_or_solstices(
                year, lam=season)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, season, result))

    #@unittest.skip("Temporarily skipped")
    def test_find_moment_of_equinoxes_or_solstices(self):
        """
        Test that the find_moment_of_equinoxes_or_solstices method returns
        the correct equinoxe and solstice R.D. moments for the given years.
        """
        SP = self._bc.SPRING
        SM = self._bc.SUMMER
        AU = self._bc.AUTUMN
        WN = self._bc.WINTER
        seasons = {SP: 'SPRING', SM: 'SUMMER', AU: 'AUTUMN', WN: 'WINTER'}
        data = (
            # Vernal Equinoxe
            ((900, 3, 1), SP,
             (900, 3, 15, 18, 19, 15.643359571695328)),
            ((1788, 3, 19, 22, 16), SP,
             (1788, 3, 19, 22, 16, 44.93229478597641)),
            ((1844, 3, 20, 11, 53), SP,
             (1844, 3, 20, 11, 53, 50.24219870567322)),
            ((1951, 3, 21, 10, 26), SP,
             (1951, 3, 21, 10, 26, 44.44636344909668)),
            ((1962, 6, 1), SM,
             (1962, 6, 21, 21, 25, 6.023333966732025)),
            ((2000, 3, 20, 7, 35), SP,
             (2000, 3, 20, 7, 36, 37.8661984205246)),
            ((2018, 3, 20), SP, # Should be 2018-03-20T16:16:36 DT
             (2018, 3, 20, 16, 16, 23.92842561006546)),  # Found error
            ((2022, 3, 20), SP, # Should be 2022-03-20T15:34:33 DT
             (2022, 3, 20, 15, 34, 40.105192959308624)), # Found error
            ((2024, 3, 20, 3, 6), SP,
             (2024, 3, 20, 3, 7, 49.11582201719284)),
            ((2026, 3, 20), SP, # Should be 2026-03-20T14:47:05 DT
             (2026, 3, 20, 14, 46, 50.817597806453705)), # Found error
            ((2038, 3, 20, 12, 40), SP,
             (2038, 3, 20, 12, 42, 2.2778096795082092)),
            ((2043, 3, 20), SP, # Should be 2043-03-20T17:28:58 DT
             (2043, 3, 20, 17, 29, 7.3780229687690735)), # Found error
            ((2047, 3, 20), SP, # Should be 2047-03-20T16:53:53 DT
             (2047, 3, 20, 16, 54, 14.04365211725235)),  # Found error
            ((2051, 3, 20), SP, # Should be 2051-03-20T16:00:28 DT
             (2051, 3, 20, 16, 0, 23.23518544435501)),   # Found error
            ((2055, 3, 20), SM, # Should be 2055-03-20T15:30:03 DT
             (2055, 6, 21, 8, 40, 54.626329243183136)),  # Found error
            ((2064, 3, 19, 19, 37), SP,
             (2064, 3, 19, 19, 40, 13.716727495193481)),
            ((2100, 3, 20, 13, 3), SP,
             (2100, 3, 20, 13, 6, 30.193404257297516)),
            ((2150, 3, 20, 16, 1), SP,
             (2150, 3, 20, 16, 6, 35.90936601161957)),
            ((2200, 3, 20, 18, 40), SP,
             (2200, 3, 20, 18, 48, 6.645336449146271)),
            ((2211, 3, 21, 10, 38), SP,
             (2211, 3, 21, 10, 45, 29.954682290554047)),
            # Summer Solstice
            ((900, 6, 1), SM, (900, 6, 17, 6, 32, 44.23422619700432)),
            ((2000, 6, 1), SM, (2000, 6, 21, 1, 48, 45.353220105171204)),
            # Autumn Equinox
            ((900, 9, 1), AU, (900, 9, 18, 8, 35, 1.4366135001182556)),
            ((2000, 9, 1), AU, (2000, 9, 22, 17, 28, 41.678222715854645)),
            # Winter Solstice
            ((900, 12, 1), WN, (900, 12, 16, 11, 38, 22.260328084230423)),
            ((2000, 12, 1), WN, (2000, 12, 21, 13, 38, 47.16909795999527)),
            )
        msg = "Expected '{}' during the {}, found '{}'"

        for date, season, expected_result in data:
            jde = self._gc.jd_from_gregorian_date(date)
            result = self._bc.find_moment_of_equinoxes_or_solstices(
                jde, lam=season)
            result = self._gc.gregorian_date_from_jd(result)
            result = self._gc.ymdhms_from_date(result)
            self.assertEqual(
                expected_result, result,
                msg.format(expected_result, seasons[season], result))

    #
    # Calandarical Calculations
    #

    #@unittest.skip("Temporarily skipped")
    def test_zone_from_longitude(self):
        """
        Test that the zone_from_longitude method returns the difference
        between UT and local mean time at longitude phi as a fraction of
        a day.
        """
        #    longitude   expected_result
        data = (
            (51.4777815, 0.1429938375), # Greenwich
            (32.94, 0.0915), # Acre
            )
        msg = "Expected {}, found {}."

        for phi, expected_result in data:
            result = self._bc.zone_from_longitude(phi)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_universal_from_local(self):
        """
        Test that the universal_from_local method converts universal
        time from local tee_ell at location.
        """
        data = (
            # Greenwich (2024-01-20)
            (738905, 738905.0, (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (738905, 738904.9025277778, (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, found {}."

        for tee_ell, expected_result, location in data:
            self._bc.location = location
            result = self._bc.universal_from_local(tee_ell)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_local_from_universal(self):
        """
        Test that the local_from_universal method converts local time
        from universal tee_rom_u at location.
        """
        data = (
            # Greenwich (2024-01-20)
            (738905.0, 738905, (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (738904.9025277778, 738905, (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, found {}."

        for tee_rom_u, expected_result, location in data:
            self._bc.location = location
            result = self._bc.local_from_universal(tee_rom_u)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_standard_from_universal(self):
        """
        Test that the zone is determined from the standard time from
        tee_rom_u in universal time at location.
        """
        data = (
            # Greenwich (2024-01-20)
            (738905.0, 738905.0, (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (738904.9025277778, 738904.9858611112, (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, found {}."

        for tee_rom_u, expected_result, location in data:
            self._bc.location = location
            result = self._bc.standard_from_universal(tee_rom_u)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_universal_from_standard(self):
        """
        Test that the zone is determined from the universal time from
        tee_rom_s in standard time at location.
        """
        data = (
            # Greenwich (2024-01-20)
            (738905.0, 738905.0, (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (738904.9858611112, 738904.9025277778, (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, found {}."

        for tee_rom_s, expected_result, location in data:
            self._bc.location = location
            result = self._bc.universal_from_standard(tee_rom_s)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_standard_from_local(self):
        """
        Test that the standard_from_local method returns the standard
        time from local time at location.
        """
        data = (
            # Greenwich (2024-01-20)
            (738905.0, 738905.0, (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (738904.9025277778, 738904.888388889, (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, found {}."

        for tee_ell, expected_result, location in data:
            self._bc.location = location
            result = self._bc.standard_from_local(tee_ell)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_local_from_standard(self):
        """
        Test that the local_from_standard method converts local time
        from standard tee_rom_s at location.
        """
        data = (
            # Greenwich (2024-01-20)
            (738905.0, 738905.0, (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (738904.9858611112, 738905.0, (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, found {}."

        for tee_rom_s, expected_result, location in data:
            self._bc.location = location
            result = self._bc.local_from_standard(tee_rom_s)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_ephemeris_correction(self):
        """
        Test that the ephemeris_correction method returns dynamical
        Time minus Universal Time (in days) for moment.

        Dynamical Time difference calculator:
        https://planetcalc.com/9200/
        """
        fraction = 0.5
        tees = (
            (3000, 1095363, 0.05133888888888889),
            (2100, 766645, 0.002997916666666666),
            (2028, 740347, 0.0008833626851851851),
            (2014, 735234, 0.0007931229629629629),
            (1990, 726468, 0.0006585027893518518),
            (1943, 709301, 0.05793252154447576),
            (1881, 686657, 0.019770369608438015),
            (1850, 675334, 1.7554929729946673),
            (1750, 638810, 0.00014899798891203703),
            (1650, 602286, 0.0005809492592592593),
            (1050, 383140, 0.01520822313332091),
            (0, -365, 0.12249537037037037),
            (-1000, -365607, 0.2943018518518518),
            )
        msg = "Expected result {} for year {}, found {}."

        for year, tee, expected_result in tees:
            result = self._bc.ephemeris_correction(tee+fraction)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test_dynamical_from_universal(self):
        """
        Test that the dynamical_from_universal method returns dynamical
        time at Universal moment.

        Tested with year 1850, see test_ephemeris_correction above.
        """
        tee_rom_u = 675334.5
        result = self._bc.dynamical_from_universal(tee_rom_u)
        expected_result = 675336.255492973
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_universal_from_dynamical(self):
        """
        Test that the universal_from_dynamical method returns universal
        moment from Dynamical time.

        This test is testing the inverse of the test_dynamical_from_universal
        test.

        Tested with year 1850, see test_ephemeris_correction above.
        """
        tee = 675336.255492973
        result = self._bc.universal_from_dynamical(tee)
        expected_result = 675334.5
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_julian_centuries_in_rd(self):
        """
        Test that the julian_centuries_in_rd returns a Julian centuries
        since 2000 at R.D. moment.
        """
        tee = 675334.5
        result = self._bc.julian_centuries_in_rd(tee)
        expected_result = -1.499910869460013
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_equation_of_time(self):
        """
        Test that the equation_of_time method returns (as fraction of day)
        for moment.
        """
        data = (
            (738965.12921296296296296296, -2.641140003457961e-05),
            (739057.86832175925925925926, -1.5413246282548904e-06),
            (739151.53, 2.672104530920557e-05),
            (739241.38881944444444444444, 1.447899046463908e-06),
            )
        msg = "Expected {}, found {}."

        for tee, expected_result in data:
            result = self._bc.equation_of_time(tee)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_apparent_from_local(self):
        """
        Test that the apparent_from_local method returns the sundial
        time from local time tee_ell at location.
        """
        tee_ell = 675334.5
        result = self._bc.apparent_from_local(tee_ell)
        expected_result = 675334.4999898048
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_local_from_apparent(self):
        """
        Test that the local_from_apparent method returns the local
        time from sundial time tee at location.
        """
        tee = 675334.4996718061
        result = self._bc.local_from_apparent(tee)
        expected_result = 675334.4996820006 # 675334.5
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_obliquity(self):
        """
        Test that the obliquity method returns obliquity of ecliptic
        at moment tee.
        """
        tee = 675334.5
        result = self._bc.obliquity(tee)
        expected_result = 23.458794133952004
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_declination(self):
        """
        Test that the declination method returns the declination at
        moment UT tee of object at latitude beta and longitude lambda.
        """
        tee = 675334.5
        lat = 35.696111
        lon = 51.423056
        result = self._bc.declination(tee, lat, lon)
        expected_result = 0.7880038536133311
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_alt_solar_longitude(self):
        """
        Test that the solar_longitude method returns the longitude of
        sun at moment tee.

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
            (738965.12921296296296296296, 357.04419198166175), # 0
            # (2024, 6, 20) Summer solstice 2024-06-20T20:50:23 UTC
            (739057.86832175925925925926, 90.11195106383903),  # 90
            # (2024, 9, 22) Autumnal equinox 2024-09-22T12:43:12 UTC
            (739151.53, 183.39425453494826),                   # 180
            # (2024, 12, 21) Winter solstice 2024-12-21T09:19:54 UTC
            (739241.38881944444444444444, 272.35241269293834), # 270
            # (1800, 3, 20) Vernal equinox 1800-03-20T20:11:48 UTC
            (657150.84152777777777777778, 357.97032859275566), # 0
            # (1800, 6, 21) Summer solstice 1800-06-21T17:51:29 UTC
            (657243.74408564814814814815, 90.66574929305018), # 90
            # (1800, 9, 23) Autumnal equinox 1800-09-23T07:25:31 UTC
            (657337.30938657407407407407, 179.98650666653378),  # 180
            # (1800, 12, 22) Winter solstice 1800-12-22T00:16:24
            (657427.01138888888888888889, 268.6335208164205), # 270
            # (2073, 3, 20) Vernal equinox 2073-03-20T00:12:24 UTC
            (756862.00861111111111111111, 356.2545621758088), # 0
            # (2073, 6, 20) Summer solstice 2073-06-20T17:06:18 UTC
            (756954.71270833333333333333, 91.2771129795312),  # 90
            # (2073, 9, 22) Autumnal equinox 2073-09-22T09:14:10 UTC
            (757048.38483796296296296296, 181.95382093003212),  # 180
            # (2073, 12, 21) Winter solstice 2073-12-21T06:49:41 UTC
            (757138.28450231481481481481, 271.70524477519655),  # 270
            # (-586, 7, 24)
            #(-214193, 118.98911336367019), # 118.98911336371384 Book value
            # (576, 5, 20)
            #(210155, 58.64246274798643),   # 59.119741
            # (1288, 4, 2)
            #(470160, 13.008819286362268),  # 13.498220
            # (1553, 9, 19)
            #(567118, 175.56893798499368),  # 176.059431
            # (1768, 6, 19)
            #(645554, 88.09044293293846),   # 88.567428
            # (1941, 9, 29)
            #(708842, 185.50156897346096),  # 185.945867
            # (2038, 11, 10)
            #(744313, 227.68273548343677),  # 228.184879
            # (2094, 7, 18)
            #(764652, 334.87117657772615),  # 116.439352 This is way off
            )
        msg = "Expected {}, found {}."

        for tee, expected_result in data:
            result = self._bc.alt_solar_longitude(tee)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_nutation(self):
        """
        Test that the nutation method returns Longitudinal nutation at
        moment tee.
        """
        tee = 712359 # (1951, 5, 17)
        result = self._bc.nutation(tee)
        expected_result = 0.000853464307791487
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_aberration(self):
        """
        Test that the aberration method returns the aberration at moment tee.
        Since it's on the ark of a circle the result must be
        0 <= result > 360.
        """
        tee = 712359 # (1951, 5, 17)
        result = self._bc.aberration(tee)
        expected_result = -0.005509800931812071
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    @unittest.skip("Temporarily skipped")
    def test_estimate_prior_solar_longitude(self):
        """
        Test that the estimate_prior_solar_longitude method returns an
        approximate moment at or before tee when solar longitude just
        exceeded lambda degrees.

        These dates and times are based on the Vernal Equinox figures
        printed in Astronomical Tables of the Sun, Moon, and Planets,
        by Jean Meeus, 1983 published by Willmann-Bell, Inc., Richmond,
        Virginia 23235, U.S.A.
        See both:
        https://nshdpi.ca/is/equinox/eqindex.html
        http://astropixels.com/ephemeris/soleq2001.html

        The solar longitude starts at the moment of the Vernal Equinox.
        Both results seem to be off the correct date and time.

        673223 (1844, 3, 22) -> 673218.1676991577 (1844, 3, 17.16769915772602)
        Correct date is:        March 20, 1844, 11:53 am
        738886 (2024, 1, 1)  -> 738599.875870285 (2023, 3, 20.875870284973644)
        Correct date is:        March 20, 2023, 21:24 pm UTC
        """
        lam = self._bc.SPRING
        data = (
            (652768, 652768.183333333),  # 1788-3-19T (652768.183333333)
            (673223, 673218.1676991577), # 1844-03-20T11:53:00
            (738886, 738599.875870285),  # 2023-03-20T21:01:15.192624
            )
        msg = "Expected result {}, found {}."

        for tee, expected_result in data:
            result = self._bc.estimate_prior_solar_longitude(lam, tee)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_approx_moment_of_depression(self):
        """
        Test that the approx_moment_of_depression method returns a moment
        in local time near tee when depression angle of sun is alpha
        (negative if above horizon) at location; early is true when
        morning event is sought and false for evening. Returns None if
        depression angle is not reached.

        This test assumes the location to be in Tehran for the
        astronomical Baha’i calendar.
        """
        data = (
            # Test tee = 675334.5, early = False, alpha >= 0
            (675334.5, 30.5, False, 675334.8564588708),
            # Test tee = 675334.5, early = False, alpha < 0
            (675334.5, -30.5, False, 675334.6415658242),
            # Test tee = 675334.5, early = True, alpha >= 0
            (675334.5, 30.5, True, 675334.1435615424),
            # Test tee = 675334.5, early = True, alpha < 0
            (675334.5, -30.5, True, 675334.3584545698),
            #(675334.5, 1000, True, None)
            )
        msg = "Expected {}, with alpha '{}' and early '{}', found {}."

        for tee, alpha, early, expected_result in data:
            result = self._bc.approx_moment_of_depression(tee, alpha, early)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, alpha, early, result))

    #@unittest.skip("Temporarily skipped")
    def test_sine_offset(self):
        """
        Test that the sine_offset method returns a sine of angle between
        position of sun at local time tee and when its depression is
        alpha at location.
        """
        tee = 675334.5
        alpha = 30.5
        result = self._bc.sine_offset(tee, alpha)
        expected_result = 0.6200701256619134
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_moment_of_depression(self):
        """
        Test that the moment_of_depression method returns a moment in
        local time near approx when depression angle of sun is alpha
        (negative if above horizon) at location; early is true when
        morning event is sought, and false for evening. Returns None
        if depression angle is not reached.
        """
        data = (
            (675334.856459419, 675334.5, False, 675334.6601649774),
            (675334.6415663845, 675334.5, False, 675334.6601649774),
            (675334.1435609942, 675334.5, True, 675334.339858309),
            (675334.3584540095, 675334.5, True, 675334.339858309)
            )
        msg = "Expected {}, with alpha '{}' and early '{}', found {}."

        for approx, alpha, early, expected_result in data:
            result = self._bc.moment_of_depression(approx, alpha, early)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, alpha, early, result))

    @unittest.skip("Temporarily skipped")
    def test_dawn(self):
        """
        Test that the dawn method returns the standard time in morning
        on fixed date at location when depression angle of sun is alpha.
        Returns None if there is no dawn on date.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_dusk(self):
        """
        Test that the dusk method returns the standard time in evening
        on fixed date at location when depression angle of sun is alpha.
        Returns bogus if there is no dusk on date.
        """
        #    date      alpha               expected_result    location
        data = (
            # Greenwich (2024-01-20)
            (738905.0, 0.8226397005246857, 738905.7525083687,
             (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (738905.0, 0.741981935570424, 738905.7377463694,
             (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, with date {} and alpha {}, found {}."

        for date, alpha, expected_result, location in data:
            self._bc.location = location
            result = self._bc.dusk(date, alpha)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, alpha, result))

    #@unittest.skip("Temporarily skipped")
    def test_refraction(self):
        """
        Test that the refraction method returns a refraction angle at
        moment tee at location. The moment is not used.
        """
        #    expected_result     location
        data = (
            # Greenwich (2024-01-20)
            (0.8226397005246857, (51.4777815, 0, 46.9, 0)),
            # Acre (2024-01-20)
            (0.741981935570424, (32.94, 35.09, 22, 2)),
            )
        msg = "Expected {}, found {}."

        for expected_result, location in data:
            self._bc.location = location
            result = self._bc.refraction()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    @unittest.skip("Temporarily skipped")
    def test_sunrise(self):
        """
        Test that the sunrise method returns the standard time of
        sunrise on fixed date at location.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_sunset(self):
        """
        Test that the sunset method returns the standard time of
        sunset on fixed date at location.

        Finds the lat and lon of a given location.
        https://www.latlong.net/
        https://www.timeanddate.com/sun/@112931?month=3&year=1844
        """
        dates = (
            # New York (2024-01-20)
            #     Official: Sunset: 2024-01-20 16:57:32.839373-05:00
            # Astronomical: Sunset: 2024-01-20 18:33:38.288695-05:00
            (738905, 738905.749950217,
             (40.7127281, -74.0060152, 10.0584, -5)),
            # Tehran (1844-03-20)
            #     Official: Sunset: 1844-03-20 18:11:15.013257+03:26
            # Astronomical: Sunset: 1844-03-20 19:36:34.035553+03:26
            (673221, 673221.7559442059,
             (35.6892523, 51.3896004, 0, 3.5)),
            )
        msg = "Expected {}, found {}."

        for date, expected_result, location in dates:
            self._bc.location = location
            result = self._bc.sunset(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_sin_deg(self):
        """
        Test that the sin_deg method returns the sine of theta
        (given in degrees).
        """
        theta = 90.0
        result = self._bc.sin_deg(theta)
        expected_result = 1.0
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_cos_deg(self):
        """
        Test that the cos_deg method returns the cosine of theta
        (given in degrees).
        """
        theta = 90.0
        result = self._bc.cos_deg(theta)
        expected_result = 6.123233995736766e-17 # is 0?
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_tan_deg(self):
        """
        Test that the tan_deg method returns the tangent of theta
        (given in degrees).
        """
        theta = 90.0
        result = self._bc.tan_deg(theta)
        expected_result = 1.633123935319537e+16
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_arctan_degrees(self):
        """
        Test that the arctan_degrees method returns the arctangent of
        y/x in degrees. Returns bogus if x and y are both 0.
        """
        x = 2.0
        y = 2.0
        result = self._bc.arctan_degrees(x, y)
        expected_result = 45
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

        # Test that both x and y are equal to 0.
        x = y = 0
        msg = "Expected {}, found {}."
        expected_result = (
            f"The value of x '{x}' and y '{y}' must not be x == 0 == y.")

        with self.assertRaises(AssertionError) as cm:
            self._bc.arctan_degrees(x, y)

        result = str(cm.exception)
        self.assertEqual(expected_result, result,
                         msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_arcsin_degrees(self):
        """
        Test that the arcsin_degrees method returns the arcsine of x
        in degrees.

        https://www.rapidtables.com/calc/math/Arcsin_Calculator.html
        """
        X = ((-1, -90), (-0.8660254, -60), (-0.7071068, -45), (-0.5, -30),
             (0, 0), (0.5, 30), (0.7071068, 45), (0.8660254, 60), (1, 90))
        msg = "Expected {} with {}, found {}."

        for x, expected_result in X:
            result = self._bc.arcsin_degrees(x)
            self.assertTrue(
                math.isclose(expected_result, result, rel_tol=0.00000009),
                msg.format(expected_result, x, result))

        # Test x out of range.
        X = (-5, 5)
        msg = "Expected error {}, found {}"

        for x in X:
            expected_result = f"The value of x '{x}' must be >= -1 and <= 1."

            with self.assertRaises(AssertionError) as cm:
                self._bc.arcsin_degrees(x)

            result = str(cm.exception)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_arccos_degrees(self):
        """
        Test that the arccos_degrees method returns the arccosine of x
        in degrees.

        https://www.rapidtables.com/calc/math/Arccos_Calculator.html
        """
        X = ((-1, 180), (-0.8660254, 150), (-0.7071068, 135), (-0.5, 120),
             (0, 90), (0.5, 60), (0.7071068, 45), (0.8660254, 30), (1, 0))
        msg = "Expected {} with {}, found {}."

        for x, expected_result in X:
            result = self._bc.arccos_degrees(x)
            self.assertTrue(
                math.isclose(expected_result, result, rel_tol=0.00000009),
                msg.format(expected_result, x, result))

        # Test x out of range.
        X = (-5, 5)
        msg = "Expected error {}, found {}"

        for x in X:
            expected_result = f"The value of x '{x}' must be >= -1 and <= 1."

            with self.assertRaises(AssertionError) as cm:
                self._bc.arccos_degrees(x)

            result = str(cm.exception)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_moment(self):
        """
        Test that the fixed_from_moment returns the floor of tee.
        """
        tee = 10.99999
        moment = self._bc.fixed_from_moment(tee)
        expected_moment = math.floor(tee)
        msg = f"Expected {expected_moment}, found {moment}."
        self.assertEqual(expected_moment, moment, msg)

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
        result = self._bc._sigma(lists, func)
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
        poly = self._bc._poly(x, a)
        expected_poly = 0
        msg = f"POLY should be {expected_poly}, found {poly}."
        self.assertEqual(expected_poly, poly, msg)
        # Test 'a' has a value
        x = 0.1
        a = (1, 2, 3, 4)
        poly = self._bc._poly(x, a)
        expected_poly = 1.234
        msg = f"POLY should be {expected_poly}, found {poly}."
        self.assertEqual(expected_poly, poly, msg)

    #@unittest.skip("Temporarily skipped")
    def test__next(self):
        """
        Test that the next_ method returns the first integer greater
        or equal to initial such that condition holds.
        """
        y = 20
        condition = lambda x: x >= y
        msg = "Expected {}, found {}"
        data = ((0, 20), (6, 20), (2, 20), (15, 20), (40, 40), (100, 100))

        for initial, expected_result in data:
            result = self._bc._next(initial , condition)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    @unittest.skip("Temporarily skipped")
    def test__final(self):
        """
        Test that the _final method returns the last integer greater
        or equal to initial such that condition holds.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test__to_radix(self):
        """
        Test that the _to_radix method returns the radix notation
        corresponding to x with base b for whole part and c for fraction.
        """
        data = (
            (10, (1, 2, 3, 4), (), (0, 0, 0, 2, 2)),
            (10, (1, 2, 3, 4), (4, 3, 2, 1), (0, 0, 0, 2, 2, 0, 0, 0, 0)),
            )
        msg = "Expected {} with '{}, {}, {}', found {}."

        for x, b, c, expected_result in data:
            result = self._bc._to_radix(x, b, c)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, x, b, c, result))

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
            result = self._bc.decimal_from_dms(*args)
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
            result = self._bc.dms_from_decimal(*args)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, args, result))
