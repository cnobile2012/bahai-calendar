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


class TestBaseCalandar(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        class FakeParent(BaseCalendar):
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

    #@unittest.skip("Temporarily skipped")
    def test_HR(self):
        """
        Test the HR (hour) lambda.
        """
        value = 6
        hr = self._bc.HR(value)
        expected_hr = value / 24
        msg = f"HR should be {expected_hr}, found {hr}."
        self.assertEqual(expected_hr, hr, msg)

    #@unittest.skip("Temporarily skipped")
    def test_MN(self):
        """
        Test the MN (minute) lambda.
        """
        value = 6
        mn = self._bc.MN(value)
        expected_mn = value / 24 / 60
        msg = f"MN should be {expected_mn}, found {mn}."
        self.assertEqual(expected_mn, mn, msg)

    #@unittest.skip("Temporarily skipped")
    def test_SEC(self):
        """
        Test the sec (second) lambda.
        """
        value = 6
        sec = self._bc.SEC(value)
        expected_sec = value / 24 / 60 / 60
        msg = f"SEC should be {expected_sec}, found {sec}."
        self.assertEqual(expected_sec, sec, msg)

    #@unittest.skip("Temporarily skipped")
    def test_MINS(self):
        """
        Test the MINS (minutes) lambda.
        """
        value = 6
        mins = self._bc.MINS(value)
        expected_mins = value / 60
        msg = f"MINS should be {expected_mins}, found {mins}."
        self.assertEqual(expected_mins, mins, msg)

    #@unittest.skip("Temporarily skipped")
    def test_SECS(self):
        """
        Test the SECS (seconds) lambda.
        """
        value = 6
        secs = self._bc.SECS(value)
        expected_secs = value / 3600
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
            all([i == j for i, j in zip(expected_dt, self._bc._time)]), msg)

    @unittest.skip("Temporarily skipped")
    def test_time_representation_getter(self):
        pass

    @unittest.skip("Temporarily skipped")
    def test_time_representation_setter(self):
        pass

    #@unittest.skip("Temporarily skipped")
    def test_zone_from_longitude(self):
        """
        Test that the zone_from_longitude method returns the difference
        between UT and local mean time at longitude phi as a fraction of
        a day.
        """
        phi = 1000
        result = self._bc.zone_from_longitude(phi)
        expected_result = 2.7777777777777777
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_universal_from_local(self):
        """
        Test that the universal_from_local method converts universal
        time from local tee_ell at location.
        """
        tee_ell = 10000.5
        result = self._bc.universal_from_local(tee_ell)
        expected_result = 10000.357158177778
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_local_from_universal(self):
        """
        Test that the local_from_universal method converts local time
        from universal tee_rom_u at location.
        """
        tee_rom_u = 10000.357158177778
        result = self._bc.local_from_universal(tee_rom_u)
        expected_result = 10000.5
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_standard_from_universal(self):
        """
        Test that the zone is determined from the standard time from
        tee_rom_u in universal time at location.
        """
        tee_rom_u = 10000.5
        result = self._bc.standard_from_universal(tee_rom_u)
        expected_result = 10004.0
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_universal_from_standard(self):
        """
        Test that the zone is determined from the universal time from
        tee_rom_s in standard time at location.
        """
        tee_rom_s = 10004.0
        result = self._bc.universal_from_standard(tee_rom_s)
        expected_result = 10000.5
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_standard_from_local(self):
        """
        Test that the standard_from_local method returns the standard
        time from local time at location.
        """
        tee_ell = 10000.5
        result = self._bc.standard_from_local(tee_ell)
        expected_result = 10003.857158177778
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_local_from_standard(self):
        """
        Test that the local_from_standard method converts local time
        from standard tee_rom_s at location.
        """
        tee_rom_s = 10003.857158177778
        result = self._bc.local_from_standard(tee_rom_s)
        expected_result = 10000.5
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_ephemeris_correction(self):
        """
        Test that the ephemeris_correction method returns dynamical
        Time minus Universal Time (in days) for moment.
        """
        fraction = 0.5
        tees = ((2100, 766645, 230.88001157407405),
                (2028, 740347, 0.0008833626851851851),
                (2014, 735234, 0.0007931229629629629),
                (1943, 709301, 0.05793252154447576),
                (1850, 675334, 1.7554929729946673),
                (1750, 638810, 0.00014899798891203703),
                (1650, 602286, 0.0005809492592592593),
                (1050, 383140, 0.01520822313332091),
                (0, -365, 0.12249537037037037),
                (3000, 1095363, 0.05133888888888889),
                (-1000, -365607, 0.2943018518518518))
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
    def test_julian_centuries(self):
        """
        Test that the julian_centuries returns a Julian centuries since
        2000 at moment.
        """
        tee = 675334.5
        result = self._bc.julian_centuries(tee)
        expected_result = -1.499910869460013
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_equation_of_time(self):
        """
        Test that the equation_of_time method returns (as fraction of day)
        for moment.
        """
        tee = 675334.5
        result = self._bc.equation_of_time(tee)
        expected_result = -1.049036856585601e-05
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

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
    def test_solar_longitude(self):
        """
        Test that the solar_longitude method returns the longitude of
        sun at moment tee.
        """
        tee = -214193
        result = self._bc.solar_longitude(tee)
        expected_result = 118.98911336371384
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    @unittest.skip("Temporarily skipped")
    def test_nutation(self):
        """
        Test that the
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_aberration(self):
        """
        Test that the aberration method returns the aberration at moment tee.
        Since it's on the ark of a circle the result must be
        0 <= result > 360.
        """
        tee = 675334 # (1850, 1, 1)
        result = self._bc.aberration(tee)
        expected_result = 0
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_estimate_prior_solar_longitude(self):
        """
        Test that the estimate_prior_solar_longitude method returns an
        approximate moment at or before tee when solar longitude just
        exceeded lambda degrees.
        """
        lambda_ = self._bc.SPRING
        tee = 673222.6070645516
        result = self._bc.estimate_prior_solar_longitude(lambda_, tee)
        expected_result = 672855.2913903068
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    @unittest.skip("Temporarily skipped")
    def test_nth_new_moon(self):
        """
        Test that the nth_new_moon method returns a moment of n-th new
        moon after (or before) the new moon of January 11, 1.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_new_moon_at_or_after(self):
        """
        Test that the new_moon_at_or_after method moment UT of first
        new moon at or after tee.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_lunar_longitude(self):
        """
        Test that the lunar_longitude method returns the longitude of
        moon (in degrees) at moment.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_lunar_phase(self):
        """
        Test that the lunar_phase method returns the lunar phase,
        as an angle in degrees, at moment.
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_approx_moment_of_depression(self):
        """
        Test that the approx_moment_of_depression method returns a moment
        in local time near tee when depression angle of sun is alpha
        (negative if above horizon) at location; early is true when
        morning event is sought and false for evening. Returns None if
        depression angle is not reached.
        """
        # 1. Test tee = 675334.5, early = False, alpha >= 0
        # 2. Test tee = 675334.5, early = False, alpha < 0
        # 3. Test tee = 675334.5, early = True, alpha >= 0
        # 4. Test tee = 675334.5, early = True, alpha < 0
        data = (
            (675334.5, 30.5, False, 675334.856459419),
            (675334.5, -30.5, False, 675334.6415663845),
            (675334.5, 30.5, True, 675334.1435609942),
            (675334.5, -30.5, True, 675334.3584540095),
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
        expected_result = 0.6200728278403642
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
            (675334.856459419, 675334.5, False, 675334.6601645576),
            (675334.6415663845, 675334.5, False, 675334.6601645576),
            (675334.1435609942, 675334.5, True, 675334.3398568633),
            (675334.3584540095, 675334.5, True, 675334.3398568633)
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
        Test that the dusk method a standard time in morning on fixed
        date at location when depression angle of sun is alpha. Returns
        bogus if there is no dawn on date.
        """
        date = 675334.5
        alpha = 0.5666666666666667
        result = self._bc.dusk(date, alpha)
        expected_result = 675339.1083339454
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_refraction(self):
        """
        Test that the refraction method returns a refraction angle at
        moment tee at location. The moment is not used.
        """
        tee = 675334.5
        result = self._bc.refraction(tee)
        expected_result = 0.5666666666666667
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

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
        """
        dates = (
            # New York (2024-01-20)
            #     Official: Sunset: 2024-01-20 16:57:32.839373-05:00
            # Astronomical: Sunset: 2024-01-20 18:33:38.288695-05:00
            #(738905, 738905, (40.7127281, -74.0060152, 10.0584, -5)),
            # Tehran (1844-03-20)
            #     Official: Sunset: 1844-03-20 18:11:15.013257+03:26
            # Astronomical: Sunset: 1844-03-20 19:36:34.035553+03:26
            (673221, 673225.1101307202, (35.6892523, 51.3896004, 0, 3.5)),
            )
        msg = "Expected {}, found {}."

        for date, expected_result, location in dates:
            self._bc.location = location
            result = self._bc.sunset(date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_sin_degrees(self):
        """
        Test that the sin_degrees method returns the sine of theta
        (given in degrees).
        """
        theta = 90.0
        result = self._bc.sin_degrees(theta)
        expected_result = 1.0
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_cos_degrees(self):
        """
        Test that the cos_degrees method returns the cosine of theta
        (given in degrees).
        """
        theta = 90.0
        result = self._bc.cos_degrees(theta)
        expected_result = 6.123233995736766e-17 # is 0?
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_tan_degrees(self):
        """
        Test that the tan_degrees method returns the tangent of theta
        (given in degrees).
        """
        theta = 90.0
        result = self._bc.tan_degrees(theta)
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

    #@unittest.skip("Temporarily skipped")
    def test_arcsin_degrees(self):
        """
        Test that the arcsin_degrees method returns the arcsine of x
        in degrees.
        """
        # Test a valid x.
        x = -0.9
        result = self._bc.arcsin_degrees(x)
        expected_result = -64.15806723683288
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)
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
        """
        # Test a valid x.
        x = -0.9
        result = self._bc.arccos_degrees(x)
        expected_result = 154.15806723683286
        msg = f"Expected {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)
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

    #@unittest.skip("Temporarily skipped")
    def test__final(self):
        """
        Test that the _final method returns the last integer greater
        or equal to initial such that condition holds.
        """
        pass
