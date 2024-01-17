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
        expected_angle = 49.35746666666667
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
        msg = f"Should be {expected_dt}, found {self._bc._time}"
        self.assertTrue(
            all([i == j for i, j in zip(expected_dt, self._bc._time)]), msg)

    @unittest.skip("Temporarily skipped")
    def test_date_representation_getter(self):
        pass

    @unittest.skip("Temporarily skipped")
    def test_date_representation_setter(self):
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
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_universal_from_local(self):
        """
        Test that the zone is determined from the time zone.
        """
        tee_ell = 10000.5
        result = self._bc.universal_from_local(tee_ell)
        expected_result = tee_ell - self._bc.zone_from_longitude(
            self._bc.longitude)
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_standard_from_universal(self):
        """
        Test that the zone is determined from the standard time from
        tee_rom_u in universal time at location.
        """
        tee_rom_u = 10000.5
        result = self._bc.standard_from_universal(tee_rom_u)
        expected_result = tee_rom_u + self._bc.zone
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_universal_from_standard(self):
        """
        Test that the zone is determined from the universal time from
        tee_rom_s in standard time at location.
        """
        tee_rom_s = 10000.5
        result = self._bc.universal_from_standard(tee_rom_s)
        expected_result = tee_rom_s - self._bc.zone
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_standard_from_local(self):
        """
        Test that the standard_from_local method returns the standard
        time from local time at location.
        """
        tee_ell = 10000.5
        result = self._bc.standard_from_local(tee_ell)
        expected_result = self._bc.standard_from_universal(
            self._bc.universal_from_local(tee_ell))
        msg = f"Should be {expected_result}, found {result}."
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
        msg = f"Should be {expected_result}, found {result}."
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
        msg = f"Should be {expected_result}, found {result}."
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
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_equation_of_time(self):
        """
        Test that the equation_of_time method returns (as fraction of day)
        for moment.
        """
        tee = 675334.5
        result = self._bc.equation_of_time(tee)
        expected_result = -0.00033553630301943875
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_apparent_from_local(self):
        """
        Test that the apparent_from_local method returns the sundial
        time from local time tee_ell at location.
        """
        tee_ell = 675334.5
        result = self._bc.apparent_from_local(tee_ell)
        expected_result = 675334.4996718061
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_local_from_apparent(self):
        """
        Test that the local_from_apparent method returns the local
        time from sundial time tee at location.
        """
        tee = 675334.4996718061
        result = self._bc.local_from_apparent(tee)
        expected_result = 675334.4999999832 # 675334.5
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    #apparent-from-universal
    #universal-from-apparent
    #midnight
    #midday
    #sidereal-from-moment

    #@unittest.skip("Temporarily skipped")
    def test_obliquity(self):
        """
        Test that the obliquity method returns obliquity of ecliptic
        at moment.
        """
        tee = 675334.5
        result = self._bc.obliquity(tee)
        expected_result = 50.52764803712032
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)

    @unittest.skip("Temporarily skipped")
    def test_declination(self):
        """
        Test that the
        """
        pass

    #mean-tropical-year
    #mean-sidereal-year

    #@unittest.skip("Temporarily skipped")
    def test_solar_longitude(self):
        """
        Test that the solar_longitude method returns the correct season.
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_nutation(self):
        """
        Test that the
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_aberration(self):
        """
        Test that the
        """
        pass

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
        msg = f"Should be {expected_result}, found {result}."
        self.assertEqual(expected_result, result, msg)





    @unittest.skip("Temporarily skipped")
    def test_new_moon_at_or_after(self):
        """
        Test that the new_moon_at_or_after method 
        """
        pass

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_moment(self):
        """
        Test that the fixed_from_moment returns the floor of tee.
        """
        tee = 10.99999
        moment = self._bc.fixed_from_moment(tee)
        expected_moment = math.floor(tee)
        msg = f"Should be {expected_moment}, found {moment}."
        self.assertEqual(expected_moment, moment, msg)

    @unittest.skip("Temporarily skipped")
    def test_nth_new_moon(self):
        """
        Test that the nth_new_moon method 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_lunar_phase(self):
        """
        Test that the lunar_phase 
        """
        pass

    @unittest.skip("Temporarily skipped")
    def test_lunar_longitude(self):
        """
        Test that the lunar_longitude 
        """
        pass



    #@unittest.skip("Temporarily skipped")
    def test_poly(self):
        """
        Test the poly (polynomial) lambda.
        """
        # Test not x
        x = 10
        a = ()
        poly = self._bc._poly(x, a)
        expected_poly = 0
        msg = f"POLY should be {expected_poly}, found {poly}."
        self.assertEqual(expected_poly, poly, msg)
        # Test a has a value
        x = 0.1
        a = (1, 2, 3, 4)
        poly = self._bc._poly(x, a)
        expected_poly = 1.234
        msg = f"POLY should be {expected_poly}, found {poly}."
        self.assertEqual(expected_poly, poly, msg)

    #@unittest.skip("Temporarily skipped")
    def test_next_(self):
        """
        Test that the next_ method returns the first integer greater
        or equal to initial such that condition holds.
        """
        con = lambda x: x not in range(20)

        for initial in (0, 6, 2, 15, 40, 100):
            result = self._bc._next(initial , con)
            #print(initial, result)




