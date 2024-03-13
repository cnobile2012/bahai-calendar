# -*- coding: utf-8 -*-
#
# bahai_calendar/test/test_julian_period.py
#
__docformat__ = "restructuredtext en"

import os
import unittest
#import datetime

from ..base_calendar import BaseCalendar
from ..julian_period import JulianPeriod
from ..gregorian_calendar import GregorianCalendar


class TestJulianPeriod(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._jp = JulianPeriod()
        self._gc = GregorianCalendar()

    #@unittest.skip("Temporarily skipped")
    def test_moment_from_jd(self):
        """
        Test that the moment_from_jd method returns an R.D. moment from a
        Julian day.
        """
        data = (
            (0.0, -1721424.5),
            (2394646.5, 673222.0),
            )
        msg = "Expected {} for jd {}, found {}"

        for jd, expected_result in data:
            result = self._jp.moment_from_jd(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_moment(self):
        """
        Test that the jd_frommoment method returns a Julian day from a
        R.D. moment.
        """
        data = (
            (-1721424.5, 0.0),
            (673222.0, 2394646.5),
            )
        msg = "Expected {}, found {}"

        for moment, expected_result in data:
            result = self._jp.jd_from_moment(moment)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_mjd(self):
        """
        Test that the fixed_from_mjd method returns an R.D. fixed day
        from a modified Julian day.
        """
        data = (
            (-2400000.5, -1721424.5),
            (-5354.0, 673222.0),
            )
        msg = "Expected {} for mjd {}, found {}"

        for mjd, expected_result in data:
            result = self._jp.fixed_from_mjd(mjd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, mjd, result))

    #@unittest.skip("Temporarily skipped")
    def test_mjd_from_fixed(self):
        """
        Test that the mjd_from_fixed method returns an R.D. fixed day
        from a modified Julian day.
        """
        data = (
            (-1721424.5, -2400000.5),
            (673222.0, -5354.0),
            )
        msg = "Expected {} for {}, found {}"

        for fixed, expected_result in data:
            result = self._jp.mjd_from_fixed(fixed)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, fixed, result))

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_jd(self):
        """
        Test that the fixed_from_jd method returns an R.D. fixed day
        from a Julian day.
        """
        data = (
            (0.0, -1721425),
            (2394646.5, 673222),
            )
        msg = "Expected {} for js {}, found {}"

        for jd, expected_result in data:
            result = self._jp.fixed_from_jd(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, jd, result))

    #@unittest.skip("Temporarily skipped")
    def test_jd_from_fixed(self):
        """
        Test that the jd_frommoment method returns a Julian day from a
        R.D. moment.
        """
        data = (
            (-1721424, 0.5),
            (673222, 2394646.5),
            )
        msg = "Expected {} for fixed {}, found {}"

        for fixed, expected_result in data:
            result = self._jp.jd_from_fixed(fixed)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, fixed, result))

    #@unittest.skip("Temporarily skipped")
    def test_time_from_moment(self):
        """
        Test that the time_from_moment method returns the time part of
        the moment.
        """
        data = (
            (2394646.5, 0.5),
            (0.75, 0.75),
            )
        msg = "Expected {}, found {}"

        for moment, expected_result in data:
            result = self._jp.time_from_moment(moment)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_julian_centuries_in_rd(self):
        """
        Test that the julian_centuries method returns the Julian
        century in dynamical time from a R.D. moment.
        """
        class FakeParent(BaseCalendar):
            pass

        fp = FakeParent()

        data = (
            (673222, -1.5577049096121691),
            (self._jp.RD_J2000, 2.0236014034713807e-08),
            )
        msg = "Expected {} for tee {}, found {}"

        for tee, expected_result in data:
            result = fp.julian_centuries_in_rd(tee)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, tee, result))

    #@unittest.skip("Temporarily skipped")
    def test_julian_centuries(self):
        """
        Test that the julian_centuries method returns the Julian
        century in dynamical time from a Julian moment.
        """
        data = (
            (2394646.5, -1.5577960301163587),
            (self._jp.J2000, 0.0),
            )
        msg = "Expected {} for tee {}, found {}"

        for tee, expected_result in data:
            result = self._jp.julian_centuries(tee)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, tee, result))

    #@unittest.skip("Temporarily skipped")
    def test_julian_leap_year(self):
        """
        Test that the julian_leap_year method returns true or false
        for the Julian leap year.
        """
        data = (
            (0, True),
            (1, False),
            (4, True)
            )
        msg = "Expected {} for {}, found {}"

        for year, expected_result in data:
            result = self._jp.julian_leap_year(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, year, result))

    #@unittest.skip("Temporarily skipped")
    def test_fixed_day_from_julian_date(self):
        """
        Test that the fixed_day_from_julian_date method returns a
        Julian date.
        """
        data = (
            ((1, 1, 1.0), -1.0),
            ((1, 1, 1.5), -0.5),
            ((1, 1, 2.0), 0.0),
            ((4714, 1, 1.0), 1721422.0),
            ((6295, 10, 4.0), 2299158.0),
            ((6295, 10, 5.0), 2299159.0),
            ((6732, 9, 19.75), 2458758.75),
            ((1844, 3, 9), 673222),
            )
        msg = "Expected {} for {}, found {}"

        for j_date, expected_result in data:
            result = self._jp.fixed_day_from_julian_date(j_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, j_date, result))

    #@unittest.skip("Temporarily skipped")
    def test_julian_day_from_julian_date(self):
        """
        Test that the julian_day_from_julian_date method returns an R.D.
        fixed day from a Julian day.
        """
        data = (
            # Monday   4713 B.C. January 1  12:00:00.0        0.0
            ((1, 1, 1.0), 0.0),
            # Monday   4713 B.C. January 2  00:00:00.0        0.0
            ((1, 1, 1.5), 0.5),
            # Tuesday  4713 B.C. January 2  12:00:00.0        1.0
            ((1, 1, 2.0), 1.0),
            # Saturday    1 A.D. January 1  00:00:00.0  1721423.5
            ((4714, 1, 1.0), 1721423.0),
            # Thursday 1582 A.D. October 4  00:00:00.0  2299159.5
            ((6295, 10, 4.0), 2299159.0),
            # Friday   1582 A.D. October 15 00:00:00.0  2299160.5
            ((6295, 10, 5.0), 2299160.0),
            # Thursday 2019 A.D. October 3  00:00:00.0  2458759.5
            ((6732, 9, 19.75), 2458759.75),
            )
        msg = "Expected {} for (), found {}"

        for j_date, expected_result in data:
            result = self._jp.julian_day_from_julian_date(j_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, j_date, result))

    #@unittest.skip("Temporarily skipped")
    def test_julian_date_from_fixed_day(self):
        """
        Test that the julian_date_from_fixed_day method returns a
        Julian date from a fixed day.
        """
        data = (
            (-1.0, (1, 1, 1.0)),
            (-0.5, (1, 1, 1.5)),
            (0.0, (1, 1, 2.0)),
            (1721422.0, (4714, 1, 1.0)),
            (2299158.0, (6295, 10, 4.0)),
            (2299159.0, (6295, 10, 5.0)),
            (2458758.75, (6732, 9, 19.75)),
            (673222, (1844, 3, 9)),
            )
        msg = "Expected {} for f_day {}, found {}"

        for f_day, expected_result in data:
            result = self._jp.julian_date_from_fixed_day(f_day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, f_day, result))

    #@unittest.skip("Temporarily skipped")
    def test_julian_date_from_julian_day(self):
        """
        Test that the julian_date_from_julian_day methods returns a
        fixed date from a Julian day.
        """
        data = (
            # Monday   4713 B.C. January 1  12:00:00.0        0.0
            (0.0, (1, 1, 1.0)),
            # Monday   4713 B.C. January 2  00:00:00.0        0.0
            (0.5, (1, 1, 1.5)),
            # Tuesday  4713 B.C. January 2  12:00:00.0        1.0
            (1.0, (1, 1, 2.0)),
            # Saturday    1 A.D. January 1  00:00:00.0  1721423.5
            (1721423.0, (4714, 1, 1.0)),
            # Thursday 1582 A.D. October 4  00:00:00.0  2299159.5
            (2299159.0, (6295, 10, 4.0)),
            # Friday   1582 A.D. October 15 00:00:00.0  2299160.5
            (2299160.0, (6295, 10, 5.0)),
            # Thursday 2019 A.D. October 3  00:00:00.0  2458759.5
            (2458759.75, (6732, 9, 19.75)),
            )
        msg = "Expected {} for j_day {}, found {}"

        for j_day, expected_result in data:
            result = self._jp.julian_date_from_julian_day(j_day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, j_day, result))
