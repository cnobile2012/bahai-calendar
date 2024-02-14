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
        msg = "Expected {}, found {}"

        for jd, expected_result in data:
            result = self._jp.moment_from_jd(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

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
        msg = "Expected {}, found {}"

        for mjd, expected_result in data:
            result = self._jp.fixed_from_mjd(mjd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

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
        msg = "Expected {}, found {}"

        for fixed, expected_result in data:
            result = self._jp.mjd_from_fixed(fixed)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

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
        msg = "Expected {}, found {}"

        for jd, expected_result in data:
            result = self._jp.fixed_from_jd(jd)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

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
        msg = "Expected {}, found {}"

        for fixed, expected_result in data:
            result = self._jp.jd_from_fixed(fixed)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

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
    def test_julian_centuries(self):
        """
        Test that the julian_centuries method returns the Julian
        century in dynamical time from a R.D. moment.
        """
        class FakeParent(BaseCalendar):
            pass

        fp = FakeParent()

        data = (
            (673222, -1.5577049096121691),
            (730120.5, 2.0236014034713807e-08),
            )
        msg = "Expected {}, found {}"

        for tee, expected_result in data:
            result = fp.julian_centuries(tee)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

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
        msg = "Expected {}, found {}"

        for year, expected_result in data:
            result = self._jp.julian_leap_year(year)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test_fixed_from_julian(self):
        """
        Test that the fixed_from_julian method returns an R.D.
        fixed day from a Julian day.
        """
        class FakeParent(BaseCalendar):
            pass

        fp = FakeParent()

        data = (
            # Monday   4713 B.C. January 1  12:00:00.0        0.0
            ((1, 1, 1, 1.0), 0.0),
            # Monday   4713 B.C. January 2  00:00:00.0        0.0
            ((1, 1, 1, 1.5), 0.5),
            # Tuesday  4713 B.C. January 2  12:00:00.0        1.0
            ((1, 1, 1, 2.0), 1.0),
            # Saturday    1 A.D. January 1  00:00:00.0  1721423.5
            ((1, 4714, 1, 1.0), 1721423.0),
            # Thursday 1582 A.D. October 4  00:00:00.0  2299159.5
            ((1, 6295, 10, 4.0), 2299159.0),
            # Friday   1582 A.D. October 15 00:00:00.0  2299160.5
            ((1, 6295, 10, 5.0), 2299160.0),
            # Thursday 2019 A.D. October 3  00:00:00.0  2458759.5
            ((1, 6732, 9, 19.75), 2458759.75),
            )
        msg = "Expected {}, found {}"

        for j_date, expected_result in data:
            result = fp.fixed_from_julian(j_date)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))

    @unittest.skip("Temporarily skipped")
    def test_julian_period_from_julian_day(self):
        """
        Test that the julian_period_from_julian_day method converts to
        the correct Julian date from the julian period.

        See:
        ----
        https://aa.usno.navy.mil/data/JulianDate
        https://ssd.jpl.nasa.gov/tools/jdc/#/jd
        """
        data = (
            # Monday   4713 B.C. January 1  12:00:00.0        0.0
            (0.0, (1, 1, 1, 1.0)),
            # Monday   4713 B.C. January 2  00:00:00.0        0.0
            (0.5, (1, 1, 1, 1.5)),
            # Tuesday  4713 B.C. January 2  12:00:00.0        1.0
            (1.0, (1, 1, 1, 2.0)),
            (366.0, (1, 2, 1, 1.0)),
            (366.5, (1, 2, 1, 1.5)),
            (366.0, (1, 2, 1, 1.0)),
            (366.5, (1, 2, 1, 1.5)),

            # Saturday    1 A.D. January 1  00:00:00.0  1721423.5
            (1721423.5, (1, 4714, 1, 1)),
            # Thursday 1582 A.D. October 4  00:00:00.0  2299159.5
            (2299159.5, (1, 6295, 10, 4.0)),
            # Friday   1582 A.D. October 15 00:00:00.0  2299160.5
            (2299160.5, (1, 6295, 10, 5.0)),
            # Thursday 2019 A.D. October 3  00:00:00.0  2458759.5
            (2458759.5, (1, 6732, 9, 19.75)),
            )
        msg = "Expected {}, found {}"

        for day, expected_result in data:
            result = self._jp.julian_period_from_julian_day(day)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, result))
