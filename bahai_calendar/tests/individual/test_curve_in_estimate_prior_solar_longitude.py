#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Test to see if the curve has deleterious effects on finding the
# Vernal Equinox.
#
__docformat__ = "restructuredtext en"

import os
import sys
import math
import pprint

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))))
sys.path.append(BASE_DIR)

from bahai_calendar.base_calendar import BaseCalendar
from bahai_calendar.gregorian_calendar import GregorianCalendar


class TestVernalEquinox(BaseCalendar):
    """
    We use the estimate_prior_solar_longitude method to test for the
    correct date for the Vernal Equinox.

    See https://nshdpi.ca/is/equinox/vern1788-2211.html for correct dates
    in the Gregorian and Julian Calendars.

    See https://gml.noaa.gov/grad/solcalc/
    """

    def __init__(self):
        super().__init__()

    def generate_curve(self):
        """
        Generates the date needed to chart the S curve of accuracy of the
        BaseCalendar.estimate_prior_solar_longitude method.
        """
        gc = GregorianCalendar()
        data = {}

        for year in range(1788, 2212):
            fixed = gc.fixed_from_gregorian((year, 4, 1))
            ve_fixed = self.estimate_prior_solar_longitude(self.SPRING, fixed)
            g_date = gc.gregorian_from_fixed(ve_fixed)
            ve_date = self.convert_to_full_date(g_date)
            iso_date = self.convert_to_iso(ve_date)
            season = self.get_solar_longitude(year, alt=True)
            data[year] = (ve_fixed, g_date, ve_date, iso_date, season)

        return data

    def convert_to_full_date(self, fixed):
        year = fixed[0]
        month = fixed[1]
        raw_day = fixed[2]
        day = math.floor(raw_day)
        day_fraction = raw_day - day
        raw_hour = day_fraction * 24
        hour = math.floor(raw_hour)
        hour_fraction = raw_hour - hour
        raw_minute = hour_fraction * 60
        minute = math.floor(raw_minute)
        minute_fraction = raw_minute - minute
        second = minute_fraction
        return (year, month, day, hour, minute, second)

    def convert_to_iso(self, ve_date):
        year = ve_date[0]
        month = ve_date[1]
        day = ve_date[2]
        hour = ve_date[3]
        minute = ve_date[4]
        second = ve_date[5]
        return f"{year}-{month:>02}-{day}T{hour:>02}:{minute:>02}:{second:>02}"

    def get_solar_longitude(self, year, alt=True):
        gc = GregorianCalendar()
        tee = gc.fixed_from_gregorian((year, 3, 20))
        return (self.alt_solar_longitude(tee) if alt
                else self.solar_longitude(tee))


if __name__ == "__main__":
    tve = TestVernalEquinox()
    # All date formats
    data = tve.generate_curve()
    #pprint.pprint(data)

    # Date as seperate fields
    #items = [values[2] for year, values in data.items()]
    #[print(f"{Y} {M} {D} {h} {m} {s}") for Y, M, D, h, m, s in items]

    # R.D. dates only
    [print(values[0]) for year, values in data.items()]

    # Solar Longitude Season
    #[print(values[4]) for year, values in data.items()]
