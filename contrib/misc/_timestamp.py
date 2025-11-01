# -*- coding: utf-8 -*-
#
# badidatetime/_timestamp.py
#

import math
from datetime import datetime, timezone, timedelta


class TimestampUtils:

    def solar_declination(self, day_of_year):
        """
        Return solar declination (radians) using approximate formula.
        """
        return math.radians(23.44) * math.sin(math.radians(
            360 / 365 * (day_of_year - 81)))

    def equation_of_time(self, day_of_year):
        """
        Return the equation of time (minutes).
        """
        B = math.radians(360 / 365 * (day_of_year - 81))
        return 9.87 * math.sin(2 * B) - 7.53 * math.cos(B) - 1.5 * math.sin(B)

    def solar_events(self, timestamp, latitude, longitude):
        """
        Calculate solar noon, sunrise, and sunset for the given timestamp
        and coordinates. All results are UTC datetimes.

        Args:
            timestamp: POSIX timestamp (seconds since epoch)
            latitude: latitude in decimal degrees (positive north)
            longitude: longitude in decimal degrees (positive east,
                                                     negative west)

        Returns:
            dict with keys: 'solar_noon', 'sunrise', 'sunset' (UTC datetimes)
        """
        # Convert timestamp to date (UTC)
        dt = datetime.fromtimestamp(timestamp, tz=timezone.utc)
        n = dt.timetuple().tm_yday
        decl = self.solar_declination(n)
        # Equation of time (minutes)
        eqt = self.equation_of_time(n)
        # Solar noon (minutes from UTC midnight)
        solar_noon_min = 720 - 4 * longitude - eqt
        # Hour angle for sunrise/sunset (degrees)
        lat_r = math.radians(latitude)
        decl_r = decl

        try:
            h0 = math.degrees(math.acos(
                (math.sin(math.radians(-0.833)) - math.sin(lat_r) * math.sin(
                    decl_r)) / (math.cos(lat_r) * math.cos(decl_r))
                ))
        except ValueError:
            # Sun never rises or never sets (polar regions)
            return {"solar_noon": None, "sunrise": None, "sunset": None}

        # Sunrise and sunset times (minutes from UTC midnight)
        sunrise_min = solar_noon_min - 4 * h0
        sunset_min = solar_noon_min + 4 * h0
        base_date = datetime(dt.year, dt.month, dt.day, tzinfo=timezone.utc)
        solar_noon = base_date + timedelta(minutes=solar_noon_min)
        sunrise = base_date + timedelta(minutes=sunrise_min)
        sunset = base_date + timedelta(minutes=sunset_min)
        return {"solar_noon": solar_noon, "sunrise": sunrise, "sunset": sunset}

    def timestamp_at_sunset(self, date: tuple, latitude: float,
                            longitude: float):
        ts = datetime(*date, tzinfo=timezone.utc).timestamp()
        events = self.solar_events(ts, latitude, longitude)
        sunset_utc = events["sunset"]
        # Get POSIX timestamp at sunset (UTC)
        return sunset_utc.timestamp()
