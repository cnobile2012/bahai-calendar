# -*- coding: utf-8 -*-
#
# badidatetime/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math

from badidatetime.base_calendar import BaseCalendar
from badidatetime.gregorian_calendar import GregorianCalendar
from badidatetime._coefficients import Coefficients

__all__ = ('BahaiCalendar',)


class BahaiCalendar(BaseCalendar, Coefficients):
    """
    Implementation of the Baha'i (Badi) Calendar.

    | WGS84--https://coordinates-converter.com/
    | https://whatismyelevation.com/location/35.63735,51.72569/Tehran--Iran-
    | https://en-us.topographic-map.com/map-g9q1h/Tehran/?center=35.69244%2C51.19492
    | https://www.google.com/maps/place/Tehran,+Tehran+Province,+Iran/@35.9098957,51.51371,9.49z/data=!4m6!3m5!1s0x3f8e02c69b919039:0x17c26479772c5928!8m2!3d35.6891975!4d51.3889736!16s%2Fm%2F025zk75?entry=ttu
    | https://gml.noaa.gov/grad/solcalc/ Sunset data
    """
    # Near Mehrabad International Airport
    #                 latitude    longitude  zone IANA name      elevation
    _BAHAI_LOCATION = (35.69435, 51.288701, 3.5, 'Asia/Tehran', 0)
    _GMT_LOCATION = (51.477928, -0.001545, 0, 0)
    _BADI_EPOCH = 2394643.262681068  # 2394645.262681068 using Meeus' algorithm
    _BADI_MONTH_NUM_DAYS = [
        (1, 19), (2, 19), (3, 19), (4, 19), (5, 19), (6, 19), (7, 19),
        (8, 19), (9, 19), (10, 19), (11, 19), (12, 19), (13, 19), (14, 19),
        (15, 19), (16, 19), (17, 19), (18, 19), (0, 0), (19, 19)
        ]
    KULLISHAY_MIN = -5
    """
    int: Constant indicating the minimum Kull-i-shay the API supports.
    """
    KULLISHAY_MAX = 4
    """
    int: Constant indicating the maximum Kull-i-shay the API supports.
    """
    MINYEAR = -1842
    """
    int: Constant indicating the minimum year this API can represent.
    """
    MAXYEAR = 1161
    """
    int: Constant indicating the maximum year this API can represent.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        """
        kull_i_shay: 361-year (19^2) vahid (integer)
        vahid: (integer) 19-year vahid
        year: (integer) 1 - 19
        month: (integer) 1 - 19 plus 0 for Ayyām-i-Hā
        day: (integer) 1 - 19
        Baha'i long form date: [kull_i_shay, vahid, year, month, day]
        """
        self._bahai_date = None
        self._gc = GregorianCalendar()

    def utc_sunset(self, date: tuple, lat: float=None, lon: float=None,
                   zone: float=None) -> tuple:
        """
        Return the time of sunset in UTC time for the given Badi Day.

        :param tuple date: A Badi date.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The time zone.
        :returns: The hour, minute, and second of sunset based on the
                 provided coordinates.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date(date[:3], lat, lon, zone)
        return self._hms_from_decimal_day(jd + 0.5)

    def naw_ruz_g_date(self, year: int, lat: float=None, lon: float=None,
                       zone: float=None, *, hms: bool=False) -> tuple:
        """
        Return the Badi date for Naw-Ruz from the given Badi year.

        :param int year: A Badi year.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The time zone.
        :param bool hms: If True the output returns the hours, minutes, and
                         seconds as separate fields. If False the day has a
                         decimal value indicating the hours, minutes, and
                         seconds.
        :returns: A Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        return self._gc.gregorian_date_from_jd(jd, hms=hms, exact=True)

    def first_day_of_ridvan_g_date(self, year: int, lat: float=None,
                                   lon: float=None, zone: float=None, *,
                                   hms: bool=False) -> tuple:
        """
        Find the first day of Riḍván either with or without hours, minutes,
        and seconds. If the latitude, longitude, and time zone are not given
        Riḍván time of day is determined for the city of Nur in Iran.

        :param int year: A Badi year.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The time zone.
        :param bool hms: If True the output returns the hours, minutes, and
                         second as separate fields. If False the day has a
                         decimal value indicating the hours, minutes, and
                         seconds.
        :returns: A Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date((year, 2, 13), lat, lon, zone)
        return self._gc.gregorian_date_from_jd(jd, hms=hms, exact=True)

    def jd_from_badi_date(self, b_date: tuple, lat: float=None,
                          lon: float=None, zone: float=None) -> float:
        """
        Convert a Badí' short form date to Julian period day.

        :param tuple b_date: A short form Badí' date.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The time zone.
        :returns: The Julian Period day.
        :rtype: float
        """
        self._check_valid_badi_date(b_date, short_in=True)
        year, month, day = b_date[:3]
        hh, mm, ss, us = self._get_hms(b_date, short_in=True)

        if month == 0:    # Ayyam-i-Ha
            days = 18 * 19
        elif month < 19:  # month 1 - 18
            days = (month - 1) * 19
        else:             # month 19
            days = 18 * 19 + 4 + self._is_leap_year(year)

        td = self._days_in_years(year - 1)
        jd = td + math.floor(self._BADI_EPOCH + 1) + days + day

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self._BAHAI_LOCATION[:3]

        # The day may have a decimal component. ex. 1.5 = (1 day and 12 hours)
        # This day is relative to UTC time, so we need to compensate for Badi
        # time since a Badi day starts at sunset not at midnight.
        jd0 = self._meeus_from_exact(jd)
        coeff = self._get_day_coeff(year)
        jd1 = jd0 + coeff
        jd_ss = self._sun_setting(jd1, lat, lon)
        local_ss = self._local_zone_correction(jd_ss, zone, mod_jd=True)
        a_ss = self._exact_from_meeus(local_ss)
        day_frac = self._decimal_day_from_hms(hh, mm, ss, us)
        f_coeff = self._get_frac_coeff(year)
        # print(f"{str(b_date):<15} {day:<9} {jd:<14} {local_ss:<20} "
        #       f"{a_ss:<20} {day_frac} {coeff}")
        return round(a_ss + day_frac + f_coeff, self._ROUNDING_PLACES)

    def badi_date_from_jd(self, jd: float, lat: float=None, lon: float=None,
                          zone: float=None, *, us: bool=False,
                          short: bool=False, fraction: bool=False,
                          trim: bool=False, rtd: bool=False) -> tuple:
        """
        Convert a Julian Period day to a Badi date.

        :param float jd: Julian Period day in the Astronomically correct
                         method and in UT time.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :param bool us: If True the seconds are split to seconds and
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool fraction: This will return a short date with a possible
                              fraction on the day.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool rtd: Round to day.
        :returns: The Badi date from an Astronomically correct Julian
                  Period day.
        :rtype: tuple
        """
        assert self._xor_boolean((fraction, us, rtd)), (
            "Cannot set more than one of fraction, us, or rtd to True.")

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self._BAHAI_LOCATION[:3]

        md = jd - self._BADI_EPOCH
        # This is only needed for the last two days of Badi year 1161
        # and the day before the epoch.
        y = 1 if md < 424046 and md != 0 else 0
        # Find the year
        year = math.floor(md / self._MEAN_TROPICAL_YEAR) + y
        ld = 4 + self._is_leap_year(year)
        # Get 1st day of year so we can find the number of days to the JD.
        fdoy = (year, 1, 1)
        fjdoy = self.jd_from_badi_date(fdoy, lat, lon, zone)

        # Fix year if needed.
        yr = year - 1 if (math.floor(fjdoy) - math.floor(jd)) > 0 else year

        if yr != year:
            year = yr
            fdoy = (year, 1, 1)
            fjdoy = self.jd_from_badi_date(fdoy, lat, lon, zone)
            ld = 4 + self._is_leap_year(year)

        days = math.floor(jd) - math.floor(fjdoy) + 1

        if days <= 342:                 # Month 1 - 18
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= 366:  # Month 19
            day = days - (342 + ld)
        else:                           # Ayyam-i-Ha
            day = days % 342

        month_days = list(self._BADI_MONTH_NUM_DAYS)
        month_days[18] = (0, ld)  # Fix Ayyám-i-Há days

        for month, ds in month_days:
            if days <= ds: break
            days -= ds

        self._debug_print(
            "Stage 0--jd: {}, date: {}, days: {}, fjdoy: {}, md: {}, y: {}, "
            "ld: {}", (jd, (year, month, day), days, fjdoy, md, y, ld))
        year, month, day, frac = self._adjust_date(jd, year, month, day,
                                                   lat, lon, zone)

        if fraction:
            b_date = year, month, round(day + frac, 6)
        elif rtd:
            day = round(day + frac)
            b_date = year, month, day

            if not short:
                b_date = self.long_date_from_short_date(b_date, trim=trim)
            assert frac < 1.0, (
                "If this assertion fires send this entire message to the "
                f"developer: jd: {jd}, day: {day}, frac: {frac}, "
                f"b_date: {b_date}")
        else:
            trim = trim if us else True
            date = year, month, day, *self._hms_from_decimal_day(frac)
            l_date = self.long_date_from_short_date(date, trim=True)
            b_date = self.kvymdhms_from_b_date(l_date, us=us, short=short,
                                               trim=trim)

        return b_date

    def _adjust_date(self, jd, year, month, day, lat, lon, zone):
        """
        Adjust the date based on the latitude, longitude, and zone. The JD
        value is in local time so all JDs that are compared to it must also
        be in local time.

        :param float jd:
        :param int year:
        :param int, month:
        :param int day:
        :param float lat:
        :param float lon:
        :param float zone:
        :returns: The year, month, day, frag in a tuple.
        :rtype: tuple
        """
        def day_before(year, month, day):
            if day <= 0:
                if 2 <= month <= 18:  # Months 2 - 18 -> to previous month
                    self._debug_print("Day Before 1: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    month -= 1
                    day = 19
                elif month == 19:  # Month 19 -> Ayyám-i-Há
                    self._debug_print("Day Before 2: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    month = 0
                    day = 4 + self._is_leap_year(year)
                elif month == 0:  # Ayyám-i-Há
                    self._debug_print("Day Before 3: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    month = 18
                    day = 19
                else:  # Month 1 -> Month 19 & year to previous year
                    self._debug_print("Day Before 4: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    year -= 1
                    month = 19
                    day = 19

            return year, month, day

        def day_after(year, month, day):
            dim = 4 + self._is_leap_year(year) if month == 0 else 19

            if day > dim:
                if 1 <= month <= 17:
                    self._debug_print("Day After 1: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    month += 1
                    day = 1
                elif month == 18:
                    self._debug_print("Day After 2: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    month = 0
                    day = 1
                elif month == 0:
                    self._debug_print("Day After 3: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    month = 19
                    day = 1
                else:  # Month 19
                    self._debug_print("Day After 4: jd: {}, year: {}, month: "
                                      "{}, day: {}", (jd, year, month, day))
                    year += 1
                    month = 1
                    day = 1

            return year, month, day

        jd0, jd_frac = divmod(jd, 1)
        jd1 = self._meeus_from_exact(jd0)
        ss0 = self._sun_setting(jd1, lat, lon)
        ss_frac = round(self._local_zone_correction(ss0, zone),
                        self._ROUNDING_PLACES)

        if (jd_frac + 0.5) % 1 < (ss_frac + 0.5) % 1:
            ss1 = self._sun_setting(jd1 - 1, lat, lon)
            ss_frac1 = round(self._local_zone_correction(ss1, zone),
                             self._ROUNDING_PLACES)
            # Calculate the time between the previous sunset and the
            # following midnight of the JD day then add the results to
            # the JD day fraction to get the Badi time.
            frac = (1 - ss_frac1 + jd_frac) % 1
            # Get the original GMT by reversing the time zone value.
            o_jd = self._local_zone_correction(jd, zone, inverse=True,
                                               mod_jd=True)
            oi_jd, o_jd_f = divmod(o_jd, 1)
            ni_jd, n_jd_f = divmod(jd, 1)

            if not (oi_jd == ni_jd and (o_jd_f < 0.5 and n_jd_f >= 0.5) or
                    (o_jd_f == n_jd_f)):
                day -= 1
                self._debug_print(
                    "Stage 1--jd: {}, jd1: {}, date: {}, ss1: {}, "
                    "jd_frac: {}, ss_frac: {}, ss_frac1: {}, frac: {}",
                    (jd, jd1, (year, month, day), ss1, jd_frac, ss_frac,
                     ss_frac1, frac))
            else:
                self._debug_print(
                    "Stage 2--jd: {}, jd1: {}, date: {}, ss1: {}, "
                    "jd_frac: {}, ss_frac: {}, ss_frac1: {}, frac: {}",
                    (jd, jd1, (year, month, day), ss1, jd_frac, ss_frac,
                     ss_frac1, frac))
        else:
            diff = ss_frac - jd_frac
            dl = self._day_length(jd - 1, lat, lon, decimal=True)
            frac = round(dl - diff, self._ROUNDING_PLACES) % 1
            self._debug_print(
                "Stage 3--jd: {}, jd0: {}, date: {}, ss0: {}, jd_frac: {}, "
                "ss_frac: {}, diff: {}, frac: {}",
                (jd, jd0, (year, month, day), ss0, jd_frac, ss_frac, diff,
                 frac))

        year, month, day = day_before(year, month, day)
        year, month, day = day_after(year, month, day)
        return year, month, day, frac

    def short_date_from_long_date(self, b_date: tuple, *, trim: bool=False,
                                  ) -> tuple:
        """
        Convert a long date (kvymdhms) to a short date (ymdhms). In either
        case microseconds could also be provided.

        :param tuple b_date: A long form date with or without microseconds.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :returns: The short form Badi date.
        :rtype: tuple
        """
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hh, mm, ss, us = self._get_hms(b_date)
        y = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        dm = 19 if month in range(1, 20) else 4 + self._is_leap_year(y)

        # The ShortFormStruct and LongFormStruct accept seconds up to 61.
        if ss >= 60:
            mm += 1
            ss -= 1

        if mm >= 60:
            hh += 1
            mm -= 1

        if hh >= 24:
            day += 1
            hh -= 1

        if day > dm:
            if dm in (4, 5):
                month = 19
                day = 1
            else:
                month += 1
                day -= 1

        if month > 19:
            month = 1
            y += 1

        hmsms = self._trim_hms((hh, mm, ss, us)) if trim else (hh, mm, ss, us)
        date = (y, month, day) + hmsms
        self._check_valid_badi_date(date, short_in=True)
        return date

    def long_date_from_short_date(self, date: tuple, *, trim: bool=False,
                                  ) -> tuple:
        """
        Convert a date to a short date (ymdhms) to a long date (kvymdhms).

        :param tuple b_date: A short form date with or without microseconds.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :returns: The long form Badi date.
        :rtype: tuple
        """
        y, month, day = date[:3]
        hh, mm, ss, us = self._get_hms(date, short_in=True)
        k = y / 361
        kull_i_shay = 0 if y == 0 else math.ceil(k)
        k0 = self._truncate_decimal(k % 1, self._ROUNDING_PLACES)
        v = k0 / 19 * 361

        if v == 0:  # If there is no fraction in v
            vahid = 19
            year = 19
        else:
            vahid = math.ceil(v)
            year = math.ceil(v % 1 * 19)

        hmsms = self._trim_hms((hh, mm, ss, us)) if trim else (hh, mm, ss, us)
        b_date = (kull_i_shay, vahid, year, month, day) + hmsms
        self._check_valid_badi_date(b_date)
        return b_date

    def date_from_kvymdhms(self, b_date: tuple, *, short: bool=False) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second,
        us) into a (Kull-i-Shay, Váḥid, year, month, day.fraction) or
        (year, month, day.fraction) date.

        :param tuple b_date: The Badi date in long form.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :returns: The long or short form Badi date with hours, minutes,
                  seconds, and microseconds if set.
        :rtype: tuple
        """
        self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second, us = self._get_hms(b_date)
        day += round(self._HR(hour) + self._MN(minute) + self._SEC(second) +
                     self._US(us), 6)  # Round to 6 decimal places
        date = (kull_i_shay, vahid, year, month, day)
        return (self.short_date_from_long_date(
            date, trim=True) if short else date)

    def kvymdhms_from_b_date(self, b_date: tuple, *, us: bool=False,
                             short: bool=False, trim: bool=False) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day.fraction) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second) or if
        short is True (year, month, day, hour, minute, second). If us is
        True the seconds are split to second and microsecond.

        :param tuple b_date: The Badi date in long form.
        :param bool us: If True the seconds are split to seconds and
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :returns: The long or short form Badi date with hours, minutes,
                  seconds, and microseconds if set.
        :rtype: tuple
        """
        dlen = len(b_date)

        # We need to trim any zero hh, mm, ss, us so partial days below
        # work correctly.
        if dlen > 5:
            hms = self._trim_hms(b_date[5:dlen])
            b_date = b_date[:5] + hms
            dlen = len(b_date)

        self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]

        if dlen == 5:
            hd = self._PARTIAL_DAY_TO_HOURS(day)
            hour = math.floor(hd)
            md = self._PARTIAL_HOUR_TO_MINUTE(hd)
            minute = math.floor(md)
            # Round to 6 decimal places
            second = round(self._PARTIAL_MINUTE_TO_SECOND(md), 6)
        else:
            hour = b_date[5] if dlen > 5 else 0
            minute = b_date[6] if dlen > 6 else 0
            second = b_date[7] if dlen > 7 else 0

        date = (kull_i_shay, vahid, year, month, math.floor(day))

        if us:
            hmsms = (hour, minute, *self._sec_microsec_from_seconds(second))
        else:
            hmsms = (hour, minute, second)

        date += self._trim_hms(hmsms) if trim else hmsms
        return (self.short_date_from_long_date(date, trim=trim)
                if short else date)

    def badi_date_from_gregorian_date(self, g_date: tuple, lat: float=None,
                                      lon: float=None, zone: float=None, *,
                                      us: bool=False, short: bool=False,
                                      trim: bool=False, rtd: bool=False,
                                      _exact: bool=True) -> tuple:
        """
        Get the Badi date from the Gregorian date.

        :param tuple g_date: A Gregorian date.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :param bool us: If True the seconds are split to seconds amd
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool rtd: Round to day.
        :param bool _exact: Use the more exact Julian Period algorithm.
                            Default is True. This should generally be set to
                            True, a False value will give inaccurate results
                            and is used for testing only.
        :returns: A Badi date long or short form.
        :rtype: tuple
        """
        jd = self._gc.jd_from_gregorian_date(g_date, exact=_exact)
        return self.badi_date_from_jd(jd, lat=lat, lon=lon, zone=zone, us=us,
                                      short=short, trim=trim, rtd=rtd)

    def gregorian_date_from_badi_date(self, b_date: tuple, lat: float=None,
                                      lon: float=None, zone: float=None, *,
                                      us: bool=False, _exact: bool=True,
                                      ) -> tuple:
        """
        Get the Gregorian date from the Badi date.

        :param tuple b_date: A Badi date short form.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :param bool us: If True the seconds are split to seconds amd
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool _exact: Use the more exact Julian Period algorithm.
                            Default is True. This should generally be set to
                            True, a False value, will give inaccurate results
                            and is used for testing only.
        :returns: The Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date(b_date, lat, lon, zone)
        return self._gc.gregorian_date_from_jd(jd, hms=True, us=us,
                                               exact=_exact)

    def badi_date_from_timestamp(self, t: float, zone: float=None, *,
                                 us: bool=False, short: bool=False,
                                 trim: bool=False, rtd: bool=False) -> tuple:
        """
        Get the Badi date from a POSIX timestamp.

        :param float t: Timestamp
        :param float zone: The time zone.
        :param bool us: If True the seconds are split to seconds and
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool rtd: Round to day.
        :returns: A Badi date long or short form.
        :rtype: tuple
        """
        jd = t / 86400 + self._POSIX_EPOCH
        jd += zone / 24
        return self.badi_date_from_jd(jd, *self._GMT_LOCATION[:3], us=us,
                                      short=short, trim=trim, rtd=rtd)

    def timestamp_from_badi_date(self, date: tuple, zone: float) -> float:
        """
        Convert a Badi date to a timestamp.

        :param tuple date: The Badi date.
        :param float zone: The time zone.
        :returns: The timestamp corrected for the time zone.
        :rtype: float
        """
        jd = self.jd_from_badi_date(date, *self._GMT_LOCATION[:3])
        jd -= zone / 24
        return round((jd - self._POSIX_EPOCH) * 86400, self._ROUNDING_PLACES)

    def midday(self, date: tuple, *, hms: bool=False, _short: bool) -> tuple:
        """
        Find the midday time in hours, minutes, and seconds with fraction.
        All calculations are done in GMT.

        :param tuple date: Badi date short or long.
        :param bool hms: If True return the hours, minutes, and seconds else
                         if False return the decimal value.
        :param bool _short: Indicates the incoming date format.
        :returns: Midday in hours, minutes, and seconds.
        :rtype: tuple
        """
        if not _short:
            b_date = self.short_date_from_long_date(date, trim=True)
        else:
            b_date = date

        jd = self.jd_from_badi_date(b_date)
        jd1 = self._meeus_from_exact(jd)
        ss0 = self._sun_setting(jd1, *self._GMT_LOCATION[:2])
        jd2 = self._meeus_from_exact(jd + 1)
        ss1 = self._sun_setting(jd2, *self._GMT_LOCATION[:2])
        ut_mid = (ss1 - ss0) / 2
        local_mid = self._local_zone_correction(ut_mid, self._GMT_LOCATION[2])
        return self._hms_from_decimal_day(local_mid) if hms else local_mid

    def _trim_hms(self, hms: tuple) -> tuple:
        """
        Trim the hours, minutes, seconds or microseconds off the date if
        zero unless a lower value was not zero.

        .. list-table:: Examples
           :widths: 18 16 66
           :header-rows: 1

           * - Examples
             - Results
             - Description
           * - (12, 30, 6, 0)
             - (12, 30, 6)
             - The zero microseconds would be trimmed.
           * - (12,  0, 6, 0)
             - (12, 0, 6)
             - The zero microseconds would be trimmed but the zero minutes
               would be left untouched.

        :param tuple hms: An hour, minute, and second object.
        :returns: An object with the lower order parts stripped off if
                  they have a zero value.
        :rtype: tuple
        """
        items = []
        has = False

        for v in reversed(hms):
            if v == 0 and not has:
                continue
            else:
                items.append(v)
                has = True

        return tuple(reversed(items))

    def _check_valid_badi_date(self, b_date: tuple, short_in: bool=False
                               ) -> None:
        """
        Check that the Kull-i-Shay, Váḥids, year, month, day, hour, minute,
        second, and microsecond values are valid.

        :param tuple b_date: A long form Badi date.
        :param bool short_in: If True then parse for a short date else if
                              False parse for a long date. This is for
                              incoming dates not outgoing dates as in most
                              other uses of 'short'.
        :returns: Nothing
        :rtype: None
        :raises AssertionError: When a date Váḥid, year, month, day, hour,
                                minute, second, or microsecond are out of
                                range.
        """
        cycle = 20

        if not short_in:  # Long Badi date
            kull_i_shay, vahid, year, month, day = b_date[:5]
            hour, minute, second, us = self._get_hms(b_date)
            assert (self.KULLISHAY_MIN-1 <= kull_i_shay
                    <= self.KULLISHAY_MAX+1), (
                f"Invalid kull-i-shay {kull_i_shay}, it must be in the range "
                f"of [{self.KULLISHAY_MIN}, {self.KULLISHAY_MAX}].")
            assert 1 <= vahid < cycle, (
                f"Invalid Váḥids '{vahid}' in a Kull-i-Shay’, it must be in "
                "the range of [1, 19].")
            assert 1 <= year < cycle, (
                f"Invalid year '{year}' in a Váḥid, it must be in the "
                "range of [1, 19].")
            ly = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        else:  # Short Badi date
            year, month, day = b_date[:3]
            hour, minute, second, us = self._get_hms(b_date, short_in=True)
            assert self.MINYEAR-1 <= year <= self.MAXYEAR+1, (
                f"Invalid year '{year}' it must be in the range of ["
                f"{self.MINYEAR}, {self.MAXYEAR}].")
            ly = year

        assert 0 <= month < cycle, (
            f"Invalid month '{month}', it must be in the range of [0, 19].")
        # This is Ayyām-i-Hā and could be 4 or 5 days depending on leap year.
        cycle = (5 + self._is_leap_year(ly)) if month == 0 else cycle
        assert 1 <= day < (cycle), (
            f"Invalid day '{day}' for month '{month}', it must be in the "
            f"range of [1, {cycle-1}].")
        self._check_valid_badi_time(hour, minute, second, us)

        # Check if there are any fractionals that invalidate other values.
        if any((hour, minute, second)):
            assert not day % 1, (
                "If there is a part day then there can be no hours, minutes, "
                "or seconds.")

        if any((minute, second)):
            assert not hour % 1, ("If there is a part hour then there can "
                                  "be no minutes or seconds.")

        if second:
            assert not minute % 1, (
                "If there is a part minute then there can be no seconds.")

    def _check_valid_badi_time(self, hour: float, minute: float, second: float,
                               us: int, maxsec: int=60) -> None:
        """
        Check that the hour, minute, second, and microsecond values are valid.

        :param float hour: Hours
        :param float minute: Minutes
        :param float second: Seconds
        :param float us: Microseconds
        :returns: Nothing
        :rtype: None
        :raises AssertionError: When an hour, minute, second, or microsecond
                                are out of range.
        """
        assert 0 <= hour < 25, (
            f"Invalid hour '{hour}', it must be in the range of [0, 24].")
        assert 0 <= minute < 60, (
            f"Invalid minute '{minute}', it must be in the range of [0, 59].")
        assert 0 <= second < maxsec, (
            f"Invalid second '{second}', it must be in the range of "
            f"[0, {maxsec}].")
        assert 0 <= us < 1000000, (
            f"Invalid microseconds '{us}', it must be in the range of "
            "[0, 999999].")

    def _is_leap_year(self, year: int) -> bool:
        """
        Return a Boolean True if a Badí' leap year, False if not.

        :param int year: This value must be a Badí' short form year.
        :returns: A Boolean indicating if a leap year or not.
        :rtype: bool
        """
        return self._days_in_year(year) == 366

    def _days_in_year(self, year: int) -> int:
        """
        Determine the number of days in the provided Badi year.

        :param int year: The Badi year to process.
        :returns: The number of days.
        :rtype: int
        """
        jd_n0 = self.jd_from_badi_date((year, 1, 1))
        # For year 1162 we need to turn off the date check so we can get
        # the leap year for 1161.
        jd_n1 = self.jd_from_badi_date((year + 1, 1, 1))
        return int(math.floor(jd_n1) - math.floor(jd_n0))

    def _get_hms(self, date: tuple, *, short_in: bool=False) -> tuple:
        """
        Parse the hours, minutes, seconds, and microseconds, if they exist
        for either the short or long form Badi date.

        :param tuple date: A long or short form Badi date.
        :param bool short_in: If True then parse for a short date else if False
                              parse for a long date. This is for incoming dates
                              not outgoing dates as in most other uses of
                              'short'.
        :returns: The relevant hours, minutes, and seconds.
        :rtype: tuple
        """
        t_len = len(date)
        s = 3 if short_in else 5
        hour = date[s] if t_len > s and date[s] is not None else 0
        minute = date[s+1] if t_len > s+1 and date[s+1] is not None else 0
        second = date[s+2] if t_len > s+2 and date[s+2] is not None else 0
        us = date[s+3] if t_len > s+3 and date[s+3] is not None else 0
        return hour, minute, second, us

    def _day_length(self, jd: float, lat: float, lon: float, *,
                    decimal: bool=False) -> tuple:
        """
        The hour, minute, and seconds of the day's offset either less than
        or more than 24 hours.

        :param float jd: The astronomically exact Julian Period day.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param bool decimal: If `False` (default) return HH:MM:SS else if
                             `True` return a decimal number.
        :returns: The hour, minute, and second or a decimal number.
        :rtype: tuple or float
        """
        jd0 = math.floor(jd)
        jd1 = jd0 + 1
        # The next day
        jd1 = self._meeus_from_exact(jd1)
        ss1 = self._sun_setting(jd1, lat, lon)
        # The first day
        jd0 = self._meeus_from_exact(jd0)
        ss0 = self._sun_setting(jd0, lat, lon)
        # Subtract the first day from the next day given the total
        # hours, minutes, and seconds between them.
        ut_ss = ss1 - ss0

        if decimal:
            ret = round(ut_ss, self._ROUNDING_PLACES)
        else:
            value = list(self._hms_from_decimal_day(ut_ss))
            value[0] = 24 if value[0] == 0 else value[0]
            ret = tuple(value)

        return ret

    def utc_to_badi_time(self, jd: float, lat: float, lon: float, zone: float
                         ) -> float:
        """
        Convert UTC time to Badi time.
        """
        jd += zone / 24
        jd0, jd0_frac = divmod(jd, 1)
        ss = self._sun_setting(self._meeus_from_exact(jd), lat, lon)

        if jd0_frac < ss % 1:  # The day before.
            ss = self._sun_setting(jd0 - 1, lat, lon)
            time_to_midnight = 0.5 - ss % 1
            frac = time_to_midnight + jd0_frac
        else:  # The day of.
            frac = jd_frac - ss % 1

        return jd0 + frac

    def badi_to_utc_time(self, jd: float, lat: float, lon: float, zone: float
                         ) -> float:
        """
        Convert Badi time to UTC time.
        """
        jd0, jd0_frac = divmod(jd, 1)
        ss = self._sun_setting(self._meeus_from_exact(jd), lat, lon)
        
