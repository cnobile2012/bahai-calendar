# -*- coding: utf-8 -*-
#
# badidatetime/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math

from badidatetime.base_calendar import BaseCalendar
from badidatetime.gregorian_calendar import GregorianCalendar

__all__ = ('BahaiCalendar',)


class BahaiCalendar(BaseCalendar):
    """
    Implementation of the Baha'i (Badi) Calendar.

    | WGS84--https://coordinates-converter.com/
    | https://whatismyelevation.com/location/35.63735,51.72569/Tehran--Iran-
    | https://en-us.topographic-map.com/map-g9q1h/Tehran/?center=35.69244%2C51.19492
    | https://www.google.com/maps/place/Tehran,+Tehran+Province,+Iran/@35.9098957,51.51371,9.49z/data=!4m6!3m5!1s0x3f8e02c69b919039:0x17c26479772c5928!8m2!3d35.6891975!4d51.3889736!16s%2Fm%2F025zk75?entry=ttu
    | https://gml.noaa.gov/grad/solcalc/ Sunset data
    """
    # Near Mehrabad International Airport
    #                 lattude    longitude  zone IANA name      elevation
    _BAHAI_LOCATION = (35.682376, 51.285817, 3.5, 'Asia/Tehran', 0)
    _GMT_LOCATION = (51.477928, -0.001545, 0, 0)
    _BADI_EPOCH = 2394643.5  # 2394645.5 using Meeus' algorithm
    _BADI_MONTH_NUM_DAYS = [
        (1, 19), (2, 19), (3, 19), (4, 19), (5, 19), (6, 19), (7, 19),
        (8, 19), (9, 19), (10, 19), (11, 19), (12, 19), (13, 19), (14, 19),
        (15, 19), (16, 19), (17, 19), (18, 19), (0, 0), (19, 19)
        ]
    KULL_I_SHAY_MIN = -5
    """
    Minimum kull_i_shay supported
    """
    KULL_I_SHAY_MAX = 4
    """
    Maximum kull_i_shay supported
    """
    MINYEAR = -1842
    """
    Minimum year supported
    """
    MAXYEAR = 1161
    """
    Maximum year supported
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
        :return: The hour, minute, and second of sunset based on the
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
                         seconds as seperate fields. If False the day has a
                         decimal value indicating the hours, minutes, and
                         seconds.
        :return: A Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        date = self._gc.gregorian_date_from_jd(jd, exact=True)
        return self._gc.ymdhms_from_date(date) if hms else date

    def first_day_of_ridvan_g_date(self, year: int, lat: float=None,
                                   lon: float=None, zone: float=None, *,
                                   hms: bool=False) -> tuple:
        """
        Find the first day of Riḍván in either with or without hours,
        minutes, and seconds.
        If the latitude, longitude, and time zone are not given Riḍván time
        of day is determined for the city of Nur in Iran.

        :param int year: A Badi year.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The time zone.
        :param bool hms: If True the output returns the hours, minutes, and
                         second as seperate fields. If False the day has a
                         decimal value indicating the hours, minutes, and
                         seconds.
        :return: A Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date((year, 2, 13), lat, lon, zone)
        date = self._gc.gregorian_date_from_jd(jd, exact=True)
        return self._gc.ymdhms_from_date(date) if hms else date

    def jd_from_badi_date(self, b_date: tuple, lat: float=None,
                          lon: float=None, zone: float=None,
                          _chk_on: bool=True) -> float:
        """
        Convert a Badi short form date to Julian period day.

        :param tuple b_date: A short form Badi date.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The time zone.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: The Julian Period day.
        :rtype: float
        """
        year, month, day = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date, trim=True, _chk_on=_chk_on),
            short=True, _chk_on=_chk_on)

        if month == 0:  # Ayyam-i-Ha
            days = 18 * 19
        elif month < 19:  # month 1 - 18
            days = (month - 1) * 19
        else:  # month == 19:
            days = 18 * 19 + 4 + self._is_leap_year(year, _chk_on=_chk_on)

        td = self._days_in_years(year-1)
        jd = td + math.floor(self._BADI_EPOCH+1) + days + day

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self._BAHAI_LOCATION[:3]

        # The diff value converts the more exact jd to the Meeus algorithm
        # for determining the sunset jd. The fractional on the day is not
        # affected.
        diff = self._meeus_from_exact(jd)
        ss_a = self._sun_setting(jd + diff, lat, lon, zone) % 1
        return round(jd + ss_a + self._get_coff(year), self._ROUNDING_PLACES)

    def _get_coff(self, year: int) -> int:
        """
        Generate the coefficients for correcting Badí' vernal equinox dates.

        .. note::

           | General ranges are determined with:
           | ./contrib/misc/badi_jd_tests.py -p -S start_year -E end_year

           Where -S is the 1st year and -E is the nth year + 1 that needs to
           be process. Use the following command to test the results of each
           segment.
           ./contrib/misc/badi_jd_tests.py -qXS start_year -E end_year

           Full range is -1842 to 1161.

        :param int year: The year to find a coefficient for.
        :return: The coefficient.
        :rtype: int
        """
        def process_segment(y, a=0, onoff0=(), b=0, onoff1=()):
            func = lambda y, onoff: 0 < y < 100 and y % 4 in onoff
            coff = 0

            if a and func(y, onoff0):    # Whatever is passed in onoff0.
                coff = a
            elif b and func(y, onoff1):  # Whatever is passed in onoff1.
                coff = b

            return coff

        def process_segments(year, pn, a=0, onoff0=(), b=0, onoff1=()):
            coff = 0

            for start, end in pn:
                if year in range(start, end):
                    # Start to end (range -S start -E end)
                    coff0 = process_segment(end - year, a=a, onoff0=onoff0)
                    coff1 = process_segment(end - year, b=b, onoff1=onoff1)
                    coff = coff0 if coff0 != 0 else coff1

            return coff

        p1 = ((-1783, -1747), (-1651, -1615), (-1499, -1483), (-1383, -1347),
              (-1251, -1215), (-1099, -1083), (-983, -947), (-851, -815),
              (-699, -683), (-583, -547), (-451, -415), (-299, -283),
              (-179, -143), (-47, -11), (101, 117), (213, 249), (345, 381),
              (501, 513), (609, 645), (741, 777), (901, 909), (1005, 1041),
              (1137, 1162))
        p1100 = ((-1699, -1683), (-1299, -1283), (-899, -883), (-499, -483),
                 (-99, -79), (301, 313), (701, 709), (1101, 1105))
        p1110 = ((-1799, -1783), (-1683, -1651), (-1399, -1383),
                 (-1283, -1251), (-999, -983), (-883, -851), (-599, -583),
                 (-483, -451), (-199, -179), (-79, -47), (201, 213),
                 (313, 345), (601, 609), (709, 741), (1001, 1005),
                 (1105, 1137))
        p2 = ((-1519, -1499), (-1119, -1099), (-319, -299), (-719, -699),
              (85, 101), (477, 501), (873, 901))
        p2111 = ((-1747, -1715), (-1615, -1583), (-1483, -1451),
                 (-1347, -1315), (-1327, -1315), (-1215, -1183),
                 (-1083, -1051), (-947, -915), (-815, -783), (-683, -651),
                 (-547, -515), (-415, -383), (-283, -243), (-143, -111),
                 (-11, 21), (117, 149), (249, 281), (381, 413), (513, 545),
                 (645, 677), (777, 809), (909, 941), (1041, 1073))
        p2211 = ((-1843, -1815), (-1715, -1699), (-1583, -1551),
                 (-1451, -1435), (-1435, -1415), (-1315, -1299),
                 (-1183, -1151), (-1051, -1019), (-915, -899), (-783, -751),
                 (-651, -619), (-515, -499), (-383, -351), (-243, -211),
                 (-111, -99), (21, 53), (149, 185), (281, 301), (413, 445),
                 (545, 577), (677, 701), (809, 841), (941, 973), (1073, 1101))
        p2221 = ((-1815, -1799), (-1551, -1519), (-1415, -1399),
                 (-1151, -1119), (-1019, -999), (-751, -719), (-619, -599),
                 (-351, -319), (-211, -199), (53, 85), (185, 201), (445, 477),
                 (577, 601), (841, 873), (973, 1001))
        coff = 0

        if not coff:
            coff = process_segments(year, p1, -1, (0, 1, 2, 3))

        if not coff:
            coff = process_segments(year, p1100, -1, (0, 3))

        if not coff:
            coff = process_segments(year, p1110, -1, (0, 2, 3))

        if not coff:
            coff = process_segments(year, p2, -2, (0, 1, 2, 3))

        if not coff:
            coff = process_segments(year, p2111, -2, (0,), -1, (1, 2, 3))

        if not coff:
            coff = process_segments(year, p2211, -2, (0, 3), -1, (1, 2))

        if not coff:
            coff = process_segments(year, p2221, -2, (0, 2, 3), -1, (1,))

        return coff

    def badi_date_from_jd(self, jd: float, lat: float=None, lon: float=None,
                          zone: float=None, *, us: bool=False,
                          short: bool=False, fraction: bool=False,
                          trim: bool=False, rtd: bool=False,
                          _chk_on: bool=True) -> tuple:
        """
        Convert a Julian Period day to a Badi date.

        :param float jd: Julian Period day.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :param bool us: If True the seconds are split to seconds amd
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool fraction: This will return a short date with a possible
                              fraction on the day.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool rtd: Round to day.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: The Badi date from a Julian Period day.
        :rtype: tuple
        """
        def get_leap_year_info(year, _chk_on):
            leap = self._is_leap_year(year, _chk_on=_chk_on)
            yds = 365 + leap
            ld = 4 + leap
            return leap, yds, ld

        def check_and_fix_day(cjd, y, lat=None, lon=None, zone=None,
                              _chk_on=True):
            fjdy = self.jd_from_badi_date((y, 1, 1), lat, lon, zone, _chk_on)
            return y-1 if (math.floor(fjdy) - math.floor(cjd)) > 0 else y

        md = jd - (self._BADI_EPOCH - 1)
        # This is only needed for the last two days of Badi year 1161
        # and the day before the epoch.
        y = 1 if md < 424046 and md != 0 else 0
        year = math.floor(md / self._MEAN_TROPICAL_YEAR) + y
        leap, yds, ld = get_leap_year_info(year, _chk_on)

        if (y := check_and_fix_day(jd, year, lat, lon, zone, _chk_on)):
            year = y
            leap, yds, ld = get_leap_year_info(year, _chk_on)

        fjdy = self.jd_from_badi_date((year, 1, 1), lat, lon, zone,
                                      _chk_on=_chk_on)
        days = math.floor(jd) - math.floor(fjdy) + 1

        if days <= 342:  # Month 1 - 18
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= yds:  # Month 19
            day = days - (342 + ld)
        else:  # Ayyam-i-Ha
            day = days % 342

        month_days = self._BADI_MONTH_NUM_DAYS
        month_days[18] = (0, ld)  # Fix Ayyám-i-Há days

        for month, ds in month_days:
            if days <= ds: break
            days -= ds

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self._BAHAI_LOCATION[:3]

        date = self._adjust_date(jd, (year, month, day), lat, lon, zone,
                                 fraction=fraction, rtd=rtd)

        if fraction:
            b_date = date
        else:
            l_date = self.long_date_from_short_date(date, trim=True,
                                                    _chk_on=_chk_on)
            b_date = self.kvymdhms_from_b_date(l_date, us=us, short=short,
                                               trim=trim, _chk_on=_chk_on)

        return b_date

    def short_date_from_long_date(self, b_date: tuple, *, trim: bool=False,
                                  _chk_on: bool=True) -> tuple:
        """
        Convert a long date (kvymdhms) to a short date (ymdhms). In either
        case microseconds could also be provided.

        :param tuple b_date: A long form date with or without microseconds.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: The short form Badi date.
        :rtype: tuple
        """
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hh, mm, ss, us = self._get_hms(b_date)
        y = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        hmsms = self._trim_hms((hh, mm, ss, us)) if trim else (hh, mm, ss, us)
        date = (y, month, day) + hmsms
        _chk_on and self._check_valid_badi_date(date, short_in=True)
        return date

    def long_date_from_short_date(self, date: tuple, *, trim: bool=False,
                                  _chk_on: bool=True) -> tuple:
        """
        Convert a date to a short date (ymdhms) to a long date (kvymdhms).

        :param tuple b_date: A short form date with or without microseconds.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool _chk_on: If True (default) all date check are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: The long form Badi date.
        :rtype: tuple
        """
        y, month, day = date[:3]
        hh, mm, ss, us = self._get_hms(date, short_in=True)
        k = y / 361
        kull_i_shay = 0 if y == 0 else math.ceil(k)
        k0 = self._truncate_decimal(k % 1, self._ROUNDING_PLACES)
        # k0 = round(k % 1, self._ROUNDING_PLACES)
        v = k0 / 19 * 361

        if v == 0:  # If there is no fraction in v
            vahid = 19
            year = 19
        else:
            vahid = math.ceil(v)
            year = math.ceil(v % 1 * 19)

        hmsms = self._trim_hms((hh, mm, ss, us)) if trim else (hh, mm, ss, us)
        b_date = (kull_i_shay, vahid, year, month, day) + hmsms
        _chk_on and self._check_valid_badi_date(b_date)
        return b_date

    def date_from_kvymdhms(self, b_date: tuple, *, short: bool=False,
                           _chk_on: bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second,
        us) into a (Kull-i-Shay, Váḥid, year, month, day.fraction) or
        (year, month, day.fraction) date.

        :param tuple b_date: The Badi date in long form.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bools _chk_on: If True (default) all date checks are enforced
                              else if False they are turned off. This is only
                              used internally. Do not use unless you know what
                              you are doing.
        :return: The long or short form Badi date with hours, minutes,
                 seconds, and microseconds if set.
        :rtype: tuple
        """
        _chk_on and self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second, us = self._get_hms(b_date)
        day += round(self._HR(hour) + self._MN(minute) + self._SEC(second) +
                     self._US(us), self._ROUNDING_PLACES)
        date = (kull_i_shay, vahid, year, month, day)
        return (self.short_date_from_long_date(date, trim=True, _chk_on=_chk_on)
                if short else date)

    def kvymdhms_from_b_date(self, b_date: tuple, *, us: bool=False,
                             short: bool=False, trim: bool=False,
                             _chk_on: bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day.fraction) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second) or if
        short is True (year, month, day, hour, minute, second). If us is
        True the seconds are split to second and microsecond.

        :param tuple b_date: The Badi date in long form.
        :param bool us: If True the seconds are split to seconds amd
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: The long or short form Badi date with hours, minutes,
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

        _chk_on and self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]

        if dlen == 5:
            hd = self._PARTIAL_DAY_TO_HOURS(day)
            hour = math.floor(hd)
            md = self._PARTIAL_HOUR_TO_MINUTE(hd)
            minute = math.floor(md)
            second = round(self._PARTIAL_MINUTE_TO_SECOND(md),
                           self._ROUNDING_PLACES)
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
        return (self.short_date_from_long_date(date, trim=trim, _chk_on=_chk_on)
                if short else date)

    def badi_date_from_gregorian_date(self, g_date: tuple, lat: float=None,
                                      lon: float=None, zone: float=None, *,
                                      short: bool=False, trim: bool=False,
                                      rtd: bool=False, _exact: bool=True
                                      ) -> tuple:
        """
        Get the Badi date from the Gregorian date.

        :param tuple g_date: A Gregorian date.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :param bool rtd: Round to day.
        :param bool _exact: Use the more exact Julian Period algorithm.
                            Default is True. This should generally be set to
                            True, a False value will give inaccurate results
                            and is used for testing only.
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        jd = self._gc.jd_from_gregorian_date(g_date, exact=_exact)
        return self.badi_date_from_jd(jd, lat=lat, lon=lon, zone=zone,
                                      short=short, trim=trim, rtd=rtd)

    def gregorian_date_from_badi_date(self, b_date: tuple, lat: float=None,
                                      lon: float=None, zone: float=None, *,
                                      _exact: bool=True, _chk_on: bool=True
                                      ) -> tuple:
        """
        Get the Gregorian date from the Badi date.

        :param tuple b_date: A Badi date short form.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :param bool _exact: Use the more exact Julian Period algorithm.
                            Default is True. This should generally be set to
                            True, a False value, in this method will give
                            inaccurate results and is used for testing only.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: The Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date(b_date, lat, lon, zone, _chk_on=_chk_on)
        return self._gc.ymdhms_from_date(self._gc.gregorian_date_from_jd(
            jd, exact=_exact), us=True)

    def posix_timestamp(self, t: float, lat: float=None, lon: float=None,
                        zone: float=None, *, us: bool=False, short: bool=False,
                        trim: bool=False) -> tuple:
        """
        Get the Badi date from a POSIX timestamp.

        :param float t: Timestamp
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The time zone.
        :param bool us: If True the seconds are split to seconds amd
                        microseconds else if False the seconds has a partial
                        day as a decimal.
        :param bool short: If True then parse for a short date else if False
                           (default) parse for a long date.
        :param bool trim: Trim the us, ss, mm, and hh in that order.
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        days = math.floor(t / 86400)
        jd = days + self._POSIX_EPOCH
        jd += t % 86400 / 86400
        return self.badi_date_from_jd(jd, lat, lon, zone, us=us, short=short,
                                      trim=trim)

    def midday(self, date: tuple, *, hms: bool=False, _chk_on: bool=True
               ) -> tuple:
        """
        Find the midday time in hours, minutes, and seconds with fraction.

        :param tuple date: Badi date short or long.
        :param bool hms: If True return the hours, minutes, and seconds else
                         if False return the decimal value.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: Midday in hours, minutes, and seconds.
        :rtype: tuple
        """
        if len(date) == 5:
            b_date = self.short_date_from_long_date(date, trim=True,
                                                    _chk_on=_chk_on)
        else:
            b_date = date

        jd = self.jd_from_badi_date(b_date)
        diff0 = self._meeus_from_exact(jd)
        ss0 = self._sun_setting(jd + diff0, *self._GMT_LOCATION[:3])
        diff1 = self._meeus_from_exact(jd + 1)
        ss1 = self._sun_setting(jd + 1 + diff1, *self._GMT_LOCATION[:3])
        mid = (ss1 - ss0) / 2
        return self._hms_from_decimal_day(mid) if hms else mid

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
        :return: An object with the lower order parts stripped off if
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
        :return: Nothing
        :rtype: None
        :raises AssertionError: When a date Váḥid, year, month, day, hour,
                                minute, second, or microsecond are out of
                                range.
        """
        cycle = 20

        if not short_in:  # Long Badi date
            kull_i_shay, vahid, year, month, day = b_date[:5]
            hour, minute, second, us = self._get_hms(b_date)
            assert (self.KULL_I_SHAY_MIN <= kull_i_shay
                    <= self.KULL_I_SHAY_MAX), (
                f"Invalid kull-i-shay {kull_i_shay}, it must be in the range "
                f"of [{self.KULL_I_SHAY_MIN}, {self.KULL_I_SHAY_MAX}].")
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
            assert self.MINYEAR <= year <= self.MAXYEAR, (
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
        :return: Nothing
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

    def _is_leap_year(self, year: tuple, _chk_on: bool=True) -> bool:
        """
        Return a Boolean True if a Badi leap year, False if not.

        :param int year: This value must be a Badi short form year.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: A Boolean indicating if a leap year or not.
        :rtype: bool
        """
        return self._days_in_year(year, _chk_on=_chk_on) == 366

    def _days_in_year(self, year: int, _chk_on: bool=True) -> int:
        """
        Determine the number of days in the provided Badi year.

        :param int year: The Badi year to process.
        :param bool _chk_on: If True (default) all date checks are enforced
                             else if False they are turned off. This is only
                             used internally. Do not use unless you know what
                             you are doing.
        :return: The number of days.
        :rtype: int
        """
        jd_n0 = self.jd_from_badi_date((year, 1, 1), _chk_on=_chk_on)
        # For year 1162 we need to turn off the date check so we can get
        # the leap year for 1161.
        on = False if (year + 1) == 1162 else True
        jd_n1 = self.jd_from_badi_date((year + 1, 1, 1), _chk_on=on)
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
        :return: The relevant hours, minutes, and seconds.
        :rtype: tuple
        """
        t_len = len(date)
        s = 3 if short_in else 5
        hour = date[s] if t_len > s and date[s] is not None else 0
        minute = date[s+1] if t_len > s+1 and date[s+1] is not None else 0
        second = date[s+2] if t_len > s+2 and date[s+2] is not None else 0
        us = date[s+3] if t_len > s+3 and date[s+3] is not None else 0
        return hour, minute, second, us

    def _adjust_date(self, jd: float, ymd: tuple, lat: float, lon: float,
                     zone: float, *, fraction: bool=False, us: bool=False,
                     rtd: bool=False) -> tuple:
        """
        The adjusted year, month, and day based on if the JD falls before
        or after sunset.

        .. warning::

           This method will give disasterous results if the jd and and ymd
           arguments are off by more than a day.

        :param float jd: Exact Julian Period day.
        :param tuple ymd: The year month, and day.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :param bool fraction: If True output the day with a fractional day as
                              a decimal. If False (default) output hour,
                              minute, and second.
        :param bool us: If True convert a fractional second to microseconds. If
                        False (default) output second with a fraction.
        :param bool rtd: Round to day.
        :return: Returns the year, month, day, and depending on other
                 arguments, the hour, minute, and second, and microsecond.
        :rtype: tuple
        """
        assert self._xor_boolean((fraction, us, rtd)), (
            "Cannot set more than one of fraction, us, or rtd to True.")
        year, month, day = ymd
        jd0 = math.floor(jd)  # So we always get the sunset on the current day.
        mjd0 = jd0 - self._meeus_from_exact(jd0)  # Current day
        jd_frac = jd % 1
        # Current day sunset
        ss0 = self._sun_setting(mjd0, lat, lon, zone)
        ss_frac = ss0 % 1

        if jd_frac < ss_frac:
            # Previous day sunset
            jd1 = jd0 - 1
            mjd1 = jd1 - self._meeus_from_exact(jd1)
            ss1 = self._sun_setting(mjd1, lat, lon, zone)
            frac = abs(0.5 - ss1 % 1 + jd_frac)
            day -= 1

            if day == 0:
                if month == 1:  # Stage 1
                    year -= 1
                    month = 19
                    day = 19
                    # print('Stage 1', jd, ymd, jd_frac, ss_frac, ss1 % 1)
                elif month in range(2, 19):  # Stage 2
                    month -= 1
                    day = 19
                    # print('Stage 2', jd, ymd, jd_frac, ss_frac, ss1 % 1)
                elif month == 19:  # Stage 3
                    month = 0
                    day = 4 + self._is_leap_year(year)
                    # print('Stage 3', jd, ymd, jd_frac, ss_frac, ss1 % 1)
                else:  # month 0 -> Ayyám-i-Há Stage 4
                    month = 18
                    day = 19
                    # print('Stage 4', jd, ymd, jd_frac, ss_frac, ss1 % 1)
        else:  # Stage 5
            frac = abs(jd_frac - ss_frac)
            # print('Stage 5', jd, ymd, jd_frac, ss_frac)

        if fraction:
            day = round(day + frac, self._ROUNDING_PLACES)
            hms = ()
        elif rtd:
            day = round(day + frac)
            hms = ()
        else:
            hh, mm, ss = self._hms_from_decimal_day(frac)

            if us:
                microsecond = self._PARTIAL_SECOND_TO_MICROSECOND(ss)
                ss = math.floor(ss)
                msec = (microsecond,)
            else:
                ss = round(ss, self._ROUNDING_PLACES)
                msec = ()

            hms = (hh, mm, ss) + msec

        return (year, month, day) + hms

    def _day_Length(self, jd: float, lat: float, lon: float, zone: float
                    ) -> tuple:
        """
        The hour, minute, and seconds of the day's offset either less than
        or more than 24 hours.

        :param float jd: The astronomically exact Julian Period day.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float zone: The standard time zone.
        :return: The hour, minute, and second.
        :rtype: tuple
        """
        jd0 = math.floor(jd)
        jd1 = jd0 + 1
        # The next day
        mjd1 = jd1 + self._meeus_from_exact(jd1)
        ss1 = self._sun_setting(mjd1, lat, lon, zone)
        # The first day
        mjd0 = jd0 + self._meeus_from_exact(jd0)
        ss0 = self._sun_setting(mjd0, lat, lon, zone)
        # Subtract the first day from the next day gived the total
        # hours, minutes, and seconds between them.
        value = list(self._hms_from_decimal_day(ss1 - ss0))
        value[0] = 24 if value[0] == 0 else value[0]
        return tuple(value)
