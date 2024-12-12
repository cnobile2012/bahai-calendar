# -*- coding: utf-8 -*-
#
# badidatetime/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from badidatetime.base_calendar import BaseCalendar
from badidatetime.gregorian_calendar import GregorianCalendar


class BahaiCalendar(BaseCalendar):
    """
    Implementation of the Baha'i (Badi) Calendar.
    """
    # WGS84:          35.689252, 51.3896 1595m 3.5
    # WGS84--https://coordinates-converter.com/
    # https://whatismyelevation.com/location/35.63735,51.72569/Tehran--Iran-
#https://en-us.topographic-map.com/map-g9q1h/Tehran/?center=35.69244%2C51.19492
#https://www.google.com/maps/place/Tehran,+Tehran+Province,+Iran/@35.9098957,51.51371,9.49z/data=!4m6!3m5!1s0x3f8e02c69b919039:0x17c26479772c5928!8m2!3d35.6891975!4d51.3889736!16s%2Fm%2F025zk75?entry=ttu
    # https://gml.noaa.gov/grad/solcalc/ Sunset data
    # Near Mehrabad International Airport
    #                 lattude    longitude  zone IANA name      elevation
    BAHAI_LOCATION = (35.682376, 51.285817, 3.5, 'Asia/Tehran', 0)
    GMT_LOCATION = (51.477928, -0.001545, 0, 0)
    BADI_EPOCH = 2394643.262113 # 2394645.261536 using Meeus' algorithm
    BADI_MONTH_NUM_DAYS = [
        (1, 19), (2, 19), (3, 19), (4, 19), (5, 19), (6, 19), (7, 19),
        (8, 19), (9, 19), (10, 19), (11, 19), (12, 19), (13, 19), (14, 19),
        (15, 19), (16, 19), (17, 19), (18, 19), (0, 0), (19, 19)
        ]
    KULL_I_SHAY_MIN = -5
    KULL_I_SHAY_MAX = 4
    MINYEAR = -1842
    MAXYEAR = 1161

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # kull_i_shay: 361-year (19^2) vahid (integer)
        # vahid: (integer) 19-year vahid
        # year: (integer) 1 - 19
        # month: (integer) 1 - 19 plus 0 for Ayyām-i-Hā
        # day: (integer) 1 - 19
        # Baha'i date: [kull_i_shay, vahid, year, month, day]
        self._bahai_date = None
        self._gc = GregorianCalendar()

    def parse_gregorian_datetime(self, dt:datetime.datetime, lat:float=None,
                                 lon:float=None, zone:float=None, *,
                                 short=False, trim:bool=False) -> None:
        """
        Parse a Gregorian date to a long form Badi date.
        """
        self._gc.parse_datetime(dt)
        jd = self._gc.jd_from_gregorian_date(self._gc.date_representation,
                                             exact=True)
        self.date_representation = self.badi_date_from_jd(
            jd, lat=lat, lon=lon, zone=zone, short=short, trim=trim)

    @property
    def date_representation(self) -> tuple:
        return self._bahai_date

    @date_representation.setter
    def date_representation(self, representation:tuple=None):
        self._bahai_date = representation

    def utc_sunset(self, date:tuple, lat:float=None, lon:float=None,
                   zone:float=None) -> tuple:
        """
        Return the time of sunset in UTC time for the given Badi Day.

        :param date: A Badi date.
        :type date: tuple
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :return: The hour, minute, and second of sunset based on the
                 provided coordinates.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date(date[:3], lat, lon, zone)
        return self.hms_from_decimal_day(jd + 0.5)

    def naw_ruz_g_date(self, year:int, lat:float=None, lon:float=None,
                       zone:float=None, *, hms:bool=False) -> tuple:
        """
        Return the Badi date for Naw-Ruz from the given Badi year.

        :param year: A Badi year.
        :type year: int
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param hms: If True the output returns the hours, minutes, and seconds
                    as seperate fields. If False the day has a decimal value
                    indicating the hours, minutes, and seconds.
        :type hms: bool
        :return: A Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        date = self._gc.gregorian_date_from_jd(jd, exact=True)
        return self._gc.ymdhms_from_date(date) if hms else date

    def first_day_of_ridvan_g_date(self, year:int, lat:float=None,
                                   lon:float=None, zone:float=None, *,
                                   hms:bool=False) -> tuple:
        """
        Find the first day of Riḍván in either with or without hours,
        minutes, and seconds.
        If the latitude, longitude, and time zone are not given Riḍván time
        of day is determined for the city of Nur in Iran.

        :param year: A Badi year.
        :type year: int
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param hms: If True the output returns the hours, minutes, and second
                    as seperate fields. If False the day has a decimal value
                    indicating the hours, minutes, and seconds.
        :type hms: bool
        :return: A Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date((year, 2, 13), lat, lon, zone)
        date = self._gc.gregorian_date_from_jd(jd, exact=True)
        return self._gc.ymdhms_from_date(date) if hms else date

    def jd_from_badi_date(self, b_date:tuple, lat:float=None, lon:float=None,
                          zone:float=None, _chk_on=True) -> float:
        """
        Convert a Badi short form date to Julian period day.

        :param b_date: A short form Badi date.
        :type b_date: tuple
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The Julian Period day.
        :rtype: float
        """
        year, month, day = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date, trim=True, _chk_on=_chk_on),
            short=True, _chk_on=_chk_on)

        if month == 0: # Ayyam-i-Ha
            days = 18 * 19
        elif month < 19: # month 1 - 18
            days = (month - 1) * 19
        else: # month == 19:
            days = 18 * 19 + 4 + self._is_leap_year(year, _chk_on=_chk_on)

        td = self._days_in_years(year-1)
        jd = td + math.floor(self.BADI_EPOCH+1) + days + day

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        # The diff value converts the more exact jd to the Meeus algorithm
        # for determining the sunset jd. The fractional on the day is not
        # affected.
        diff = self._meeus_from_exact(jd)
        ss_a = self._sun_setting(jd + diff, lat, lon, zone) % 1
        return round(jd + ss_a + self._get_coff(year), self.ROUNDING_PLACES)


    def _get_coff(self, year):
        """
        General ranges are determined with:
        ./contrib/misc/badi_jd_tests.py -p -S start_year -E end_year Where -S
        is the 1st year and -E is the nth year + 1 that needs to be process.
        Use the following command to test the results of each segment.
        ./contrib/misc/badi_jd_tests.py -qXS start_year -E end_year
        Full range is -1842 to 1161.
        """
        def process_segment(y, a=0, onoff0=(), b=0, onoff1=()):
            func = lambda y, onoff: 0 < y < 100 and y % 4 in onoff
            coff = 0

            if a and func(y, onoff0):   # Whatever is passed in onoff0.
                coff = a
            elif b and func(y, onoff1): # Whatever is passed in onoff1.
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
                 (-1347, -1315), (-1327, -1315 ), (-1215,-1183 ),
                 (-1083, -1051), (-947, -915), (-815, -783), (-683, -651),
                 (-547, -515), (-415, -383), (-283, -243), (-143, -111),
                 (-11, 21), (117, 149), (249, 281), (381, 413), (513, 545),
                 (645, 677), (777, 809), (909, 941), (1041, 1073))
        p2211 = ((-1843, -1815), (-1715, -1699), (-1583, -1551),
                 (-1451, -1435), (-1435, -1415), (-1315, -1299 ),
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

    def badi_date_from_jd(self, jd:float, lat:float=None, lon:float=None,
                          zone:float=None, *, us:bool=False, short:bool=False,
                          fraction:bool=False, trim:bool=False,
                          rtd:bool=False, _chk_on=True) -> tuple:
        """
        Convert a Julian Period day to a Badi date.

        :param jd: Julian Period day.
        :type jd: float
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param us: If True the seconds are split to seconds amd microseconds
                   else if False the seconds has a partial day as a decimal.
        :type us: bool
        :param short: If True then parse for a short date else if False
                      (default) parse for a long date.
        :type short: bool
        :param fraction: This will return a short date with a possible
                         fraction on the day.
        :type fraction: bool
        :param trim: Trim the us, ss, mm, and hh in that order.
        :type trim: bool
        :param rtd: Round to day.
        :type rtd: bool
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
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

        md = jd - (self.BADI_EPOCH - 1)
        year = math.floor(md / self.MEAN_TROPICAL_YEAR) + 1
        leap, yds, ld = get_leap_year_info(year, _chk_on)

        if (y := check_and_fix_day(jd, year, lat, lon, zone, _chk_on)):
            year = y
            leap, yds, ld = get_leap_year_info(year, _chk_on)

        fjdy = self.jd_from_badi_date((year, 1, 1), lat, lon, zone,
                                      _chk_on=_chk_on)
        days = math.floor(jd) - math.floor(fjdy) + 1

        if days <= 342: # Month 1 - 18
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= yds: # Month 19
            day = days - (342 + ld)
        else: # Ayyam-i-Ha
            day = days % 342

        month_days = self.BADI_MONTH_NUM_DAYS
        month_days[18] = (0, ld) # Fix Ayyám-i-Há days

        for month, ds in month_days:
            if days <= ds: break
            days -= ds

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        day = self._adjust_day_for_24_hours(
            jd, lat, lon, zone, day=day, rtd=rtd)

        if fraction:
            b_date = (year, month, day)
        else:
            date = self.long_date_from_short_date((year, month, day),
                                                  trim=True, _chk_on=_chk_on)
            b_date = self.kvymdhms_from_b_date(date, us=us, short=short,
                                               trim=trim, _chk_on=_chk_on)

        return b_date

    def short_date_from_long_date(self, b_date:tuple, *, trim:bool=False,
                                  _chk_on:bool=True) -> tuple:
        """
        Convert a long date (kvymdhms) to a short date (ymdhms). In either
        case microseconds could also be provided.

        :param b_date: A long form date with or without microseconds.
        :type b_date: tuple
        :param trim: Trim the us, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
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

    def long_date_from_short_date(self, date:tuple, *, trim:bool=False,
                                  _chk_on:bool=True) -> tuple:
        """
        Convert a date to a short date (ymdhms) to a long date (kvymdhms).

        :param b_date: A short form date with or without microseconds.
        :type b_date: tuple
        :param trim: Trim the us, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long form Badi date.
        :rtype: tuple
        """
        y, month, day = date[:3]
        hh, mm, ss, us = self._get_hms(date, short_in=True)
        k = y / 361
        kull_i_shay = 0 if y == 0 else math.ceil(k)
        k0 = self._truncate_decimal(k % 1, self.ROUNDING_PLACES)
        #k0 = round(k % 1, self.ROUNDING_PLACES)
        v = k0 / 19 * 361

        if v == 0: # If there is no fraction in v
            vahid = 19
            year = 19
        else:
            vahid = math.ceil(v)
            year = math.ceil(v % 1 * 19)

        hmsms = self._trim_hms((hh, mm, ss, us)) if trim else (hh, mm, ss, us)
        b_date = (kull_i_shay, vahid, year, month, day) + hmsms
        _chk_on and self._check_valid_badi_date(b_date)
        return b_date

    def date_from_kvymdhms(self, b_date:tuple, *, short:bool=False,
                           _chk_on:bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second,
        us) into a (Kull-i-Shay, Váḥid, year, month, day.fraction) or
        (year, month, day.fraction) date.

        :param b_date: The Badi date in long form.
        :type b_date: tuple
        :param short: If True then parse for a short date else if False
                      (default) parse for a long date.
        :type short: bool
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long or short form Badi date with hours, minutes,
                 seconds, and microseconds if set.
        :rtype: tuple
        """
        _chk_on and self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second, us = self._get_hms(b_date)
        day += round(self.HR(hour) + self.MN(minute) + self.SEC(second) +
                     self.US(us), self.ROUNDING_PLACES)
        date = (kull_i_shay, vahid, year, month, day)
        return (self.short_date_from_long_date(date, trim=True, _chk_on=_chk_on)
                if short else date)

    def kvymdhms_from_b_date(self, b_date:tuple, *, us:bool=False,
                             short:bool=False, trim:bool=False,
                             _chk_on:bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day.fraction) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second) or if
        short is True (year, month, day, hour, minute, second). If us is
        True the seconds are split to second and microsecond.

        :param b_date: The Badi date in long form.
        :type b_date: tuple
        :param us: If True the seconds are split to seconds amd microseconds
                   else if False the seconds has a partial day as a decimal.
        :type us: bool
        :param short: If True then parse for a short date else if False
                      (default) parse for a long date.
        :type short: bool
        :param trim: Trim the us, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
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
            hd = self.PARTIAL_DAY_TO_HOURS(day)
            hour = math.floor(hd)
            md = self.PARTIAL_HOUR_TO_MINUTE(hd)
            minute = math.floor(md)
            second = round(self.PARTIAL_MINUTE_TO_SECOND(md),
                           self.ROUNDING_PLACES)
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

    def badi_date_from_gregorian_date(self, g_date:tuple, lat:float=None,
                                      lon:float=None, zone:float=None, *,
                                      short:bool=False, trim:bool=False,
                                      rtd:bool=False,
                                      _exact:bool=True) -> tuple:
        """
        Get the Badi date from the Gregorian date.

        :param g_date: A Gregorian date.
        :type g_date: tuple
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param short: If True then parse for a short date else if False
                      (default) parse for a long date.
        :type short: bool
        :param trim: Trim the us, ss, mm, and hh in that order.
        :type trim: bool
        :param rtd: Round to day.
        :type rtd: bool
        :param _exact: Use the more exact Julian Period algorithm. Default
                       is True. This should generally be set to True, a
                       False value will give inaccurate results and is used
                       for testing only.
        :type _exact: bool
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        jd = self._gc.jd_from_gregorian_date(g_date, exact=_exact)
        return self.badi_date_from_jd(jd, lat=lat, lon=lon, zone=zone,
                                      short=short, trim=trim, rtd=rtd)

    def gregorian_date_from_badi_date(self, b_date:tuple, lat:float=None,
                                      lon:float=None, zone:float=None, *,
                                      _exact=True, _chk_on=True) -> tuple:
        """
        Get the Gregorian date from the Badi date.

        :param b_date: A Badi date short form.
        :type b_date: tuple
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param _exact: Use the more exact Julian Period algorithm. Default
                       is True. This should generally be set to True, a
                       False value, in this method will give inaccurate
                       results and is used for testing only.
        :type _exact: bool
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date(b_date, lat, lon, zone, _chk_on=_chk_on)
        return self._gc.ymdhms_from_date(self._gc.gregorian_date_from_jd(
            jd, exact=_exact), us=True)

    def posix_timestamp(self, t:float, lat:float=None, lon:float=None,
                        zone:float=None, *, us:bool=False, short=False,
                        trim:bool=False) -> tuple:
        """
        Get the Badi date from a POSIX timestamp.

        :param t: Timestamp
        :type t: float
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param us: If True the seconds are split to seconds amd microseconds
                   else if False the seconds has a partial day as a decimal.
        :type us: bool
        :param short: If True then parse for a short date else if False
                      (default) parse for a long date.
        :type short: bool
        :param trim: Trim the us, ss, mm, and hh in that order.
        :type trim: bool
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        days = math.floor(t / 86400)
        jd = days + self.POSIX_EPOCH
        jd += t % 86400 / 86400
        return self.badi_date_from_jd(jd, lat, lon, zone, us=us, short=short,
                                      trim=trim)

    def midday(self, date:tuple, *, hms=False, _chk_on:bool=True) -> tuple:
        """
        Find the midday time in hours with fraction.

        :param date: Badi date short or long.
        :type date: tuple
        :param hms: If True return the hours, minutes, and seconds else
                    if False return the decimal value.
        :type hms: bool
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
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
        ss0 = self._sun_setting(jd + diff0, *self.GMT_LOCATION[:3])
        diff1 = self._meeus_from_exact(jd + 1)
        ss1 = self._sun_setting(jd + 1 + diff1, *self.GMT_LOCATION[:3])
        mid = (ss1 - ss0) / 2
        return self.hms_from_decimal_day(mid) if hms else mid

    def _trim_hms(self, hms:tuple) -> tuple:
        """
        Trim the hours, minutes, seconds or microseconds off the date if
        zero unless a lower value was not zero.
        Examples: (12, 30, 6, 0) The zero microseconds would be trimmed.
                  (12, 0, 6, 0) The zero microseconds would be trimmed but
                                the zero minutes would not be trimmed.

        :param hms: An hour, minute, and second object.
        :type hms: tuple
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

    def _check_valid_badi_date(self, b_date:tuple, short_in=False) -> None:
        """
        Check that the Kull-i-Shay, Váḥids, year, month, day, hour, minute,
        second, and microsecond values are valid.

        :param b_date: A long form Badi date.
        :type b_date: tuple
        :param short_in: If True then parse for a short date else if False
                         parse for a long date. This is for incoming dates
                         not outgoing dates as in most other uses of 'short'.
        :type short_in: bool
        :return: Nothing
        :rtype: None
        :raises AssertionError: When a date Váḥid, year, month, day, hour,
                                minute, second, or microsecond are out of
                                range.
        """
        cycle = 20

        if not short_in: # Long Badi date
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
        else: # Short Badi date
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

    def _check_valid_badi_time(self, hour:float, minute:float, second:float,
                               us:int, maxsec:int=60) -> None:
        """
        Check that the hour, minute, second, and microsecond values are valid.

        :param hour: Hours
        :type hour: float
        :param minute: Minutes
        :type minute: float
        :param second: Seconds
        :type second: float
        :param us: Microseconds
        :type us: int
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

    def _is_leap_year(self, year:tuple, _chk_on:bool=True) -> bool:
        """
        Return a Boolean True if a Badi leap year, False if not.

        :param date: This value must be a Badi short form year.
        :type year: int
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
         :return: A Boolean indicating if a leap year or not.
        :rtype: bool
        """
        return self._days_in_year(year, _chk_on=_chk_on) == 366

    def _days_in_year(self, year:int, _chk_on:bool=True) -> int:
        """
        Determine the number of days in the provided Badi year.

        :param year: The Badi year to process.
        :type year: int
        :param _chk_on: If True (default) all date checks are enforced else
                        if False they are turned off. This is only used
                        internally. Do not use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The number of days.
        :rtype: int
        """
        jd_n0 = self.jd_from_badi_date((year, 1, 1), _chk_on=_chk_on)
        # For year 1162 we need to turn off the date check so we can get
        # the leap year for 1661.
        on = False if (year + 1) == 1162 else True
        jd_n1 = self.jd_from_badi_date((year + 1, 1, 1), _chk_on=on)
        return int(math.floor(jd_n1) - math.floor(jd_n0))

    def _get_hms(self, date:tuple, *, short_in:bool=False) -> tuple:
        """
        Parse the hours, minutes, seconds, and microseconds, if they exist
        for either the short or long form Badi date.

        :param date: A long or short form Badi date.
        :type date: tuple
        :param short_in: If True then parse for a short date else if False
                         parse for a long date. This is for incoming dates
                         not outgoing dates as in most other uses of 'short'.
        :type short_in: bool
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

    def _adjust_day_for_24_hours(self, jd:float, lat:float, lon:float,
                                 zone:float, *, day:float=None,
                                 rtd=False, hms:bool=False) -> float|tuple:
        """
        We have to deal with days that are either more or less than 24 hours.
        This method does two things. It corrects the Badi time which starts
        at sunset when given a Julian Period day which starts at noon. It
        also corrects the day and the fraction of the day when the Badi day
        is more or less than 24 hours.

        :param jd: Exact Julian Period day possibly with a fraction.
        :type jd: float
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param day: The day that gets modified. This parameter is optional.
        :type day: float
        :param rtd: Round to closest day only when the day is used. Has no
                    effect if hms is True.
        :type rtd: bool
        :param hms: If True the returned value is a tuple in the form of
                    (hh, mm, ss) indicating the length of the day else if
                    False one of the other two values are returned.
        :type hms: bool
        :return: See the note below.
        :rtype: float | tuple

        .. note::

           1. There are two different outputs that can be returned.
              a. The adjusted day based on when the day ends.
              b. The hour, minute, and seconds of the days offset either
                 less than or more than 24 hours.
        """
        if day is not None and hms:
            raise ValueError(
                "Cannot use the day and hms arguments at the same time.")

        if day is None and not hms:
            raise ValueError("Must provide a day or hms must be True.")

        jd0 = math.floor(jd)
        jd1 = jd0 + 1
        # The return value from _meeus_from_exact() is used to convert the
        # more exact jd to the Meeus algorithm jd so that the sunset can be
        # determined properly. The fractional day of either algorithm would
        # be the same.
        mjd1 = jd1 + self._meeus_from_exact(jd1)
        ss1 = self._sun_setting(mjd1, lat, lon, zone)

        if hms:
            mjd0 = jd0 + self._meeus_from_exact(jd0)
            ss0 = self._sun_setting(mjd0, lat, lon, zone)
            value = list(self.hms_from_decimal_day(ss1 - ss0))
            value[0] = 24 if value[0] == 0 else value[0]
            value = tuple(value)
        else:
            fraction = round(jd % 1 - ss1 % 1, self.ROUNDING_PLACES)
            v = 1 if (day + fraction) < 1 else day + fraction
            value = round(v) if rtd else v

        return value

    def _adjust_date_for_24_hours(self, jd:float, ymd:tuple, lat:float,
                                  lon:float, zone:float, rtd=False) -> tuple:
        """
        The adjusted year, month, and day based on when the sunset of the
        day or the day before..
        """
        jd1 = math.floor(jd)
        mjd1 = jd1 + self._meeus_from_exact(jd1) # Current day
        ss1 = self._sun_setting(mjd1, lat, lon, zone) # Current day sunset
        jd_frac = jd % 1
        ss_frac = ss1 % 1
        jd0 = jd1 - 1
        year, month, day = ymd

        if jd_frac < ss_frac: # Are we on the previous day?
            mjd0 = jd0 + self._meeus_from_exact(jd0) # Previous day
            ss0 = self._sun_setting(mjd0, lat, lon, zone) # Previous day sunset
            hour = 0.5 - ss0 % 1 + jd_frac

            if month == 19 and day == 1:
                day = 4 + self._is_leap_year(year)
                month = 0
            elif month == 0 and day == 1:
                day = 19
                month = 18
            elif month == 1 and day == 1:
                day = 19
                month = 19
                year -= 1
            elif day == 1:
                day = 19
                month -= 1
        else:
            hour = jd_frac - ss_frac


        return year, month, day

    def _day_Length(self, jd:float, lat:float, lon:float, zone:float) -> tuple:
        """
        The hour, minute, and seconds of the days offset either less than
        or more than 24 hours.
        """
        jd0 = math.floor(jd)
        jd1 = jd0 + 1
        mjd1 = jd1 + self._meeus_from_exact(jd1)
        ss1 = self._sun_setting(mjd1, lat, lon, zone)
        mjd0 = jd0 + self._meeus_from_exact(jd0)
        ss0 = self._sun_setting(mjd0, lat, lon, zone)
        value = list(self.hms_from_decimal_day(ss1 - ss0))
        value[0] = 24 if value[0] == 0 else value[0]
        return tuple(value)
