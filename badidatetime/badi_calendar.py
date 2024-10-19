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
    BAHAI_LOCATION = (35.682376, 51.285817, 3.5, 0)
    GMT_LOCATION = (51.477928, -0.001545, 0, 0)
    BADI_EPOCH = 2394644.261791 # 2394646.261791 using Meeus' algorithm
    BADI_MONTH_NAMES = (
        (1, 'Bahá'), (2, 'Jalál'), (3, 'Jamál'), (4, "'Aẓamat"), (5, 'Núr'),
        (6, 'Raḥmat'), (7, 'Kalimát'), (8, 'Kamál'), (9, "Asmá'"),
        (10, "'Izzat"), (11, 'Mashíyyat'), (12, "'Ilm"), (13, 'Qudrat'),
        (14, 'Qawl'), (15, 'Masá’il'), (16, 'Sharaf'), (17, 'Sulṭán'),
        (18, 'Mulk'), (0, 'Ayyám-i-Há'), (19, "'Alá'")
        )
    KULL_I_SHAY_MIN = -5
    KULL_I_SHAY_MAX = 4
    MINYEAR = -1842
    MAXYEAR = 1161

    def __init__(self, *args, **kwargs):
        super().__init__()
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
                                 short=False) -> None:
        """
        Parse a Gregorian date to a long form Badi date.
        """
        self._gc.parse_datetime(dt)
        jd = self._gc.jd_from_gregorian_date(self._gc.date_representation,
                                             exact=True)
        self.date_representation = self.badi_date_from_jd(
            jd, lat=lat, lon=lon, zone=zone, short=short)

    #def parse_badi_datetime(self, dt:badi_datetime) -> None:
    #    """
    #    Parse a Badi date to a long form Badi date.
    #    """
    #    self._gc.parse_datetime(dt)
    #    jd = self._bc.jd_from_badi_date(self._gc.date_representation)
    #    self.date_representation = self.badi_date_from_jd(jd)

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
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

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
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

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
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

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
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The Julian Period day.
        :rtype: float
        """
        year, month, day = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date, trim=True, _chk_on=_chk_on),
            short=True, _chk_on=_chk_on)

        if month == 0: # Ayyam-i-Ha
            m = 18 * 19
        elif month < 19: # month 1 - 18
            m = (month - 1) * 19
        else: # month == 19:
            m = 18 * 19 + 4 + self._is_leap_year(year)

        td = self._days_in_years(year-1)
        jd = td + math.floor(self.BADI_EPOCH) + m + day

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        # The diff value converts my jd to the Meeus algorithm for
        # determining the sunset jd.
        diff = self._meeus_from_exact(jd)
        ss_a = self._sun_setting(jd + diff, lat, lon, zone) % 1
        return round(jd + ss_a + self._get_coff(year), self.ROUNDING_PLACES)

    def _get_coff(self, year):
        def process_segment(y, pc, c=0, onoff=(), *, c0=0, c1=0, c2=0, c3=0):
            func = lambda y, onoff: 1 < y < 100 and y % 4 in onoff
            coff = 0

            if pc and y in (1, 34, 67, 100):
                coff = pc
            elif c and func(y, onoff): # Whatever is passed in onoff.
                coff = c
            elif c0 and func(y, (0,)):
                coff = c0
            elif c1 and func(y, (1,)):
                coff = c1
            elif c2 and func(y, (2,)):
                coff = c2
            elif c3 and func(y, (3,)):
                coff = c3

            return coff

        def process_segments(year, p, pc, c=0, onoff=(), *,
                             c0=0, c1=0, c2=0, c3=0):
            coff = 0

            for start, end in p:
                if year in range(start, end):
                    # start to end (range -S start -E end)
                    coff = process_segment(end - year, pc, c=c, onoff=onoff,
                                           c0=c0, c1=c1, c2=c2, c3=c3)

            return coff

        p1 = ((-1842, -1819), (-1727, -1699), (-1599, -1563), (-1500, -1499),
              (-1471, -1435), (-1401, -1399), (-1343, -1307), (-1306, -1303),
              (-1302, -1299), (-1215, -1179), (-1087, -1051), (-955, -919),
              (-827, -795), (-700, -663), (-567, -535), (-501, -499),
              (-439, -403), (-299, -275), (-179, -143), (-47, -11), (101, 117),
              (213, 249), (345, 381), (501, 513), (609, 645), (741, 777),
              (901, 909), (1005, 1041), (1137, 1162))
        p0111 = ((-1819, -1799), (-1563, -1531), (-1435, -1403),
                 (-1179, -1151), (-1051, -1019), (-919, -899), (-795, -763),
                 (-663, -631), (-535, -503), (-403, -371), (-275, -243),
                 (-143, -111), (-11, 21), (117, 149), (249, 281), (381, 413),
                 (513, 545), (645, 677), (777, 809), (909, 941), (1041, 1073))
        p1222 = ((-1799, -1787), (-1691, -1659), (-1299, -1275), (-899, -891))
        p1122 = ((-1787, -1755), (-1659, -1627), (-1399, -1375),
                 (-1275, -1247), (-999, -987), (-891, -859), (-499, -471),
                 (-99, -79), (301, 313), (701, 709), (1101, 1105))
        p1112 = ((-1755, -1727), (-1627, -1599), (-1499, -1471),
                 (-1375, -1343), (-1247, -1215), (-1099, -1087), (-987, -955),
                 (-859, -827), (-599, -567), (-471, -439), (-199, -179),
                 (-79, -47), (201, 213), (313, 345), (601, 609), (709, 741),
                 (1001, 1005), (1105, 1137))
        p2 = ((-1699, -1691),)
        p0011 = ((-1531, -1503), (-1151, -1119), (-1019, -999), (-763, -731),
                 (-631, -599), (-371, -339), (-243, -211), (-111, -99),
                 (21, 53), (149, 185), (281, 301), (413, 445), (545, 577),
                 (677, 701), (809, 841), (941, 973), (1073, 1101))
        p0001 = ((-1119, -1099), (-731, -699), (-339, -307), (-211, -199),
                 (53, 85), (185, 201), (445, 477), (513, 545), (577, 601),
                 (841, 873), (973, 1001))

        # General ranges are determined with:
        # ./contrib/misc/badi_jd_tests.py -p -S start_year -E end_year
        # Where -S is the 1st year and -E is the nth year + 1 that needs to
        # be process. Use the following command to test the results of each
        # segment. ./contrib/misc/badi_jd_tests.py -qXS start_year -E end_year
        # Full range is -1842 to 1161
        coff = process_segments(year, p1, 1, 1, (0, 1, 2, 3))

        if not coff:
            coff = process_segments(year, p0111, 1, 1, (1, 2, 3))

        if not coff:
            coff = process_segments(year, p1222, 2, 2, (1, 2, 3), c0=1)

        if not coff:
            coff = process_segments(year, p1122, 2, 1, (0, 3,), c1=2, c2=2)

        if not coff:
            coff = process_segments(year, p1112, 2, 1, (0, 2, 3), c1=2)

        if not coff:
            coff = process_segments(year, p2, 2, 2, (0, 1, 2, 3))

        if not coff:
            coff = process_segments(year, p0011, 1, 1, (1, 2))

        if not coff:
            coff = process_segments(year, p0001, 1, c1=1)

        return coff

    def badi_date_from_jd(self, jd:float, lat:float=None, lon:float=None,
                          zone:float=None, *, short:bool=False,
                          fraction:bool=False, rtd:bool=False) -> tuple:
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
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :param fraction: This will return a short date with a possible
                         fraction on the day.
        :type fraction: bool
        :param rtd: Round to day.
        :type rtd: bool
        :return: The Badi date from a Julian Period day.
        :rtype: tuple
        """
        def get_leap_year_info(year):
            leap = self._is_leap_year(year)
            yds = 366 if leap else 365
            ld = 4 + leap
            return leap, yds, ld

        def check_and_fix_day(cjd, y, lat=None, lon=None, zone=None):
            fjdy = self.jd_from_badi_date((y, 1, 1), lat, lon, zone)
            return y-1 if (math.floor(fjdy) - math.floor(cjd)) > 0 else y

        md = jd - (self.BADI_EPOCH - 1)
        year = math.floor(md / self.MEAN_TROPICAL_YEAR) + 1
        leap, yds, ld = get_leap_year_info(year)

        if (y := check_and_fix_day(jd, year, lat, lon, zone)):
            year = y
            leap, yds, ld = get_leap_year_info(year)

        fjdy = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        days = math.floor(jd) - math.floor(fjdy) + 1

        if days <= 342: # Month 1 - 18
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= yds: # Month 19
            day = days - (342 + ld)
        else: # Ayyam-i-Ha
            day = days % 342

        month_days = [(n, 19) for n, v in self.BADI_MONTH_NAMES]
        month_days[18] = (0, ld)

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
                                                  trim=True)
            b_date = self.kvymdhms_from_b_date(date, short=short, trim=True)

        return b_date

    def short_date_from_long_date(self, b_date:tuple, *, trim:bool=False,
                                  _chk_on:bool=True) -> tuple:
        """
        Convert a long date (kvymdhms) to a short date (ymdhms). In either
        case microseconds could also be provided.

        :param b_date: A long form date with or without microseconds.
        :type b_date: tuple
        :param trim: Trim the ms, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The short form Badi date.
        :rtype: tuple
        """
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hh, mm, ss, ms = self._get_hms(b_date)
        y = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        hmsms = self._trim_hms((hh, mm, ss, ms)) if trim else (hh, mm, ss, ms)
        date = (y, month, day) + hmsms
        _chk_on and self._check_valid_badi_date(b_date, short_in=True)
        return date

    def long_date_from_short_date(self, date:tuple, *, trim:bool=False,
                                  _chk_on:bool=True) -> tuple:
        """
        Convert a date to a short date (ymdhms) to a long date (kvymdhms).

        :param b_date: A short form date with or without microseconds.
        :type b_date: tuple
        :param trim: Trim the ms, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long form Badi date.
        :rtype: tuple
        """
        y, month, day = date[:3]
        hh, mm, ss, ms = self._get_hms(date, short_in=True)
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

        hmsms = self._trim_hms((hh, mm, ss, ms)) if trim else (hh, mm, ss, ms)
        b_date = (kull_i_shay, vahid, year, month, day) + hmsms
        _chk_on and self._check_valid_badi_date(b_date)
        return b_date

    def date_from_kvymdhms(self, b_date:tuple, *, short:bool=False,
                           _chk_on:bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second,
        ms) into a (Kull-i-Shay, Váḥid, year, month, day.partial) date.

        :param b_date: The Badi date in long form.
        :type b_date: tuple
        :param short: If True the short form Badi date is returned else the
                      long form is returned.
        :type short: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long or short form Badi date with hours, minutes,
                 seconds, and microseconds if set.
        :rtype: tuple
        """
        _chk_on and self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second, ms = self._get_hms(b_date)
        day += round(self.HR(hour) + self.MN(minute) + self.SEC(second) +
                     self.MS(ms), self.ROUNDING_PLACES)
        date = (kull_i_shay, vahid, year, month, day)
        return (self.short_date_from_long_date(date, trim=True)
                if short else date)

    def kvymdhms_from_b_date(self, b_date:tuple, *, ms:bool=False,
                             short:bool=False, trim:bool=False,
                             _chk_on:bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day.partial) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second) or if
        short is True (year, month, day, hour, minute, second). If ms is
        True the seconds are split to second and microsecond.

        :param b_date: The Badi date in long form.
        :type b_date: tuple
        :param ms: If True the seconds are split to seconds amd microseconds
                   else if False the seconds has a partial day as a decimal.
        :type ms: bool
        :param short: If True the short form Badi date is returned else the
                      long form is returned.
        :type short: bool
        :param trim: Trim the ms, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long or short form Badi date with hours, minutes,
                 seconds, and microseconds if set.
        :rtype: tuple
        """
        _chk_on and self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        dlen = len(b_date)

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

        if ms:
            hmsms = (hour, minute, *self._sec_microsec_from_seconds(second))
        else:
            hmsms = (hour, minute, second)

        date += self._trim_hms(hmsms) if trim else hmsms
        return (self.short_date_from_long_date(date, trim=trim)
                if short else date)

    def badi_date_from_gregorian_date(self, g_date:tuple, *, short:bool=False,
                                      _exact:bool=True,
                                      rtd:bool=False) -> tuple:
        """
        Get the Badi date from the Gregorian date.

        :param g_date: A Gregorian date.
        :type g_date: tuple
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :param _exact: Use the more exact Julian Period algorithm. Default
                       is True. This should generally be set to True, a
                       False value will give inaccurate results and is used
                       for testing only.
        :type _exact: bool
        :param rtd: Round to day.
        :type rtd: bool
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        jd = self._gc.jd_from_gregorian_date(g_date, exact=_exact)
        return self.badi_date_from_jd(jd, short=short, rtd=rtd)

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
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date(b_date, lat, lon, zone, _chk_on=_chk_on)
        return self._gc.gregorian_date_from_jd(jd, exact=_exact)

    def posix_timestamp(self, t:float, lat=None, lon=None, zone=None, *,
                        short=False) -> tuple:
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
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        days = math.floor(t / 86400)
        jd = days + self.POSIX_EPOCH
        jd += t % 86400 / 86400
        return self.badi_date_from_jd(jd, lat, lon, zone, short=short)

    def midday(self, date:tuple, *, hms=False) -> tuple:
        """
        Find the midday time in hours with fraction.

        :param date: Badi date short or long.
        :type date: tuple
        :param hms: If True return the hours, minutes, and seconds else
                    if False return the decimal value.
        :type hms: bool
        :return: Midday in hours, minutes, and seconds.
        :rtype: tuple
        """
        if len(date) == 5:
            b_date = self.short_date_from_long_date(date, trim=True)
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
        Trim the hours, minutes, seconds or microseconds off if zero unless
        a lower value was not zero.

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
        and second values are valid.

        :param b_date: A long form Badi date.
        :type b_date: tuple
        :param short_in: If True then parse for a short date else if False
                         parse for a long date. This is for incoming dates
                         not outgoing dates as in most other uses of 'short'.
        :type short_in: bool
        :return: Nothing
        :rtype: None
        :raises AssertionError: When a date Váḥid, year, month, day, hour,
                                minute, or second are out of range.
        """
        cycle = 20

        if not short_in: # Long Badi date
            kull_i_shay, vahid, year, month, day = b_date[:5]
            hour, minute, second, ms = self._get_hms(b_date)
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
            hour, minute, second, ms = self._get_hms(b_date, short_in=True)
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
        assert 0 <= hour < 25, (
            f"Invalid hour '{hour}', it must be in the range of [0, 24].")
        assert 0 <= minute < 60, (
            f"Invalid minute '{minute}', it must be in the range of [0, 59].")
        assert 0 <= second < 60, (
            f"Invalid second '{second}', it must be in the range of [0, 59].")
        assert 0 <= ms < 1000000, (
            f"Invalid microseconds '{ms}', it must be in the range of "
            "[0, 999999].")

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

    def _is_leap_year(self, year:tuple) -> bool:
        """
        Return a Boolean True if a Badi leap year, False if not.

        :param date: This value must be a Badi short form year.
        :type year: int
        :return: A Boolean indicating if a leap year or not.
        :rtype: bool
        """
        return self._days_in_year(year) == 366

    def _days_in_year(self, year:int) -> int:
        """
        Determine the number of days in the provided Badi year.

        :param year: The Badi year to process.
        :type year: int
        :return: The number of days.
        :rtype: int
        """
        jd_n0 = self.jd_from_badi_date((year, 1, 1))
        jd_n1 = self.jd_from_badi_date((year + 1, 1, 1))
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
        ms = date[s+3] if t_len > s+3 and date[s+3] is not None else 0
        return hour, minute, second, ms

    def _adjust_day_for_24_hours(self, jd:float, lat:float, lon:float,
                                 zone:float, *, day:float=None,
                                 hms:bool=False, rtd=False) -> float|tuple:
        """
        We have to deal with days that are either more or less than 24 hours.
        This method does two things. It corrects the Badi time which starts
        at sunset when given a Julian Period day which starts at noon. It
        also corrects the day and the fraction of the day when the Badi day
        is more or less than 24 hours.

        :param jd: The Julian Period day possibly with a fraction.
        :type jd: float
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param day: The day that gets modified. This parameter is optional.
        :type day: float
        :param hms: If True the returned value is a tuple in the form of
                    (hh, mm, ss) indicating the length of the day else if
                    False one of the other two values are returned.
        :type hms: bool
        :param rtd: Round to closest day only when the day is used. Has no
                    effect if hms is True.
        :type rtd: bool
        :return: See the below note.
        :rtype: float | tuple

        .. note::

           1. There are three different outputs that can be returned.
              a. The adjusted day.
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
        # The diff values converts my jd to the Meeus algorithm jd so
        # that the sunset can be determined properly. The fractional day
        # of either algorithm would be the same.
        diff1 = self._meeus_from_exact(jd1)
        mjd1 = jd1 + diff1
        ss1 = self._sun_setting(mjd1, lat, lon, zone)

        if hms:
            diff0 = self._meeus_from_exact(jd0)
            mjd0 = jd0 + diff0
            ss0 = self._sun_setting(mjd0, lat, lon, zone)
            value = self.hms_from_decimal_day(ss1 - ss0)
        else:
            fraction = round(jd % 1 - ss1 % 1, self.ROUNDING_PLACES)
            v = 1 if (day + fraction) < 1 else day + fraction
            value = round(v) if rtd else v

        return value
