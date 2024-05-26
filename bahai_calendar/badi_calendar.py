# -*- coding: utf-8 -*-
#
# bahai_calendar/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from bahai_calendar.base_calendar import BaseCalendar
from bahai_calendar.gregorian_calendar import GregorianCalendar


class BahaiCalendar(BaseCalendar):
    """
    Implementation of the Baha'i (Badi) Calendar.
    """
    #(defconstant bahai-location
    #  ;; TYPE location
    #  ;; Location of Tehran for astronomical Baha’i calendar.
    #  (location (deg 35.696111L0) (deg 51.423056L0) (mt 0) (hr (+ 3 1/2))))
    # WGS84:          35.689252, 51.3896 1595m 3.5
    # WGS84--https://coordinates-converter.com/
    # https://whatismyelevation.com/location/35.63735,51.72569/Tehran--Iran-
# https://en-us.topographic-map.com/map-g9q1h/Tehran/?center=35.69244%2C51.19492
    #BAHAI_LOCATION = (35.696111, 51.423056, 0, 3.5)
    # Nur Mazandaran Province, Iran (City Center)
# https://www.google.com/maps/place/Nur,+Mazandaran+Province,+Iran/@36.569336,52.0050234,15z/data=!3m1!4b1!4m6!3m5!1s0x3f8efdf2a3fc7385:0x1f76f83486da57be!8m2!3d36.5763485!4d52.0133073!16zL20vMGJ6cjl6?entry=ttu
    BAHAI_LOCATION = (36.569336, 52.0050234, 0, 3.5)

    BADI_EPOCH = 2394646.5 # 2394646.261111

    BADI_MONTHS = (
        (1, 'Bahá'), (2, 'Jalál'), (3, 'Jamál'), (4, "'Aẓamat"), (5, 'Núr'),
        (6, 'Raḥmat'), (7, 'Kalimát'), (8, 'Kamál'), (9, "Asmá'"),
        (10, "'Izzat"), (11, 'Mashíyyat'), (12, "'Ilm"), (13, 'Qudrat'),
        (14, 'Qawl'), (15, 'Masá’il'), (16, 'Sharaf'), (17, 'Sulṭán'),
        (18, 'Mulk'), (0, 'Ayyám-i-Há'), (19, "'Alá'")
        )

    def __init__(self):
        super().__init__()
        # kull_i_shay: 361-year (19^2) vahid (integer)
        # vahid: (integer) 19-year vahid
        # year: (integer) 1 - 19
        # month: (integer) 1 - 19 plus 0 for Ayyām-i-Hā
        # day: (integer) 1 - 19
        # Baha'i date: [kull_i_shay, vahid, year, month, day]
        self._bahai_date = None
        self._gc = GregorianCalendar()

    def parse_gregorian_datetime(self, dt:datetime.datetime) -> None:
        """
        Parse a Gregorian date to a long form Badi date.
        """
        self._gc.parse_datetime(dt)
        jd = self._gc.jd_from_gregorian_date(self._gc.date_representation)
        self.date_representation = self.badi_date_from_jd(jd)

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

    def sunset(self, date:tuple, lat:float, lon:float, zone:float=0,
               short:bool=False) -> float:
        """
        Return the sunset for the given Badi Day in either the long or
        short form date.

        :param date: Short form Badi date.
        :type date: tuple

        """
        jd = self.jd_from_badi_date(date)
        ss = self._sun_setting(jd, lat, lon, zone)
        print(date, jd, ss)
        return self.badi_date_from_jd(ss, short=short)

    def naw_ruz(self, year:int, short:bool=False) -> tuple:
        """
        Return the Badi date for Naw-Ruz from the given Badi year.

        """
        jd = self.jd_from_badi_date((year, 1, 1))
        ve = self.find_moment_of_equinoxes_or_solstices(jd)
        lat, lon, elev, zone = self.BAHAI_LOCATION
        ss = self._sun_setting(ve, lat, lon, zone)
        return self.badi_date_from_jd(ss, short)

    def first_day_of_ridvan(self, year:int, lat:float=0, lon:float=0,
                            zone:float=0, short:bool=False) -> tuple:
        """
        Find the first day of Riḍván in either the long or short form date.
        If the latitude, longitude, and time zone are not given Riḍván time
        of day is determined for the city of Nur in Iran.
        """
        naw_ruz = self.naw_ruz(year, short=True)

        if lat == 0 and lon == 0 and zone == 0:
            lat, lon, elv, zone = self.BAHAI_LOCATION

        year, month, day = naw_ruz[:3]
        ss_date = self.sunset((year, 2, 13), lat, lon, zone)
        kull_i_shay, vahid, year, month, day = ss_date[:5]
        hour, minute, second = self._get_hms(ss_date)
        b_date = (kull_i_shay, vahid, year, month, day, hour, minute, second)
        print(ss_date, b_date)
        return self.short_date_from_long_date(b_date) if short else b_date

    def _is_leap_year(self, date:tuple) -> bool:
        """
        Return a boolean True if a Badi leap year, False if not.

        :param date: This value can be either the Badi year or a long form
                     date.
        :type date: int or tuple
        :return:
        :rtype: bool
        """
        if isinstance(date, tuple):
            assert len(date) >= 3, ("If tuple it must be the Kull-i-Shay', "
                                    f"Váḥid, and year, found {date}")
            year = (date[0] - 1) * 361 + (date[1] - 1) * 19 + date[2]
        else:
            year = date

        return True if self._days_in_year(year) == 366 else False

    def jd_from_badi_date(self, b_date:tuple) -> float:
        """
        Convert a Badi short form date to Julian period day.

        :param b_date: A short form Badi date.
        :type b_date: tuple
        :return: The Julian Period day.
        :rtype: float
        """
        def process_century(y, coff1, coff2, onoff=0):
            first = False if onoff == 1 else True
            second = False if onoff == 2 else True
            third = False if onoff == 3 else True
            func = lambda low, y, high, v: low < y < high and y % 4 == v
            coff = 0

            if coff1 and y in (1, 34, 67, 100):
                coff = coff1
            # Only one can be turned off at a time.
            elif coff2 and (first and func(1, y, 34, 1) or
                            second and func(34, y, 67, 2) or
                            third and func(67, y, 100, 3)):
                coff = coff2

            return coff

        date = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date), short=True)
        year, month, day = date[:3]

        if month == 0: # Ayyam-i-Ha
            d = 18 * 19 + day
        elif month < 19:
            d = (month - 1) * 19 + day
        else: # month == 19:
            # Because we have to use 4 days without knowing the leap year
            # it's necessary fix for the leap year using coefficients below.
            d = 18 * 19 + 4 + day

        if year < -159: # -259 to -160
            coff = process_century(-159 - year, 0.04, 0.18)
        elif year < -64: # -159 to -65
            coff = process_century(-64 - year, 0.04, 0.16)
        elif year < 35: # -64 to 34
            coff = process_century(35 - year, 0.15, 0.14)
        elif year < 134: # 35 to 133
            coff = process_century(134 - year, 0, 0.12)
        elif year < 233: # 134 to 232
            coff = process_century(233 - year, 0, 0.075)
        elif year < 332: # 234 to 331
            coff = process_century(332 - year, 0, 0.035)
        elif year < 386: # 332 to 385
            coff = process_century(386 - year, 0.037, 0)
        elif year < 517: # 386 to 516
            coff = 0
        elif year < 617: # 517 to 616
            coff = process_century(617 - year, -0.047, -0.023)
        elif year < 716: # 617 to 715
            coff = process_century(716 - year, -0.078, -0.046, onoff=3)
        elif year < 815: # 716 to 814
            coff = process_century(815 - year, -0.11, -0.07)
        elif year < 914: # 815 to 913
            coff = process_century(914 - year, -0.13, -0.093)
        elif year < 1013: # 915 to 1012
            coff = process_century(1013 - year, -0.15, -0.181)
        elif year < 1112: # 1013 to 1111
            coff = process_century(1112 - year, -0.171, -0.195)
        elif year < 1211: # 1112 to 1210
            coff = process_century(1211 - year, -0.18, -0.21)
        else:
            coff = 0

        return round(self.BADI_EPOCH - 1 +
                     math.floor(self.MEAN_TROPICAL_YEAR * (year - 1) - coff) +
                     d, self.ROUNDING_PLACES)

    def badi_date_from_jd(self, jd:float, short:bool=False) -> tuple:
        """
        Convert a Julian period day to a Badi date.
        """
        a = jd - (self.BADI_EPOCH - 1)
        y = a / self.MEAN_TROPICAL_YEAR
        year = math.floor(y) + 1
        m = (y % 1) * self.MEAN_TROPICAL_YEAR
        m1 = math.floor(m / 19)
        month = m1 + 1

        if month > 18:
            ay_years = {365: 4, 366: 5}
            ytd = 18 * 19
            days_in_year = self._days_in_year(year)
            ay_days = ay_years.get(days_in_year)
            assert days_in_year in ay_years.keys(), (
                "Programming error, incorrect number of days in any "
                f"year, found {days}.")
            d = ytd + ay_days
            dsf = math.ceil(m)

            if ytd < dsf <= d:
                month = 0
                day = dsf - ytd
            else:
                month -= 1
                day = days_in_year - d
        else:
            day = math.floor(m) - m1 * 19 + 1

        day += round(a, self.ROUNDING_PLACES) % 1
        date = self.long_date_from_short_date((year, month, day))
        return self.kvymdhms_from_b_date(date, short)

    def _days_in_year(self, year:int) -> int:
        """
        Determine the number of days in the current year.
        """
        jd_n0 = self.jd_from_badi_date((year, 1, 1))
        jd_n1 = self.jd_from_badi_date((year + 1, 1, 1))
        return int(jd_n1 - jd_n0)

    def short_date_from_long_date(self, b_date:tuple) -> tuple:
        """
        Convert a long date (kvymdhms) to a short date (ymdhms)
        """
        self._check_valid_badi_month_day(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second = self._get_hms(b_date)
        y = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        return (y, month, day) + self._trim_hms((hour, minute, second))

    def long_date_from_short_date(self, date:tuple) -> tuple:
        """
        Convert a date to a short date (ymdhms) to a long date (kvymdhms).
        """
        year, month, day = date[:3]
        hour, minute, second = self._get_hms(date, True)
        k = year / 361
        k0 = self._truncate_decimal(k % 1, 6)
        v = k0 / 19 * 361
        kull_i_shay = math.floor(k)

        if v == 0: # If there is no fraction in v
            vahid = 19
            y = 19
        else:
            kull_i_shay += 1
            vahid = math.ceil(v)
            y = math.ceil(self._truncate_decimal(v % 1, 6) * 19)

        hms = self._trim_hms((hour, minute, second))
        b_date = (kull_i_shay, vahid, y, month, day) + hms
        self._check_valid_badi_month_day(b_date)
        return b_date

    def date_from_kvymdhms(self, b_date:tuple, short:bool=False) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second)
        into a (Kull-i-Shay, Váḥid, year, month, day.partial) date.
        """
        self._check_valid_badi_month_day(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second = self._get_hms(b_date)
        day += round(self.HR(hour) + self.MN(minute) + self.SEC(second),
                     self.ROUNDING_PLACES)
        date = (kull_i_shay, vahid, year, month, day)
        return self.short_date_from_long_date(date) if short else date

    def kvymdhms_from_b_date(self, b_date:tuple, short:bool=False) -> tuple:
        """
        Convert ((Kull-i-Shay, Váḥid, year, month, day.partial) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second).
        """
        self._check_valid_badi_month_day(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hd = self.PARTIAL_DAY_TO_HOURS(day)
        hour = math.floor(hd)
        md = self.PARTIAL_HOUR_TO_MINUTE(hd)
        minute = math.floor(md)
        second = round(self.PARTIAL_MINUTE_TO_SECOND(md), self.ROUNDING_PLACES)
        hms = self._trim_hms((hour, minute, second))
        date = (kull_i_shay, vahid, year, month, math.floor(day)) + hms
        return self.short_date_from_long_date(date) if short else date

    def _trim_hms(self, hms:tuple) -> tuple:
        """
        Trim the hours, minutes, or seconds off if zero unless a lower
        value was not zero.
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

    def _check_valid_badi_month_day(self, b_date:tuple) -> bool:
        """
        Check that the month and day values are valid.
        """
        cycle = 20
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second = self._get_hms(b_date)
        assert 1 <= vahid < cycle, (
            f"The number of Váḥids in a Kull-i-Shay’ should be >= 1 or <= 19, "
            f"found {vahid}")
        assert 1 <= year < cycle, (
            f"The number of years in a Váḥid should be >= 1 or <= 19, "
            f"found {year}")
        assert 0 <= month < cycle, (
            f"Invalid month '{month}', should be 0 - 19.")

        # This is Ayyām-i-Hā and could be 4 or 5 days depending on leap year.
        if month == 0:
            cycle = 6 if self._is_leap_year(b_date) else 5

        assert 1 <= day < (cycle), (
            f"Invalid day '{day}' for month '{month}' and year '{year}' "
            f"should be 1 - < {cycle-1}.")
        assert 0 <= hour < 24, (f"Invalid hour '{hour}' it must be "
                                f"0 <= {hour} < 24")
        assert 0 <= minute < 60, (f"Invalid minute '{minute}' should be "
                                  f"0 <= {minute} < 60.")

        if any((hour, minute, second)):
            assert not day % 1, ("If there is a part day then there can be no "
                                 "hours, minutes, or seconds.")

        if any((minute, second)):
            assert not hour % 1, (
                "If there is a part hour then there can be no minutes or "
                "seconds.")

        if second:
            assert not minute % 1, (
                "If there is a part minute then there can be no seconds.")

    def _get_hms(self, date:tuple, short:bool=False) -> tuple:
        """
        Parse the hours, minutes, and seconds correctly for either the
        short or long form Badi date.

        :param date: A short or long form Badi date.
        :type date: tuple
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :return: The relevant hours, minutes, and seconds.
        :rtype: tuple
        """
        t_len = len(date)
        s = 3 if short else 5
        hour = date[s] if t_len > s and date[s] is not None else 0
        minute = date[s+1] if t_len > s+1 and date[s+1] is not None else 0
        second = date[s+2] if t_len > s+2 and date[s+2] is not None else 0
        return hour, minute, second

    def badi_date_from_gregorian_date(self, g_date:tuple,
                                      short:bool=False) -> tuple:
        """
        Get the Badi date from the Gregorian date.
        """
        jd = self._gc.jd_from_gregorian_date(g_date)
        return self.badi_date_from_jd(jd, short=short)

    def gregorian_date_from_badi_date(self, b_date:tuple) -> tuple:
        """
        Get the Gregorian date from the Badi date.
        """
        return self._gc.gregorian_date_from_jd(self.jd_from_badi_date(b_date))
