# -*- coding: utf-8 -*-
#
# bahai_calendar/gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import math
from datetime import datetime

from bahai_calendar.base_calendar import BaseCalendar


class GregorianCalendar(BaseCalendar):
    """
    Implementation of the Gregorian Calendar.
    """
    # Julian date for the gregorian epoch
    # https://www.grc.nasa.gov/www/k-12/Numbers/Math/Mathematical_Thinking/calendar_calculations.htm
    GREGORIAN_EPOCH = 1721423.5
    MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)
    GREGORIAN_LEAP_YEAR_ALT = lambda self, year: (
        (year % 4 == 0) * (year % 128 != 0) == 1)

    def __init__(self) -> None:
        super().__init__()
        # [year, month, day]
        self._gregorian_date = None

    def parse_datetime(self, dt:datetime) -> None:
        self.date_representation = (dt.year, dt.month, dt.day)
        super().parse_datetime(dt)

    @property
    def date_representation(self):
        return self._gregorian_date + super().time_representation

    @date_representation.setter
    def date_representation(self, representation):
        self._gregorian_date = representation

    def jd_from_gregorian_date(self, g_date:tuple, *, exact:bool=False,
                               alt=False) -> float:
        """
        Convert Gregorian dates to Julian day count with the 1582 10, 15
        correction.

        :param g_date: A Gregorinan date in the (year, month, day) format.
        :type g_date: tuple
        :param exact: Julian days as if the Gregorian calendar started on
                      year 1. This astronomicaly correct but not
                      historically correct.
        :type exact: bool
        :param alt: Use a more accurate leap year calculation, only valid when
                    the `exact` keyword is used, there is no effect otherwise.
        :type apt: bool
        :return: A Julian day.
        :rtype: float

        .. note::

           1. See Astronomical Formulae for Calculators Enlarged & Revised,
              by Jean Meeus ch3 p24-25
           2. See: https://www.fourmilab.ch/documents/calendar/
              https://core2.gsfc.nasa.gov/time/julian.html
           3. Caution when using the `exact=True` keyword the Julian Period
              days returned will not always be the same when `exact=False`
              is used. This means date comparisons will be in error if both
              dates do not use the same True or False `exact` keyword.
        """
        year, month, day = self.date_from_ymdhms(g_date)

        if exact: # An astronomically correct algorithm
            GLY = (self.GREGORIAN_LEAP_YEAR_ALT if alt
                   else self.GREGORIAN_LEAP_YEAR)
            year, month, day = self.date_from_ymdhms(g_date)
            y = self.JULIAN_YEAR * (year - 1)
            y = math.floor(y)
            month_days = list(self.MONTHS)
            month_days[1] = 29 if GLY(year) else 28
            md = sum([v for v in month_days[:month-1]])
            md += day - self._increment_index(year) + (self.GREGORIAN_EPOCH - 1)
            jd = round(y + md, self.ROUNDING_PLACES)
        else: # Meeus historically correct algorithm
            if (year, month) == (1582, 10):
                assert day not in (5, 6, 7, 8, 9, 10, 11, 12, 13, 14), (
                    f"The days 5-14 in 1582-10 are invalid, found day '{day}'.")

            if month <= 2:
                year -= 1
                month += 12

            if (year, month, day) >= (1582, 10, 15):
                a = math.floor(year / 100)
                b = 2 - a + math.floor(a / 4)
            else:
                b = 0

            jd = round(math.floor(self.JULIAN_YEAR * year) + math.floor(
                30.6001 * (month + 1)) + day + b + 1720994.5,
                       self.ROUNDING_PLACES)

        return jd

    def _increment_index(self, year):
        i = 0

        if year > 99:
            if year % 400 != 1 and year % 100 == 1:
                # Years that increment nn1 etc.
                i += year / 100 - math.floor(year / 400)
            elif year % 400 < 100:
                # Non-incremented years, all years. 400, 401, 800, 801, etc.
                i += math.floor(year / 400) * 3
            elif year % 100 == 0:
                # Incremented Years 100, 200, 300 and 500, 600, 700, etc.
                i += year / 100

                if 100 <= year <= 300:
                    i -= 1
                elif 500 <= year <= 700:
                    i -= 2
                elif 900 <= year <= 1100:
                    i -= 3
                elif 1300 <= year <= 1500:
                    i -= 4
                elif 1700 <= year <= 1900:
                    i -= 5
                elif 2100 <= year <= 2300:
                    i -= 6
                elif 2500 <= year <= 2700:
                    i -= 7
                elif 2900 <= year <= 3100:
                    i -= 8
                elif 3300 <= year <= 3500:
                    i -= 9
                elif 3700 <= year <= 3900:
                    i -= 10
                elif 4100 <= year <= 4300:
                    i -= 11
                elif 4500 <= year <= 4700:
                    i -= 12
            elif year % 400 != 1 and 1 < year % 100 < 100:
                # Incremented Years 502 - 599, 602 - 699, etc.
                i += math.floor(year / 100)

                if i < 4:    # 102-199 = 1, 202-299 = 2, 302-399 = 3
                    pass
                elif i < 8:  # 502-599 = 4, 602-699 = 5, 702-799 = 6
                    i -= 1
                elif i < 12: # 902-999 = 7, 1002-1099 = 8, 1102-1199 = 9
                    i -= 2
                elif i < 16: # 1302-1399 = 10, 1402-1499 = 11, 1502-1599 = 12
                    i -= 3
                elif i < 20: # 1702-1799 = 13, 1802-1899 = 14, 1902-1999 = 15
                    i -= 4
                elif i < 24: # 2102-2199 = 16, 2202-2299 = 17, 2302-2399 = 18
                    i -= 5
                elif i < 28: # 2502-2599 = 19, 2602-2699 = 20, 2702-2799 = 21
                    i -= 6
                elif i < 32: # 2902-2999 = 22, 3002-3099 = 23, 3102-3199 = 24
                    i -= 7
                elif i < 36: # 3302-3399 = 25, 3402-3499 = 26, 3502-3599 = 27
                    i -= 8
                elif i < 40: # 3702-3799 = 28, 3802-3899 = 29, 3902-3999 = 30
                    i -= 9
                elif i < 44: # 4102-4199 = 31, 4202-4299 = 32, 4302-4399 = 33
                    i -= 10
                elif i < 48: # 4502-4599 = 34, 4602-4699 = 35, 4702-4799 = 36
                    i -= 11

        return math.floor(i)

    def gregorian_date_from_jd(self, jd:float, *, exact:bool=False,
                               alt=False) -> tuple:
        """
        Convert Julian day to Gregorian date.

        :param jd: A Julian period day.
        :type jd: float
        :return: A Gregorian date in the (year, month, day) format.
        :rtype: tuple

        .. note::

           See Astronomical Formulae for Calculators Enlarged & Revised,
           by Jean Meeus ch3 p26-29
        """
        if exact: # An astronomically correct algorithm
            def days_in_year(y, alt=False):
                n_4 = y // 4

                if alt:
                    n_128 = y // 128
                    n_leap_years = n_4 - n_128
                else:
                    n_100 = y // 100
                    n_400 = y // 400
                    n_leap_years = n_4 - n_100 + n_400

                a = y - n_leap_years # Non-leap years
                b = y - a # Leap years
                return a * 365 + b * 366

            GLY = (self.GREGORIAN_LEAP_YEAR_ALT if alt
                   else self.GREGORIAN_LEAP_YEAR)
            # Get the number of days since the Gregorian epoch.
            md = jd - (self.GREGORIAN_EPOCH - 1)
            year = math.floor(md / self.JULIAN_YEAR)
            # A refined number of days since epoch for the date.
            td = days_in_year(year, alt=alt)
            days = md - td

            while days > 365:
                year += 1
                td = days_in_year(year, alt=alt)
                days = md - td

            if days == 0:
                days = 366
            else:
                year += 1

            month_days = list(self.MONTHS)
            month_days[1] = 29 if GLY(year) else 28
            d = day = 0

            for month, ds in enumerate(month_days, start=1):
                d += ds
                if days > d: continue
                day = math.ceil(days - (d - ds))
                break

            date = (year, month, round( day + (jd % 1) - 0.5,
                                        self.ROUNDING_PLACES))
        else:
            j_day = jd + 0.5
            z = math.floor(j_day)
            f = j_day % 1

            if z >= 2299161: # 1582-10-15 Julian and Gregorian crossover.
                alpha = math.floor((z - 1867216.25) / 36524.25)
                a = z + 1 + alpha - math.floor(alpha / 4)
            else:
                a = z

            b = a + 1524
            c = math.floor((b - 122.1) / 365.25)
            d = math.floor(365.25 * c)
            e = math.floor((b - d) / 30.6001)
            day = b - d - math.floor(30.6001 * e) + f
            month = 0
            year = 0

            if e > 13:
                month = e - 13
            else:
                month = e - 1

            if month > 2:
                year = c - 4716
            else:
                year = c - 4715

            date = (year, month, round(day, self.ROUNDING_PLACES))

        return date

    def gregorian_year_from_jd(self, jde:float) -> int:
        """
        Find the Gregorian year from a Julian Period day.
        """
        return self.gregorian_date_from_jd(jde)[0]

    def date_from_ymdhms(self, date:tuple) -> tuple:
        """
        Convert (year, month, day, hour, minute, second) into a
        (year, month, day.partial) date.
        """
        self._check_valid_gregorian_month_day(date)
        t_len = len(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hour = date[3] if t_len > 3 and date[3] is not None else 0
        minute = date[4] if t_len > 4 and date[4] is not None else 0
        second = date[5] if t_len > 5 and date[5] is not None else 0
        day += round(self.HR(hour) + self.MN(minute) + self.SEC(second),
                     self.ROUNDING_PLACES)
        return (year, month, day)

    def ymdhms_from_date(self, date:tuple) -> tuple:
        """
        Convert (year, month, day.partial) into a
        (year, month, day, hour, minute, second).
        """
        self._check_valid_gregorian_month_day(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hd = self.PARTIAL_DAY_TO_HOURS(day)
        hour = math.floor(hd)
        md = self.PARTIAL_HOUR_TO_MINUTE(hd)
        minute = math.floor(md)
        second = round(self.PARTIAL_MINUTE_TO_SECOND(md), self.ROUNDING_PLACES)
        return (year, month, math.floor(day), hour, minute, second)

    def _check_valid_gregorian_month_day(self, g_date:tuple) -> bool:
        """
        Check that the month and day values are valid.
        """
        t_len = len(g_date)
        year = g_date[0]
        month = abs(g_date[1])
        day = abs(g_date[2])
        hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
        minute = g_date[4] if t_len > 4 and g_date[4] is not None else 0
        second = g_date[5] if t_len > 5 and g_date[5] is not None else 0
        assert 1 <= month <= 12, f"Invalid month '{month}', should be 1 - 12."
        days = self.MONTHS[month - 1]

        if month == 2: # Subtract 0 or 1 from Febuary if leap year.
            days -= 0 if self.GREGORIAN_LEAP_YEAR(year) else 1

        assert 1 <= math.floor(day) <= days, (
            f"Invalid day '{day}' for month '{month}' and year '{year}' "
            f"should be 1 - {days}.")
        assert hour < 24, f"Invalid hour '{hour}' it must be < 24"
        assert minute < 60, f"Invalid minute '{minute}' should be < 60."

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
