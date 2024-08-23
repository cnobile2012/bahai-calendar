# -*- coding: utf-8 -*-
#
# badidatetime/gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import math
from datetime import datetime

from badidatetime.base_calendar import BaseCalendar


class GregorianCalendar(BaseCalendar):
    """
    Implementation of the Gregorian Calendar.
    """
    # Julian date for the gregorian epoch
    # https://www.grc.nasa.gov/www/k-12/Numbers/Math/Mathematical_Thinking/calendar_calculations.htm
    GREGORIAN_EPOCH = 1721423.5
    MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    JULIAN_LEAP_YEAR = lambda self, year: year % 4 == 0
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
        return self._gregorian_date + self.time_representation

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
            td = self._days_in_years(year-1, alt=alt)
            days = td + (self.GREGORIAN_EPOCH - 1)
            month_days = list(self.MONTHS)
            month_days[1] = 29 if GLY(year) else 28
            days += sum(month_days[:month-1]) + day
            jd = round(days, self.ROUNDING_PLACES)
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
            GLY = (self.GREGORIAN_LEAP_YEAR_ALT if alt
                   else self.GREGORIAN_LEAP_YEAR)
            # Get the number of days since the Gregorian epoch.
            md = jd - (self.GREGORIAN_EPOCH - 1)
            year = math.floor(abs(md / self.MEAN_TROPICAL_YEAR)) + 1
            year *= -1 if md < (self.GREGORIAN_EPOCH - 1) else 1
            # Refine the number of days since the epoch for the date.
            td = self._days_in_years(year, alt=alt)
            days = md - td

            while days > 365:
                year += 1
                td = self._days_in_years(year, alt=alt)
                days = md - td

            if days == 0:
                days = 365 if year < 0 and year % 4 != 0 else 366
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

            f = jd % 1
            day += f - (1.5 if f > 0.5 else 0.5)

            if day < 1:
                month -= 1 if month > 1 else -11
                day = month_days[month-1] + day
                year -= 1 if month == 12 else 0

            date = (year, month, round(day, self.ROUNDING_PLACES))
            #print('jd', jd, 'md', md, 'td', td, 'days', days, 'd', d,
            #      'f', f, 'months', month_days, 'date', date)
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

    def ymdhms_from_posix_time(self, t:float, *, lon:float=0,
                               zone:float=None) -> tuple:
        """
        Find the year, month, day, hours, minutes, and seconds from a
        POSIX timestamp updated for timezone.
        """
        if zone is None: zone = lon / 15
        days = math.floor(t / 86400)
        year = 1970
        leap = False

        while True:
            leap = self.GREGORIAN_LEAP_YEAR(year)
            diy = 366 if leap else 365
            if days < diy: break
            days -= diy
            year += 1

        month_days = list(self.MONTHS)
        month_days[1] -= 0 if leap else 1
        days_in_month = 0

        for month, ds in enumerate(month_days, start=1):
            days_in_month += ds
            if days > days_in_month: continue
            day = math.ceil(days - (days_in_month - ds)) + 1
            break

        seconds = t % 86400
        hours = math.floor(seconds / 3600 + zone)
        tzp = zone % 1
        seconds = seconds % 3600
        minutes = math.floor(seconds / 60)
        minutes += tzp * 60
        print(year, month, day, hours, minutes)



        if hours < 0:
            day -= abs(hours) - 24 + 1
            hours = hours + 24
        elif hours >= 24:
            day += 1
            hours -= 24

            if minutes >= 60:
                minutes -= 60

        seconds = seconds % 60
        return year, month, day, hours, minutes, seconds

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

    def _check_valid_gregorian_month_day(self, g_date:tuple,
                                         historical:bool=False) -> None:
        """
        Check that the month and day values are valid.

        :param g_date: The date to check.
        :type g_date: tuple
        :param historical: If True use the Julian leap year before 1883.
        :type historical: bool
        :return: Nothing
        :rtype: None
        """
        t_len = len(g_date)
        year = g_date[0]
        month = abs(g_date[1])
        day = abs(g_date[2])
        LY = (self.JULIAN_LEAP_YEAR if historical and year < 1883
              else self.GREGORIAN_LEAP_YEAR)
        hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
        minute = g_date[4] if t_len > 4 and g_date[4] is not None else 0
        second = g_date[5] if t_len > 5 and g_date[5] is not None else 0
        assert 1 <= month <= 12, f"Invalid month '{month}', should be 1 - 12."
        days = self.MONTHS[month - 1]

        if month == 2: # Subtract 0 or 1 from Febuary if leap year.
            days -= 0 if LY(year) else 1

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
