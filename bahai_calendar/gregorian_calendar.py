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
    GREGORIAN_EPOCH = 1721425.5

    #(defconstant gregorian-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of the (proleptic) Gregorian calendar.
    #  (rd 1))
    RD_GREGORIAN_EPOCH = 1  # See BaseCalender notes.
    _MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    #(defun gregorian-leap-year? (g-year)
    #  ;; TYPE gregorian-year -> boolean
    #  ;; True if g-year is a leap year on the Gregorian calendar.
    #  (and (= (mod g-year 4) 0)
    #       (not (member (mod g-year 400)
    #                    (list 100 200 300)))))
    #GREGORIAN_LEAP_YEAR = lambda self, year: (
    #    year % 4 == 0 and (year % 400 not in (100, 200, 300)))

    # ((MOD(year, 4) = 0) * ((MOD(year, 100) <> 0) + (MOD(year, 400) = 0)) = 1)
    GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)

    def __init__(self):
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

    def jd_from_gregorian_date(self, g_date:tuple) -> float:
        """
        Convert Gregorian dates to Julian day count with the 1582 10, 15
        correction.

        :param g_date: A Gregorinan date in the (year, month, day) format.
        :type g_date: tuple
        :return: A Julian day.
        :rtype: float

        .. note::

           See Astronomical Formulae for Calculators Enlarged & Revised,
           by Jean Meeus ch3 p24-25
           See: https://www.fourmilab.ch/documents/calendar/
                https://core2.gsfc.nasa.gov/time/julian.html
        """
        year, month, day = self.date_from_ymdhms(g_date)

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

        return round(math.floor(self.JULIAN_YEAR * year) + math.floor(
            30.6001 * (month + 1)) + day + b + 1720994.5, self.ROUNDING_PLACES)

    def gregorian_date_from_jd(self, jd:float) -> tuple:
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

        return year, month, round(day, self.ROUNDING_PLACES)

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
        days = self._MONTHS[month - 1]

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
