# -*- coding: utf-8 -*-
#
# badidatetime/gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import math

from badidatetime.base_calendar import BaseCalendar


class GregorianCalendar(BaseCalendar):
    """
    Implementation of the Gregorian Calendar.
    """
    # Julian date for the Gregorian epoch
    # https://www.grc.nasa.gov/www/k-12/Numbers/Math/Mathematical_Thinking/calendar_calculations.htm
    _GREGORIAN_EPOCH = 1721423.5
    _MONTHS = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    _GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)
    _GREGORIAN_LEAP_YEAR_ALT = lambda self, year: (
        (year % 4 == 0) * (year % 128 != 0) == 1)

    def __init__(self) -> None:
        super().__init__()
        # [year, month, day]
        self._gregorian_date = None

    def jd_from_gregorian_date(self, g_date: tuple, *, exact: bool=False,
                               alt: bool=False) -> float:
        """
        Convert Gregorian dates to Julian day count with the 1582 10, 15
        correction.

        :param tuple g_date: A Gregorian date in the (year, month, day) format.
        :param bool exact: Julian days as if the Gregorian calendar started on
                           year 1. This is astronomically correct but not
                           historically correct.
        :param bool alt: Use a more accurate leap year calculation, only valid
                         when the `exact` keyword is used, there is no effect
                         otherwise.
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

        if exact:  # An astronomically correct algorithm
            GLY = (self._GREGORIAN_LEAP_YEAR_ALT if alt
                   else self._GREGORIAN_LEAP_YEAR)
            td = self._days_in_years(year-1, alt=alt)
            days = td + (self._GREGORIAN_EPOCH - 1)
            month_days = list(self._MONTHS)
            month_days[1] += GLY(year)
            days += sum(month_days[:month-1]) + day
            jd = round(days, self._ROUNDING_PLACES)
        else:  # Meeus historically correct algorithm
            if (year, month) == (1582, 10):
                assert day not in (5, 6, 7, 8, 9, 10, 11, 12, 13, 14), (
                    f"The days 5-14 in 1582-10 are invalid, found day '{day}'."
                    )

            if month <= 2:
                year -= 1
                month += 12

            if (year, month, day) >= (1582, 10, 15):
                a = math.floor(year / 100)
                b = 2 - a + math.floor(a / 4)
            else:
                b = 0

            jd = round(math.floor(self._JULIAN_YEAR * year) + math.floor(
                30.6001 * (month + 1)) + day + b + 1720994.5,
                       self._ROUNDING_PLACES)

        return jd

    def gregorian_date_from_jd(self, jd: float, *, exact: bool=False,
                               alt: bool=False) -> tuple:
        """
        Convert Julian day to Gregorian date.

        :param float jd: A Julian period day. This value should be an
                         historical JD if exact is False and an astronomical
                         JD if exact is True.
        :param bool exact: Julian days as if the Gregorian calendar started on
                           year 1. This is astronomically correct but not
                           historically correct.
        :param bool alt: If True use the 4/128 algorithm for leap years else
                         use the 4/100/400 algorithm. The default is False.
        :return: A Gregorian date in the (year, month, day) format.
        :rtype: tuple

        .. note::

           See Astronomical Formulae for Calculators Enlarged & Revised,
           by Jean Meeus ch3 p26-29
        """
        if exact:  # An astronomically correct algorithm
            GLY = (self._GREGORIAN_LEAP_YEAR_ALT if alt
                   else self._GREGORIAN_LEAP_YEAR)
            # Get the number of days since the Gregorian epoch.
            md = jd - (self._GREGORIAN_EPOCH - 1)
            year = math.floor(abs(md / self._MEAN_TROPICAL_YEAR)) + 1
            year *= -1 if md < (self._GREGORIAN_EPOCH - 1) else 1
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

            month_days = list(self._MONTHS)
            month_days[1] += GLY(year)
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

            date = (year, month, round(day, self._ROUNDING_PLACES))
        else:  # Meeus algorithm
            j_day = jd + 0.5
            z = math.floor(j_day)
            f = j_day % 1

            if z >= 2299161:  # 1582-10-15 Julian and Gregorian crossover.
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

            date = (year, month, round(day, self._ROUNDING_PLACES))

        return date

    def posix_timestamp(self, t: float, *, zone: float=0,
                        us: bool=False) -> tuple:
        """
        Find the year, month, day, hours, minutes, and seconds from a
        POSIX timestamp updated for provided time zone.

        .. warning::

           DO NOT USE THIS METHOD -- It's experimental and is not used in any
           other code.

        *** TODO *** Needs to be fixed for years before 1970 Gregorian.

        :param float t: POSIX timestamp
        :param float zone: Timezone
        :param bool us: If True the seconds are split to seconds amd
                        microseconds else if False the seconds has a
                        fractional day as a decimal.
        :return: The time of the day corrected for the timezone.
        :rtype: tuple
        """
        t += zone * 3600
        days = math.floor(t / 86400)
        year = 1970
        leap = False
        days = abs(days) + 1

        # Find the year and remaining number of days.
        while True:
            leap = self._GREGORIAN_LEAP_YEAR(year)
            diy = 365 + leap
            if days < diy: break
            days -= diy
            year += 1

        month_days = list(self._MONTHS)
        month_days[1] += leap
        days_before_month = 0

        for month, days_in_month in enumerate(month_days, start=1):
            days_before_month += days_in_month
            if days > days_before_month: continue
            day = days - (days_before_month - days_in_month)

            if day > days_in_month:
                month += 1
                day = day - days_in_month

            break

        seconds = t % 86400
        minutes = math.floor(seconds / 60)
        minute = minutes % 60
        hour = math.floor(minutes / 60)
        second = round(seconds % 60, 6)
        microsecond = 0

        if us:
            microsecond = self._PARTIAL_SECOND_TO_MICROSECOND(second)
            second = math.floor(second)

        # print(f"{t:18.6f} {zone:>+2.2f} {year:02} {month:02} {day:02} "
        #      f"{hour:02} {int(minute):02} {second:02} {microsecond}")

        date = (year, month, day, hour, minute, second
                ) + ((microsecond,) if us else ())
        self._check_valid_gregorian_month_day(date, historical=True)
        return date

    def gregorian_year_from_jd(self, jd: float) -> int:
        """
        Find the Gregorian year from a Julian Period day.

        :param float jd: The Julian Period day.
        :return: The year portion of the Julian day.
        :rtype: int
        """
        return self.gregorian_date_from_jd(jd)[0]

    def date_from_ymdhms(self, date: tuple) -> tuple:
        """
        Convert (year, month, day, hour, minute, second) into a
        (year, month, day.fractional) date.

        :param tuple date: A six part date (y, m, d, hh, mm, ss).
        :return: The three part (y, m, d.nnn).
        :rtype: tuple
        """
        self._check_valid_gregorian_month_day(date)
        t_len = len(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hour = date[3] if t_len > 3 and date[3] is not None else 0
        minute = date[4] if t_len > 4 and date[4] is not None else 0
        second = date[5] if t_len > 5 and date[5] is not None else 0
        day += round(self._HR(hour) + self._MN(minute) + self._SEC(second),
                     self._ROUNDING_PLACES)
        return year, month, day

    def ymdhms_from_date(self, date: tuple, us: bool=False) -> tuple:
        """
        Convert (year, month, day.fractional) into a
        (year, month, day, hour, minute, second).

        :param tuple date: A three part date (y, m, d.nnn).
        :param bool us: If True return microseconds as seperate field from
                   seconds else return seconds with fractional seconds.
                   Default is False.
        :return: A six part date (y, m, d, hh, mm, ss).
        :rtype: tuple
        """
        self._check_valid_gregorian_month_day(date)
        date_len = len(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hour = date[3] if date_len > 3 else 0
        minute = date[4] if date_len > 4 else 0
        second = date[5] if date_len > 5 else 0
        microsec = date[6] if date_len > 6 else 0
        total_seconds = ((hour * 3600) + (minute * 60) + second +
                         (microsec / 1e6))
        day += total_seconds / 86400
        hhmmssus = self._hms_from_decimal_day(day, us=us)
        return (year, month, math.floor(day)) + hhmmssus

    def _check_valid_gregorian_month_day(self, g_date: tuple,
                                         historical: bool=False) -> None:
        """
        Check that the month and day values are valid.

        :param tuple g_date: The date to check.
        :param bool historical: If True use the Julian leap year before 1883.
        :return: Nothing
        :rtype: None
        """
        t_len = len(g_date)
        year = g_date[0]
        month = abs(g_date[1])
        day = abs(g_date[2])
        LY = (self._JULIAN_LEAP_YEAR if historical and year < 1883
              else self._GREGORIAN_LEAP_YEAR)
        hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
        minute = g_date[4] if t_len > 4 and g_date[4] is not None else 0
        second = g_date[5] if t_len > 5 and g_date[5] is not None else 0
        assert 1 <= month <= 12, f"Invalid month '{month}', should be 1 - 12."
        days = self._MONTHS[month - 1]

        if month == 2:  # Subtract 0 or 1 from February if leap year.
            days += LY(year)

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
