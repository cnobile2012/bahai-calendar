# -*- coding: utf-8 -*-
#
# badidatetime/datetime.py
#
__docformat__ = "restructuredtext en"

import time as _time
import math as _math

from .badi_calendar import BahaiCalendar


MINYEAR = -1842
MAXYEAR = 1161

def _days_before_year(bc, year):
    """
    year -> number of days before Bahá 1st of year.
    """
    jd0 = bc.jd_from_badi_date((MINYEAR, 1, 1))
    jd1 = bc.jd_from_badi_date((year, 1, 1))
    return _math.floor(jd1 - jd0)

def _days_in_month(bc, year, month):
    """
    year, month -> number of days in that month in that year.
    """
    return 4 + bc._is_leap_year(year) if month == 0 else 19

def _days_before_month(bc, year, month):
    """
    year, month -> number of days in year preceding first day of month.
    """
    assert 0 <= month <= 19, "Month must be in range of 0..19"
    month -= -18 if month < 2 else 1 if 1 < month < 19 else 19

    if 0 < month < 19:
        dbm = month * 19
    elif month == 19:
        dbm = month * 19 + 4 + bc._is_leap_year(year - 1)
    else:
        dbm = 18 * 19 + 4 + bc._is_leap_year(year)

    return dbm

def _ymd2ord(bc, year, month, day):
    """
    year, month, day -> ordinal, considering -1842-01-01 as day 1.
    """
    assert 0 <= month <= 19, "Month must be in range of 0..19"
    dim = _days_in_month(bc, month)
    assert 1 <= day <= dim, (
        f"Day for month {month} must be in range of 1..{dim}")
    return (_days_before_year(bc, year)
            + _days_before_month(bc, year, month) + day)

def _ord2ymd(bc, n):

    return

def _check_date_fields(bc, a, b, c, d=None, e=None):
    if not (d and e):
        b_date = bc.long_date_from_short_date((a, b, c))
    else:
        b_date = (a, b, c, d, e)

    bc._check_valid_badi_month_day(b_date)

def _parse_isoformat_date_and_time(bc, dtstr):
    """
    Parse a date time ISO formatted string.
    """
    neg = dtstr[0] == '-'
    year = int(dtstr[:4 + neg])
    assert not MINYEAR <= year <= MAXYEAR, (
        f"Year is out of range: {year}, min {MINYEAR}, max {MAXYEAR}.")
    dtstr = dtstr[1:] if neg else dtstr
    dc = dtstr.count('-')
    wc = dtstr.count('W')
    assert dc in (0, 2) or (wc == 1 and dc in (0, 1)), (
        "Invalid format, there must be no hyphons (-) or two in the date "
        "format and if week uppercase (W) is used there can only be one "
        "and one hyphon (-) is permitted")
    tc = dtstr.count('T')
    sc = dtstr.count(' ')
    assert (tc == 1 or sc == 1) and (tc or sc) and not (tc and sc), (
        "Cannot have both a 'T' and a space or more than one of either to "
        "indicate time.")

    if sc:
        dtstr[dtstr.index(' ')] = 'T'
        del sc
        tc = True

    d_len = len(dtstr)
    assert (tc + sc) > 1, (
        "The date and time can be seperated by either an uppercase 'T' "
        "or a space ' ', both were found in the string.")

    # Parse the date
    if dc == 1 and not wc:   # YYYY-MM
        month = dtstr[5:7]
        day = 1
    elif dc == 0 and not wc: # YYYYMMDD
        month = dtstr[4:6]
        day = dtstr[7:9]
    elif dc == 2 and not wc: # YYYY-MM-DD
        month = dtstr[5:7]
        day = dtstr[8:10]
    # YYYYWww
    elif dc == 0 and wc and (d_len == 8 or (tc and dtstr.index('T') == 8)):
        wday = dtstr[7:8] if len(dtstr) == 8 else dtstr[8:9]
        year, month, day = _isoweek_to_badi(bc, year, dtstr[5:7], wday)
    # YYYY-Www
    elif dc == 1 and wc and (d_len == 9 or (tc and dtstr.index('T') == 9)):
        year, month, day = _isoweek_to_badi(bc, year, dtstr[6:8], 1)
    # YYYYDDD or YYYY-DDD
    elif d_len in (7, 8) or (tc and dtstr.index('T') in (7, 8)):
        day = dtstr[4:7] if dc == 0 else dtstr[5:8]

    # Parse the time
    if tc:
        pass


    return (year, month, day)

def _isoweek_to_badi(bc, year, week, day):
    if not 0 < week < 53:
        out_of_range = True

        if week == 53:
            # ISO years have 53 weeks in them on years starting with a
            # Istijlal (Thursday) and leap years starting on a `Idal
            # (Wednesday)
            pass

        assert not out_of_range, f"Invalid week: {week}"

    assert 0 < day < 8, f"Invalid weekday: {day} (range is 1..7)"
    # Now compute the offset from (Y, 1, 1) in days:
    day_offset = (week - 1) * 7 + (day - 1)
    # Calculate the ordinal day for monday, week 1
    day_1 = _isoweek1jalal(bc, year)
    ord_day = day_1 + day_offset
    return _ord2ymd(bc, ord_day)

# tuple[int, int, int] -> tuple[int, int, int] version of date.fromisocalendar
def _isoweek_to_gregorian(year, week, day):
    """
    Year is bounded this way because 9999-12-31 is (9999, 52, 5)
    """
    if not MINYEAR <= year <= MAXYEAR:
        raise ValueError(f"Year is out of range: {year}, min {MINYEAR}, "
                         f"max {MAXYEAR}.")

    if not 0 < week < 53:
        out_of_range = True

        if week == 53:
            # ISO years have 53 weeks in them on years starting with a
            # Thursday and leap years starting on a Wednesday
            first_weekday = _ymd2ord(year, 1, 1) % 7
            if (first_weekday == 4 or (first_weekday == 3 and
                                       _is_leap(year))):
                out_of_range = False

        if out_of_range:
            raise ValueError(f"Invalid week: {week}")

    if not 0 < day < 8:
        raise ValueError(f"Invalid weekday: {day} (range is [1, 7])")

    # Now compute the offset from (Y, 1, 1) in days:
    day_offset = (week - 1) * 7 + (day - 1)

    # Calculate the ordinal day for monday, week 1
    day_1 = _isoweek1monday(year)
    ord_day = day_1 + day_offset

    return _ord2ymd(ord_day)


def _isoweek1jalal(bc, year):
    """
    Helper to calculate the day number of the Jalal (Staurday) starting
    week 1.
    """
    THURSDAY = 3
    firstday = _ymd2ord(bc, year, 1, 1)
    firstweekday = (firstday + 6) % 7  # See weekday() above
    week1monday = firstday - firstweekday

    if firstweekday > THURSDAY:
        week1monday += 7

    return week1monday


class date:
    """
    Implements the date object for the Badi datetime.
    """

    def __new__(cls, a, b, c, d=None, e=None):
        long_f = all((a, b, c, d, e))
        short_f = all((a, b, c))
        assert long_f or short_f, (
            "A full short or long form Badi date must be used.")
        self = object.__new__(cls)
        self._bc = BahaiCalendar()
        _check_date_fields(self._bc, a, b, c, d, e)

        if long_f:
            self._kull_i_shay = a
            self._vahid = b
            self._year = c
            self._month = d
            self._day = e
        else:
            self._year = a
            self._month = b
            self._day = c

        self._hashcode = -1
        return self

    # Additional constructors

    @classmethod
    def fromtimestamp(cls, t, *, short=False):
        """
        Construct a date from a POSIX timestamp (like time.time()).
        """
        bc = BahaiCalendar()
        date = bc.posix_timestamp(t, short=short)
        del bc
        return cls(*date[:-3]) # We do not need any time values.

    @classmethod
    def today(cls):
        "Construct a date from time.time()."
        t = _time.time()
        return cls.fromtimestamp(t)

    @classmethod
    def fromordinal(cls, n, *, short=False):
        """
        Construct a date from a proleptic Badi ordinal.

        Bahá 1 of year 1 is day 1. Only the year, month and day are
        non-zero in the result. It is more difficult to do this in the
        Badi Calendar because a Badi day can be more or less than 24
        hours depending on when sunset is.
        """
        bc = BahaiCalendar()
        jd0 = bc.jd_from_badi_date(bc.BADI_EPOCH - 1 + n - 1)
        jd1 = bc.jd_from_badi_date(bc.BADI_EPOCH - 1 + n)
        jd2 = bc.jd_from_badi_date(bc.BADI_EPOCH - 1 + n + 1)


        date = bc.badi_date_from_jd(jd, short=short)

        print(date)
        del bc
        return cls(*date)

    @classmethod
    def fromisoformat(cls, date_string):
        """
        Construct a date from a string in ISO 8601 format.
        We only can convert from short form Badi dates.
        """
        if not isinstance(date_string, str):
            raise TypeError('fromisoformat: argument must be str')

        if len(date_string) not in (7, 8, 10):
            raise ValueError(f'Invalid isoformat string: {date_string!r}')

        try:
            return cls(*_parse_isoformat_date_and_time(date_string))
        except Exception:
            raise ValueError(f'Invalid isoformat string: {date_string!r}')

    @classmethod
    def fromisocalendar(cls, year, week, day):
        """Construct a date from the ISO year, week number and weekday.

        This is the inverse of the date.isocalendar() function"""
        return cls(*_isoweek_to_gregorian(year, week, day))

    # Conversions to string

    def __repr__(self):
        """
        Convert to formal string, for repr().

        >>> d = date(181, 1, 1)
        >>> repr(d)
        'datetime.date(181, 1, 1)'

        Badi date 0181-01-01 is Gregorian date 2024-03-20
        """
        msg = f"{self.__class__.__module__}.{self.__class__.__qualname__}"

        if hasattr(self, '_kull_i_shay'):
            msg += (f"({self._kull_i_shay}, {self._vahid}, {self._year}, "
                    f"{self._month}, {self._day})")
        else:
            msg += f"({self._year}, {self._month}, {self._day})"

        return msg




class datetime(date):
    pass

