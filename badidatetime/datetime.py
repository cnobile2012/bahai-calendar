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

def _check_date_fields(self, a, b, c, d=None, e=None):
    if not (d and e):
        b_date = self._bc.long_date_from_short_date((a, b, c))
    else:
        b_date = (a, b, c, d, e)

    self._bc._check_valid_badi_month_day(b_date)


def _parse_isoformat_date(dtstr):
    """
    It is assumed that this is an ASCII-only string of lengths 7, 8 or 10,
    unless the year is negative then add one to each length above.
    See the comment on
    Modules/_datetimemodule.c:_find_isoformat_datetime_separator
    """
    neg = dtstr[0] == '-'
    year = int(dtstr[:4 + neg])
    dtstr = dtstr[1:] if neg else dtstr
    dc = dtstr.count('-')
    wc = dtstr.count('W')
    assert dc in (0, 2) or (wc == 1 and dc in (0, 1)), (
        "Invalid format, there must be no hyphons (-) or two in the date "
        "format and if week uppercase (W) is used there can only be one "
        "and one hyphon (-) is permitted")
    tc = dtstr.count('T')
    sc = dtstr.count(' ')
    assert (tc + sc) > 1, (
        "The date and time can be seperated by either an uppercase 'T' "
        "or a space ' ', both were found in the string.")

    if dc == 2:              # YYYY-MM-DD
        month = dtstr[5:7]
        day = dtstr[8:10]
    elif dc == 1 and not wc: # YYYY-MM
        month = dtstr[5:7]
    elif dc == 0 and not wc: # YYYYMMDD
        month = dtstr[4:6]
        day = dtstr[7:9]
    elif dc == 1 and wc:     # YYYY-Www
        week = dtstr[6:8]





    assert len(dtstr) in (7, 8, 10), (
        "Must be a string of lengths 7, 8, or 10")
    has_sep = dtstr[4] == '-'
    pos = 4 + has_sep

    if dtstr[pos:pos + 1] == "W":
        # YYYY-?Www-?D?
        pos += 1
        weekno = int(dtstr[pos:pos + 2])
        pos += 2
        dayno = 1

        if len(dtstr) > pos:
            sd = dtstr[pos:pos + 1]

            if (sd == '-') != has_sep:
                raise ValueError(
                    f"Inconsistent use of dash separator, found {sd}")

            pos += has_sep

            dayno = int(dtstr[pos:pos + 1])

        return list(_isoweek_to_gregorian(year, weekno, dayno))
    else:
        month = int(dtstr[pos:pos + 2])
        pos += 2
        sd = dtstr[pos:pos + 1]

        if (sd == "-") != has_sep:
            raise ValueError(f"Inconsistent use of dash separator, found {sd}")

        pos += has_sep
        day = int(dtstr[pos:pos + 2])
        return [year, month, day]


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


def _isoweek1monday(year):
    """
    Helper to calculate the day number of the Monday starting week 1
    XXX This could be done more efficiently
    """
    THURSDAY = 3
    firstday = _ymd2ord(year, 1, 1)
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
        _check_date_fields(self, a, b, c, d, e)

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
        return cls(*date[:-3]) # We do not need any time values.

    @classmethod
    def today(cls):
        "Construct a date from time.time()."
        t = _time.time()
        return cls.fromtimestamp(t)

    ## @classmethod
    ## def fromordinal(cls, n, *, short=False):
    ##     """
    ##     Construct a date from a proleptic Badi ordinal.

    ##     Bahá 1 of year 1 is day 1. Only the year, month and day are
    ##     non-zero in the result. It is more difficult to do this in the
    ##     Badi Calendar because a Badi day can be more or less than 24
    ##     hours depending on when sunset is.
    ##     """
    ##     bc = BahaiCalendar()
    ##     jd0 = bc.jd_from_badi_date(bc.BADI_EPOCH - 1 + n - 1)
    ##     jd1 = bc.jd_from_badi_date(bc.BADI_EPOCH - 1 + n)
    ##     jd2 = bc.jd_from_badi_date(bc.BADI_EPOCH - 1 + n + 1)


    ##     date = bc.badi_date_from_jd(jd, short=short)

    ##     print(date)

    ##     return cls(*date)

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
            return cls(*_parse_isoformat_date(date_string))
        except Exception:
            raise ValueError(f'Invalid isoformat string: {date_string!r}')

    @classmethod
    def fromisocalendar(cls, year, week, day):
        """Construct a date from the ISO year, week number and weekday.

        This is the inverse of the date.isocalendar() function"""
        return cls(*_isoweek_to_gregorian(year, week, day))

    # Conversions to string

    def __repr__(self):
        """Convert to formal string, for repr().

        >>> d = date(2010, 1, 1)
        >>> repr(d)
        'datetime.date(2010, 1, 1)'
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

