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

def _days_before_year(bc:BahaiCalendar, year:int) -> float:
    """
    Get the number of days before the 1st of Baha of the year.

    :param bc: BahaiCalendar instance.
    :type bc: object
    :param year: Badi year
    :type year: int
    :return: The number of days since (-1841, 19, 19) of the Badi calendar.
    :rtype: int
    """
    jd0 = bc.jd_from_badi_date((MINYEAR-1, 19, 19))
    jd1 = bc.jd_from_badi_date((year, 1, 1))
    return _math.floor(jd1 - jd0) - 1

def _days_in_month(bc:BahaiCalendar, year:int, month:int) -> int:
    """
    The number of days in that month in that year.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param year: Badi year
    :type year: int
    :param month: Badi month (0..19)
    :type month: int
    :return: The number of in the current month.
    :rtype: int
    """
    return 4 + bc._is_leap_year(year) if month == 0 else 19

def _days_before_month(bc:BahaiCalendar, year:int, month:int) -> int:
    """
    The number of days in the year preceding the first day of month.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param year: Badi year
    :type year: int
    :param month: Badi month (0..19)
    :type month: int
    :return: The number in the year preceding the first day of month.
    :rtype: int
    """
    assert 0 <= month <= 19, "Month must be in range of 0..19"
    month -= -18 if month < 2 else 1 if 1 < month < 19 else 19
    dbm = 0

    if 0 < month < 19:
        dbm += month * 19
    elif month == 0:
        dbm += 18 * 19 + 4 + bc._is_leap_year(year)

    return dbm

def _ymd2ord(bc:BahaiCalendar, year:int, month:int, day:int) -> int:
    """
    Get the number of days since Badi year -1842 (Gregorian 0001-03-20)
    including the current day.

    year, month, day -> ordinal, considering -1842-01-01 as day 1.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param year: Badi year
    :type year: int
    :param month: Badi month (0..19)
    :type month: int
    :param day: Badi day
    :type day: int
    :return: The number of days since Badi year -1842 including the
             current day.
    :rtype: int
    """
    assert 0 <= month <= 19, "Month must be in range of 0..19"
    dim = _days_in_month(bc, year, month)
    assert 1 <= day <= dim, (
        f"Day for month {month} must be in range of 1..{dim}")
    return (_days_before_year(bc, year)
            + _days_before_month(bc, year, month) + day)

def _ord2ymd(bc:BahaiCalendar, n:int, *, short:bool=False) -> tuple:
    """
    It is more difficult to do this in the Badi Calendar because a Badi
    day can be more or less than 24 hours depending on when sunset is
    and the time of the year. From the summer Solstis to the winter solstis
    the days get shorter so it's less than 24 hours and the inverse between
    the winter solstis and the summer solstis. As such we just use the
    BadiCalendar API.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param n: The ordinal number of days from the MINYEAR.
    :type n: int
    :param short: If True then parse for a short date else if False
                  parse for a long date.
    :type short: bool
    :return: The Badi date.
    :rtype: tuple
    """
    jd = bc.jd_from_badi_date((MINYEAR-1, 19, 19)) + n
    return bc.badi_date_from_jd(jd, short=short)

def _isoweek_to_badi(bc:BahaiCalendar, year:int, week:int, day:int, *,
                     short:bool=False) -> tuple:
    """
    We count the week from Jalal (Staurday) as the first day and
    Istiqlal (Friday) the last day of the week. This is different from
    the usual way ISO weeks are counted which is Monday to Sunday.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param year: Badi year
    :type year: int
    :param month: Badi month (0..19)
    :type month: int
    :param day: Badi day
    :type day: int
    :param short: If True then parse for a short date else if False
                  parse for a long date.
    :type short: bool
    :return: A Badi date.
    :rtype: tuple
    """
    if not 0 < week < 53:
        out_of_range = True

        if week == 53:
            # ISO years have 53 weeks in them on years starting with a
            # Fidal (Tuesday) and leap years starting on a Kamal
            # (Monday). Badi weeks start on Jalal (Saturday).
            first_weekday = _ymd2ord(bc, year, 1, 1) % 7

            if (first_weekday == 4 or
                (first_weekday == 3 and bc._is_leap_year(year))):
                out_of_range = False

        assert not out_of_range, f"Invalid week: {week}"

    assert 0 < day < 8, f"Invalid weekday: {day} (range is 1..7)"
    # Now compute the offset from (Y, 1, 1) in days:
    day_offset = (week - 1) * 7 + (day - 1)
    # Calculate the ordinal day for Jalal, week 1
    day_1 = _isoweek1jalal(bc, year)
    ord_day = day_1 + day_offset
    return _ord2ymd(bc, ord_day, short=short)

def _isoweek1jalal(bc:BahaiCalendar, year:int) -> int:
    """
    Calculate the day number of the first Jalal (Saturday) in the year
    which would be the first week with 4 or more days in the year in
    question.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param year: Badi year
    :type year: int
    :return: The number of the first Jalal in the Badi year.
    :rtype: int
    """
    firstday = _ymd2ord(bc, year, 1, 1)
    firstweekday = (firstday + 3) % 7
    week1jalal = firstday - firstweekday

    if firstweekday > 3: # First week day > Fidal
        week1jalal += 7

    return week1jalal

def _parse_isoformat_date_time(bc:BahaiCalendar, dtstr:str) -> tuple:
    """
    """
    assert ((tc + sc) > 1) or ((tc + sc) == 0), (
        "The date and time can be seperated by either an uppercase 'T' "
        "or a space ' ', both were found in the string.")


def _parse_isoformat_date(bc:BahaiCalendar, dtstr:str) -> tuple:
    """
    Parse a date ISO formatted string.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param dtstr: A ISO complient time string.
    :type dtstr: str
    :return: The year, month, and day parsed from a ISO string.
    :rtype: tuple
    """
    neg = dtstr[0] == '-'
    year = int(dtstr[:4 + neg])
    assert MINYEAR <= year <= MAXYEAR, (
        f"Year is out of range: {year}, min {MINYEAR}, max {MAXYEAR}.")
    dtstr = dtstr[1:] if neg else dtstr
    dc = dtstr.count('-')
    wc = dtstr.count('W')
    assert dc in (0, 1, 2) or (wc == 1 and dc in (0, 1)), (
        "Invalid format, there must be between 1 and 3 hyphons (-) in the "
        "date format or there can be one uppercase (W) week identifier and "
        "no or one hyphon (-) used.")
    d_len = len(dtstr)

    # Parse the date
    if dc == 1 and d_len == 7 and not wc:   # YYYY-MM
        date = (year, int(dtstr[5:7]), 1)
    elif dc == 0 and d_len == 8 and not wc: # YYYYMMDD
        date = (year, int(dtstr[4:6]), int(dtstr[7:9]))
    elif dc == 2 and not wc:                # YYYY-MM-DD
        date = (year, int(dtstr[5:7]), int(dtstr[8:10]))
    elif wc and (7 <= d_len <=10 or (tc and 7 <= dtstr.index('T') <= 10)):
        # YYYYWww, YYYY-Www, YYYYWwwD, YYYY-Www-D
        pos = 5 if dc == 0 else 6
        wday = int(dtstr[pos:pos+2])
        pos += 2 if dc == 0 else 3
        d = dtstr[pos:pos+1]
        day = int(d) if d.isdigit() else 1
        date = _isoweek_to_badi(bc, year, wday, day, short=True)[:3]
    # YYYYDDD or YYYY-DDD
    elif d_len in (7, 8) or (tc and dtstr.index('T') in (7, 8)):
        month_days = [(n, 19) for n, v in bc.BADI_MONTH_NAMES]
        month_days[18] = (0, 4 + bc._is_leap_year(year))
        days = int(dtstr[4:7] if dc == 0 else dtstr[5:8])

        for month, ds in month_days:
            if days <= ds: break
            days -= ds

        date = (year, month, days)
    else:
        date = ()

    return date

def _parse_isoformat_time(bc:BahaiCalendar, dtstr:str) -> tuple:
    """
    Parse a time ISO formatted string.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param dtstr: A ISO complient time string.
    :type dtstr: str
    :return: The hour, minute, and second parsed from an ISO string.
    :rtype: tuple
    """
    tc = dtstr.count('T')
    sc = dtstr.count(' ')
    assert ((tc == 0 and sc == 0) or
            (tc == 1 or sc == 1) and (tc or sc) and not (tc and sc)), (
        "Cannot have both a 'T' and a space or more than one of either to "
        "indicate time.")

    if sc:
        dtstr[dtstr.index(' ')] = 'T'
        del sc
        tc = 1

    cc = dtstr.count(':')
    assert cc < 3, f"Invalid number of colons (:), can be 0 - 2, found {cc}"
    pc = dtstr.count('.')
    assert pc <= 1, f"Invalid number of dots (.), can be 0 - 1, found {pc}"
    t_len = len(dtstr)

    if t_len > 2:
        hour = int(dtstr[1:3])
        pos0 = 1 if cc else 0
        pos1 = 2 if cc == 2 else 0

        if t_len > 3:
            if dtstr[3] == '.': # Thh.hhh
                ph = float(dtstr[3:]) * 60
                minute = _math.floor(ph)
                second = (ph % 1) * 60
                time = (hour, minute, second)
            elif dtstr[5+pos0:6+pos0] == '.': # Thhmm.mmm or Thh:mm.mmm
                minute = int(dtstr[3+pos0:5+pos0])
                pm = float(dtstr[5+pos0:])
                second = pm * 60
                time = (hour, minute, second)
            elif dtstr[7+pos0:8+pos0] == '.': # Thhmmss.sss or Thh:mm:ss.sss
                minute = int(dtstr[3+pos0:5+pos0])
                second = float(dtstr[5+pos0:])
                time = (hour, minute, second)
            elif t_len == 5+pos0: # Thhmm or Thh:mm
                minute = int(dtstr[3+pos0:5+pos0])
                time = (hour, minute, 0)
            elif t_len >= 7+pos1: # Thhmmss.sss or Thh:mm:ss.sss
                minute = int(dtstr[3+pos0:5+pos0])
                second = float(dtstr[5+pos1:])
                time = (hour, minute, second)
            else:
                raise ValueError(f"Invalid time string, found {dtstr}")
        else: # Thh
            time = (hour, 0, 0)
    else:
        time = ()

    return time

def _check_date_fields(bc:BahaiCalendar, a:int, b:int, c:int, d:int=None,
                       e:int=None) -> None:
    """
    Check the validity of the date.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param a: The long form Kull-i-Shay or short form year.
    :type a: int
    :param b: The long form Váḥid or short form month.
    :type b: int
    :param c: The long form year or short form day.
    :type c: int
    :param d: The long form month.
    :type d: int
    :param e: The long form day.
    :return: Nothing
    :rtype: None
    :raises AssertionError: If any of the year values are out of range.
    """
    if d == None or e == None:
        b_date = bc.long_date_from_short_date((a, b, c))
    else:
        b_date = (a, b, c, d, e)

    bc._check_valid_badi_month_day(b_date)


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
        non-zero in the result.
        """
        bc = BahaiCalendar()
        y, m, d = _ord2ymd(bc, n)
        del bc
        return cls(y, m, d)

    @classmethod
    def fromisoformat(cls, date_string):
        """
        Construct a date from a string in ISO 8601 format.
        We only can convert from short form Badi dates.
        """
        if not isinstance(date_string, str):
            raise TypeError('fromisoformat: argument must be a string.')

        if date_string.count('T') > 0 or date_string.count(' ') > 0:
            raise ValueError("A time indicator was found, this is invalid "
                             "for date parsing, isoformat string: "
                             f"{date_string!r}.")

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

