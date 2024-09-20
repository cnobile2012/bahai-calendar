# -*- coding: utf-8 -*-
#
# badidatetime/datetime.py
#
__docformat__ = "restructuredtext en"

import time as _time
import math as _math

from .badi_calendar import BahaiCalendar
from .structures import struct_time


MINYEAR = -1842
MAXYEAR = 1161
_MAXORDINAL = 1097267 # date.max.toordinal()
# This keeps the Badi day count in par with the Gregorian day count.
DAYS_BEFORE_1ST_YEAR = 78
DAYNAMES = ('Jalál', 'Jamál', 'Kamál', 'Fiḍāl',
            '`Idāl', 'Istijlāl', 'Istiqlāl')


def _cmp(x, y):
    return 0 if x == y else 1 if x > y else -1

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

def _day_of_week(bc:BahaiCalendar, year:int, month:int, day:int) -> int:
    """
    Find the day of the week where 0 == Jalál (Saturday) and
    6 == Istiqlāl (Friday). For ISO compatability add 1 to the result.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param year: Badi year
    :type year: int
    :param month: Badi month (0..19)
    :type month: int
    :param day: Badi day
    :type day: int
    :return: The numerical day of the week.
    :rtype: int
    """
    # Since the usual start day is Monday (Kamál) a properly aligned
    # day number to the day name we need to add 1 to the ordinal.
    return ((_ymd2ord(bc, year, month, day) + 1) % 7 + 7) % 7

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
    # We add 78 days to the total so that the ordinal number can be
    # compared to the ordinals in the standard datetime package.
    return (DAYS_BEFORE_1ST_YEAR + _days_before_year(bc, year) +
            _days_before_month(bc, year, month) + day)

def _ord2ymd(bc:BahaiCalendar, n:int, *, short:bool=False) -> tuple:
    """
    It is more difficult to do this in the Badi Calendar because a Badi
    day can be more or less than 24 hours depending on when sunset is
    and the time of the year. From the summer Solstice to the winter
    Solstice the days get shorter. The day slowly comes down to 24 hours
    around the Fall Equinox and then below 24 hours. The inverse happens
    between the Winter Solstice and the Summer Solstice. We just use the
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
    # We subtract 78 days from the total so that the Badi date will
    # be the same as the date value passed into _ymd2ord.
    jd = bc.jd_from_badi_date((MINYEAR-1, 19, 19)) - DAYS_BEFORE_1ST_YEAR + n
    return bc.badi_date_from_jd(jd, short=short, rtd=True)

def _isoweek_to_badi(bc:BahaiCalendar, year:int, week:int, day:int, *,
                     short:bool=False) -> tuple:
    """
    The week counts from Jalal (Saturday) as the first day and Istiqlal
    (Friday) the last day of the week. This is different from the usual
    way ISO weeks are counted which is Monday to Sunday.

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
    :raises AssertionError: If the week or weekday is out of range.
    """
    if not 0 < week < 53:
        out_of_range = True

        if week == 53:
            # ISO years have 53 weeks in them on years starting with a
            # Fidal (Tuesday) and leap years starting on a Kamal
            # (Monday). Badi weeks start on Jalal (Saturday).
            first_weekday = _ymd2ord(bc, year, 1, 1) % 7

            if (first_weekday == 4 or (first_weekday == 3 and
                                       bc._is_leap_year(year))):
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
    Calculate the day number of Jalal (Saturday) starting week 1. It
    would be the first week with 4 or more days in the year in question.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param year: Badi year
    :type year: int
    :return: The number of the first Jalal in the Badi year.
    :rtype: int
    """
    firstday = _ymd2ord(bc, year, 1, 1)
    firstweekday = (firstday - 6) % 7
    week1jalal = firstday - firstweekday

    if firstweekday > 3: # First week day >= Fidal
        week1jalal += 7

    return week1jalal

def _parse_isoformat_date_time(bc:BahaiCalendar, dtstr:str) -> tuple:
    """
    Parse both the date and time represented in an ISO string into a
    date and time tuple.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param dtstr: A ISO compliant time string.
    :type dtstr: str
    :return: The year, month, and day parsed from a ISO string.
    :rtype: tuple

    """
    tc = dtstr.count('T')
    sc = dtstr.count(' ')
    idx = dtstr.index('T') if tc else dtstr.index(' ') if sc else len(dtstr)
    str_date = dtstr[:idx].strip('T ')
    str_time = dtstr[idx:]
    date = _parse_isoformat_date(bc, str_date) if str_date else ()
    time = _parse_isoformat_time(bc, str_time) if str_time else ()
    return date + time

def _parse_isoformat_date(bc:BahaiCalendar, dtstr:str) -> tuple:
    """
    Parse a date ISO formatted string.

    :param bc: BahaiCalendar instance.
    :type bc: BahaiCalendar
    :param dtstr: A ISO compliant time string.
    :type dtstr: str
    :return: The year, month, and day parsed from a ISO string.
    :rtype: tuple
    :raises AssertionError: Raised when the year is out of range or when too
                            many hyphens are used.
    :raises IndexError: When a string index is out of range.
    :raises ValueError: Raised when an invalid string is being parsed to
                        an integer or when an invalid ISO string is being
                        parsed.
    """
    if dtstr != '':
        neg = dtstr[0] == '-'
        year = int(dtstr[:4 + neg])
        assert MINYEAR <= year <= MAXYEAR, (
            f"Year is out of range: {year}, min {MINYEAR}, max {MAXYEAR}.")
        dtstr = dtstr[1:] if neg else dtstr

    dc = dtstr.count('-')
    wc = dtstr.count('W')
    assert (wc == 0 and dc in (0, 1, 2)) or (wc == 1 and dc in (0, 1, 2)), (
        "Invalid format, there must be between 0 to 2 hyphens (-) in the "
        "date format or there can be one uppercase (W) week identifier and "
        "between 0 and 2 hyphens (-) used.")
    d_len = len(dtstr)

    if dc == 1 and d_len == 7 and not wc:   # YYYY-MM
        date = (year, int(dtstr[5:7]), 1)
    elif dc == 0 and d_len == 8 and not wc: # YYYYMMDD
        date = (year, int(dtstr[4:6]), int(dtstr[7:9]))
    elif dc == 2 and not wc:                # YYYY-MM-DD
        date = (year, int(dtstr[5:7]), int(dtstr[8:10]))
    elif wc and 7 <= d_len <=10:
        # YYYYWww, YYYY-Www, YYYYWwwD, YYYY-Www-D
        pos = 5 if dc == 0 else 6
        wday = int(dtstr[pos:pos+2])
        pos += 2 if dc == 0 else 3
        d = dtstr[pos:]
        assert (dc == 1 and d_len == 8) or dc in (0, 2), (
            f"Invalid ISO string {dtstr}.")
        day = int(d) if d.isdigit() else 1
        date = _isoweek_to_badi(bc, year, wday, day, short=True)[:3]
    # YYYYDDD or YYYY-DDD
    elif d_len in (7, 8):
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
    :param dtstr: A ISO compliant time string.
    :type dtstr: str
    :return: The hour, minute, and second parsed from an ISO string.
    :rtype: tuple
    :raises AssertionError: Raised when there are invalid time designators,
                            when to many colons used, or when too many dots
                            are used.
    :raises ValueError: Raised when an invalid string is being parsed to
                        an integer or when an invalid ISO string is being
                        parsed.
    """
    tc = dtstr.count('T')
    sc = dtstr.count(' ')
    assert ((tc == 0 and sc == 0) or
            (tc == 1 or sc == 1) and (tc or sc) and not (tc and sc)), (
        "Cannot have both a 'T' and a space or more than one of either to "
        "indicate time.")

    if sc:
        dtstr = "T" + dtstr[1:]
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

    bc._check_valid_badi_date(b_date)

def _wrap_strftime(object, format, timetuple):
    """
    Correctly substitute for %z and %Z escapes in strftime formats.
    """
    # Don't call utcoffset() or tzname() unless actually needed.
    freplace = None  # the string to use for %f
    zreplace = None  # the string to use for %z
    Zreplace = None  # the string to use for %Z

    # Scan format for %z and %Z escapes, replacing as needed.
    newformat = []
    push = newformat.append
    i, n = 0, len(format)

    while i < n:
        ch = format[i]
        i += 1

        if ch == '%':
            if i < n:
                ch = format[i]
                i += 1

                if ch == 'f':
                    if freplace is None:
                        freplace = '%06d' % getattr(object, 'microsecond', 0)

                    newformat.append(freplace)
                elif ch == 'z':
                    if zreplace is None:
                        zreplace = ""

                        if hasattr(object, "utcoffset"):
                            offset = object.utcoffset()

                            if offset is not None:
                                sign = '+'

                                if offset.days < 0:
                                    offset = -offset
                                    sign = '-'

                                h, rest = divmod(offset, timedelta(hours=1))
                                m, rest = divmod(rest, timedelta(minutes=1))
                                s = rest.seconds
                                u = offset.microseconds

                                if u:
                                    zreplace = '%c%02d%02d%02d.%06d' % (
                                        sign, h, m, s, u)
                                elif s:
                                    zreplace = '%c%02d%02d%02d' % (
                                        sign, h, m, s)
                                else:
                                    zreplace = '%c%02d%02d' % (sign, h, m)

                    assert '%' not in zreplace
                    newformat.append(zreplace)
                elif ch == 'Z':
                    if Zreplace is None:
                        Zreplace = ""

                        if hasattr(object, "tzname"):
                            s = object.tzname()

                            if s is not None:
                                # strftime is going to have at this: escape %
                                Zreplace = s.replace('%', '%%')

                    newformat.append(Zreplace)
                else:
                    push('%')
                    push(ch)
            else:
                push('%')
        else:
            push(ch)

    newformat = "".join(newformat)
    return _time.strftime(newformat, timetuple)

def _divide_and_round(a, b):
    """
    Divide a by b and round result to the nearest integer.

    When the ratio is exactly half-way between two integers,
    the even integer is returned.
    """
    # Based on the reference implementation for divmod_near
    # in Objects/longobject.c.
    q, r = divmod(a, b)
    # round up if either r / b > 0.5, or r / b == 0.5 and q is odd.
    # The expression r / b > 0.5 is equivalent to 2 * r > b if b is
    # positive, 2 * r < b if b negative.
    r *= 2
    greater_than_half = r > b if b > 0 else r < b
    return q + (1 if greater_than_half or r == b and q % 2 == 1 else 0)


class timedelta:
    """
    Represent the difference between two datetime objects.

    Supported operators:

    - add, subtract timedelta
    - unary plus, minus, abs
    - compare to timedelta
    - multiply, divide by int

    In addition, datetime supports subtraction of two datetime objects
    returning a timedelta, and addition or subtraction of a datetime
    and a timedelta giving a datetime.

    Representation: (days, seconds, microseconds).  Why?  Because I
    felt like it.
    """
    __slots__ = '_days', '_seconds', '_microseconds', '_hashcode'

    def __new__(cls, days=0, seconds=0, microseconds=0,
                milliseconds=0, minutes=0, hours=0, weeks=0):
        # Doing this efficiently and accurately in C is going to be difficult
        # and error-prone, due to ubiquitous overflow possibilities, and that
        # C double doesn't have enough bits of precision to represent
        # microseconds over 10K years faithfully.  The code here tries to make
        # explicit where go-fast assumptions can be relied on, in order to
        # guide the C implementation; it's way more convoluted than speed-
        # ignoring auto-overflow-to-long idiomatic Python could be.

        # XXX Check that all inputs are ints or floats.

        # Final values, all integer.
        # s and us fit in 32-bit signed ints; d isn't bounded.
        d = s = us = 0

        # Normalize everything to days, seconds, microseconds.
        days += weeks*7
        seconds += minutes*60 + hours*3600
        microseconds += milliseconds*1000

        # Get rid of all fractions, and normalize s and us.
        # Take a deep breath <wink>.
        if isinstance(days, float):
            dayfrac, days = _math.modf(days)
            daysecondsfrac, daysecondswhole = _math.modf(dayfrac * (24.*3600.))
            assert daysecondswhole == int(daysecondswhole)  # can't overflow
            s = int(daysecondswhole)
            assert days == int(days)
            d = int(days)
        else:
            daysecondsfrac = 0.0
            d = days

        assert isinstance(daysecondsfrac, float)
        assert abs(daysecondsfrac) <= 1.0
        assert isinstance(d, int)
        assert abs(s) <= 24 * 3600
        # days aren't referenced again before redefinition

        if isinstance(seconds, float):
            secondsfrac, seconds = _math.modf(seconds)
            assert seconds == int(seconds)
            seconds = int(seconds)
            secondsfrac += daysecondsfrac
            assert abs(secondsfrac) <= 2.0
        else:
            secondsfrac = daysecondsfrac

        # daysecondsfrac isn't referenced again
        assert isinstance(secondsfrac, float)
        assert abs(secondsfrac) <= 2.0

        assert isinstance(seconds, int)
        days, seconds = divmod(seconds, 24*3600)
        d += days
        s += int(seconds)    # can't overflow
        assert isinstance(s, int)
        assert abs(s) <= 2 * 24 * 3600
        # seconds isn't referenced again before redefinition

        usdouble = secondsfrac * 1e6
        assert abs(usdouble) < 2.1e6    # exact value not critical
        # secondsfrac isn't referenced again

        if isinstance(microseconds, float):
            microseconds = round(microseconds + usdouble)
            seconds, microseconds = divmod(microseconds, 1000000)
            days, seconds = divmod(seconds, 24*3600)
            d += days
            s += seconds
        else:
            microseconds = int(microseconds)
            seconds, microseconds = divmod(microseconds, 1000000)
            days, seconds = divmod(seconds, 24*3600)
            d += days
            s += seconds
            microseconds = round(microseconds + usdouble)

        assert isinstance(s, int)
        assert isinstance(microseconds, int)
        assert abs(s) <= 3 * 24 * 3600
        assert abs(microseconds) < 3.1e6

        # Just a little bit of carrying possible for microseconds and seconds.
        seconds, us = divmod(microseconds, 1000000)
        s += seconds
        days, s = divmod(s, 24*3600)
        d += days

        assert isinstance(d, int)
        assert isinstance(s, int) and 0 <= s < 24*3600
        assert isinstance(us, int) and 0 <= us < 1000000

        if abs(d) > 999999999:
            raise OverflowError("timedelta # of days is too large: %d" % d)

        self = object.__new__(cls)
        self._days = d
        self._seconds = s
        self._microseconds = us
        self._hashcode = -1
        return self

    def __repr__(self):
        args = []

        if self._days:
            args.append(f"days={self._days}")

        if self._seconds:
            args.append(f"seconds={self._seconds}")

        if self._microseconds:
            args.append(f"microseconds={self._microseconds}")

        if not args:
            args.append('0')

        return (f"{self.__class__.__module__}.{self.__class__.__qualname__}"
                f"({', '.join(args)})")

    def __str__(self):
        mm, ss = divmod(self._seconds, 60)
        hh, mm = divmod(mm, 60)
        s = f'{hh:d}:{mm:02d}:{ss:02d}'

        if self._days:
            def plural(n):
                return n, abs(n) != 1 and "s" or ""

            day, p = plural(self._days)
            s = f'{day:d} day{p:s}, ' + s

        if self._microseconds:
            s = s + f'.{self._microseconds:06d}'

        return s

    def total_seconds(self):
        """Total seconds in the duration."""
        return ((self.days * 86400 + self.seconds) * 10**6 +
                self.microseconds) / 10**6

    # Read-only field accessors
    @property
    def days(self):
        """days"""
        return self._days

    @property
    def seconds(self):
        """seconds"""
        return self._seconds

    @property
    def microseconds(self):
        """microseconds"""
        return self._microseconds

    def __add__(self, other):
        if isinstance(other, timedelta):
            # for CPython compatibility, we cannot use
            # our __class__ here, but need a real timedelta
            return timedelta(self._days + other._days,
                             self._seconds + other._seconds,
                             self._microseconds + other._microseconds)

        return NotImplemented

    __radd__ = __add__

    def __sub__(self, other):
        if isinstance(other, timedelta):
            # for CPython compatibility, we cannot use
            # our __class__ here, but need a real timedelta
            return timedelta(self._days - other._days,
                             self._seconds - other._seconds,
                             self._microseconds - other._microseconds)

        return NotImplemented

    def __rsub__(self, other):
        return -self+other if isinstance(other, timedelta) else NotImplemented

    def __neg__(self):
        # for CPython compatibility, we cannot use
        # our __class__ here, but need a real timedelta
        return timedelta(-self._days, -self._seconds, -self._microseconds)

    def __pos__(self):
        return self

    def __abs__(self):
        return -self if self._days < 0 else self

    def __mul__(self, other):
        if isinstance(other, int):
            # for CPython compatibility, we cannot use
            # our __class__ here, but need a real timedelta
            return timedelta(self._days * other,
                             self._seconds * other,
                             self._microseconds * other)

        if isinstance(other, float):
            usec = self._to_microseconds()
            a, b = other.as_integer_ratio()
            return timedelta(0, 0, _divide_and_round(usec * a, b))

        return NotImplemented

    __rmul__ = __mul__

    def _to_microseconds(self):
        return ((self._days * (24*3600) + self._seconds) * 1000000 +
                self._microseconds)

    def __floordiv__(self, other):
        if not isinstance(other, (int, timedelta)):
            return NotImplemented

        usec = self._to_microseconds()

        if isinstance(other, timedelta):
            return usec // other._to_microseconds()

        if isinstance(other, int):
            return timedelta(0, 0, usec // other)

    def __truediv__(self, other):
        if not isinstance(other, (int, float, timedelta)):
            return NotImplemented

        usec = self._to_microseconds()

        if isinstance(other, timedelta):
            return usec / other._to_microseconds()

        if isinstance(other, int):
            return timedelta(0, 0, _divide_and_round(usec, other))

        if isinstance(other, float):
            a, b = other.as_integer_ratio()
            return timedelta(0, 0, _divide_and_round(b * usec, a))

    def __mod__(self, other):
        if isinstance(other, timedelta):
            r = self._to_microseconds() % other._to_microseconds()
            return timedelta(0, 0, r)

        return NotImplemented

    def __divmod__(self, other):
        if isinstance(other, timedelta):
            q, r = divmod(self._to_microseconds(),
                          other._to_microseconds())
            return q, timedelta(0, 0, r)

        return NotImplemented

    # Comparisons of timedelta objects with other.

    def __eq__(self, other):
        return (self._cmp(other) == 0 if isinstance(other, timedelta)
                else NotImplemented)

    def __le__(self, other):
        return (self._cmp(other) <= 0 if isinstance(other, timedelta)
                else NotImplemented)

    def __lt__(self, other):
        return (self._cmp(other) < 0 if isinstance(other, timedelta)
                else NotImplemented)

    def __ge__(self, other):
        return (self._cmp(other) >= 0 if isinstance(other, timedelta)
                else NotImplemented)

    def __gt__(self, other):
        return (self._cmp(other) > 0 if isinstance(other, timedelta)
                else NotImplemented)

    def _cmp(self, other):
        assert isinstance(other, timedelta)
        return _cmp(self._getstate(), other._getstate())

    def __hash__(self):
        if self._hashcode == -1:
            self._hashcode = hash(self._getstate())

        return self._hashcode

    def __bool__(self):
        return (self._days != 0 or
                self._seconds != 0 or
                self._microseconds != 0)

    # Pickle support.

    def _getstate(self):
        return (self._days, self._seconds, self._microseconds)

    def __reduce__(self):
        return (self.__class__, self._getstate())

timedelta.min = timedelta(-999999999)
timedelta.max = timedelta(days=999999999, hours=23, minutes=59, seconds=59,
                          microseconds=999999)
timedelta.resolution = timedelta(microseconds=1)


class date(BahaiCalendar):
    """
    Implements the date object for the Badi datetime package.
    """
    #__slots__ = '_kull_i_shay, _vahid, _year', '_month', '_day', '_hashcode'

    def __new__(cls, a:int, b:int=None, c:int=None, d:int=None,
                e:int=None) -> object:
        """
        Instantiate the class.

        :param cls: The class object.
        :type cls: date class
        :param a: Long form this value is the Kill-i-Shay and short form
                  it's the year. If b and c are None then a becomes the
                  pickle value that is parsed to the remaining values below.
        :type a: int
        :param b: Long form this value is the Váḥid and short form it's
                  the month.
        :type b: int
        :param c: Long form this is the year and short form it's the day.
        :type c: int
        :param d: Long for this value is the month and in the short form
                  it's not used.
        :type d: int
        :param e: Long form this value is the day and in the short form
                  it's not used.
        :type e: int
        :return: The instantiated class.
        :rtype: date
        """
        # Pickle support
        if (short := date.is_pickle_data(a, b)) is not None:
            self = object.__new__(cls)
            self.__short = short
            self.__setstate(a)
        else:
            b_date = tuple([x for x in (a, b, c, d, e) if x is not None])
            date_len = len(b_date)
            assert date_len in (3, 5), (
                "A full short or long form Badi date must be used, found "
                f"{date_len} fields.")
            self = object.__new__(cls)

            if date_len == 5:
                self._kull_i_shay = a
                self._vahid = b
                self._year = c
                self._month = d
                self._day = e
                self.__date = b_date
                self.__short = False
            else:
                self._year = a
                self._month = b
                self._day = c
                self.__date = b_date
                self.__short = True

        self._MONTHNAMES = {num: name for num, name in self.BADI_MONTH_NAMES}
        super().__init__(self)
        _check_date_fields(self, *self.__date)
        self._hashcode = -1
        return self

    @property
    def is_short(self):
        return self.__short

    # Additional constructors

    @classmethod
    def fromtimestamp(cls, t:float, *, short:bool=False) -> object:
        """
        Construct a date from a POSIX timestamp (like time.time()).

        :param cls: The class object.
        :type cls: date class
        :param t: The POSIX timestamp.
        :type t: float
        :param short: If True the short for is returned. The default
                      is False.
        :type short: bool
        :return: The instantiated class.
        :rtype: date
        """
        bc = BahaiCalendar()
        date = bc.posix_timestamp(t, short=short)
        del bc
        return cls(*date[:-3]) # We do not need any time values.

    @classmethod
    def today(cls, *, short:bool=False) -> object:
        """
        Construct a date from time.time().

        :param cls: The class object.
        :type cls: date class
        :param short: If True the short for is returned. The default
                      is False.
        :type short: bool
        :return: The instantiated class.
        :rtype: date
        """
        return cls.fromtimestamp(_time.time(), short=short)

    @classmethod
    def fromordinal(cls, n:int, *, short:bool=False) -> object:
        """
        Construct a date from a proleptic Badi ordinal.

        Bahá 1 of year 1 is day 1. Only the year, month and day are
        non-zero in the result.

        :param cls: The class object.
        :type cls: date class
        :param n:
        :type n:
        :param short: If True the short for is returned. The default
                      is False.
        :type short: bool
        :return: The instantiated class.
        :rtype: date
        """
        bc = BahaiCalendar()
        date = _ord2ymd(bc, n, short=short)
        del bc
        return cls(*date)

    @classmethod
    def fromisoformat(cls, date_string:str, *, short:bool=False) -> object:
        """
        Construct a date from a string in ISO 8601 format.
        We only can convert from short form Badi dates.
        """
        if not isinstance(date_string, str):
            raise TypeError("fromisoformat: argument must be a string.")

        if date_string.count('T') > 0 or date_string.count(' ') > 0:
            raise ValueError("A time indicator was found, this is invalid "
                             "for date parsing, isoformat string: "
                             f"{date_string!r}.")

        try:
            bc = BahaiCalendar()
            date = _parse_isoformat_date(bc, date_string)
        except Exception as e:
            del bc
            raise ValueError(str(e))
        else:
            if date == ():
                raise ValueError(f"Invalid isoformat string: {date_string!r}.")

            if short:
                b_date = date
            else:
                b_date = bc.long_date_from_short_date(date)
                del bc

            return cls(*b_date)

    @classmethod
    def fromisocalendar(cls, year:int, week:int, day:int, *,
                        short:bool=False) -> object:
        """
        Construct a date from the ISO year, week number and weekday.

        This is the inverse of the date.isocalendar() function
        """
        bc = BahaiCalendar()
        date = _isoweek_to_badi(bc, year, week, day, short=short)
        del bc
        b_date = date[:3] if short else date[:5]
        return cls(*b_date)

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

    def __short_from_long_form(self):
        """
        Convert the long form Badi date to a short form Badi date.
        """
        return (self.__date if self.__short else
                self.short_date_from_long_date(self.__date))

    def ctime(self):
        """
        Return ctime() style string in the short form Badi date.
        """
        date = self.__short_from_long_form()
        weekday = _day_of_week(self, *date[:3])
        wd_name = DAYNAMES[weekday]
        year, month, day = date[:3]
        m_name = self._MONTHNAMES[month]
        y_shim = 4 if year > -1 else 5
        return f"{wd_name} {m_name} {day:2d} 00:00:00 {year:0{y_shim}d}"

    def strftime(self, fmt):
        """
        Format using strftime().

        Example: '%d/%m/%Y, %H:%M:%S'
        """
        return _wrap_strftime(self, fmt, self.timetuple())

    def __format__(self, fmt):
        if not isinstance(fmt, str):
            raise TypeError("must be str, not %s" % type(fmt).__name__)

        if len(fmt) != 0:
            return self.strftime(fmt)

        return str(self)

    def isoformat(self):
        """
        Return the date formatted according to ISO.

        This is 'YYYY-MM-DD'.

        References:
        - http://www.w3.org/TR/NOTE-datetime
        - http://www.cl.cam.ac.uk/~mgk25/iso-time.html
        """
        year, month, day = self.__short_from_long_form()
        return f"{year:04d}-{month:02d}-{day:02d}"

    def _str_convertion(self):
        """
        Return a string representation od the date. In the case of a short
        from date the returned Badi date is in ISO format. Thre is not iso
        standard for the long form Badi date.
        """
        if self.__short:
            ret = self.isoformat()
        else:
            ind = 3 if self._kull_i_shay < 0 else 2
            ret = (f"{self._kull_i_shay:0{ind}d}-{self._vahid:02d}-"
                   f"{self._year:02d}-{self._month:02d}-{self._day:02d}")

        return ret

    __str__ = _str_convertion

    # Read-only field accessors
    @property
    def kull_i_shay(self):
        return self._kull_i_shay

    @property
    def vahid(self):
        return self._vahid

    @property
    def year(self):
        """year (1-9999)"""
        return self._year

    @property
    def month(self):
        """month (1-12)"""
        return self._month

    @property
    def day(self):
        """day (1-31)"""
        return self._day

    # Standard conversions, __eq__, __le__, __lt__, __ge__, __gt__,
    # __hash__ (and helpers)

    def timetuple(self):
        """
        Return local time tuple compatible with time.localtime().
        """
        return struct_time(self.__date + (0, 0, 0, 0, 0, -1))

    def toordinal(self):
        """
        Return proleptic Badi ordinal for the year, month and day.

        Baha 1 of year -1842 is day 1. Only the year, month and day values
        contribute to the result. If this class provides the long form
        Badi date it is converted to the short form before processing.
        """
        return _ymd2ord(self, *self.__short_from_long_form())

    def replace(self, *, kull_i_shay:int=None, vahid:int=None, year:int=None,
                month:int=None, day:int=None) -> object:
        """
        Return a new date with new values for the specified fields.
        """
        if self.__short and (kull_i_shay or vahid):
            msg = "Cannot convert from a short to a long form date."
            raise ValueError(msg)
        elif not self.__short and year is not None and (year < 1 or year > 19):
            msg = ("Cannot convert from a long to a short form date. The "
                   f"value {year} is not valid for long form dates.")
            raise ValueError(msg)
        elif self.__short:
            obj = self._replace_short(year=year, month=month, day=day)
        else:
            obj = self._replace_long(kull_i_shay=kull_i_shay, vahid=vahid,
                                     year=year, month=month, day=day)

        return obj

    def _replace_short(self, *, year:int=None, month:int=None,
                       day:int=None) -> object:
        """
        Replace any of the year, month, or day.
        """
        if year is None:
            year = self._year

        if month is None:
            month = self._month

        if day is None:
            day = self._day

        return type(self)(year, month, day, None, None)

    def _replace_long(self, *, kull_i_shay:int=None, vahid:int=None,
                      year:int=None, month:int=None, day:int=None) -> object:
        """
        Replace any of the kull_i_shay, vahid, year, month, or day.
        """
        if kull_i_shay is None:
            kull_i_shay = self._kull_i_shay

        if vahid is None:
            vahid = self._vahid

        if year is None:
            year = self._year

        if month is None:
            month = self._month

        if day is None:
            day = self._day

        return type(self)(kull_i_shay, vahid, year, month, day)

    # Comparisons of date objects with other.

    def __eq__(self, other):
        return (self._cmp(other) == 0 if isinstance(other, date)
                else NotImplemented)

    def __le__(self, other):
        return (self._cmp(other) <= 0 if isinstance(other, date)
                else NotImplemented)

    def __lt__(self, other):
        return (self._cmp(other) < 0 if isinstance(other, date)
                else NotImplemented)

    def __ge__(self, other):
        return (self._cmp(other) >= 0 if isinstance(other, date)
                else NotImplemented)

    def __gt__(self, other):
        return (self._cmp(other) > 0 if isinstance(other, date)
                else NotImplemented)

    def _cmp(self, other):
        assert isinstance(other, date)

        if self.__short:
            d0 = self._year, self._month, self._day
            d1 = other._year, other._month, other._day
        else:
            d0 = (self._kull_i_shay, self._vahid, self._year,
                  self._month, self._day)
            d1 = (other._kull_i_shay, other._vahid, other._year,
                  other._month, other._day)

        return _cmp(d0, d1)

    def __hash__(self):
        "Hash"
        if self._hashcode == -1:
            self._hashcode = hash(self._getstate())

        return self._hashcode

    # Computations

    def __add__(self, other):
        "Add a date to a timedelta."
        if isinstance(other, timedelta):
            od = self.toordinal() + other.days

            if 0 < od <= _MAXORDINAL:
                return type(self).fromordinal(o)

            raise OverflowError("Result out of range.")

        return NotImplemented

    __radd__ = __add__

    def __sub__(self, other):
        """Subtract two dates, or a date and a timedelta."""
        if isinstance(other, timedelta):
            return self + timedelta(-other.days)

        if isinstance(other, date):
            days1 = self.toordinal()
            days2 = other.toordinal()
            return timedelta(days1 - days2)

        return NotImplemented

    def weekday(self):
        """
        Return day of the week, where Jalál (Saturday) == 0 ...
        Istiqlāl (Friday) == 6.
        """
        date = self.__short_from_long_form()
        return _day_of_week(self, *date[:3])

    # Day-of-the-week and week-of-the-year, according to ISO

    def isoweekday(self):
        """
        Return day of the week, where Jalál (Saturday) == 1 ...
        Istiqlāl (Friday) == 7.
        """
        date = self.__short_from_long_form()
        return _day_of_week(self, *date) + 1

    def isocalendar(self):
        """
        Return a named tuple containing ISO year, week number, and weekday.

        The first ISO week of the year is the (Jalál-Istiqlāl) week
        containing the year's first Fiḍāl; everything else derives
        from that.

        The first week is 1; Jalál is 1 ... Istiqlāl is 7.

        ISO calendar algorithm taken from
        http://www.phys.uu.nl/~vgent/calendar/isocalendar.htm
        modified for the Badi Calendar.
        """
        year, month, day = self.__short_from_long_form()
        week1jalal = _isoweek1jalal(self, year)
        today = _ymd2ord(self, year, month, day)
        # Internally, week and day have origin 0
        week, day = divmod(today - week1jalal, 7)

        if week < 0:
            year -= 1
            week1monday = _isoweek1jalal(self, year)
            week, day = divmod(today - week1jalal, 7)
        elif week >= 52 and today >= _isoweek1jalal(self, year+1):
            year += 1
            week = 0

        return _IsoCalendarDate(year, week+1, day+1)

    # Pickle support.

    @classmethod
    def is_pickle_data(cls, a, b):
        if b is None and isinstance(a, (bytes, str)):
            a_len = len(a)
            assert a_len in (4, 5), (
                f"Invalid string {a} had length of {a_len} for pickle.")
            short = True if a_len == 4 else False

            if ((short and 1 <= ord(a[2:3]) <= 19)
                or not short and 1 <= ord(a[3:4]) <= 19):
                if isinstance(a, str):
                    try:
                        a = a.encode('latin1')
                    except UnicodeEncodeError:
                        raise ValueError(
                            "Failed to encode latin1 string when unpickling "
                            "a date object. "
                            "pickle.load(data, encoding='latin1') is assumed.")
            else:
                short = None
        else:
            short = None

        return short

    def _getstate(self):
        if self.__short:
            yhi, ylo = divmod(self._year - MINYEAR, 256)
            state = (yhi, ylo, self._month, self._day)
        else:
            # We need to add an arbitrarily number (19) larger that any
            # Kull-i-Shay that I support so we don't get a negative number
            # making bytes puke.
            kull_i_shay = self._kull_i_shay + 19
            vahid = self._vahid
            year = self._year
            month = self._month
            day = self._day
            state = (kull_i_shay, vahid, year, month, day)

        return bytes(state),

    def __setstate(self, bytes_str):
        if self.__short:
            yhi, ylo, self._month, self._day = bytes_str
            self._year = yhi * 256 + MINYEAR + ylo
            self.__date = (self._year, self._month, self._day)
        else:
            k, v, y, m, d = bytes_str
            self._kull_i_shay = k - 19
            self._vahid = v
            self._year = y
            self._month = m
            self._day = d
            self.__date = (self._kull_i_shay, v, y, m, d)

    def __reduce__(self):
        return (self.__class__, self._getstate())

_date_class = date  # so functions w/ args named "date" can get at the class

# This also needs to be done for long form date. *** TODO ***
date.min = date(MINYEAR, 1, 1)
date.max = date(MAXYEAR, 19, 19)

## date.resolution = timedelta(days=1)


class tzinfo:
    """
    Abstract base class for time zone info classes.

    Subclasses must override the tzname(), utcoffset() and dst() methods.
    """
    __slots__ = ()

    def tzname(self, dt):
        "datetime -> string name of time zone."
        raise NotImplementedError("tzinfo subclass must override tzname()")

    def utcoffset(self, dt):
        """
        datetime -> timedelta, positive for east of UTC, negative for west
                    of UTC
        """
        raise NotImplementedError("tzinfo subclass must override utcoffset()")

    def dst(self, dt):
        """
        datetime -> DST offset as timedelta, positive for east of UTC.

        Return 0 if DST not in effect.  utcoffset() must include the DST
        offset.
        """
        raise NotImplementedError("tzinfo subclass must override dst()")

    def fromutc(self, dt):
        """
        datetime in UTC -> datetime in local time.
        """

        if not isinstance(dt, datetime):
            raise TypeError("fromutc() requires a datetime argument")

        if dt.tzinfo is not self:
            raise ValueError("dt.tzinfo is not self")

        dtoff = dt.utcoffset()

        if dtoff is None:
            raise ValueError("fromutc() requires a non-None utcoffset() "
                             "result")

        # See the long comment block at the end of this file for an
        # explanation of this algorithm.
        dtdst = dt.dst()

        if dtdst is None:
            raise ValueError("fromutc() requires a non-None dst() result")
        delta = dtoff - dtdst

        if delta:
            dt += delta
            dtdst = dt.dst()

            if dtdst is None:
                raise ValueError("fromutc(): dt.dst gave inconsistent "
                                 "results; cannot convert")
        return dt + dtdst

    # Pickle support.
    def __reduce__(self):
        getinitargs = getattr(self, "__getinitargs__", None)

        if getinitargs:
            args = getinitargs()
        else:
            args = ()

        return (self.__class__, args, self.__getstate__())


class _IsoCalendarDate(tuple):

    def __new__(cls, year, week, weekday, /):
        return super().__new__(cls, (year, week, weekday))

    @property
    def year(self):
        return self[0]

    @property
    def week(self):
        return self[1]

    @property
    def weekday(self):
        return self[2]

    def __reduce__(self):
        # This code is intended to pickle the object without making the
        # class public. See https://bugs.python.org/msg352381
        return (tuple, (tuple(self),))

    def __repr__(self):
        return (f'{self.__class__.__name__}'
                f'(year={self[0]}, week={self[1]}, weekday={self[2]})')

class time:
    pass


class datetime(date):
    pass


class timezone:
    pass
