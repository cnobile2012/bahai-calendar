# -*- coding: utf-8 -*-
#
# badidatetime/datetime.py
#
__docformat__ = "restructuredtext en"

__all__ = ("date", "datetime", "time", "timedelta", "timezone", "tzinfo",
           "MINYEAR", "MAXYEAR", "UTC", "BADI_TZ")

import sys
import time as _time
import math as _math
from datetime import datetime as _dt, tzinfo as _tzinfo
from types import NoneType
from zoneinfo import ZoneInfo
from tzlocal import get_localzone

from .badi_calendar import BahaiCalendar
from ._structures import struct_time
from ._timedateutils import _td_utils
from typing import NamedTuple

MINYEAR = _td_utils.MINYEAR
MAXYEAR = _td_utils.MAXYEAR
BADI_TZ = ZoneInfo('Asia/Tehran')
_MAXORDINAL = 1097267 # date.max.toordinal()

def _cmp(x, y):
    return 0 if x == y else 1 if x > y else -1

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

def _check_utc_offset(name, offset):
    """
    name is the offset-producing method, 'utcoffset' or 'dst'.
    offset is what it returned.
    If offset isn't None or timedelta, raises TypeError.
    If offset is None, returns None.
    Else offset is checked for being in range.
    If it is, its integer value is returned.  Else ValueError is raised.
    """
    assert name in ("utcoffset", "dst"), (
        f"Invalid name argument '{name}' must be one of ('utcoffset', 'dst').")

    if offset is not None:
        if not isinstance(offset, timedelta):
            raise TypeError(f"tzinfo.{name}() must return None "
                            f"or timedelta, not '{type(offset)}'")

        if not -timedelta(1) < offset < timedelta(1):
            raise ValueError(f"{name}()={offset}, must be strictly between "
                             "-timedelta(hours=24) and timedelta(hours=24)")

def _check_tzinfo_arg(tz):
    if tz is not None and not isinstance(tz, tzinfo):
        raise TypeError("tzinfo argument must be None or of a tzinfo subclass")

def _format_time(hh, mm, ss, us, timespec='auto'):
    specs = {
        'auto': '',
        'hours': '{:02}',
        'minutes': '{:02}:{:02}',
        'seconds': '{:02}:{:02}:{:02}',
        'milliseconds': '{:02}:{:02}:{:02}.{:03}',
        'microseconds': '{:02}:{:02}:{:02}.{:06}'
        }

    if timespec == 'auto':
        # Skip trailing microseconds when us==0.
        timespec = 'microseconds' if us else 'seconds'
    elif timespec == 'milliseconds':
        us //= 1000

    try:
        fmt = specs[timespec]
    except KeyError:
        raise ValueError( f"Invalid timespec '{timespec}', must be one "
                          f"of {tuple(specs.keys())}.")
    else:
        return fmt.format(hh, mm, ss, us)

def _format_offset(off):
    if not isinstance(off, (timedelta, NoneType)):
        raise TypeError(f"The off value '{off}', must be a timedelta object "
                        "or None.")

    s = ''

    if off is not None:
        if off.days < 0:
            sign = "-"
            off = -off
        else:
            sign = "+"

        hh, mm = divmod(off, timedelta(hours=1))
        mm, ss = divmod(mm, timedelta(minutes=1))
        s += f"{sign}{hh:02d}:{mm:02d}"

        if ss or ss.microseconds:
            s += f":{ss.seconds:02d}"

            if ss.microseconds:
                s += f'.{ss.microseconds:06d}'

    return s

def _check_tzname(name):
    """
    Just raise TypeError if the arg isn't None or a string.
    """
    if name is not None and not isinstance(name, str):
        raise TypeError("tzinfo.tzname() must return None or string, "
                        f"not {type(name)!r}")

def _local_tz_utc_offset_seconds():
    return _dt.now(get_localzone()).utcoffset().total_seconds()


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
            raise OverflowError(f"timedelta # of days is too large: {d}")

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
            ret = timedelta(self._days + other._days,
                            self._seconds + other._seconds,
                            self._microseconds + other._microseconds)
        else:
            ret = NotImplemented

        return ret

    __radd__ = __add__

    def __sub__(self, other):
        if isinstance(other, timedelta):
            # for CPython compatibility, we cannot use
            # our __class__ here, but need a real timedelta
            ret = timedelta(self._days - other._days,
                            self._seconds - other._seconds,
                            self._microseconds - other._microseconds)
        else:
            ret = NotImplemented

        return ret

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
            ret = timedelta(self._days * other,
                             self._seconds * other,
                             self._microseconds * other)
        elif isinstance(other, float):
            usec = self._to_microseconds()
            a, b = other.as_integer_ratio()
            ret = timedelta(0, 0, _divide_and_round(usec * a, b))
        else:
            ret = NotImplemented

        return ret

    __rmul__ = __mul__

    def _to_microseconds(self):
        return ((self._days * (24*3600) + self._seconds) * 1000000 +
                self._microseconds)

    def __floordiv__(self, other):
        if isinstance(other, (int, timedelta)):
            usec = self._to_microseconds()

            if isinstance(other, timedelta):
                ret = usec // other._to_microseconds()
            else: # isinstance(other, int):
                ret = timedelta(0, 0, usec // other)
        else:
            ret = NotImplemented

        return ret

    def __truediv__(self, other):
        if isinstance(other, (int, float, timedelta)):
            usec = self._to_microseconds()

            if isinstance(other, timedelta):
                ret = usec / other._to_microseconds()
            elif isinstance(other, int):
                ret = timedelta(0, 0, _divide_and_round(usec, other))
            else: # isinstance(other, float):
                a, b = other.as_integer_ratio()
                ret = timedelta(0, 0, _divide_and_round(b * usec, a))
        else:
            ret = NotImplemented

        return ret

    def __mod__(self, other):
        if isinstance(other, timedelta):
            r = self._to_microseconds() % other._to_microseconds()
            ret = timedelta(0, 0, r)
        else:
            ret = NotImplemented

        return ret

    def __divmod__(self, other):
        if isinstance(other, timedelta):
            q, r = divmod(self._to_microseconds(), other._to_microseconds())
            ret = q, timedelta(0, 0, r)
        else:
            ret = NotImplemented

        return ret

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
    __slots__ = ('_kull_i_shay', '_vahid', '_year', '_month', '_day',
                 '_hashcode')

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
        if (short := date._is_pickle_data(a, b)) is not None:
            self = object.__new__(cls)
            self._short = short
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
                self._date = b_date
                self._short = False
            else:
                self._year = a
                self._month = b
                self._day = c
                self._date = b_date
                self._short = True

        self._MONTHNAMES = {num: name for num, name in self.BADI_MONTH_NAMES}
        super().__init__(self)
        _td_utils._check_date_fields(*self._date, short_in=self._short)
        self._hashcode = -1
        return self

    @property
    def is_short(self):
        return self._short

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
        date = bc.posix_timestamp(t, short=short, trim=True)
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
        date = _td_utils._ord2ymd(n, short=short)
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
            date = _td_utils._parse_isoformat_date(date_string)
        except Exception as e:
            del bc
            raise ValueError(str(e))
        else:
            if date == ():
                raise ValueError(f"Invalid isoformat string: {date_string!r}.")

            if short:
                b_date = date
            else:
                b_date = bc.long_date_from_short_date(date, trim=True)
                del bc

            return cls(*b_date)

    @classmethod
    def fromisocalendar(cls, year:int, week:int, day:int, *,
                        short:bool=False) -> object:
        """
        Construct a date from the ISO year, week number and weekday.

        This is the inverse of the date.isocalendar() function

        :param year: The Badi year.
        :type year: int
        :param week: The number of the week in the year.
        :type week: int
        :param day: Badi day in week.
        :type day: int
        :return: Returns an instantiated date object.
        :rtype: date
        """
        bc = BahaiCalendar()
        date = _td_utils._isoweek_to_badi(year, week, day, short=short)
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
        return (self._date if self._short else
                self.short_date_from_long_date(self._date, trim=True))

    def ctime(self):
        """
        Return ctime() style string in the short form Badi date.
        """
        date = self.__short_from_long_form()
        weekday = _td_utils._day_of_week(*date[:3])
        wd_name = _td_utils.DAYNAMES[weekday]
        year, month, day = date[:3]
        m_name = self._MONTHNAMES[month]
        y_shim = 4 if year > -1 else 5
        return f"{wd_name} {m_name} {day:2d} 00:00:00 {year:0{y_shim}d}"

    def strftime(self, fmt):
        """
        Format using strftime().

        Example: '%d/%m/%Y, %H:%M:%S.%f'
        """
        return _td_utils._wrap_strftime(self, fmt, self.timetuple())

    def __format__(self, fmt):
        if isinstance(fmt, str):
            if len(fmt) != 0:
                ret = self.strftime(fmt)
            else:
                ret = str(self)

            return ret

        raise TypeError(
            f"Must be a str, not a {type(fmt).__name__}") # pragma: no cover

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
        if self._short:
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
        return _td_utils._build_struct_time(self._date + (0, 0, 0), -1,
                                            short_in=self._short)

    def toordinal(self):
        """
        Return proleptic Badi ordinal for the year, month and day.

        Baha 1 of year -1842 is day 1. Only the year, month and day values
        contribute to the result. If this class provides the long form
        Badi date it is converted to the short form before processing.
        """
        return _td_utils._ymd2ord(*self.__short_from_long_form())

    def replace(self, *, kull_i_shay:int=None, vahid:int=None, year:int=None,
                month:int=None, day:int=None) -> object:
        """
        Return a new date with new values for the specified fields.
        """
        if self._short and (kull_i_shay or vahid):
            msg = "Cannot convert from a short to a long form date."
            raise ValueError(msg)
        elif not self._short and year is not None and (year < 1 or year > 19):
            msg = ("Cannot convert from a long to a short form date. The "
                   f"value {year} is not valid for long form dates.")
            raise ValueError(msg)
        elif self._short:
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

        if self._short:
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

            if _td_utils.DAYS_BEFORE_1ST_YEAR < od <= _MAXORDINAL:
                ret = type(self).fromordinal(od, short=self._short)
            else:
                raise OverflowError("Result out of range.")
        else:
            ret = NotImplemented

        return ret

    __radd__ = __add__

    def __sub__(self, other):
        """Subtract two dates, or a date and a timedelta."""
        if isinstance(other, timedelta):
            ret = self + timedelta(-other.days)
        elif isinstance(other, date):
            days1 = self.toordinal()
            days2 = other.toordinal()
            ret = timedelta(days1 - days2)
        else:
            ret = NotImplemented

        return ret

    def weekday(self):
        """
        Return day of the week, where Jalál (Saturday) == 0 ...
        Istiqlāl (Friday) == 6.
        """
        date = self.__short_from_long_form()
        return _td_utils._day_of_week(*date[:3])

    # Day-of-the-week and week-of-the-year, according to ISO

    def isoweekday(self):
        """
        Return day of the week, where Jalál (Saturday) == 1 ...
        Istiqlāl (Friday) == 7.
        """
        date = self.__short_from_long_form()
        return _td_utils._day_of_week(*date) + 1

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
        week1jalal = _td_utils._isoweek1jalal(year)
        today = _td_utils._ymd2ord(year, month, day)
        # Internally, week and day have origin 0
        week, day = divmod(today - week1jalal, 7)

        if week < 0:
            year -= 1
            week1jalel = _td_utils._isoweek1jalal(year)
            week, day = divmod(today - week1jalal, 7)
        elif week >= 52 and today >= _td_utils._isoweek1jalal(year+1):
            year += 1
            week = 0

        return _IsoCalendarDate(year, week+1, day+1)

    # Pickle support.

    @classmethod
    def _is_pickle_data(cls, a, b):
        """
        Check to see if the incoming date is pickle data or actual date
        information.

        :param a: Pickle data, the kull_i_shay, or year.
        :type a: int, str, or bypes
        :param b: None, vahid, or month
        :type b: NoneType or int
        :return: A Boolean if a short or long Badi date derived from pickle
                 data. A None can be returned if a and b are real date
                 information.
        :rtype: bool or NoneType
        """
        if b is None and isinstance(a, (bytes, str)):
            a_len = len(a)
            assert a_len in (4, 5), (
                f"Invalid string {a} had length of {a_len} for pickle.")
            short = True if a_len == 4 else False

            if ((short and 1 <= ord(a[2:3])&0x7F <= 19)
                or not short and 1 <= ord(a[3:4])&0x7F <= 19):
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
        if self._short:
            yhi, ylo = divmod(self._year - _td_utils.MINYEAR, 256)
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
        if self._short:
            yhi, ylo, self._month, self._day = bytes_str
            self._year = yhi * 256 + _td_utils.MINYEAR + ylo
            self._date = (self._year, self._month, self._day)
        else:
            k, v, y, m, d = bytes_str
            self._kull_i_shay = k - 19
            self._vahid = v
            self._year = y
            self._month = m
            self._day = d
            self._date = (self._kull_i_shay, v, y, m, d)

    def __reduce__(self):
        return (self.__class__, self._getstate())

_date_class = date  # so functions w/ args named "date" can get at the class

# This also needs to be done for long form date. *** TODO ***
date.min = date(_td_utils.MINYEAR, 1, 1)
date.max = date(_td_utils.MAXYEAR, 19, 19)

date.resolution = timedelta(days=1)


class tzinfo(_tzinfo):
    """
    Abstract base class for time zone info classes.

    Subclasses must override the tzname(), utcoffset() and dst() methods.
    """
    __slots__ = ()

    def frombadi(self, dt):
        """
        datetime in Badi TZ -> datetime in local time.
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

    ## def tzname(self, dt):
    ##     """
    ##     datetime -> string name of time zone.
    ##     """
    ##     raise NotImplementedError("tzinfo subclass must override tzname()")

    ## def utcoffset(self, dt):
    ##     """
    ##     datetime -> timedelta, positive for east of UTC, negative for
    ##     west of UTC
    ##     """
    ##     raise NotImplementedError("tzinfo subclass must override utcoffset()")

    ## def dst(self, dt):
    ##     """
    ##     datetime -> DST offset as timedelta, positive for east of UTC.

    ##     Return 0 if DST not in effect.  utcoffset() must include the DST
    ##     offset.
    ##     """
    ##     raise NotImplementedError("tzinfo subclass must override dst()")

    ## def fromutc(self, dt):
    ##     """
    ##     datetime in UTC -> datetime in local time.
    ##     """
    ##     if not isinstance(dt, datetime):
    ##         raise TypeError("fromutc() requires a datetime argument")

    ##     if dt.tzinfo is not self:
    ##         raise ValueError("dt.tzinfo is not self")

    ##     dtoff = dt.utcoffset()

    ##     if dtoff is None:
    ##         raise ValueError("fromutc() requires a non-None utcoffset() "
    ##                          "result")

    ##     # See the long comment block at the end of this file for an
    ##     # explanation of this algorithm.
    ##     dtdst = dt.dst()

    ##     if dtdst is None:
    ##         raise ValueError("fromutc() requires a non-None dst() result")

    ##     delta = dtoff - dtdst

    ##     if delta:
    ##         dt += delta
    ##         dtdst = dt.dst()

    ##         if dtdst is None:
    ##             raise ValueError("fromutc(): dt.dst gave inconsistent "
    ##                              "results; cannot convert")

    ##     return dt + dtdst

    ## # Pickle support.

    ## def __reduce__(self):
    ##     getinitargs = getattr(self, "__getinitargs__", None)

    ##     if getinitargs:
    ##         args = getinitargs()
    ##     else:
    ##         args = ()

    ##     return (self.__class__, args, self.__getstate__())


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

_tzinfo_class = tzinfo

class time:
    """
    Time with time zone.

    Constructors:

    __new__()

    Operators:

    __repr__, __str__
    __eq__, __le__, __lt__, __ge__, __gt__, __hash__

    Methods:

    strftime()
    isoformat()
    utcoffset()
    tzname()
    dst()

    Properties (readonly):
    hour, minute, second, microsecond, tzinfo, fold
    """
    __slots__ = ('_hour', '_minute', '_second', '_microsecond', '_tzinfo',
                 '_hashcode', '_fold')

    def __new__(cls, hour:float=0, minute:float=0, second:float=0,
                microsecond:int=0, tzinfo:tzinfo=None, *, fold:int=0):
        """
        Constructor.

        :param hour: Hours (required)
        :type hour: float
        :param minute: Minutes (required)
        :type minute: float
        :param second: Seconds (default to zero)
        :type second: float
        :param microsecond: Microseconds (default to zero)
        :type microsecond: int
        :param tzinfo: Timezone information (default to None)
        :type tzinfo: tzinfo
        :param fold:  (keyword only, default to zero)
        :type fold: int
        :return: the instantiated object self.
        :rtype: time
        """
        if (isinstance(hour, (bytes, str)) and len(hour) == 6 and
            ord(hour[0:1])&0x7F < 24):
            # Pickle support
            if isinstance(hour, str):
                try:
                    hour = hour.encode('latin1')
                except UnicodeEncodeError:
                    # More informative error message.
                    raise ValueError(
                        "Failed to encode latin1 string when unpickling "
                        "a time object. "
                        "pickle.load(data, encoding='latin1') is assumed.")

            self = object.__new__(cls)
            self.__setstate(hour, minute or None)
            self._hashcode = -1
        else:
            _td_utils._check_time_fields(hour, minute, second,
                                         microsecond, fold)
            _check_tzinfo_arg(tzinfo)
            self = object.__new__(cls)
            self._hour = hour
            self._minute = minute
            self._second = second
            self._microsecond = microsecond
            self._tzinfo = tzinfo
            self._hashcode = -1
            self._fold = fold

        return self

    # Read-only field accessors
    @property
    def hour(self):
        """hour (0-24)"""
        return self._hour

    @property
    def minute(self):
        """minute (0-59)"""
        return self._minute

    @property
    def second(self):
        """second (0-59)"""
        return self._second

    @property
    def microsecond(self):
        """microsecond (0-999999)"""
        return self._microsecond

    @property
    def tzinfo(self):
        """timezone info object"""
        return self._tzinfo

    @property
    def fold(self):
        return self._fold

    # Comparisons of time objects with other.

    def __eq__(self, other):
        return (self._cmp(other, allow_mixed=True) == 0
                if isinstance(other, time) else NotImplemented)

    def __le__(self, other):
        return (self._cmp(other, time) <= 0 if isinstance(other, time)
                else NotImplemented)

    def __lt__(self, other):
        return (self._cmp(other) < 0 if isinstance(other, time)
                else NotImplemented)

    def __ge__(self, other):
        return (self._cmp(other) >= 0 if isinstance(other, time)
                else NotImplemented)

    def __gt__(self, other):
        return (self._cmp(other) > 0 if isinstance(other, time)
                else NotImplemented)

    def _cmp(self, other, allow_mixed=False):
        assert isinstance(other, time), f"Invalid time module, found {other}."
        mytz = self._tzinfo
        ottz = other._tzinfo
        myoff = otoff = None

        if mytz is ottz:
            base_compare = True
        else:
            myoff = self.utcoffset()
            otoff = other.utcoffset()
            base_compare = myoff == otoff

        if base_compare:
            return _cmp((self._hour, self._minute, self._second,
                         self._microsecond),
                        (other._hour, other._minute, other._second,
                         other._microsecond))

        if myoff is None or otoff is None:
            if allow_mixed:
                return 2 # arbitrary non-zero value
            else:
                raise TypeError("cannot compare naive and aware times")

        myhhmm = self._hour * 60 + self._minute - myoff//timedelta(minutes=1)
        othhmm = other._hour * 60 + other._minute - otoff//timedelta(minutes=1)
        return _cmp((myhhmm, self._second, self._microsecond),
                    (othhmm, other._second, other._microsecond))

    # Standard conversions, __hash__ (and helpers)

    def __hash__(self):
        """
        Hash the hour, minute, second and microsecond.
        """
        if self._hashcode == -1:
            if self.fold:
                t = self.replace(fold=0)
            else:
                t = self

            tzoff = t.utcoffset()

            if not tzoff: # zero or None
                self._hashcode = hash(t._getstate()[0])
            else:
                h, m = divmod(timedelta(hours=self.hour,
                                        minutes=self.minute) - tzoff,
                              timedelta(hours=1))
                assert not m % timedelta(minutes=1), "whole minute"
                m //= timedelta(minutes=1)

                if 0 <= h < 24:
                    self._hashcode = hash(time(h, m, self.second,
                                               self.microsecond))
                else:
                    self._hashcode = hash((h, m, self.second,
                                           self.microsecond))

        return self._hashcode

    # Conversion to string

    def _tzstr(self):
        """
        Return a formatted timezone offset (+xx:xx) or an empty string.
        """
        off = self.utcoffset()
        return _format_offset(off)

    def __repr__(self):
        """
        Convert to formal string, for repr().
        """
        if self._microsecond != 0:
            s = f", {self._second:d}, {self._microsecond:d}"
        elif self._second != 0:
            s = f", {self._second:d}"
        else:
            s = ""

        s = (f"{self.__class__.__module__}.{self.__class__.__qualname__}"
             f"({self._hour:d}, {self._minute:d}{s})")

        if self._tzinfo is not None:
            assert s[-1:] == ")"
            s = s[:-1] + f", tzinfo={self._tzinfo})"

        if self._fold:
            assert s[-1:] == ")"
            s = s[:-1] + ", fold=1)"

        return s

    def isoformat(self, timespec='auto'):
        """
        Return the time formatted according to ISO.

        The full format is 'HH:MM:SS.mmmmmm+zz:zz'. By default, the fractional
        part is omitted if self.microsecond == 0.

        The optional argument timespec specifies the number of additional
        terms of the time to include. Valid options are 'auto', 'hours',
        'minutes', 'seconds', 'milliseconds' and 'microseconds'.
        """
        s = _format_time(self._hour, self._minute, self._second,
                         self._microsecond, timespec)
        tz = self._tzstr()

        if tz:
            s += tz

        return s

    __str__ = isoformat

    @classmethod
    def fromisoformat(cls, time_string):
        """
        Construct a time from a string in one of the ISO 8601 formats.
        """
        if isinstance(time_string, str):
            try:
                return cls(*_td_utils._parse_isoformat_time(time_string))
            except Exception as e:
                raise ValueError(
                    f'Invalid isoformat string: {time_string!r}, {e}')

        raise TypeError('fromisoformat: argument must be str')

    def strftime(self, format):
        """
        Format using strftime().  The date part of the timestamp passed
        to underlying strftime should not be used.
        """
        # We use the Badi epoch for the year, month and day.
        timetuple = (1, 1, 1, self._hour, self._minute, self._second, 0, 1, -1)
        return _td_utils._wrap_strftime(self, format, timetuple)

    def __format__(self, fmt):
        if isinstance(fmt, str):
            if len(fmt) != 0:
                ret = self.strftime(fmt)
            else:
                ret = str(self)

            return ret

        raise TypeError(f"Must be a str, not {type(fmt).__name__}")

    # Timezone functions

    def utcoffset(self):
        """
        Return the timezone offset as timedelta, positive east of UTC
        (negative west of UTC).
        """
        if self._tzinfo is not None:
            offset = self._tzinfo.utcoffset(None)
            _check_utc_offset("utcoffset", offset)
            return offset

    def tzname(self):
        """
        Return the timezone name.

        Note that the name is 100% informational -- there's no requirement that
        it mean anything in particular. For example, 'GMT', 'UTC', '-500',
        '-5:00', 'EDT', 'US/Eastern', 'America/New York' are all valid replies.
        """
        if self._tzinfo is not None:
            name = self._tzinfo.tzname(None)
            _check_tzname(name)
            return name

    def dst(self):
        """
        Return 0 if DST is not in effect, or the DST offset (as timedelta
        positive eastward) if DST is in effect.

        This is purely informational; the DST offset has already been added to
        the UTC offset returned by utcoffset() if applicable, so there's no
        need to consult dst() unless you're interested in displaying the DST
        info.
        """
        if self._tzinfo is not None:
            offset = self._tzinfo.dst(None)
            _check_utc_offset("dst", offset)
            return offset

    def replace(self, hour=None, minute=None, second=None, microsecond=None,
                tzinfo=True, *, fold=None):
        """
        Return a new time with new values for the specified fields.
        """
        if hour is None:
            hour = self.hour

        if minute is None:
            minute = self.minute

        if second is None:
            second = self.second

        if microsecond is None:
            microsecond = self.microsecond

        if tzinfo is True:
            tzinfo = self.tzinfo

        if fold is None:
            fold = self._fold

        return type(self)(hour, minute, second, microsecond, tzinfo, fold=fold)

    # Pickle support.

    def _getstate(self, protocol=3):
        us2, us3 = divmod(self._microsecond, 256)
        us1, us2 = divmod(us2, 256)
        h = self._hour
        h += 128 if self._fold and protocol > 3 else 0
        basestate = bytes([h, self._minute, self._second, us1, us2, us3])

        if self._tzinfo is None:
            return (basestate,)
        else:
            return (basestate, self._tzinfo)

    def __setstate(self, string, tzinfo):
        if tzinfo is not None and not isinstance(tzinfo, _tzinfo_class):
            raise TypeError("bad tzinfo state arg")

        h, self._minute, self._second, us1, us2, us3 = string

        if h > 127:
            self._fold = 1
            self._hour = h - 128
        else:
            self._fold = 0
            self._hour = h

        self._microsecond = (((us1 << 8) | us2) << 8) | us3
        self._tzinfo = tzinfo

    def __reduce_ex__(self, protocol):
        return (self.__class__, self._getstate(protocol))

    def __reduce__(self): # pragma: no cover
        return self.__reduce_ex__(2)

_time_class = time # so functions w/ args named "time" can get at the class

time.min = time(0, 0, 0)
time.max = time(24, 0, 3) # See contrib/misc/badi_jd_tests.py --day
time.resolution = timedelta(microseconds=1)


class datetime(date):
    """
    datetime(year, month, day[, hour[, minute[, second[, microsecond[,tzinfo]]]]])

    The year, month and day arguments are required. tzinfo may be None, or an
    instance of a tzinfo subclass. The remaining arguments may be ints.
    """
    __slots__ = date.__slots__ + time.__slots__

    def __new__(cls, a:int, b:int=None, c:int=None, d:int=None, e:int=None,
                hour:float=0, minute:float=0, second:float=0,
                microsecond:int=0, tzinfo:tzinfo=None, *, fold:int=0):
        if (short := datetime._is_pickle_data(a, b)) is not None:
            self = object.__new__(cls)
            self._short = short
            self.__setstate(a)
        else:
            b_date = tuple([x for x in (a, b, c, d, e) if x is not None])
            #b_date += (hour, minute, second, microsecond)
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
                self._date = b_date
                self._short = False
            else:
                self._year = a
                self._month = b
                self._day = c
                self._date = b_date
                self._short = True

        _td_utils._check_date_fields(*self._date, short_in=self._short)
        _td_utils._check_time_fields(hour, minute, second, microsecond, fold)
        _check_tzinfo_arg(tzinfo)
        self._hour = hour
        self._minute = minute
        self._second = second
        self._microsecond = microsecond
        self._tzinfo = tzinfo
        self._fold = fold
        self._hashcode = -1
        return self

    # Read-only field accessors
    @property
    def hour(self):
        """hour (0-23)"""
        return self._hour

    @property
    def minute(self):
        """minute (0-59)"""
        return self._minute

    @property
    def second(self):
        """second (0-59)"""
        return self._second

    @property
    def microsecond(self):
        """microsecond (0-999999)"""
        return self._microsecond

    @property
    def tzinfo(self):
        """
        timezone info object
        """
        return self._tzinfo

    @property
    def fold(self):
        return self._fold

    @classmethod
    def _fromtimestamp(cls, t, badi, tz, *, short=False):
        """
        Construct a datetime from a POSIX timestamp (like time.time()).

        A timezone info object may be passed in as well.
        """
        def split_date_time(date_time, short):
            date = date_time[:3] if short else date_time[:5]
            time = date_time[3:] if short else date_time[5:]
            return date, time[0], time[1], time[2], time[3]

        bc = BahaiCalendar()

        # *** TODO *** Look into what should be done here, this isn't correct
        if not badi:
            # Get local UTC offset then correct for the Baha'i location.
            offset_sec = _local_tz_utc_offset_seconds()
            offset_sec -= bc.BAHAI_LOCATION[2] * 3600
        else:
            offset_sec = 0

        t += offset_sec
        date_time = bc.posix_timestamp(t, ms=True, short=short, trim=False)
        date, hh, mm, ss, us = split_date_time(date_time, short)
        # clamp out leap seconds if the platform has them
        ss = min(ss, 59)
        result = cls(*date, hour=hh, minute=mm, second=ss, microsecond=us,
                     tzinfo=tz)

        if tz is None and not badi:
            # As of version 2015f max fold in IANA database is
            # 23 hours at 1969-09-30 13:00:00 in Kwajalein.
            # Let's probe 24 hours in the past to detect a transition:
            max_fold_seconds = 24 * 3600

            # On Windows localtime_s throws an OSError for negative values,
            # thus we can't perform fold detection for values of time less
            # than the max time fold. See comments in _datetimemodule's
            # version of this method for more details.
            if t < max_fold_seconds and sys.platform.startswith("win"):
                return result

            date_time = bc.posix_timestamp(t - max_fold_seconds, ms=True,
                                           short=short, trim=False)
            date, hh, mm, ss, um = split_date_time(date_time, short)
            probe1 = cls(*date, hour=hh, minute=mm, second=ss, microsecond=us,
                         tzinfo=tz)
            trans = result - probe1 - timedelta(0, max_fold_seconds)

            if trans.days < 0:
                t += trans // timedelta(0, 1)
                date_time = bc.posix_timestamp(t, ms=True, short=short,
                                               trim=False)
                probe2 = cls(*date, hour=hh, minute=mm, second=ss,
                             microsecond=us, tzinfo=tz)

                if probe2 == result:
                    result._fold = 1
        elif tz is not None:
            result = tz.fromutc(result)

        return result

    @classmethod
    def fromtimestamp(cls, t, tz=None):
        """Construct a datetime from a POSIX timestamp (like time.time()).

        A timezone info object may be passed in as well.
        """
        _check_tzinfo_arg(tz)
        return cls._fromtimestamp(t, tz is not None, tz)

    @classmethod
    def utcfromtimestamp(cls, t):
        """Construct a naive UTC datetime from a POSIX timestamp."""
        return cls._fromtimestamp(t, True, None)

    @classmethod
    def now(cls, tz=None):
        "Construct a datetime from time.time() and optional time zone info."
        t = _time.time()
        return cls.fromtimestamp(t, tz)

    @classmethod
    def utcnow(cls):
        "Construct a UTC datetime from time.time()."
        t = _time.time()
        return cls.utcfromtimestamp(t)

    @classmethod
    def combine(cls, date, time, tzinfo=True):
        "Construct a datetime from a given date and a given time."
        if not isinstance(date, _date_class):
            raise TypeError("date argument must be a date instance")

        if not isinstance(time, _time_class):
            raise TypeError("time argument must be a time instance")

        if tzinfo is True:
            tzinfo = time.tzinfo

        return cls(date.year, date.month, date.day,
                   time.hour, time.minute, time.second, time.microsecond,
                   tzinfo, fold=time.fold)

    @classmethod
    def fromisoformat(cls, date_string):
        """Construct a datetime from a string in one of the ISO 8601 formats."""
        if not isinstance(date_string, str):
            raise TypeError('fromisoformat: argument must be str')

        if len(date_string) < 7:
            raise ValueError(f'Invalid isoformat string: {date_string!r}')

        # Split this at the separator
        try:
            separator_location = _find_isoformat_datetime_separator(date_string)
            dstr = date_string[0:separator_location]
            tstr = date_string[(separator_location+1):]

            date_components = _parse_isoformat_date(dstr)
        except ValueError:
            raise ValueError(
                f'Invalid isoformat string: {date_string!r}') from None

        if tstr:
            try:
                time_components = _parse_isoformat_time(tstr)
            except ValueError:
                raise ValueError(
                    f'Invalid isoformat string: {date_string!r}') from None
        else:
            time_components = [0, 0, 0, 0, None]

        return cls(*(date_components + time_components))

    def timetuple(self):
        "Return local time tuple compatible with time.localtime()."
        dst = self.dst()

        if dst is None:
            dst = -1
        elif dst:
            dst = 1
        else:
            dst = 0

        return _build_struct_time(self.year, self.month, self.day,
                                  self.hour, self.minute, self.second,
                                  dst)

    def _mktime(self):
        """Return integer POSIX timestamp."""
        epoch = datetime(1970, 1, 1)
        max_fold_seconds = 24 * 3600
        t = (self - epoch) // timedelta(0, 1)

        def local(u):
            y, m, d, hh, mm, ss = _time.localtime(u)[:6]
            return (datetime(y, m, d, hh, mm, ss) - epoch) // timedelta(0, 1)

        # Our goal is to solve t = local(u) for u.
        a = local(t) - t
        u1 = t - a
        t1 = local(u1)

        if t1 == t:
            # We found one solution, but it may not be the one we need.
            # Look for an earlier solution (if `fold` is 0), or a
            # later one (if `fold` is 1).
            u2 = u1 + (-max_fold_seconds, max_fold_seconds)[self.fold]
            b = local(u2) - u2
            if a == b:
                return u1
        else:
            b = t1 - u1
            assert a != b

        u2 = t - b
        t2 = local(u2)

        if t2 == t:
            return u2

        if t1 == t:
            return u1
        # We have found both offsets a and b, but neither t - a nor t - b is
        # a solution.  This means t is in the gap.
        return (max, min)[self.fold](u1, u2)

    def timestamp(self):
        "Return POSIX timestamp as float"
        if self._tzinfo is None:
            s = self._mktime()
            return s + self.microsecond / 1e6
        else:
            return (self - _EPOCH).total_seconds()

    def utctimetuple(self):
        "Return UTC time tuple compatible with time.gmtime()."
        offset = self.utcoffset()

        if offset:
            self -= offset

        y, m, d = self.year, self.month, self.day
        hh, mm, ss = self.hour, self.minute, self.second
        return _build_struct_time(y, m, d, hh, mm, ss, 0)

    def date(self):
        "Return the date part."
        return date(self._year, self._month, self._day)

    def time(self):
        "Return the time part, with tzinfo None."
        return time(self.hour, self.minute, self.second, self.microsecond,
                    fold=self.fold)

    def timetz(self):
        "Return the time part, with same tzinfo."
        return time(self.hour, self.minute, self.second, self.microsecond,
                    self._tzinfo, fold=self.fold)

    def replace(self, year=None, month=None, day=None, hour=None,
                minute=None, second=None, microsecond=None, tzinfo=True,
                *, fold=None):
        """
        Return a new datetime with new values for the specified fields.
        """
        if year is None:
            year = self.year

        if month is None:
            month = self.month

        if day is None:
            day = self.day

        if hour is None:
            hour = self.hour

        if minute is None:
            minute = self.minute

        if second is None:
            second = self.second

        if microsecond is None:
            microsecond = self.microsecond

        if tzinfo is True:
            tzinfo = self.tzinfo

        if fold is None:
            fold = self.fold

        return type(self)(year, month, day, hour, minute, second,
                          microsecond, tzinfo, fold=fold)

    def _local_timezone(self):
        if self.tzinfo is None:
            ts = self._mktime()
        else:
            ts = (self - _EPOCH) // timedelta(seconds=1)
        localtm = _time.localtime(ts)
        local = datetime(*localtm[:6])
        # Extract TZ data
        gmtoff = localtm.tm_gmtoff
        zone = localtm.tm_zone
        return timezone(timedelta(seconds=gmtoff), zone)

    def astimezone(self, tz=None):
        if tz is None:
            tz = self._local_timezone()
        elif not isinstance(tz, tzinfo):
            raise TypeError("tz argument must be an instance of tzinfo")

        mytz = self.tzinfo

        if mytz is None:
            mytz = self._local_timezone()
            myoffset = mytz.utcoffset(self)
        else:
            myoffset = mytz.utcoffset(self)

            if myoffset is None:
                mytz = self.replace(tzinfo=None)._local_timezone()
                myoffset = mytz.utcoffset(self)

        if tz is mytz:
            return self

        # Convert self to UTC, and attach the new time zone object.
        utc = (self - myoffset).replace(tzinfo=tz)

        # Convert from UTC to tz's local time.
        return tz.fromutc(utc)

    # Ways to produce a string.

    def ctime(self):
        "Return ctime() style string."
        weekday = self.toordinal() % 7 or 7
        return "%s %s %2d %02d:%02d:%02d %04d" % (
            _DAYNAMES[weekday],
            _MONTHNAMES[self._month],
            self._day,
            self._hour, self._minute, self._second,
            self._year)

    def isoformat(self, sep='T', timespec='auto'):
        """
        Return the time formatted according to ISO.

        The full format looks like 'YYYY-MM-DD HH:MM:SS.mmmmmm'.
        By default, the fractional part is omitted if self.microsecond == 0.

        If self.tzinfo is not None, the UTC offset is also attached, giving
        giving a full format of 'YYYY-MM-DD HH:MM:SS.mmmmmm+HH:MM'.

        Optional argument sep specifies the separator between date and
        time, default 'T'.

        The optional argument timespec specifies the number of additional
        terms of the time to include. Valid options are 'auto', 'hours',
        'minutes', 'seconds', 'milliseconds' and 'microseconds'.
        """
        s = ("%04d-%02d-%02d%c" % (self._year, self._month, self._day, sep) +
             _format_time(self._hour, self._minute, self._second,
                          self._microsecond, timespec))

        off = self.utcoffset()
        tz = _format_offset(off)

        if tz:
            s += tz

        return s

    def __repr__(self):
        """Convert to formal string, for repr()."""
        L = [self._year, self._month, self._day,  # These are never zero
             self._hour, self._minute, self._second, self._microsecond]

        if L[-1] == 0:
            del L[-1]

        if L[-1] == 0:
            del L[-1]

        s = "%s.%s(%s)" % (self.__class__.__module__,
                           self.__class__.__qualname__,
                           ", ".join(map(str, L)))

        if self._tzinfo is not None:
            assert s[-1:] == ")"
            s = s[:-1] + ", tzinfo=%r" % self._tzinfo + ")"

        if self._fold:
            assert s[-1:] == ")"
            s = s[:-1] + ", fold=1)"

        return s

    def __str__(self):
        "Convert to string, for str()."
        return self.isoformat(sep=' ')

    @classmethod
    def strptime(cls, date_string, format):
        'string, format -> new datetime parsed from a string (like time.strptime()).'
        import _strptime
        return _strptime._strptime_datetime(cls, date_string, format)

    def utcoffset(self):
        """Return the timezone offset as timedelta positive east of UTC (negative west of
        UTC)."""
        if self._tzinfo is None:
            return None

        offset = self._tzinfo.utcoffset(self)
        _check_utc_offset("utcoffset", offset)
        return offset

    def tzname(self):
        """Return the timezone name.

        Note that the name is 100% informational -- there's no requirement that
        it mean anything in particular. For example, "GMT", "UTC", "-500",
        "-5:00", "EDT", "US/Eastern", "America/New York" are all valid replies.
        """
        if self._tzinfo is None:
            return None

        name = self._tzinfo.tzname(self)
        _check_tzname(name)
        return name

    def dst(self):
        """Return 0 if DST is not in effect, or the DST offset (as timedelta
        positive eastward) if DST is in effect.

        This is purely informational; the DST offset has already been added to
        the UTC offset returned by utcoffset() if applicable, so there's no
        need to consult dst() unless you're interested in displaying the DST
        info.
        """
        if self._tzinfo is None:
            return None

        offset = self._tzinfo.dst(self)
        _check_utc_offset("dst", offset)
        return offset

    # Comparisons of datetime objects with other.

    def __eq__(self, other):
        if isinstance(other, datetime):
            return self._cmp(other, allow_mixed=True) == 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            return False

    def __le__(self, other):
        if isinstance(other, datetime):
            return self._cmp(other) <= 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def __lt__(self, other):
        if isinstance(other, datetime):
            return self._cmp(other) < 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def __ge__(self, other):
        if isinstance(other, datetime):
            return self._cmp(other) >= 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def __gt__(self, other):
        if isinstance(other, datetime):
            return self._cmp(other) > 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def _cmp(self, other, allow_mixed=False):
        assert isinstance(other, datetime)
        mytz = self._tzinfo
        ottz = other._tzinfo
        myoff = otoff = None

        if mytz is ottz:
            base_compare = True
        else:
            myoff = self.utcoffset()
            otoff = other.utcoffset()
            # Assume that allow_mixed means that we are called from __eq__

            if allow_mixed:
                if myoff != self.replace(fold=not self.fold).utcoffset():
                    return 2

                if otoff != other.replace(fold=not other.fold).utcoffset():
                    return 2

            base_compare = myoff == otoff

        if base_compare:
            return _cmp((self._year, self._month, self._day,
                         self._hour, self._minute, self._second,
                         self._microsecond),
                        (other._year, other._month, other._day,
                         other._hour, other._minute, other._second,
                         other._microsecond))

        if myoff is None or otoff is None:
            if allow_mixed:
                return 2 # arbitrary non-zero value
            else:
                raise TypeError("cannot compare naive and aware datetimes")

        # XXX What follows could be done more efficiently...
        diff = self - other     # this will take offsets into account

        if diff.days < 0:
            return -1

        return diff and 1 or 0

    def __add__(self, other):
        "Add a datetime and a timedelta."
        if not isinstance(other, timedelta):
            return NotImplemented

        delta = timedelta(self.toordinal(),
                          hours=self._hour,
                          minutes=self._minute,
                          seconds=self._second,
                          microseconds=self._microsecond)
        delta += other
        hour, rem = divmod(delta.seconds, 3600)
        minute, second = divmod(rem, 60)

        if 0 < delta.days <= _MAXORDINAL:
            return type(self).combine(date.fromordinal(delta.days),
                                      time(hour, minute, second,
                                           delta.microseconds,
                                           tzinfo=self._tzinfo))

        raise OverflowError("result out of range")

    __radd__ = __add__

    def __sub__(self, other):
        "Subtract two datetimes, or a datetime and a timedelta."
        if not isinstance(other, datetime):
            if isinstance(other, timedelta):
                return self + -other
            return NotImplemented

        days1 = self.toordinal()
        days2 = other.toordinal()
        secs1 = self._second + self._minute * 60 + self._hour * 3600
        secs2 = other._second + other._minute * 60 + other._hour * 3600
        base = timedelta(days1 - days2,
                         secs1 - secs2,
                         self._microsecond - other._microsecond)

        if self._tzinfo is other._tzinfo:
            return base

        myoff = self.utcoffset()
        otoff = other.utcoffset()

        if myoff == otoff:
            return base

        if myoff is None or otoff is None:
            raise TypeError("cannot mix naive and timezone-aware time")

        return base + otoff - myoff

    def __hash__(self):
        if self._hashcode == -1:
            if self.fold:
                t = self.replace(fold=0)
            else:
                t = self

            tzoff = t.utcoffset()

            if tzoff is None:
                self._hashcode = hash(t._getstate()[0])
            else:
                days = _ymd2ord(self.year, self.month, self.day)
                seconds = self.hour * 3600 + self.minute * 60 + self.second
                self._hashcode = hash(timedelta(days, seconds,
                                                self.microsecond) - tzoff)
        return self._hashcode

    # Pickle support.

    def _getstate(self, protocol=3):
        yhi, ylo = divmod(self._year, 256)
        us2, us3 = divmod(self._microsecond, 256)
        us1, us2 = divmod(us2, 256)
        m = self._month

        if self._fold and protocol > 3:
            m += 128

        basestate = bytes([yhi, ylo, m, self._day,
                           self._hour, self._minute, self._second,
                           us1, us2, us3])

        if self._tzinfo is None:
            return (basestate,)
        else:
            return (basestate, self._tzinfo)

    def __setstate(self, string, tzinfo):
        if tzinfo is not None and not isinstance(tzinfo, _tzinfo_class):
            raise TypeError("bad tzinfo state arg")

        (yhi, ylo, m, self._day, self._hour,
         self._minute, self._second, us1, us2, us3) = string

        if m > 127:
            self._fold = 1
            self._month = m - 128
        else:
            self._fold = 0
            self._month = m

        self._year = yhi * 256 + ylo
        self._microsecond = (((us1 << 8) | us2) << 8) | us3
        self._tzinfo = tzinfo

    def __reduce_ex__(self, protocol):
        return (self.__class__, self._getstate(protocol))

    def __reduce__(self):
        return self.__reduce_ex__(2)

#datetime.min = datetime(1, 1, 1)
#datetime.max = datetime(9999, 12, 31, 23, 59, 59, 999999)
datetime.resolution = timedelta(microseconds=1)


class timezone(tzinfo):
    __slots__ = '_offset', '_name'

    # Sentinel value to disallow None
    _Omitted = object()

    def __new__(cls, offset, name=_Omitted):
        if not isinstance(offset, timedelta):
            raise TypeError("offset must be a timedelta")

        if name is cls._Omitted:
            if not offset:
                return cls.utc
            name = None
        elif not isinstance(name, str):
            raise TypeError("name must be a string")

        if not cls._minoffset <= offset <= cls._maxoffset:
            raise ValueError("offset must be a timedelta "
                             "strictly between -timedelta(hours=24) and "
                             "timedelta(hours=24).")

        return cls._create(offset, name)

    @classmethod
    def _create(cls, offset, name=None):
        self = tzinfo.__new__(cls)
        self._offset = offset
        self._name = name
        return self

    def __getinitargs__(self):
        """pickle support"""
        if self._name is None:
            return (self._offset,)

        return (self._offset, self._name)

    def __eq__(self, other):
        if isinstance(other, timezone):
            return self._offset == other._offset

        return NotImplemented

    def __hash__(self):
        return hash(self._offset)

    def __repr__(self):
        """Convert to formal string, for repr().

        >>> tz = timezone.utc
        >>> repr(tz)
        'datetime.timezone.utc'
        >>> tz = timezone(timedelta(hours=-5), 'EST')
        >>> repr(tz)
        "datetime.timezone(datetime.timedelta(-1, 68400), 'EST')"
        """
        if self is self.utc:
            return 'datetime.timezone.utc'

        if self._name is None:
            return "%s.%s(%r)" % (self.__class__.__module__,
                                  self.__class__.__qualname__,
                                  self._offset)

        return "%s.%s(%r, %r)" % (self.__class__.__module__,
                                  self.__class__.__qualname__,
                                  self._offset, self._name)

    def __str__(self):
        return self.tzname(None)

    def utcoffset(self, dt):
        if isinstance(dt, datetime) or dt is None:
            return self._offset

        raise TypeError("utcoffset() argument must be a datetime instance"
                        " or None")

    def tzname(self, dt):
        if isinstance(dt, datetime) or dt is None:
            if self._name is None:
                return self._name_from_offset(self._offset)

            return self._name

        raise TypeError("tzname() argument must be a datetime instance"
                        " or None")

    def dst(self, dt):
        if isinstance(dt, datetime) or dt is None:
            return None

        raise TypeError("dst() argument must be a datetime instance"
                        " or None")

    def fromutc(self, dt):
        if isinstance(dt, datetime):
            if dt.tzinfo is not self:
                raise ValueError("fromutc: dt.tzinfo "
                                 "is not self")
            return dt + self._offset

        raise TypeError("fromutc() argument must be a datetime instance"
                        " or None")

    _maxoffset = timedelta(hours=24, microseconds=-1)
    _minoffset = -_maxoffset

    @staticmethod
    def _name_from_offset(delta):
        if not delta:
            return 'UTC'

        if delta < timedelta(0):
            sign = '-'
            delta = -delta
        else:
            sign = '+'

        hours, rest = divmod(delta, timedelta(hours=1))
        minutes, rest = divmod(rest, timedelta(minutes=1))
        seconds = rest.seconds
        microseconds = rest.microseconds

        if microseconds:
            return (f'UTC{sign}{hours:02d}:{minutes:02d}:{seconds:02d}'
                    f'.{microseconds:06d}')

        if seconds:
            return f'UTC{sign}{hours:02d}:{minutes:02d}:{seconds:02d}'

        return f'UTC{sign}{hours:02d}:{minutes:02d}'

UTC = timezone.utc = timezone._create(timedelta(0))

# bpo-37642: These attributes are rounded to the nearest minute for backwards
# compatibility, even though the constructor will accept a wider range of
# values. This may change in the future.
timezone.min = timezone._create(-timedelta(hours=23, minutes=59))
timezone.max = timezone._create(timedelta(hours=23, minutes=59))
_EPOCH = datetime(126, 16, 2, tzinfo=timezone.utc)
