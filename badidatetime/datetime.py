# -*- coding: utf-8 -*-
#
# badidatetime/datetime.py
#
__docformat__ = "restructuredtext en"

import time as _time
import math as _math

from .badi_calendar import BahaiCalendar
from ._structures import struct_time
from ._timedateutils import _td_utils
from typing import NamedTuple


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
        _td_utils._check_date_fields(*self.__date, short_in=self.__short)
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
        return (self.__date if self.__short else
                self.short_date_from_long_date(self.__date, trim=True))

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
        return _wrap_strftime(self, fmt, self.timetuple())

    def __format__(self, fmt):
        if isinstance(fmt, str):
            if len(fmt) != 0:
                ret = self.strftime(fmt)
            else:
                ret = str(self)

            return ret

        raise TypeError(f"must be str, not {type(fmt).__name__}")

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
        return _td_utils._build_struct_time(self.__date + (0, 0, 0), -1,
                                            short_in=self.__short)

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
                ret = type(self).fromordinal(od, short=self.__short)
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
    def is_pickle_data(cls, a, b):
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
        if self.__short:
            yhi, ylo, self._month, self._day = bytes_str
            self._year = yhi * 256 + _td_utils.MINYEAR + ylo
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
date.min = date(_td_utils.MINYEAR, 1, 1)
date.max = date(_td_utils.MAXYEAR, 19, 19)

date.resolution = timedelta(days=1)


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

    def __new__(cls, hour=0, minute=0, second=0, microsecond=0, tzinfo=None,
                *, fold=0):
        """Constructor.

        Arguments:

        hour, minute (required)
        second, microsecond (default to zero)
        tzinfo (default to None)
        fold (keyword only, default to zero)
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
            return self
        hour, minute, second, microsecond, fold = _check_time_fields(
            hour, minute, second, microsecond, fold)
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
        """timezone info object"""
        return self._tzinfo

    @property
    def fold(self):
        return self._fold

    # Standard conversions, __hash__ (and helpers)

    # Comparisons of time objects with other.

##     def __eq__(self, other):
##         if isinstance(other, time):
##             return self._cmp(other, allow_mixed=True) == 0
##         else:
##             return NotImplemented

##     def __le__(self, other):
##         if isinstance(other, time):
##             return self._cmp(other) <= 0
##         else:
##             return NotImplemented

##     def __lt__(self, other):
##         if isinstance(other, time):
##             return self._cmp(other) < 0
##         else:
##             return NotImplemented

##     def __ge__(self, other):
##         if isinstance(other, time):
##             return self._cmp(other) >= 0
##         else:
##             return NotImplemented

##     def __gt__(self, other):
##         if isinstance(other, time):
##             return self._cmp(other) > 0
##         else:
##             return NotImplemented

##     def _cmp(self, other, allow_mixed=False):
##         assert isinstance(other, time)
##         mytz = self._tzinfo
##         ottz = other._tzinfo
##         myoff = otoff = None

##         if mytz is ottz:
##             base_compare = True
##         else:
##             myoff = self.utcoffset()
##             otoff = other.utcoffset()
##             base_compare = myoff == otoff

##         if base_compare:
##             return _cmp((self._hour, self._minute, self._second,
##                          self._microsecond),
##                         (other._hour, other._minute, other._second,
##                          other._microsecond))
##         if myoff is None or otoff is None:
##             if allow_mixed:
##                 return 2 # arbitrary non-zero value
##             else:
##                 raise TypeError("cannot compare naive and aware times")
##         myhhmm = self._hour * 60 + self._minute - myoff//timedelta(minutes=1)
##         othhmm = other._hour * 60 + other._minute - otoff//timedelta(minutes=1)
##         return _cmp((myhhmm, self._second, self._microsecond),
##                     (othhmm, other._second, other._microsecond))

##     def __hash__(self):
##         """Hash."""
##         if self._hashcode == -1:
##             if self.fold:
##                 t = self.replace(fold=0)
##             else:
##                 t = self
##             tzoff = t.utcoffset()
##             if not tzoff:  # zero or None
##                 self._hashcode = hash(t._getstate()[0])
##             else:
##                 h, m = divmod(timedelta(hours=self.hour,
##                                         minutes=self.minute) - tzoff,
##                               timedelta(hours=1))
##                 assert not m % timedelta(minutes=1), "whole minute"
##                 m //= timedelta(minutes=1)
##                 if 0 <= h < 24:
##                     self._hashcode = hash(time(h, m, self.second,
##                                                self.microsecond))
##                 else:
##                     self._hashcode = hash((h, m, self.second,
##                                            self.microsecond))
##         return self._hashcode

##     # Conversion to string

##     def _tzstr(self):
##         """Return formatted timezone offset (+xx:xx) or an empty string."""
##         off = self.utcoffset()
##         return _format_offset(off)

##     def __repr__(self):
##         """Convert to formal string, for repr()."""
##         if self._microsecond != 0:
##             s = ", %d, %d" % (self._second, self._microsecond)
##         elif self._second != 0:
##             s = ", %d" % self._second
##         else:
##             s = ""
##         s= "%s.%s(%d, %d%s)" % (_get_class_module(self),
##                                 self.__class__.__qualname__,
##                                 self._hour, self._minute, s)
##         if self._tzinfo is not None:
##             assert s[-1:] == ")"
##             s = s[:-1] + ", tzinfo=%r" % self._tzinfo + ")"
##         if self._fold:
##             assert s[-1:] == ")"
##             s = s[:-1] + ", fold=1)"
##         return s

##     def isoformat(self, timespec='auto'):
##         """Return the time formatted according to ISO.

##         The full format is 'HH:MM:SS.mmmmmm+zz:zz'. By default, the fractional
##         part is omitted if self.microsecond == 0.

##         The optional argument timespec specifies the number of additional
##         terms of the time to include. Valid options are 'auto', 'hours',
##         'minutes', 'seconds', 'milliseconds' and 'microseconds'.
##         """
##         s = _format_time(self._hour, self._minute, self._second,
##                           self._microsecond, timespec)
##         tz = self._tzstr()
##         if tz:
##             s += tz
##         return s

##     __str__ = isoformat

##     @classmethod
##     def fromisoformat(cls, time_string):
##         """Construct a time from a string in one of the ISO 8601 formats."""
##         if not isinstance(time_string, str):
##             raise TypeError('fromisoformat: argument must be str')

##         # The spec actually requires that time-only ISO 8601 strings start with
##         # T, but the extended format allows this to be omitted as long as there
##         # is no ambiguity with date strings.
##         time_string = time_string.removeprefix('T')

##         try:
##             return cls(*_parse_isoformat_time(time_string))
##         except Exception:
##             raise ValueError(f'Invalid isoformat string: {time_string!r}')

##     def strftime(self, format):
##         """Format using strftime().  The date part of the timestamp passed
##         to underlying strftime should not be used.
##         """
##         # The year must be >= 1000 else Python's strftime implementation
##         # can raise a bogus exception.
##         timetuple = (1900, 1, 1,
##                      self._hour, self._minute, self._second,
##                      0, 1, -1)
##         return _wrap_strftime(self, format, timetuple)

##     def __format__(self, fmt):
##         if not isinstance(fmt, str):
##             raise TypeError("must be str, not %s" % type(fmt).__name__)
##         if len(fmt) != 0:
##             return self.strftime(fmt)
##         return str(self)

##     # Timezone functions

##     def utcoffset(self):
##         """Return the timezone offset as timedelta, positive east of UTC
##          (negative west of UTC)."""
##         if self._tzinfo is None:
##             return None
##         offset = self._tzinfo.utcoffset(None)
##         _check_utc_offset("utcoffset", offset)
##         return offset

##     def tzname(self):
##         """Return the timezone name.

##         Note that the name is 100% informational -- there's no requirement that
##         it mean anything in particular. For example, "GMT", "UTC", "-500",
##         "-5:00", "EDT", "US/Eastern", "America/New York" are all valid replies.
##         """
##         if self._tzinfo is None:
##             return None
##         name = self._tzinfo.tzname(None)
##         _check_tzname(name)
##         return name

##     def dst(self):
##         """Return 0 if DST is not in effect, or the DST offset (as timedelta
##         positive eastward) if DST is in effect.

##         This is purely informational; the DST offset has already been added to
##         the UTC offset returned by utcoffset() if applicable, so there's no
##         need to consult dst() unless you're interested in displaying the DST
##         info.
##         """
##         if self._tzinfo is None:
##             return None
##         offset = self._tzinfo.dst(None)
##         _check_utc_offset("dst", offset)
##         return offset

##     def replace(self, hour=None, minute=None, second=None, microsecond=None,
##                 tzinfo=True, *, fold=None):
##         """Return a new time with new values for the specified fields."""
##         if hour is None:
##             hour = self.hour
##         if minute is None:
##             minute = self.minute
##         if second is None:
##             second = self.second
##         if microsecond is None:
##             microsecond = self.microsecond
##         if tzinfo is True:
##             tzinfo = self.tzinfo
##         if fold is None:
##             fold = self._fold
##         return type(self)(hour, minute, second, microsecond, tzinfo, fold=fold)

##     # Pickle support.

##     def _getstate(self, protocol=3):
##         us2, us3 = divmod(self._microsecond, 256)
##         us1, us2 = divmod(us2, 256)
##         h = self._hour
##         if self._fold and protocol > 3:
##             h += 128
##         basestate = bytes([h, self._minute, self._second,
##                            us1, us2, us3])
##         if self._tzinfo is None:
##             return (basestate,)
##         else:
##             return (basestate, self._tzinfo)

##     def __setstate(self, string, tzinfo):
##         if tzinfo is not None and not isinstance(tzinfo, _tzinfo_class):
##             raise TypeError("bad tzinfo state arg")
##         h, self._minute, self._second, us1, us2, us3 = string
##         if h > 127:
##             self._fold = 1
##             self._hour = h - 128
##         else:
##             self._fold = 0
##             self._hour = h
##         self._microsecond = (((us1 << 8) | us2) << 8) | us3
##         self._tzinfo = tzinfo

##     def __reduce_ex__(self, protocol):
##         return (self.__class__, self._getstate(protocol))

##     def __reduce__(self):
##         return self.__reduce_ex__(2)

## _time_class = time  # so functions w/ args named "time" can get at the class

## time.min = time(0, 0, 0)
## time.max = time(23, 59, 59, 999999)
## time.resolution = timedelta(microseconds=1)


class datetime(date):
    pass


class timezone:
    pass
