# -*- coding: utf-8 -*-
#
# badidatetime/datetime.py
#
__docformat__ = "restructuredtext en"

__all__ = ("date", "datetime", "time", "timedelta", "timezone", "tzinfo",
           "MINYEAR", "MAXYEAR", "UTC", "BADI", "BADI_TZ")

import sys
import time as _time
import math as _math
from datetime import datetime as dtime, timedelta, tzinfo
from types import NoneType
from tzlocal import get_localzone
import geocoder

from .badi_calendar import BahaiCalendar
from ._structures import struct_time
from ._timedateutils import _td_utils

_MAXORDINAL = 1097267 # date.max.toordinal()
MINYEAR = BahaiCalendar.MINYEAR
MAXYEAR = BahaiCalendar.MAXYEAR
BADI_INFO = BahaiCalendar.BAHAI_LOCATION[2:4] # 3.5, Asia/Terhan
GMT_COORD = (51.477928, -0.001545, 0)

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

def _check_offset(name, offset):
    """
    name is the offset-producing method, 'utcoffset', 'badioffset', or 'dst'.
    offset is what it returned.
    If offset isn't None or timedelta, raises TypeError.
    If offset is None, returns None.
    Else offset is checked for being in range.
    If it is, its integer value is returned else ValueError is raised.
    """
    assert name in ('utcoffset', 'badioffset', 'dst'), (
        f"Invalid name argument '{name}' must be one of "
        "('utcoffset', 'badioffset', 'dst').")

    if offset is not None:
        if not isinstance(offset, timedelta):
            raise TypeError(f"tzinfo.{name}() must return None "
                            f"or timedelta, not {type(offset)}")

        if not -timedelta(1) < offset < timedelta(1):
            raise ValueError(f"{name}()={offset}, must be strictly between "
                             "-timedelta(hours=24) and timedelta(hours=24)")

def _check_tzinfo_arg(tz):
    if tz is not None and not isinstance(tz, tzinfo):
        raise TypeError("tzinfo argument must be None or of a tzinfo "
                        f"subclass, found {tz!r}")

def _cmperror(x, y):
    raise TypeError(f"Cannot compare {type(x).__name__!r} to "
                    f"{type(y).__name__!r}")

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

def fromutc(this, dt):
    """
    datetime in UTC -> datetime in local time.
    The method fromutc() in tzinfo will not accept the Badi datetime object,
    so this method needs to fill the role.

    :param this: The tzinfo object.
    :type this: tzinfo
    :param dt: A datetime object.
    :type dt: datetime.datetime
    :returns: The adjusted datetime from the provided UTC datetime.
    :rtype: datetime.datetime

    .. note::

       This function needs to be implimented outside the tzinfo class so
       that the tzinfo class can be used with the zoneinfo class. Normally
       the tzinfo class would just be overridden with this function added.
    """

    if not isinstance(dt, datetime):
        raise TypeError("fromutc() requires a datetime argument.")

    if dt.tzinfo is not this:
        raise ValueError("dt.tzinfo is not self")

    dtoff = dt.utcoffset()

    if dtoff is None:
        raise ValueError("fromutc() requires a non-None utcoffset() "
                         "result.")

    # See the long comment block at the end of this file for an
    # explanation of this algorithm.
    dtdst = dt.tzinfo.dst(dt)

    #print(dtdst, type(dtdst))

    if dtdst is None:
        raise ValueError("fromutc() requires a non-None dst() result.")

    delta = dtoff - dtdst
    #print(dt, delta)

    if delta:
        dt += delta
        dtdst = dt.dst()

        if dtdst is None:
            raise ValueError("fromutc(): dt.dst gave inconsistent "
                             "results; cannot convert.")

    return dt + dtdst

def _local_timezone_info():
    """
    Returns the offset in seconds, dst, IANA timezone key.

    :returns: Offset in seconds, True or False for dst, and the IANA key.
    :rtype: tuple

    .. note::

       Currently this must use the Python built in datetime, because the
       tzlocal package does not work completely with the badi datetime
       package.
    """
    localzone = get_localzone()
    dt = dtime.now(localzone)
    offset = dt.utcoffset().total_seconds()
    dst = dt.dst().total_seconds() != 0
    return offset, dst, localzone.key

def _get_local_coordinates():
    """
    Get the locales coordinents and timezone offset.
    """
    offset, dst, key = _local_timezone_info()
    # Get latitude and longitude
    g = geocoder.ip('me')
    latitude = g.lat
    longitude = g.lng
    return latitude, longitude, offset / 3600

LOCAL_COORD = _get_local_coordinates()

def _module_name(module):
    """
    Find the package name without the first directory.
    """
    idx = module.find('.')
    dt_true = True if module[:idx] == 'datetime' else False
    return module[idx + 1:] if idx != -1 and not dt_true else module


class date(BahaiCalendar):
    """
    Implements the date object for the Badi datetime package.
    """
    __slots__ = ('_kull_i_shay', '_vahid', '_year', '_month', '_day',
                 '_hashcode', '__date', '__short')

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

        super().__init__(self)
        _td_utils._check_date_fields(*self.__date, short_in=self.__short)
        self._hashcode = -1
        return self

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
        date = bc.posix_timestamp(t, *LOCAL_COORD, short=short, trim=True)
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
        msg = (f"{_module_name(self.__class__.__module__)}."
               f"{self.__class__.__qualname__}")

        if hasattr(self, '_kull_i_shay'):
            msg += (f"({self._kull_i_shay}, {self._vahid}, {self._year}, "
                    f"{self._month}, {self._day})")
        else:
            msg += f"({self._year}, {self._month}, {self._day})"

        return msg

    def _short_from_long_form(self, time=()):
        """
        Convert the long form Badi date to a short form Badi date.
        """
        return (self.b_date + time if self.is_short else
                self.short_date_from_long_date(self.b_date + time))

    def ctime(self):
        """
        Return ctime() style string in the short form Badi date.
        """
        date = self._short_from_long_form()
        weekday = _td_utils._day_of_week(*date[:3])
        wd_name = _td_utils.DAYNAMES[weekday]
        year, month, day = date[:3]
        m_name = _td_utils.MONTHNAMES[month]
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
        year, month, day = self._short_from_long_form()[:3]
        return f"{year:04d}-{month:02d}-{day:02d}"

    def _str_convertion(self):
        """
        Return a string representation of the date. In the case of a short
        from date the returned Badi date is in ISO format. Thre is not iso
        standard for the long form Badi date.
        """
        if self.is_short:
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

    @property
    def is_short(self):
        return self.__short

    @property
    def b_date(self):
        return self.__date

    # Standard conversions, __eq__, __le__, __lt__, __ge__, __gt__,
    # __hash__ (and helpers)

    def timetuple(self):
        """
        Return local time tuple compatible with time.localtime().
        """
        return _td_utils._build_struct_time(self.b_date + (0, 0, 0), -1,
                                            short_in=self.is_short)

    def toordinal(self):
        """
        Return proleptic Badi ordinal for the year, month and day.

        Baha 1 of year -1842 is day 1. Only the year, month and day values
        contribute to the result. If this class provides the long form
        Badi date it is converted to the short form before processing.
        """
        return _td_utils._ymd2ord(*self._short_from_long_form()[:3])

    def replace(self, kull_i_shay:int=None, vahid:int=None, year:int=None,
                month:int=None, day:int=None) -> object:
        """
        Return a new date with new values for the specified fields.
        """
        if self.is_short and (kull_i_shay or vahid):
            msg = "Cannot convert from a short to a long form date."
            raise ValueError(msg)
        elif not self.is_short and year is not None and (year < 1 or year > 19):
            msg = ("Cannot convert from a long to a short form date. The "
                   f"value {year} is not valid for long form dates.")
            raise ValueError(msg)
        elif self.is_short:
            obj = self._replace_short(year=year, month=month, day=day)
        else:
            obj = self._replace_long(kull_i_shay=kull_i_shay, vahid=vahid,
                                     year=year, month=month, day=day)

        return obj

    def _replace_short(self, *, year:int=None, month:int=None, day:int=None,
                       hour:int=None, minute:int=None, second:int=None,
                       microsecond:int=None, tzinfo=True,
                       fold:int=None) -> object:
        """
        Replace any of the year, month, or day.
        """
        if year is None:
            year = self.year

        if month is None:
            month = self.month

        if day is None:
            day = self.day

        if isinstance(self, datetime):
            obj = type(self)(year, month, day, None, None, hour, minute,
                             second, microsecond, tzinfo, fold=fold)
        else:
            obj = type(self)(year, month, day, None, None)

        return obj

    def _replace_long(self, *, kull_i_shay:int=None, vahid:int=None,
                      year:int=None, month:int=None, day:int=None,
                      hour:int=None, minute:int=None, second:int=None,
                      microsecond:int=None, tzinfo=True,
                      fold:int=None) -> object:
        """
        Replace any of the kull_i_shay, vahid, year, month, or day.
        """
        if kull_i_shay is None:
            kull_i_shay = self.kull_i_shay

        if vahid is None:
            vahid = self.vahid

        if year is None:
            year = self.year

        if month is None:
            month = self.month

        if day is None:
            day = self.day

        if isinstance(self, datetime):
            obj = type(self)(kull_i_shay, vahid, year, month, day, hour,
                             minute, second, microsecond, tzinfo, fold=fold)
        else:
            obj = type(self)(kull_i_shay, vahid, year, month, day)

        return obj

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

        if self.is_short:
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
                ret = type(self).fromordinal(od, short=self.is_short)
            else:
                raise OverflowError("Result out of range.")
        else:
            ret = NotImplemented

        return ret

    __radd__ = __add__

    def __sub__(self, other):
        """
        Subtract two dates, or a date and a timedelta.
        """
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
        date = self._short_from_long_form()
        return _td_utils._day_of_week(*date[:3])

    # Day-of-the-week and week-of-the-year, according to ISO

    def isoweekday(self):
        """
        Return day of the week, where Jalál (Saturday) == 1 ...
        Istiqlāl (Friday) == 7.
        """
        date = self._short_from_long_form()[:3]
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
        year, month, day = self._short_from_long_form()[:3]
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
        Check if the incoming date is pickle data or actual date information.

        :param a: Pickle data, the kull_i_shay, or year.
        :type a: int, str, or bypes
        :param b: None, vahid, or month
        :type b: NoneType or int
        :return: A Boolean if a short or long Badi date derived from pickle
                 data. A None can be returned if a and b are real date
                 information.
        :rtype: bool or NoneType
        """
        if isinstance(b, NoneType) and isinstance(a, (bytes, str)):
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
                            "a date or datetime object. "
                            "pickle.load(data, encoding='latin1') is assumed.")
            else:
                short = None
        else:
            short = None

        return short

    def _getstate(self):
        if self.is_short:
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
        if self.is_short:
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

date.resolution = timedelta(days=1)


_tzinfo_class = tzinfo


class _IsoCalendarDate(tuple):

    def __new__(cls, year, week, weekday, /) -> object:
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
        return (f'{_module_name(self.__class__.__name__)}'
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

    def __new__(cls, hour:float=0, minute:float=0, second:float=0,
                microsecond:int=0, tzinfo:tzinfo=None, *,
                fold:int=0) -> object:
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
        mytz = self.tzinfo
        ottz = other.tzinfo
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

        s = (f"{_module_name(self.__class__.__module__)}."
             f"{self.__class__.__qualname__}"
             f"({self._hour:d}, {self._minute:d}{s})")

        if self.tzinfo is not None:
            assert s[-1:] == ")"
            s = s[:-1] + f", tzinfo={self.tzinfo})"

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
    def fromisoformat(cls, t_str):
        """
        Construct a time from a string in one of the ISO 8601 formats.
        """
        if isinstance(t_str, str):
            try:
                return cls(*_td_utils._parse_isoformat_time(t_str))
            except Exception as e:
                raise ValueError(f'Invalid isoformat string: {t_str!r}, {e}')

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

        raise TypeError(f"Must be a str, not {type(fmt).__name__}.")

    # Timezone functions

    def utcoffset(self):
        """
        Return the timezone offset as timedelta, positive east of UTC
        (negative west of UTC).
        """
        if self.tzinfo is not None:
            offset = self.tzinfo.utcoffset(None)

            #print('POOP', offset)

            _check_offset("utcoffset", offset)
            return offset

    def badioffset(self):
        """
        Return the timezone offset as timedelta, positive east of UTC
        (negative west of UTC).

        *** TODO *** Should just need to call utcoffset then add 3.5 hours.
        """
        if self.tzinfo is not None:
            offset = self.utcoffset() + 3.5 * 3600
            return offset

    def tzname(self):
        """
        Return the timezone name.

        Note that the name is 100% informational -- there's no requirement that
        it mean anything in particular. For example, 'GMT', 'UTC', '-500',
        '-5:00', 'EDT', 'US/Eastern', 'America/New York' are all valid replies.
        """
        if self.tzinfo is not None:
            name = self.tzinfo.tzname(None)
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
        if self.tzinfo is not None:
            offset = self.tzinfo.dst(None)
            _check_offset("dst", offset)
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

        if self.tzinfo is None:
            return (basestate,)
        else:
            return (basestate, self.tzinfo)

    def __setstate(self, string, tzinfo):
        _check_tzinfo_arg(tzinfo)
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
                microsecond:int=0, tzinfo:tzinfo=None, *,
                fold:int=0) -> object:
        if (short := datetime._is_pickle_data(a, b)) is not None:
            self = object.__new__(cls)
            self.__short = short
            self.__setstate(a, b)
        else:
            b_date = tuple([x for x in (a, b, c, d, e) if x is not None])
            date_len = len(b_date)
            #print(b_date)
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

            self._hour = hour
            self._minute = minute
            self._second = second
            self._microsecond = microsecond
            self.__time = (self._hour, self._minute,
                           self._second, self._microsecond)
            self._tzinfo = tzinfo
            self._fold = fold

        _td_utils._check_date_fields(*self.__date, short_in=self.__short)
        _td_utils._check_time_fields(*self.__time, fold)
        _check_tzinfo_arg(tzinfo)
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

    @property
    def b_date(self):
        return self.__date

    @property
    def b_time(self):
        return self.__time

    @property
    def is_short(self):
        return self.__short

    @classmethod
    def _fromtimestamp(cls, t, utc, tz, *, short=False):
        """
        Construct a datetime from a POSIX timestamp (like time.time()).

        :param t: POSIX timestamp.
        :type t: float
        :param utc: If True then the result is relative to UTC time else
                    if False it is relative to local time.
        :type utc: bool
        :param tz: A tzinfo object.
        :type tz: tzinfo
        :param short: If False (default) the result is in the long Badi form,
                      else if True it is in the short form.
        :returns: A datetime object set to the date derived from the
                  POSIX timestamp.
        :rtype: datetime.datetime
        """
        def _fix_short_date(date, short):
            return date[:3] + (None, None) + date[3:] if short else date

        bc = BahaiCalendar()
        coord = GMT_COORD if utc else LOCAL_COORD
        dt = bc.posix_timestamp(t, *coord, us=True, short=short, trim=False)
        date = _fix_short_date(dt, short)
        # Clamp out leap seconds if the platform has them.
        date = date[:7] + (min(date[7], 59),) + date[8:]
        result = cls(*date, tzinfo=tz)

        if tz is None and not utc:
            # As of version 2015f max fold in IANA database is
            # 23 hours at 1969-09-30 13:00:00 in Kwajalein.
            # Let's probe 24 hours in the past to detect a transition:
            max_fold_seconds = 24 * 3600

            # On Windows localtime_s throws an OSError for negative values,
            # thus we can't perform fold detection for values of time less
            # than the max time fold. See comments in _datetimemodule's
            # version of this method for more details.
            if t < max_fold_seconds and sys.platform.startswith('win'):
                del bc
                return result

            dt = bc.posix_timestamp(t - max_fold_seconds, *LOCAL_COORD,
                                    us=True, short=short, trim=False)
            date = _fix_short_date(dt, short)
            probe1 = cls(*date, tzinfo=tz)
            trans = result - probe1 - timedelta(0, max_fold_seconds)

            if trans.days < 0:
                t += trans // timedelta(0, 1)
                dt = bc.posix_timestamp(t, *LOCAL_COORD, us=True, short=short,
                                        trim=False)
                date = _fix_short_date(dt, short)
                probe2 = cls(*date, tzinfo=tz)

                if probe2 == result:
                    result._fold = 1

        elif tz is not None:
            result = fromutc(tz, result)

        del bc
        return result

    @classmethod
    def fromtimestamp(cls, t:float, tz:tzinfo=None, *, short:bool=False):
        """
        Construct a datetime from a POSIX timestamp representing local time
        (like time.time()).
        """
        _check_tzinfo_arg(tz)
        return cls._fromtimestamp(t, tz is not None, tz, short=short)

    # Both the ustfromtimestamp() and utcnow() methods have been depricated.
    # https://docs.python.org/3/deprecations/index.html

    @classmethod
    def now(cls, tz=None):
        """
        Construct a datetime from time.time() and optional time zone info.
        """
        return cls.fromtimestamp(_time.time(), tz)

    @classmethod
    def combine(cls, date:date, time:time, tzinfo:time.tzinfo=True):
        """
        Construct a datetime from a given date and a given time.
        """
        if not isinstance(date, _date_class):
            raise TypeError("date argument must be a date instance, "
                            f"found {date!r}.")

        if not isinstance(time, _time_class):
            raise TypeError("time argument must be a time instance, "
                            f"found {time!r}.")

        if tzinfo is True:
            tzinfo = time.tzinfo

        if date.is_short:
            d = (date.year, date.month, date.day)
        else:
            d = (date.kull_i_shay, date.vahid, date.year, date.month, date.day)

        return cls(*d, hour=time.hour, minute=time.minute, second=time.second,
                   microsecond=time.microsecond, tzinfo=tzinfo, fold=time.fold)

    @classmethod
    def fromisoformat(cls, date_string):
        """
        Construct a datetime from a string in one of the ISO 8601 formats.
        This only works with short form dates.
        """
        date, time, tz = _td_utils._parse_isoformat_date_time_timezone(
            date_string)
        t_len = len(time)
        time = list(time)
        time += [0 for v in range(4 - t_len)]
        hh, mm, ss, us = time
        return cls(*date, hour=hh, minute=mm, second=ss, microsecond=us,
                   tzinfo=tz)

    def timetuple(self):
        """
        Return local time tuple compatible with time.localtime().
        """
        dst = self.dst()

        if dst is None:
            dst = -1
        elif dst:
            dst = 1
        else:
            dst = 0

        return _td_utils._build_struct_time(
            self.b_date + (self.hour, self.minute, self.second),
            dst, short_in=self.is_short)

    def _mktime(self):
        """
        Return integer POSIX timestamp of local time.
        """
        if self.is_short:
            l_date = (126, 16, 2, None, None, 7, 58, 31)
        else:
            l_date = (1, 7, 12, 16, 2, 7, 58, 31)

        epoch = datetime(*l_date)
        t = (self - epoch) // timedelta(0, 1)

        def local(u):
            date = self.posix_timestamp(0, *LOCAL_COORD, trim=True,
                                        short=self.is_short)

            if self.is_short:
                date = (*date[:3], None, None, *date[3:])

            date = date[:-1] + (_math.floor(date[-1]),)
            dt = datetime(*date)

            #print(dt, epoch)

            return (dt - epoch) // timedelta(0, 1)

        # Our goal is to solve t = local(u) for u.
        a = local(t) - t
        u1 = t - a
        t1 = local(u1)

        if t1 == t:
            # We found one solution, but it may not be the one we need.
            # Look for an earlier solution (if `fold` is 0), or a
            # later one (if `fold` is 1).
            max_fold_seconds = 24 * 3600
            u2 = u1 + (-max_fold_seconds, max_fold_seconds)[self.fold]
            b = local(u2) - u2

            if a == b:
                return u1
        else:
            b = t1 - u1
            assert a != b

        u2 = t - b
        t2 = local(u2)
        #print(t, t1, t2, a, b, u1, u2, l_date)

        if t2 == t:
            return u2

        if t1 == t:
            return u1

        # We have found both offsets a and b, but neither t - a nor t - b is
        # a solution.  This means t is in the gap.
        return (max, min)[self.fold](u1, u2)

    def timestamp(self):
        """
        Return POSIX timestamp as float for the current datetime instance.
        """
        if self.tzinfo is None:
            s = self._mktime()
            return s + self.microsecond / 1e6
        else:
            return (self - _EPOCH).total_seconds()

    def utctimetuple(self):
        """
        Return UTC time tuple compatible with time.gmtime().
        """
        return self._timetuple(self.utcoffset())

    def baditimetuple(self):
        """
        Return BADI time tuple compatible with time.gmtime().
        """
        return self._timetuple(self.badioffset())

    def _timetuple(self, offset):
        """
        Return UTC or BADI time tuple compatible with time.gmtime().
        """
        if offset:
            self -= offset

        date = self.b_date + (self.hour, self.minute, self.second)
        return _td_utils._build_struct_time(date, 0, short_in=self.is_short)

    def date(self):
        """
        Return the date part.
        """
        return date(self._year, self._month, self._day)

    def time(self):
        """
        Return the time part, with tzinfo None.
        """
        return time(self.hour, self.minute, self.second, self.microsecond,
                    fold=self.fold)

    def timetz(self):
        """
        Return the time part, with same tzinfo.
        """
        return time(self.hour, self.minute, self.second, self.microsecond,
                    self.tzinfo, fold=self.fold)

    def replace(self, kull_i_shay=None, vahid=None, year=None, month=None,
                day=None, hour=None, minute=None, second=None,
                microsecond=None, tzinfo=True, *, fold=None):
        """
        Return a new datetime with new values for the specified fields.
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
            fold = self.fold

        if self.is_short and (kull_i_shay or vahid):
            msg = "Cannot convert from a short to a long form date."
            raise ValueError(msg)
        elif not self.is_short and year is not None and (year < 1 or year > 19):
            msg = ("Cannot convert from a long to a short form date. The "
                   f"value {year} is not valid for long form dates.")
            raise ValueError(msg)
        elif self.is_short:
            obj = self._replace_short(
                year=year, month=month, day=day, hour=hour, minute=minute,
                second=second, microsecond=microsecond, tzinfo=tzinfo,
                fold=fold)
        else:
            obj = self._replace_long(
                kull_i_shay=kull_i_shay, vahid=vahid, year=year, month=month,
                day=day, hour=hour, minute=minute, second=second,
                microsecond=microsecond, tzinfo=tzinfo, fold=fold)

        return obj

    def _local_timezone(self):
        """
        """
        if self.tzinfo is None:
            ts = self._mktime()
        else:
            ts = (self - _EPOCH) // timedelta(seconds=1)

        ts = self.posix_timestamp(ts, *LOCAL_COORD, short=self.is_short)
        ts_len = len(ts)

        if self.is_short:
            need = 6 - ts_len
            ts += (0,) * need if need > 0 else ()
            localtm = _td_utils._build_struct_time(ts, -1, short_in=True)
            local = datetime(*localtm[:6])
        else:
            need = 8 - ts_len
            ts += (0,) * need if need > 0 else ()
            localtm = _td_utils._build_struct_time(ts, -1, short_in=False)
            local = datetime(*localtm[:8])

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
        """
        Return ctime() style string.
        """
        if self.is_short:
            date = self.b_date + self.b_time
        else:
            date = self._short_from_long_form(time=self.b_time)

        weekday = _td_utils._day_of_week(*date[:3])
        wd_name = _td_utils.DAYNAMES[weekday]
        year, month, day, hour, minute, second, us = date
        m_name = _td_utils.MONTHNAMES[month]
        y_shim = 4 if year > -1 else 5
        return (f"{wd_name} {m_name} {day:2d} "
                f"{hour:02d}:{minute:02d}:{second:02d} {year:0{y_shim}d}")

    def isoformat(self, sep='T', timespec='auto'):
        """
        Return the time formatted according to ISO.

        The full format looks like 'YYYY-MM-DDTHH:MM:SS.mmmmmm'.
        By default, the fractional part is omitted if self.microsecond == 0.

        If self.tzinfo is not None, the UTC offset is also attached, giving
        giving a full format of 'YYYY-MM-DDTHH:MM:SS.mmmmmm+HH:MM'.

        Optional argument sep specifies the separator between date and
        time, default ' '.

        The optional argument timespec specifies the number of additional
        terms of the time to include. Valid options are 'auto', 'hours',
        'minutes', 'seconds', 'milliseconds' and 'microseconds'.
        """
        s = (f"{self._year:04d}-{self._month:02d}-{self._day:02d}{sep}" +
             _format_time(self._hour, self._minute, self._second,
                          self._microsecond, timespec))

        off = self.utcoffset()
        tz = _format_offset(off)

        if tz:
            s += tz

        return s

    def __repr__(self):
        """
        Convert to formal string, for repr().
        """
        L = [self._hour, self._minute, self._second, self._microsecond]

        for i in range(2):
            if L[-1] == 0:
                del L[-1]

        s = (f"{_module_name(self.__class__.__module__)}."
             f"{self.__class__.__qualname__}")

        if hasattr(self, '_kull_i_shay'):
            s += (f"({self._kull_i_shay}, {self._vahid}, {self._year}, "
                  f"{self._month}, {self._day}")
        else:
            s += f"({self._year}, {self._month}, {self._day}"

        s += f", {', '.join(map(str, L))})"

        if self.tzinfo is not None:
            assert s[-1:] == ")"
            s = s[:-1] + f", tzinfo={self.tzinfo!r})"

        if self._fold:
            assert s[-1:] == ")"
            s = s[:-1] + ", fold=1)"

        return s

    def _dt_str_conversion(self, sep='T'):
        if self.is_short:
            ret = self.isoformat(sep=sep)
        else:
            ret = self._str_convertion()
            ret += f"{sep}{self.hour:02}:{self.minute:02}:{self.second:02}"
            ret += f".{self.microsecond}" if self.microsecond else ""
            off = self.utcoffset()
            tz = _format_offset(off)

            if tz:
                ret += tz

        return ret

    __str__ = _dt_str_conversion

    @classmethod
    def strptime(cls, date_string, format):
        """
        String, format -> new datetime parsed from a string
        (like time.strptime()).

        *** TODO *** _strptime may need to be rewritten, bummer.
        """
        import _strptime
        return _strptime._strptime_datetime(cls, date_string, format)

    def utcoffset(self):
        """
        Return the timezone offset as timedelta positive east of UTC
        (negative west of UTC).
        """
        if self.tzinfo is not None:
            offset = self.tzinfo.utcoffset(self)
            _check_offset("utcoffset", offset)
            return offset

    def badioffset(self):
        """
        Return the timezone offset as timedelta positive east of the BADI
        (negative west of the BADI).
        """
        if self.tzinfo is not None:
            offset = self.tzinfo.badioffset(self)
            _check_offset("badioffset", offset)
            return offset

    def tzname(self):
        """
        Return the timezone name.

        Note that the name is 100% informational -- there's no requirement that
        it mean anything in particular. For example, 'GMT', 'UTC', '-500',
        '-5:00', 'EDT', 'US/Eastern', 'America/New_York' are all valid replies.
        """
        if self.tzinfo is not None:
            name = self.tzinfo.tzname(self)
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
        if self.tzinfo is not None:
            offset = self.tzinfo.dst(self)
            _check_offset("dst", offset)
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
        assert isinstance(other, datetime), (
            f"The other must be a datetime object, found '{type(other)}'.")
        mytz = self.tzinfo
        ottz = other.tzinfo
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
            return _cmp((self._year, self._month, self._day, self._hour,
                         self._minute, self._second, self._microsecond),
                        (other._year, other._month, other._day, other._hour,
                         other._minute, other._second, other._microsecond))

        if myoff is None or otoff is None:
            if allow_mixed:
                return 2 # arbitrary non-zero value
            else:
                raise TypeError("Cannot compare naive and aware datetimes.")

        # XXX What follows could be done more efficiently...
        diff = self - other     # this will take offsets into account

        if diff.days < 0:
            return -1

        return diff and 1 or 0

    def __add__(self, other):
        """
        Add a datetime and a timedelta.
        """
        if isinstance(other, timedelta):
            delta = timedelta(self.toordinal(),
                              hours=self._hour,
                              minutes=self._minute,
                              seconds=self._second,
                              microseconds=self._microsecond)
            delta += other
            hour, rem = divmod(delta.seconds, 3600)
            minute, second = divmod(rem, 60)

            if _td_utils.DAYS_BEFORE_1ST_YEAR < delta.days <= _MAXORDINAL:
                return type(self).combine(
                    date.fromordinal(delta.days, short=self.is_short),
                    time(hour, minute, second, delta.microseconds,
                         tzinfo=self.tzinfo))
        else:
            return NotImplemented

        raise OverflowError("Result out of range.")

    __radd__ = __add__

    def __sub__(self, other):
        """
        Subtract two datetimes, or a datetime and a timedelta.
        """
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

        if self.tzinfo is other.tzinfo:
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

    @classmethod
    def _is_pickle_data(cls, a, b):
        """
        Check if the incoming date is pickle data or actual date information.

        :param a: Pickle data, the kull_i_shay, or year.
        :type a: int, str, or bypes
        :param b: None, vahid, or month
        :type b: NoneType or int
        :return: A Boolean if a short or long Badi date derived from pickle
                 data. A None can be returned if a and b are real date
                 information.
        :rtype: bool or NoneType
        """
        if isinstance(b, (NoneType, tzinfo)) and isinstance(a, (bytes, str)):
            a_len = len(a)
            assert a_len in (10, 11), (
                f"Invalid string {a} had length of {a_len} for pickle.")
            short = True if a_len == 10 else False

            if ((short and 1 <= ord(a[2:3])&0x7F <= 19)
                or not short and 1 <= ord(a[3:4])&0x7F <= 19):
                if isinstance(a, str):
                    try:
                        a = a.encode('latin1')
                    except UnicodeEncodeError:
                        raise ValueError(
                            "Failed to encode latin1 string when unpickling "
                            "a date or datetime object. "
                            "pickle.load(data, encoding='latin1') is assumed.")
            else:
                short = None
        else:
            short = None

        return short

    def _getstate(self, protocol=3):
        if self.is_short:
            yhi, ylo = divmod(self._year - MINYEAR, 256)
            state = (yhi, ylo)
        else:
            # We need to add an arbitrarily number (19) larger that any
            # Kull-i-Shay that I support so we don't get a negative number
            # making bytes puke.
            kull_i_shay = self._kull_i_shay + 19
            vahid = self._vahid
            year = self._year
            state = (kull_i_shay, vahid, year)

        m = self._month

        if self._fold and protocol > 3:
            m += 128

        us2, us3 = divmod(self._microsecond, 256)
        us1, us2 = divmod(us2, 256)
        basestate = bytes(state + (m, self._day, self._hour, self._minute,
                                   self._second, us1, us2, us3))

        if self.tzinfo is None:
            return (basestate,)
        else:
            return (basestate, self.tzinfo)

    def __setstate(self, bytes_str, tzinfo):
        if tzinfo is not None and not isinstance(tzinfo, _tzinfo_class):
            raise TypeError("Bad tzinfo state arg.")

        if self.is_short:
            (yhi, ylo, m, self._day, self._hour,
             self._minute, self._second, us1, us2, us3) = bytes_str
            self._year = yhi * 256 + MINYEAR + ylo
            self.__date = (self.year,)
        else:
            (k, v, y, m, d, self._hour, self._minute,
             self._second, us1, us2, us3) = bytes_str
            self._kull_i_shay = k - 19
            self._vahid = v
            self._year = y
            self._day = d
            self.__date = (self.kull_i_shay, v, y)

        if m > 127:
            self._fold = 1
            self._month = m - 128
        else:
            self._fold = 0
            self._month = m

        self._microsecond = (((us1 << 8) | us2) << 8) | us3
        self.__date += (self._month, self._day)
        self.__time = (self._hour, self._minute,
                       self._second, self._microsecond)
        self._tzinfo = tzinfo

    def __reduce_ex__(self, protocol):
        return (self.__class__, self._getstate(protocol))

    def __reduce__(self): # pragma: no cover
        return self.__reduce_ex__(2)

datetime.min = datetime(-1842, 1, 1)
datetime.max = datetime(1161, 19, 19)
datetime.resolution = timedelta(microseconds=1)


class timezone(tzinfo):
    __slots__ = '_offset', '_name'

    # Sentinel value to disallow None
    _Omitted = object()

    def __new__(cls, offset:timedelta, name:str=_Omitted) -> object:
        if not isinstance(offset, timedelta):
            raise TypeError("offset must be a timedelta")

        if name is cls._Omitted:
            if not offset:
                return cls.utc

            name = None
        elif not isinstance(name, str):
            raise TypeError("name must be a string")

        if not cls._minoffset <= offset <= cls._maxoffset:
            raise ValueError("offset must be a timedelta strictly between "
                             "-timedelta(hours=24) and timedelta(hours=24).")

        return cls._create(offset, name)

    @classmethod
    def _create(cls, offset:timedelta, name:str=None):
        self = tzinfo.__new__(cls)
        self._offset = offset
        self._name = name
        return self

    def __eq__(self, other):
        if isinstance(other, timezone):
            return self._offset == other._offset

        return NotImplemented

    def __repr__(self):
        """
        Convert to formal string, for repr().

        >>> tz = timezone.utc
        >>> repr(tz)
        'datetime.timezone.utc'
        >>> tz = timezone(timedelta(hours=-5), 'EST')
        >>> repr(tz)
        'datetime.timezone(datetime.timedelta(-1, 68400), 'EST')'
        """
        if self is self.utc:
            return 'datetime.timezone.utc'

        if self is self.badi:
            return 'datetime.BADI'

        module = self.__class__.__module__
        offset_name = f"{self._offset!r}"

        if self._name is None:
            return (f"{_module_name(module)}.{self.__class__.__qualname__}"
                    f"({_module_name(offset_name)})")

        return (f"{_module_name(module)}.{self.__class__.__qualname__}"
                f"({_module_name(offset_name)}, {self._name!r})")

    def __str__(self):
        return self.tzname(None)

    def utcoffset(self, dt:datetime):
        if isinstance(dt, datetime) or dt is None:
            return self._offset

        raise TypeError("utcoffset() argument must be a datetime instance "
                        "or None")

    def badioffset(self, dt:datetime):
        if isinstance(dt, datetime) or dt is None:
            return self._offset

        raise TypeError("badioffset() argument must be a datetime instance "
                        "or None")

    def tzname(self, dt:datetime):
        if isinstance(dt, datetime) or dt is None:
            if self._name is None:
                return self._name_from_offset(self._offset)

            return self._name

        raise TypeError("tzname() argument must be a datetime instance "
                        "or None")

    def dst(self, dt:datetime):
        if isinstance(dt, datetime) or dt is None:
            return None

        raise TypeError("dst() argument must be a datetime instance or None")

    def fromutc(self, dt:datetime):
        if isinstance(dt, datetime):
            if dt.tzinfo is not self:
                raise ValueError("fromutc: dt.tzinfo is not self")

            return dt + self._offset

        raise TypeError("fromutc() argument must be a datetime instance "
                        "or None")

    _maxoffset = timedelta(hours=24, microseconds=-1)
    _minoffset = -_maxoffset

    @staticmethod
    def _name_from_offset(delta:timedelta) -> str:
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

    # pickle support

    def __getinitargs__(self):
        if self._name is None:
            return (self._offset,)

        return (self._offset, self._name)

    def __hash__(self):
        return hash(self._offset)

UTC = timezone.utc = timezone._create(timedelta(0))
BADI = timezone.badi = timezone._create(timedelta(hours=BADI_INFO[0]))
LOCAL = timezone.local = timezone._create(timedelta(hours=LOCAL_COORD[2]))

# bpo-37642: These attributes are rounded to the nearest minute for backwards
# compatibility, even though the constructor will accept a wider range of
# values. This may change in the future.
timezone.min = timezone._create(-timedelta(hours=23, minutes=59))
timezone.max = timezone._create(timedelta(hours=23, minutes=59))
_EPOCH = datetime(126, 16, 2, None, None, 7, 58,  31.4976, tzinfo=timezone.utc)
# bc.badi_date_from_gregorian_date((1970, 1, 1), *datetime.GMT_COORD,
#                                  short=True)
# (126, 16, 2, 7, 58, 31.4976, 0)
#
# bc.gregorian_date_from_badi_date((126, 16, 2, 7, 57, 27.7),
#                                  *datetime.GMT_COORD)
# (1970, 1, 1.0)
#
# jd = gc.jd_from_gregorian_date((1969, 12, 31))
# ss =  gc._sun_setting(jd, *datetime.GMT_COORD)
# (ss == 2440587.166985)
# gc.ymdhms_from_date(gc.gregorian_date_from_jd(ss))
# (1969, 12, 31, 16, 0, 27.504)
