# -*- coding: utf-8 -*-
#
# badidatetime/datetime.py
#
__docformat__ = "restructuredtext en"

__all__ = ('date', 'datetime', 'time', 'timezone', 'timedelta', 'tzinfo',
           'MINYEAR', 'MAXYEAR', 'BADI_IANA', 'BADI_COORD', 'GMT_COORD',
           'UTC', 'BADI', 'LOCAL_COORD', 'LOCAL', 'MONTHNAMES',
           'MONTHNAMES_ABV', 'DAYNAMES', 'DAYNAMES_ABV')

import sys
import time as _time
import math as _math
from datetime import timedelta, tzinfo
from types import NoneType

from .badi_calendar import BahaiCalendar
from ._coefficients import Coefficients
from ._timedateutils import _td_utils


_MAXORDINAL = 1097267  # date.max.toordinal()
MINYEAR = BahaiCalendar.MINYEAR
MAXYEAR = BahaiCalendar.MAXYEAR
BADI_IANA = BahaiCalendar._BAHAI_LOCATION[3]  # Asia/Terhan
BADI_COORD = BahaiCalendar._BAHAI_LOCATION[:3]
GMT_COORD = (51.477928, -0.001545, 0)
# LOCAL_COORD and LOCAL is lazily configured to the local coordinates
# if enables or the default is BADI_COORD, and BADI.
MONTHNAMES = [v for k, v in _td_utils.MONTHNAMES.items()]
MONTHNAMES_ABV = [v for k, v in _td_utils.MONTHNAMES_ABV.items()]
DAYNAMES = _td_utils.DAYNAMES
DAYNAMES_ABV = _td_utils.DAYNAMES_ABV


def _cmp(x, y):
    """
    Compare two items.

    :param object x: Item one.
    :param object y: Item two.
    :returns: If 0 then x == y, else if 1 then x > y else if -1 then x < y.
    """
    return 0 if x == y else 1 if x > y else -1


def _divide_and_round(a: int, b: int) -> int:
    """
    Divide a by b and round result to the nearest integer.

    When the ratio is exactly half-way between two integers,
    the even integer is returned.

    :param int a: numerator
    :param int b: denomerator
    :returns: Resultant value.
    :rtype: int
    """
    # Based on the reference implementation for divmod_near
    # in Objects/longobject.c.
    q, r = divmod(a, b)
    # Round up if either r / b > 0.5, or r / b == 0.5 and q is odd.
    # The expression r / b > 0.5 is equivalent to 2 * r > b if b is
    # positive, 2 * r < b if b negative.
    r *= 2
    greater_than_half = r > b if b > 0 else r < b
    return q + (1 if greater_than_half or r == b and q % 2 == 1 else 0)


def _check_offset(name: str, offset: timedelta) -> None:
    """
    Check that the arguments are valid. If offset is None, returns None else
    offset is checked for being in range.

    :param str name: Name is the offset-producing method, `utcoffset`,
                     `badioffset`, or `dst`.
    :param timedelta offset: The timezone offset.
    :raises assert: If the name is not in a list of constants.
    :raises TypeError: If `offset` is not a `timedelta` instance.
    :raises ValueError: If `offset` not greater than -1 and less than 1.
    """
    assert name in ('utcoffset', 'badioffset', 'dst'), (
        f"Invalid name argument '{name}' must be one of "
        "('utcoffset', 'badioffset', 'dst').")

    if offset is not None:
        if offset is not None:
            if not isinstance(offset, timedelta):
                raise TypeError(f"tzinfo.{name}() must return None "
                                f"or timedelta, not {type(offset)}")

            if not -timedelta(1) < offset < timedelta(1):
                raise ValueError(
                    f"{name}()={offset}, must be strictly between "
                    "-timedelta(hours=24) and timedelta(hours=24)")


def _check_tzinfo_arg(tz: tzinfo) -> None:
    """
    Check that the `tz` argument is either `None` or a `tzinfo` subclass.

    :param tzinfo tz: A `tzinfo` instance.
    :raises TypeError: If `tz is not `None` or a `tzinfo` subclass.
    """
    if tz is not None and not isinstance(tz, tzinfo):
        raise TypeError("tzinfo argument must be None or of a tzinfo "
                        f"subclass, found {tz!r}")


def _cmperror(x, y) -> None:
    """
    Test that `x` and `y` are the correct types to be compared.

    :param x: Item one.
    :param y: Item two.
    :raises TypeError: Argument `a` and `b` cannot be compared.
    """
    raise TypeError(f"Cannot compare {type(x).__name__!r} to "
                    f"{type(y).__name__!r}")


def _format_time(hh: int, mm: int, ss: int, us: int, timespec: str='auto'
                 ) -> str:
    """
    Format time to a string.

    :param int hh: The hour.
    :param int mm: The minute.
    :param int ss: The second.
    :param int us: The microsecond.
    :returns: A formatted string.
    :rtype: str
    """
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
        raise ValueError(f"Invalid timespec '{timespec}', must be one "
                         f"of {tuple(specs.keys())}.")
    else:
        return fmt.format(hh, mm, ss, us)


def _format_offset(off: timedelta) -> str:
    """
    Format an ISO offset.

    :param timedelta off: A `timedelta` instance.
    :returns: A formatted ISO offset.
    :rtype: str
    :raises TypeError: If `off` is not `None` or a `timedelta` instance.
    """
    if not isinstance(off, (timedelta, NoneType)):
        raise TypeError(f"The off value '{off}', must be a timedelta instance "
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


def _check_tzname(name: str) -> None:
    """
    Check that the `name` argument is either `None` or a `str`.

    :param str name: The name of the timezone.
    :raises TypeError: If `name` is not `None` or a `str`.
    """
    if name is not None and not isinstance(name, str):
        raise TypeError("tzinfo.tzname() must return None or string, "
                        f"not {type(name)!r}")


def _fromutc(this: tzinfo, dt):
    """
    The method fromutc() in tzinfo will not accept the Badí' datetime object,
    so this method needs to fill the role.

    .. note::

       This function needs to be implemented outside the `tzinfo` class so
       that the `tzinfo` class can be used with the `zoneinfo` class. Normally
       the tzinfo class would just be overridden with this function added.

    :param tzinfo this: The tzinfo instance.
    :param datetime dt: A datetime instance.
    :returns: The adjusted datetime from the provided UTC datetime.
    :rtype: datetime
    :raises TypeError: If `dt` is not a `datetime` instance.
    :raises ValueError: If dt.tzinfo is not self.
    :raises ValueError: If a `None` value returned from utcoffset().
    :raises ValueError: If a `None` value returned from dst().
    """
    if not isinstance(dt, datetime):
        raise TypeError("_fromutc() requires a datetime argument.")

    if dt.tzinfo is not this:
        raise ValueError("_fromutc() dt.tzinfo is not this.")

    dtoff = dt.utcoffset()

    if dtoff is None:
        raise ValueError("_fromutc() requires a non-None utcoffset() result.")

    dtdst = dt.dst()

    if dtdst is None:
        raise ValueError("_fromutc() requires a non-None dst() result.")

    delta = dtoff - dtdst

    if delta:
        dt += delta
        dtdst = dt.dst()

        if dtdst is None:  # pragma: no cover
            raise ValueError("_fromutc(): dt.dst gave inconsistent "
                             "results; cannot convert.")

    return dt + dtdst


def _module_name(module: str) -> str:
    """
    Find the package name without the first directory.

    :param str module: The module name including it path information.
    :returns: The name of the module.
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

    def __new__(cls, a: int, b: int=None, c: int=None, d: int=None,
                e: int=None):
        """
        Instantiate the class.

        :param date cls: The class object.
        :param int a: Long form this value is the Kill-i-Shay and short form
                      it's the year. If b and c are None then a becomes the
                      pickle value that is parsed to the remaining values
                      below.
        :param int b: Long form this value is the Váḥid and short form it's
                      the month.
        :param int c: Long form this is the year and short form it's the day.
        :param int d: Long for this value is the month and in the short form
                      it's not used.
        :param int e: Long form this value is the day and in the short form
                      it's not used.
        :returns: The instantiated class.
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
    def fromtimestamp(cls, t: float, *, short: bool=False) -> object:
        """
        Construct a date from a POSIX timestamp (like time.time()).

        :param date cls: The class object.
        :param float t: The POSIX timestamp.
        :param bool short: If True the short for is returned. The default
                           is False.
        :returns: The instantiated class.
        :rtype: date
        """
        bc = BahaiCalendar()
        date = bc.posix_timestamp(t, *LOCAL_COORD, short=short, trim=True)
        del bc
        date = date[:3] if short else date[:5]
        return cls(*date)  # We do not need any time values.

    @classmethod
    def today(cls, *, short: bool=False) -> object:
        """
        Construct a date from time.time().

        :param date cls: The class object.
        :param bool short: If True the short for is returned. The default
                           is False.
        :returns: The instantiated class.
        :rtype: date
        """
        return cls.fromtimestamp(_time.time(), short=short)

    @classmethod
    def fromordinal(cls, n: int, *, short: bool=False) -> object:
        """
        Construct a date from a proleptic Badi ordinal.

        Bahá 1 of year 1 is day 1. Only the year, month and day are
        non-zero in the result.

        :param date cls: The class object.
        :param int n: The ordinal value.
        :param bool short: If True the short for is returned. The default
                           is False.
        :returns: The instantiated class.
        :rtype: date
        """
        bc = BahaiCalendar()
        date = _td_utils._ord2ymd(n, short=short)
        del bc
        return cls(*date)

    @classmethod
    def fromisoformat(cls, date_string: str, *, short: bool=False) -> object:
        """
        Construct a date from a string in ISO 8601 format.
        We only can convert from short form Badi dates.

        :param str date_string: A string representing the date.
        :param bool short: If True the short for is returned. The default
                           is False.
        :returns: The date instance derived from the string representation.
        :rtype: date
        """
        if not isinstance(date_string, str):
            raise TypeError("fromisoformat: argument must be a string.")

        if date_string.count('T') > 0 or date_string.count(' ') > 0:
            raise ValueError("A time indicator was found, this is invalid "
                             "for date parsing, isoformat string: "
                             f"{date_string!r}.")

        bc = BahaiCalendar()

        try:
            date = _td_utils._parse_isoformat_date(date_string)
        except Exception as e:
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
    def fromisocalendar(cls, year: int, week: int, day: int, *,
                        short: bool=False) -> object:
        """
        Construct a date from the ISO year, week number and weekday.

        This is the inverse of the date.isocalendar() function.

        :param int year: The Badi year.
        :param int week: The number of the week in the year.
        :param int day: Badi day in week.
        :returns: The date instance.
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
        Convert to formal a string, for repr().

        :returns: A string representing the date.
        :rtype: str

        .. note::

           >>> d = date(181, 1, 1)
           >>> repr(d)
           'datetime.date(181, 1, 1)'
        """
        msg = (f"{_module_name(self.__class__.__module__)}."
               f"{self.__class__.__qualname__}")

        if hasattr(self, '_kull_i_shay'):
            msg += (f"({self._kull_i_shay}, {self._vahid}, {self._year}, "
                    f"{self._month}, {self._day})")
        else:
            msg += f"({self._year}, {self._month}, {self._day})"

        return msg

    def _short_from_long_form(self, time: tuple=()) -> tuple:
        """
        Convert the long form Badi date to a short form Badi date and add
        the time if it exists.

        :param tuple time: A tuple representing the time. This is used by the
                           datetime class.
        :returns: The short form date from a long for date with the possible
                 time.
        :rtype: tuple
        """
        if self.is_short:
            date = (*self.b_date, None, None, *time)
        else:
            b_date = self.short_date_from_long_date(self.b_date + time)
            date = (*b_date[:3], None, None, *b_date[3:])

        return date

    def ctime(self) -> str:
        """
        Return ctime() style string in the short form Badi date.

        :returns: A string representing the weekday, month name, and year.
        :rtype: str
        """
        date = self._short_from_long_form()
        weekday = _td_utils._day_of_week(*date[:3])
        wd_name = _td_utils.DAYNAMES[weekday]
        year, month, day = date[:3]
        m_name = _td_utils.MONTHNAMES[month]
        y_shim = 4 if year > -1 else 5
        return f"{wd_name} {m_name} {day:2d} 00:00:00 {year:0{y_shim}d}"

    def strftime(self, fmt: str) -> str:
        """
        Returns a string representing the date from a format string.

        :param str fmt:The format string.
        :returns: A string representing the date.
        :rtype: str

        .. note::

           Example: '%d/%m/%Y'
        """
        return _td_utils._wrap_strftime(self, fmt, self.timetuple())

    def __format__(self, fmt: str) -> str:
        """
        Returns the same as strftime above except if the strings length is zero
        then __str__ is returned.

        :param str fmt: The format string used for formatting the output.
        :returns: A string representation of the format.
        :rtype: str
        """
        if isinstance(fmt, str):
            if len(fmt) != 0:
                ret = self.strftime(fmt)
            else:
                ret = str(self)

            return ret

        raise TypeError(
            f"Must be a str, not a {type(fmt).__name__}")  # pragma: no cover

    def isoformat(self) -> str:
        """
        Return the date formatted according to ISO.

        :returns: String of the year, month, and day.
        :rtype: str

        .. note::

           References:
             - http://www.w3.org/TR/NOTE-datetime
             - http://www.cl.cam.ac.uk/~mgk25/iso-time.html
        """
        year, month, day = self._short_from_long_form()[:3]
        return f"{year:04d}-{month:02d}-{day:02d}"

    def _str_convertion(self) -> str:
        """
        Return a string representation of the date. In the case of a short
        form date the returned Badí' date is in ISO format. There is no ISO
        standard for the long form Badí' date.

        :returns: A string representation of the short or long form Badí' date.
        :rtype: str
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
    def kull_i_shay(self) -> int:
        """
        Get the Kull-i-Shay’.

        :returns: The value associated with the Kull-i-Shay’.
        :rtype: int
        """
        return self._kull_i_shay

    @property
    def vahid(self) -> int:
        """
        Get the Váḥid.

        :returns: The value associated with the Váḥid.
        :rtype: int
        """
        return self._vahid

    @property
    def year(self) -> int:
        """
        Get the year.

        .. note::

           This value has a different meaning depending on if the date
           instance represents a long or short form date.

        :returns: The value associated with the year.
        :rtype: int
        """
        return self._year

    @property
    def month(self) -> int:
        """
        Get the month where 1 - 19 represents the normal Badí' month and 0
        represents Ayyám-i-Há.

        :returns: The value associated with the Ayyám-i-Há.
        :rtype: int
        """
        return self._month

    @property
    def day(self) -> int:
        """
        Get the day of the month where 1 - 19 represents the normal Badí'
        month and 1 - 4 or 5 represents Ayyám-i-Há.

        :returns: The value associated with the day of the month.
        :rtype: int
        """
        return self._day

    @property
    def is_short(self) -> bool:
        """
        Get `True` if the current instance represents a short form Badí'
        date or `False` if a long form Badí' date.

        :return `True` if short form date or `False` is long form date.
        :rtype: bool
        """
        return self.__short

    @property
    def b_date(self) -> tuple:
        """
        Get the Badí' date as a tuple.

        :returns: The Badí' date.
        :rtype: tuple
        """
        return self.__date

    # Standard conversions, __eq__, __le__, __lt__, __ge__, __gt__,
    # __hash__ (and helpers)

    def timetuple(self) -> tuple:
        """
        Return local time tuple compatible with time.localtime().

        :returns: The short or long form date.
        :rtype: NamedTuple
        """
        return _td_utils._build_struct_time(self.b_date + (0, 0, 0), -1,
                                            short_in=self.is_short)

    def toordinal(self) -> int:
        """
        Return proleptic Badí' ordinal for the year, month and day.

        Bahá 1 of year -1842 is day 1. Only the year, month and day values
        contribute to the result. If this class provides the long form
        Badí' date it is converted to the short form before processing.

        :returns: The ordinal representing the year, month, and day.
        :rtype: int
        """
        return _td_utils._ymd2ord(*self._short_from_long_form()[:3])

    def replace(self, kull_i_shay: int=None, vahid: int=None, year: int=None,
                month: int=None, day: int=None) -> object:
        """
        Return a new date with new values for the specified fields.

        :param int kull_i_shay: A value representing the Kull-i-Shay’.
        :param int vahid: A value representing the Váḥid.
        :param int year: A value representing the year.
        :param int month: A value representing the month.
        :param int day: A value representing the day.
        :returns: A date instance with replaced items.
        :rtype: date
        """
        if self.is_short and (kull_i_shay or vahid):
            msg = "Cannot convert from a short to a long form date."
            raise ValueError(msg)
        elif (not self.is_short and year is not None and
              (year < 1 or year > 19)):
            msg = ("Cannot convert from a long to a short form date. The "
                   f"value {year} is not valid for long form dates.")
            raise ValueError(msg)
        elif self.is_short:
            obj = self._replace_short(year=year, month=month, day=day)
        else:
            obj = self._replace_long(kull_i_shay=kull_i_shay, vahid=vahid,
                                     year=year, month=month, day=day)

        return obj

    def _replace_short(self, *, year: int=None, month: int=None, day: int=None,
                       hour: int=None, minute: int=None, second: int=None,
                       microsecond: int=None, tzinfo: tzinfo=True,
                       fold: int=None) -> object:
        """
        Replace any of the year, month, or day values.

        :param int year: A value representing the year.
        :param int month: A value representing the month.
        :param int day: A value representing the day.
        :param int hour: A value representing the hour.
        :param int minute: A value representing the minute.
        :param int second: A value representing the second.
        :param int microsecond: A value representing the microsecond.
        :param tzinfo tzinfo: A `tzinfo` instance representing the timezone.
        :param int fold: Either a 0 meaning not fold or a 1 indicating the
                         date in in the fold.
        :returns: The short form date instance with replaced items.
        :rtype: date
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

    def _replace_long(self, *, kull_i_shay: int=None, vahid: int=None,
                      year: int=None, month: int=None, day: int=None,
                      hour: int=None, minute: int=None, second: int=None,
                      microsecond: int=None, tzinfo=True,
                      fold: int=None) -> object:
        """
        Replace any of the kull_i_shay, vahid, year, month, or day values.

        :param int kull_i_shay: A value representing the Kull-i-Shay’.
        :param int vahid: A value representing the Váḥid.
        :param int year: A value representing the year.
        :param int month: A value representing the month.
        :param int day: A value representing the day.
        :param int hour: A value representing the hour.
        :param int minute: A value representing the minute.
        :param int second: A value representing the second.
        :param int microsecond: A value representing the microsecond.
        :param tzinfo tzinfo: A `tzinfo` instance representing the timezone.
        :param int fold: Either a 0 meaning not fold or a 1 indicating the
                         date in in the fold.
        :returns: The long form date instance with replaced items.
        :rtype: date
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

    def __eq__(self, other) -> bool:
        return (self._cmp(other) == 0 if isinstance(other, date)
                else NotImplemented)

    def __le__(self, other) -> bool:
        return (self._cmp(other) <= 0 if isinstance(other, date)
                else NotImplemented)

    def __lt__(self, other) -> bool:
        return (self._cmp(other) < 0 if isinstance(other, date)
                else NotImplemented)

    def __ge__(self, other) -> bool:
        return (self._cmp(other) >= 0 if isinstance(other, date)
                else NotImplemented)

    def __gt__(self, other) -> bool:
        return (self._cmp(other) > 0 if isinstance(other, date)
                else NotImplemented)

    def _cmp(self, other) -> int:
        """
        Returns an integer representation of >, ==, or < of two date instances.

        :param date other: The other date instance to compare to.
        :returns: If 0 then x == y, else if 1 then x > y else if -1 then x < y,
                  where x is the `self` instance and y is the `other` instance.
        :rtype: int
        """
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

    def __hash__(self) -> int:
        """
        Get the hash of the `self` instance.

        :returns: The hash.
        :rtype: int
        """
        if self._hashcode == -1:
            self._hashcode = hash(self._getstate())

        return self._hashcode

    # Computations

    def __add__(self, other):
        """
        Add a date to a timedelta.

        :param date other: The other `date` instance which is added to the
                           `self` instance.
        :rtype: date
        """
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

        :param date other: The other date instance which is subtracted from
                           the `self` instance.
        :returns: Subreacted date instances.
        :rtype: date
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

    def weekday(self) -> int:
        """
        Return day of the week, where Jalál (Saturday) == 0 ...
        Istiqlāl (Friday) == 6.

        :returns: The numerical (0 - 6) day-of-the-week.
        :rtype: int
        """
        date = self._short_from_long_form()
        return _td_utils._day_of_week(*date[:3])

    def isoweekday(self):
        """
        Return day of the week, where Jalál (Saturday) == 1 ...
        Istiqlāl (Friday) == 7. This is the ISO standard.

        :returns: The numerical (1 - 7) day-of-the-week.
        :rtype: int
        """
        date = self._short_from_long_form()[:3]
        return _td_utils._day_of_week(*date) + 1

    def isocalendar(self):
        """
        Return a `NamedTuple` containing ISO year, week number, and weekday.

        The first ISO week of the year is the (Jalál-Istiqlāl) week
        containing the year's first Fiḍāl; everything else derives
        from that.

        The first week is 1; Jalál is 1 ... Istiqlāl is 7.

        :returns: An ISO year, week, and day.
        :rtype: _IsoCalendarDate

        .. note::

           ISO calendar algorithm taken from
           http://www.phys.uu.nl/~vgent/calendar/isocalendar.htm
           modified for the Badi Calendar.
        """
        y, m, d = self._short_from_long_form()[:3]
        year, week, day = _td_utils._year_week_day(y, m, d)
        return _IsoCalendarDate(year, week, day)

    # Pickle support.

    @classmethod
    def _is_pickle_data(cls, a, b) -> bool:
        """
        Check if the incoming date is pickle data or actual date information.

        :param a: Pickle data, the Kull-i-Shay’, or year.
        :type a: int, str, or bytes
        :param b: None, Váḥid, or month
        :type b: NoneType or int
        :returns: A Boolean if a short or long Badi date derived from pickle
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
                            "a date or datetime instance. "
                            "pickle.load(data, encoding='latin1') is assumed.")
            else:
                short = None
        else:
            short = None

        return short

    def _getstate(self) -> bytes:
        """
        Get the current state.

        :returns: The state of the current `self` instance.
        :rtype: bytes
        """
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

    def __setstate(self, bytes_str: bytes) -> None:
        """
        Set the current state.

        :param bytes bytes_str: The bytes string.
        """
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

    def __reduce__(self) -> tuple:
        """
        A tuple of the class name and current state.

        :returns: The class name and current state.
        :rtype: tuple
        """
        return (self.__class__, self._getstate())


_date_class = date  # So functions w/ args named "date" can get at the class

# This also needs to be done for long form date. *** TODO ***
date.min = date(MINYEAR, 1, 1)
date.max = date(MAXYEAR, 19, 19)
date.resolution = timedelta(days=1)


_tzinfo_class = tzinfo


class _IsoCalendarDate(tuple):

    def __new__(cls, year: int, week: int, weekday: int, /) -> object:
        """
        This is a wrapper class around ISO year, week, and weekday.

        :param int, year: The year of the date.
        :param int, week: The week of the year.
        :param int weekday: The day of the week.
        :returns: The ISO representation of the year, week, and weekday.
        :rtype: _IsoCalendarDate
        """
        return super().__new__(cls, (year, week, weekday))

    @property
    def year(self) -> int:
        """
        Get the year.

        :returns: The year.
        :rtype: int
        """
        return self[0]

    @property
    def week(self) -> int:
        """
        Get the week.

        :returns: The week.
        :rtype: int
        """
        return self[1]

    @property
    def weekday(self) -> int:
        """
        Get the weekday.

        :returns: The weekday.
        :rtype: int
        """
        return self[2]

    def __reduce__(self) -> tuple:
        """
        A tuple of the class name and current state.

        :returns: The class name and current state.
        :rtype: tuple

        .. note::

           This code is intended to pickle the object without making the
           class public. See https://bugs.python.org/msg352381
        """
        return (tuple, (tuple(self),))

    def __repr__(self) -> str:
        """
        Convert to a formal string, for repr().

        :returns: A string representing the date.
        :rtype: str
        """
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

    def __new__(cls, hour: float=0, minute: float=0, second: float=0,
                microsecond: int=0, tzinfo: tzinfo=None, *,
                fold: int=0) -> object:
        """
        Constructor.

        :param float hour: Hours (required)
        :param float minute: Minutes (required)
        :param float second: Seconds (default to zero)
        :param int microsecond: Microseconds (default to zero)
        :param tzinfo tzinfo: Timezone information (default to None)
        :param int fold:  (keyword only, default to zero)
        :returns: The `self` instance.
        :rtype: time
        """
        if (isinstance(hour, (bytes, str)) and len(hour) == 6 and
            ord(hour[0:1]) & 0x7F < 24):
            # Pickle support
            if isinstance(hour, str):
                try:
                    hour = hour.encode('latin1')
                except UnicodeEncodeError:
                    # More informative error message.
                    raise ValueError(
                        "Failed to encode latin1 string when unpickling "
                        "a time instance. "
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
    def hour(self) -> float:
        """
        Get the hour.

        :returns: The hour.
        :rtype: float
        """
        return self._hour

    @property
    def minute(self) -> float:
        """
        Get the minute.

        :returns: The minute.
        :rtype: float
        """
        return self._minute

    @property
    def second(self) -> float:
        """
        Get the second.

        :returns: The second.
        :rtype: float
        """
        return self._second

    @property
    def microsecond(self) -> int:
        """
        Get the microsecond.

        :returns: The microsecond.
        :rtype: int
        """
        return self._microsecond

    @property
    def tzinfo(self) -> tzinfo:
        """
        Get the timezone info instance.

        :returns: The timezone info instance.
        :rtype: tzinfo
        """
        return self._tzinfo

    @property
    def fold(self) -> int:
        """
        Get the time fold. This is in the Autumn when the time is set back
        from daylight savings time to standard time and the same hour is
        repeated.

        :returns: The time fold.
        :rtype: int
        """
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
        """
        A low level time compare method.

        :param time other: Another time instance.
        :param bool allow_mixed: True if a naive and aware time objects are
                                 allowed else if False they are not allowed.
                                 Only the __eq__ method sets this to True.
        :returns: 0 if self and other are equal, 1 if self > other, and -1
                  self < other.
        :rtype: int
        """
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
                return 2  # arbitrary non-zero value
            else:
                raise TypeError("Cannot compare naive and aware times.")

        myhhmm = self._hour * 60 + self._minute - myoff//timedelta(minutes=1)
        othhmm = other._hour * 60 + other._minute - otoff//timedelta(minutes=1)
        return _cmp((myhhmm, self._second, self._microsecond),
                    (othhmm, other._second, other._microsecond))

    # Standard conversions, __hash__ (and helpers)

    def __hash__(self) -> int:
        """
        Get the hash of the `self` instance.

        :returns: The hash.
        :rtype: int
        """
        if self._hashcode == -1:
            if self.fold:
                t = self.replace(fold=0)
            else:
                t = self

            tzoff = t.utcoffset()

            if not tzoff:  # zero or None
                self._hashcode = hash(t._getstate()[0])
            else:
                h, m = divmod(timedelta(
                    hours=self.hour, minutes=self.minute) - tzoff,
                              timedelta(hours=1))
                assert not m % timedelta(minutes=1), "Must be a whole minute."
                m //= timedelta(minutes=1)

                if 0 <= h < 24:
                    self._hashcode = hash(time(h, m, self.second,
                                               self.microsecond))
                else:
                    self._hashcode = hash((h, m, self.second,
                                           self.microsecond))

        return self._hashcode

    # Conversion to string

    def _tzstr(self) -> str:
        """
        Return a formatted timezone offset (+xx:xx) or an empty string.

        :returns: The formatted timezone offset.
        :rtype: str
        """
        off = self.utcoffset()
        return _format_offset(off)

    def __repr__(self) -> str:
        """
        Convert to formal string, for repr().

        :returns: A string representing the current `self` instance.
        :rtype: str
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

    def isoformat(self, timespec: str='auto') -> str:
        """
        Return the time formatted according to ISO.

        The full format is 'HH:MM:SS.mmmmmm+zz:zz'. By default, the fractional
        part is omitted if self.microsecond == 0.

        The optional argument timespec specifies the number of additional
        terms of the time to include. Valid options are `auto`, `hours`,
        `minutes`, `seconds`, `milliseconds` and `microseconds`.

        :param str timespec: The specification is either `auto` (default) or
                             `milliseconds`.
        :returns: An ISO formatted string.
        :rtype: str
        """
        s = _format_time(self._hour, self._minute, self._second,
                         self._microsecond, timespec)
        tz = self._tzstr()

        if tz:
            s += tz

        return s

    __str__ = isoformat

    @classmethod
    def fromisoformat(cls, t_str: str):
        """
        Construct a time from a string in one of the ISO 8601 formats.

        :param str t_str: An ISO formatted string.
        :returns: An instance of time.
        :rtype: time
        """
        if isinstance(t_str, str):
            try:
                return cls(*_td_utils._parse_isoformat_time(t_str))
            except Exception as e:
                raise ValueError(f'Invalid isoformat string: {t_str!r}, {e}')

        raise TypeError('fromisoformat: argument must be str')

    def strftime(self, format: str) -> str:
        """
        Format using strftime(). The date part of the timestamp passed to
        underlying strftime should not be used.

        :param str format: The string format.
        :returns: An updated format string.
        :rtype: str
        """
        # We use the Badí' epoch for the year, month and day.
        timetuple = (1, 1, 1, self._hour, self._minute, self._second, 0, 1, -1)
        return _td_utils._wrap_strftime(self, format, timetuple)

    def __format__(self, fmt: str) -> str:
        """
        More or less the same as strftime, however, if `fmt` is 0 length,
        then return `str(self)`.

        :param str fmt: The format string.
        :returns: A string formatted as pre the `fmt` argument.
        :rtype: str
        """
        if isinstance(fmt, str):
            if len(fmt) != 0:
                ret = self.strftime(fmt)
            else:
                ret = str(self)

            return ret

        raise TypeError(f"Must be a str, not {type(fmt).__name__}.")

    # Timezone functions

    def utcoffset(self) -> timedelta:
        """
        Return the timezone offset as timedelta, positive east of UTC
        (negative west of UTC).

        :returns: The offset from UTC.
        :rtype: timedelta
        """
        if self.tzinfo is not None:
            offset = self.tzinfo.utcoffset(None)
            _check_offset("utcoffset", offset)
            return offset

    def badioffset(self) -> timedelta:
        """
        Return the timezone offset as timedelta, positive east of Asia/Tehran
        (negative west of UTC).

        :returns: The offset from UTC.
        :rtype: timedelta
        """
        if self.tzinfo is not None:
            offset = self.utcoffset()

            if offset is not None:
                offset -= timedelta(hours=BADI_COORD[2])

            return offset

    def tzname(self) -> str:
        """
        Return the timezone name.

        :returns: A name representing the time zone.
        :rtype: str

        .. note::

           The name is 100% informational -- there's no requirement that
           it mean anything in particular. For example, *GMT*, *UTC*, *-500*,
           *-5:00*, *EDT*, *US/Eastern*, *America/New York* are all valid
           responses.
        """
        if self.tzinfo is not None:
            name = self.tzinfo.tzname(None)
            _check_tzname(name)
            return name

    def dst(self) -> timedelta:
        """
        Return 0 if DST is not in effect, or the DST offset (as timedelta
        positive eastward) if DST is in effect.

        :returns: The DST offset from UTC.
        :rtype: timedelta

        .. note::

           This is purely informational; the DST offset has already been
           added to the UTC offset returned by utcoffset() if applicable,
           so there's no need to consult dst() unless you're interested in
           displaying the DST info.
        """
        if self.tzinfo is not None:
            offset = self.tzinfo.dst(None)
            _check_offset("dst", offset)
            return offset

    def replace(self, hour: float=None, minute: float=None, second: float=None,
                microsecond: int=None, tzinfo: tzinfo=True, *, fold: int=None):
        """
        Return a new time with new values for the specified fields.

        :param float hour: Hours (required)
        :param float minute: Minutes (required)
        :param float second: Seconds (default to zero)
        :param int microsecond: Microseconds (default to zero)
        :param tzinfo tzinfo: Timezone information (default to None)
        :param int fold:  (keyword only, default to zero)
        :returns: A new time instance updated with the provided information.
        :rtype: time
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

    def _getstate(self, protocol: int=3) -> tuple:
        """
        Get the state of the current time instance.

        :param int protocol: The protocol used to derive the state
                             (default is 3).
        :returns: The current state of the `self` instance.
        :rtype: tuple
        """
        us2, us3 = divmod(self._microsecond, 256)
        us1, us2 = divmod(us2, 256)
        h = self._hour
        h += 128 if self._fold and protocol > 3 else 0
        basestate = bytes([h, self._minute, self._second, us1, us2, us3])

        if self.tzinfo is None:
            return (basestate,)
        else:
            return (basestate, self.tzinfo)

    def __setstate(self, string: str, tzinfo: tzinfo) -> None:
        """
        Set the current state of the `self` instance.

        :param str string: A byte string.
        :param tzinfo, tzinfo: Time zone information.
        :raises TypeError: If the `tzinfo` argument is not a `tzinfo` instance.
        """
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

    def __reduce_ex__(self, protocol: int) -> tuple:
        """
        Get the class name and the default protocol state.

        :param int protocol: The protocol used to derive the state.
        :returns: The class name and current state.
        :rtype: tuple
        """
        return (self.__class__, self._getstate(protocol))

    def __reduce__(self) -> tuple:  # pragma: no cover
        """
        Get the class name and current state using protocol 2.

        :returns: The class name and current state.
        :rtype: tuple
        """
        return self.__reduce_ex__(2)


_time_class = time  # so functions w/ args named "time" can get at the class

time.min = time(0, 0, 0)
time.max = time(24, 0, 3)  # See contrib/misc/badi_jd_tests.py --day
time.resolution = timedelta(microseconds=1)


class datetime(date, Coefficients):
    """
    datetime(year, month, day[, hour[, minute[, second[,
             microsecond[,tzinfo]]]]])

    The year, month and day arguments are required. tzinfo may be None, or an
    instance of a tzinfo subclass. The remaining arguments may be ints.
    """
    __slots__ = date.__slots__ + time.__slots__

    def __new__(cls, a: int, b: int=None, c: int=None, d: int=None,
                e: int=None, hour: float=0, minute: float=0, second: float=0,
                microsecond: int=0, tzinfo: tzinfo=None, *,
                fold: int=0) -> object:
        """
        Check if there is pickle data. If so parse and create the objcet. If
        not pickle data create the instance from the incoming date data.

        :param int a: If pickle data this is the bytes string. If not pickle
                      data this could be the Kull-i-Shay’ if a long form date
                      or the year if a short form date.
        :param int b: If pickle data this is the `tzinfo` instance. If not
                      pickle data this could be the Váḥid if a long form date
                      or the month if a short form date.
        :param int c: If a long form date this is the year or the day if a
                      short form date is used.
        :param int d: If a long form date this is the month, it is not used
                      with a short form date.
        :param int e: If a long form date this is the day, it is not used
                      with a short form date.
        :param float hour: The hour of the dat.
        :param float minute: The minute of the hour.
        :param float second: The second of the minute.
        :param int microsecond: The microsecond.
        :param tzinfo tzinfo: The time zone information.
        :param int fold: If *0* there is no fold in time, this is the more
                         common situation, however, if it is *1* there is a
                         fold in time.
        :returns: The instance of the datetime class.
        :rtype: datetime
        """
        if (short := datetime._is_pickle_data(a, b)) is not None:
            self = object.__new__(cls)
            super().__init__(self)
            self.__short = short
            self.__setstate(a, b)
        else:
            b_date = tuple([x for x in (a, b, c, d, e) if x is not None])
            date_len = len(b_date)
            assert date_len in (3, 5), (
                "A full short or long form Badi date must be used, found "
                f"{date_len} fields.")
            self = object.__new__(cls)
            super().__init__(self)

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

            self._tzinfo = tzinfo
            self._fold = fold
            self._create_time(hour, minute, second, microsecond)

        _td_utils._check_date_fields(*self.__date, short_in=self.__short)
        _td_utils._check_time_fields(hour, minute, second, microsecond, fold)
        _check_tzinfo_arg(tzinfo)
        self._hashcode = -1
        return self

    def _create_time(self, hour: float, minute: float, second: float,
                     microsecond: int) -> None:
        """
        Create the time portion of the `datetime` instance.

        :param float hour: The hour of the dat.
        :param float minute: The minute of the hour.
        :param float second: The second of the minute.
        :param int microsecond: The microsecond.
        """
        def fractionals(value, items):
            if value % 1 and any(items):
                raise ValueError("A fractional value cannot be followed by "
                                 "a least significant value.")

        fractionals(hour, (minute, second, microsecond))
        fractionals(minute, (second, microsecond))
        fractionals(second, (microsecond,))

        if hour % 1:
            mm = self._PARTIAL_HOUR_TO_MINUTE(hour)
            ss = self._PARTIAL_MINUTE_TO_SECOND(mm)
            self._hour = _math.floor(hour)
            self._minute = _math.floor(mm)
            self._second = _math.floor(ss)
            self._microsecond = self._PARTIAL_SECOND_TO_MICROSECOND(ss)
        elif minute % 1:
            self._hour = hour
            ss = self._PARTIAL_MINUTE_TO_SECOND(minute)
            self._minute = _math.floor(minute)
            self._second = _math.floor(ss)
            self._microsecond = self._PARTIAL_SECOND_TO_MICROSECOND(ss)
        elif second % 1:
            self._hour = hour
            self._minute = minute
            self._second = _math.floor(second)
            self._microsecond = self._PARTIAL_SECOND_TO_MICROSECOND(second)
        else:
            self._hour = hour
            self._minute = minute
            self._second = second
            self._microsecond = round(microsecond, 6)

        self.__time = (self._hour, self._minute,
                       self._second, self._microsecond)

    # Read-only field accessors
    @property
    def hour(self) -> float:
        """
        Get the hour.

        :returns: The hour.
        :rtype: float
        """
        return self._hour

    @property
    def minute(self) -> float:
        """
        Get the minute.

        :returns: The minute.
        :rtype: float
        """
        return self._minute

    @property
    def second(self) -> float:
        """
        Get the second.

        :returns: The second.
        :rtype: float
        """
        return self._second

    @property
    def microsecond(self) -> int:
        """
        Get the microsecond.

        :returns: The microsecond.
        :rtype: int
        """
        return self._microsecond

    @property
    def tzinfo(self) -> tzinfo:
        """
        Get the timezone info instance.

        :returns: The timezone info instance.
        :rtype: tzinfo
        """
        return self._tzinfo

    @property
    def fold(self) -> int:
        """
        Get the time fold. This is in the Autumn when the time is set back
        from daylight savings time to standard time and the same hour is
        repeated.

        :returns: The time fold.
        :rtype: int
        """
        return self._fold

    @property
    def b_date(self) -> tuple:
        """
        Get the Badí' date as a tuple.

        :returns: The Badí' date.
        :rtype: tuple
        """
        return self.__date

    @property
    def b_time(self) -> tuple:
        """
        Get the time as a tuple.

        :returns: The time.
        :rtype: tuple
        """
        return self.__time

    @property
    def is_short(self) -> bool:
        """
        Get `True` if the current instance represents a short form Badí'
        date or `False` if a long form Badí' date.

        :return `True` if short form date or `False` is long form date.
        :rtype: bool
        """
        return self.__short

    @classmethod
    def _fromtimestamp(cls, t, utc, tz, *, short=False):
        """
        Construct a datetime from a POSIX timestamp (like time.time()).

        :param float t: POSIX timestamp.
        :param bool utc: If True then the result is relative to UTC time else
                         if False it is relative to local time.
        :param tzinfo tz: A tzinfo instance.
        :param bool short: If False (default) the result is in the long Badi
                           form, else if True it is in the short form.
        :returns: A datetime instance set to the date derived from the
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
                del bc  # pragma: no cover
                return result  # pragma: no cover

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

                if probe2 == result:  # pragma: no cover
                    result._fold = 1
        elif tz is not None:
            result = _fromutc(tz, result)

        del bc
        return result

    @classmethod
    def fromtimestamp(cls, t: float, tz: tzinfo=None, *, short: bool=False):
        """
        Construct a datetime from a POSIX timestamp representing local time
        (like time.time()).

        :param float t: The timestamp.
        :param tzinfo tz: The `tzinfo` instance.
        :param bool short: If True the short for is returned. The default
                           is False.
        :returns: A `datetime` instance.
        :rtype: datetime
        """
        _check_tzinfo_arg(tz)
        return cls._fromtimestamp(t, tz is not None, tz, short=short)

    # Both the ustfromtimestamp() and utcnow() methods have been deprecated.
    # https://docs.python.org/3/deprecations/index.html

    @classmethod
    def now(cls, tz: tzinfo=None, short: bool=False):
        """
        Construct a datetime from time.time() and optional time zone info.

        :param tzinfo tz: The `timezone` instance.
        :param bool short: If True the short for is returned. The default
                           is False.
        :returns: A `datetime` instance.
        :rtype: datetime
        """
        return cls.fromtimestamp(_time.time(), tz, short=short)

    @classmethod
    def combine(cls, date: date, time: time, tzinfo: tzinfo=True):
        """
        Construct a datetime from a given date and a given time.

        :param date date: A date instance.
        :param time time: A time instance.
        :param tzinfo tzinfo: A tzinfo instance.
        :returns: A `datetime` instance.
        :rtype: datetime
        """
        if not isinstance(date, _date_class):
            raise TypeError("The date argument must be a date instance, "
                            f"found {date!r}.")

        if not isinstance(time, _time_class):
            raise TypeError("The time argument must be a time instance, "
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
    def fromisoformat(cls, date_string: str):
        """
        Construct a datetime from a string in one of the ISO 8601 formats.
        This only works with short form dates.

        :param str date_string: An ISO formatted string.
        :returns: A `datetime` instance.
        :rtype: datetime
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

        :returns: A tuple of the date and time values.
        :rtype: NamedTuple
        """
        date = self.b_date + self.b_time[:3]
        return _td_utils._build_struct_time(date, -1, tzinfo=self.tzinfo,
                                            short_in=self.is_short)

    def _mktime(self) -> float:
        """
        Return integer POSIX timestamp.

        .. warning::

           Because of the nature of the Badí' calendar the resulting
           timestamps will have a deviation from -241 to 121 seconds with
           regards to the GMT timestamp. This means that any code derived
           from this method may also have issues. Also effected are:
           `timestamp()`, `_local_timezone()`, and `astimezone()`.

        :returns: The POSIX timestamp.
        :rtype: float
        """
        def gmt(u):
            date = self.posix_timestamp(u, *GMT_COORD, short=True, us=True)
            return (datetime(*date[:3], None, None, *date[3:6]) -
                    epoch) // timedelta(0, 1)

        epoch = datetime(126, 16, 2, None, None, *self._get_badi_hms(126))
        date = self._short_from_long_form(time=self.b_time)
        t = (datetime(*date) - epoch) // timedelta(0, 1)
        a = gmt(t) - t
        u1 = t - a
        t1 = gmt(u1)
        coeff = self._get_ts_coeff(date[0])
        return t1 - coeff

    def _get_badi_hms(self, year: int):
        """
        Find the correct hour and minute of the day based on the coordinents.
        """
        jd = self.jd_from_badi_date((year, 16, 2), *LOCAL_COORD)
        mjd = jd + self._meeus_from_exact(jd)
        ss = self._sun_setting(mjd, *LOCAL_COORD)
        # Round to the nearest minute.
        f_ss = _math.floor(ss) + round(ss % 1 * 1440) / 1440
        # Where 24 is hours in a day and offset from GMT.
        b_time = (((24 + LOCAL_COORD[-1]) / 24) - (f_ss + 0.5)) % 1
        return self._hms_from_decimal_day(b_time)[:2]  # hh & mm only

    def _get_ts_coeff(self, year: int) -> int:
        def years(pn_all):
            data = []

            for pn in pn_all:
                if isinstance(pn, int):
                    data.append(pn)
                elif isinstance(pn, tuple):
                    start, end = pn
                    data += range(start, end+1)
                else:  # pragma: no cover
                    assert False, ("The 'pn' argument can only be an int or "
                                   f"tuple, found: {type(pn)}")

            return data

        if year in self._PN01:
            coeff = 86520
        elif year in self._PN02:
            coeff = 86400
        elif year in self._PN03:
            coeff = 86160
        elif year in self._PN04:
            coeff = 86040
        elif year in self._PN05:
            coeff = 85860
        elif year in self._PN06:
            coeff = 85800
        elif year in self._PN07:
            coeff = 85620
        elif year in self._PN08:
            coeff = 180
        elif year in self._PN09:
            coeff = 121
        elif year in years(self._PN10):
            coeff = 120
        elif year in self._PN11:
            coeff = 119
        elif year in self._PN12:
            coeff = -1
        elif year in self._PN13:
            coeff = -60
        elif year in years(self._PN14):
            coeff = -180
        elif year in self._PN15:
            coeff = -181
        elif year in years(self._PN16):
            coeff = -240
        elif year in self._PN17:
            coeff = -359
        elif year in years(self._PN18):
            coeff = -360
        elif year in self._PN19:
            coeff = -419
        elif year in years(self._PN20):
            coeff = -420
        elif year in years(self._PN21):
            coeff = -540
        elif year in years(self._PN22):
            coeff = -600
        elif year in years(self._PN23):
            coeff = -720
        elif year in self._PN24:
            coeff = -721
        elif year in self._PN25:
            coeff = -780
        elif year in self._PN26:
            coeff = -86220
        elif year in self._PN27:
            coeff = -86279
        elif year in years(self._PN28):
            coeff = -86280
        elif year in self._PN29:
            coeff = -86281
        elif year in years(self._PN30):
            coeff = -86400
        elif year in self._PN31:
            coeff = -86401
        elif year in years(self._PN32):
            coeff = -86460
        elif year in years(self._PN33):
            coeff = -86580
        elif year in self._PN34:
            coeff = -86581
        elif year in self._PN35:
            coeff = -86640
        elif year in self._PN36:
            coeff = -86759
        elif year in years(self._PN37):
            coeff = -86760
        elif year in self._PN38:
            coeff = -86819
        elif year in self._PN39:
            coeff = -86820
        elif year in years(self._PN40):
            coeff = -86940
        elif year in self._PN41:
            coeff = -87000
        elif year in self._PN42:
            coeff = -87120
        elif year in self._PN43:
            coeff = -87121
        elif year in self._PN44:
            coeff = -172620
        elif year in self._PN45:
            coeff = -172679
        elif year in self._PN46:
            coeff = -172680
        elif year in self._PN47:
            coeff = -172800
        elif year in self._PN48:
            coeff = -172980
        elif year in self._PN49:
            coeff = -173160
        elif year in self._PN50:
            coeff = -173340
        else:
            coeff = 0

        return coeff

    def timestamp(self) -> float:
        """
        Return POSIX timestamp for the current datetime instance.

        .. warning::

           This method is derived from `_mktime()` and may have accuracy
           issues.

        :returns: The POSIX timestamp.
        :rtype: float
        """
        if self.tzinfo is None:
            return self._mktime() + self.microsecond / 1e6
        else:
            return (self - _EPOCH).total_seconds()

    def utctimetuple(self) -> tuple:
        """
        Return UTC time tuple compatible with time.gmtime().

        :returns: The UTC time tuple.
        :rtype: NamedTuple
        """
        return self._timetuple(self.utcoffset())

    def _timetuple(self, offset) -> tuple:
        """
        Return UTC or BADI time tuple compatible with time.gmtime().

        :returns: The UTC time tuple.
        :rtype: NamedTuple
        """
        if offset:
            self -= offset

        date = self.b_date + (self.hour, self.minute, self.second)
        return _td_utils._build_struct_time(date, 0, short_in=self.is_short)

    def date(self) -> date:
        """
        Return the date part of the `datetime` instance.

        :returns: The date part of the `datetime` instance.
        :rtype: date
        """
        return date(self._year, self._month, self._day)

    def time(self):
        """
        Return the time part, with tzinfo None of the `datetime` instance.

        :returns: The time part of the `datetime` instance.
        :rtype: time
        """
        return time(self.hour, self.minute, self.second, self.microsecond,
                    fold=self.fold)

    def timetz(self) -> time:
        """
        Return the time part, with same tzinfo of the `datetime` instance.

        :returns: The time part of the `datetime` instance.
        :rtype: time
        """
        return time(self.hour, self.minute, self.second, self.microsecond,
                    self.tzinfo, fold=self.fold)

    def replace(self, kull_i_shay: int=None, vahid: int =None, year: int =None,
                month: int =None, day: int=None, hour: float=None,
                minute: float=None, second: float=None, microsecond: int=None,
                tzinfo: tzinfo=True, *, fold: int=None):
        """
        Return a new datetime with new values for the specified fields.

        :param int kull_i_shay: The Kull-i-Shay’.
        :param int vahid: The Váḥid.
        :param int year: The year.
        :param int month: The month.
        :param int day: The day.
        :param float hour: The hour.
        :param float minute: The minute.
        :param float second: The second.
        :param int microsecond: The microsecond.
        :param tzinfo tzinfo: A `tzinfo` instance.
        :param int fold: The time fold.
        :returns: The updated `datetime` instance.
        :rtype: datetime
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

        if fold is None:  # pragma: no cover
            fold = self.fold

        if self.is_short and (kull_i_shay or vahid):
            msg = "Cannot convert from a short to a long form date."
            raise ValueError(msg)
        elif (not self.is_short and year is not None and
              (year < 1 or year > 19)):
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
        Always return the local time offset in a timezone instance.

        .. warning::

           This method is derived from `_mktime()` and may have accuracy
           issues.

        :returns: The local time zone.
        :rtype: timezone
        """
        if self.tzinfo is None:
            ts = self._mktime()
        else:
            ts = (self - _EPOCH) // timedelta(seconds=1)

        ts = self.posix_timestamp(ts, *LOCAL_COORD, short=self.is_short,
                                  trim=True)
        ts_len = len(ts)

        if self.is_short:
            need = 6 - ts_len
            ts += (0,) * need if need > 0 else ()
            localtm = _td_utils._build_struct_time(ts, -1, tzinfo=LOCAL,
                                                   short_in=True)
        else:
            need = 8 - ts_len
            ts += (0,) * need if need > 0 else ()
            localtm = _td_utils._build_struct_time(ts, -1, tzinfo=LOCAL,
                                                   short_in=False)

        # Extract TZ data
        gmtoff = localtm.tm_gmtoff
        zone = localtm.tm_zone
        return timezone(timedelta(seconds=gmtoff), zone)

    def astimezone(self, tz: tzinfo=None):
        """
        Returns a datetime instance with the provided tzinfo instance attached.

        .. warning::

           This method is derived from `_mktime()` and may have accuracy
           issues.

        :param tzinfo tz: A timezone instance.
        :returns: A `datetime` instance with a tzinfo instance attached.
        :rtype: datetime
        """
        if tz is None:
            tz = self._local_timezone()
        elif not isinstance(tz, tzinfo):
            raise TypeError("tz argument must be an instance of tzinfo, "
                            f"found {type(tz)}.")

        mytz = self.tzinfo

        if mytz is None:
            mytz = self._local_timezone()
            myoffset = mytz.utcoffset(self)
        else:
            myoffset = mytz.utcoffset(self)

            if myoffset is None:
                mytz = self.replace(tzinfo=None)._local_timezone()
                myoffset = mytz.utcoffset(self)

        if tz is not mytz:
            # Convert self to UTC, and attach the new time zone instance.
            utc = (self - myoffset).replace(tzinfo=tz)
            # Convert from UTC to tz's local time.
            ret = tz.fromutc(utc)
        else:
            ret = self

        return ret

    # Ways to produce a string.

    def ctime(self) -> str:
        """
        Return a `ctime` formatted string.

        :returns: A string with weekday, month name, day, hour, minute, second,
                  and year.
        :rtype: str
        """
        if self.is_short:
            date = self.b_date + self.b_time
        else:
            d = self._short_from_long_form(time=self.b_time)
            date = (*d[:3], *d[5:])

        weekday = _td_utils._day_of_week(*date[:3])
        wd_name = _td_utils.DAYNAMES[weekday]
        year, month, day, hour, minute, second, us = date
        m_name = _td_utils.MONTHNAMES[month]
        y_shim = 4 if year > -1 else 5
        return (f"{wd_name} {m_name} {day:2d} "
                f"{hour:02d}:{minute:02d}:{second:02d} {year:0{y_shim}d}")

    def isoformat(self, sep: str='T', timespec: str='auto') -> str:
        """
        Return the time formatted according to ISO.

        The full format looks like 'YYYY-MM-DDTHH:MM:SS.mmmmmm'.
        By default, the fractional part is omitted if self.microsecond == 0.

        If `self.tzinfo` is not `None`, the UTC offset is also attached,
        giving a full format of 'YYYY-MM-DDTHH:MM:SS.mmmmmm+HH:MM'.

        Optional argument sep specifies the separator between date and
        time, default 'T'.

        The optional argument timespec specifies the number of additional
        terms of the time to include. Valid options are 'auto', 'hours',
        'minutes', 'seconds', 'milliseconds' and 'microseconds'.

        :param str sep: The ISO date and time seperator. The standard is to
                        use *T*.
        :param str timespec: A special string as stated above that will
                             append additional data to the string.
        :returns: An ISO formatted string.
        :rtype: str
        """
        s = (f"{self._year:04d}-{self._month:02d}-{self._day:02d}{sep}" +
             _format_time(self._hour, self._minute, self._second,
                          self._microsecond, timespec))

        off = self.utcoffset()
        tz = _format_offset(off)

        if tz:
            s += tz

        return s

    def __repr__(self) -> str:
        """
        Convert to formal string, for repr().

        :returns: A string representation of the `datetime` instance.
        :rtype: str
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

    def _dt_str_conversion(self, sep='T') -> str:
        """
        A representation of the `datetime` instance.

        :param str sep: The ISO date and time seperator. The standard is to
                        use *T*.
        :returns: A representation of the `datetime` instance.
        :rtype: str
        """
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
    def strptime(cls, date_string: str, format: str) -> str:
        """
        String, format -> new datetime parsed from a string
        (like time.strptime()).

        :param str date_string: A string representing the date.
        :param str format: A format string
        :returns: A date and time string representing the `format` string.
        :rtype: str
        """
        from badidatetime import _strptime
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

    def tzname(self) -> str:
        """
        Return the timezone name.

        Note that the name is 100% informational -- there's no requirement that
        it mean anything in particular. For example, 'GMT', 'UTC', '-500',
        '-5:00', 'EDT', 'US/Eastern', 'America/New_York' are all valid replies.

        :returns: The name of the time zone or `None` if not `tzinfo`
                  instance was found.
        :rtype: str
        """
        if self.tzinfo is not None:
            name = self.tzinfo.tzname(self)
            _check_tzname(name)
            return name

    def dst(self) -> int:
        """
        Return 0 if DST is not in effect, or the DST offset (as timedelta
        positive eastward) if DST is in effect.

        This is purely informational; the DST offset has already been added to
        the UTC offset returned by utcoffset() if applicable, so there's no
        need to consult dst() unless you're interested in displaying the DST
        info.

        :returns: The value as described above.
        :rtype: int
        """
        if self.tzinfo is not None:
            offset = self.tzinfo.dst(self)
            _check_offset("dst", offset)
            return offset

    # Comparisons of datetime objects with other.

    def __eq__(self, other) -> bool:
        if isinstance(other, datetime):
            return self._cmp(other, allow_mixed=True) == 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            return False

    def __le__(self, other) -> bool:
        if isinstance(other, datetime):
            return self._cmp(other) <= 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def __lt__(self, other) -> bool:
        if isinstance(other, datetime):
            return self._cmp(other) < 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def __ge__(self, other) -> bool:
        if isinstance(other, datetime):
            return self._cmp(other) >= 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def __gt__(self, other) -> bool:
        if isinstance(other, datetime):
            return self._cmp(other) > 0
        elif not isinstance(other, date):
            return NotImplemented
        else:
            _cmperror(self, other)

    def _cmp(self, other, allow_mixed: bool=False) -> int:
        """
        Returns an integer representation of >, ==, or < of two date instances.

        :param date other: The other date instance to compare to.
        :param bool allow_mixed: An integer representation of two date
                                 instances.
        :returns: If 0 then x == y, else if 1 then x > y else if -1 then x < y,
                  where x is the `self` instance and y is the `other` instance.
        :rtype: int
        """
        assert isinstance(other, datetime), (
            f"The other must be a datetime instance, found '{type(other)}'.")
        mytz = self.tzinfo
        ottz = other.tzinfo
        myoff = otoff = None

        if mytz is ottz:
            base_compare = True
        else:
            myoff = self.utcoffset()
            otoff = other.utcoffset()

            # Assume that allow_mixed means that we are called from __eq__
            if allow_mixed:  # pragma: no cover
                if myoff != self.replace(fold=not self.fold).utcoffset():
                    return 2  # arbitrary non-zero value

                if otoff != other.replace(fold=not other.fold).utcoffset():
                    return 2  # arbitrary non-zero value

            base_compare = myoff == otoff

        if base_compare:
            return _cmp((self._year, self._month, self._day, self._hour,
                         self._minute, self._second, self._microsecond),
                        (other._year, other._month, other._day, other._hour,
                         other._minute, other._second, other._microsecond))

        if myoff is None or otoff is None:
            if allow_mixed:
                return 2  # arbitrary non-zero value
            else:
                raise TypeError("Cannot compare naive and aware datetimes.")

        # XXX What follows could be done more efficiently...
        diff = self - other  # this will take offsets into account

        if diff.days < 0:
            return -1

        return diff and 1 or 0

    def __add__(self, other):
        """
        Add a datetime and a timedelta.

        :param datetime other: The other `datetime` instance which is added
                               to the `self` instance.
        :rtype: datetime
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

    def __sub__(self, other) -> object:
        """
        Subtract two datetimes, or a datetime and a timedelta.

        :param date other: The other datetime instance which is subtracted
                           from the `self` instance.
        :returns: Subtracted datetime instances.
        :rtype: date

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
            raise TypeError("Cannot mix naive and timezone-aware time.")

        return base + otoff - myoff

    def __hash__(self) -> int:
        """
        Get the hash of the `self` instance.

        :returns: The hash.
        :rtype: int
        """
        if self._hashcode == -1:
            if self.fold:
                t = self.replace(fold=0)
            else:
                t = self

            tzoff = t.utcoffset()

            if tzoff is None:
                self._hashcode = hash(t._getstate()[0])
            else:
                days = _td_utils._ymd2ord(self.year, self.month, self.day)
                seconds = self.hour * 3600 + self.minute * 60 + self.second
                self._hashcode = hash(timedelta(days, seconds,
                                                self.microsecond) - tzoff)
        return self._hashcode

    # Pickle support.

    @classmethod
    def _is_pickle_data(cls, a, b) -> int:
        """
        Check if the incoming date is pickle data or actual date information.

        :param a: Pickle data, the kull_i_shay, or year.
        :type a: int, str, or bytes
        :param b: None, vahid, or month
        :type b: NoneType or int
        :returns: A Boolean if a short or long Badi date derived from pickle
                  data. A None can be returned if a and b are real date
                  information.
        :rtype: bool or NoneType
        """
        if isinstance(b, (NoneType, tzinfo)) and isinstance(a, (bytes, str)):
            a_len = len(a)
            assert a_len in (10, 11), (
                f"Invalid string {a} had length of {a_len} for pickle.")
            short = True if a_len == 10 else False

            if ((short and 1 <= ord(a[2:3]) & 0x7F <= 19)
                or not short and 1 <= ord(a[3:4]) & 0x7F <= 19):
                if isinstance(a, str):
                    try:
                        a = a.encode('latin1')
                    except UnicodeEncodeError:
                        raise ValueError(
                            "Failed to encode latin1 string when unpickling "
                            "a date or datetime instance. "
                            "pickle.load(data, encoding='latin1') is assumed.")
            else:
                short = None
        else:
            short = None

        return short

    def _getstate(self, protocol: int=3)-> tuple:
        """
        Get the current state.

        :param int protocol: The pickle protocol to use defaults to 3.
        :returns: The state of the current `self` instance.
        :rtype: tuple
        """
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

    def __setstate(self, bytes_str, tzinfo) -> None:
        """
        Set the current state.

        :param bytes bytes_str: The bytes string.
        :param tzinfo tzinfo: A `tzinfo` instance.
        """
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

    def __reduce_ex__(self, protocol: int) -> tuple:
        """
        A tuple of the class name and current state.

        :param int protocol:  The protocol used to derive the state.
        :returns: The class name and current state.
        :rtype: tuple
        """
        return (self.__class__, self._getstate(protocol))

    def __reduce__(self) -> tuple:  # pragma: no cover
        """
        A tuple of the class name and current state using protocol 2.

        :returns: The class name and current state.
        :rtype: tuple
        """
        return self.__reduce_ex__(2)


datetime.min = datetime(-1842, 1, 1)
datetime.max = datetime(1161, 19, 19)
datetime.resolution = timedelta(microseconds=1)


class timezone(tzinfo):
    __slots__ = '_offset', '_name'

    # Sentinel value to disallow None
    _Omitted = object()

    def __new__(cls, offset: timedelta, name: str=_Omitted) -> object:
        """
        Construct the constructor.

        :param timedelta offset: A timedelta instance representing the
                                 difference between the local time and UTC.
        :param str name: A string that will be used as the value returned by
                         the datetime.tzname() method.
        :returns: An instance of timezone.
        :rtype: timezone
        """
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
    def _create(cls, offset: timedelta, name: str=None) -> object:
        """
        Creat an instance of `tzinfo`.

        :param timedelta offset: A `timedelta` instance representing the
                                 offset from UTC.
        :param str name: An optional name indicating the time zone as an
                         IANA name, however, it could just be UTC or UTC+0330.
        :returns: The `timezone` instance.
        :rtype: timezone
        """
        self = tzinfo.__new__(cls)
        self._offset = offset
        self._name = name
        return self

    def __eq__(self, other):
        if isinstance(other, timezone):
            return self._offset == other._offset

        return NotImplemented

    def __repr__(self) -> str:
        """
        Convert to formal string, for repr().

        :returns: A string representing the date.
        :rtype: str

        .. note::

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

    def __str__(self) -> str:
        """
        A string representation of the current `timezone` instane.

        :returns: A string representation of the current `timezone` instane.
        :rtype: str
        """
        return self.tzname(None)

    def utcoffset(self, dt: datetime) -> timedelta:
        """
        Return the fixed value specified when the `timezone` instance is
        constructed.

        :returns: The fixed value specified when the `timezone` instance is
                  constructed.
        :rtype: timedelta
        """
        if isinstance(dt, datetime) or dt is None:
            return self._offset

        raise TypeError("utcoffset() argument must be a datetime instance "
                        "or None")

    def badioffset(self, dt: datetime) -> timedelta:
        if isinstance(dt, datetime) or dt is None:
            return self._offset - timedelta(hours=BADI_COORD[2])

        raise TypeError("badioffset() argument must be a datetime instance "
                        "or None")

    def tzname(self, dt: datetime):
        if isinstance(dt, datetime) or dt is None:
            if self._name is None:
                return self._name_from_offset(self._offset)

            return self._name

        raise TypeError("tzname() argument must be a datetime instance "
                        "or None")

    def dst(self, dt: datetime):
        if isinstance(dt, datetime) or dt is None:
            return None

        raise TypeError("dst() argument must be a datetime instance or None")

    def fromutc(self, dt: datetime):
        if isinstance(dt, datetime):
            if dt.tzinfo is not self:
                raise ValueError("fromutc: dt.tzinfo is not self")

            return dt + self._offset

        raise TypeError("fromutc() argument must be a datetime instance "
                        "or None")

    _maxoffset = timedelta(hours=24, microseconds=-1)
    _minoffset = -_maxoffset

    @staticmethod
    def _name_from_offset(delta: timedelta) -> str:
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
        return (self._offset,) + ((self._name,) if self._name else ())

    def __hash__(self):
        return hash(self._offset)


UTC = timezone.utc = timezone._create(timedelta(0))
BADI = timezone.badi = timezone._create(timedelta(hours=BADI_COORD[2]))

# bpo-37642: These attributes are rounded to the nearest minute for backwards
# compatibility, even though the constructor will accept a wider range of
# values. This may change in the future.
timezone.min = timezone._create(-timedelta(hours=23, minutes=59))
timezone.max = timezone._create(timedelta(hours=23, minutes=59))
_EPOCH = datetime(126, 16, 2, None, None, 7, 58, tzinfo=timezone.utc)
