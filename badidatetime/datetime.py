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
_DAYNAMES = {1: 'Jalál', 2: 'Jamál', 3: 'Kamál', 4: 'Fiḍāl',
             5: '`Idāl', 6: 'Istijlāl', 7: 'Istiqlāl'}

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
    return (_days_before_year(bc, year) +
            _days_before_month(bc, year, month) + day)

def _ord2ymd(bc:BahaiCalendar, n:int, *, short:bool=False) -> tuple:
    """
    It is more difficult to do this in the Badi Calendar because a Badi
    day can be more or less than 24 hours depending on when sunset is
    and the time of the year. From the summer Solstice to the winter solstice
    the days get shorter so it's less than 24 hours and the inverse between
    the winter solstice and the summer solstice. As such we just use the
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
    We count the week from Jalal (Saturday) as the first day and
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
    :raises AssertionError: If the week or weekday is out of range.
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
    day_offset = (week - 1) * 7 + day
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

def _day_of_week(bc:BahaiCalendar, year:int, month:int, day:int) -> int:
    """
    Find the day of the week.

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
    wd = (_ymd2ord(bc, year, month, day) + 3) % 7
    return 7 if wd == 0 else wd

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


class timedalta:
    pass




class date(BahaiCalendar):
    """
    Implements the date object for the Badi datetime.
    """

    def __new__(cls, a:int, b:int, c:int, d:int=None, e:int=None) -> object:
        """
        Instantiate the class.

        :param cls: The class object.
        :type cls: date class
        :param a: Long form this value is the Kill-i-Shay and short form
                  it's the year.
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
        date = tuple([x for x in (a, b, c, d, e) if x is not None])
        date_len = len(date)
        assert date_len in (3, 5), (
            "A full short or long form Badi date must be used.")
        self = object.__new__(cls)
        super().__init__(self)
        self._MONTHNAMES = {num: name for num, name in self.BADI_MONTH_NAMES}
        _check_date_fields(self, a, b, c, d, e)

        if date_len == 5:
            self._kull_i_shay = a
            self._vahid = b
            self._year = c
            self._month = d
            self._day = e
            self.__date = date
            self.__short = False
        else:
            self._year = a
            self._month = b
            self._day = c
            self.__date = date
            self.__short = True

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
        t = _time.time()
        return cls.fromtimestamp(t, short=short)

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
        if self.__short:
            date = self.__date
        else:
            date = self.short_date_from_long_date(self.__date)

        return date

    def ctime(self):
        """
        Return ctime() style string in the short form Badi date.
        """
        date = self.__short_from_long_form()
        year, month, day = date[:3]
        weekday = _day_of_week(self, *date[:3])
        wd_name = _DAYNAMES[weekday]
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

    __str__ = isoformat

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
        l_form = (kull_i_shay, vahid, year, month, day)
        s_form = (year, month, day)

        if (self.__short and (kull_i_shay or vahid)
            and not all(l_form)):
            msg = ("If converting from a short to a long form date all long "
                   "form fields must be entered.")
            raise ValueError(msg)
        elif not self.__short and any(l_form):
            short = self.__short
        elif not self.__short and not all(s_form):
            msg = ("If converting from a long to a short form date all short "
                   "form fields must be entered.")
            raise ValueError(msg)
        elif self.__short:
            short = self.__short
        else:
            short = False if kull_i_shay and vahid else True

        if kull_i_shay is None and not self.__short:
            kull_i_shay = self._kull_i_shay

        if vahid is None and not self.__short:
            vahid = self._vahid

        if year is None:
            year = self._year

        if month is None:
            month = self._month

        if day is None:
            day = self._day

        if short:
            obj = type(self)(year, month, day)
        else:
            obj = type(self)(kull_i_shay, vahid, year, month, day)

        return obj

    # Comparisons of date objects with other.

##     def __eq__(self, other):
##         if isinstance(other, date):
##             return self._cmp(other) == 0
##         return NotImplemented

##     def __le__(self, other):
##         if isinstance(other, date):
##             return self._cmp(other) <= 0
##         return NotImplemented

##     def __lt__(self, other):
##         if isinstance(other, date):
##             return self._cmp(other) < 0
##         return NotImplemented

##     def __ge__(self, other):
##         if isinstance(other, date):
##             return self._cmp(other) >= 0
##         return NotImplemented

##     def __gt__(self, other):
##         if isinstance(other, date):
##             return self._cmp(other) > 0
##         return NotImplemented

##     def _cmp(self, other):
##         assert isinstance(other, date)
##         y, m, d = self._year, self._month, self._day
##         y2, m2, d2 = other._year, other._month, other._day
##         return _cmp((y, m, d), (y2, m2, d2))

##     def __hash__(self):
##         "Hash."
##         if self._hashcode == -1:
##             self._hashcode = hash(self._getstate())
##         return self._hashcode

##     # Computations

##     def __add__(self, other):
##         "Add a date to a timedelta."
##         if isinstance(other, timedelta):
##             o = self.toordinal() + other.days
##             if 0 < o <= _MAXORDINAL:
##                 return type(self).fromordinal(o)
##             raise OverflowError("result out of range")
##         return NotImplemented

##     __radd__ = __add__

##     def __sub__(self, other):
##         """Subtract two dates, or a date and a timedelta."""
##         if isinstance(other, timedelta):
##             return self + timedelta(-other.days)
##         if isinstance(other, date):
##             days1 = self.toordinal()
##             days2 = other.toordinal()
##             return timedelta(days1 - days2)
##         return NotImplemented

##     def weekday(self):
##         "Return day of the week, where Monday == 0 ... Sunday == 6."
##         return (self.toordinal() + 6) % 7

##     # Day-of-the-week and week-of-the-year, according to ISO

##     def isoweekday(self):
##         "Return day of the week, where Monday == 1 ... Sunday == 7."
##         # 1-Jan-0001 is a Monday
##         return self.toordinal() % 7 or 7

##     def isocalendar(self):
##         """Return a named tuple containing ISO year, week number, and weekday.

##         The first ISO week of the year is the (Mon-Sun) week
##         containing the year's first Thursday; everything else derives
##         from that.

##         The first week is 1; Monday is 1 ... Sunday is 7.

##         ISO calendar algorithm taken from
##         http://www.phys.uu.nl/~vgent/calendar/isocalendar.htm
##         (used with permission)
##         """
##         year = self._year
##         week1monday = _isoweek1monday(year)
##         today = _ymd2ord(self._year, self._month, self._day)
##         # Internally, week and day have origin 0
##         week, day = divmod(today - week1monday, 7)
##         if week < 0:
##             year -= 1
##             week1monday = _isoweek1monday(year)
##             week, day = divmod(today - week1monday, 7)
##         elif week >= 52:
##             if today >= _isoweek1monday(year+1):
##                 year += 1
##                 week = 0
##         return _IsoCalendarDate(year, week+1, day+1)

##     # Pickle support.

##     def _getstate(self):
##         yhi, ylo = divmod(self._year, 256)
##         return bytes([yhi, ylo, self._month, self._day]),

##     def __setstate(self, string):
##         yhi, ylo, self._month, self._day = string
##         self._year = yhi * 256 + ylo

##     def __reduce__(self):
##         return (self.__class__, self._getstate())

## _date_class = date  # so functions w/ args named "date" can get at the class

## date.min = date(1, 1, 1)
## date.max = date(9999, 12, 31)
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


class IsoCalendarDate:
    pass


class time:
    pass


class datetime(date):
    pass


class timezone:
    pass
