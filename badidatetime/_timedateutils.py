# -*- coding: utf-8 -*-
#
# badidatetime/_timedateutils.py
#
__docformat__ = "restructuredtext en"

import time
import locale
import math
from typing import NamedTuple

from ._structures import struct_time, ShortFormStruct, LongFormStruct
from .badi_calendar import BahaiCalendar


class TimeDateUtils(BahaiCalendar):
    # Badi additions are %:K for Kull-i-Shay and %:V for Váḥid
    VALID_FORMAT_CHRS = 'aAbBcCdDefGhHIjkKlmMnprSTuUVWxXyYzZ%'
    DAYNAMES = ('Jalál', 'Jamál', 'Kamál', 'Fiḍāl', '`Idāl',
                'Istijlāl', 'Istiqlāl')
    DAYNAMES_ABV = ('Jal', 'Jam', 'Kam', 'Fiḍ', 'Idā', 'Isj', 'Isq')
    MONTHNAMES = ('Ayyám-i-Há', 'Bahá', 'Jalál', 'Jamál', "'Aẓamat", 'Núr',
                  'Raḥmat', 'Kalimát', 'Kamál', "Asmá'", "'Izzat", 'Mashíyyat',
                  "'Ilm", 'Qudrat', 'Qawl', 'Masá’il', 'Sharaf', 'Sulṭán',
                  'Mulk', 'Ayyám-i-Há', "'Alá'")
    MONTHNAMES_ABV = ('Ayy', 'Bah', 'Jal', 'Jam', 'Aẓa', 'Núr', 'Raḥ', 'Kal',
                      'Kam', 'Asm', 'Izz', 'Mas', 'Ilm', 'Qud', 'Qaw', 'Mas',
                      'Sha', 'Sul', 'Mul', 'Ayy', 'Alá')
    # This keeps the Badi day count in par with the Gregorian day count.
    DAYS_BEFORE_1ST_YEAR = 78 # This keeps us insync with Gregorian dates.
    MINYEAR = -1842
    MAXYEAR = 1161

    def __init__(self):
        """
        We need to set the locale. However if an unsupported locale is
        required it may nee to be installed. This is the Linux method.

        1. Find the supported locales:
           $ locale -a
        2. Set a new locale:
           $ sudo locale-gen fr_FR.UTF-8 # Use the the locale you need.
           $ sudo update-locale
        """
        super().__init__(self)
        self._locale_data = {}
        self._date_and_time_locale()

    def _date_and_time_locale(self):
        self._locale_data['locale'] = locale.setlocale(locale.LC_TIME, '')
        self._locale_data['am'] = locale.nl_langinfo(locale.AM_STR)
        self._locale_data['pm'] = locale.nl_langinfo(locale.PM_STR)

        try:
            # Get the date format for the current locale
            date_format = locale.nl_langinfo(locale.D_FMT)
        except AttributeError: # pragma: no cover
            date_format = '%m/%d/%y'

        self._locale_data['d_format'] = self._order_format(
            date_format, '%m/%d/%y')
        self._locale_data['t_format'] = self._order_format(
            self._find_time_order(), '%H:%M:%S')

    def _order_format(self, fmt, default):
        if len(fmt) != 8:
            fmt = default

        data = [fmt[2]]

        for idx, char in enumerate(fmt):
            if idx in (1, 4, 7):
                data.append((char))

        return data

    def _find_time_order(self):
        t_str = time.strftime('%X')
        delim = t_str[2]
        h = 'I' if time.strftime('%p') != "" else 'H'
        return f"%{h}{delim}%M{delim}%S"

    @property
    def locale(self):
        return self._locale_data['locale']

    @property
    def am(self):
        return self._locale_data['am']

    @property
    def pm(self):
        return self._locale_data['pm']

    @property
    def date_format(self):
        return self._locale_data['d_format']

    @property
    def time_format(self):
        return self._locale_data['t_format']

    def _checktm(self, ttup:tuple) -> None:
        """
        Check that the fields in the tuple are of the correct type. This
        check on date information is different than what is found inn the
        badi_calendar.py module as it needs to conform with ISO standards.
        """
        if not issubclass(ttup.__class__, tuple):
            raise TypeError(
                f"The ttup argument {ttup.__class__} is not a proper tuple.")

        def process_long_form(ttup):
            assert (self.KULL_I_SHAY_MIN <= ttup[0] <= self.KULL_I_SHAY_MAX), (
                f"Invalid kull-i-shay {ttup[0]}, it must be in the range of "
                f"[{self.KULL_I_SHAY_MIN}, {self.KULL_I_SHAY_MAX}].")
            assert 1 <= ttup[1] < 20, (
                f"Invalid Váḥids '{ttup[1]}' in a Kull-i-Shay’, it must be in "
                "the range of [1, 19].")
            assert 1 <= ttup[2] < 20, (
                f"Invalid year '{ttup[2]}' in a Váḥid, it must be in the "
                "range of [1, 19].")
            return (ttup[0] - 1) * 361 + (ttup[1] - 1) * 19 + ttup[2]

        def process_short_form(ttup):
            assert self.MINYEAR <= ttup[0] <= self.MAXYEAR, (
                f"Invalid year '{ttup[0]}' it must be in the range of ["
                f"{self.MINYEAR}, {self.MAXYEAR}].")
            return ttup[0]

        t_len = len(ttup)
        named_tuple = hasattr(ttup, '_asdict') # _make also can work

        if named_tuple: # Both long and short NamedTuple
            if t_len == 13: # Long form
                year = process_long_form(ttup)
                idx = 3
            elif t_len == 11: # Short form
                year = process_short_form(ttup)
                idx = 1
            else: # pragma: no cover
                raise TypeError(f"Invalid timetuple, found length {t_len}, "
                                f"{dir(ttup)}.")
        else: # A Tuple or class derived from a standard tuple
            if t_len == 11: # Long form
                year = process_long_form(ttup)
                idx = 3
            elif t_len == 9: # Short form
                year = process_short_form(ttup)
                idx = 1
            else:
                raise TypeError(f"Invalid timetuple, found length {t_len}, "
                                f"{dir(ttup)}.")

        month = ttup[idx]
        day = ttup[idx+1]
        hour = ttup[idx+2]
        minute = ttup[idx+3]
        second = ttup[idx+4]
        wday = ttup[idx+5]
        yday = ttup[idx+6]
        isdst = ttup[idx+7]
        assert 0 <= month < 20, (
            f"Invalid month '{month}', it must be in the range of [0, 19].")
        cycle = (4 + self._is_leap_year(year)) if month == 0 else 19
        assert 1 <= day <= cycle, (
            f"Invalid day '{day}' for month '{month}', it must be in the "
            f"range of [1, {cycle}].")
        assert 0 <= hour <= 24, (
            f"Invalid hour '{hour}', it must be in the range of [0, 24].")
        assert 0 <= minute < 60, (
            f"Invalid minute '{minute}', it must be in the range of [0, 60].")
        assert 0 <= second <= 61, (
            f"Invalid second '{second}', it must be in the range of [0, 61].")
        assert 0 <= wday <= 6, (
            f"Invalid week day '{wday}', it must be inthe range of [0, 6].")
        assert 1 <= yday <= 366, (
            f"Invalid day '{yday}' in year, it must be in the range of "
            "[1, 366].")
        assert -1 <= isdst <= 1, (
            f"Invalid isdst '{isdst}', it must be in the range of [-1, 1].")

    def a(self, ttup, org, mod):
        """
        """
        return self.DAYNAMES_ABV[ttup.tm_wday]

    def A(self, ttup, org, mod):
        """
        """
        return self.DAYNAMES[ttup.tm_wday]

    def b(self, ttup, org, mod):
        """
        Return the abbreviated month name if %b or %h.
        """
        return self.MONTHNAMES_ABV[ttup.tm_mon]

    def B(self, ttup, org, mod):
        """
        """
        return self.MONTHNAMES[ttup.tm_mon]

    def c(self, ttup, org, mod):
        """
        """
        st = f"{self.DAYNAMES_ABV[ttup.tm_wday]} "
        st += f"{self.MONTHNAMES_ABV[ttup.tm_mon]} "
        st += f"{ttup.tm_mday: 2} "
        st += f"{ttup.tm_hour:02}:"
        st += f"{ttup.tm_min:02}:"
        st += f"{ttup.tm_sec:02} "

        if not ttup.short:
            st += f"{ttup.tm_kull_i_shay} "
            st += f"{ttup.tm_vahid:02} "
            st += f"{ttup.tm_year:02}"
        else:
            st += f"{ttup.tm_year:04}"

        return st

    def C(self, ttup, org, mod):
        """
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        return f"{n}{abs(math.floor(year / 100)):02}"

    def d(self, ttup, org, mod):
        """
        Return a zero padded month. If the format was %-d or %e then
        return an un-padded decimal number.
        """
        if mod == '-':
            st = f"{ttup.tm_mday}"
        else:
            pad = ' ' if org == 'e' else 0
            st = f"{ttup.tm_mday:{pad}2}"

        return st

    def D(self, ttup, org, mod):
        """
        Return a locale dependent Badi short date. Badi long dates are
        converted to short dates first.
        This method does not tak into account format extenders, in other
        words the - or : after the %. They should never show up in the locale.
        """
        year = self._get_year(ttup)
        century = int(year / 100) * 100
        year -= 0 if year < century else century
        sep = self.date_format[0]
        data = []

        for p in range(1, 4):
            fmt = self.date_format[p]

            if fmt[-1] in 'yY':
                data.append(f"{year:02}")
            else:
                org = fmt[0]
                mod = ''
                data.append(f"{getattr(self, org)(ttup, org, mod):02}")

        return sep.join(data)

    def f(self, ttup, org, mod):
        """
        """
        s, m = self._sec_microsec_from_seconds(ttup.tm_sec)
        return f"{round(m, 6):06}"

    def G(self, ttup, org, mod):
        """
        Return an ISO 8601 year with century as a zero-padded decimal number.
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        return f"{n}{abs(year):04}"

    def H(self, ttup, org, mod):
        """
        """
        if mod == '-': # %-H
            st = f"{ttup.tm_hour}"
        else: # %H
            st = f"{ttup.tm_hour:02}"

        return st

    def I(self, ttup, org, mod):
        """
        If we assume that sunset was at 1800 hrs UTC then the Badi noon
        would be about 0600 hrs UTC the next morning. This changes on a
        daily bases because sunset changes and there is seldon exactly 24
        hours between two sunsets.

        *** TODO *** Does a 12-hour clock make sense in a Badi time?

        1st we need to find sunset for the provided date and the day after.
        Subreact these two times and divide the results by 2 to determine
        noon. Then determine which side of noon the current Badi time is on.
        """
        midday_frac = self._find_midday(ttup)
        time_frac = self.decimal_day_from_hms(ttup.tm_hour, ttup.tm_min,
                                              ttup.tm_sec)

        if midday_frac <= time_frac:
            hour = ttup.tm_hour - 12
        else:
            hour = ttup.tm_hour

        if org == 'l' and mod == '-':
            st = f"{hour}"
        elif org == 'l':
            st = f"{hour: 2}"
        else: # %I
            st = f"{hour:02}"

        return st

    def j(self, ttup, org, mod):
        """
        """
        return f"{ttup.tm_yday}" if mod == '-' else f"{ttup.tm_yday:03}"

    def K(self, ttup, org, mod):
        """
        Return the Kull-i-Shay. If the mod is not a : them return an empty
        string.
        """
        st = ""

        if mod == ':':
            if ttup.short:
                k = ttup.tm_year / 361
                kull_i_shay = 0 if ttup.tm_year == 0 else math.ceil(k)
            else:
                kull_i_shay = ttup.tm_kull_i_shay

            n = '-' if kull_i_shay < 0 else ''
            st += f"{n}{kull_i_shay}"

        return st

    def m(self, ttup, org, mod):
        """
        """
        return f"{ttup.tm_mon}" if mod == '-' else f"{ttup.tm_mon:02}"

    def M(self, ttup, org, mod):
        """
        """
        return f"{ttup.tm_min}" if mod == '-' else f"{ttup.tm_min:02}"

    def n(self, ttup, org, mod):
        """
        """
        return "\n"

    def p(self, ttup, org, mod):
        """
        """
        midday_frac = self._find_midday(ttup)
        time_frac = self.decimal_day_from_hms(ttup.tm_hour, ttup.tm_min,
                                              ttup.tm_sec)

        if midday_frac <= time_frac:
            st = self.pm
        else:
            st = self.am

        return st

    def r(self, ttup, org, mod):
        """
        """
        sec = math.floor(ttup.tm_sec)

        if org == 'T':
            st = f"{ttup.tm_hour:02}:{ttup.tm_min:02}:{sec:02}"
        else:
            hour = self.I(ttup, '', '')
            p = self.p(ttup, '', '')
            st = hour + f":{ttup.tm_min:02}:{sec:02} " + p

        return st

    def S(self, ttup, org, mod):
        """
        """
        return f"{ttup.tm_sec}" if mod == '-' else f"{ttup.tm_sec:02}"

    def u(self, ttup, org, mod):
        """
        """
        return f"{ttup.tm_wday + 1}"

    def U(self, ttup, org, mod):
        """
        """
        year = self._get_year(ttup)
        year, week, day = self._year_week_day(year, ttup.tm_mon,
                                              ttup.tm_mday, week0=True)
        return f"{week:02}"

    def V(self, ttup, org, mod):
        """
        """
        if mod == ':':
            if ttup.short:
                date = self.long_date_from_short_date(
                    (ttup.tm_year, ttup.tm_mon, ttup.tm_mday))
                st = f"{date[1]:02}"
            else:
                st = f"{ttup.tm_vahid:02}"
        else:
            year = self._get_year(ttup)
            year, week, day = self._year_week_day(year, ttup.tm_mon,
                                                  ttup.tm_mday)
            st = f"{week:02}"

        return st

    def x(self, ttup, org, mod):
        """
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        delim = self.date_format[0]
        century = int(year / 100) * 100
        data = []

        for fmt in self.date_format[1:]:
            data.append(getattr(self, fmt)(ttup, '', ''))

        return f"{delim}".join(data)

    def X(self, ttup, org, mod):
        """
        """
        delim = self.time_format[0]
        data = []

        for fmt in self.time_format[1:]:
            data.append(getattr(self, fmt)(ttup, '', ''))

        return f"{delim}".join(data)

    def y(self, ttup, org, mod):
        """
        """
        year = self._get_year(ttup)
        century = int(year / 100) * 100
        year = year - century
        return f"{year}" if mod == '-' else f"{year:02}"

    def Y(self, ttup, org, mod):
        """
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        return f"{n}{abs(year):04}"

    def z(self, ttup, org, mod):
        """
        -14400.0       == -0400
        37080          == +1030
        22829.4        == +063415
        11056.44427776 == +030712.
        """
        st = ""

        if ttup.tm_gmtoff:
            n = '-' if ttup.tm_gmtoff < 0 else '+'
            h = abs(ttup.tm_gmtoff / 3600)
            hh = int(h)
            m = h - hh
            m0 = m * 100
            mm = math.floor(m0)
            s = m0 - mm
            ss = int(s * 100)
            ms = int((s * 100 - ss) * 1000000)
            delim = ':' if mod == ':' else ""
            st += f"{n}{hh:02}{delim}{mm:02}"
            st += f"{delim}{ss:02}" if ss > 0 else ""
            st += f".{ms:06}" if ms > 0 else ""

        return st

    def Z(self, ttup, org, mod):
        """
        """
        return f"{ttup.tm_zone}" if ttup.tm_zone else ""

    def percent(self, ttup, org, mod):
        """
        """
        return "%"

    __METHOD_LOOKUP = {'a': a, 'A': A, 'b': b, 'B': B, 'c': c, 'C': C, 'd': d,
                       'D': D, 'e': d, 'f': f, 'G': G, 'h': b, 'H': H, 'I': I,
                       'j': j, 'k': H, 'K': K, 'l': I, 'm': m, 'M': M, 'm': m,
                       'M': M, 'n': n, 'p': p, 'r': r, 'S': S, 'T': r, 'u': u,
                       'U': U, 'V': V, 'W': U, 'x': x, 'X': X, 'y': y, 'Y': Y,
                       'z': z, 'Z': Z, '%': percent
                       }

    def strftime(self, format:str, ttup:tuple) -> str:
        """
        """
        self._check_format(format)
        self._checktm(ttup)

        if not isinstance(ttup, (ShortFormStruct, LongFormStruct)):
            ttup = struct_time(ttup)

        idx, fmtlen = 0, len(format)
        strf = ""

        while idx < fmtlen:
            ch = format[idx]

            if ch == '%' and idx+1 < fmtlen:
                ch0 = format[idx+1]
                i = 2 if ch0 in '-:' else 1
                ch1 = format[idx+i]
                strf += self.__METHOD_LOOKUP[ch1](
                    self, ttup, ch1, ch0 if i == 2 else '')
            elif format[idx-1] not in '%-:':
                strf += ch

            idx += 1

        return strf

    def _check_format(self, format):
        """
        Check that the correct format was provided.
        """
        idx = 0
        fmtlen = len(format)

        while idx < fmtlen:
            ch = format[idx]

            if ch == '%' and format[idx-1] != '%':
                ch0 = format[idx+1]
                i = 2 if ch0 in '-:' else 1
                ch1 = format[idx+i]

                if ((ch1 not in self.VALID_FORMAT_CHRS) or
                    (ch0 == '-' and ch1 not in 'dHjlmMSy') or
                    (ch0 == ':' and ch1 not in 'KVz')):
                    raise ValueError(
                        f"Invalid format character '{format[idx:idx+i+1]}'")

            idx += 1

        if fmtlen == 0:
            raise ValueError("Found an empty format string.")

    def _find_midday(self, ttup):
        if ttup.short:
            date = (ttup.tm_year, ttup.tm_mon, ttup.tm_mday, ttup.tm_hour,
                    ttup.tm_min, ttup.tm_sec)
        else:
            date = (ttup.tm_kull_i_shay, ttup.tm_vahid, ttup.tm_year,
                    ttup.tm_mon, ttup.tm_mday, ttup.tm_hour, ttup.tm_min,
                    ttup.tm_sec)

        return self.midday(date)

    def _get_year(self, ttup):
        return (ttup.tm_year if ttup.short else
                ((ttup.tm_kull_i_shay - 1) * 361 + (ttup.tm_vahid - 1) * 19 +
                 ttup.tm_year))

    def _year_week_day(self, year:int, month:int, day:int,
                       week0:bool=False) -> tuple:
        week1jalal = self._isoweek1jalal(year)
        today = self._ymd2ord(year, month, day)
        # Internally, week and day have origin 0
        week, day = divmod(today - week1jalal, 7)

        if not week0:
            if week < 0:
                year -= 1
                week1jalal = self._isoweek1jalal(year)
                week, day = divmod(today - week1jalal, 7)
            elif week >= 52 and today >= self._isoweek1jalal(year+1):
                year += 1
                week = 0

        return year, week+1, day+1

    def _days_before_year(self, year:int) -> float:
        """
        Get the number of days before the 1st of Baha of the year.

        :param year: Badi year
        :type year: int
        :return: The number of days since (-1841, 19, 19) of the Badi calendar.
        :rtype: int
        """
        jd0 = self.jd_from_badi_date((self.MINYEAR-1, 19, 19))
        jd1 = self.jd_from_badi_date((year, 1, 1), _chk_on=False)
        return math.floor(jd1 - jd0) - 1

    def _days_in_month(self, year:int, month:int) -> int:
        """
        The number of days in that month in that year.

        :param year: Badi year
        :type year: int
        :param month: Badi month (0..19)
        :type month: int
        :return: The number of in the current month.
        :rtype: int
        """
        return 4 + self._is_leap_year(year) if month == 0 else 19

    def _days_before_month(self, year:int, month:int) -> int:
        """
        The number of days in the year preceding the first day of month.

        :param year: Badi year
        :type year: int
        :param month: Badi month (0..19)
        :type month: int
        :return: The number in the year preceding the first day of month.
        :rtype: int
        """
        #assert 0 <= month <= 19, (f"Invalid month '{month}', it must be "
        #                          "in the range of [0, 19].")
        month -= -18 if month < 2 else 1 if 1 < month < 19 else 19
        dbm = 0

        if 0 < month < 19:
            dbm += month * 19
        elif month == 0:
            dbm += 18 * 19 + 4 + self._is_leap_year(year)

        return dbm

    def _day_of_week(self, year:int, month:int, day:int) -> int:
        """
        Find the day of the week where 0 == Jalál (Saturday) and
        6 == Istiqlāl (Friday). For ISO compatability add 1 to the result.

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
        return ((self._ymd2ord(year, month, day) + 1) % 7 + 7) % 7

    def _ymd2ord(self, year:int, month:int, day:int) -> int:
        """
        Get the number of days since Badi year -1842 (Gregorian 0001-03-20)
        including the current day.

        year, month, day -> ordinal, considering -1842-01-01 as day 1.

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
        #assert 0 <= month <= 19, (f"Invalid month '{month}', it must be "
        #                          "in the range of [0, 19].")
        dim = self._days_in_month(year, month)
        assert 1 <= day <= dim, (
            f"Day '{day}' for month {month} must be in range of 1..{dim}")
        # We add 78 days to the total so that the ordinal number can be
        # compared to the ordinals in the standard datetime package.
        return (self.DAYS_BEFORE_1ST_YEAR + self._days_before_year(year) +
                self._days_before_month(year, month) + day)

    def _ord2ymd(self, n:int, *, short:bool=False) -> tuple:
        """
        It is more difficult to do this in the Badi Calendar because a Badi
        day can be more or less than 24 hours depending on when sunset is
        and the time of the year. From the summer Solstice to the winter
        Solstice the days get shorter. The day slowly comes down to 24 hours
        around the Fall Equinox and then below 24 hours. The inverse happens
        between the Winter Solstice and the Summer Solstice. We just use the
        BadiCalendar API.

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
        jd = (self.jd_from_badi_date((self.MINYEAR-1, 19, 19)) -
              self.DAYS_BEFORE_1ST_YEAR + n)
        return self.badi_date_from_jd(jd, short=short, rtd=True)

    def _build_struct_time(self, date:tuple, dstflag:int, *,
                           short_in=False) -> NamedTuple:
        if short_in:
            y, m, d, hh, mm, ss = date
        else:
            y, m, d, hh, mm, ss, ms = self.short_date_from_long_date(
                date, _chk_on=False)

        wday = self._day_of_week(y, m, d)
        dnum = self._days_before_month(y, m) + d
        return struct_time(date + (wday, dnum, dstflag))

    def _isoweek_to_badi(self, year:int, week:int, day:int, *,
                         short:bool=False) -> tuple:
        """
        The week counts from Jalal (Saturday) as the first day and Istiqlal
        (Friday) the last day of the week. This is different from the usual
        way ISO weeks are counted which is Monday to Sunday.

        :param year: Badi year.
        :type year: int
        :param month: Badi month (0..19)
        :type month: int
        :param day: Badi day in week.
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
                first_weekday = self._ymd2ord(year, 1, 1) % 7

                if (first_weekday == 4 or (first_weekday == 3 and
                                           self._is_leap_year(year))):
                    out_of_range = False

            assert not out_of_range, f"Invalid week: {week}"

        assert 0 < day < 8, f"Invalid weekday: {day} (range is 1..7)"
        # Now compute the offset from (Y, 1, 1) in days:
        day_offset = (week - 1) * 7 + (day - 1)
        # Calculate the ordinal day for Jalal, week 1
        day_1 = self._isoweek1jalal(year)
        ord_day = day_1 + day_offset
        return self._ord2ymd(ord_day, short=short)

    def _isoweek1jalal(self, year:int) -> int:
        """
        Calculate the day number of Jalal (Saturday) starting week 1. It
        would be the first week with 4 or more days in the year in question.

        :param year: Badi year
        :type year: int
        :return: The number of the first Jalal in the Badi year.
        :rtype: int
        """
        firstday = self._ymd2ord(year, 1, 1)
        firstweekday = (firstday - 6) % 7
        week1jalal = firstday - firstweekday

        if firstweekday > 3: # First week day >= Fidal
            week1jalal += 7

        return week1jalal

    def _parse_isoformat_date_time(self, dtstr:str) -> tuple:
        """
        Parse both the date and time represented in an ISO string into a
        date and time tuple.

        :param dtstr: A ISO compliant time string.
        :type dtstr: str
        :return: The year, month, and day parsed from a ISO string.
        :rtype: tuple
        :return: The date and time.
        :rtype: tuple
        """
        tc = dtstr.count('T')
        sc = dtstr.count(' ')
        idx = dtstr.index('T') if tc else dtstr.index(' ') if sc else len(dtstr)
        str_date = dtstr[:idx].strip('T ')
        str_time = dtstr[idx:]
        date = self._parse_isoformat_date(str_date) if str_date else ()
        time = self._parse_isoformat_time(str_time) if str_time else ()
        return date + time

    def _parse_isoformat_date(self, dtstr:str) -> tuple:
        """
        Parse a date ISO formatted string.

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
            assert _td_utils.MINYEAR <= year <= _td_utils.MAXYEAR, (
                f"Year is out of range: {year}, min {_td_utils.MINYEAR}, "
                f"max {_td_utils.MAXYEAR}.")
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
        elif wc and 7 <= d_len <=10: # YYYYWww, YYYY-Www, YYYYWwwD, YYYY-Www-D
            pos = 5 if dc == 0 else 6
            wday = int(dtstr[pos:pos+2])
            pos += 2 if dc == 0 else 3
            d = dtstr[pos:]
            assert (dc == 1 and d_len == 8) or dc in (0, 2), (
                f"Invalid ISO string {dtstr}.")
            day = int(d) if d.isdigit() else 1
            date = self._isoweek_to_badi(year, wday, day, short=True)[:3]
        elif d_len in (7, 8):                   # YYYYDDD or YYYY-DDD
            month_days = [(n, 19) for n, v in self.BADI_MONTH_NAMES]
            month_days[18] = (0, 4 + self._is_leap_year(year))
            days = int(dtstr[4:7] if dc == 0 else dtstr[5:8])

            for month, ds in month_days:
                if days <= ds: break
                days -= ds

            date = (year, month, days)
        else:
            date = ()

        return date

    def _parse_isoformat_time(self, dtstr:str) -> tuple:
        """
        Parse a time ISO formatted string.

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
        t_len = len(dtstr)
        tmp_dtstr = dtstr
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

        if t_len > 0 and 'T' != dtstr[0]:
            raise ValueError("Invalid time string, 1st character must be "
                             f"one of ( T), found {tmp_dtstr!r}")

        del tmp_dtstr
        cc = dtstr.count(':')
        assert cc < 3, f"Invalid number of colons (:), can be 0 - 2, found {cc}"
        pc = dtstr.count('.')
        assert pc <= 1, f"Invalid number of dots (.), can be 0 - 1, found {pc}"

        if t_len > 2:
            hour = int(dtstr[1:3])
            pos0 = 1 if cc else 0
            pos1 = 2 if cc == 2 else 0

            if t_len > 3:
                if dtstr[3] == '.': # Thh.hhh
                    ph = float(dtstr[3:]) * 60
                    minute = math.floor(ph)
                    second = (ph % 1) * 60
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                elif dtstr[5 + pos0:6 + pos0] == '.':
                    # Thhmm.mmm or Thh:mm.mmm
                    minute = int(dtstr[3 + pos0:5 + pos0])
                    pm = float(dtstr[5 + pos0:])
                    second = pm * 60
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                elif dtstr[7 + pos0:8 + pos0] == '.':
                    # Thhmmss.sss or Thh:mm:ss.sss
                    minute = int(dtstr[3 + pos0:5 + pos0])
                    second = float(dtstr[5 + pos0:])
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                elif t_len == 5 + pos0: # Thhmm or Thh:mm
                    minute = int(dtstr[3 + pos0:5 + pos0])
                    time = (hour, minute, 0)
                elif t_len >= 7 + pos1: # Thhmmss.sss or Thh:mm:ss.sss
                    minute = int(dtstr[3 + pos0:5 + pos0])
                    second = float(dtstr[5 + pos1:])
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                else:
                    raise ValueError(f"Invalid time string, found {dtstr!r}")
            else: # Thh
                time = (hour, 0, 0)
        else:
            time = ()

        return time

    def _check_date_fields(self, a:int, b:int, c:int, d:int=None,
                           e:int=None, *, short_in:bool=False) -> None:
        """
        Check the validity of the date.

        :param a: The long form Kull-i-Shay or short form year.
        :type a: int
        :param b: The long form Váḥid or short form month.
        :type b: int
        :param c: The long form year or short form day.
        :type c: int
        :param d: The long form month.
        :type d: int
        :param e: The long form day.
        :param short_in: If True then parse for a short date else if False
                         parse for a long date. This is for incoming dates
                         not outgoing dates as in most other uses of 'short'.
        :type short_in: bool
        :return: Nothing
        :rtype: None
        :raises AssertionError: If any of the date values are out of range.
        """
        if short_in:
            b_date = (a, b, c)
        else:
            b_date = (a, b, c, d, e)

        self._check_valid_badi_date(b_date, short_in=short_in)

    def _check_time_fields(self, hour, minute, second, microsecond, fold):
        self._check_valid_badi_time(hour, minute, second, microsecond)
        assert fold in (0, 1), (
            f"The fold argument '{fold}' must be either 0 or 1.")

    def _wrap_strftime(self, object, format, timetuple):
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
                            freplace = f'{getattr(object, "microsecond", 0):06}'

                        push(freplace)
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
                                    zreplace = f'{sign}{h:02}{m:02}'

                                    if u:
                                        zreplace += f'{s:02}.{u:06}'
                                    elif s:
                                        zreplace += f'{s02}'

                        push(zreplace)
                    elif ch == 'Z':
                        if Zreplace is None:
                            Zreplace = ""

                            if hasattr(object, "tzname"):
                                s = object.tzname()

                                # strftime is going to have at this: escape %
                                if s is not None:
                                    Zreplace = s.replace('%', '%%')

                        push(Zreplace)
                    else:
                        push('%')
                        push(ch)
                else:
                    push('%')
            else:
                push(ch)

        newformat = "".join(newformat)
        return self.strftime(newformat, timetuple)

_td_utils = TimeDateUtils()
