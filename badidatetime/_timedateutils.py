# -*- coding: utf-8 -*-
#
# badidatetime/_timedateutils.py
#
__docformat__ = "restructuredtext en"

import time
import locale
import math

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
    DAYS_BEFORE_1ST_YEAR = 78

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
        except AttributeError:
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
            else:
                raise TypeError(f"Ivalid timetuple, found length {t_len}, "
                                f"{dir(ttup)}.")
        else: # A Tuple or class derived from a standard tuple
            if t_len == 11: # Long form
                year = process_long_form(ttup)
                idx = 3
            elif t_len == 9: # Short form
                year = process_short_form(ttup)
                idx = 1
            else:
                raise TypeError(f"Ivalid timetuple, found length {t_len}, "
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
        """
        st = ""
        year = self._get_year(ttup)
        century = int(year / 100) * 100

        for idx, ch in enumerate(self.date_format):
            if idx == 0:
                sep = ch
            else:
                st += getattr(self, ch)(ttup, org, mod) + sep

        return st.strip('/')

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
        if org == 'k' and mod == '-':
            st = f"{ttup.tm_hour: 2}"
        elif mod == '-': # %-H
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

        idx = 0
        fmtlen = len(format)
        strf = ""

        while idx < fmtlen:
            ch = format[idx]

            if ch == '%':
                ch0 = format[idx+1]
                i = 2 if ch0 in '-:' else 1
                ch1 = format[idx+i]
                strf += self.__METHOD_LOOKUP[ch1](
                    self, ttup, ch1, ch0 if i == 2 else '')

            idx += 1

        return strf

    def _check_format(self, fmt):
        """
        Check that the correct format was provided.
        """
        idx = 0
        fmtlen = len(fmt)

        while idx < fmtlen:
            ch = fmt[idx]

            if ch == '%':
                ch0 = fmt[idx+1]
                i = 2 if ch0 in '-:' else 1
                ch1 = fmt[idx+i]

                if ((ch1 not in self.VALID_FORMAT_CHRS) or
                    (ch0 == '-' and ch1 not in 'dHjlmMSy') or
                    (ch0 == ':' and ch1 not in 'KVz')):
                    raise ValueError(
                        f"Invalid format character '{fmt[idx:idx+i+1]}'")

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

        :param bc: BahaiCalendar instance.
        :type bc: object
        :param year: Badi year
        :type year: int
        :return: The number of days since (-1841, 19, 19) of the Badi calendar.
        :rtype: int
        """
        jd0 = self.jd_from_badi_date((self.MINYEAR-1, 19, 19))
        jd1 = self.jd_from_badi_date((year, 1, 1))
        return math.floor(jd1 - jd0) - 1

    def _days_in_month(self, year:int, month:int) -> int:
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
        return 4 + self._is_leap_year(year) if month == 0 else 19

    def _days_before_month(self, year:int, month:int) -> int:
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
            dbm += 18 * 19 + 4 + self._is_leap_year(year)

        return dbm

    def _ymd2ord(self, year:int, month:int, day:int) -> int:
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
        dim = self._days_in_month(year, month)
        assert 1 <= day <= dim, (
            f"Day '{day}' for month {month} must be in range of 1..{dim}")
        # We add 78 days to the total so that the ordinal number can be
        # compared to the ordinals in the standard datetime package.
        return (self.DAYS_BEFORE_1ST_YEAR + self._days_before_year(year) +
                self._days_before_month(year, month) + day)

    def _isoweek1jalal(self, year:int) -> int:
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
        firstday = self._ymd2ord(year, 1, 1)
        firstweekday = (firstday - 6) % 7
        week1jalal = firstday - firstweekday

        if firstweekday > 3: # First week day >= Fidal
            week1jalal += 7

        return week1jalal

_td_utils = TimeDateUtils()
