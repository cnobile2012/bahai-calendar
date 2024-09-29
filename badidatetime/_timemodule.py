# -*- coding: utf-8 -*-
#
# badidatetime/_timemodule.py
#
__docformat__ = "restructuredtext en"

import time
import locale
import math

from ._structures import struct_time, ShortFormStruct, LongFormStruct
from .badi_calendar import BahaiCalendar


class TimeModule(BahaiCalendar):
    # Badi additions are %:K for Kull-i-Shay and %:V for Váḥid
    VALID_FORMAT_CHRS = 'aAbBcCdDefGhHIjkKlmMnprSTuUVWxXyYzZ%'
    DAYNAMES = ('Jalál', 'Jamál', 'Kamál', 'Fiḍāl', '`Idāl',
                'Istijlāl', 'Istiqlāl')
    DAYNAMES_ABV = ('Jal', 'Jam', 'Kam', 'Fiḍ', 'Idā', 'Isj', 'Isq')
    MONTHNAMES = ('Bahá', 'Jalál', 'Jamál', "'Aẓamat", 'Núr', 'Raḥmat',
                  'Kalimát', 'Kamál', "Asmá'", "'Izzat", 'Mashíyyat',
                  "'Ilm", 'Qudrat', 'Qawl', 'Masá’il', 'Sharaf', 'Sulṭán',
                  'Mulk', 'Ayyám-i-Há', "'Alá'")
    MONTHNAMES_ABV = ('Bah', 'Jal', 'Jam', 'Aẓa', 'Núr', 'Raḥ', 'Kal', 'Kam',
                      'Asm', 'Izz', 'Mas', 'Ilm', 'Qud', 'Qaw', 'Mas', 'Sha',
                      'Sul', 'Mul', 'Ayy', 'Alá')
    MINYEAR = -1842
    MAXYEAR = 1161
    DAYS_BEFORE_1ST_YEAR = 78
    ERR_MSG0 = "Illegal format character '{}'"
    ERR_MSG2 = "strftime(): Illegal time tuple argument"

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
        def order_format(fmt):
            if len(fmt) == 8:
                data = [fmt[2]]

                for idx, char enumerate(fmt):
                    if idx in (1, 4, 7):
                        data.append((char))
            else:
                ret = ((0, '/'), (1, 'm'), (2, 'd'), (3, 'y'))

            return ret

        self._locale_data['locale'] = locale.setlocale(locale.LC_TIME, '')
        self._locale_data['am'] = locale.nl_langinfo(locale.AM_STR)
        self._locale_data['pm'] = locale.nl_langinfo(locale.PM_STR)

        try:
            # Get the date format for the current locale
            date_format = locale.nl_langinfo(locale.D_FMT)
        except AttributeError:
            date_format = '%m/%d/%y'

        self._locale_data['d_format'] = order_format(date_format)
        time_format = time.strftime('%X')
        self._locale_data['t_format'] = order_format(time_format)

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

    @property
    def is_date_defined(self):
        return self._locale_data['d_defined']

    def strftime(self, format, timetuple):
        """
        """
        self._check_format(format)
        self._checkType(timetuple, self.ERR_MSG2)
        self._checktm(timetuple, self.ERR_MSG2)

        if not isinstance(timetuple, (ShortFormStruct, LongFormStruct)):
            ttup = struct_time(timetuple)

        return self._parse_format(ttup, format)

    def _check_format(self, fmt):
        """
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
                    raise ValueError(self.ERR_MSG0.format(fmt[idx:idx+i+1]))

            idx += 1

    def _checkType(self, ttup, err_msg):
        """
        """
        if not isinstance(ttup, (tuple, ShortFormStruct, LongFormStruct)):
            raise TypeError(err_msg)

    def _checktm(self, ttup:tuple, err_msg:str) -> None:
        """
        """
        if not issubclass(ttup.__class__, tuple):
            raise TypeError(err_msg)

        if len(ttup) == 13:   # LongFormStruct
            pass
        elif len(ttup) == 11: # ShortFormStruct
            pass
        elif len(ttup) == 6:  # Short tuple value
            pass
        elif len(ttup) == 8:  # Long tuple value
            pass
        else:
            raise TypeError(err_msg)

        # *** TODO *** Update later when formatting is better understood.

    # %[aAbBcCdDefGhHIjkKlmMnprSTuUVWxXyYzZ%]
    # %-[dHjlmMSy]
    # %:[KVz]

    def a(self, ttup, org, mod):
        """
        """
        return self.DAYNAME_ABV[ttup.tm_wday]

    def A(self, ttup, org, mod):
        """
        """
        return self.DATNAME[ttup.tm_wday]

    def b(self, ttup, org, mod):
        """
        Return the abbreviated month name if %b or %h.
        """
        return self.MONTHNAMES_ABV[ttup.tm_mon]

    def B(self, ttup, org, mod):
        """
        """
        return self.MONTHNAMES[ttup.tm_mon]

    def c(self, ttup, mod):
        """
        """
        st = f"{self.DAYNAME_ABV[ttup.tm_wday]} "
        st += f"{self.MONTHNAMES_ABV[ttup.tm_mon]} "
        st += f"{ttup.tm_mday:02} "
        st += f"{ttup.tm_hour:02}:"
        st += f"{ttup.min:02}:"
        st += f"{ttup.sec:02} "

        if not ttup.short:
            st += f"{ttup.tm_kull_i_shay} "
            st += f"{ttup.tm_vahid:02} "

        st += f"{ttup.tm_year}"
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
        n = '-' if ttup.tm_year < 0 else ''
        st_d = {'y': f"{n}{abs(year):04}",
                'm': f"{ttup.tm_mon:02}",
                'd': f"{ttup.tm_mday:02}"}

        for ch in locale.nl_langinfo(locale.D_FMT):
            if ch == '%': continue
            st += st_d[ch] + ' '

        return st.strip()

    def f(self, ttup, org, mod):
        """
        """
        s = math.floor(ttup.tm_sec)
        m = math.floor(ttup.tm_sec % 1 * 1000000)
        return f"{round(m, 6):06}"

    def G(self, ttup, org, mod):
        """
        Return an ISO 8601 year with century as a zero-padded decimal number.
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        return f"{n}{year:04}"

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
        If we assume that sunset was at 1800 hrs UTC then noon would be about
        0600 hrs UTC the next morning. This changes on a daily bases because
        sunset changes and there is seldon 24 hours between two sunsets.

        *** TODO *** Does a 12-hour clock make sense in a Badi time?

        1st we need to find sunset for the provided date and the day after.
        Subreact these two times and divide the results by 2 to determine
        noon. Then determine which side of noon the current Badi time is on.
        """
        midday_frac = self._find_midday(ttup)
        time_frac = self.decimal_day_from_hms(ttup.hour, ttup.min, ttup.sec)

        if midday_frac <= time_frac:
            hour = ttup.hour - midday_frac
        else:
            hour = ttup.hour

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
        """
        st = ""

        if not ttup.short and mod == ':':
            n = '-' if ttup.tm_kull_i_shay < 0 else ''
            st += f"{n}{ttup.tm_kull_i_shay}"

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
        time_frac = self.decimal_day_from_hms(ttup.hour, ttup.min, ttup.sec)

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
        delim = self.date_format[0]
        data = []

        for value in self.date_format[1:]:
            if value == 'y':
                data.append(f"{year}")
            elif value == 'Y':
                century = int(year / 100) * 100
                data.append(f"{year - century}")
            elif value == 'm':
                data.append(f"{ttup.tm_mon:02}")
            else: # %d
                data.append(f"ttup.tm_mday:02")

        return "".join([v + f"{delim}" if i < (len(data) - 1)
                        for i, v in enumerate(data)])

    def X(self, ttup, org, mod):
        """
        """
        delim = self.time_format[0]
        data = []

        for value in self.time_format[1:]:
            if value == 's':
                data.append(f"{ttup.tm_sec:02}")
            elif value == 'm':
                data.append(f"{ttup.tm_min:02}")
            else: # %h
                data.append(f"{ttup.tm_hour:02}")

        return "".join([v + f"{delim}" if i < (len(data) - 1)
                        for i, v in enumerate(data)])

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




    __METHOD_LOOKUP = {'a': a, 'A': A, 'b': b, 'B': B, 'c': c, 'C': C, 'd': d,
                       'D': D, 'e': d, 'f': f, 'G': G, 'h': b, 'H': H, 'I': I,
                       'j': j, 'k': H, 'l': I, 'm': m, 'M': M, 'm': m, 'M': M,
                       'n': n, 'p': p, 'r': r, 'S': S, 'T': r, 'u': u, 'U': U,
                       'W': U, 'x': x, 'X': X, 'y': y, 'Y': Y, 
                       }

    def _parse_format(self, ttup:struct_time, format:str) -> str:
        """
        """
        idx = 0
        fmtlen = len(fmt)
        strf = ""

        while idx < fmtlen:
            ch = fmt[idx]

            if ch == '%':
                ch0 = fmt[idx+1]
                i = 2 if ch0 in '-:' else 1
                ch1 = fmt[idx+i]
                strf += __METHOD_LOOKUP[ch1](ttup, ch1, ch0 if i == 2 else '')

        return strf

    def _find_midday(self, ttup):
        if ttup.short:
            date = (ttup.tm_year, ttup.tm_mon, ttup.tm_mday)
        else:
            date = (ttup.tm_kull_i_shay, ttup.tm_vahid, ttup.tm_year,
                    ttup.tm_mon, ttup.tm_mday)

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
            f"Day for month {month} must be in range of 1..{dim}")
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

_time_module = TimeModule()
