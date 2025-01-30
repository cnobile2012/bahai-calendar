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
    """
    This class provides utility functionality to the datetime package. Its
    use is through a pre-instantiated object _td_utils.
    """
    # Badi additions are %:K for Kull-i-Shay and %:V for Váḥid
    VALID_FORMAT_CHRS = 'aAbBcCdDefGhHIjkKlmMnprSTuUVWxXyYzZ%'
    DAYNAMES = ('Jalál', 'Jamál', 'Kamál', 'Fiḍāl', '`Idāl',
                'Istijlāl', 'Istiqlāl')
    DAYNAMES_ABV = ('Jal', 'Jam', 'Kam', 'Fiḍ', 'Idā', 'Isj', 'Isq')
    MONTHNAMES = {1: 'Bahá', 2: 'Jalál', 3: 'Jamál', 4: "'Aẓamat", 5: 'Núr',
                  6: 'Raḥmat', 7: 'Kalimát', 8: 'Kamál', 9: "Asmá'",
                  10: "'Izzat", 11: 'Mashíyyat', 12: "'Ilm", 13: 'Qudrat',
                  14: 'Qawl', 15: 'Masá’il', 16: 'Sharaf', 17: 'Sulṭán',
                  18: 'Mulk', 0: 'Ayyám-i-Há', 19: "'Alá'"}
    MONTHNAMES_ABV = {1: 'Bah', 2: 'Jal', 3: 'Jam', 4: 'Aẓa', 5: 'Núr',
                      6: 'Raḥ', 7: 'Kal', 8: 'Kam', 9: 'Asm', 10: 'Izz',
                      11: 'Mas', 12: 'Ilm', 13: 'Qud', 14: 'Qaw', 15: 'Mas',
                      16: 'Sha', 17: 'Sul', 18: 'Mul', 0: 'Ayy', 19: 'Alá'}
    FIRST_YEAR_EPOCH = 1721501.261143
    """
    The first day that we start our count Julian year 1, March 19th.
    """
    DAYS_BEFORE_1ST_YEAR = 77
    """
    This keeps the Badi day count in par with the Gregorian day count.
    """

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
        """
        Set the `_locale_data` dictionary instance object to locale date
        and time information.
        """
        self._locale_data['locale'] = locale.setlocale(locale.LC_TIME, '')
        self._locale_data['am'] = locale.nl_langinfo(locale.AM_STR)
        self._locale_data['pm'] = locale.nl_langinfo(locale.PM_STR)

        try:
            # Get the date format for the current locale
            date_format = locale.nl_langinfo(locale.D_FMT)
        except AttributeError:  # pragma: no cover
            date_format = '%m/%d/%y'

        self._locale_data['d_format'] = self._order_format(
            date_format, '%m/%d/%y')
        self._locale_data['t_format'] = self._order_format(
            self._find_time_order(), '%H:%M:%S')

    def _order_format(self, fmt: str, default: str) -> None:
        """
        Pulls out of the incoming strings data needed to processing date
        and time formats.

        :param str fmt: The string format to parse.
        :param str default: A default format if `fmt` is invalid.
        :return: Data for processing date and time formats.
        :rtype: str
        """
        if len(fmt) != 8:
            fmt = default

        data = [fmt[2]]

        for idx, char in enumerate(fmt):
            if idx in (1, 4, 7):
                data.append((char))

        return data

    def _find_time_order(self):
        """
        Find the locale time delimiter and id 24 or 12 hour time is used.

        :return: The time delimiter and 24 or 12 hour time.
        :rtype: str
        """
        delim = time.strftime('%X')[2]
        h = 'I' if time.strftime('%p') != "" else 'H'
        return f"%{h}{delim}%M{delim}%S"

    @property
    def locale(self):
        """
        Returns the current locale.
        """
        return self._locale_data['locale']

    @property
    def am(self):
        """
        Returns the AM designator.
        """
        return self._locale_data['am']

    @property
    def pm(self):
        """
        Returns the PM designator.
        """
        return self._locale_data['pm']

    @property
    def date_format(self):
        """
        Returns the locale's date format.
        """
        return self._locale_data['d_format']

    @property
    def time_format(self):
        """
        Returns the locale's time format.
        """
        return self._locale_data['t_format']

    def _checktm(self, ttup: tuple) -> None:
        """
        Check that the fields in the tuple are of the correct type. This
        check on date information is different than what is found inn the
        badi_calendar.py module as it needs to conform with ISO standards.

        :param ttup: A time tuple.
        :type ttup: ShortFormStruct or LongFormStruct
        """
        if not issubclass(ttup.__class__, tuple):
            raise TypeError(
                f"The ttup argument {ttup.__class__} is not a proper tuple.")

        def process_long_form(ttup: tuple):
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

        def process_short_form(ttup: tuple):
            assert self.MINYEAR <= ttup[0] <= self.MAXYEAR, (
                f"Invalid year '{ttup[0]}' it must be in the range of ["
                f"{self.MINYEAR}, {self.MAXYEAR}].")
            return ttup[0]

        t_len = len(ttup)
        named_tuple = hasattr(ttup, '_asdict')  # _make also can work

        if named_tuple:  # Both long and short NamedTuple
            if t_len == 13:  # Long form
                year = process_long_form(ttup)
                idx = 3
            elif t_len == 11:  # Short form
                year = process_short_form(ttup)
                idx = 1
            else:  # pragma: no cover
                raise TypeError(f"Invalid timetuple, found length {t_len}.")
        else:  # A Tuple or class derived from a standard tuple
            if t_len == 11:  # Long form
                year = process_long_form(ttup)
                idx = 3
            elif t_len == 9:  # Short form
                year = process_short_form(ttup)
                idx = 1
            else:
                raise TypeError(f"Invalid timetuple, found length {t_len}.")

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
            f"Invalid week day '{wday}', it must be in the range of [0, 6].")
        assert 1 <= yday <= 366, (
            f"Invalid day '{yday}' in year, it must be in the range of "
            "[1, 366].")
        assert -1 <= isdst <= 1, (
            f"Invalid isdst '{isdst}', it must be in the range of [-1, 1].")

    def a(self, ttup, org, mod):
        """
        Abbreviated weekday.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The abbreviated weekday.
        :rtype: str
        """
        return self.DAYNAMES_ABV[ttup.tm_wday]

    def A(self, ttup, org, mod):
        """
        Full weekday name.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The full weekday.
        :rtype: str
        """
        return self.DAYNAMES[ttup.tm_wday]

    def b(self, ttup, org, mod):
        """
        Abbreviated month name.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The abbreviated month name.
        :rtype: str
        """
        return self.MONTHNAMES_ABV[ttup.tm_mon]

    def B(self, ttup, org, mod):
        """
        Full month name.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The full month name.
        :rtype: str
        """
        return self.MONTHNAMES[ttup.tm_mon]

    def c(self, ttup, org, mod):
        """
        Locale specific date and time. Equivilent to "%a %b %e %H:%M:%S %Y".

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The locale specific date and time.
        :rtype: str
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
        Century as a space-padded decimal number. (The year divided by 100
        then truncated to an integer.)

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The century as a space-padded decimal number.
        :rtype: str
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        return f"{n}{abs(math.floor(year / 100)):02}"

    def d(self, ttup, org, mod):
        """
        Day of month as a zero-padded decimal number.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The day of month as a zero-padded decimal number.
        :rtype: str
        """
        if mod == '-':
            st = f"{ttup.tm_mday}"
        else:
            pad = ' ' if org == 'e' else 0
            st = f"{ttup.tm_mday:{pad}2}"

        return st

    def D(self, ttup, org, mod):
        """
        Date where year is without century. Equivilent to a localized %m/%d/%y.

        .. note::

           Return a locale dependent Badí' short date. Badí' long dates are
           converted to short dates first.
           This method does not take into account format extenders, in other
           words the - or : after the %. They should never show up in the
           locale.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The date where year is without century.
        :rtype: str
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
        Microseconds as a decimal number.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The Microseconds as a decimal number.
        :rtype: str
        """
        s, m = self._sec_microsec_from_seconds(ttup.tm_sec)
        return f"{round(m, 6):06}"

    def G(self, ttup, org, mod):
        """
        ISO 8601 year with century as a zero-padded decimal number.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The ISO 8601 year with century as a zero-padded decimal
                 number.
        :rtype: str
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        return f"{n}{abs(year):04}"

    def H(self, ttup, org, mod):
        """
        Hour (24-hour clock) as a decimal number. Either zero-padded if %H
        or not zero-padded if %-H.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The hour (24-hour clock) as a decimal number.
        :rtype: str
        """
        if mod == '-':  # %-H
            st = f"{ttup.tm_hour}"
        else:  # %H
            st = f"{ttup.tm_hour:02}"

        return st

    def I(self, ttup, org, mod):
        """
        Hour (12-hour clock) as a zero-padded decimal number.

        .. note::

           1. If we assume that sunset was at 1800 hrs UTC then the Badí' noon
              would be about 0600 hrs UTC the next morning. This changes on a
              daily bases because sunset changes and there is seldom exactly 24
              hours between two sunsets.
           2. 1st we need to find sunset for the provided date and the day
              after. Subtract these two times and divide the results by 2 to
              determine noon. Then determine which side of noon the current
              Badí' time is on.

        *** TODO *** Does a 12-hour clock make sense in a Badí' time?

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The hour (12-hour clock) as a zero-padded decimal number.
        :rtype: str
         """
        midday_frac = self._find_midday(ttup)
        time_frac = self._decimal_day_from_hms(ttup.tm_hour, ttup.tm_min,
                                               ttup.tm_sec)

        if midday_frac <= time_frac:
            hour = ttup.tm_hour - 12
        else:
            hour = ttup.tm_hour

        if org == 'l' and mod == '-':
            st = f"{hour}"
        elif org == 'l':
            st = f"{hour: 2}"
        else:  # %I
            st = f"{hour:02}"

        return st

    def j(self, ttup, org, mod):
        """
        Day of the year as a decimal number. Either zero-padded if %j or
        not zero-padded if %-j.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The day of the year as a decimal number.
        :rtype: str
        """
        return f"{ttup.tm_yday}" if mod == '-' else f"{ttup.tm_yday:03}"

    def K(self, ttup, org, mod):
        """
        Kull-i-Shay as a negative or positive decimal number.

        .. note::

           If the mod is not a : (colon) them return an empty string.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The Kull-i-Shay as a decimal number.
        :rtype: str
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
        Month as a decimal number. Either zero-padded if %m or not
        zero-padded if %-m.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The month as a decimal number.
        :rtype: str
        """
        return f"{ttup.tm_mon}" if mod == '-' else f"{ttup.tm_mon:02}"

    def M(self, ttup, org, mod):
        """
        Minute as a zero-padded decimal number. Either zero-padded if %M or not
        zero-padded if %-M.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The minute as a zero-padded decimal number.
        :rtype: str
        """
        return f"{ttup.tm_min}" if mod == '-' else f"{ttup.tm_min:02}"

    def n(self, ttup, org, mod):
        """
        Newline character.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The newline character.
        :rtype: str
        """
        return "\n"

    def p(self, ttup, org, mod):
        """
        Locale defined AM and PM.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The locale defined AM and PM.
        :rtype: str
        """
        midday_frac = self._find_midday(ttup)
        time_frac = self._decimal_day_from_hms(ttup.tm_hour, ttup.tm_min,
                                               ttup.tm_sec)

        if midday_frac <= time_frac:
            st = self.pm
        else:
            st = self.am

        return st

    def r(self, ttup, org, mod):
        """
        Locale defined 12-hour clock time (am/pm).

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The locale defined 12-hour clock time.
        :rtype: str
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
        Second as a decimal number. Either zero-padded if %S or not
        zero-padded if %-S.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The second as a decimal number.
        :rtype: str
        """
        return f"{ttup.tm_sec}" if mod == '-' else f"{ttup.tm_sec:02}"

    def u(self, ttup, org, mod):
        """
        Weekday as a decimal number. With Jalál as 1.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The weekday as a decimal number.
        :rtype: str
        """
        return f"{ttup.tm_wday + 1}"

    def U(self, ttup, org, mod):
        r"""
        Week number of the year (Jalál as the first day of the week) as a
        zero-padded decimal number. All days in a new year preceding the
        first \`Idāl are considered to be in week 0.

        .. note::

           It make no sense to start a week on different day in the Badí'
           Calendar. So the %W format is the same as %U.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The week number of the year.
        :rtype: str
        """
        year = self._get_year(ttup)
        year, week, day = self._year_week_day(year, ttup.tm_mon,
                                              ttup.tm_mday, week0=True)
        return f"{week:02}"

    def V(self, ttup, org, mod):
        """
        ISO 8601 week as a decimal number with Jalál as the first day of the
        week. Week 01 is the week containing the 4th of Bahá. Either
        zero-padded if %V or not zero-padded if %-V.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The ISO 8601 week as a decimal number.
        :rtype: str
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
        Locale defined date representation.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The locale defined date representation.
        :rtype: str
        """
        delim = self.date_format[0]
        data = []

        for fmt in self.date_format[1:]:
            data.append(getattr(self, fmt)(ttup, '', ''))

        return f"{delim}".join(data)

    def X(self, ttup, org, mod):
        """
        Locale defined time representation.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The locale defined time representation.
        :rtype: str
         """
        delim = self.time_format[0]
        data = []

        for fmt in self.time_format[1:]:
            data.append(getattr(self, fmt)(ttup, '', ''))

        return f"{delim}".join(data)

    def y(self, ttup, org, mod):
        """
        Year without century as a decimal number. Either zero-padded if %y
        or not zero-padded if %-y.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The year without century as a decimal number.
        :rtype: str
        """
        year = self._get_year(ttup)
        century = int(year / 100) * 100
        year = year - century
        return f"{year}" if mod == '-' else f"{year:02}"

    def Y(self, ttup, org, mod):
        """
        Year with century as a zero-padded decimal number.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The year with century as a decimal number.
        :rtype: str
        """
        year = self._get_year(ttup)
        n = '-' if year < 0 else ''
        return f"{n}{abs(year):04}"

    def z(self, ttup, org, mod):
        """
        UTC offset in the form ±HHMM[SS[.ffffff]] (empty string if the
        object is naive).

        .. note::

           Some equivalents from offset to ISO standard.
           -14400.0       == -0400
           37080          == +1030
           22829.4        == +063415
           11056.44427776 == +030712.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The UTC offset in the form ±HHMM[SS[.ffffff]].
        :rtype: str
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
        Time zone name (empty string if the object is naive).

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The time zone name.
        :rtype: str
        """
        return f"{ttup.tm_zone}" if ttup.tm_zone else ""

    def percent(self, ttup, org, mod):
        """
        The literal '%' character.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :param str, org: The original text code.
        :param str mod: Code modifier.
        :return: The literal '%' character.
        :rtype: str
        """
        return "%"

    __METHOD_LOOKUP = {'a': a, 'A': A, 'b': b, 'B': B, 'c': c, 'C': C, 'd': d,
                       'D': D, 'e': d, 'f': f, 'G': G, 'h': b, 'H': H, 'I': I,
                       'j': j, 'k': H, 'K': K, 'l': I, 'm': m, 'M': M, 'm': m,
                       'M': M, 'n': n, 'p': p, 'r': r, 'S': S, 'T': r, 'u': u,
                       'U': U, 'V': V, 'W': U, 'x': x, 'X': X, 'y': y, 'Y': Y,
                       'z': z, 'Z': Z, '%': percent
                       }

    def strftime(self, format: str, ttup: tuple) -> str:
        """
        Convert a struck_time object into a string according to a specified
        format.

        :param str format: A string format.
        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :return: A struck_time object converted to a formated string.
        :rtype: str
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

    def _check_format(self, format: str) -> None:
        """
        Check that the correct format was provided.

        :param str format: A string format.
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

    def _find_midday(self, ttup: tuple) -> tuple:
        """
        Midday time in hours, minutes, and seconds representing the Badí'
        midday.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :return: The Badí' midday.
        :rtype: tuple
        """
        if ttup.short:
            date = (ttup.tm_year, ttup.tm_mon, ttup.tm_mday, ttup.tm_hour,
                    ttup.tm_min, ttup.tm_sec)
        else:
            date = (ttup.tm_kull_i_shay, ttup.tm_vahid, ttup.tm_year,
                    ttup.tm_mon, ttup.tm_mday, ttup.tm_hour, ttup.tm_min,
                    ttup.tm_sec)

        return self.midday(date)

    def _get_year(self, ttup: tuple) -> int:
        """
        Convert The Kull-i-Shay', Váḥid, year to a short mode year.

        :param ttup: A struct_time object.
        :type ttup: ShortFormStruct or LongFormStruct
        :return: The short form year converted if necessary.
        :rtype: int
        """
        return (ttup.tm_year if ttup.short else
                ((ttup.tm_kull_i_shay - 1) * 361 + (ttup.tm_vahid - 1) * 19 +
                 ttup.tm_year))

    def _year_week_day(self, year: int, month: int, day: int,
                       week0: bool=False) -> tuple:
        """
        Return the year, week, and day of the week from a short form
        Badí' date.

        :param int, year: The year.
        :param int month: The month.
        :param int day: The day.
        :return: The year, week, and day of the week.
        :rtype: tuple
        """
        week1jalal = self._isoweek1jalal(year)
        today = self._ymd2ord(year, month, day)
        # Internally, week and day have origin 0
        week, day = divmod(today - week1jalal, 7)

        if not week0:
            if week < 0:
                year -= 1
                week1jalal = self._isoweek1jalal(year)
                week, day = divmod(today - week1jalal, 7)
            elif week >= 52:
                if today >= self._isoweek1jalal(year+1):
                    year += 1
                    week = 0

        return year, week+1, day+1

    def _days_before_year(self, year: int) -> float:
        """
        Get the number of days before the 1st of Bahá of the year.

        :param int year: Badí' year
        :return: The number of days since (-1841, 19, 19) of the Badí'
                 calendar.
        :rtype: int
        """
        jd0 = self.jd_from_badi_date((self.MINYEAR-1, 19, 19), _chk_on=False)
        jd1 = self.jd_from_badi_date((year, 1, 1), _chk_on=False)
        return math.floor(jd1 - jd0) - 1

    def _days_in_month(self, year: int, month: int) -> int:
        """
        The number of days in provided month in provided year.

        :param int year: Badí' year
        :param int month: Badí' month (0..19)
        :return: The number of in the current month.
        :rtype: int
        """
        return 4 + self._is_leap_year(year) if month == 0 else 19

    def _days_before_month(self, year: int, month: int) -> int:
        """
        The number of days in the year preceding the first day of month.

        :param int year: Badí' year
        :param int month: Badí' month (0..19)
        :return: The number in the year preceding the first day of month.
        :rtype: int
        """
        month -= -18 if month < 2 else 1 if 1 < month < 19 else 19
        dbm = 0

        if 0 < month < 19:
            dbm += month * 19
        elif month == 0:
            dbm += 18 * 19 + 4 + self._is_leap_year(year)

        return dbm

    def _day_of_week(self, year: int, month: int, day: int) -> int:
        """
        Find the day of the week where 0 == Jalál (Saturday) and
        6 == Istiqlāl (Friday).

        :param int year: Badí' year
        :param int month: Badí' month (0..19)
        :param int day: Badí' day
        :return: The numerical day of the week.
        :rtype: int
        """
        # Since the usual start day is Monday (Kamál) a properly aligned
        # day number to the day name we need to add 1 to the ordinal.
        return ((self._ymd2ord(year, month, day) + 1) % 7 + 7) % 7

    def _ymd2ord(self, year: int, month: int, day: int) -> int:
        """
        Get the number of days since Badí' year -1842 (Julian 0001-03-19)
        including the current day.

        year, month, day -> ordinal, considering -1842-01-01 as day 1

        :param int year: Badí' year
        :param int month: Badí' month [0, 19]
        :param int day: Badí' day
        :return: The number of days since Badí' year -1842 including the
                 current day.
        :rtype: int
        """
        dim = self._days_in_month(year, month)
        assert 1 <= day <= dim, (
            f"Day '{day}' for month {month} must be in range of 1..{dim}")
        # We add 77 days to the total so that the ordinal number can be
        # compared to the ordinals in the standard datetime package.

        # For some reason out of the 3004 year that are provided only
        # these three years are off by 1.
        if year in (-1796, -1792, -1788):
            fudge = 1
        else:
            fudge = 0

        return (self.DAYS_BEFORE_1ST_YEAR + self._days_before_year(year) +
                self._days_before_month(year, month) + day + fudge)

    def _ord2ymd(self, n: int, *, short: bool=False) -> tuple:
        """
        It is difficult to do this in the Badí' Calendar because a Badí' day
        can be more or less than 24 hours depending on when sunset is and the
        time of the year. From the summer Solstice to the winter Solstice the
        days get shorter. The day slowly comes down to 24 hours around the
        Fall Equinox and then below 24 hours. The inverse happens between the
        Winter Solstice and the Summer Solstice. We just use the BadiCalendar
        API.

        :param int n: The ordinal number of days from the MINYEAR.
        :param bool short: If True then parse for a short date else if False
                           parse for a long date.
        :return: The Badí' date.
        :rtype: tuple
        """
        # We subtract 77 days from the total then add the value of n so that
        # the Badí' date will be the same as the date value passed into
        # _ymd2ord and give the same date as Python standard datetime package.
        # The reason we need to do this is that the first date that this
        # package can provide is equivalent to Julian year 1, March, 19th.
        jd = self.FIRST_YEAR_EPOCH - 1 - self.DAYS_BEFORE_1ST_YEAR + n
        return self.badi_date_from_jd(math.floor(jd) + 0.5, short=short,
                                      trim=True, rtd=True, _chk_on=False)

    def _build_struct_time(self, date: tuple, dstflag: int, *, tzinfo=None,
                           short_in: bool=False) -> NamedTuple:
        """
        Build either a ShortFormStruct or LongFormStruct struct_time.

        :param tuple date: A tuple date and time object.
        :param int dstflag: A flag indicating daylight savings time. May be
                            set to 1 when daylight savings time is in effect,
                            and 0 when it is not. A value of -1 indicates
                            that this is not known.
        :param tzinfo tzinfo: If provided a tzinfo object.
        :return: A struct_time object.
        :rtype: ShortFormStruct or LongFormStruct
        """
        if short_in:
            y, m, d, hh, mm, ss = date
        else:
            y, m, d, hh, mm, ss, ms = self.short_date_from_long_date(
                date, _chk_on=False)

        wday = self._day_of_week(y, m, d)
        dnum = self._days_before_month(y, m) + d
        return struct_time(date + (wday, dnum, dstflag), tzinfo=tzinfo)

    def _isoweek_to_badi(self, year: int, week: int, day: int, *,
                         short: bool=False) -> tuple:
        """
        The week counts from Jalál (Saturday) as the first day and Istiqlāl
        (Friday) the last day of the week. This is different from the usual
        way ISO weeks are counted in the Gregorian Calendar which is Monday
        to Sunday.

        .. note::

           Whereas a Gregorian year can have 53 weeks in it a Badí' year can
           have 51 weeks in it and never 53.

        :param int year: Badí' year.
        :param int month: Badí' month (0..19)
        :param int day: Badí' day in week.
        :param bool short: If True then parse for a short date else if False
                           parse for a long date.
        :return: A Badí' date.
        :rtype: tuple
        :raises AssertionError: If the week or weekday is out of range.
        """
        p_offset = 0

        if not 0 < week < 52:  # We're looking for only the 53rd week.
            day_one = _td_utils._day_of_week(year, 1, 1) + 1

            if day_one in (3, 4):
                out_of_range = True

                if week == 52:
                    # In Badí' years that have 52 weeks and start on the 3rd
                    # day (Kamál) or the 4th day (Fiḍāl).
                    # Badí' weeks start on Jalal (Saturday).
                    # *** TODO *** This needs to be tested over a larger year
                    #              spread.
                    p_offset = 7
                    out_of_range = False

                assert not out_of_range, f"Invalid week: {week}"

        assert 0 < day < 8, f"Invalid weekday: {day} (range is [1, 7])"
        # Now compute the offset from (Y, 1, 1) in days:
        day_offset = (week - 1) * 7 + (day - 1) + p_offset
        # Calculate the ordinal day for Jalal, week 1
        day_1 = self._isoweek1jalal(year)
        ord_day = day_1 + day_offset
        return self._ord2ymd(ord_day, short=short)

    def _isoweek1jalal(self, year: int) -> int:
        """
        Calculate the day number of Jalál (Saturday) starting week 1. It
        would be the first week with 4 or more days in the year in question.

        :param int year: Badí' year
        :return: The number of the first Jalál in the Badí' year.
        :rtype: int
        """
        firstday = self._ymd2ord(year, 1, 1)
        # We subtract 6 instead of add 6 as is done in _isoweek1Monday.
        firstweekday = (firstday - 6) % 7
        week1jalal = firstday - firstweekday

        if firstweekday > 3:  # First week day >= Fidal
            week1jalal += 7

        return week1jalal

    def _parse_isoformat_date_time_timezone(self, dtstr: str) -> tuple:
        """
        Parse both the date and time represented in an ISO string into a
        date and time tuple.

        :param str dtstr: A ISO compliant time string.
        :return: The date, time, and timezone.
        :rtype: tuple, tuple, timezone
        """
        def find_index(string, lst):
            indexes = [i for c in lst if (i := string.rfind(c)) > -1]
            return indexes[0] if len(indexes) > 0 else len(string)

        tz_chars = ('Z', 'B', '+', '-')
        init_chars = ('T', ' ') + tz_chars
        idx = find_index(dtstr, init_chars)
        str_date = dtstr[:idx]
        date = self._parse_isoformat_date(str_date) if str_date else ()
        str_other = dtstr[idx:]
        tz_1st = str_other[0] in tz_chars

        if tz_1st:
            tz = (self._parse_isoformat_timezone(str_other)
                  if str_other else None)
            time = ()
        else:
            idx = find_index(str_other, tz_chars)
            str_time = str_other[:idx]
            str_tz = str_other[idx:]
            time = self._parse_isoformat_time(str_time) if str_time else ()
            tz = self._parse_isoformat_timezone(str_tz) if str_tz else None

        return date, time, tz

    def _parse_isoformat_date(self, dtstr: str) -> tuple:
        """
        Parse a date ISO formatted string.

        :param str dtstr: A ISO compliant time string.
        :return: The year, month, and day parsed from a ISO string.
        :rtype: tuple
        :raises AssertionError: Raised when the year is out of range or when
                                too many hyphens are used.
        :raises IndexError: When a string index is out of range.
        :raises ValueError: Raised when an invalid string is being parsed to
                            an integer or when an invalid ISO string is being
                            parsed.
        """
        for c in filter(lambda x: not x.isnumeric(), dtstr):
            if c not in ('-', 'W'):
                raise ValueError(
                    f"Invalid character {c!r} in incoming date string.")

        if dtstr != '':
            neg = dtstr[0] == '-'
            year = int(dtstr[:4 + neg])
            assert _td_utils.MINYEAR <= year <= _td_utils.MAXYEAR, (
                f"Year is out of range: {year}, min {_td_utils.MINYEAR}, "
                f"max {_td_utils.MAXYEAR}.")
            dtstr = dtstr[1:] if neg else dtstr

        dc = dtstr.count('-')
        wc = dtstr.count('W')
        assert ((wc == 0 and dc in (0, 1, 2)) or
                (wc == 1 and dc in (0, 1, 2))), (
                "Invalid format, there must be between 0 to 2 hyphens (-) in "
                "the date format or there can be one uppercase (W) week "
                "identifier and between 0 and 2 hyphens (-) used.")
        d_len = len(dtstr)

        if dc == 1 and d_len == 7 and not wc:    # YYYY-MM
            date = (year, int(dtstr[5:7]), 1)
        elif dc == 0 and d_len == 8 and not wc:  # YYYYMMDD
            date = (year, int(dtstr[4:6]), int(dtstr[7:9]))
        elif dc == 2 and not wc:                 # YYYY-MM-DD
            date = (year, int(dtstr[5:7]), int(dtstr[8:10]))
        # YYYYWww, YYYY-Www, YYYYWwwD, YYYY-Www-D
        elif wc and 7 <= d_len <= 10:
            pos = 5 if dc == 0 else 6
            wday = int(dtstr[pos:pos+2])
            pos += 2 if dc == 0 else 3
            d = dtstr[pos:]
            assert (dc == 1 and d_len == 8) or dc in (0, 2), (
                f"Invalid ISO string {dtstr}.")
            day = int(d) if d.isdigit() else 1
            date = self._isoweek_to_badi(year, wday, day, short=True)[:3]
        elif d_len in (7, 8):                    # YYYYDDD or YYYY-DDD
            month_days = self._BADI_MONTH_NUM_DAYS
            month_days[18] = (0, 4 + self._is_leap_year(year))
            days = int(dtstr[4:7] if dc == 0 else dtstr[5:8])

            for month, ds in month_days:
                if days <= ds: break
                days -= ds

            date = (year, month, days)
        else:
            date = ()

        return date

    def _parse_isoformat_time(self, tmstr: str) -> tuple:
        """
        Parse a time ISO formatted string.

        :param str tmstr: A ISO compliant time string.
        :return: The hour, minute, and second parsed from an ISO string.
        :rtype: tuple
        :raises AssertionError: Raised when there are invalid time designators,
                                when to many colons used, or when too many dots
                                are used.
        :raises ValueError: Raised when an invalid string is being parsed to
                            an integer or when an invalid ISO string is being
                            parsed.
        """
        for c in filter(lambda x: not x.isnumeric(), tmstr):
            if c not in ('T', ' ', ':', '.'):
                raise ValueError(
                    f"Invalid character {c!r} in incoming time string.")

        t_len = len(tmstr)
        tmp_tmstr = tmstr
        tc = tmstr.count('T')
        sc = tmstr.count(' ')
        assert ((tc == 0 and sc == 0) or
                (tc == 1 or sc == 1) and (tc or sc) and not (tc and sc)), (
            "Cannot have both a 'T' and a space or more than one of either to "
            "indicate time.")

        if sc:
            tmstr = "T" + tmstr[1:]
            del sc
            tc = 1

        if t_len > 0 and 'T' != tmstr[0]:
            raise ValueError("Invalid time string, 1st character must be "
                             f"one of ( T), found {tmp_tmstr!r}")

        del tmp_tmstr
        cc = tmstr.count(':')
        assert cc < 3, (
            f"Invalid number of colons (:), can be 0 - 2, found {cc}")
        pc = tmstr.count('.')
        assert pc <= 1, f"Invalid number of dots (.), can be 0 - 1, found {pc}"

        if t_len > 2:
            hour = int(tmstr[1:3])
            pos0 = 1 if cc else 0
            pos1 = 2 if cc == 2 else 0

            if t_len > 3:
                if tmstr[3] == '.':  # Thh.hhh
                    ph = float(tmstr[3:]) * 60
                    minute = math.floor(ph)
                    second = (ph % 1) * 60
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                elif tmstr[5 + pos0:6 + pos0] == '.':
                    # Thhmm.mmm or Thh:mm.mmm
                    minute = int(tmstr[3 + pos0:5 + pos0])
                    pm = float(tmstr[5 + pos0:])
                    second = pm * 60
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                elif tmstr[7 + pos0:8 + pos0] == '.':
                    # Thhmmss.sss or Thh:mm:ss.sss
                    minute = int(tmstr[3 + pos0:5 + pos0])
                    second = float(tmstr[5 + pos0:])
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                elif t_len == 5 + pos0:  # Thhmm or Thh:mm
                    minute = int(tmstr[3 + pos0:5 + pos0])
                    time = (hour, minute, 0)
                elif t_len >= 7 + pos1:  # Thhmmss.sss or Thh:mm:ss.sss
                    minute = int(tmstr[3 + pos0:5 + pos0])
                    second = float(tmstr[5 + pos1:])
                    second = math.floor(second) if second % 1 == 0 else second
                    time = (hour, minute, second)
                else:
                    raise ValueError(f"Invalid time string, found {tmstr!r}")
            else:  # Thh
                time = (hour, 0, 0)
        else:
            time = ()

        return time

    def _parse_isoformat_timezone(self, tzstr: str) -> tuple:
        """
        Parse a timezone ISO formatted string.

        :param str tzstr: A ISO compliant time string.
        :return: A timezone object indicating the offset from UTC.
        :rtype: timezone
        :raises AssertionError: Raised when there are invalid timezone
                                delimiters are found.
        :raises ValueError: Raised when an invalid ISO string is being parsed.
        """
        from .datetime import timezone, timedelta

        for c in filter(lambda x: not x.isnumeric(), tzstr):
            if c not in ('Z', 'B', '+', '-', ':'):
                raise ValueError(
                    f"Invalid character {c!r} in incoming timezone string.")

        tz_len = len(tzstr)
        tmp_tzstr = tzstr
        nc = tzstr.count('-')
        pc = tzstr.count('+')
        zc = tzstr.count('Z')
        bc = tzstr.count('B')  # This is an extension to the ISO standard
        c_none = all([True for c in (nc, pc, zc, bc) if c == 0])  # All == 0
        ct_gt_1 = sum((nc, pc, zc, bc)) > 1  # More than one == 1
        ca_gt_1 = any([True for c in (nc, pc, zc, bc) if c > 1])  # Any > 1
        assert c_none and not ct_gt_1 and not ca_gt_1, (
            "Can only have one of (-+Z) and no more than one of (-+Z) to "
            "indicate a timezone.")

        if tz_len > 0 and ('-' != tzstr[0] and '+' != tzstr[0] and
                           'Z' != tzstr[0]) and 'B' != tzstr[0]:
            raise ValueError("Invalid timezone string, 1st character must be "
                             f"one of (-+Z), found {tmp_tzstr!r}")

        del tmp_tzstr
        cc = tzstr.count(':')
        assert cc < 2, (
            f"Invalid number of colons (:), can be 0 - 1, found {cc}")

        if tz_len > 0:
            if zc == 1 and tz_len == 1:
                tz = timezone.utc
            elif bc == 1 and tz_len == 1:
                tz = timezone.badi
            else:
                offset = [int(x) for x in tzstr[1:].split(':')]
                offset[0] = offset[0] * -1 if nc else offset[0]
                offset += [0] if len(offset) == 1 else []
                tz = timezone(timedelta(hours=offset[0], minutes=offset[1]))
        else:
            tz = None

        return tz

    def _check_date_fields(self, a: int, b: int, c: int, d: int=None,
                           e: int=None, *, short_in: bool=False) -> None:
        """
        Check the validity of the date.

        :param int a: The long form Kull-i-Shay or short form year.
        :param int b: The long form Váḥid or short form month.
        :param int c: The long form year or short form day.
        :param int d: The long form month.
        :param int e: The long form day.
        :param bool short_in: If True then parse for a short date else if False
                              parse for a long date. This is for incoming dates
                              not outgoing dates as in most other uses of
                              'short'.
        :return: Nothing
        :rtype: None
        :raises AssertionError: If any of the date values are out of range.
        """
        if short_in:
            b_date = (a, b, c)
        else:
            b_date = (a, b, c, d, e)

        self._check_valid_badi_date(b_date, short_in=short_in)

    def _check_time_fields(self, hour: int, minute: int, second: int,
                           microsecond: int, fold: int) -> None:
        """
        Check the validity of the time.

        :param int hour: The hour.
        :param int minute: The minute.
        :param int second: The second.
        :param int microsecond: The microsecond.
        :param int fold: The value of 1 if the time is in the time fold when
                         the time falls back one hour in the Autumn or 0 any
                         other time of the year.
        """
        self._check_valid_badi_time(hour, minute, second, microsecond,
                                    maxsec=61)
        assert fold in (0, 1), (
            f"The fold argument '{fold}' must be either 0 or 1.")

    def _wrap_strftime(self, object, format: str,
                       timetuple: tuple) -> str:
        """
        Correctly substitute for %z and %Z escapes in strftime formats.

        :param class object: A class instance that uses this method.
        :param str format: The formatted string.
        :param timetuple:
        :type ttup: ShortFormStruct or LongFormStruct
        :return: A correctly formatted string.
        :rtype: str
        """
        from .datetime import timedelta
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

                                    h, rest = divmod(offset,
                                                     timedelta(hours=1))
                                    m, rest = divmod(rest,
                                                     timedelta(minutes=1))
                                    s = rest.seconds
                                    u = offset.microseconds
                                    zreplace = f'{sign}{h:02}{m:02}'

                                    if u:
                                        zreplace += f'{s:02}.{u:06}'
                                    elif s:
                                        zreplace += f'{s:02}'

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
