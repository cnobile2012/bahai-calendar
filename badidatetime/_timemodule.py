# -*- coding: utf-8 -*-
#
# badidatetime/_timemodule.py
#
__docformat__ = "restructuredtext en"

import locale

from .structures import struct_time, ShortFormStruct, LongFormStruct
from .badi_calendar import BahaiCalendar


class TimeModule(BahaiCalendar):
    # Badi additions are %:K for Kull-i-Shay and %:V for Váḥid
    VALID_FORMAT_CHRS = 'aAbBcCdDefGhHIjkKlmMnprSTuUvVwWxXyYzZ%'
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
        locale.setlocale(locale.LC_TIME, '')

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

    # %[aAbBcCdDefGhHIjkKlmMnprSTuUvVwWxXyYzZ%]
    # %-[dHjlmMSy]
    # %:[KVz]

    def a(self, ttup, mod):
        return self.DAYNAME_ABV[ttup.tm_wday]

    def A(self, ttup, mod):
        return self.DATNAME[ttup.tm_wday]

    def b(self, ttup, mod):
        """
        Return the abbreviated month name if %b or %h.
        """
        return self.MONTHNAMES_ABV[ttup.tm_mon]

    def B(self, ttup, mod):
        return self.MONTHNAMES[ttup.tm_mon]

    def c(self, ttup, mod):
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

    def C(self, ttup, mod):
        return math.floor(ttup.year / 100) if ttup.short else ""

    def d(self, ttup, mod):
        """
        Return a zero padded month. If the format was %-d or %e then
        return an un-padded decimal number.
        """
        if mod == '-':
            st = f"{ttup.tm_mday}"
        else:
            st = f"{ttup.tm_mday:02}"

        return st

    def D(self, ttup, mod):
        """
        Return a locale dependent Badi short date. Not valid for Badi
        long dates.
        """
        st = ""

        if ttup.short:
            n = '-' if ttup.tm_year < 0 else ''
            st_d = {'y': f"{n}{abs(ttup.tm_year):04}", 'm': f"{ttup.tm_mon:02}",
                    'd': f"{ttup.tm_mday:02}"}

            for ch in locale.nl_langinfo(locale.D_FMT):
                if ch == '%': continue
                st += st_d[ch] + ' '

        return st.strip()

    def f(self, ttup, mod):
        return f"{round(ttup.tm_sec % 1, 6)}"

    def G(self, ttup, mod):
        st = ""

        if ttup.short:
            n = '-' if ttup.tm_year < 0 else ''
            st += f"{n}{ttup.tm_year:04}"

        return st

    def H(self, ttup, mod):
        if  mod == '-':
            st = f"{ttup.tm_hour}"
        else:
            st = f"{ttup.tm_hour:02}"

        return st

    def I(self, ttup, mod):
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

        if  mod == '-':
            st = f"{hour}"
        else:
            st = f"{hour:02}"

        return st

    def j(self, ttup, mod):
        """
        """
        return f"{ttup.tm_yday:03}"

    def K(self, ttup, mod):
        """
        """
        st = ""

        if not ttup.short and mod == ':':
            n = '-' if ttup.tm_kull_i_shay < 0 else ''
            st += f"{n}{ttup.tm_kull_i_shay}"

        return st

    def m(self, ttup, mod):
        """
        """
        return f"{ttup.tm_mon}" if mod == '-' else f"{ttup.tm_mon:02}"

    def M(self, ttup, mod):
        """
        """
        return f"{ttup.tm_min:02}"

    def m(self, ttup, mod):
        """
        """
        if  mod == '-':
            st = f"{ttup.tm_mon}"
        else:
            st = f"{ttup.tm_mon:02}"

        return st

    def M(self, ttup, mod):
        """
        """
        if  mod == '-':
            st = f"{ttup.tm_min}"
        else:
            st = f"{ttup.tm_min:02}"

        return st

    def n(self, ttup, mod):
        """
        """
        return "\n"

    def p(self, ttup, mod):
        """
        """
        midday_frac = self._find_midday(ttup)
        time_frac = self.decimal_day_from_hms(ttup.hour, ttup.min, ttup.sec)

        if midday_frac <= time_frac:
            st = locale.nl_langinfo(locale.PM_STR)
        else:
            st = locale.nl_langinfo(locale.AM_STR)

        return st

    def r(self, ttup, mod):
        """
        """
        midday_frac = self._find_midday(ttup)


        return





    __METHOD_LOOKUP = {'a': a, 'A': A, 'b': b, 'B': B, 'c': c, 'C': C, 'd': d,
                       'D': D, 'e': d, 'f': f, 'G': G, 'h': b, 'H': H, 'I': I,
                       'j': j, 'k': H, 'l': I, 'm': m, 'M': M, 'm': m, 'M': M,
                       'n': n, 'p': p, 'r': r, 
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
                strf += __METHOD_LOOKUP[ch1](ttup, ch0 if i == 2 else '')

        return strf

    def _find_midday(self, ttup):
        if ttup.short:
            date = (ttup.tm_year, ttup.tm_mon, ttup.tm_mday)
        else:
            date = (ttup.tm_kull_i_shay, ttup.tm_vahid, ttup.tm_year,
                    ttup.tm_mon, ttup.tm_mday)

        return self.midday(date)
