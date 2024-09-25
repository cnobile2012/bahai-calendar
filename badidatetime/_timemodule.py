# -*- coding: utf-8 -*-
#
# badidatetime/_timemodule.py
#
__docformat__ = "restructuredtext en"

import locale

from .structures import struct_time, ShortFormStruct, LongFormStruct


class TimeModule:
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
        """
        self._ticks_per_second_initialized = False
        self._ticks_per_second = -1

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
        return f"{ttup.tm_hour:02}"

    def I(self, ttup, mod):
        """
        *** TODO *** Does a 12-hour clock this make sense in a Badi time.
        """
        return


    __METHOD_LOOKUP = {'a': a, 'A': A, 'b': b, 'B': B, 'c': c, 'C': C, 'd': d,
                       'D': D, 'e': d, 'f': f, 'G': G, 'h': b, 'H', H, 'I': I,
                       
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


