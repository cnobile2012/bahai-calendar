# -*- coding: utf-8 -*-
#
# badidatetime/structures.py
#
__docformat__ = "restructuredtext en"

import time
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError
from typing import NamedTuple

from .badi_calendar import BahaiCalendar


class ShortFormStruct(NamedTuple):
    tm_year: int       # range[1, 19]
    tm_month: int      # range[0, 19]
    tm_day: int        # range[1, 19]
    tm_hour: int       # range[0, 23]
    tm_min: int        # range[0, 59]
    tm_sec: float      # range[0, 61]
    tm_wday: int       # range[0, 6]; Jalál (Saturday) is 0
    tm_yday: int       # range[1, 366]; day in year
    # tm_isdst, may be set to 1 when daylight savings time is in effect,
    # and 0 when it is not. A value of -1 indicates that this is not known,
    # and will usually result in the correct state being filled in.
    tm_isdst: int      # 0, 1 or -1
    tm_zone: str = ''  # abbreviation of timezone name
    tm_gmtoff: int = 0 # offset east of UTC in seconds

    def __repr__(self) -> str:
        return (f"ShortFormStruct(tm_year={self.tm_year}, "
                f"tm_month={self.tm_month}, tm_day={self.tm_day}, "
                f"tm_hour={self.tm_hour}, tm_min={self.tm_min}, "
                f"tm_sec={self.tm_sec}, tm_wday={self.tm_wday}, "
                f"tm_yday={self.tm_yday}, tm_isdst={self.tm_isdst})")


class LongFormStruct(NamedTuple):
    tm_kull_i_shay: int  # example, 1
    tm_vahid: int        # range[1, 19]
    tm_year: int         # range[1, 19]
    tm_month: int        # range[0, 19]
    tm_day: int          # range[1, 19]
    tm_hour: int         # range[0, 23]
    tm_min: int          # range[0, 59]
    tm_sec: float        # range[0, 61]
    tm_wday: int         # range[0, 6]; Jalál (Saturday) is 0
    tm_yday: int         # range[1, 366]; day in year
    # tm_isdst, may be set to 1 when daylight savings time is in effect,
    # and 0 when it is not. A value of -1 indicates that this is not known,
    # and will usually result in the correct state being filled in.
    tm_isdst: int        # 0, 1 or -1
    tm_zone: str = ''    # abbreviation of timezone name
    tm_gmtoff: int = 0   # offset east of UTC in seconds

    def __repr__(self) -> str:
        return (f"LongFormStruct(tm_kull_i_shay={self.tm_kull_i_shay}, "
                f"tm_vahid={self.tm_vahid}, tm_year={self.tm_year}, "
                f"tm_month={self.tm_month}, tm_day={self.tm_day}, "
                f"tm_hour={self.tm_hour}, tm_min={self.tm_min}, "
                f"tm_sec={self.tm_sec}, tm_wday={self.tm_wday}, "
                f"tm_yday={self.tm_yday}, tm_isdst={self.tm_isdst})")


class struct_time(BahaiCalendar):
    """
    Create a structure representing a Badi date and time.
    """
    def __new__(cls, date:tuple):
        self = object.__new__(cls)
        super().__init__(self)

        if cls.__is_short_form_type(date):
            inst = ShortFormStruct(*cls.__fill_ins(date))
        else:
            inst = LongFormStruct(*cls.__fill_ins(date))

        return inst

    @classmethod
    def __is_short_form_type(cls, date):
        d_size = len(date)

        if d_size == 9:
            short = True
        elif d_size == 11:
            short = False
        else:
            raise TypeError(f"struct_time() takes a 9 or 11-sequence "
                            f"({d_size}-sequence given)")

        # Check for a valid date format.
        cls.__check_values(date, short)
        return short

    @classmethod
    def __check_values(cls, date, short):
        """
        Test the Váḥid, year, month, day, hour, minute, and second
        values are valid.
        """
        cycle = 20

        if short:
            y, m, d, hh, mm, ss, wday, yday, isdst = date
            b_date = date[:5]
        else:
            k, v, y, m, d, hh, mm, ss, wday, yday, isdst = date
            b_date = date[:7]
            assert 1 <= v < cycle, f"Invalid Váḥid must be 1 to 19, found {v}."
            assert 1 <= y < cycle, f"Invalid year must be 1 to 19, found {y}."

        assert 0 <= m < cycle, f"Invalid month must be 0 to 19, found {m}"
        if m == 0: cycle = + 5 + self._is_leap_year(b_date)

    @classmethod
    def __fill_ins(cls, date):
        """
        Fill in missing data.

        *** TODO *** Do we really have DST (EDT) in a Badi Calendar?
        """
        date = list(date)
        org_tm_isdst = date[-1]
        localtime = time.localtime()

        if org_tm_isdst not in (-1, 0, 1):
            msg = (f"Invalid value for tm_isdst, found {tm_isdst}, should be "
                   "one of (-1, 0, 1).")
            raise ValueError(msg)

        tm_isdst = localtime.tm_isdst
        tm_zone = time.tzname[tm_isdst]
        tm_gmtoff = localtime.tm_gmtoff
        date[-1] = tm_isdst
        date += [tm_zone, tm_gmtoff]
        return tuple(date)
