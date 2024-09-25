# -*- coding: utf-8 -*-
#
# badidatetime/structures.py
#
__docformat__ = "restructuredtext en"

import time
import tzlocal
from datetime import datetime, timezone
from zoneinfo import ZoneInfo
from typing import NamedTuple

from .badi_calendar import BahaiCalendar, GregorianCalendar
import badidatetime.datetime as dtime


class ShortFormStruct(NamedTuple):
    tm_year: int       # range[-1842, 1161]
    tm_mon: int        # range[0, 19]
    tm_mday: int       # range[1, 19]
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

    @property
    def short(self):
        return True

    def __repr__(self) -> str:
        return (f"structures.ShortFormStruct(tm_year={self.tm_year}, "
                f"tm_mon={self.tm_mon}, tm_mday={self.tm_mday}, "
                f"tm_hour={self.tm_hour}, tm_min={self.tm_min}, "
                f"tm_sec={self.tm_sec}, tm_wday={self.tm_wday}, "
                f"tm_yday={self.tm_yday}, tm_isdst={self.tm_isdst})")


class LongFormStruct(NamedTuple):
    tm_kull_i_shay: int  # example, 1
    tm_vahid: int        # range[1, 19]
    tm_year: int         # range[1, 19]
    tm_mon: int          # range[0, 19]
    tm_mday: int         # range[1, 19]
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

    @property
    def short(self):
        return False

    def __repr__(self) -> str:
        return (f"structures.LongFormStruct("
                f"tm_kull_i_shay={self.tm_kull_i_shay}, "
                f"tm_vahid={self.tm_vahid}, tm_year={self.tm_year}, "
                f"tm_mon={self.tm_mon}, tm_mday={self.tm_mday}, "
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

        if cls.__is_short_form(date):
            inst = ShortFormStruct(*cls.__fill_in_missing(date, True))
        else:
            inst = LongFormStruct(*cls.__fill_in_missing(date, False))

        return inst

    @classmethod
    def __is_short_form(cls, date):
        d_size = len(date)

        if d_size == 9:
            short = True
        elif d_size == 11:
            short = False
        else:
            raise TypeError("struct_time() takes a 9 or 11-sequence "
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
    def __fill_in_missing(cls, date, short):
        """
        Fill in missing data.

        *** TODO ***
        We need to convert everything to a Gregorian datetime object until
        the badidatetime objects are completed.
        """
        bc = BahaiCalendar()
        gc = GregorianCalendar()
        date = list(date)
        org_tm_isdst = date[-1]

        if org_tm_isdst not in (-1, 0, 1):
            msg = (f"Invalid value for tm_isdst, found {org_tm_isdst}, "
                   "should be one of (-1, 0, 1).")
            raise ValueError(msg)

        if short:
            b_date = date[:6]
        else:
            b_date = bc.short_date_from_long_date(date[:8])

        g_date = gc.ymdhms_from_date(bc.gregorian_date_from_badi_date(
            b_date), ms=True)
        g_dt = datetime(*g_date, tzinfo=tzlocal.get_localzone())
        # tm_wday
        date[-3] = dtime._day_of_week(bc, *b_date[:3])
        # tm_yday
        date[-2] = dtime._days_before_month(bc, *b_date[:2]) + b_date[2]
        # tm_isdst
        date[-1] = 1 if g_dt.dst().total_seconds() > 0 else 0
        # tm_zone and tm_gmtoff
        date += [g_dt.tzname(), g_dt.utcoffset().total_seconds()]
        return tuple(date)
