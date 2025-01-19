# -*- coding: utf-8 -*-
#
# badidatetime/_structures.py
#
__docformat__ = "restructuredtext en"

from typing import NamedTuple


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
    tm_isdst: int      # 0, 1 or -1 (set only with localtime)
    # abbreviation of timezone name (set only with localtime)
    tm_zone: str = None
    # offset east of UTC in seconds (set only with localtime)
    tm_gmtoff: int = None

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
    tm_isdst: int        # 0, 1 or -1 (set only with localtime)
    # abbreviation of timezone name (set only with localtime)
    tm_zone: str = None
    # offset east of UTC in seconds (set only with localtime)
    tm_gmtoff: int = None

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


class struct_time:
    """
    Create a structure representing a Badi date and time.
    """
    def __new__(cls, date:tuple, tzinfo=None):
        self = object.__new__(cls)
        super().__init__(self)
        short = cls.__is_short_form(date)

        if date[-1] not in (-1, 0, 1):
            msg = (f"Invalid isdst '{date[-1]}', it must be in the range "
                   "of [-1, 1].")
            raise ValueError(msg)

        if short:
            inst = ShortFormStruct(*self.__fill_in_missing(date, tzinfo, short))
        else:
            inst = LongFormStruct(*self.__fill_in_missing(date, tzinfo, short))

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

        return short

    @classmethod
    def __fill_in_missing(cls, date, tzinfo, short):
        """
        Fill in missing data.
        """
        from badidatetime import datetime

        if short:
            b_date = date[:3] + (None, None) + date[3:6]
        else:
            b_date = date[:8]

        date = list(date)
        dt = datetime.datetime(*b_date, tzinfo=tzinfo)

        # Build out the tm_isdst, tm_zone and tm_gmtoff.
        if tzinfo:
            dst = tzinfo.dst(dt)

            if hasattr(dst, 'total_seconds'):
                ts = dst.total_seconds()

                if ts:
                    date[-1] = 1
                elif ts == 0:
                    date[-1] = 0

        offset = dt.utcoffset()
        total_seconds = offset.total_seconds() if offset else None

        if tzinfo and hasattr(tzinfo, 'key'):
            tm_zone = tzinfo.key
        else:
            tm_zone = dt.tzname()

        date += [tm_zone, total_seconds]
        return tuple(date)
