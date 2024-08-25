# -*- coding: utf-8 -*-
#
# badidatetime/structures.py
#
__docformat__ = "restructuredtext en"

from dataclasses import dataclass, field
from .badi_calendar import BahaiCalendar

class BaseFormStruct:
    pass


@dataclass(order=True)
class ShortFormStruct(BaseFormStruct):
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
    tm_gmtoff: int = 0.0 # offset east of UTC in seconds

    def __getitem__(self, idx):
        return (self.tm_year, self.tm_month, self.tm_day,
                self.tm_hour, self.tm_min, self.tm_sec,
                self.tm_wday, self.tm_yday, self.tm_isdst)[idx]


@dataclass(order=True)
class LongFormStruct(BaseFormStruct):
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
    tm_gmtoff: int = 0.0 # offset east of UTC in seconds

    def __getitem__(self, idx):
        return (self.tm_kull_i_shay, self.tm_vahid, self.tm_year,
                self.tm_month, self.tm_day, self.tm_hour, self.tm_min,
                self.tm_sec, self.tm_wday, self.tm_yday, self.tm_isdst)[idx]


class struct_time(BahaiCalendar):
    """
    Create a structure representing a Badi date and time.
    """
    def __new__(cls, date:tuple) -> BaseFormStruct:
        self = object.__new__(cls)
        super().__init__(self)

        if cls.__is_short_form_type(date):
            inst = ShortFormStruct(*date)
        else:
            inst = LongFormStruct(*date)

        # *** TODO *** Fill in the tm_zone and tm_gmtoff arguments here.

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
