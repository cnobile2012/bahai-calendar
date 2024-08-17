# -*- coding: utf-8 -*-
#
# badidatetime/structures.py
#
__docformat__ = "restructuredtext en"

from dataclasses import dataclass
from .badi_calendar import BahaiCalendar


class BaseFormStruct:
    pass



class ShortFormStruct(BaseFormStruct):
    pass



class LongFormStruct(BaseFormStruct):
    pass



@dataclass
class struct_time(BahaiCalendar):
    """
    Create a structure representing a Badi date and time.
    """
    def __new__(cls, date:tuple) -> BaseFormStruct:
        self = object.__new__(cls)
        super().__init__(self)

        if cls.__find_form_type(date):
            (tm_year, tm_month, tm_day, tm_hour, tm_min, tm_sec, tm_wday,
             tm_yday, tm_isdst) = date
            self._date = date
            #self.__short = True
        else:
            (tm_kull_i_shay, tm_vahid, tm_year, tm_month, tm_day, tm_hour,
             tm_min, tm_sec, tm_wday, tm_yday, tm_isdst) = date
            self._date = date
            #self.__short = False

        return self

    @classmethod
    def __find_form_type(cls, date):
        d_size = len(date)

        if d_size == 9:
            short = True
        elif d_size == 11:
            short = False
        else:
            raise TypeError(f"struct_time() takes 9 or 11-sequence "
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
