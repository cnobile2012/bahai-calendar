# -*- coding: utf-8 -*-

from badidatetime import BahaiCalendar


class Rollover(BahaiCalendar):

    # def badi_time_fraction(self, h, m, s):
    #     return (h * 3600 + m * 60 + s) / self._SECONDS_PER_DAY

    # def badi_ordinal(self, year, month, day):
    #     leap = self._is_leap_year(year)

    #     # Days in previous years
    #     days = (year - 1) * 361 + self._leap_days_before(year)

    #     # Days in previous months of this year
    #     if month == 0:  # Ayyám-i-Há
    #         days += 18 * 19
    #     else:
    #         days += (month - 1) * 19

    #         if month > 18:
    #             days += 4 + leap

    #     # Days in current month
    #     days += day - 1
    #     return days

    # def ordinal_to_year(self, n):
    #     year = n // 361 + 1

    #     while n >= badi_ordinal(year + 1, 1, 1):
    #         year += 1

    #     return year

    # def ordinal_to_month_day(self, n, year):
    #     leap = self._is_leap_year(year)
    #     y0 = badi_ordinal(year, 1, 1)
    #     d = n - y0

    #     # First 18 months
    #     if d < 18 * 19:
    #         month = d // 19 + 1
    #         day = d % 19 + 1
    #         return month, day

    #     d -= 18 * 19

    #     # Ayyám-i-Há
    #     ayyamiha = 4 + leap

    #     if d < ayyamiha:
    #         return 0, d + 1

    #     d -= ayyamiha
    #     # Month 19
    #     return 19, d + 1

    # def fraction_to_time(self, frac):
    #     total = int(round(frac * self._SECONDS_PER_DAY))
    #     h, rem = divmod(total, 3600)
    #     m, s = divmod(rem, 60)
    #     return h, m, s

    # Add delta

    def add_minutes(self, year, month, day, hour, minute, second,
                    delta_minutes):
        total_seconds = self.badi_to_ordinal_seconds(
            year, month, day, hour, minute, second)
        total_seconds += delta_minutes * 60
        return self.ordinal_seconds_to_badi(total_seconds)

    def badi_to_ordinal_seconds(self, year, month, day, hour, minute, second):
        leap = self._is_leap_year(year)
        # Days before this Badí year
        days = 77 + 361 * (year - 1) + self._leap_days_before(year)

        # Days before this month
        if month == 0:  # Ayyám-i-Há
            days += 19 * 18
        elif month < 19:
            days += 19 * (month - 1)
        else:  # month 19
            days += 19 * 18 + (4 + leap)

        # Days before today
        days += (day - 1)
        # Time of day
        seconds = hour * 3600 + minute * 60 + second
        return days * self._SECONDS_PER_DAY + seconds

    def ordinal_seconds_to_badi(self, total_seconds: float):
        # Split into day + time
        days, seconds = divmod(total_seconds, self._SECONDS_PER_DAY)

        if seconds < 0:
            days -= 1
            seconds += self._SECONDS_PER_DAY

        # Remove Gregorian anchor
        days -= 77
        # --- Exact year normalization ---
        year = 1

        if days >= 0:
            while True:
                year_len = 361 + self._is_leap_year(year)

                if days < year_len:
                    break

                days -= year_len
                year += 1
        else:
            while days < 0:
                year -= 1
                days += 361 + self._is_leap_year(year)

        leap = self._is_leap_year(year)

        # --- Month / day ---
        if days < 19 * 18:
            month = days // 19 + 1
            day = days % 19 + 1
        elif days < 19 * 18 + (4 + leap):
            month = 0
            day = days - 19 * 18 + 1
        else:
            month = 19
            day = days - (19 * 18 + (4 + leap)) + 1

        # --- Time ---
        hour, rem = divmod(int(seconds), 3600)
        minute, second = divmod(rem, 60)
        return year, month, day, hour, minute, second

    def _leap_days_before(self, year):
        return sum(1 for y in range(1, year) if self._is_leap_year(y))
