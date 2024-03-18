# -*- coding: utf-8 -*-
#
# bahai_calendar/gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from bahai_calendar.base_calendar import BaseCalendar


class GregorianCalendar(BaseCalendar):
    """
    Implementation of the Gregorian Calendar.
    """
    #(defconstant gregorian-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of the (proleptic) Gregorian calendar.
    #  (rd 1))
    GREGORIAN_EPOCH = 1  # See BaseCalender notes.
    _MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    #(defun gregorian-leap-year? (g-year)
    #  ;; TYPE gregorian-year -> boolean
    #  ;; True if g-year is a leap year on the Gregorian calendar.
    #  (and (= (mod g-year 4) 0)
    #       (not (member (mod g-year 400)
    #                    (list 100 200 300)))))
    #GREGORIAN_LEAP_YEAR = lambda self, year: (
    #    year % 4 == 0 and (year % 400 not in (100, 200, 300)))

    # ((MOD(year, 4) = 0) * ((MOD(year, 100) <> 0) + (MOD(year, 400) = 0)) = 1)
    GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)

    def __init__(self):
        super().__init__()
        # [year, month, day]
        self._gregorian_date = None

    def parse_datetime(self, dt:datetime.datetime) -> None:
        self.date_representation = (dt.year, dt.month, dt.day)
        super().parse_datetime(dt)

    @property
    def date_representation(self):
        return self._gregorian_date

    @date_representation.setter
    def date_representation(self, representation):
        self._gregorian_date = representation

    def fixed_from_gregorian(self, g_date:tuple) -> int:
        """
        used

        (defun fixed-from-gregorian (g-date)
          ;; TYPE gregorian-date -> fixed-date
          ;; Fixed date equivalent to the Gregorian date g-date.
          (let* ((month (standard-month g-date))
                 (day (standard-day g-date))
                 (year (standard-year g-date)))
            (+ (1- gregorian-epoch)   ; Days before start of calendar
               (* 365 (1- year))      ; Ordinary days since epoch
               (quotient (1- year)
                         4)           ; Julian leap days since epoch...
               (-                     ; ...minus century years since epoch...
                (quotient (1- year) 100))
               (quotient              ; ...plus years since epoch divisible...
                (1- year) 400)        ; ...by 400.
               (quotient              ; Days in prior months this year...
                (- (* 367 month) 362) ; ...assuming 30-day Feb
                12)
               (if (<= month 2)       ; Correct for 28- or 29-day Feb
                   0
                 (if (gregorian-leap-year? year)
                     -1
                   -2))
               day)))                 ; Days so far this month.
        """
        self._check_valid_gregorian_month_day(g_date)
        t_len = len(g_date)
        year = g_date[0]
        month = g_date[1]
        day = g_date[2]
        year_1 = year - 1
        hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
        minute = g_date[4] if t_len > 4 and g_date[4] is not None else 0
        second = g_date[5] if t_len > 5 and g_date[5] is not None else 0
        day += self.HR(hour) + self.MN(minute) + self.SEC(second)
        result = (self.GREGORIAN_EPOCH - 1 + 365 * year_1 +
                  self.QUOTIENT(year_1, 4) - self.QUOTIENT(year_1, 100) +
                  self.QUOTIENT(year_1, 400) +
                  self.QUOTIENT(367 * month - 362, 12))

        if month > 2:
            if self.GREGORIAN_LEAP_YEAR(year):
                result -= 1
            else:
                result -= 2

        return result + day

    def gregorian_new_year(self, g_year:int) -> int:
        """
        used

        (defun gregorian-new-year (g-year)
          ;; TYPE gregorian-year -> fixed-date
          ;; Fixed date of January 1 in g-year.
          (fixed-from-gregorian
           (gregorian-date g-year january 1) )))
        """
        return self.fixed_from_gregorian((g_year, self.JANUARY, 1))

    # gregorian-year-end
    # gregorian-year-range

    def gregorian_year_from_fixed(self, date:float) -> int:
        """
        (defun gregorian-year-from-fixed (date)
          ;; TYPE fixed-date -> gregorian-year
          ;; Gregorian year corresponding to the fixed date.
          (let* ((d0             ; Prior days.
                  (- date gregorian-epoch))
                 (n400           ; Completed 400-year cycles.
                  (quotient d0 146097))
                 (d1             ; Prior days not in n400.
                  (mod d0 146097))
                 (n100           ; 100-year cycles not in n400.
                  (quotient d1 36524))
                 (d2             ; Prior days not in n400 or n100.
                  (mod d1 36524))
                 (n4             ; 4-year cycles not in n400 or n100.
                  (quotient d2 1461))
                 (d3             ; Prior days not in n400, n100, or n4.
                  (mod d2 1461))
                 (n1             ; Years not in n400, n100, or n4.
                  (quotient d3 365))
                 (year (+ (* 400 n400) (* 100 n100) (* 4 n4) n1)))
            (if (or (= n100 4) (= n1 4))
                year             ; Date is day 366 in a leap year.
              (1+ year))))       ; Date is ordinal day (1+ (mod d3 365))
                                 ; in (1+ year).
        """
        d0 = date - self.GREGORIAN_EPOCH
        n400 = self.QUOTIENT(d0, 146097)
        d1 = d0 % 146097
        n100 = self.QUOTIENT(d1, 36524)
        d2 = d1 % 36524
        n4 = self.QUOTIENT(d2, 1461)
        d3 = d2 % 1461
        n1 = self.QUOTIENT(d3, 365)
        year = (400 * n400) + (100 * n100) + (4 * n4) + n1
        return year if n100 == 4 or n1 == 4 else year + 1

    def gregorian_from_fixed(self, day:float) -> tuple:
        """
        (defun gregorian-from-fixed (date)
          ;; TYPE fixed-date -> gregorian-date
          ;; Gregorian (year month day) corresponding to fixed date.
          (let* ((year (gregorian-year-from-fixed date))
                 (prior-days          ; This year
                  (- date (gregorian-new-year year)))
                 (correction          ; To simulate a 30-day Feb
                  (if (< date (fixed-from-gregorian
                               (gregorian-date year march 1)))
                      0
                    (if (gregorian-leap-year? year)
                        1
                      2)))
                 (month               ; Assuming a 30-day Feb
                  (quotient
                   (+ (* 12 (+ prior-days correction)) 373)
                   367))
                 (day                 ; Calculate the day by subtraction.
                  (1+ (- date
                         (fixed-from-gregorian
                          (gregorian-date year month 1))))))
            (gregorian-date year month day)))
        """
        year = self.gregorian_year_from_fixed(day)
        prior_days = day - self.gregorian_new_year(year)
        fixed = self.fixed_from_gregorian((year, self.MARCH, 1))

        if not (day < fixed):
            if self.GREGORIAN_LEAP_YEAR(year):
                prior_days += 1
            else:
                prior_days += 2

        month = self.QUOTIENT(12 * prior_days + 373, 367)
        day = day - self.fixed_from_gregorian((year, month, 1)) + 1
        return (year, month, day)

    def gregorian_date_difference(self, g_date1:tuple, g_date2:tuple) -> int:
        """
        used

        (defun gregorian-date-difference (g-date1 g-date2)
          ;; TYPE (gregorian-date gregorian-date) -> integer
          ;; Number of days from Gregorian date g-date1 until g-date2.
          (- (fixed-from-gregorian g-date2)
             (fixed-from-gregorian g-date1)))
        """
        self._check_valid_gregorian_month_day(g_date1)
        self._check_valid_gregorian_month_day(g_date2)
        return (self.fixed_from_gregorian(g_date1) -
                self.fixed_from_gregorian(g_date2))

    #days-remaining
    #last-day-of-gregorian-month

    def alt_fixed_from_gregorian(self, g_date:tuple) -> int:
        """
        (defun alt-fixed-from-gregorian (g-date)
          ;; TYPE gregorian-date -> fixed-date
          ;; Alternative calculation of fixed date equivalent to the
          ;; Gregorian date g-date.
          (let* ((month (standard-month g-date))
                 (day (standard-day g-date))
                 (year (standard-year g-date))
                 (m-prime (mod (- month 3) 12))
                 (y-prime (- year (quotient m-prime 10))))
            (+ (1- gregorian-epoch)
               -306              ; Days in March...December.
               (* 365 y-prime)   ; Ordinary days.
               (sigma ((y (to-radix y-prime (list 4 25 4)))
                       (a (list 97 24 1 0)))
                      (* y a))
               (quotient         ; Days in prior months.
                (+ (* 3 m-prime) 2)
                5)
               (* 30 m-prime)
               day)))            ; Days so far this month.
        """
        year = g_date[0]
        month = g_date[1]
        day = g_date[2]
        m_prime = (month - 3) % 12
        y_prime = year - self.QUOTIENT(m_prime, 10)
        func = lambda y, a: y * a
        return (self.GREGORIAN_EPOCH - 1 - 306 + 365 * y_prime +
                self._sigma(
                    (self._to_radix(y_prime, (4, 25, 4)), (97, 24, 1, 0)),
                    func) + self.QUOTIENT(3 * m_prime + 2, 5) +
                30 * m_prime + day)

    def alt_gregorian_from_fixed(self, date):
        """
        defun alt-gregorian-from-fixed (date)
         ;; TYPE fixed-date -> gregorian-date
         ;; Alternative calculation of Gregorian (year month day)
         ;; corresponding to fixed $date$.
         (let* ((y (gregorian-year-from-fixed
                    (+ (1- gregorian-epoch)
                       date
                       306)))
                (prior-days
                 (- date (fixed-from-gregorian
                          (gregorian-date (1- y) march 1))))
                (month
                 (amod (+ (quotient
                           (+ (* 5 prior-days) 2)
                           153)
                          3)
                       12))
                (year (- y (quotient (+ month 9) 12)))
                (day
                 (1+ (- date
                        (fixed-from-gregorian
                         (gregorian-date year month 1))))))
           (gregorian-date year month day)))
        """
        y = self.gregorian_year_from_fixed(
            self.GREGORIAN_EPOCH - 1 + date + 306)
        prior_days = date - self.fixed_from_gregorian((y - 1, self.MARCH, 1))
        month = self.AMOD(self.QUOTIENT(5 * prior_days + 2, 153) + 3, 12)
        year = y - self.QUOTIENT(month + 9, 12)
        day = date - self.fixed_from_gregorian((year, month, 1)) + 1
        return (year, month, day)

    def alt_gregorian_year_from_fixed(self, date):
        """
        (defun alt-gregorian-year-from-fixed (date)
          ;; TYPE fixed-date -> gregorian-year
          ;; Gregorian year corresponding to the fixed $date$.
          (let* ((approx        ; approximate year
                 (quotient (- date gregorian-epoch -2)
                           146097/400))
                 (start         ; start of next year
                  (+ gregorian-epoch
                     (* 365 approx)
                     (sigma ((y (to-radix approx (list 4 25 4)))
                             (a (list 97 24 1 0)))
                            (* y a)))))
            (if (< date start)
                approx
              (1+ approx))))
        """
        approx = self.QUOTIENT(date - self.GREGORIAN_EPOCH + 2, 365.2425)
        func = lambda y, a: y * a
        start = self.GREGORIAN_EPOCH + 365 * approx + self._sigma(
            (self._to_radix(approx, (4, 25, 4)), (97, 24, 1, 0)), func)
        return approx if date < start else approx + 1

    #
    # Methods from other places mostly from Jean Meeus.
    #

    def alt_jd_from_gregorian_date(self, g_date):
        # Julian day calculation formula
        year = g_date[0]
        month = g_date[1]
        day = g_date[2]
        hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
        minute = g_date[4] if t_len > 4 and g_date[4] is not None else 0
        second = g_date[5] if t_len > 5 and g_date[5] is not None else 0
        return (367 * year - 7 * (year + (month + 9) // 12) // 4 + 275 *
                month // 9 + day + 1721013.5 + (hour + minute / 60 +
                                                second / 3600) / 24)

    def alt_gregorian_date_from_je(self, jd):
        # Calculate year
        Y = math.floor((jd - 1721013.5) / 367)
        # Calculate day of year
        Z = jd - 1721013.5 - (365 * Y + (Y + 3) // 4)
        # Calculate month
        W = math.floor(Z / 30.6)
        # Calculate day
        D = math.floor(Z - (30.6 * W))

        # Adjust month and year
        if W < 14:
            month = W - 1
        else:
            month = W - 13
            Y += 1

        return Y, month, D

    #def julian_to_gregorian_year(self, jd):
    #    # Julian day calculation formula
    #    return math.floor((jd - 1721013.5) / 367)

    def jd_from_gregorian_date(self, g_date:tuple) -> float:
        """
        Convert Gregorian dates to Julian day count with the 1582 10, 15
        correction.

        .. note::

           See Astronomical Formulae for Calculators Enlarged & Revised,
           by Jean Meeus ch3 p23-25
           See: https://core2.gsfc.nasa.gov/time/julian.html
        """
        t_len = len(g_date)
        year = g_date[0]
        month = g_date[1]
        day = g_date[2]
        hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
        minute = g_date[4] if t_len > 4 and g_date[4] is not None else 0
        second = g_date[5] if t_len > 5 and g_date[5] is not None else 0
        self._check_valid_gregorian_month_day(g_date)

        a = day % 1

        if a:
            hour = a * 24
            b = hour % 1
            hour = math.floor(hour)
            minute = b * 60
            c = minute % 1
            minute = math.floor(minute)
            second = c * 60

        if month <= 2:
            year -= 1
            month += 12

        h = self.HR(hour) + self.MN(minute) + self.SEC(second)
        day += h if h > 0 else 0
        jd =  (math.floor(365.25 * year) + math.floor(30.6001 * (month + 1)) +
               day + 1720994.5)
        print(jd)

        if g_date >= (1582, 10, 15):
            a = math.floor(year / 100)
            b = 2 - a + math.floor(a / 4)
            jd += b

        return jd

    def gregorian_date_from_jd(self, jd:float) -> tuple:
        """
        Convert Julian day to Gregorian date.

        .. note::

           See Astronomical Formulae for Calculators Enlarged & Revised,
           by Jean Meeus ch3 p26-29
        """
        j_day = jd + 0.5
        z = math.floor(j_day)
        f = j_day % 1

        if z >= 2299161:
            alpha = math.floor((z - 1867216.25) / 36524.25)
            a = z + 1 + alpha - math.floor(alpha / 4)
        else:
            a = z

        b = a + 1524
        c = math.floor((b - 122.1) / 365.25)
        d = math.floor(365.25 * c)
        e = math.floor((b - d) / 30.6001)
        day = b - d - math.floor(30.6001 * e) + f
        month = 0
        year = 0

        if e < 14:
            month = e - 1
        elif e in (14, 15):
            month = e - 13

        if month > 2:
            year = c - 4716
        elif month in (1, 2):
            year = c - 4715

        return year, month, day

    def gregorian_year_from_jd(self, jde:float) -> int:
        """
        Find the Gregorian year from a Julian Period day.
        """
        return self.gregorian_date_from_jd(jde)[0]

    def date_from_ymdhms(self, date:tuple) -> tuple:
        """
        Convert (year, month, day, hour, minute, second) into a
        (year, month, day.partial) date.
        """
        self._check_valid_gregorian_month_day(date)
        t_len = len(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hour = date[3] if t_len > 3 and date[3] is not None else 0
        minute = date[4] if t_len > 4 and date[4] is not None else 0
        second = date[5] if t_len > 5 and date[5] is not None else 0
        day += self.HR(hour) + self.MN(minute) + self.SEC(second)
        return (year, month, day)

    def ymdhms_from_date(self, date:tuple) -> tuple:
        """
        Convert (year, month, day.partial) into a
        (year, month, day, hour, minute, second).
        """
        self._check_valid_gregorian_month_day(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hd = self.PARTIAL_DAY_TO_HOURS(day)
        hour = math.floor(hd)
        md = self.PARTIAL_HOUR_TO_MINUTE(hd)
        minute = math.floor(md)
        second = self.PARTIAL_MINUTE_TO_SECOND(md)
        return (year, month, math.floor(day), hour, minute, second)

    def _check_valid_gregorian_month_day(self, g_date:tuple) -> bool:
        """
        Check that the monmth and day values are valid.
        """
        t_len = len(g_date)
        year = g_date[0]
        month = abs(g_date[1])
        day = abs(g_date[2])
        hour = g_date[3] if t_len > 3 and g_date[3] is not None else 0
        minute = g_date[4] if t_len > 4 and g_date[4] is not None else 0
        second = g_date[5] if t_len > 5 and g_date[5] is not None else 0
        assert 1 <= month <= 12, f"Invalid month '{month}', should be 1 - 12."
        days = self._MONTHS[month - 1]

        if month == 2: # Subtract 0 or 1 from Febuary if leap year.
            days -= 0 if self.GREGORIAN_LEAP_YEAR(year) else 1

        assert 1 <= day <= days, (
            f"Invalid day '{day}' for month '{month}' and year '{year}' "
            f"should be 1 - {days}.")
        assert hour < 24, f"Invalid hour '{hour}' it must be < 24"
        assert minute < 60, f"Invalid minute '{minute}' should be < 60."

        if any((hour, minute, second)):
            assert not day % 1, ("If there is a part day then there can be no "
                                 "hours, minutes, or seconds.")

        if any((minute, second)):
            assert not hour % 1, (
                "If there is a part hour then there can be no minutes or "
                "seconds.")

        if second:
            assert not minute % 1, (
                "If there is a part minute then there can be no seconds.")
