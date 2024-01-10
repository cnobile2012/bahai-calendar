# -*- coding: utf-8 -*-
#
# bahai_calendar/gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from bahai_calendar.base_calendar import BaseCalender


class GregorianCalendar(BaseCalender):
    """
    Implementation of the Gregorian Calendar.
    """
    #(defconstant gregorian-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of the (proleptic) Gregorian calendar.
    #  (rd 1))
    GREGORIAN_EPOCH = 1  # See BaseCalender notes.

    #(defconstant march
    #  ;; TYPE standard-month
    #  ;; March on Julian/Gregorian calendar.
    #  3)
    JANUARY = 1
    FEBRUARY = 2
    MARCH = 3
    APRIL = 4
    MAY = 5
    JUNE = 6
    JULY = 7
    AUGUEST = 8
    SEPTEMBER = 9
    OCTOBER = 10
    NOVEMBER = 11
    DECEMBER = 12
    # ((MOD(year, 4) = 0) * ((MOD(year, 100) <> 0) + (MOD(year, 400) = 0)) = 1)
    GREGORIAN_LEAP_YEAR = lambda self, year: (
        (year % 4 == 0) * ((year % 100 != 0) + (year % 400 == 0)) == 1)

    def __init__(self):
        super().__init__()
        self._date = []

    def parse_datetime(self, dt:datetime.datetime):
        self.gregorian_date(dt.year, dt.month, dt.day)
        self.time_of_day(dt.hour, dt.minute, dt.second)

    def gregorian_date(self, year, month, day):
        """
        (defun gregorian-date (year month day)
          ;; TYPE (gregorian-year gregorian-month gregorian-day)
          ;; TYPE -> gregorian-date
          (list year month day))
        """
        self._date[:] = (year, month, day)
        return self._date

    @property
    def standard_year(self):
        """
        (defun standard-year (date)
          ;; TYPE standard-date -> standard-year
          ;; Year field of date = (year month day).
          (first date))
        """
        return self._date[0]

    @property
    def standard_month(self):
        """
        (defun standard-month (date)
          ;; TYPE standard-date -> standard-month
          ;; Month field of date = (year month day).
          (second date))
        """
        return self._date[1]

    @property
    def standard_day(self):
        """
        (defun standard-day (date)
          ;; TYPE standard-date -> standard-day
          ;; Day field of date = (year month day).
          (third date))
        """
        return self._date[2]

    def gregorian_year_from_fixed(self, date:int) -> int:
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

    def fixed_from_gregorian(self, g_date:tuple) -> int:
        """
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
        year = g_date[0]
        month = g_date[1]
        day = g_date[2]
        year_1 = year - 1
        result = (self.GREGORIAN_EPOCH - 1 + 365 * year_1 +
                  self.QUOTIENT(year_1, 4) - self.QUOTIENT(year_1, 100) +
                  self.QUOTIENT(year_1, 400) +
                  self.QUOTIENT(367 * month - 362, 12))

        if month <= 2:
            if self.GREGORIAN_LEAP_YEAR(year):
                result -= 1
            else:
                result -= 2

        return result + day

    ## def gregorian_leap_year(self, g_year):
    ##     """
    ##     (defun gregorian-leap-year? (g-year)
    ##       ;; TYPE gregorian-year -> boolean
    ##       ;; True if g-year is a leap year on the Gregorian
    ##       ;; calendar.
    ##       (and (= (mod g-year 4) 0)
    ##            (not (member (mod g-year 400)
    ##                         (list 100 200 300)))))
    ##     """
    ##     return (g_year % 4) == 0 and ((g_year % 400) in (100, 200, 300))
