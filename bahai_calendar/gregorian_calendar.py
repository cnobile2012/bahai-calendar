# -*- coding: utf-8 -*-
#
# bahai_calendar/gregorian_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from .base_calendar import BaseCalender


class GregorianCalendar(BaseCalender):
    """
    Implementation of the Gregorian Calendar.
    """
    GREGORIAN_EPOCH = 1

    # Build out some lambdas
    # (defun rd (tee)
    #  ;; TYPE moment -> moment
    #  ;; Identity function for fixed dates/moments. If internal
    #  ;; timekeeping is shifted, change epoch to be RD date of
    #  ;; origin of internal count. epoch should be an integer.
    #  (let* ((epoch 0))
    #    (- tee epoch)))
    # Nothing is implemented for this.

    # (defun quotient (m n)
    #   ;; TYPE (real nonzero-real) -> integer
    #   ;; Whole part of m/n.
    #   (floor m n))
    QUOTIENT = lambda self, m, n: int(math.floor(m / n))

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


"""
(defun standard-day (date)
  ;; TYPE standard-date -> standard-day
  ;; Day field of date = (year month day).
  (third date))

(defun standard-year (date)
  ;; TYPE standard-date -> standard-year
  ;; Year field of date = (year month day).
  (first date))

(defun time-of-day (hour minute second)
  ;; TYPE (hour minute second) -> clock-time
  (list hour minute second))
"""




    def gregorian_year_from_fixed(self, date):
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
                 (year (+ (* 400 n400)
                          (* 100 n100)
                          (* 4 n4)
                          n1)))
            (if (or (= n100 4) (= n1 4))
                year             ; Date is day 366 in a leap year.
              (1+ year)))); Date is ordinal day (1+ (mod d3 365))
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
        return year if n100 == 4 or n1 == 1 else year + 1

    def fixed_from_gregorian(self, g_date):
        """
        (defun fixed-from-gregorian (g-date)
          ;; TYPE gregorian-date -> fixed-date
          ;; Fixed date equivalent to the Gregorian date g-date.
          (let* ((month (standard-month g-date))
                 (day (standard-day g-date))
                 (year (standard-year g-date)))
            (+ (1- gregorian-epoch); Days before start of calendar
               (* 365 (1- year)) ; Ordinary days since epoch
               (quotient (1- year)
                         4)      ; Julian leap days since epoch...
               (-                ; ...minus century years since epoch...
                (quotient (1- year) 100))
               (quotient         ; ...plus years since epoch divisible...
                (1- year) 400)   ; ...by 400.
               (quotient         ; Days in prior months this year...
                (- (* 367 month) 362) ; ...assuming 30-day Feb
                12)
               (if (<= month 2)  ; Correct for 28- or 29-day Feb
                   0
                 (if (gregorian-leap-year? year)
                     -1
                   -2))
               day)))           ; Days so far this month.
        """
        #month = 
        return

    def standard_month(self, date):
        """
        (defun standard-month (date)
          ;; TYPE standard-date -> standard-month
          ;; Month field of date = (year month day).
          (second date))
        """
        return 

