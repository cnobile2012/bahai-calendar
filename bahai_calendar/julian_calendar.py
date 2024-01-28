# -*- coding: utf-8 -*-
#
# bahai_calendar/julian_calendar.py
#
__docformat__ = "restructuredtext en"

import math

from bahai_calendar.base_calendar import BaseCalendar


class JulianCalendar(BaseCalendar):
    """
    Implementation of the Calendar.
    """
    #(defconstant julian-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of the Julian calendar.
    #  (fixed-from-gregorian (gregorian-date 0 december 30)))
    JULIAN_EPOCH = -1

    def __init__(self):
        super().__init__()
        self._julian_date = None

    def julian_leap_year(self, j_year):
        """
        (defun julian-leap-year? (j-year)
          ;; TYPE julian-year -> boolean
          ;; True if $j-year$ is a leap year on the Julian calendar.
          (= (mod j-year 4) (if (> j-year 0) 0 3)))
        """
        return (j_year % 4) == 0 if j_year > 0 else 3

    def fixed_from_julian(self, j_date):
        """
        (defun fixed-from-julian (j-date)
          ;; TYPE julian-date -> fixed-date
          ;; Fixed date equivalent to the Julian date $j-date$.
          (let* ((month (standard-month j-date))
                 (day (standard-day j-date))
                 (year (standard-year j-date))
                 (y (if (< year 0)
                        (1+ year)     ; No year zero
                      year)))
            (+ (1- julian-epoch)      ; Days before start of calendar
               (* 365 (1- y))         ; Ordinary days since epoch.
               (quotient (1- y) 4)    ; Leap days since epoch...
               (quotient              ; Days in prior months this year...
                (- (* 367 month) 362) ; ...assuming 30-day Feb
                12)
               (if (<= month 2)       ; Correct for 28- or 29-day Feb
                   0
                 (if (julian-leap-year? year)
                     -1
                   -2))
               day)))                 ; Days so far this month.
        """
        year = j_date[0]
        month = j_date[1]
        day = j_date[2]
        y = year + 1 if year < 0 else year
        correction = 0

        if month > 2:
            if self.julian_leap_year(year):
                correction = -1
            else:
                correction = -2

        return (self.JULIAN_EPOCH - 1) + 365 * (y - 1) + self.QUOTIENT(
            y - 1, 4) + self.QUOTIENT(367 * month- 362, 12) + correction + day

    def julian_from_fixed(self, date):
        """
        (defun julian-from-fixed (date)
          ;; TYPE fixed-date -> julian-date
          ;; Julian (year month day) corresponding to fixed $date$.
          (let* ((approx               ; Nominal year.
                  (quotient (+ (* 4 (- date julian-epoch)) 1464)
                            1461))
                 (year (if (<= approx 0)
                           (1- approx) ; No year 0.
                         approx))
                 (prior-days           ; This year
                  (- date (fixed-from-julian
                           (julian-date year january 1))))
                 (correction           ; To simulate a 30-day Feb
                  (if (< date (fixed-from-julian
                               (julian-date year march 1)))
                      0
                    (if (julian-leap-year? year)
                        1
                      2)))
                 (month                ; Assuming a 30-day Feb
                  (quotient
                   (+ (* 12 (+ prior-days correction)) 373)
                   367))
                 (day                  ; Calculate the day by subtraction.
                  (1+ (- date
                         (fixed-from-julian
                          (julian-date year month 1))))))
            (julian-date year month day)))
        """
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()
        approx = self.QUOTIENT(4 * (date - self.JULIAN_EPOCH) + 1464, 1461)
        year = approx - 1 if approx <= 0 else approx
        prior_day = date - self.fixed_from_julian((year, gc.JANUARY, 1))
        correction = 0

        if date >= self.fixed_from_julian((year, gc.MARCH, 1)):
            if self.julian_leap_year(year):
                correction = 1
            else:
                correction = 2

        month = self.QUOTIENT(12 * (prior_day + correction) + 373, 367)
        day = (date - self.fixed_from_julian((year, month, 1))) + 1
        return (year, month, day)
