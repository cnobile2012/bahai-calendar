# -*- coding: utf-8 -*-
#
# bahai_calendar/julian_period.py
#
__docformat__ = "restructuredtext en"

import math


class JulianPeriod:
    """
    Implementation of the Julian Period.

    This Julian period is said to be named after Julius Cæsar Scaliger,
    the father of Josephus Justus Scaliger (Born: August 5, 1540, Agen,
    France), who invented the concept. It has nothing to do with the
    Julian Calendar introduced by Julius Caesar.

    See https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html
    See https://www.tondering.dk/claus/cal/julperiod.php
    """
    #(defconstant j2000
    #  ;; TYPE moment
    #  ;; Noon at start of Gregorian year 2000.
    #  (+ (hr 12L0) (gregorian-new-year 2000)))
    # https://aa.usno.navy.mil/faq/sun_approx 2451545.0
    J2000 = 730120.5

    #(defconstant julian-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of the Julian calendar.
    #  (fixed-from-gregorian (gregorian-date 0 december 30)))
    JULIAN_EPOCH = -1
    JULIAN_PERIOD = 7980
    JULIAN_CENTURY = 100
    JULIAN_YEAR = 365.25
    JD_EPOCH = -1721424.5
    MJD_EPOCH = 678576

    def __init__(self):
        super().__init__()
        # (period, century, year, month, day)
        self._julian_date = None

    def julian_period(self, jd):
        c = jd / self.JULIAN_YEAR
        period = math.floor(c / self.JULIAN_PERIOD) + 1
        century = math.floor(period * c) + 1
        year = None #math.floor()
        month = None
        day = None
        return (period, century, year, month, day)

    def moment_from_jd(self, jd:float) -> float:
        """
        (defun moment-from-jd (jd)
          ;; TYPE julian-day-number -> moment
          ;; Moment of julian day number jd.
          (+ jd jd-epoch))
        """
        return jd + self.JD_EPOCH

    def jd_from_moment(self, t:float) -> float:
        """
        (defun jd-from-moment (tee)
          ;; TYPE moment -> julian-day-number
          ;; Julian day number of moment tee.
          (- tee jd-epoch))
        """
        return t - self.JD_EPOCH

    def fixed_from_mjd(self, mjd:float) -> float:
        """
        (defun fixed-from-mjd (mjd)
          ;; TYPE julian-day-number -> fixed-date
          ;; Fixed date of modified julian day number mjd.
          (+ mjd mjd-epoch))
        """
        return mjd + self.MJD_EPOCH

    def mjd_from_fixed(self, date:float) -> float:
        """
        (defun mjd-from-fixed (date)
          ;; TYPE fixed-date -> julian-day-number
          ;; Modified julian day number of fixed date.
          (- date mjd-epoch))
        """
        return date - self.MJD_EPOCH

    def fixed_from_moment(self, t):
        """
        (defun fixed-from-moment (tee)
          ;; TYPE moment -> fixed-date
          ;; Fixed-date from moment tee.
          (floor tee))
        """
        return math.floor(t)

    def fixed_from_jd(self, jd):
        """
        (defun fixed-from-jd (jd)
          ;; TYPE julian-day-number -> fixed-date
          ;; Fixed date of julian day number jd.
          (floor (moment-from-jd jd)))
        """
        return math.floor(self.moment_from_jd(jd))

    def jd_from_fixed(self, date):
        """
        (defun jd-from-fixed (date)
          ;; TYPE fixed-date -> julian-day-number
          ;; Julian day number of fixed date.
          (jd-from-moment date))
        """
        return self.jd_from_moment(date)

    def time_from_moment(self, t):
        """
        (defun time-from-moment (tee)
          ;; TYPE moment -> time
          ;; Time from moment tee.
          (mod tee 1))
        """
        return t % 1

    def julian_centuries(self, tee):
        """
        used

        (defun julian-centuries (tee)
          ;; TYPE moment -> century
          ;; Julian centuries since 2000 at moment tee.
          (/ (- (dynamical-from-universal tee) j2000) 36525))
        """
        return (self.dynamical_from_universal(tee) - self.J2000) / 36525

    def julian_leap_year(self, j_year:float) -> bool:
        """
        (defun julian-leap-year? (j-year)
          ;; TYPE julian-year -> boolean
          ;; True if $j-year$ is a leap year on the Julian calendar.
          (= (mod j-year 4) (if (> j-year 0) 0 3)))
        """
        return (j_year % 4) == 0 if j_year > 0 else 3

    def fixed_from_julian(self, j_date:tuple) -> float:
        """
        To be tested with (-4713, 1, 1.5) -> -1721424.5

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

    def julian_from_fixed(self, date:tuple) -> float:
        """
        To be tested with -1721424.5 -> (-4713, 1, 1.5)

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

    def julian_from_gregorian(self, g_date:tuple) -> int:
        """
        Convert Gregorian dates to Julian day count.
        https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html
        """
        year = g_date[0]
        month = g_date[1]
        day = g_date[2]
        a = math.floor(year / 100)
        b = math.floor(a / 4)
        c = 2 - a + b
        e = math.floor(365.25 * (year + 4716))
        f = math.floor(30.6001 * (month + 1))
        return c + day + e + f - 1524.5

    def gregorian_from_julian(self, jd:float) -> tuple:
        """
        Convert Julian day to Gregorian date.
        """
        q = jd + 0.5
        z = math.floor(q)
        w = math.floor((z - 1867216.25) / 36524.25)
        x = math.floor(w / 4)
        a = z + 1 + w - x
        b = a + 1524
        c = math.floor((b - 122.1) / 365.25)
        d = math.floor(365.25 * c)
        e = math.floor((b - d) / 30.6001)
        f = math.floor(30.6001 * e)
        day = int(b - d - f + ( q - z))
        month = e - 1 if e <= 12 else e - 13
        year = c - 4715 if month in (1, 2) else c - 4716
        return (year, month, day)

    def fixed_from_julian_day(self, jd:float) -> float:
        """
        Convert Julian day to fixed moment.

        A.D. 2024 January 31	00:00:00.0	2460340.500000
        A.D. 2024 January 31	12:00:00.0	2460341.000000
        See https://aa.usno.navy.mil/data/JulianDate
        """
        centuries = jd / 36525
        


