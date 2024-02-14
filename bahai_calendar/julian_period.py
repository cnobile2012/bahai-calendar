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
    France), who invented the concept. The Julian Period is related to
    the Julian Calendar introduced by Julius Caesar, but it is not the
    same.

    See https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html
    See https://www.tondering.dk/claus/cal/julperiod.php
    """
    #(defconstant j2000
    #  ;; TYPE moment
    #  ;; Noon at start of Gregorian year 2000.
    #  (+ (hr 12L0) (gregorian-new-year 2000)))
    RD_J2000 = 730120.5

    # January 1, 2000, at 12:00 TT (Terrestrial Time)
    # TAI = TT - 32.184 seconds
    # January 1, 2000, 11:59:27.816 TAI (International Atomic Time)
    # January 1, 2000, 11:58:55.816 UTC (Coordinated Universal Time)
    # See: https://aa.usno.navy.mil/faq/sun_approx
    J2000 = 2451545.0

    #(defconstant julian-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of the Julian calendar.
    #  (fixed-from-gregorian (gregorian-date 0 december 30)))
    # The above seems to be off by one day.
    JULIAN_EPOCH = 0 # -1

    # 28 (solar cycle) × 19 (lunar cycle) × 15 (indiction cycle) = 7980 years
    JULIAN_PERIOD = 7980
    JULIAN_YEAR = 365.25
    JD_EPOCH = -1721424.5
    MJD_EPOCH = 678576
    JULIAN_MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    _CORRECT = lambda self, x: 0.75 if x % 1 in (0.25, 0.75) else 0

    def __init__(self):
        super().__init__()
        # (period, year, month, day)
        self._julian_date = None

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
        return (self.dynamical_from_universal(tee) - self.RD_J2000) / 36525

    def julian_leap_year(self, j_year:float) -> bool:
        """
        (defun julian-leap-year? (j-year)
          ;; TYPE julian-year -> boolean
          ;; True if $j-year$ is a leap year on the Julian calendar.
          (= (mod j-year 4) (if (> j-year 0) 0 3)))
        """
        #return (j_year % 4) == 0 if j_year > 0 else 3
        return not j_year % 4

    def fixed_from_julian(self, j_date:tuple) -> float:
        """
        This function seems to not return an R.D. fixed day, but returns
        a Julian day.

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
        period = j_date[0] # ** TODO ** Do something with this.
        year = j_date[1]
        month = j_date[2]
        day = j_date[3]
        y = year + 1 if year < 0 else year
        correction = 0

        if month > 2:
            if self.julian_leap_year(year):
                correction = -1
            else:
                correction = -2

        return (self.JULIAN_EPOCH - 1) + 365 * (y - 1) + self.QUOTIENT(
            y - 1, 4) + self.QUOTIENT(367 * month - 362, 12) + correction + day

    def julian_period_from_julian_day(self, jd:float) -> tuple:
        """
        Convert the Julian day to a Julian date.

        .. note::

           The Julian Period is 13 days behind the Gregorian Calendar.
        """
        assert jd >= 0, f"A Julian Period day must be 'jd >= 0', found {jd}"
        jd_start = math.floor(jd)
        jd_part = jd % 1
        c = jd_start / self.JULIAN_YEAR
        period = math.floor(c / self.JULIAN_PERIOD) + 1
        year = math.floor(c) + 1
        days_remaining = jd_start - (year - 1) * self.JULIAN_YEAR
        accumulated_days = 0

        for month, md in enumerate(self.JULIAN_MONTHS, start=1):
            if month == 2: # Subtract 0 or 1 from Febuary if leap year.
                md -= 0 if self.julian_leap_year(jd) else 1

            accumulated_days += md

            if days_remaining < accumulated_days:
                accumulated_days -= md
                break
            elif days_remaining == accumulated_days:
                accumulated_days -= md - 1
                break

        #print(f"({jd}, {month}, {md}, {days_remaining}, "
        #      f"{accumulated_days})")
        # ** TODO ** Since the leap year is every 4 years we are accumulating
        #            0.25 on a day per year before the next leap year happens.
        # These 0.25 needs to be subtracted.
        #print(days_remaining, accumulated_days)

        day = days_remaining - accumulated_days + 1 + jd_part
        day -= self._CORRECT(day)
        return (period, year, month, day)

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
        prior_day = date - self.fixed_from_julian((1, year, gc.JANUARY, 1))
        correction = 0

        if date >= self.fixed_from_julian((1, year, gc.MARCH, 1)):
            if self.julian_leap_year(year):
                correction = 1
            else:
                correction = 2

        month = self.QUOTIENT(12 * (prior_day + correction) + 373, 367)
        day = (date - self.fixed_from_julian((1, year, month, 1))) + 1
        return (1, year, month, day) # ** TODO ** Do something with the period.

    def julian_from_gregorian(self, g_date:tuple) -> float:
        """
        Convert Gregorian dates to Julian day count with the 1582 10, 15
        correction.

        .. note::

          See Astronomical Formulae for Calculators Enlarged & Revised,
          by Jean Meeus
        """
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()
        year = g_date[0]
        month = abs(g_date[1])
        day = abs(g_date[2])
        gc._check_valid_gregorian_month_day(g_date)

        if month > 2:
            y = year
            m = month
        elif month <= 2:
            y = year - 1
            m = month + 12

        a = int(365.25 * y - (0 if y >= 0 else 0.75))
        jd = a + int(30.6001 * (m + 1)) + day + 1720994.5

        if g_date >= (1582, 10, 15):
            b = int(y / 100)
            c = 2 - b + int(b / 4)
            jd += c

        return jd

    def gregorian_from_julian(self, jd:float) -> tuple:
        """
        Convert Julian day to Gregorian date.
        """
        a = jd + 0.5
        z = int(a)
        f = a % 1

        if z < 2299161:
            a = z
        else:
            alpha = int((z - 1867216.25) / 36524.25)
            a = z + 1 + alpha - int(alpha / 4)

        b = a + 1524
        c = int((b - 122.1) / 365.25)
        d = int(365.25 * c)
        e = int((b - d) / 30.6001)
        day = b - d - int(30.6001 * e) + f
        month = 0
        year = 0

        if e < 13.5:
            month = e - 1
        elif e > 13.5:
            month = e - 13

        if month > 2.5:
            year = c - 4716
        elif month < 2.5:
            year = c - 4715

        return (year, month, day)

    ## def fixed_from_julian_day(self, jd:float) -> float:
    ##     """
    ##     Convert Julian day to fixed moment.

    ##     A.D. 2024 January 31	00:00:00.0	2460340.500000
    ##     A.D. 2024 January 31	12:00:00.0	2460341.000000
    ##     See https://aa.usno.navy.mil/data/JulianDate
    ##     """
    ##     date = self.julian_period_from_julian_day(jd)
    ##     return self.fixed_from_julian(date)

    ## def julian_day_from_fixed(self, tee):
    ##     """
    ##     Convert a fixed day to a Julian day.
    ##     """

