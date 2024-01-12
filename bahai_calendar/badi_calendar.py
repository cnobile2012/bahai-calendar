# -*- coding: utf-8 -*-
#
# bahai_calendar/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from bahai_calendar.base_calendar import BaseCalender
from bahai_calendar.gregorian_calendar import GregorianCalendar


class BahaiCalendar(BaseCalender):
    """
    Implementation of the Baha'i (Badi) Calendar.
    """
    #(defconstant bahai-location
    #  ;; TYPE location
    #  ;; Location of Tehran for astronomical Baha’i calendar.
    #  (location (deg 35.696111L0) (deg 51.423056L0)
    #   (mt 0) (hr (+ 3 1/2))))
    BAHAI_LOCATION = (35.696111, 51.423056, 0, 3.5)

    #(defconstant ayyam-i-ha
    #  ;; TYPE bahai-month
    #  ;; Signifies intercalary period of 4 or 5 days.
    #  0)
    AYYAM_I_HA = 0

    #(defconstant bahai-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of Baha’i calendar.
    #  (fixed-from-gregorian (gregorian-date 1844 march 21)))
    BAHAI_EPOCH = 673222

    def __init__(self):
        super().__init__()
        # Baha'i date: [major, cycle, year, month, day]
        self._bahai_date = []
        self._gc = GregorianCalendar()

    def bahai_date(self, major, cycle, year, month, day):
        """
        (defun bahai-date (major cycle year month day)
          ;; TYPE (bahai-major bahai-cycle bahai-year
          ;; TYPE bahai-month bahai-day) -> bahai-date
          (list major cycle year month day))
        """
        self._bahai_date[:] = (major, cycle, year, month, day)
        return self._bahai_date

    def astro_bahai_from_fixed(self, ):
        """
        (defun astro-bahai-from-fixed (date)
          ;; TYPE fixed-date -> bahai-date
          ;; Astronomical Baha’i date corresponding to fixed date.
          (let* ((new-year (astro-bahai-new-year-on-or-before date))
                 (years (round (/ (- new-year bahai-epoch)
                                  mean-tropical-year)))
                 (major (1+ (quotient years 361)))
                 (cycle (1+ (quotient (mod years 361) 19)))
                 (year (1+ (mod years 19)))
                 (days        ; Since start of year
                  (- date new-year))
                 (month
                  (cond
                   ((>= date (fixed-from-astro-bahai
                              (bahai-date major cycle year 19 1)))
                              ; last month of year
                    19)
                   ((>= date
                        (fixed-from-astro-bahai
                         (bahai-date major cycle year ayyam-i-ha 1)))
                              ; intercalary month
                    ayyam-i-ha)
                   (t (1+ (quotient days 19)))))
                 (day (- date -1
                         (fixed-from-astro-bahai
                          (bahai-date major cycle year month 1)))))
            (bahai-date major cycle year month day)))
        """
        new_year = self.astro_bahai_new_year_on_or_before(date)
        years = (new_year - self.BAHAI_EPOCH) / self.MEAN_TROPICAL_YEAR
        major = self.QUOTIENT(years, 361) + 1
        cycle = self.QUOTIENT(years % 361, 19) + 1
        year = (years % 19) + 1
        days = date - new_year

        if date >= self.fixed_from_astro_bahai((major, cycle, year, 19, 1)):
            month = 19
        elif date >= self.fixed_from_astro_bahai((major, cycle, year,
                                                  self.AYYAM_I_HA, 1)):
            month = self.AYYAM_I_HA
        else:
            month = self.QUOTIENT(days, 19)

        return (major, cycle, year, month, day)

    def fixed_from_astro_bahai(self, b_date):
        """
        (defun fixed-from-astro-bahai (b-date)
          ;; TYPE bahai-date -> fixed-date
          ;; Fixed date of Baha’i date b-date.
          (let* ((major (bahai-major b-date))
                 (cycle (bahai-cycle b-date))
                 (year (bahai-year b-date))
                 (month (bahai-month b-date))
                 (day (bahai-day b-date))
                 (years; Years from epoch
                  (+ (* 361 (1- major))
                     (* 19 (1- cycle))
                     year)))
            (cond ((= month 19); last month of year
                   (+ (astro-bahai-new-year-on-or-before
                       (+ bahai-epoch
                          (floor (* mean-tropical-year
                                    (+ years 1/2)))))
                      -20 day))
                  ((= month ayyam-i-ha)
                   ;; intercalary month, between 18th & 19th
                   (+ (astro-bahai-new-year-on-or-before
                       (+ bahai-epoch
                          (floor (* mean-tropical-year
                                    (- years 1/2)))))
                      341 day))
                  (t (+ (astro-bahai-new-year-on-or-before
                         (+ bahai-epoch
                            (floor (* mean-tropical-year
                                      (- years 1/2)))))
                        (* (1- month) 19)
                        day -1)))))
        """
        major = b_date[0]
        cycle = b_date[1]
        year = b_date[2]
        month = b_date[3]
        day = b_date[4]
        years = (major + 1) * 361 + (cycle + 1) * 19 + year
        on_or_before = self.astro_bahai_new_year_on_or_before(
            self.BAHAI_EPOCH + math.floor(self.MEAN_TROPICAL_YEAR *
                                          (years - 0.5)))

        if month == 19:
            result = on_or_before - 20 + day
        elif month == self.AYYAM_I_HA:
            result = on_or_before + 341 + day
        else:
            result = on_or_before + (month - 1) * 19 + day - 1

        return result

    def astro_bahai_new_year_on_or_before(self, date):
        """
        (defun astro-bahai-new-year-on-or-before (date)
          ;; TYPE fixed-date -> fixed-date
          ;; Fixed date of astronomical Bahai New Year on or before fixed
          ;; date.
        (let* ((approx ; Approximate time of equinox.
                (estimate-prior-solar-longitude spring (bahai-sunset date))))
          (next day (1- (floor approx))
                (<= (solar-longitude (bahai-sunset day))
                    (+ spring (deg 2))))))
        """
        approx = self.estimate_prior_solar_longitude(
            self.SPRING, self.bahai_sunset(date))

        initial = math.floor(approx) - 1
        condition = (self.solar_longitude(self.bahai_sunset(day))
                     <= (self.SPRING + 2))
        return self.next_index(initial, 360, condition)

    def feast_of_ridvan(self, g_year):
        """
        (defun feast-of-ridvan (g-year)
          ;; TYPE gregorian-year -> fixed-date
          ;; Fixed date of Feast of Ridvan in Gregorian year g-year.
          (+ (naw-ruz g-year) 31))
        """
        return self.nam_ruz(g_year) + 31

    def birth_of_the_bab(self, g_year):
        """
        (defun birth-of-the-bab (g-year)
          ;; TYPE gregorian-year -> fixed-date
          ;; Fixed date of the Birthday of the Bab
          ;; in Gregorian year g-year.
          (let* ((ny ; Beginning of Baha’i year.
                 (naw-ruz g-year))
                 (set1 (bahai-sunset ny))
                 (m1 (new-moon-at-or-after set1))
                 (m8 (new-moon-at-or-after (+ m1 190)))
                 (day (fixed-from-moment m8))
                 (set8 (bahai-sunset day)))
            (if (< m8 set8)
                (1+ day)
              (+ day 2))))
        """
        return

    def nam_ruz(self, g_year):
        """
        (defun naw-ruz (g-year)
          ;; TYPE gregorian-year -> fixed-date
          ;; Fixed date of Baha’i New Year (Naw-Ruz) in Gregorian
          ;; year g-year.
          (astro-bahai-new-year-on-or-before
           (gregorian-new-year (1+ g-year))))
        """
        return self.astro_bahai_new_year_on_or_before(
            self._gc.gregorian_new_year(g_year + 1))

    def bahai_sunset(self, date):
        """
        (defun bahai-sunset (date)
          ;; TYPE fixed-date -> moment
          ;; Universal time of sunset on fixed date in Bahai-Location.
          (universal-from-standard
           (sunset date bahai-location)
           bahai-location))
        """
        return self.universal_from_standard(self.sunset(data))

    @property
    def latitude(self):
        """
        (defun latitude (location)
          ;; TYPE location -> half-circle
          (first location))
        """
        return self.BAHAI_LOCATION[0]

    @property
    def longitude(self):
        """
        (defun longitude (location)
          ;; TYPE location -> circle
          (second location))
        """
        return self.BAHAI_LOCATION[1]

    @property
    def elevation(self):
        """
        (defun elevation (location)
          ;; TYPE location -> distance
          (third location))
        """
        return self.BAHAI_LOCATION[2]

    @property
    def zone(self):
        """
        (defun zone (location)
          ;; TYPE location -> real
          (fourth location))
        """
        return self.BAHAI_LOCATION[3]

"""
(defun bahai-major (date)
  ;; TYPE bahai-date -> bahai-major
  (first date))

(defun bahai-cycle (date)
  ;; TYPE bahai-date -> bahai-cycle
  (second date))

(defun bahai-year (date)
  ;; TYPE bahai-date -> bahai-year
  (third date))

(defun bahai-month (date)
  ;; TYPE bahai-date -> bahai-month
  (fourth date))

(defun bahai-day (date)
  ;; TYPE bahai-date -> bahai-day
  (fifth date))
"""

    ## def bahai_new_year(self, g_year):
    ##     """
    ##     (defun bahai-new-year (g-year)
    ##       ;; TYPE gregorian-year -> fixed-date
    ##       ;; Fixed date of Baha’i New Year in Gregorian year g-year.
    ##       (fixed-from-gregorian
    ##        (gregorian-date g-year march 21)))
    ##     """
    ##     return self._gc.gregorian_new_year((g_year, self._gc.MARCH, 21))

    ## def bahai_from_fixed(self, data):
    ##     """
    ##     (defun bahai-from-fixed (date)
    ##       ;; TYPE fixed-date -> bahai-date
    ##       ;; Baha’i (major cycle year month day) corresponding to fixed date.
    ##       (let* ((g-year (gregorian-year-from-fixed date))
    ##              (start               ; 1844
    ##               (gregorian-year-from-fixed bahai-epoch))
    ##              (years               ; Since start of Baha’i calendar.
    ##               (- g-year start
    ##                  (if (<= date (fixed-from-gregorian
    ##                                (gregorian-date g-year march 20)))
    ##                      1 0)))
    ##               (major (1+ (quotient years 361)))
    ##               (cycle (1+ (quotient (mod years 361) 19)))
    ##               (year (1+ (mod years 19)))
    ##               (days               ; Since start of year
    ##                (- date (fixed-from-bahai
    ##                         (bahai-date major cycle year 1 1))))
    ##               (month
    ##                (cond ((>= date
    ##                           (fixed-from-bahai
    ##                            (bahai-date major cycle year 19 1)))
    ##                       19)         ; Last month of year.
    ##                      ((>= date    ; Intercalary days.
    ##                           (fixed-from-bahai
    ##                           (bahai-date major cycle year
    ##                                       ayyam-i-ha 1)))
    ##                      ayyam-i-ha)  ; Intercalary period.
    ##                     (t (1+ (quotient days 19)))))
    ##              (day (- date -1
    ##                      (fixed-from-bahai
    ##                       (bahai-date major cycle year month 1)))))
    ##         (bahai-date major cycle year month day)))
    ##     """
    ##     g_year = self.gregorian_year_from_fixed(date)
    ##     start = self.gregorian_year_from_fixed(bahai-epoch)
    ##     years = g_year - start

    ##     if date <= self._gc.fixed_from_gregorian(
    ##         self.fixed_from_gregorian((g_year, self._gc.MARCH, 20))):
    ##         year -= 1.0

    ##     major = self.QUOTIENT(years, 361) + 1
    ##     cycle = self.QUOTIENT(years % 361, 19) + 1
    ##     year = (years % 19) + 1
    ##     days = date - self.fixed_from_bahai((major, year, 1, 1))

    ##     if date >= self.fixed_from_bahai((major, cycle, year, 19, 1)):
    ##         month = 19
    ##     elif date >= self.fixed_from_bahai(
    ##         (major, cycle, year, self.AYYAM_I_HA, 1)):
    ##         month = self.AYYAM_I_HA
    ##     else:
    ##         month = self.QUOTIENT(days, 19) + 1

    ##     day = date + 1 - self.fixed_from_bahai((major, cycle, year, month, 1))
    ##     return (major, cycle, year, month, day)

    ## def fixed_from_bahai(self, b_date):
    ##     """
    ##     (defun fixed-from-bahai (b-date)
    ##       ;; TYPE bahai-date -> fixed-date
    ##       ;; Fixed date equivalent to the Baha’i date b-date.
    ##       (let* ((major (bahai-major b-date))
    ##              (cycle (bahai-cycle b-date))
    ##              (year (bahai-year b-date))
    ##              (month (bahai-month b-date))
    ##              (day (bahai-day b-date))
    ##              (g-year                    ; Corresponding Gregorian year.
    ##               (+ (* 361 (1- major))
    ##                  (* 19 (1- cycle)) year -1
    ##                  (gregorian-year-from-fixed bahai-epoch))))
    ##         (+ (fixed-from-gregorian        ; Prior years.
    ##             (gregorian-date g-year march 20))
    ##            (cond ((= month ayyam-i-ha)  ; Intercalary period.
    ##                   342)                  ; 18 months have elapsed.
    ##                  ((= month 19)          ; Last month of year.
    ##                   (if (gregorian-leap-year? (1+ g-year))
    ##                       347               ; Long ayyam-i-ha.
    ##                     346))               ; Ordinary ayyam-i-ha.
    ##                  (t (* 19 (1- month)))) ; Elapsed months.
    ##            day)))                       ; Days of current month.
    ##     """
    ##     major = b_date[0]
    ##     cycle = b_date[1]
    ##     year = b_date[2]
    ##     month = b_date[3]
    ##     day = b_date[4]
    ##     g_year = ((major - 1) * 361 + (cycle - 1) * 19 + year - 1 +
    ##               self._gc.gregorian_year_from_fixed(self.BAHAI_EPOCH))
    ##     result = self._gc.fixed_from_gregorian((g_year, self._gc.MARCH, 20))

    ##     if month == self.AYYAM_I_HA:
    ##         result += 342
    ##     elif month == 19:
    ##         if self._gc.GREGORIAN_LEAP_YEAR(g_year + 1):
    ##             result += 347
    ##         else:
    ##             result += 346
    ##     else:
    ##         result += (month - 1) * 19

    ##     return result + day
