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
    EVENING = False
    MORNING = True
    SPRING = 0
    MEAN_TROPICAL_YEAR = 365.242189

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

    def zone_from_longitude(self, phi):
        """
        (defun zone-from-longitude (phi)
          ;; TYPE circle -> duration
          ;; Difference between UT and local mean time at longitude
          ;; phi as a fraction of a day.
          (/ phi (deg 360)))
        """
        return phi / 360

    def sunrise(self, date):
        """
        (defun sunrise (date location)
          ;; TYPE (fixed-date location) -> moment
          ;; Standard time of sunrise on fixed date at location.
          (let* ((alpha (+ (refraction (+ date (hr 6)) location) (mins 16))))
            (dawn date location alpha)))
        """
        alpha = self.refraction(date + self.HR(6)) + self.MINS(16)
        return self.dawn(date, alpha)

    def sunset(self, date):
        """
        (defun sunset (date location)
          ;; TYPE (fixed-date location) -> moment
          ;; Standard time of sunset on fixed date at location.
          (let* ((alpha (+ (refraction (+ date (hr 18)) location) (mins 16))))
            (dusk date location alpha)))
        """
        alpha = self.refraction(date + self.HR(18)) + self.MINS(16)
        return self.dusk(date, alpha)

    def dawn(self, date, alpha):
        """
        (defun dawn (date location alpha)
          ;; TYPE (fixed-date location half-circle) -> moment
          ;; Standard time in morning on fixed date at
          ;; location when depression angle of sun is alpha.
          ;; Returns bogus if there is no dawn on date.
          (let* ((result (moment-of-depression
                          (+ date (hr 6)) location alpha morning)))
            (if (equal result bogus)
                bogus
              (standard-from-local result location))))
        """
        result = self.moment_of_depression(date + self.HR(6), alpha,
                                           self.EVENING)
        return self.standard_from_local(result) if result else result

    def dusk(self, date, alpha):
        """
        (defun dusk (date location alpha)
          ;; TYPE (fixed-date location half-circle) -> moment
          ;; Standard time in evening on fixed date at
          ;; location when depression angle of sun is alpha.
          ;; Returns bogus if there is no dusk on date.
          (let* ((result (moment-of-depression
                          (+ date (hr 18)) location alpha evening)))
            (if (equal result bogus)
                bogus
              (standard-from-local result location))))
        """
        result = self.moment_of_depression(date + self.HR(18), alpha,
                                           self.EVENING)
        return self.standard_from_local(result) if result else result

    def moment_of_depression(self, approx, alpha, early=EVENING):
        """
        (defun moment-of-depression (approx location alpha early?)
          ;; TYPE (moment location half-circle boolean) - moment
          ;; Moment in local time near approx when depression angle of sun
          ;; is alpha (negative if above horizon) at location; early? is
          ;; true when morning event is sought, and false for evening.
          ;; Returns bogus if depression angle is not reached.
          (let* ((tee (approx-moment-of-depression
                       approx location alpha early?)))
            (if (equal tee bogus)
                bogus
              (if (< (abs (- approx tee))
                     (sec 30))
                  tee
                (moment-of-depression tee location alpha early?)))))
        """
        tee = self.approx_moment_of_depression(approx, alpha, early=early)
        return (self.moment_of_depression(tee, alpha, early=early)
                if tee and abs(approx - tee) < self.SEC(30) else None)

    def approx_moment_of_depression(self, approx, alpha, early):
        """
        (defun approx-moment-of-depression (tee location alpha early?)
          ;; TYPE (moment location half-circle boolean) - moment
          ;; Moment in local time near tee when depression angle of sun
          ;; is alpha (negative if above horizon) at location; early? is
          ;; true when morning event is sought and false for evening.
          ;; Returns bogus if depression angle is not reached.
          (let* ((try (sine-offset tee location alpha))
                 (date (fixed-from-moment tee))
                 (alt (if (>= alpha 0)
                          (if early? date (1+ date))
                        (+ date (hr 12))))
                 (value (if (> (abs try) 1)
                            (sine-offset alt location alpha) try)))
           (if (<= (abs value) 1) ; Event occurs
               (let* ((offset (mod3 (/ (arcsin-degrees value) (deg 360))
                                    (hr -12) (hr 12))))
                 (local-from-apparent
                  (+ date
                     (if early?
                         (- (hr 6) offset)
                       (+ (hr 18) offset)))
                  location))
             bogus)))
        """
        result = None
        try_ = self.sine_offset(tee, alpha)
        date = self.fixed_from_moment(tee)

        if alpha >= 0:
            alt = date if early else date + 1
        else:
            alt = date + self.HR(12)

        value = self.sine_offset(alt, alpha) if abs(try_) > 1 else try_

        if abs(value) <= 1:
            offset = self.MOD3(self.arcsin_degrees(value) / 360,
                               self.HR(-12), self.HR(12))
            date += self.HR(6) - offset if early else self.HR(18) + offset
            result = self.local_from_apparent(date)

        return result

    def sine_offset(self, tee, alpha):
        """
        (defun sine-offset (tee location alpha)
          ;; TYPE (moment location half-circle) -> real
          ;; Sine of angle between position of sun at local time tee and
          ;; when its depression is alpha at location.
          ;; Out of range when it does not occur.
          (let* ((phi (latitude location))
                 (tee-prime (universal-from-local tee location))
                 (delta ; Declination of sun.
                  (declination tee-prime (deg 0L0)
                               (solar-longitude tee-prime))))
            (+ (* (tan-degrees phi)
                  (tan-degrees delta))
               (/ (sin-degrees alpha)
                  (* (cos-degrees delta)
                     (cos-degrees phi))))))
        """
        phi = self.latitude
        tee_prime = self.universal_from_local(tee)
        delta = self.declination(tee-prime, 0, self.solar_longitude(tee_prime))
        return (self.tan_degrees(phi) * self.tan_degrees(delta) +
                self.sin_degrees(alpha) / (self.cos_degrees(delta) *
                                           self.cos_degrees(phi)))

    def estimate_prior_solar_longitude(self, lambda_, tee):
        """
        (defun estimate-prior-solar-longitude (lambda tee)
          ;; TYPE (season moment) -> moment
          ;; Approximate moment at or before tee when solar
          ;; longitude just exceeded lambda degrees.
          (let* ((rate ; Mean change of one degree.
                  (/ mean-tropical-year (deg 360)))
                 (tau ; First approximation.
                  (- tee
                     (* rate (mod (- (solar-longitude tee)
                                     lambda)
                              360))))
                   (cap-Delta ; Difference in longitude.
                    (mod3 (- (solar-longitude tau) lambda)
                          -180 180)))
            (min tee (- tau (* rate cap-Delta)))))
        """
        rate = self.MEAN_TROPICAL_YEAR / 360
        tau = tee - (rate * (self.solar_longitude(tee) - lambda_) % 360)
        cap_delta = self.MOD3(self.solar_longitude(tau) - lambda_, -180, 180)
        return min(tee, tau - rate * cap_delta)

    def standard_from_universal(self, tee_rom_u):
        """
        (defun standard-from-universal (tee_rom-u location)
          ;; TYPE (moment location) -> moment
          ;; Standard time from tee_rom-u in universal time at location.
          (+ tee_rom-u (zone location)))
        """
        return tee_rom_u + self.zone

    def universal_from_standard(self, tee_rom_s):
        """
        (defun universal-from-standard (tee_rom-s location)
          ;; TYPE (moment location) -> moment
          ;; Universal time from tee_rom-s in standard time at location.
          (- tee_rom-s (zone location)))
        """
        return tee_rom_s - self.zone

    def standard_from_local(self, tee_ell):
        """
        (defun standard-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Standard time from local tee_ell at location.
          (standard-from-universal (universal-from-local tee_ell location)
          location))
        """
        return self.standard_from_universal(self.universal_from_local(tee_ell))

    def universal_from_local(self, tee_ell):
        """
        (defun universal-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Universal time from local tee_ell at location.
          (- tee_ell (zone-from-longitude (longitude location))))
        """
        return tee_ell - self.zone_from_longitude(self.longitude)

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

    ## def mt(self, x):
    ##     """
    ##     (defun mt (x)
    ##       ;; TYPE real -> distance
    ##       ;; x meters.
    ##       ;; For typesetting purposes.
    ##       x)
    ##     """
    ##     return x

    ## def deg(self, x):
    ##     """
    ##     (defun deg (x)
    ##       ;; TYPE real -> angle
    ##       ;; TYPE list-of-reals -> list-of-angles
    ##       ;; x degrees.
    ##       ;; For typesetting purposes.
    ##       x)
    ##     """
    ##     return x

    ## def degrees_minutes_seconds(self, d, m, s):
    ##     """
    ##     (defun degrees-minutes-seconds (d m s)
    ##       ;; TYPE (degree minute real) -> angle
    ##       (list d m s))
    ##     """
    ##     return d, m, s

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
