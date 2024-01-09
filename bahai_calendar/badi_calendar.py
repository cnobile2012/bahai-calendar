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
    _AYYAM_I_HA = 0
    _TEHRAN_LOCATION = (35.696111, 51.423056, 0, 3.5)

    #(defconstant bahai-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of Baha’i calendar.
    #  (fixed-from-gregorian (gregorian-date 1844 march 21)))
    _BAHAI_EPOCH = 673222
    EVENING = False
    MORNING = True
    SPRING = 0
    MEAN_TROPICAL_YEAR = 365.242189

    # Various lists
    COEFFICIENTS = (
        403406, 195207, 119433, 112392, 3891, 2819, 1721, 660, 350, 334, 314,
        268, 242, 234, 158, 132, 129, 114, 99, 93, 86, 78, 72, 68, 64, 46, 38,
        37, 32, 29, 28, 27, 27, 25, 24, 21, 21, 20, 18, 17, 14, 13, 13, 13, 12,
        10, 10, 10, 10
        )
    MULTIPLIERS = (
        0.9287892, 35999.1376958, 35999.4089666, 35998.7287385, 71998.20261,
        71998.4403, 36000.35726, 71997.4812, 32964.4678, -19.4410, 445267.1117,
        45036.8840, 3.1008, 22518.4434, -19.9739, 65928.9345, 9038.0293,
        3034.7684, 33718.148, 3034.448, -2280.773, 29929.992, 31556.493,
        149.588, 9037.750, 107997.405, -4444.176, 151.771, 67555.316,
        31556.080, -4561.540, 107996.706, 1221.655, 62894.167, 31437.369,
        14578.298, -31931.757, 34777.243, 1221.999, 62894.511, -4442.039,
        107997.909, 119.066, 16859.071, -4.578, 26895.292, -39.127, 12297.536,
        90073.778
        )
    ADDENDS = (
        270.54861, 340.19128, 63.91854, 331.26220, 317.843, 86.631, 240.052,
        310.26, 247.23, 260.87, 297.82, 343.14, 166.79, 81.53, 3.50, 132.75,
        182.95, 162.03, 29.8, 266.4, 249.2, 157.6, 257.8, 185.1, 69.9, 8.0,
        197.1, 250.4, 65.3, 162.7, 341.5, 291.6, 98.5, 146.7, 110.0, 5.2,
        342.6, 230.9, 256.1, 45.3, 242.9, 115.2, 151.8, 285.3, 53.3, 126.6,
        205.7, 85.9, 146.1
        )

    def __init__(self):
        super().__init__()
        # Baha'i date: [major, cycle, year, month, day]
        self._bahai_date = []
        self._gc = GregorianCalendar()

    def bahai_from_fixed(self, data):
        """
        (defun bahai-from-fixed (date)
          ;; TYPE fixed-date -> bahai-date
          ;; Baha’i (major cycle year month day) corresponding to fixed date.
          (let* ((g-year (gregorian-year-from-fixed date))
                 (start ; 1844
                  (gregorian-year-from-fixed bahai-epoch))
                 (years ; Since start of Baha’i calendar.
                  (- g-year start
                     (if (<= date
                             (fixed-from-gregorian
                              (gregorian-date g-year march 20)))
                         1 0)))
                  (major (1+ (quotient years 361)))
                  (cycle (1+ (quotient (mod years 361) 19)))
                  (year (1+ (mod years 19)))
                  (days; Since start of year
                   (- date (fixed-from-bahai
                            (bahai-date major cycle year 1 1))))
                  (month
                   (cond ((>= date
                              (fixed-from-bahai
                               (bahai-date major cycle year 19 1)))
                          19) ; Last month of year.
                         ((>= date ; Intercalary days.
                              (fixed-from-bahai
                              (bahai-date major cycle year
                                          ayyam-i-ha 1)))
                         ayyam-i-ha) ; Intercalary period.
                        (t (1+ (quotient days 19)))))
                 (day (- date -1
                         (fixed-from-bahai
                          (bahai-date major cycle year month 1)))))
            (bahai-date major cycle year month day)))
        """
        g_year = self.gregorian_year_from_fixed(date)
        start = self.gregorian_year_from_fixed(bahai-epoch)
        years = g_year - start - self.gc.fixed_from_gregorian()

        return

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
        major = None



        return

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

    def solar_longitude(self, tee):
        '''
        (defun solar-longitude (tee)
          ;; TYPE moment -> season
          ;; Longitude of sun at moment tee.
          ;; Adapted from "Planetary Programs and Tables from -4000
          ;; to +2800" by Pierre Bretagnon and Jean-Simon,
          ;; Willmann-Bell, 1986.
          (let* ((c       ; moment in Julian centuries
                  (julian-centuries tee))
                 (coefficients
                  (list 403406 195207 119433 112392 3891 2819 1721
                        660 350 334 314 268 242 234 158 132 129 114
                        99 93 86 78 72 68 64 46 38 37 32 29 28 27 27
                        25 24 21 21 20 18 17 14 13 13 13 12 10 10 10
                        10))
                 (multipliers
                  (list 0.9287892L0 35999.1376958L0 35999.4089666L0
                        35998.7287385L0 71998.20261L0 71998.4403L0
                        36000.35726L0 71997.4812L0 32964.4678L0
                        -19.4410L0 445267.1117L0 45036.8840L0 3.1008L0
                        22518.4434L0 -19.9739L0 65928.9345L0
                        9038.0293L0 3034.7684L0 33718.148L0 3034.448L0
                        -2280.773L0 29929.992L0 31556.493L0 149.588L0
                        9037.750L0 107997.405L0 -4444.176L0 151.771L0
                        67555.316L0 31556.080L0 -4561.540L0
                        107996.706L0 1221.655L0 62894.167L0
                        31437.369L0 14578.298L0 -31931.757L0
                        34777.243L0 1221.999L0 62894.511L0
                        -4442.039L0 107997.909L0 119.066L0 16859.071L0
                        -4.578L0 26895.292L0 -39.127L0 12297.536L0
                        90073.778L0))
                 (addends
                  (list 270.54861L0 340.19128L0 63.91854L0 331.26220L0
                        317.843L0 86.631L0 240.052L0 310.26L0 247.23L0
                        260.87L0 297.82L0 343.14L0 166.79L0 81.53L0
                        3.50L0 132.75L0 182.95L0 162.03L0 29.8L0
                        266.4L0 249.2L0 157.6L0 257.8L0 185.1L0 69.9L0
                        8.0L0 197.1L0 250.4L0 65.3L0 162.7L0 341.5L0
                        291.6L0 98.5L0 146.7L0 110.0L0 5.2L0 342.6L0
                        230.9L0 256.1L0 45.3L0 242.9L0 115.2L0 151.8L0
                        285.3L0 53.3L0 126.6L0 205.7L0 85.9L0
                        146.1L0))
                 (lambda
                   (+ (deg 282.7771834L0)
                      (* (deg 36000.76953744L0) c)
                      (* (deg 0.000005729577951308232L0)
                         (sigma ((x coefficients)
                                 (y addends)
                                 (z multipliers))
                                (* x (sin-degrees (+ y (* z c)))))))))
            (mod (+ lambda (aberration tee) (nutation tee)) 360)))
        '''
        c = self.julian_centuries(tee)
        lambda_ = (282.7771834 + (36000.76953744 * c) +
                   (0.000005729577951308232 *
                    self.sigma(((x, self.COEFFICIENTS), (y, self.MULTIPLIERS),
                                (z, self.ADDENDS)),
                               'x * self.sin_degrees(y + (z * c))')))
        return (lambda_ + self.aberration(tee) + self.nutation(tee) + 360)

    def aberration(self, tee):
        """
        (defun aberration (tee)
          ;; TYPE moment -> circle
          ;; Aberration at moment tee.
          (let* ((c       ; moment in Julian centuries
                  (julian-centuries tee)))
            (- (* (deg 0.0000974L0)
                  (cos-degrees
                   (+ (deg 177.63L0) (* (deg 35999.01848L0) c))))
               (deg 0.005575L0))))
        """
        c = self.julian_centuries(tee)
        return ((0.0000974 * self.cos_degrees(177.63 + (35999.01848 * c)))
                - 0.005575)

    def nutation(self, tee):
        """
        (defun nutation (tee)
          ;; TYPE moment -> circle
          ;; Longitudinal nutation at moment tee.
          (let* ((c       ; moment in Julian centuries
                  (julian-centuries tee))
                 (cap-A (poly c (deg (list 124.90L0 -1934.134L0 0.002063L0))))
                 (cap-B (poly c (deg (list 201.11L0 72001.5377L0 0.00057L0)))))
             (+ (* (deg -0.004778L0) (sin-degrees cap-A))
                (* (deg -0.0003667L0) (sin-degrees cap-B)))))
        """
        c = self.julian_centuries(tee)
        cap_a = self.poly(c, (124.90, -1934.134, 0.002063))
        car_b = self.poly(c, (201.11, 72001.5377, 0.00057))
        return ((-0.004778 * self.sin_degrees(cap_a)) +
                (-0.0003667 * self.sin_degrees(cap_a)))

    def refraction(self, tee):
        """
        (defun refraction (tee location)
          ;; TYPE (moment location) -> half-circle
          ;; Refraction angle at moment tee at location.
          ;; The moment is not used.
          (let* ((h (max (mt 0) (elevation location)))
                 (cap-R (mt 6.372d6)) ; Radius of Earth.
                 (dip ; Depression of visible horizon.
                  (arccos-degrees (/ cap-R (+ cap-R h)))))
            (+ (mins 34) dip (* (secs 19) (sqrt h)))))
        """
        h = max(0, self.elevation)
        cap_r = 6.372 #d6 -- Wikipedia average is 6371 Km
        dip = self.arccos_degrees(cap_r / (cap_r + h))
        return self.MINS(34) + dip + (self.SECS(19) * math.sqrt(h))

    def declination(self, tee, beta, lambda_):
        """
        (defun declination (tee beta lambda)
          ;; TYPE (moment half-circle circle) -> angle
          ;; Declination at moment UT tee of object at
          ;; latitude beta and longitude lambda.
          (let* ((varepsilon (obliquity tee)))
            (arcsin-degrees (+ (* (sin-degrees beta)
                                  (cos-degrees varepsilon))
                               (* (cos-degrees beta)
                                  (sin-degrees varepsilon)
                                  (sin-degrees lambda))))))
        """
        varepsilon = self.obliquity(tee)
        return ((self.sin_degrees(beta) * self.cos_degrees(varepsilon)) +
                (self.cos_degrees(beta) * self.sin_degrees(varepsilon)
                 * self.sin_degrees(lambda_)))

    def obliquity(self, tee):
        """
        (defun obliquity (tee)
          ;; TYPE moment -> angle
          ;; Obliquity of ecliptic at moment tee.
          (let* ((c (julian-centuries tee)))
            (+ (angle 23 26 21.448L0)
               (poly c (list 0L0
                             (angle 0 0 -46.8150L0)
                             (angle 0 0 -0.00059L0)
                             (angle 0 0 0.001813L0))))))
        """
        c = self.julian_centuries(tee)
        angle_list = (0, self.ANGLE(0, 0, -46.8150),
                      self.ANGLE(0, 0, -0.00059), self.ANGLE(0, 0, 0.001813))
        return self.ANGLE(23, 26, 21.448) + self.POLY(c, angle_list)

    def equation_of_time(self, tee):
        '''
        (defun equation-of-time (tee)
          ;; TYPE moment -> fraction-of-day
          ;; Equation of time (as fraction of day) for moment tee.
          ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
          ;; Willmann-Bell, 2nd edn., 1998, p. 185.
          (let* ((c (julian-centuries tee))
                 (lambda
                   (poly c
                         (deg (list 280.46645L0 36000.76983L0 0.0003032L0))))
                 (anomaly
                  (poly c
                        (deg (list 357.52910L0 35999.05030L0
                                   -0.0001559L0 -0.00000048L0))))
                 (eccentricity
                  (poly c
                        (list 0.016708617L0 -0.000042037L0 -0.0000001236L0)))
                 (varepsilon (obliquity tee))
                 (y (expt (tan-degrees (/ varepsilon 2)) 2))
                 (equation
                  (* (/ 1 2 pi)
                     (+ (* y (sin-degrees (* 2 lambda)))
                        (* -2 eccentricity (sin-degrees anomaly))
                        (* 4 eccentricity y (sin-degrees anomaly)
                           (cos-degrees (* 2 lambda)))
                        (* -0.5L0 y y (sin-degrees (* 4 lambda)))
                        (* -1.25L0 eccentricity eccentricity
                           (sin-degrees (* 2 anomaly)))))))
            (* (sign equation) (min (abs equation) (hr 12L0)))))
        '''
        c = self.julian_centuries(tee)
        lambda_ = self.POLY(c, (280.46645, 36000.76983, 0.0003032))
        anomaly = self.POLY(c, (357.52910, 35999.05030, -0.0001559,
                                -0.00000048))
        eccentricity = self.POLY(c, (0.016708617, -0.000042037, -0.0000001236))
        varepsilon = self.obliquity(tee)
        y = self.tan_degrees(varepsilon / 2) ** 2
        equation = ((0.5 / math.pi) *
                    ((y * self.sin_degrees(2 * lambda_)) +
                     (-2 * eccentricity * self.sin_degrees(anomaly) *
                      self.cos_degrees(2 * lambda_)) +
                     (-0.5 * y * y * self.sin_degrees(4 * lambda_)) +
                     (-1.25 * eccentricity * eccentricity *
                      self.sin_degrees(2 * anomaly))))
        return math.sin(equation) * min(abs(equation)) * self.HR(12)

    def apparent_from_local(self, tee_ell):
        """
        (defun apparent-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Sundial time from local time tee_ell at location.
          (+ tee_ell (equation-of-time
                      (universal-from-local tee_ell location))))
        """
        return tee_ell + self.equation_of_time(
            self.universal_from_local(tee_ell))

    def local_from_apparent(self, tee):
        """
        (defun local-from-apparent (tee location)
          ;; TYPE (moment location) -> moment
          ;; Local time from sundial time tee at location.
          (- tee (equation-of-time (universal-from-local tee location))))
        """
        return tees - self.equation_of_time(self.universal_from_local(tee))

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

    def fixed_from_moment(self, tee):
        """
        (defun fixed-from-moment (tee)
          ;; TYPE moment -> fixed-date
          ;; Fixed-date from moment tee.
          (floor tee))
        """
        return math.floor(tee)

    def radians_from_degrees(self, theta):
        """
        (defun radians-from-degrees (theta)
          ;; TYPE real -> radian
          ;; Convert angle theta from degrees to radians.
          (* (mod theta 360) pi 1/180))
        """
        return (theta % 360) * math.pi * 1/180

    def degrees_from_radians(self, theta):
        """
        (defun degrees-from-radians (theta)
          ;; TYPE radian -> angle
          ;; Convert angle theta from radians to degrees.
          (mod (/ theta pi 1/180) 360))
        """
        return (theta / math.pi / 1/180) % 360

    def sin_degrees(self, theta):
        """
        (defun sin-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Sine of theta (given in degrees).
          (sin (radians-from-degrees theta)))
        """
        return math.sin(self.radians_from_degrees(theta))

    def cos_degrees(self, theta):
        """
        (defun cos-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Cosine of theta (given in degrees).
          (cos (radians-from-degrees theta)))
        """
        return math.cos(self.radians_from_degrees(theta))

    def tan_degrees(self, theta):
        """
        (defun tan-degrees (theta)
          ;; TYPE angle -> real
          ;; Tangent of theta (given in degrees).
          (tan (radians-from-degrees theta)))
        """
        return math.tan(self.radians_from_degrees(theta))

    def dynamical_from_universal(self, tee_rom_u):
        """
        (defun dynamical-from-universal (tee_rom-u)
          ;; TYPE moment -> moment
          ;; Dynamical time at Universal moment tee_rom-u.
          (+ tee_rom-u (ephemeris-correction tee_rom-u)))
        """
        return tee_rom_u + self.ephemeris_correction(tee_rom_u)

    def universal_from_dynamical(self, tee):
        """
        (defun universal-from-dynamical (tee)
          ;; TYPE moment -> moment
          ;; Universal moment from Dynamical time tee.
          (- tee (ephemeris-correction tee)))
        """
        return tee - self.ephemeris_correction(tee_rom_u)

    def julian_centuries(self, tee):
        """
        (defun julian-centuries (tee)
          ;; TYPE moment -> century
          ;; Julian centuries since 2000 at moment tee.
          (/ (- (dynamical-from-universal tee) j2000) 36525))
        """
        return (self.dynamical_from_universal(tee) - 2000) / 36525

    def arcsin_degrees(self, x):
        """
        (defun arcsin-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arcsine of x in degrees.
          (degrees-from-radians (asin x)))
        """
        return self.degrees_from_radians(math.asin(x))

    def arccos_degrees(x):
        """
        (defun arccos-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arccosine of x in degrees.
          (degrees-from-radians (acos x)))
        """
        return self.degrees_from_radians(math.acos(x))

    @property
    def latitude(self):
        """
        (defun latitude (location)
          ;; TYPE location -> half-circle
          (first location))
        """
        return self._TEHRAN_LOCATION[0]

    @property
    def longitude(self):
        """
        (defun longitude (location)
          ;; TYPE location -> circle
          (second location))
        """
        return self._TEHRAN_LOCATION[1]

    @property
    def elevation(self):
        """
        (defun elevation (location)
          ;; TYPE location -> distance
          (third location))
        """
        return self._TEHRAN_LOCATION[2]

    @property
    def zone(self):
        """
        (defun zone (location)
          ;; TYPE location -> real
          (fourth location))
        """
        return self._TEHRAN_LOCATION[3]

    #def bahai_date(self, ):

"""
(defun bahai-date (major cycle year month day)
  ;; TYPE (bahai-major bahai-cycle bahai-year
  ;; TYPE bahai-month bahai-day) -> bahai-date
  (list major cycle year month day))

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

(defconstant ayyam-i-ha
  ;; TYPE bahai-month
  ;; Signifies intercalary period of 4 or 5 days.
  0)
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
"""
1
 (defun apparent-from-universal (tee_rom-u location)
2
 ;; TYPE (moment location) -> moment
3
 ;; True (apparent) time at universal time tee at location.
4
 (apparent-from-local
5
 (local-from-universal tee_rom-u location)
6
 location))
 (14.23)
1
 (defun universal-from-apparent (tee location)
2
 ;; TYPE (moment location) -> moment
3
 ;; Universal time from sundial time tee at location.
4
 (universal-from-local
5
 (local-from-apparent tee location)
6
 location))
"""
