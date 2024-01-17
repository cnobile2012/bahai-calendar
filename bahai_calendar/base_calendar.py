# -*- coding: utf-8 -*-
#
# bahai_calendar/base_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime


class BaseCalendar:
    """
    Basic functionality used with all calenders.

    R.D. = Fixed date—elapsed days since the onset of Monday, January 1, 1
           (Gregorian)
    U.T. = Mean solar time at Greenwich, England (0◦ meridian), reckoned
           from midnight; sometimes g.m.t., Greenwich Mean Time
    Moment = We call an r.d. that has a fractional part giving the time of
             day a “moment.”
    """
    # Build out some lambdas
    #(defun hr (x)
    #  ;; TYPE real -> duration
    #  ;; x hours.
    #  (/ x 24))
    HR = lambda self, x: x / 24

    #(defun mn (x)
    #  ;; TYPE real -> duration
    #  ;; x minutes.
    #  (/ x 24 60))
    MN = lambda self, x: x / 24 / 60

    #(defun sec (x)
    #  ;; TYPE real -> duration
    #  ;; x seconds.
    #  (/ x 24 60 60))
    SEC = lambda self, x: x / 24 / 60 / 60

    #(defun mins (x)
    #  ;; TYPE real -> angle
    #  ;; x arcminutes
    #  (/ x 60))
    MINS = lambda self, x: x / 60

    #(defun secs (x)
    #  ;; TYPE real -> angle
    #  ;; x arcseconds
    #  (/ x 3600))
    SECS = lambda self, x: x / 3600

    #(defun angle (d m s)
    #  ;; TYPE (integer integer real) -> angle
    #  ;; d degrees, m arcminutes, s arcseconds.
    #  (+ d (/ (+ m (/ s 60)) 60)))
    ANGLE = lambda self, d, m, s: d + (m + (s / 60)) # 0 - 360

    #(defun mod3 (x a b)
    #  ;; TYPE (real real real) -> real
    #  ;; The value of x shifted into the range [a..b). Returns x if a=b.
    #  (if (= a b)
    #      x
    #    (+ a (mod (- x a) (- b a)))))
    MOD3 = lambda self, x, a, b : x if a == b else a + (x - a) % (b - a)

    #(defconstant j2000
    #  ;; TYPE moment
    #  ;; Noon at start of Gregorian year 2000.
    #  (+ (hr 12L0) (gregorian-new-year 2000)))
    J2000 = 730120.5

    # (defun quotient (m n)
    #   ;; TYPE (real nonzero-real) -> integer
    #   ;; Whole part of m/n.
    #   (floor m n))
    QUOTIENT = lambda self, m, n: math.floor(m / n)

    #(defconstant mean-synodic-month
    #  ;; TYPE duration
    #  29.530588861L0)
    MEAN_SYNODIC_MONTH = 29.530588861

    # (defun rd (tee)
    #  ;; TYPE moment -> moment
    #  ;; Identity function for fixed dates/moments. If internal timekeeping
    #  ;; is shifted, change epoch to be RD date of origin of internal count.
    #  ;; epoch should be an integer.
    #  (let* ((epoch 0))
    #    (- tee epoch)))
    # Unless the epoch is changed to something other than 0 there is no need
    # to implement this function.
    #RD = tee - 0

    MEAN_TROPICAL_YEAR = 365.242189
    MORNING = True
    EVENING = False
    SPRING = 0

    # Lists for solar_longitude()
    _COEFFICIENTS = (
        403406, 195207, 119433, 112392, 3891, 2819, 1721, 660, 350, 334, 314,
        268, 242, 234, 158, 132, 129, 114, 99, 93, 86, 78, 72, 68, 64, 46, 38,
        37, 32, 29, 28, 27, 27, 25, 24, 21, 21, 20, 18, 17, 14, 13, 13, 13, 12,
        10, 10, 10, 10
        )
    _MULTIPLIERS = (
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
    _ADDENDS = (
        270.54861, 340.19128, 63.91854, 331.26220, 317.843, 86.631, 240.052,
        310.26, 247.23, 260.87, 297.82, 343.14, 166.79, 81.53, 3.50, 132.75,
        182.95, 162.03, 29.8, 266.4, 249.2, 157.6, 257.8, 185.1, 69.9, 8.0,
        197.1, 250.4, 65.3, 162.7, 341.5, 291.6, 98.5, 146.7, 110.0, 5.2,
        342.6, 230.9, 256.1, 45.3, 242.9, 115.2, 151.8, 285.3, 53.3, 126.6,
        205.7, 85.9, 146.1
        )

    def __init__(self):
        self._time = None
        from bahai_calendar.gregorian_calendar import BaseGregorianCalendar
        self._bgc = BaseGregorianCalendar()

    def parse_datetime(self, dt:datetime.datetime) -> None:
        self.date_representation = (dt.hour, dt.minute, dt.second,
                                    dt.microsecond)

    @property
    def date_representation(self):
        return self._time

    @date_representation.setter
    def date_representation(self, representation):
        self._time = representation

    #
    # Time and Astronomy (Time)
    #

    def zone_from_longitude(self, phi):
        """
        (defun zone-from-longitude (phi)
          ;; TYPE circle -> duration
          ;; Difference between UT and local mean time at longitude
          ;; phi as a fraction of a day.
          (/ phi (deg 360)))
        """
        return phi / 360

    def universal_from_local(self, tee_ell):
        """
        (defun universal-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Universal time from local tee_ell at location.
          (- tee_ell (zone-from-longitude (longitude location))))
        """
        return tee_ell - self.zone_from_longitude(self.longitude)

    # local-from-universal

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

    #local-from-standard

    def ephemeris_correction(self, tee):
        '''
        used

        (defun ephemeris-correction (tee)
          ;; TYPE moment -> fraction-of-day
          ;; Dynamical Time minus Universal Time (in days) for moment tee.
          ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
          ;; Willmann-Bell (1991) for years 1600-1986 and from polynomials
          ;; on the NASA Eclipse web site for other years.
          (let* ((year (gregorian-year-from-fixed (floor tee)))
                 (c (/ (gregorian-date-difference
                        (gregorian-date 1900 january 1)
                        (gregorian-date year july 1))
                       36525))
                 (c2051 (* 1/86400
                           (+ -20 (* 32 (expt (/ (- year 1820) 100) 2))
                              (* 0.5628L0 (- 2150 year)))))
                 (y2000 (- year 2000))
                 (c2006 (* 1/86400
                           (poly y2000
                               (list 62.92L0 0.32217L0 0.005589L0))))
                 (c1987 (* 1/86400
                           (poly y2000
                                 (list 63.86L0 0.3345L0 -0.060374L0
                                 0.0017275L0 0.000651814L0 0.00002373599L0))))
                 (c1900 (poly c
                              (list -0.00002L0 0.000297L0 0.025184L0
                              -0.181133L0 0.553040L0 -0.861938L0 0.677066L0
                              -0.212591L0)))
                 (c1800 (poly c
                              (list -0.000009L0 0.003844L0 0.083563L0
                              0.865736L0 4.867575L0 15.845535L0 31.332267L0
                              38.291999L0 28.316289L0 11.636204L0 2.043794L0)))
                 (y1700 (- year 1700))
                 (c1700 (* 1/86400
                           (poly y1700
                                 (list 8.118780842L0 -0.005092142L0
                                 0.003336121L0 -0.0000266484L0))))
                 (y1600 (- year 1600))
                 (c1600 (* 1/86400
                           (poly y1600
                                 (list 120 -0.9808L0 -0.01532L0
                                 0.000140272128L0))))
                 (y1000 (/ (- year 1000) 100L0))
                 (c500 (* 1/86400
                          (poly y1000
                                (list 1574.2L0 -556.01L0 71.23472L0 0.319781L0
                                -0.8503463L0 -0.005050998L0 0.0083572073L0))))
                 (y0 (/ year 100L0))
                 (c0 (* 1/86400
                        (poly y0
                              (list 10583.6L0 -1014.41L0 33.78311L0
                              -5.952053L0 -0.1798452L0 0.022174192L0
                              0.0090316521L0))))
                 (y1820 (/ (- year 1820) 100L0))
                 (other (* 1/86400
                           (poly y1820 (list -20 0 32)))))
            (cond ((<= 2051 year 2150) c2051)
                  ((<= 2006 year 2050) c2006)
                  ((<= 1987 year 2005) c1987)
                  ((<= 1900 year 1986) c1900)
                  ((<= 1800 year 1899) c1800)
                  ((<= 1700 year 1799) c1700)
                  ((<= 1600 year 1699) c1600)
                  ((<= 500 year 1599) c500)
                  ((< -500 year 500) c0)
                  (t other))))
        '''
        year = self._bgc.gregorian_year_from_fixed(math.floor(tee))
        c = self._bgc.gregorian_date_difference(
            (1900, self._bgc.JANUARY, 1), (year, self._bgc.JULY, 1)) / 36525

        if 2051 <= year <= 2150:
            result = 1/86400 - 20 + 32 * (((year - 1820) / 100) ** 2)
        elif 2006 <= year <= 2050:
            y2000 = year - 2000
            result = 1/86400 * self._poly(y2000, (62.92, 0.32217, 0.005589))
        elif 1987 <= year <= 2005:
            result = 1/86400 * self._poly(y2000,
                                          (63.86, 0.3345, -0.060374, 0.0017275,
                                           0.000651814, 0.00002373599))
        elif 1900 <= year <= 1986:
            result = self._poly(c, (-0.00002, 0.000297, 0.025184, -0.181133,
                                    0.553040, -0.861938, 0.677066, -0.212591))
        elif 1800 <= year <= 1899:
            result = self._poly(c, (-0.000009, 0.003844, 0.083563, 0.865736,
                                    4.867575, 15.845535, 31.332267, 38.291999,
                                    28.316289, 11.636204, 2.043794))
        elif 1700 <= year <= 1799:
            y1700 = year - 1700
            result = 1/86400 * self._poly(
                y1700, (8.118780842, -0.005092142, 0.003336121, -0.0000266484))
        elif 1600 <= year <= 1699:
            y1600 = year - 1600
            result = 1/86400 * self._poly(
               y1600, (120, -0.9808, -0.01532, 0.000140272128))
        elif 500 <= year <= 1599:
            y1000 = (year - 1000) / 100
            result = 1/86400 * self._poly(
                y1000, (1574.2, -556.01, 71.23472, 0.319781, -0.8503463,
                        -0.005050998, 0.0083572073))
        elif -500 < year < 500:
            y0 = year / 100
            result = 1/86400 * self._poly(
                y0, (10583.6, -1014.41, 33.78311, -5.952053, -0.1798452,
                     0.022174192, 0.0090316521))
        else:
            y1820 = (year - 1820) / 100
            result = 1/86400 * self._poly(y1820, (-20, 0, 32))

        return result

    def dynamical_from_universal(self, tee_rom_u):
        """
        used

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
        return tee - self.ephemeris_correction(tee)

    def julian_centuries(self, tee):
        """
        used

        (defun julian-centuries (tee)
          ;; TYPE moment -> century
          ;; Julian centuries since 2000 at moment tee.
          (/ (- (dynamical-from-universal tee) j2000) 36525))
        """
        return (self.dynamical_from_universal(tee) - self.J2000) / 36525

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
                        (deg (list 357.52910L0 35999.05030L0 -0.0001559L0
                                   -0.00000048L0))))
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
        lambda_ = self._poly(c, (280.46645, 36000.76983, 0.0003032))
        anomaly = self._poly(c, (357.52910, 35999.05030, -0.0001559,
                                 -0.00000048))
        eccentricity = self._poly(c, (0.016708617, -0.000042037,
                                      -0.0000001236))
        varepsilon = self.obliquity(tee)
        y = self.tan_degrees(varepsilon / 2) ** 2
        equation = ((0.5 / math.pi) *
                    ((y * self.sin_degrees(2 * lambda_)) +
                     (-2 * eccentricity * self.sin_degrees(anomaly)) +
                     (4 * eccentricity * y * self.sin_degrees(anomaly) *
                      self.cos_degrees(2 * lambda_)) +
                     (-0.5 * y * y * self.sin_degrees(4 * lambda_)) +
                     (-1.25 * eccentricity * eccentricity *
                      self.sin_degrees(2 * anomaly))))
        return math.sin(equation) * min(abs(equation), self.HR(12))

    def apparent_from_local(self, tee_ell):
        """
        Apparent time is the time measured by devices such as a sundial.

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
        used

        (defun local-from-apparent (tee location)
          ;; TYPE (moment location) -> moment
          ;; Local time from sundial time tee at location.
          (- tee (equation-of-time (universal-from-local tee location))))
        """
        return tee - self.equation_of_time(self.universal_from_local(tee))

    #apparent-from-universal
    #universal-from-apparent
    #midnight
    #midday
    #sidereal-from-moment

    #
    # Time and Astronomy (The Year)
    #

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
        angles = (0, self.ANGLE(0, 0, -46.8150), self.ANGLE(0, 0, -0.00059),
                  self.ANGLE(0, 0, 0.001813))
        return self.ANGLE(23, 26, 21.448) + self._poly(c, angles)

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

    #mean-tropical-year
    #mean-sidereal-year

    def solar_longitude(self, tee):
        '''
        used

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
                    self.sigma(
                        (self._COEFFICIENTS, self._MULTIPLIERS, self._ADDENDS),
                        lambda x, y, z, : x * self.sin_degrees(y + (z * c)))))
        return (lambda_ + self.aberration(tee) + self.nutation(tee) + 360)

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
        cap_a = self._poly(c, (124.90, -1934.134, 0.002063))
        car_b = self._poly(c, (201.11, 72001.5377, 0.00057))
        return ((-0.004778 * self.sin_degrees(cap_a)) +
                (-0.0003667 * self.sin_degrees(cap_a)))

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

    #solar-longitude-after
    #season-in-gregorian
    #urbana-winter
    #precession
    #sideread-solar-longitude
    #solar-altitude

    ################################
    # Astronomical Solar Calandars #
    ################################

    def estimate_prior_solar_longitude(self, lambda_, tee):
        """
        used

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

    #
    # Time and Astronomy (The Month)
    #

    #mean-synodic-month

    def nth_new_moon(self, ):
        """
        (defun nth-new-moon (n)
          ;; TYPE integer -> moment
          ;; Moment of n-th new moon after (or before) the new moon
          ;; of January 11, 1. Adapted from "Astronomical Algorithms"
          ;; by Jean Meeus, Willmann-Bell, corrected 2nd edn., 2005.
          (let* ((n0 24724)              ; Months from RD 0 until j2000.
                 (k (- n n0)) ; Months since j2000.
                 (c (/ k 1236.85L0))     ; Julian centuries.
                 (approx (+ j2000
                            (poly c (list 5.09766L0
                                          (* mean-synodic-month
                                             1236.85L0)
                                          0.00015437L0
                                          -0.000000150L0
                                          0.00000000073L0))))
                 (cap-E (poly c (list 1 -0.002516L0 -0.0000074L0)))
                 (solar-anomaly
                  (poly c (deg (list 2.5534L0
                                     (* 1236.85L0 29.10535670L0)
                                     -0.0000014L0 -0.00000011L0))))
                 (lunar-anomaly
                  (poly c (deg (list 201.5643L0 (* 385.81693528L0
                                     1236.85L0)
                                     0.0107582L0 0.00001238L0
                                     -0.000000058L0))))
                 (moon-argument          ; Moon’s argument of latitude.
                  (poly c (deg (list 160.7108L0 (* 390.67050284L0 1236.85L0)
                                     -0.0016118L0 -0.00000227L0
                                     0.000000011L0))))
                 (cap-omega ; Longitude of ascending node.
                  (poly c (deg (list 124.7746L0 (* -1.56375588L0 1236.85L0)
                                     0.0020672L0 0.00000215L0))))
                 (E-factor (list 0 1 0 0 1 1 2 0 0 1 0 1 1 1 0 0 0 0
                                 0 0 0 0 0 0))
                 (solar-coeff (list 0 1 0 0 -1 1 2 0 0 1 0 1 1 -1 2
                                    0 3 1 0 1 -1 -1 1 0))
                 (lunar-coeff (list 1 0 2 0 1 1 0 1 1 2 3 0 0 2 1 2
                                    0 1 2 1 1 1 3 4))
                 (moon-coeff (list 0 0 0 2 0 0 0 -2 2 0 0 2 -2 0 0
                                   -2 0 -2 2 2 2 -2 0 0))
                 (sine-coeff (list -0.40720L0 0.17241L0 0.01608L0 0.01039L0
                                   0.00739L0 -0.00514L0 0.00208L0 -0.00111L0
                                   -0.00057L0 0.00056L0 -0.00042L0 0.00042L0
                                   0.00038L0 -0.00024L0 -0.00007L0 0.00004L0
                                   0.00004L0 0.00003L0 0.00003L0 -0.00003L0
                                   0.00003L0 -0.00002L0 -0.00002L0 0.00002L0))
                (correction
                 (+ (* -0.00017L0 (sin-degrees cap-omega))
                    (sigma ((v sine-coeff)
                            (w E-factor)
                            (x solar-coeff)
                            (y lunar-coeff)
                            (z moon-coeff))
                           (* v (expt cap-E w)
                              (sin-degrees
                               (+ (* x solar-anomaly)
                                  (* y lunar-anomaly)
                                  (* z moon-argument)))))))
                (add-const (list 251.88L0 251.83L0 349.42L0 84.66L0
                                 141.74L0 207.14L0 154.84L0 34.52L0 207.19L0
                                 291.34L0 161.72L0 239.56L0 331.55L0))
                (add-coeff (list 0.016321L0 26.651886L0 36.412478L0
                                 18.206239L0 53.303771L0 2.453732L0
                                 7.306860L0 27.261239L0 0.121824L0
                                 1.844379L0 24.198154L0 25.513099L0
                                 3.592518L0))
                (add-factor (list 0.000165L0 0.000164L0 0.000126L0 0.000110L0
                                  0.000062L0 0.000060L0 0.000056L0 0.000047L0
                                  0.000042L0 0.000040L0 0.000037L0 0.000035L0
                                  0.000023L0))
                (extra
                 (* 0.000325L0
                    (sin-degrees
                     (poly c
                           (deg (list 299.77L0 132.8475848L0 -0.009173L0))))))
                (additional
                 (sigma ((i add-const)
                         (j add-coeff)
                         (l add-factor))
                        (* l (sin-degrees (+ i (* j k)))))))
             (universal-from-dynamical
              (+ approx correction extra additional))))
        """
        return

    #new-moon-before

    def new_moon_at_or_after(self, tee):
        """
        used

        (defun new-moon-at-or-after (tee)
          ;; TYPE moment -> moment
          ;; Moment UT of first new moon at or after tee.
          (let* ((t0 (nth-new-moon 0))
                 (phi (lunar-phase tee))
                 (n (round (- (/ (- tee t0) mean-synodic-month)
                              (/ phi (deg 360))))))
            (nth-new-moon (next k n (>= (nth-new-moon k) tee))))
        """
        t0 = self.nth_new_moon(0)
        phi = self.lunar_phase(tee)
        n = round((tee - t0) / self.MEAN_TROPICAL_YEAR - phi / 360)
        return self.nth_new_moon(self._next(n , self.nth_new_moon(n) >= tee))

    def lunar_longitude(self, tee):
        '''
        (defun lunar-longitude (tee)
          ;; TYPE moment -> angle
          ;; Longitude of moon (in degrees) at moment tee.
          ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
          ;; Willmann-Bell, 2nd edn., 1998, pp. 338-342.
          (let* ((c (julian-centuries tee))
                 (cap-L-prime (mean-lunar-longitude c))
                 (cap-D (lunar-elongation c))
                 (cap-M (solar-anomaly c))
                 (cap-M-prime (lunar-anomaly c))
                 (cap-F (moon-node c))
                 (cap-E (poly c (list 1 -0.002516L0 -0.0000074L0)))
                 (args-lunar-elongation
                  (list 0 2 2 0 0 0 2 2 2 2 0 1 0 2 0 0 4 0 4 2 2 1
                        1 2 2 4 2 0 2 2 1 2 0 0 2 2 2 4 0 3 2 4 0 2
                        2 2 4 0 4 1 2 0 1 3 4 2 0 1 2))
                 (args-solar-anomaly
                  (list 0 0 0 0 1 0 0 -1 0 -1 1 0 1 0 0 0 0 0 0 1 1
                        0 1 -1 0 0 0 1 0 -1 0 -2 1 2 -2 0 0 -1 0 0 1
                        -1 2 2 1 -1 0 0 -1 0 1 0 1 0 0 -1 2 1 0))
                 (args-lunar-anomaly
                  (list 1 -1 0 2 0 0 -2 -1 1 0 -1 0 1 0 1 1 -1 3 -2
                        -1 0 -1 0 1 2 0 -3 -2 -1 -2 1 0 2 0 -1 1 0
                        -1 2 -1 1 -2 -1 -1 -2 0 1 4 0 -2 0 2 1 -2 -3
                        2 1 -1 3))
                 (args-moon-node
                  (list 0 0 0 0 0 2 0 0 0 0 0 0 0 -2 2 -2 0 0 0 0 0
                        0 0 0 0 0 0 0 2 0 0 0 0 0 0 -2 2 0 2 0 0 0 0
                        0 0 -2 0 0 0 0 -2 -2 0 0 0 0 0 0 0))
                 (sine-coeff
                  (list 6288774 1274027 658314 213618 -185116 -114332
                        58793 57066 53322 45758 -40923 -34720 -30383
                        15327 -12528 10980 10675 10034 8548 -7888
                        -6766 -5163 4987 4036 3994 3861 3665 -2689
                        -2602 2390 -2348 2236 -2120 -2069 2048 -1773
                        -1595 1215 -1110 -892 -810 759 -713 -700 691
                        596 549 537 520 -487 -399 -381 351 -340 330
                        327 -323 299 294))
                 (correction
                  (* (deg 1/1000000)
                     (sigma ((v sine-coeff)
                     (w args-lunar-elongation)
                     (x args-solar-anomaly)
                     (y args-lunar-anomaly)
                     (z args-moon-node))
                    (* v (expt cap-E (abs x))
                       (sin-degrees
                        (+ (* w cap-D)
                           (* x cap-M)
                           (* y cap-M-prime)
                           (* z cap-F)))))))
                 (venus (* (deg 3958/1000000)
                           (sin-degrees
                            (+ (deg 119.75L0) (* c (deg 131.849L0))))))
                 (jupiter (* (deg 318/1000000)
                             (sin-degrees
                              (+ (deg 53.09L0)
                                 (* c (deg 479264.29L0))))))
                 (flat-earth
                  (* (deg 1962/1000000)
                     (sin-degrees (- cap-L-prime cap-F)))))
            (mod (+ cap-L-prime correction venus jupiter flat-earth
                    (nutation tee))
                 360)))
        '''
        return

    #mean-lunar-longitude
    #lunar-elongation
    #solar-anomaly
    #lunar-anomaly
    #moon-node
    #lunar-node
    #sidereal-lunar-longitude

    def lunar_phase(self, n):
        """
        (defun lunar-phase (tee)
          ;; TYPE moment -> phase
          ;; Lunar phase, as an angle in degrees, at moment tee.
          ;; An angle of 0 means a new moon, 90 degrees means the
          ;; first quarter, 180 means a full moon, and 270 degrees
          ;; means the last quarter.
          (let* ((phi (mod (- (lunar-longitude tee)
                              (solar-longitude tee))
                           360))
                 (t0 (nth-new-moon 0))
                 (n (round (/ (- tee t0) mean-synodic-month)))
                 (phi-prime (* (deg 360)
                               (mod (/ (- tee (nth-new-moon n))
                                       mean-synodic-month)
                                    1))))
            (if (> (abs (- phi phi-prime)) (deg 180)) ; close call
                phi-prime
              phi)))
        """
        phi = (self.lunar_longitude(tee) - self.solar_longitude(tee)) % 360
        t0 = self.nth_new_moon(0)
        n = round((tee -t0) / self.MEAN_SYNODIC_MONTH)
        phi_prime = (360 * (tee - self.nth_new_moon(n)) /
                     self.MEAN_SYNODIC_MONTH) % 1
        return phi_prime if abs(phi - phi_prime) > 180 else phi

    #lunar-phase-at-or-before
    #lunar-phase-at-or-after
    #new
    #first-quarter
    #full
    #last-quarter
    #lunar-latitude
    #lunar-altitude
    #lunar-distance
    #lunar-parallax
    #topocentric-lunar-altitude

    #
    # Time and Astronomy (Rising and Setting of the Sun and Moon)
    #

    def approx_moment_of_depression(self, tee, alpha, early):
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
        delta = self.declination(tee_prime, 0, self.solar_longitude(tee_prime))
        return (self.tan_degrees(phi) * self.tan_degrees(delta) +
                self.sin_degrees(alpha) / (self.cos_degrees(delta) *
                                           self.cos_degrees(phi)))

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
        bogus = None

        if tee is bogus:
            result = bogus
        elif abs(approx - tee) < self.SEC(30):
            result = tee
        else:
            result = self.moment_of_depression(tee, alpha, early=early)

        return result

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
        used

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

    def refraction(self, tee):
        """
        used

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
        used

        (defun sunset (date location)
          ;; TYPE (fixed-date location) -> moment
          ;; Standard time of sunset on fixed date at location.
          (let* ((alpha (+ (refraction (+ date (hr 18)) location) (mins 16))))
            (dusk date location alpha)))
        """
        alpha = self.refraction(date + self.HR(18)) + self.MINS(16)
        return self.dusk(date, alpha)

    #urbana-sunset
    #cfs-alent
    #jewish-sabbath-ends
    #jewish-dusk
    #observed-lunar-altitude
    #moonrise
    #moonset

    #
    # Time and Astronomy (Times of Day)
    #

    #padua
    #local-zero-hour
    #italian-from-local
    #daytime-temporal-hour
    #nighttime-temporal-hour
    #standard-from-sundial
    #jewish-morning-end
    #alt-asr

    # We bypass all the Lunar Crescent Visability

    #
    # Calendar Basics (Mathematical Notation)
    #

    def radians_from_degrees(self, theta):
        """
        used

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
        return theta * 180 / math.pi

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

    def arctan_degrees(self, y, x):
        """
        (defun arctan-degrees (y x)
          ;; TYPE (real real) -> angle
          ;; Arctangent of y/x in degrees.
          ;; Returns bogus if x and y are both 0.
          (if (and (= x y 0))
              bogus
            (mod
             (if (= x 0)
                 (* (sign y) (deg 90L0))
               (let* ((alpha (degrees-from-radians
                              (atan (/ y x)))))
                 (if (>= x 0)
                      alpha
                   (+ alpha (deg 180L0)))))
              360)))
        """
        if not (x == 0 == y):
            if x == 0:
                result = math.sin(y) * 90
            else:
                alpha = self.degrees_from_radians(math.atan2(y, x))
                result = alpha if x >= 0 else alpha + 180

            result %= 360
        else:
            result = None

        return result

    def arcsin_degrees(self, x):
        """
        used

        (defun arcsin-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arcsine of x in degrees.
          (degrees-from-radians (asin x)))
        """
        assert -1 <= x <= 1, f"The value of x '{x}' must be >= -1 and <= 1."
        return self.degrees_from_radians(math.asin(x))

    def arccos_degrees(self, x):
        """
        (defun arccos-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arccosine of x in degrees.
          (degrees-from-radians (acos x)))
        """
        assert -1 <= x <= 1, f"The value of x '{x}' must be >= -1 and <= 1."
        return self.degrees_from_radians(math.acos(x))

    def fixed_from_moment(self, tee):
        """
        (defun fixed-from-moment (tee)
          ;; TYPE moment -> fixed-date
          ;; Fixed-date from moment tee.
          (floor tee))
        """
        return math.floor(tee)

    @property
    def hour(self):
        """
        (defun hour (clock)
          ;; TYPE clock-time -> hour
          (first clock))
        """
        return self._time[0]

    @property
    def minute(self):
        """
        (defun minute (clock)
          ;; TYPE clock-time -> minute
          (second clock))
        """
        return self._time[1]

    @property
    def seconds(self):
        """
        (defun seconds (clock)
          ;; TYPE clock-time -> second
          (third clock))
        """
        return self._time[2]

    @property
    def microsecond(self):
        """
        Not in the original implementation
        """
        return self._time[3]

    def sigma(self, lists, func):
        """
        used

        (defmacro sigma (list body)
          ;; TYPE (list-of-pairs (list-of-reals->real))
          ;; TYPE -> real
          ;; list is of the form ((i1 l1)...(in ln)).
          ;; Sum of body for indices i1...in
          ;; running simultaneously thru lists l1...ln.
          `(apply `+ (mapcar (function (lambda
                                         ,(mapcar `car list)
                                         ,body))
                             ,@(mapcar `cadr list))))
        """
        # Ensure all lists have the same length
        assert len(set(len(lst) for lst in lists)) == 1, (
            "Lists must have the same length")
        return sum(func(*e) for e in zip(*lists))

    def _poly(self, x, a):
        """
        used

        (defun poly (x a)
          ;; TYPE (real list-of-reals) -> real
          ;; Sum powers of x with coefficients (from order 0 up) in list a.
          (if (equal a nil)
              0
            (+ (first a) (* x (poly x (rest a))))))
        """
        return 0 if not a else a[0] + (x * self._poly(x, a[1:]))

    def _next(self, initial, condition):
        """
        used

        (defmacro next (index initial condition)
          ;; TYPE (* integer (integer->boolean)) -> integer
          ;; First integer greater or equal to initial such that
          ;; condition holds.
          ‘(loop for ,index from ,initial
                 when ,condition
                 return ,index))
        """
        idx = initial

        while not condition(idx):
            if idx >= initial: break
            idx += 1

        return idx

    def _final(self, initial, condition):
        """
        (defmacro final (index initial condition)
          ;; TYPE (* integer (integer->boolean)) -> integer
          ;; Last integer greater or equal to initial such that
          ;; condition holds.
          `(loop for ,index from ,initial
                 when (not ,condition)
                 return (1- ,index)))
        """
        return (initial - 1 if not condition(initial)
                else self._final(initial + 1, condition))
