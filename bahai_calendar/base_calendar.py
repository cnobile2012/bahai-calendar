# -*- coding: utf-8 -*-
#
# bahai_calendar/base_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from functools import reduce
from operator import mul

from bahai_calendar.julian_period import JulianPeriod


class BaseCalendar(JulianPeriod):
    """
    Basic functionality used with all calenders.

    R.D. = Fixed date—elapsed days since the onset of Monday, January 1, 1
           (Gregorian)
    U.T. = Mean solar time at Greenwich, England (0◦ meridian), reckoned
           from midnight; sometimes g.m.t., Greenwich Mean Time
    Moment = We call an r.d. that has a fractional part giving the time of
             day a “moment.”

    Transformations between Time Systems:
    https://gssc.esa.int/navipedia/index.php/Transformations_between_Time_Systems
    """
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
    ANGLE = lambda self, d, m, s: d + (m + (s / 60)) / 60 # 0 - 360

    #(defun amod (x y)
    #  ;; TYPE (integer nonzero-integer) -> integer
    #  ;; The value of ($x$ mod $y$) with $y$ instead of 0.
    #  (+ y (mod x (- y))))
    AMOD = lambda self, x, y: y + x % -y

    #(defun mod3 (x a b)
    #  ;; TYPE (real real real) -> real
    #  ;; The value of x shifted into the range [a..b). Returns x if a=b.
    #  (if (= a b)
    #      x
    #    (+ a (mod (- x a) (- b a)))))
    MOD3 = lambda self, x, a, b : x if a == b else (
        a + math.fmod((x - a), (b - a)))

    #(defconstant mean-synodic-month
    #  ;; TYPE duration
    #  29.530588861L0)
    MEAN_SYNODIC_MONTH = 29.530588861

    #(defun time-from-moment (tee)
    #  ;; TYPE moment -> time
    #  ;; Time from moment tee.
    #  (mod tee 1))
    TIME_FROM_MOMENT = lambda self, tee: math.fmod(tee, 1)

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

    PARTIAL_DAY_TO_HOURS = lambda self, x: (x % 1) * 24
    PARTIAL_HOUR_TO_MINUTE = lambda self, x: (x % 1) * 60
    PARTIAL_MINUTE_TO_SECOND = PARTIAL_HOUR_TO_MINUTE

    MEAN_TROPICAL_YEAR = 365.242189
    MEAN_SIDEREAL_YEAR = 365.25636

    MORNING = True
    EVENING = False
    # Seasons set to degrees.
    SPRING = 0
    SUMMER = 90
    AUTUMN = 180
    WINTER = 270

    def __init__(self):
        self._time = None

    def parse_datetime(self, dt:datetime.datetime) -> None:
        self.time_representation = (dt.hour, dt.minute, dt.second)

    @property
    def time_representation(self):
        return self._time

    @time_representation.setter
    def time_representation(self, representation):
        self._time = representation

    #
    # Time and Astronomy (Time)
    #

    #greenwich
    #acre
    #direction
    #arctan

    def zone_from_longitude(self, phi):
        """
        used

        (defun zone-from-longitude (phi)
          ;; TYPE circle -> duration
          ;; Difference between UT and local mean time at longitude
          ;; phi as a fraction of a day.
          (/ phi (deg 360)))
        """
        return phi / 360

    def universal_from_local(self, tee_ell):
        """
        used

        (defun universal-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Universal time from local tee_ell at location.
          (- tee_ell (zone-from-longitude (longitude location))))
        """
        return tee_ell - self.zone_from_longitude(self.longitude)

    def local_from_universal(self, tee_rom_u):
        """
        (defun local-from-universal (tee_rom-u location)
          ;; TYPE (moment location) -> moment
          ;; Local time from universal tee_rom-u at location.
          (+ tee_rom-u (zone-from-longitude (longitude location))))
        """
        return tee_rom_u + self.zone_from_longitude(self.longitude)

    def standard_from_universal(self, tee_rom_u):
        """
        (Fixed bug in LISP code)

        (defun standard-from-universal (tee_rom-u location)
          ;; TYPE (moment location) -> moment
          ;; Standard time from tee_rom-u in universal time at location.
          (+ tee_rom-u (zone location)))
        """
        return tee_rom_u + self.HR(self.zone)

    def universal_from_standard(self, tee_rom_s):
        """
        used (Fixed bug in LISP code)

        (defun universal-from-standard (tee_rom-s location)
          ;; TYPE (moment location) -> moment
          ;; Universal time from tee_rom-s in standard time at location.
          (- tee_rom-s (zone location)))
        """
        return tee_rom_s - self.HR(self.zone)

    def standard_from_local(self, tee_ell):
        """
        used

        (defun standard-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Standard time from local tee_ell at location.
          (standard-from-universal (universal-from-local tee_ell location)
          location))
        """
        return self.standard_from_universal(self.universal_from_local(tee_ell))

    def local_from_standard(self, tee_rom_s):
        """
        (defun local-from-standard (tee_rom-s location)
          ;; TYPE (moment location) -> moment
          ;; Local time from standard tee_rom-s at location.
          (local-from-universal
           (universal-from-standard tee_rom-s location)
           location))
        """
        return self.local_from_universal(self.universal_from_standard(
            tee_rom_s))

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
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()

        year = gc.gregorian_year_from_fixed(math.floor(tee))
        y2000 = year - 2000
        c = gc.gregorian_date_difference(
            (1900, gc.JANUARY, 1), (year, gc.JULY, 1)) / 36525

        if 2051 <= year <= 2150:
            result = 1/86400 * (-20 + (32 * (((year - 1820) / 100) ** 2))
                                + 0.5628 * (2150 - year))
        elif 2006 <= year <= 2050:
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
        lam = self._poly(c, (280.46645, 36000.76983, 0.0003032))
        anomaly = self._poly(c, (357.52910, 35999.05030, -0.0001559,
                                 -0.00000048))
        eccentricity = self._poly(c, (0.016708617, -0.000042037,
                                      -0.0000001236))
        varepsilon = self.obliquity(tee)
        y = self.tan_degrees(varepsilon / 2) ** 2
        equation = ((0.5 / math.pi) *
                    ((y * self.sin_degrees(2 * lam)) +
                     (-2 * eccentricity * self.sin_degrees(anomaly)) +
                     (4 * eccentricity * y * self.sin_degrees(anomaly) *
                      self.cos_degrees(2 * lam)) +
                     (-0.5 * y * y * self.sin_degrees(4 * lam)) +
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
        Obliquity is the inclination of the Earth’s equator. The value
        of this inclination, called the obliquity, varies in a
        100000-year cycle, ranging from 24.2◦ 10000 years ago to 22.6◦
        in another 10000 years.

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

    def declination(self, tee, lat, lon):
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
        return ((self.sin_degrees(lat) * self.cos_degrees(varepsilon)) +
                (self.cos_degrees(lat) * self.sin_degrees(varepsilon)
                 * self.sin_degrees(lon)))

    #mean-tropical-year
    #mean-sidereal-year

    def solar_longitude(self, moment):
        jd = self.jd_from_moment(moment)
        t = (jd - self.J2000) / 365250
        func = lambda a, b, c: a * math.cos(b + c * t)
        l0_a = (175347046, 3341656, 34894, 3497, 3418, 3136, 2676, 2343, 1324,
                1273, 1199, 990, 902, 857, 780, 753, 505, 492, 357, 317, 284,
                271, 243, 206, 205, 202, 156, 132, 126, 115, 103, 102, 102, 99,
                98, 86, 85, 85, 80, 79, 75, 74, 74, 70, 62, 61, 57, 56, 56, 52,
                52, 51, 49, 41, 41, 39, 37, 37, 36, 36, 33, 30, 30, 25)
        l0_b = (0, 4.6692568, 4.62610, 2.7441, 2.8289, 3.6277, 4.4181, 6.1352,
                0.7425, 2.0371, 1.1096, 5.233, 2.045, 3.508, 1.179, 2.533,
                4.583, 4.205, 2.920, 5.849, 1.899, 0.315, 0.345, 4.806, 1.869,
                2.548, 0.833, 3.411, 1.083, 0.645, 0.636, 0.976, 4.267, 6.21,
                0.68, 5.98, 1.30, 3.67, 1.81, 3.04, 1.76, 3.50, 4.68, 0.83,
                3.98, 1.82, 2.78, 4.39, 3.47, 0.19, 1.33, 0.28, 0.49, 5.37,
                2.40, 6.17, 6.04, 2.57, 1.71, 1.78, 0.59, 0.44, 2.74, 3.16)
        l0_c = (0, 6283.075850, 12566.15171, 5753.3849, 35231, 77713.7715,
                7860.4194, 3930.2097, 11506.7698, 529.6910, 1577.3435,
                5884.927, 26.298, 398.149, 5223.694, 5507.553, 18849.228,
                775.523, 0.067, 11791.629, 796.298, 10977.079, 5486.778,
                2544.314, 5573.143, 6069.777, 213.299, 2942.463, 20.775,
                0.980, 4694.003, 15720.839, 7.114, 2146.17, 155.42, 161000.69,
                6275.96, 71430.70, 17260.15, 12036.46, 5088.63, 3154.69,
                801.82, 9437.76, 8827.39, 7084.90, 6286.60, 14143.50, 6279.55,
                12139.55, 1748.02, 5856.48, 1194.45, 8429.24, 19651.05,
                10447.37, 10213.29, 1059.38, 2352.87, 6812.77, 17789.85,
                83996.85, 1349.87, 4690.48)
        l0 = self._sigma((l0_a, l0_b, l0_c), func)
        b0_a = (280, 102, 80, 44, 32)
        b0_b = (3.199, 5.422, 3.88, 3.70, 4.00)
        b0_c = (84334.662, 5507.553, 5223.69, 2352.87, 1577.34)
        b0 = self._sigma((b0_a, b0_b, b0_c), func)
        r0_a = (1000013989, 1670700, 13956, 3084, 1628, 1576, 925, 542, 472,
                346, 329, 307, 243, 212, 186, 175, 110, 98, 86, 86, 65, 63,
                57, 56, 49, 47, 45, 43, 39, 38, 37, 37, 36, 35, 33, 32, 32,
                28, 28, 26)
        r0_b = (0, 30984635, 3.05525, 5.1985, 1.1739, 2.8469, 5.453, 4.564,
                3.661, 0.964, 5.90, 0.299, 4.273, 5.847, 5.022, 3.012, 5.055,
                0.89, 5.69, 1.27, 0.27, 0.92, 2.01, 5.24, 3.25, 2.58, 5.54,
                6.01, 5.36, 2.39, 0.83, 4.90, 1.67, 1.84, 0.24, 0.18, 1.78,
                1.21, 1.90, 4.59)
        r0_c = (0, 6283.075850, 12566.15170, 77713.7715, 5753.3849, 7860.4194,
                11506.770, 3930.210, 5884.927, 5507.553, 5223.694, 5573.143,
                11790.629, 1577.344, 10977.079, 18849.228, 5486.778, 6069.78,
                15720.84, 161000.69, 17260.15, 529.69, 83996.85, 71430.70,
                2544.31, 775.52, 9437.76, 6275.96, 4694.00, 8827.39, 19651.05,
                12139.55, 12036.46, 2942.46, 7084.90, 5088.63, 398.15, 6286.60,
                6279.55, 10447.39)
        r0 = self._sigma((r0_a, r0_b, r0_c), func)

        return l0, b0, r0

    def alt_solar_longitude(self, tee):
        """
        Find the solar longitude in degrees (0 - 360).

        See the following links:
        https://aa.usno.navy.mil/faq/sun_approx
        https://squarewidget.com/solar-coordinates/
        """
        jd = self.jd_from_moment(tee)
        d = jd - self.jd_from_moment(self.RD_J2000)
        g = self.coterminal_angle(357.529 + 0.98560028 * d)
        q = self.coterminal_angle(280.459 + 0.98564736 * d)
        return q + 1.915 * math.sin(g) + 0.02 * math.sin(2 * g)

    def nutation(self, tee):
        """
        used

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
        cap_b = self._poly(c, (201.11, 72001.5377, 0.00057))
        return (-0.004778 * self.sin_degrees(cap_a) +
                -0.0003667 * self.sin_degrees(cap_b))

    def aberration(self, tee):
        """
        used

        (defun aberration (tee)
          ;; TYPE moment -> circle
          ;; Aberration at moment tee.
          (let* ((c       ; moment in Julian centuries
                  (julian-centuries tee)))
            (- (* (deg 0.0000974L0)
                  (cos-degrees (+ (deg 177.63L0) (* (deg 35999.01848L0) c))))
               (deg 0.005575L0))))
        """
        c = self.julian_centuries(tee)
        return (0.0000974 * self.cos_degrees(177.63 + 35999.01848 * c)
                - 0.005575)

    #solar-longitude-after
    #season-in-gregorian
    #urbana-winter
    #precession
    #sideread-solar-longitude
    #solar-altitude

    # Here α is the sun’s right ascension, δ is its declination,
    # and H is the local sidereal hour angle.
    # Greek letters phi (φ) for latitude and lambda (λ) for longitude.

    ################################
    # Astronomical Solar Calandars #
    ################################

    def estimate_prior_solar_longitude(self, lam:int, tee:int) -> float:
        """
        used

        https://farside.ph.utexas.edu/books/Syntaxis/Almagest/node34.html

        (defun estimate-prior-solar-longitude (lambda tee)
          ;; TYPE (season moment) -> moment
          ;; Approximate moment at or before tee when solar
          ;; longitude just exceeded lambda degrees.
          (let* ((rate            ; Mean change of one degree.
                  (/ mean-tropical-year (deg 360)))
                 (tau             ; First approximation.
                  (- tee
                     (* rate (mod (- (solar-longitude tee)
                                     lambda)
                              360))))
                 (cap-Delta       ; Difference in longitude.
                  (mod3 (- (solar-longitude tau) lambda)
                        -180 180)))
            (min tee (- tau (* rate cap-Delta)))))
        """
        rate = self.MEAN_TROPICAL_YEAR / 360 # always = 1.01456163611111111111
        tau = tee - rate * (math.fmod(
            (self.alt_solar_longitude(tee) - lam), 360))
        cap_delta = self.MOD3(self.alt_solar_longitude(tau) - lam, -180, 180)
        return min(tee, tau - rate * cap_delta)

    def approx_julian_day_for_equinoxes_or_solstices(self, g_year:int,
                                                     lam:int) -> float:
        """
        Find the approximate Julian day for the equinoxes or solstices.

        See: Astronomical Algorithms, 1998, by Jean Meeus Chapter 27 pg 177
        """
        if g_year <= 1000:
            y = g_year / 1000

            if lam == self.SPRING:
                jde = (1721139.29189 + 365242.13740 * y + 0.06134 * y**2 +
                       0.00111 * y**3 - 0.00071 * y**4)
            elif lam == self.SUMMER:
                jde = (1721233.25401 + 365241.72562 * y - 0.05323 * y**2 +
                       0.00907 * y**3 + 0.00025 * y**4)
            elif lam == self.AUTUMN:
                jde = (1721325.70455 + 365242.49558 * y - 0.11677 * y**2 -
                       0.00297 * y**3 + 0.00074 * y**4)
            else: # lam == self.WINTER:
                jde = (1721414.39987 + 365242.88257 * y - 0.00769 * y**2 -
                       0.00933 * y**3 - 0.00006 * y**4)
        else:
            y = (g_year - 2000) / 1000

            if lam == self.SPRING:
                jde = (2451623.80984 + 365242.37404 * y + 0.05169 * y**2 -
                       0.00411 * y**3 - 0.00057 * y**4)
            elif lam == self.SUMMER:
                jde = (2451716.56767 + 365241.62603 * y + 0.00325 * y**2 +
                       0.00888 * y**3 - 0.00030 * y**4)
            elif lam == self.AUTUMN:
                jde = (2451810.21715 + 365242.01767 * y - 0.11575 * y**2 +
                       0.00337 * y**3 + 0.00078 * y**4)
            else: # lam == self.WINTER:
                jde = (2451900.05952 + 365242.74049 * y - 0.06223 * y**2 -
                       0.00823 * y**3 + 0.00032 * y**4)

        return jde


    def find_moment_of_equinoxes_or_solstices(self, tee:int,
                                              lam:int=SPRING) -> float:
        """
        With the rd moment and time of year find an equinoxe or solstice.
        """
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()
        year = gc.gregorian_year_from_fixed(tee)
        jde = self.approx_julian_day_for_equinoxes_or_solstices(year, lam)
        t = (jde - self.J2000) / 36525
        w = 35999.373 * t - 2.47
        dl = 1 + 0.0334 * self.cos_degrees(w + 0.0007) * self.cos_degrees(2*w)
        a0 = (485, 203, 199, 182, 156, 136, 77, 74, 70, 58, 52, 50,
              45, 44, 29, 18, 17, 16, 14, 12, 12, 12, 9, 8)
        b0 = (324.96, 337.23, 342.08, 27.85, 73.14, 171.52, 222.54, 296.72,
              243.58, 119.81, 297.17, 21.02, 247.54, 325.15, 60.93, 155.12,
              288.79, 198.04, 199.76, 95.39, 287.11, 320.81, 227.73, 15.45)
        c0 = (1934.136, 32964.467, 20.186, 445267.112, 45036.886, 22518.443,
              65928.934, 3034.906, 9037.513, 33718.147, 150.678, 2281.226,
              29929.562, 31555.956, 4443.417, 67555.328, 4562.452, 62894.029,
              31436.921, 14577.848, 31931.756, 34777.259, 1222.114, 16859.074)
        s = self._sigma(
            (a0, b0, c0), lambda a, b, c: a * self.cos_degrees(b + c * t))
        return self.moment_from_jd(jde + (0.00001 * s) / dl)

    #
    # Time and Astronomy (The Month)
    #

    #mean-synodic-month
    #nth-new-moon
    #new-moon-before
    #new-moon-at-or-after
    #lunar-longitude
    #mean-lunar-longitude
    #lunar-elongation
    #solar-anomaly
    #lunar-anomaly
    #moon-node
    #lunar-node
    #sidereal-lunar-longitude
    #lunar-phase
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

    def approx_moment_of_depression(self, tee:float, alpha:float,
                                    early:bool) -> float:
        """
        used

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
           (if (<= (abs value) 1)          ; Event occurs
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
                 (delta                 ; Declination of sun.
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
        delta = self.declination(tee_prime, 0,
                                 self.alt_solar_longitude(tee_prime))
        return (self.tan_degrees(phi) * self.tan_degrees(delta) +
                self.sin_degrees(alpha) /
                (self.cos_degrees(delta) * self.cos_degrees(phi)))

    def moment_of_depression(self, approx, alpha, early=EVENING):
        """
        used

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

        if tee is not None:
            if abs(approx - tee) < self.SEC(30):
                result = tee
            else:
                result = self.moment_of_depression(tee, alpha, early=early)
        else:
            result = None

        return result

    def dawn(self, date:float, alpha:float) -> float:
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
                                           self.MORNING)
        return self.standard_from_local(result) if result else None

    def dusk(self, date:float, alpha:float) -> float:
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
        return self.standard_from_local(result) if result else None

    def refraction(self) -> float:
        """
        used

        Refraction is the bending of the sun’s light by the Earth’s
        atmosphere.

        (defun refraction (tee location)
          ;; TYPE (moment location) -> half-circle
          ;; Refraction angle at moment tee at location.
          ;; The moment is not used.
          (let* ((h (max (mt 0) (elevation location)))
                 (cap-R (mt 6.372d6)) ; Radius of Earth.
                 (dip                 ; Depression of visible horizon.
                  (arccos-degrees (/ cap-R (+ cap-R h)))))
            (+ (mins 34) dip (* (secs 19) (sqrt h)))))
        """
        h = max(0, self.elevation)
        cap_r = 6.372e6 # Wikipedia average is 6371 Km
        dip = self.arccos_degrees(cap_r / (cap_r + h))
        return self.MINS(34) + dip + self.SECS(19) * math.sqrt(h)

    def sunrise(self, date):
        """
        (defun sunrise (date location)
          ;; TYPE (fixed-date location) -> moment
          ;; Standard time of sunrise on fixed date at location.
          (let* ((alpha (+ (refraction (+ date (hr 6)) location) (mins 16))))
            (dawn date location alpha)))
        """
        alpha = self.refraction() + self.MINS(16)
        return self.dawn(date, alpha)

    def sunset(self, day):
        """
        used
        This is based on  standard time which itself is based on location.

        (defun sunset (date location)
          ;; TYPE (fixed-date location) -> moment
          ;; Standard time of sunset on fixed date at location.
          (let* ((alpha (+ (refraction (+ date (hr 18)) location) (mins 16))))
            (dusk date location alpha)))
        """
        alpha = self.refraction() + self.MINS(16)
        return self.dusk(day, alpha)

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
    # Both of the below function have equivelent Python builtin functions.
    #
    #    (defun radians-from-degrees (theta)
    #      ;; TYPE real -> radian
    #      ;; Convert angle theta from degrees to radians.
    #      (* (mod theta 360) pi 1/180))
    #
    #    (defun degrees-from-radians (theta)
    #      ;; TYPE radian -> angle
    #      ;; Convert angle theta from radians to degrees.
    #      (mod (/ theta pi 1/180) 360))

    def sin_degrees(self, theta:float) -> float:
        """
        (defun sin-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Sine of theta (given in degrees).
          (sin (radians-from-degrees theta)))
        """
        return math.sin(math.radians(theta))

    def cos_degrees(self, theta:float) -> float:
        """
        (defun cos-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Cosine of theta (given in degrees).
          (cos (radians-from-degrees theta)))
        """
        return math.cos(math.radians(theta))

    def tan_degrees(self, theta:float) -> float:
        """
        (defun tan-degrees (theta)
          ;; TYPE angle -> real
          ;; Tangent of theta (given in degrees).
          (tan (radians-from-degrees theta)))
        """
        return math.tan(math.radians(theta))

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
        assert not (x == 0 == y), (
            f"The value of x '{x}' and y '{y}' must not be x == 0 == y.")

        if x == 0:
            result = math.sin(y) * 90
        else:
            alpha = math.degrees(math.atan2(y, x))
            result = alpha if x >= 0 else alpha + 180

        return math.fmod(result, 360)

    def arcsin_degrees(self, x):
        """
        used

        (defun arcsin-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arcsine of x in degrees.
          (degrees-from-radians (asin x)))
        """
        assert -1 <= x <= 1, f"The value of x '{x}' must be >= -1 and <= 1."
        return math.degrees(math.asin(x))

    def arccos_degrees(self, x):
        """
        (defun arccos-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arccosine of x in degrees.
          (degrees-from-radians (acos x)))
        """
        assert -1 <= x <= 1, f"The value of x '{x}' must be >= -1 and <= 1."
        return math.degrees(math.acos(x))

    def fixed_from_moment(self, tee:float) -> float:
        """
        (defun fixed-from-moment (tee)
          ;; TYPE moment -> fixed-date
          ;; Fixed-date from moment tee.
          (floor tee))
        """
        return math.floor(tee)

    def _sigma(self, lists:tuple, func:object) -> float:
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

    def _poly(self, x:float, a:list) -> float:
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

    def _next(self, initial:int, func:object) -> int:
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
        while not func(initial):
            initial += 1

        return initial

    def _final(self, initial:int, func:object) -> int:
        """
        (defmacro final (index initial condition)
          ;; TYPE (* integer (integer->boolean)) -> integer
          ;; Last integer greater or equal to initial such that
          ;; condition holds.
          `(loop for ,index from ,initial
                 when (not ,condition)
                 return (1- ,index)))
        """
        return (initial - 1 if not func(initial)
                else self._final(initial + 1, func))

    def _to_radix(self, x:int, b:tuple, c:tuple=()):
        """
        (defun to-radix (x b &optional c)
          ;; TYPE (real list-of-rationals list-of-rationals)
          ;; TYPE -> list-of-reals
          ;; The radix notation corresponding to x
          ;; with base b for whole part and c for fraction.
          (if (null c)
              (if (null b)
                  (list x)
            (append (to-radix (quotient x (nth (1- (length b)) b))
                              (butlast b) nil)
                    (list (mod x (nth (1- (length b)) b)))))
           (to-radix (* x (apply ’* c)) (append b c))))
        """
        if not c:
            if not b:
                result = (x,)
            else:
                result = self._to_radix(self.QUOTIENT(x, b[-1]),
                                        b[:-1]) + (math.fmod(x, b[-1]),)
        else:
            result = self._to_radix(x * reduce(mul, c), b + c)

        return result

    #
    # Additional methods
    #

    def decimal_from_dms(self, degrees:int, minutes:int, seconds:float,
                         direction:str) -> float:
        '''
        Coordinantes in degrees, minutes, and seconds.
        The Shrine of Baha’u’llah: 32°56’36.86″N, 35° 5’30.38″E
        The Shrine of The Bab: 32°48’52.49″N, 34°59’13.91″E
        The Guardian’s Resting Place (not 3D): 51°37’21.85″N, 0°08’35.57″W

        ****

        Convert degrees, minutes, and seconds to a decimal.

        Degrees, minutes, and seconds to a decimal coordinant:

        1. Add the degrees to the minutes divided by 60
        2. Add the seconds divided by (60 x 60), which is 3600

        Example: To convert 35° 20′ 35", the answer is
                 35 + (20/60) + (35/3600) = 35.34306 degrees.

        :param degrees: The degree part of the coordinats.
        :type degrees: int
        :param minutes: The minute part of the coordinate.
        :type minutes: int
        :param seconds: The second part of the coordinate.
        :param direction: The direction part of the coordinate which can be
                          any of the following N, S, E, W in upper or lower
                          case.
        :type direction: str
        :return: latitude and longitude
        :rtype: tuple
        '''
        dirs = ('N', 'S', 'E', 'W')
        assert direction.upper() in dirs, (
            f"The 'direction' argument must be one of {dirs}")
        # Remove the minus sign iof it exists.
        degrees = -degrees if degrees < 0 else degrees
        decimal = degrees + (minutes / 60) + (seconds / 3600)
        # Adjust the sign based on the direction.
        return -decimal if direction.upper() in ['S', 'W'] else decimal

    def dms_from_decimal(self, coord:float, direction:str) -> tuple:
        """
        Convert a decimal degree into degrees, minutes, and seconds.

        :param coord: The decimal coordinant.
        :type coord: float
        :param direction: The direction part of the coordinate which can be
                          any of the following N, S, E, W in upper or lower
                          case.
        :type direction: str
        :return: The degree, minute, second, and direction for of the
                 coordinate.
        :rtype: tuple
        """
        dirs = ('LATITUDE', 'LONGITUDE')
        direction = direction.upper()
        size = len(direction)
        assert size >= 3 and any([d.startswith(direction) for d in dirs]), (
            f"The direction argument must be one of {dirs}")
        # degrees
        degrees = int(abs(coord))
        # minutes
        minutes = int((abs(coord) - degrees) * 60)
        # seconds
        seconds = (abs(coord) - degrees - (minutes / 60)) * 3600

        if coord < 0:
            direc = 'S' if direction == 'LAT' else 'W'
        else:
            direc = 'N' if direction == 'LAT' else 'E'

        return degrees, minutes, seconds, direc

    def coterminal_angle(self, value:float) -> float:
        """
        Find a Coterminal Angle.
        """
        value = math.fmod(value, 360)
        return value + 360 if value < 0 else value

