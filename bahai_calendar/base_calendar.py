# -*- coding: utf-8 -*-
#
# bahai_calendar/base_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime


class BaseCalender:
    """
    Basic functionality used with all calenders.
    """
    # Build out some lambdas
    HR = lambda self, x: x / 24
    MN = lambda self, x: x / 24 / 60
    SEC = lambda self, x: x / 24 / 60 / 60
    MINS = lambda self, x: x / 60
    SECS = lambda self, x: x / 3600
    ANGLE = lambda self, d, m, s: d + (m + (s / 60))
    MOD3 = lambda self, x, a, b : x if a == b else a + (x - a) % (b - a)

    # (defun quotient (m n)
    #   ;; TYPE (real nonzero-real) -> integer
    #   ;; Whole part of m/n.
    #   (floor m n))
    QUOTIENT = lambda self, m, n: int(math.floor(m / n))

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
    # Nothing is implemented for this.

    # Lists for solar_longitude().
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
        self._time = []

    def parse_datetime(self, dt:datetime.datetime) -> None:
        self._time[:] = (dt.hour, dt.minute, dt.second)

    def new_moon_at_or_after(self, tee):
        """
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
        return

    def fixed_from_moment(self, tee):
        """
        (defun fixed-from-moment (tee)
          ;; TYPE moment -> fixed-date
          ;; Fixed-date from moment tee.
          (floor tee))
        """
        return math.floor(tee)

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

    def julian_centuries(self, tee):
        """
        (defun julian-centuries (tee)
          ;; TYPE moment -> century
          ;; Julian centuries since 2000 at moment tee.
          (/ (- (dynamical-from-universal tee) j2000) 36525))
        """
        return (self.dynamical_from_universal(tee) - 2000) / 36525

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

    def poly(self, x, a):
        """
        (defun poly (x a)
          ;; TYPE (real list-of-reals) -> real
          ;; Sum powers of x with coefficients (from order 0 up) in list a.
          (if (equal a nil)
              0
            (+ (first a) (* x (poly x (rest a))))))
        """
        return 0 if not a else a[0] + (x * self.poly(x, a[1:]))

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

    def next_index(self, initial, to, condition):
        """
        (defmacro next (index initial condition)
          ;; TYPE (* integer (integer->boolean)) -> integer
          ;; First integer greater or equal to initial such that
          ;; condition holds.
          `(loop for ,index from ,initial
                 when ,condition
                 return ,index))
        """
        for idx in range(initial, initial + to):
            if condition(idx):
                break
            else:
                idx = None

        return idx  # Return the index if condition is met

    def final_index(self, initial, to, condition):
        """
        (defmacro final (index initial condition)
          ;; TYPE (* integer (integer->boolean)) -> integer
          ;; Last integer greater or equal to initial such that
          ;; condition holds.
          `(loop for ,index from ,initial
                 when (not ,condition)
                 return (1- ,index)))
        """
        for idx in range(initial, initial + to):
            if not condition(idx):
                idx -= 1
                break
            else:
                idx = None

        return idx  # Return the index if condition is met

    def sigma(self, list_of_pairs, body):
        """
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
        indices, lists = zip(*list_of_pairs)
        # Ensure all lists have the same length
        assert len(set(len(lst) for lst in lists)) == 1, (
            "Lists must have the same length")
        # Zip lists together to iterate through corresponding
        #elements simultaneously
        zipped_lists = zip(*lists)
        # Perform the summation using a lambda function and reduce
        return reduce(lambda acc, values: acc + eval(
            body, dict(zip(indices, values))), zipped_lists, 0)
