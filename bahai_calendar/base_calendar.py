# -*- coding: utf-8 -*-
#
# bahai_calendar/base_calendar.py
#
__docformat__ = "restructuredtext en"

import math


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

    # (defun poly (x a)
    #  ;; TYPE (real list-of-reals) -> real
    #  ;; Sum powers of x with coefficients (from order 0 up)
    #  ;; in list a.
    #  (if (equal a nil)
    #      0
    #    (+ (first a) (* x (poly x (rest a))))))
    POLY = lambda self, x, a: 0 if not a else a[0] + (x * POLY(x, a[1:]))

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
    #  ;; Identity function for fixed dates/moments. If internal
    #  ;; timekeeping is shifted, change epoch to be RD date of
    #  ;; origin of internal count. epoch should be an integer.
    #  (let* ((epoch 0))
    #    (- tee epoch)))
    # Nothing is implemented for this.

    def __init__(self):
        self._time = []

    def time_of_day(self, hour, minute, second, microsecond):
        """
        (defun time-of-day (hour minute second)
          ;; TYPE (hour minute second) -> clock-time
          (list hour minute second))
        """
        #self._time[:] = (hour, minute, second, microsecond)
        return (hour, minute, second, microsecond)

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
        return

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
