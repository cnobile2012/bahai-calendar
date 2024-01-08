# -*- coding: utf-8 -*-
#
# bahai_calendar/base_calendar.py
#
__docformat__ = "restructuredtext en"


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

    def __init__(self):
        pass   

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
        if len(set(len(lst) for lst in lists)) != 1:
            raise ValueError("Lists must have the same length")

        # Zip lists together to iterate through corresponding
        #elements simultaneously
        zipped_lists = zip(*lists)
        # Perform the summation using a lambda function and reduce
        return reduce(lambda acc, values: acc + eval(
            body, dict(zip(indices, values))), zipped_lists, 0)
