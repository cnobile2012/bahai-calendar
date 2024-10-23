#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/badi_jd_tests.py
#

import os
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar, datetime


class DateTests(BahaiCalendar):
    TRAN_COFF = 1843
    # Equinox and Solstices, Perihelion, and Aphelion
    # https://www.timeanddate.com/sun/@112931?month=3&year=1844
    # The site below is where I've gotten the Vernal Equinox data it uses
    # the 4, 100, and 400 algorithm, so we must also. The 4 and 128 algorithm
    # is more accurate, but I've not found Vernal Equinox data that uses it.
    # https://data.giss.nasa.gov/modelE/ar5plots/srvernal.html
    # https://aa.usno.navy.mil/data/Earth_Seasons
    # https://www.astropixels.com/ephemeris/soleq2001.html
    # https://stellafane.org/misc/equinox.html
    #------------------------------------------------------------------------
    # Julian Period day into:
    # https://aa.usno.navy.mil/data/JulianDate
    # -----------------------------------------------------------------------
    # Sun rise and set info:
    # https://gml.noaa.gov/grad/solcalc/
    # https://aa.usno.navy.mil/data/RS_OneYear
    # https://www.sunrisesunset.com/England/GreaterLondon/Greenwich.asp
    # Tehran: 35.682376, 51.285817 USED
    ########################################################
    # Nur Mazandaran Province, Iran (City Center) NOT USED #
    # Nur: 36.569336, 52.0050234 NOT USED                  #
    ########################################################
    # I use coordinates and the sunset in the city of Tehran to determine the
    # yearly Badi epochs. Below are the Gregorian dates of the Vernal Equinox.
    VE_0001_1582 = (
        (   1, 3, 20, 13, 19), (   2, 3, 20, 19,  8), (   3, 3, 21,  0, 58),
        (   4, 3, 20,  6, 47), (   5, 3, 20, 12, 36), (   6, 3, 20, 18, 25),
        (   7, 3, 21,  0, 14), (   8, 3, 20,  6,  4), (   9, 3, 20, 11, 53),
        (  10, 3, 20, 17, 42), (  11, 3, 20, 23, 31), (  12, 3, 20,  5, 20),
        (  13, 3, 20, 11, 10), (  14, 3, 20, 16, 59), (  15, 3, 20, 22, 48),
        (  16, 3, 20,  4, 37), (  17, 3, 20, 10, 26), (  18, 3, 20, 16, 16),
        (  19, 3, 20, 22,  5), (  20, 3, 20,  3, 54), (  21, 3, 20,  9, 43),
        (  22, 3, 20, 15, 32), (  23, 3, 20, 21, 22), (  24, 3, 20,  3, 11),
        (  25, 3, 20,  9,  0), (  26, 3, 20, 14, 49), (  27, 3, 20, 20, 38),
        (  28, 3, 20,  2, 28), (  29, 3, 20,  8, 17), (  30, 3, 20, 14,  6),
        (  31, 3, 20, 19, 55), (  32, 3, 20,  1, 44), (  33, 3, 20,  7, 34),
        (  34, 3, 20, 13, 23), (  35, 3, 20, 19, 12), (  36, 3, 20,  1,  1),
        (  37, 3, 20,  6, 50), (  38, 3, 20, 12, 40), (  39, 3, 20, 18, 29),
        (  40, 3, 20,  0, 18), (  41, 3, 20,  6,  7), (  42, 3, 20, 11, 56),
        (  43, 3, 20, 17, 46), (  44, 3, 19, 23, 35), (  45, 3, 20,  5, 24),
        (  46, 3, 20, 11, 13), (  47, 3, 20, 17,  2), (  48, 3, 19, 22, 52),
        (  49, 3, 20,  4, 41), (  50, 3, 20, 10, 30), (  51, 3, 20, 16, 19),
        (  52, 3, 19, 22,  8), (  53, 3, 20,  3, 58), (  54, 3, 20,  9, 47),
        (  55, 3, 20, 15, 36), (  56, 3, 19, 21, 25), (  57, 3, 20,  3, 14),
        (  58, 3, 20,  9,  4), (  59, 3, 20, 14, 53), (  60, 3, 19, 20, 42),
        (  61, 3, 20,  2, 31), (  62, 3, 20,  8, 20), (  63, 3, 20, 14, 10),
        (  64, 3, 19, 19, 59), (  65, 3, 20,  1, 48), (  66, 3, 20,  7, 37),
        (  67, 3, 20, 13, 26), (  68, 3, 19, 19, 16), (  69, 3, 20,  1,  5),
        (  70, 3, 20,  6, 54), (  71, 3, 20, 12, 43), (  72, 3, 19, 18, 32),
        (  73, 3, 20,  0, 22), (  74, 3, 20,  6, 11), (  75, 3, 20, 12,  0),
        (  76, 3, 19, 17, 49), (  77, 3, 19, 23, 38), (  78, 3, 20,  5, 28),
        (  79, 3, 20, 11, 17), (  80, 3, 19, 17,  6), (  81, 3, 19, 22, 55),
        (  82, 3, 20,  4, 44), (  83, 3, 20, 10, 34), (  84, 3, 19, 16, 23),
        (  85, 3, 19, 22, 12), (  86, 3, 20,  4,  1), (  87, 3, 20,  9, 50),
        (  88, 3, 19, 15, 40), (  89, 3, 19, 21, 29), (  90, 3, 20,  3, 18),
        (  91, 3, 20,  9,  7), (  92, 3, 19, 14, 56), (  93, 3, 19, 20, 46),
        (  94, 3, 20,  2, 35), (  95, 3, 20,  8, 24), (  96, 3, 19, 14, 13),
        (  97, 3, 19, 20,  2), (  98, 3, 20,  1, 52), (  99, 3, 20,  7, 41),
        ( 100, 3, 20, 13, 30), ( 101, 3, 20, 19, 19), ( 102, 3, 21,  1,  8),
        ( 103, 3, 21,  6, 58), ( 104, 3, 20, 12, 47), ( 105, 3, 20, 18, 36),
        ( 106, 3, 21,  0, 25), ( 107, 3, 21,  6, 14), ( 108, 3, 20, 12,  4),
        ( 109, 3, 20, 17, 53), ( 110, 3, 20, 23, 42), ( 111, 3, 21,  5, 31),
        ( 112, 3, 20, 11, 20), ( 113, 3, 20, 17, 10), ( 114, 3, 20, 22, 59),
        ( 115, 3, 21,  4, 48), ( 116, 3, 20, 10, 37), ( 117, 3, 20, 16, 26),
        ( 118, 3, 20, 22, 16), ( 119, 3, 21,  4,  5), ( 120, 3, 20,  9, 54),
        ( 121, 3, 20, 15, 43), ( 122, 3, 20, 21, 32), ( 123, 3, 21,  3, 22),
        ( 124, 3, 20,  9, 11), ( 125, 3, 20, 15,  0), ( 126, 3, 20, 20, 49),
        ( 127, 3, 21,  2, 38), ( 128, 3, 20,  8, 28), ( 129, 3, 20, 14, 17),
        ( 130, 3, 20, 20,  6), ( 131, 3, 21,  1, 55), ( 132, 3, 20,  7, 44),
        ( 133, 3, 20, 13, 34), ( 134, 3, 20, 19, 23), ( 135, 3, 21,  1, 12),
        ( 136, 3, 20,  7,  1), ( 137, 3, 20, 12, 50), ( 138, 3, 20, 18, 40),
        ( 139, 3, 21,  0, 29), ( 140, 3, 20,  6, 18), ( 141, 3, 20, 12,  7),
        ( 142, 3, 20, 17, 56), ( 143, 3, 20, 23, 46), ( 144, 3, 20,  5, 35),
        ( 145, 3, 20, 11, 24), ( 146, 3, 20, 17, 13), ( 147, 3, 20, 23,  2),
        ( 148, 3, 20,  4, 52), ( 149, 3, 20, 10, 41), ( 150, 3, 20, 16, 30),
        ( 151, 3, 20, 22, 19), ( 152, 3, 20,  4,  8), ( 153, 3, 20,  9, 58),
        ( 154, 3, 20, 15, 47), ( 155, 3, 20, 21, 36), ( 156, 3, 20,  3, 25),
        ( 157, 3, 20,  9, 14), ( 158, 3, 20, 15,  4), ( 159, 3, 20, 20, 53),
        ( 160, 3, 20,  2, 42), ( 161, 3, 20,  8, 31), ( 162, 3, 20, 14, 20),
        ( 163, 3, 20, 20, 10), ( 164, 3, 20,  1, 59), ( 165, 3, 20,  7, 48),
        ( 166, 3, 20, 13, 37), ( 167, 3, 20, 19, 26), ( 168, 3, 20,  1, 16),
        ( 169, 3, 20,  7,  5), ( 170, 3, 20, 12, 54), ( 171, 3, 20, 18, 43),
        ( 172, 3, 20,  0, 32), ( 173, 3, 20,  6, 22), ( 174, 3, 20, 12, 11),
        ( 175, 3, 20, 18,  0), ( 176, 3, 19, 23, 49), ( 177, 3, 20,  5, 38),
        ( 178, 3, 20, 11, 28), ( 179, 3, 20, 17, 17), ( 180, 3, 19, 23,  6),
        ( 181, 3, 20,  4, 55), ( 182, 3, 20, 10, 44), ( 183, 3, 20, 16, 34),
        ( 184, 3, 19, 22, 23), ( 185, 3, 20,  4, 12), ( 186, 3, 20, 10,  1),
        ( 187, 3, 20, 15, 50), ( 188, 3, 19, 21, 40), ( 189, 3, 20,  3, 29),
        ( 190, 3, 20,  9, 18), ( 191, 3, 20, 15,  7), ( 192, 3, 19, 20, 56),
        ( 193, 3, 20,  2, 46), ( 194, 3, 20,  8, 35), ( 195, 3, 20, 14, 24),
        ( 196, 3, 19, 20, 13), ( 197, 3, 20,  2,  2), ( 198, 3, 20,  7, 52),
        ( 199, 3, 20, 13, 41), ( 200, 3, 20, 19, 30), ( 201, 3, 21,  1, 19),
        ( 202, 3, 21,  7,  8), ( 203, 3, 21, 12, 58), ( 204, 3, 20, 18, 47),
        ( 205, 3, 21,  0, 36), ( 206, 3, 21,  6, 25), ( 207, 3, 21, 12, 14),
        ( 208, 3, 20, 18,  4), ( 209, 3, 20, 23, 53), ( 210, 3, 21,  5, 42),
        ( 211, 3, 21, 11, 31), ( 212, 3, 20, 17, 20), ( 213, 3, 20, 23, 10),
        ( 214, 3, 21,  4, 59), ( 215, 3, 21, 10, 48), ( 216, 3, 20, 16, 37),
        ( 217, 3, 20, 22, 26), ( 218, 3, 21,  4, 16), ( 219, 3, 21, 10,  5),
        ( 220, 3, 20, 15, 54), ( 221, 3, 20, 21, 43), ( 222, 3, 21,  3, 32),
        ( 223, 3, 21,  9, 22), ( 224, 3, 20, 15, 11), ( 225, 3, 20, 21,  0),
        ( 226, 3, 21,  2, 49), ( 227, 3, 21,  8, 38), ( 228, 3, 20, 14, 28),
        ( 229, 3, 20, 20, 17), ( 230, 3, 21,  2,  6), ( 231, 3, 21,  7, 55),
        ( 232, 3, 20, 13, 44), ( 233, 3, 20, 19, 34), ( 234, 3, 21,  1, 23),
        ( 235, 3, 21,  7, 12), ( 236, 3, 20, 13,  1), ( 237, 3, 20, 18, 50),
        ( 238, 3, 21,  0, 40), ( 239, 3, 21,  6, 29), ( 240, 3, 20, 12, 18),
        ( 241, 3, 20, 18,  7), ( 242, 3, 20, 23, 56), ( 243, 3, 21,  5, 46),
        ( 244, 3, 20, 11, 35), ( 245, 3, 20, 17, 24), ( 246, 3, 20, 23, 13),
        ( 247, 3, 21,  5,  2), ( 248, 3, 20, 10, 52), ( 249, 3, 20, 16, 41),
        ( 250, 3, 20, 22, 30), ( 251, 3, 21,  4, 19), ( 252, 3, 20, 10,  8),
        ( 253, 3, 20, 15, 58), ( 254, 3, 20, 21, 47), ( 255, 3, 21,  3, 36),
        ( 256, 3, 20,  9, 25), ( 257, 3, 20, 15, 14), ( 258, 3, 20, 21,  4),
        ( 259, 3, 21,  2, 53), ( 260, 3, 20,  8, 42), ( 261, 3, 20, 14, 31),
        ( 262, 3, 20, 20, 20), ( 263, 3, 21,  2, 10), ( 264, 3, 20,  7, 59),
        ( 265, 3, 20, 13, 48), ( 266, 3, 20, 19, 37), ( 267, 3, 21,  1, 26),
        ( 268, 3, 20, 13,  5), ( 269, 3, 20,  7, 16), ( 270, 3, 20, 18, 54),
        ( 271, 3, 21,  0, 43), ( 272, 3, 20, 12, 22), ( 273, 3, 20, 12, 22),
        ( 274, 3, 20, 18, 11), ( 275, 3, 21,  0,  0), ( 276, 3, 20, 11, 38),
        ( 277, 3, 20, 11, 38), ( 278, 3, 20, 17, 28), ( 279, 3, 20, 23, 17),
        ( 280, 3, 20,  5,  6), ( 281, 3, 20, 10, 55), ( 282, 3, 20, 16, 44),
        ( 283, 3, 20, 22, 34), ( 284, 3, 20,  4, 23), ( 285, 3, 20, 10, 12),
        ( 286, 3, 20, 16,  1), ( 287, 3, 20, 21, 50), ( 288, 3, 20,  3, 40),
        ( 289, 3, 20,  9, 29), ( 290, 3, 20, 15, 18), ( 291, 3, 20, 21,  7),
        ( 292, 3, 20,  2, 56), ( 293, 3, 20,  8, 46), ( 294, 3, 20, 14, 35),
        ( 295, 3, 20, 20, 24), ( 296, 3, 20,  2, 13), ( 297, 3, 20,  8,  2),
        ( 298, 3, 20, 13, 52), ( 299, 3, 20, 19, 41), ( 300, 3, 21,  1, 39),
        ( 301, 3, 21,  7, 19), ( 302, 3, 21, 13,  8), ( 303, 3, 21, 18, 58),
        ( 304, 3, 21,  0, 47), ( 305, 3, 21,  6, 36), ( 306, 3, 21, 12, 25),
        ( 307, 3, 21, 18, 14), ( 308, 3, 21,  0,  4), ( 309, 3, 21,  5, 53),
        ( 310, 3, 21, 11, 42), ( 311, 3, 21, 17, 31), ( 312, 3, 20, 23, 20),
        ( 313, 3, 21,  5, 10), ( 314, 3, 21, 10, 59), ( 315, 3, 21, 16, 48),
        ( 316, 3, 20, 22, 37), ( 317, 3, 21,  4, 26), ( 318, 3, 21, 10, 16),
        ( 319, 3, 21, 16,  5), ( 320, 3, 20, 21, 54), ( 321, 3, 21,  3, 43),
        ( 322, 3, 21,  9, 32), ( 323, 3, 21, 15, 22), ( 324, 3, 20, 21, 11),
        ( 325, 3, 21,  3,  0), ( 326, 3, 21,  8, 49), ( 327, 3, 21, 14, 38),
        ( 328, 3, 20, 20, 28), ( 329, 3, 21,  2, 17), ( 330, 3, 21,  8,  6),
        ( 331, 3, 21, 13, 55), ( 332, 3, 20, 19, 44), ( 333, 3, 21,  1, 34),
        ( 334, 3, 21,  7, 23), ( 335, 3, 21, 13, 12), ( 336, 3, 20, 19,  1),
        ( 337, 3, 21,  0, 50), ( 338, 3, 21,  6, 40), ( 339, 3, 21, 12, 29),
        ( 340, 3, 20, 18, 18), ( 341, 3, 21,  0,  7), ( 342, 3, 21,  5, 56),
        ( 343, 3, 21, 11, 46), ( 344, 3, 20, 17, 35), ( 345, 3, 20, 23, 24),
        ( 346, 3, 21,  5, 13), ( 347, 3, 21, 11,  2), ( 348, 3, 20, 15, 52),
        ( 349, 3, 20, 22, 41), ( 350, 3, 21,  4, 30), ( 351, 3, 21, 10, 19),
        ( 352, 3, 20, 16,  8), ( 353, 3, 20, 21, 58), ( 354, 3, 21,  3, 47),
        ( 355, 3, 21,  9, 36), ( 356, 3, 20, 15, 25), ( 357, 3, 20, 21, 14),
        ( 358, 3, 21,  3,  4), ( 359, 3, 21,  8, 53), ( 360, 3, 20, 14, 42),
        ( 361, 3, 20, 20, 31), ( 362, 3, 21,  2, 20), ( 363, 3, 21,  8, 10),
        ( 364, 3, 20, 13, 59), ( 365, 3, 20, 19, 48), ( 366, 3, 21,  1, 37),
        ( 367, 3, 21,  7, 26), ( 368, 3, 20, 13, 16), ( 369, 3, 20, 19,  5),
        ( 370, 3, 21,  0, 54), ( 371, 3, 21,  6, 43), ( 372, 3, 20, 12, 32),
        ( 373, 3, 20, 18, 22), ( 374, 3, 21,  0, 11), ( 375, 3, 21,  6,  0),
        ( 376, 3, 20, 11, 49), ( 377, 3, 20, 17, 38), ( 378, 3, 20, 23, 28),
        ( 379, 3, 21,  5, 17), ( 380, 3, 20, 11,  6), ( 381, 3, 20, 16, 55),
        ( 382, 3, 20, 22, 44), ( 383, 3, 21,  4, 34), ( 384, 3, 20, 10, 23),
        ( 385, 3, 20, 16, 12), ( 386, 3, 20, 22,  1), ( 387, 3, 21,  3, 50),
        ( 388, 3, 20,  9, 40), ( 389, 3, 20, 15, 29), ( 390, 3, 20, 21, 18),
        ( 391, 3, 21,  3,  7), ( 392, 3, 20,  8, 56), ( 393, 3, 20, 14, 46),
        ( 394, 3, 20, 20, 35), ( 395, 3, 21,  2, 24), ( 396, 3, 20,  8, 13),
        ( 397, 3, 20, 14,  2), ( 398, 3, 20, 19, 52), ( 399, 3, 21,  1, 41),
        ( 400, 3, 20,  7, 30), ( 401, 3, 20, 13, 19), ( 402, 3, 20, 19,  8),
        ( 403, 3, 21,  0, 58), ( 404, 3, 20,  6, 47), ( 405, 3, 20, 12, 36),
        ( 406, 3, 20, 18, 25), ( 407, 3, 21,  0, 14), ( 408, 3, 20,  6,  4),
        ( 409, 3, 20, 11, 53), ( 410, 3, 20, 17, 42), ( 411, 3, 20, 23, 31),
        ( 412, 3, 20,  5, 20), ( 413, 3, 20, 11, 10), ( 414, 3, 20, 16, 59),
        ( 415, 3, 20, 22, 48), ( 416, 3, 20,  4, 37), ( 417, 3, 20, 10, 26),
        ( 418, 3, 20, 16, 16), ( 419, 3, 20, 20,  5), ( 420, 3, 20,  3, 54),
        ( 421, 3, 20,  9, 43), ( 422, 3, 20, 15, 32), ( 423, 3, 20, 21, 22),
        ( 424, 3, 20,  3, 11), ( 425, 3, 20,  9,  0), ( 426, 3, 20, 14, 49),
        ( 427, 3, 20, 20, 38), ( 428, 3, 20,  2, 28), ( 429, 3, 20,  8, 17),
        ( 430, 3, 20, 14,  6), ( 431, 3, 20, 19, 55), ( 432, 3, 20,  1, 44),
        ( 433, 3, 20,  7, 34), ( 434, 3, 20, 13, 23), ( 435, 3, 20, 19, 12),
        ( 436, 3, 20,  1,  1), ( 437, 3, 20,  6, 50), ( 438, 3, 20, 12, 40),
        ( 439, 3, 20, 18, 29), ( 440, 3, 20,  0, 18), ( 441, 3, 20,  6,  7),
        ( 442, 3, 20, 11, 56), ( 443, 3, 20, 17, 46), ( 444, 3, 19, 23, 35),
        ( 445, 3, 20,  5, 24), ( 446, 3, 20, 11, 13), ( 447, 3, 20, 17,  2),
        ( 448, 3, 19, 22, 52), ( 449, 3, 20,  4, 41), ( 450, 3, 20, 10, 30),
        ( 451, 3, 20, 16, 19), ( 452, 3, 19, 22,  8), ( 453, 3, 20,  3, 58),
        ( 454, 3, 20,  9, 47), ( 455, 3, 20, 15, 36), ( 456, 3, 19, 21, 25),
        ( 457, 3, 20,  3, 14), ( 458, 3, 20,  9,  4), ( 459, 3, 20, 14, 53),
        ( 460, 3, 19, 20, 42), ( 461, 3, 20,  2, 31), ( 462, 3, 20,  8, 20),
        ( 463, 3, 20, 14, 10), ( 464, 3, 19, 19, 59), ( 465, 3, 20,  1, 48),
        ( 466, 3, 20,  7, 37), ( 467, 3, 20, 13, 26), ( 468, 3, 19, 19, 16),
        ( 469, 3, 20,  1,  5), ( 470, 3, 20,  6, 54), ( 471, 3, 20, 12, 43),
        ( 472, 3, 19, 18, 32), ( 473, 3, 20,  0, 22), ( 474, 3, 20,  6, 11),
        ( 475, 3, 20, 12,  0), ( 476, 3, 19, 17, 49), ( 477, 3, 19, 23, 38),
        ( 478, 3, 20,  5, 28), ( 479, 3, 20, 11, 17), ( 480, 3, 19, 17,  6),
        ( 481, 3, 19, 22, 55), ( 482, 3, 20,  4, 44), ( 483, 3, 20, 10, 34),
        ( 484, 3, 19, 16, 33), ( 485, 3, 19, 22, 12), ( 486, 3, 20,  4,  1),
        ( 487, 3, 20,  9, 50), ( 488, 3, 19, 15, 40), ( 489, 3, 19, 21, 29),
        ( 490, 3, 20,  3, 18), ( 491, 3, 20,  9,  7), ( 492, 3, 19, 14, 56),
        ( 493, 3, 19, 20, 46), ( 494, 3, 20,  2, 35), ( 495, 3, 20,  8, 24),
        ( 496, 3, 19, 14, 13), ( 497, 3, 19, 20,  2), ( 498, 3, 20,  1, 52),
        ( 499, 3, 20,  7, 41), ( 500, 3, 20, 13, 30), ( 501, 3, 20, 19, 19),
        ( 502, 3, 21,  1,  8), ( 503, 3, 21,  6, 58), ( 504, 3, 20, 12, 47),
        ( 505, 3, 20, 18, 36), ( 506, 3, 21,  0, 25), ( 507, 3, 21,  6, 14),
        ( 508, 3, 20, 12,  4), ( 509, 3, 20, 17, 53), ( 510, 3, 20, 23, 42),
        ( 511, 3, 21,  5, 31), ( 512, 3, 20, 11, 20), ( 513, 3, 20, 17, 10),
        ( 514, 3, 20, 22, 59), ( 515, 3, 21,  4, 48), ( 516, 3, 20, 10, 37),
        ( 517, 3, 20, 16, 26), ( 518, 3, 20, 22, 16), ( 519, 3, 21,  4,  5),
        ( 520, 3, 20,  9, 54), ( 521, 3, 20, 15, 43), ( 522, 3, 20, 21, 32),
        ( 523, 3, 21,  3, 22), ( 524, 3, 20,  9, 11), ( 525, 3, 20, 15,  0),
        ( 526, 3, 20, 20, 49), ( 527, 3, 21,  2, 38), ( 528, 3, 20,  8, 28),
        ( 529, 3, 20, 14, 17), ( 530, 3, 20, 20,  6), ( 531, 3, 21,  1, 55),
        ( 532, 3, 20,  7, 44), ( 533, 3, 20, 13, 34), ( 534, 3, 20, 19, 23),
        ( 535, 3, 21,  1, 12), ( 536, 3, 20,  7,  1), ( 537, 3, 20, 12, 50),
        ( 538, 3, 20, 18, 40), ( 539, 3, 21,  0, 29), ( 540, 3, 20,  6, 18),
        ( 541, 3, 20, 12,  7), ( 542, 3, 20, 17, 56), ( 543, 3, 20, 23, 46),
        ( 544, 3, 20,  5, 35), ( 545, 3, 20, 11, 24), ( 546, 3, 20, 17, 13),
        ( 547, 3, 20, 23,  2), ( 548, 3, 20,  4, 52), ( 549, 3, 20, 10, 41),
        ( 550, 3, 20, 16, 30), ( 551, 3, 20, 22, 19), ( 552, 3, 20,  4,  8),
        ( 553, 3, 20,  9, 58), ( 554, 3, 20, 15, 47), ( 555, 3, 20, 21, 36),
        ( 556, 3, 20,  3, 25), ( 557, 3, 20,  9, 14), ( 558, 3, 20, 15,  4),
        ( 559, 3, 20, 20, 53), ( 560, 3, 20,  2, 42), ( 561, 3, 20,  8, 31),
        ( 562, 3, 20, 14, 20), ( 563, 3, 20, 20, 10), ( 564, 3, 20,  1, 59),
        ( 565, 3, 20,  7, 48), ( 566, 3, 20, 13, 37), ( 567, 3, 20, 19, 26),
        ( 568, 3, 20,  1, 16), ( 569, 3, 20,  7,  5), ( 570, 3, 20, 12, 54),
        ( 571, 3, 20, 18, 43), ( 572, 3, 20,  0, 32), ( 573, 3, 20,  6, 22),
        ( 574, 3, 20, 12, 11), ( 575, 3, 20, 18,  0), ( 576, 3, 19, 23, 49),
        ( 577, 3, 20,  5, 38), ( 578, 3, 20, 11, 28), ( 579, 3, 20, 17, 17),
        ( 580, 3, 19, 23,  6), ( 581, 3, 20,  4, 55), ( 582, 3, 20, 10, 44),
        ( 583, 3, 20, 16, 34), ( 584, 3, 19, 22, 23), ( 585, 3, 20,  4, 12),
        ( 586, 3, 20, 10,  1), ( 587, 3, 20, 15, 50), ( 588, 3, 19, 21, 40),
        ( 589, 3, 20,  3, 29), ( 590, 3, 20,  9, 18), ( 591, 3, 20, 15,  7),
        ( 592, 3, 19, 20, 56), ( 593, 3, 20,  2, 46), ( 594, 3, 20,  8, 35),
        ( 595, 3, 20, 14, 24), ( 596, 3, 19, 20, 13), ( 597, 3, 20,  2,  2),
        ( 598, 3, 20,  7, 52), ( 599, 3, 20, 13, 41), ( 600, 3, 20, 19, 30),
        ( 601, 3, 21,  1, 19), ( 602, 3, 21,  7,  8), ( 603, 3, 21, 12, 58),
        ( 604, 3, 20, 18, 47), ( 605, 3, 21,  0, 36), ( 606, 3, 21,  6, 25),
        ( 607, 3, 21, 12, 14), ( 608, 3, 20, 18,  4), ( 609, 3, 20, 23, 53),
        ( 610, 3, 21,  5, 42), ( 611, 3, 21, 11, 31), ( 612, 3, 20, 17, 20),
        ( 613, 3, 20, 23, 20), ( 614, 3, 21,  4, 59), ( 615, 3, 21, 10, 48),
        ( 616, 3, 20, 16, 37), ( 617, 3, 20, 22, 26), ( 618, 3, 21,  4, 16),
        ( 619, 3, 21, 10,  5), ( 620, 3, 20, 15, 54), ( 621, 3, 20, 21, 43),
        ( 622, 3, 21,  3, 32), ( 623, 3, 21,  9, 22), ( 624, 3, 20, 15, 11),
        ( 625, 3, 20, 21,  0), ( 626, 3, 21,  2, 49), ( 627, 3, 21,  8, 38),
        ( 628, 3, 20, 14, 28), ( 629, 3, 20, 20, 17), ( 630, 3, 21,  2,  6),
        #( ), ( ), ( ),

        #(), (), (),
        (1523, 3, 21, 15, 22), (1524, 3, 20, 21, 11), (1525, 3, 21,  3,  0),
        (1526, 3, 21,  8, 49), (1527, 3, 21, 14, 38), (1528, 3, 20, 20, 28),
        (1529, 3, 21,  2, 17), (1530, 3, 21,  8,  6), (1531, 3, 21, 13, 55),
        (1532, 3, 20, 19, 44), (1533, 3, 21,  1, 34), (1534, 3, 21,  7, 23),
        (1535, 3, 21, 13, 12), (1536, 3, 20, 19,  1), (1537, 3, 21,  0, 50),
        (1538, 3, 21,  6, 40), (1539, 3, 21, 12, 29), (1540, 3, 20, 18, 18),
        (1541, 3, 21,  5, 56), (1542, 3, 21,  5, 56), (1543, 3, 21, 11, 46),
        (1544, 3, 20, 17, 35), (1545, 3, 20, 23, 24), (1546, 3, 21,  5, 13),
        (1547, 3, 21, 11,  2), (1548, 3, 20, 16, 52), (1549, 3, 20, 22, 41),
        (1550, 3, 21,  4, 30), (1551, 3, 21, 10, 19), (1552, 3, 20, 16,  8),
        (1553, 3, 20, 21, 58), (1554, 3, 21,  3, 21), (1555, 3, 21,  9, 36),
        (1556, 3, 20, 15, 25), (1557, 3, 20, 21, 14), (1558, 3, 21,  3,  4),
        (1559, 3, 21,  8, 53), (1560, 3, 20, 14, 42), (1561, 3, 20, 20, 31),
        (1562, 3, 21,  2, 20), (1563, 3, 21,  8, 10), (1564, 3, 20, 13, 59),
        (1565, 3, 20, 19, 48), (1566, 3, 21,  1, 37), (1567, 3, 21,  7, 26),
        (1568, 3, 20, 13, 16), (1569, 3, 20, 19,  5), (1570, 3, 21,  0, 54),
        (1571, 3, 21,  6, 43), (1572, 3, 20, 12, 32), (1573, 3, 20, 18, 22),
        (1574, 3, 21,  0, 11), (1575, 3, 21,  6,  0), (1576, 3, 20, 11, 49),
        (1577, 3, 20, 17, 38), (1578, 3, 20, 23, 28), (1579, 3, 21,  5, 17),
        (1580, 3, 20, 11,  6), (1581, 3, 20, 16, 55), (1582, 3, 20, 22, 44),
        )
    INJECT = (
        ((178, 0, 5), (2022, 3, 1, 17, 59)), # *** TODO *** Convert all to
        ((178, 19, 1), (2022, 3, 2, 18)),    #              correct sunset
        ((178, 19, 2), (2022, 3, 3, 18, 1)),
        ((181, 1, 2), (2024, 3, 21, 18, 17)),
        ((181, 1, 5), (2024, 3, 24, 18, 20)),
        ((181, 2, 13), (2024, 4, 20, 18, 42)),
        # Sunset Tehran -> 15:02:
        ((181, 3, 18, 20), (2024, 5, 15, 15, 3, 36.0864)),
        ((181, 3, 19, 20), (2024, 5, 16, 15, 4, 23.4336)),
        ((181, 4, 1, 17), (2024, 5, 17, 12, 4, 2.6112)),
        ((181, 4, 1, 20), (2024, 5, 17, 15, 5, 10.3488)),
        ((181, 18, 1), (2025, 2, 6, 17, 31, 3.7056)),
        ((181, 18, 19), (2025, 2, 24, 17, 56, 46.7232)),
        ((181, 0, 1), (2025, 2, 25, 17, 57, 42.4512)),
        # (181, 0, 4) -> (2025, 3, 0.750321) is an error in
        # GregorianCalendar._check_valid_gregorian_month_day
        ((181, 0, 4), (2025, 2, 28, 17, 53, 36.0384)),
        ((181, 19, 1), (2025, 3, 1, 18, 1, 22.1664)),
        ((181, 19, 19), (2025, 3, 19, 18, 16, 7.7664)),
        ((182, 18, 19), (2026, 2, 24, 17, 56, 33.2448)),
        ((182, 0, 1), (2026, 2, 25, 17, 57, 29.0592)),
        ((182, 0, 5), (2026, 3, 1, 18, 1, 8.9472)),
        )
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)

    def __init__(self):
        super().__init__()
        # https://qr.ae/psZONa
        # https://www.someweekendreading.blog/leap-year-revised/
        # 365 + 1/4 − 1/128 = 365.2421875 or 365 + 31/128
        # 365.2421897
        #self.MEAN_TROPICAL_YEAR = 365.2421897
        self._MONTHNAMES = {num: name for num, name in self.BADI_MONTH_NAMES}
        self.gc = GregorianCalendar()

    def analyze_date_error(self, options):
        """
        Finds the the Julian Period day (yearly epoch) that corresponds to
        the day of the Vernal Equinox.

        -a, optional -C, -G, and -X
        Also if -S and -E are used they must be used together.
        """
        return self._date_range(options)

    def check_long_date_from_short_date(self, data):
        """
        -c or --ck-dates
        """
        items = []

        for item in data:
            b_date, date = item
            bd = self.long_date_from_short_date(date)

        if bd != (b_date + (0, 0, 0)):
            items.append((item, bd))

        return items

    def find_leap_years(self, options):
        """
        -e and -S and -E

        Find all the leap years between -1842 and 1161.
        """
        data = []
        start = options.start
        end = options.end
        assert -1843 < start < 1162, (
            f"Start '{start}' must be from -1842 to 1161.")
        assert -1842 < end < 1163, (
            f"End '{end}' must be from -1841 to 1162")

        for year in range(start, end):
            is_leap = self._is_leap_year(year)
            days = 365 + is_leap
            total = 0

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + self._is_leap_year(year)

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    g_date = self.gregorian_date_from_badi_date(date)
                    # weekday, wd_name, m_name
                    weektuple = self._day_of_week(*date)
                    total += 1
                    data.append((date, g_date, *weektuple,
                                 is_leap, days, total))

        return data

    def find_gregorian_dates(self, options):
        """
        -g or --g-dates and -S and -E

        Converts Badi to Gregorian dates for the given range.
        """
        data = []

        for item in self._date_range(options):
            b_date, bjd, g_date, gjd, diff, offby = item
            g_date = self._gregorian_date_from_badi_date(
                b_date, options, *self.BAHAI_LOCATION[:3])
            gjd = self.gc.jd_from_gregorian_date(
                g_date, exact=options.exact, alt=options.alt_leap)
            diff = round(bjd - gjd, 6)
            offby = math.floor(bjd) - math.floor(gjd)
            data.append((b_date, bjd, g_date, gjd, diff, offby))

        return data

    def create_date_lists(self, options):
        """
        -l or --list
        Also -S and -E must both used together.
        """
        data = []

        for k in reversed(range(options.start, options.end)):
            for v in reversed(range(1, 20)):
                for y in reversed(range(1, 20)):
                    for m in reversed(range(0, 20)):
                        if m == 0:
                            self._calc_kvymd(5, k, v, y, m, data)
                        else:
                            self._calc_kvymd(20, k, v, y, m, data)

        return data

    def find_coefficents_precursor(self, options):
        """
        -p or --precursor
        -S and -E must be Gregorian dates

        This determines which coefficient group should be used for the
        years provided. The years provided are on the Badi Calendar - or +
        the epoch.

        Arguments to the process_segment() function.
        --------------------------------------------
        1. First run `badi_jd_tests.py -aX > filename.txt`
           This file will be long so use `less filename.txt` to look at it.
           The last column will usually be -1, 0, or 1. The 0 values are
           already correct, the other two values means there is a difference
           between the Gregorian and Badi Julian Period days. These are the
           ones than need the coefficients to fix them.
        2. The first argument is the current Badi year being processed
           subtracted from the end year argument.
        3. The second argument is the 1st coefficient corresponding to the
           (1, 34, 67, 100) numbers in the output from this method.
        4. The third argument is the 2nd coefficient which fixes the 1 values
           that were not included in the 1st coefficient and the 2 and 3 values.

        If an error JD falls on a 0 (zero) value then you need to change
        the start and end years so that no error JDs fall on a 0 value. The
        average number of years fixed in a group is 99, but this is not a
        hard and fast rule. Obvious break points are where a sequence
        changes. For example where there are two consecutive already good
        values where the values you need to fix had one.

        Note: Zero values never get processes.
        """
        data = []

        for y in range(options.start, options.end):
            year = options.end - y

            if year in (1, 34, 67, 100):
                a = year
            else:
                a = ''

            data.append((y + self.TRAN_COFF, y, year % 4, a))

        return data

    def find_coefficents(self, options):
        """
        -q or --coeff and -S and -E

        If -X is used the more exact mode is used. This should be the
        normal usage.
        """
        start = options.start
        end = options.end
        options.start = options.start + self.TRAN_COFF
        options.end = options.end + self.TRAN_COFF
        data = self._date_range(options)
        options.start = start
        options.end = end
        cp = {by: (n, a)
              for gy, by, n, a in self.find_coefficents_precursor(options)}
        items = []

        for item in data:
            b_year, month, day = item[0][:3]
            h, m, s, ms = self._get_hms(item[0], short_in=True)
            bjd = item[1]
            msg = (f"{b_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                   f"{s:<02} {bjd:<14} ")
            g_year, month, day = item[2][:3]
            h, m, s, ms = self._get_hms(item[2], short_in=True)
            gjd = item[3]
            msg += (f"{g_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                    f"{s:<07} {gjd:<14} ")
            diff = item[4]
            offby = item[5]
            msg += f"{diff:< 9} {offby:< 1} "
            j, k = cp.get(b_year)
            msg += f"{j} {k:<3}"
            items.append(msg)

        return items

    def get_range(self, end):
        """
        -r or --range
        """
        seq = {-159: -259, -64: -159, 35: -64, 134: 35, 233: 134, 332: 233,
               386: 332, 617: 517, 716: 617, 815: 716, 914: 815, 1013: 914,
               1112: 1013, 1211: 1112}
        valid_dates = list(seq.keys())
        start = seq.get(end)
        assert start is not None, (f"You must use valid dates, found {end}, "
                                   f"Valid dates are {valid_dates}.")
        data = []

        for y in range(start, end):
            yj = end - y
            jump = yj if yj in (1, 34, 67, 100) else 0 # jump values
            data.append((y, (end - y) % 4, jump))      # mod 4 values

        return data

    def twenty_four_hours(self, options):
        """
        Dump the hours, minutes, and seconds of the day.
        -t and -S and -E
        """
        data = []
        start = options.start
        end = options.end
        assert -1843 < start < 1162, (
            f"Start '{start}' must be from -1842 to 1161.")
        assert -1842 < end < 1163, (
            f"End '{end}' must be from -1841 to 1162")
        lat, lon, zone = self.BAHAI_LOCATION[:3]

        for year in range(start, end):
            is_leap = self._is_leap_year(year)
            days = 365 + is_leap
            total = 0

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + self._is_leap_year(year)

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    jd = self.jd_from_badi_date(date, lat, lon, zone)
                    g_date = self.gc.gregorian_date_from_jd(jd, exact=True)
                    jd0 = math.floor(jd)
                    jd1 = jd0 + 1
                    diff0 = self._meeus_from_exact(jd0)
                    diff1 = self._meeus_from_exact(jd1)
                    mjd0 = jd0 + diff0
                    mjd1 = jd1 + diff1
                    ss0 = self._sun_setting(mjd0, lat, lon, zone)
                    ss1 = self._sun_setting(mjd1, lat, lon, zone)
                    b_date = self.badi_date_from_jd(jd, short=True,
                                                    fraction=True)
                    ss_diff = ss1 - ss0
                    hms = self.hms_from_decimal_day(ss_diff)
                    data.append((b_date, g_date, round(ss0 % 1, 6),
                                 round(ss1 % 1, 6), round(ss_diff, 6), hms))

        return data

    def find_day_of_week(self, options):
        """
        Dump both the Badi and Gregorian weekdays.
        -w and -S and -E
        Optional -R to change the referance day from Jalál to whatever.
        """
        data = []
        _daynames = ['Jalál', 'Jamál', 'Kamál', 'Fiḍāl',
                     '`Idāl', 'Istijlāl', 'Istiqlāl']
        _g_daynames = ['Saturday', 'Sunday', 'Monday', 'Tuesday',
                       'Wednesday', 'Thursday', 'Friday']
        start = options.start
        end = options.end
        assert -1843 < start < 1162, (
            f"Start '{start}' must be from -1842 to 1161.")
        assert -1842 < end < 1163, (
            f"End '{end}' must be from -1841 to 1162")
        ref_day = options.ref_day

        if ref_day != 'Jalál':
            if ref_day in _daynames:
                r_idx = _daynames.index(ref_day)
                _daynames = _daynames[r_idx:] + _daynames[:r_idx]
                _g_daynames = _g_daynames[r_idx:] + _g_daynames[:r_idx]
            else:
                raise ValueError(f"Invalid reference_day {options.ref_day}. "
                                 "Must be a valid day of the week.")

        for year in range(start, end):
            is_leap = self._is_leap_year(year)
            days = 365 + is_leap
            total = 0

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + self._is_leap_year(year)

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    idx = (datetime._ymd2ord(
                        self, year, month, day) % 7 + 7) % 7
                    badi_weekday = _daynames[idx]
                    greg_weekday = _g_daynames[idx]
                    data.append((date, ref_day, idx,
                                 badi_weekday, greg_weekday))

        return data

    #
    # Supporting methods
    #
    def _jd_from_badi_date(self, b_date, lat=None, lon=None, zone=None, *,
                           coeffon=True):
        year, month, day = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date), short=True)

        if month == 0: # Ayyam-i-Ha
            m = 18 * 19
        elif month < 19: # month 1 - 18
            m = (month - 1) * 19
        else: # month == 19:
            m = 18 * 19 + 4 + self._is_leap_year(year)

        td = self._days_in_years(year-1)
        jd = td + math.floor(self.BADI_EPOCH) + m + day

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        # The diff value converts my jd to the Meeus algorithm for
        # determining the sunset jd.
        diff = self._meeus_from_exact(jd)
        ss_a = self._sun_setting(jd + diff, lat, lon, zone) % 1
        #print(f"{str(b_date):<15} {day:<9} {jd:<14} {ss_a:<20}",
        #      file=sys.stderr)

        if coeffon:
            coff = 0 if coeffon else self._get_coff(year)
        else:
            coff = self._get_coff(year)

        return round(jd + ss_a + coff, 6)

    def _get_coff(self, year):
        def process_segment(y, a=0, onoff0=(), b=0, onoff1=()):
            func = lambda y, onoff: 0 < y < 100 and y % 4 in onoff
            coff = 0

            if a and func(y, onoff0): # Whatever is passed in onoff0.
                coff = a
            elif b and func(y, onoff1): # Whatever is passed in onoff0.
                coff = b

            return coff

        def process_segments(year, pn, a=0, onoff0=(), b=0, onoff1=()):
            coff = 0

            for start, end in pn:
                if year in range(start, end):
                    # start to end (range -S start -E end)
                    coff0 = process_segment(end - year, a=a, onoff0=onoff0)
                    coff1 = process_segment(end - year, b=b, onoff1=onoff1)
                    coff = coff0 if coff0 != 0 else coff1

            return coff

        z0 = ((-12, -11), (128, 129), (364, 365))
        p0100 = ((-95, -91),)
        p1 = ((-1783, -1750), (-1650, -1615), (-1499, -1483), (-1383, -1347),
              (-1251, -1215), (-299, -283), (-179, -143), (-47, -12),
              (101, 116), (213, 249), (345, 364), (365, 381), (501, 513),
              (609, 645), (741, 777), (901, 909), (1005, 1041), (1137, 1162))
        p1001 = ((-1698, -1682),)
        p1100 = ((-1299, -1283), (-99, -95), (-91, -79), (301, 313),
                 (605, 609), (701, 709), (1101, 1105))
        p1101 = ((-1682, -1650),)
        p1110 = ((-1799, -1783), (-1399, -1383), (-1283, -1251), (-199, -179),
                 (-79, -47), (201, 213), (313, 345), (601, 605), (709, 741),
                 (1001, 1005), (1105, 1137))
        p1112 = ((-1750, -1714),)
        p1211 = ((116, 128),)
        p2 = ((-1519, -1499), (-319, -299), (85, 101), (477, 501), (873, 901))
        p2111 = ((-1702, -1698), (-1615, -1583), (-1483, -1451),
                 (-1347, -1315), (-1327,-1315 ), (-283, -243), (-143, -111),
                 (-11, 21), (117, 125), (129, 149), (249, 281), (381, 413),
                 (513, 545), (645, 677), (777, 809), (909, 941), (1041, 1073))
        p2112 = ((-1842, -1814), (-1714, -1702))
        p2211 = ((-1583, -1551), (-1451, -1435), (-1435, -1415),
                 (-1315,-1299 ), (-243, -211), (-111, -99), (21, 53),
                 (149, 185), (281, 301), (413, 445), (545, 577), (677, 701),
                 (809, 841), (941, 973), (1073, 1101))
        p2212 = ((-1814, -1802),)
        p2221 = ((-1803, -1799), (-1551, -1519), (-1415, -1399), (-211, -199),
                 (53, 85), (185, 201), (445, 477), (577, 601), (841, 873),
                 (973, 1001))
        z0_flag = False
        coff = 0

        # General ranges are determined with:
        # ./contrib/misc/badi_jd_tests.py -p -S start_year -E end_year
        # Where -S is the 1st year and -E is the nth year + 1 that needs to
        # be process. Use the following command to test the results of each
        # segment. ./contrib/misc/badi_jd_tests.py -qXS start_year -E end_year
        # Full range is -1842 to 1161
        for start, end in z0:
            if year in range(start, end):
                z0_flag = True

        if not z0_flag:
            if not coff:
                coff = process_segments(year, p0100, -1, (3,))

            if not coff:
                coff = process_segments(year, p1, -1, (0, 1, 2, 3))

            if not coff:
                coff = process_segments(year, p1001, -1, (0, 1))

            if not coff:
                coff = process_segments(year, p1100, -1, (0, 3))

            if not coff:
                coff = process_segments(year, p1101, -1, (0, 1, 3))

            if not coff:
                coff = process_segments(year, p1110, -1, (0, 2, 3))

            if not coff:
                coff = process_segments(year, p1112, -1, (0, 2, 3), -2, (1,))

            if not coff:
                coff = process_segments(year, p1211, -1, (0, 1, 2), -2, (3,))

            if not coff:
                coff = process_segments(year, p2, -2, (0, 1, 2, 3))

            if not coff:
                coff = process_segments(year, p2111, -2, (0,), -1, (1, 2, 3))

            if not coff:
                coff = process_segments(year, p2112, -2, (0, 1), -1, (2, 3))

            if not coff:
                coff = process_segments(year, p2211, -2, (0, 3), -1, (1, 2))

            if not coff:
                coff = process_segments(year, p2212, -2, (0, 1, 3), -1, (2,))

            if not coff:
                coff = process_segments(year, p2221, -2, (0, 2, 3), -1, (1,))

        return coff

    def _badi_date_from_jd_alt(self, jd:float, lat:float=None, lon:float=None,
                               zone:float=None) -> tuple:
        """
        Convert a Julian period day to a Badi date.
        """
        def get_leap_year_info(y):
            leap = self._is_leap_year(year)
            yds = 366 if leap else 365
            ld = 4 + leap
            return leap, yds, ld

        def check_and_fix_day(cjd, y, lat=None, lon=None, zone=None):
            fjdy = self.jd_from_badi_date((y, 1, 1), lat, lon, zone)
            return y-1 if (fjdy - cjd) > 0 else y

        md = jd - (self.BADI_EPOCH - 1)
        year = math.floor(md / self.MEAN_TROPICAL_YEAR) + 1
        #year = math.floor(abs(md / self.MEAN_TROPICAL_YEAR))
        #year *= -1 if md < (self.BADI_EPOCH - 1) else 1

        leap, yds, ld = get_leap_year_info(year)

        if (y := check_and_fix_day(jd, year, lat, lon, zone)):
            year = y
            leap, yds, ld = get_leap_year_info(year)

        fjdy = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        days = math.floor(jd) - math.floor(fjdy) + 1

        if days <= 342: # Month 1 - 18
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= yds: # Month 19
            day = days - (342 + ld)
        else: # Ayyam-i-Ha
            day = days % 342

        month_days = [(n, 19) for n, v in self.BADI_MONTH_NAMES]
        month_days[18] = (0, ld)

        for month, ds in month_days:
            if days > ds:
                days -= ds
            else:
                break

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        #diff = jd % 1 - self._sun_setting(jd, lat, lon, zone) % 1
        #day += jd % 1 + 0.5

        ## print('jd:', jd, 'md:', md, #'td', td,
        ##       'days:', days,
        ##       'fjdy', fjdy,
        ##       #'d', d,
        ##       #'diff', diff,
        ##       'ld', ld, 'date:', (year, month, day),
        ##       file=sys.stderr)

        return year, month, day

    def _date_range(self, options):
        data = []
        ve_jd_0001_1582 = self.pre_process_vernal_equinoxs()
        last_year = 0
        inject = [(b_date[0], (b_date, g_date))
                  for b_date, g_date in self.INJECT]

        for g_year in range(options.start, options.end):
            if g_year < 1583:
                if g_year in ve_jd_0001_1582:
                    ve_jd = ve_jd_0001_1582[g_year]
                else:
                    continue
            else:
                g_date = (g_year, 3, 1)
                # We must use the Meeus algorithm not mine when finding the
                # equinox and sunset. So don't use exact=options.exact here.
                jd = self.gc.jd_from_gregorian_date(g_date) # Julian Period day
                ve_jd = self.find_moment_of_equinoxes_or_solstices(
                    jd, zone=3.5)

            ss_jd = self._sun_setting(ve_jd, *self.BAHAI_LOCATION[:3])

            # It is allowed to have a Vernal Equinox to be up to one minute
            # before sunset and still use that sunset as the beginning of
            # the year. If a day == 1 then 1 minute is 0.0006944444444444444
            if ve_jd >= (ss_jd - 0.0006944444444444444):
                jd_ss = ss_jd
            else:
                jd_ss = self._sun_setting(ve_jd-1, *self.BAHAI_LOCATION[:3])

            # Make the Badi date for the beginning of the year.
            b_date = (g_year - self.TRAN_COFF, 1, 1)
            self._calculate_b_date(b_date, jd_ss, data, options)

            #for b_date, g_ss_date in self._find_dates(b_date[0], inject):
            #    jd_ss = self.gc.jd_from_gregorian_date(g_ss_date)
            #    self._calculate_b_date(b_date, jd_ss, data, options)

        return data

    def _calculate_b_date(self, b_date, jd_ss, data, options):
        try:
            g_date = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                jd_ss)) # Sunset before VE
            bjd = self._jd_from_badi_date(b_date, coeffon=options.coff)
            e_jd_ss = jd_ss - self._exact_from_meeus(jd_ss)
            diff = round(bjd - e_jd_ss, 6)
            offby = math.floor(diff)
            data.append((b_date, bjd, g_date, e_jd_ss, diff, offby))
        except Exception as e:
            msg = f"Badi date {b_date} and Gregorian date {g_date}, {e}"
            print(msg, file=sys.stderr)

    def pre_process_vernal_equinoxs(self):
        data = {}

        for date in self.VE_0001_1582:
            jd = self.gc.jd_from_gregorian_date(date)
            jd += 0.14583333333333333333 # 3.5 hours for Tehran time
            data[date[0]] = jd

        return data

    def _find_dates(self, year, inject):
        items = []

        for y, item in inject:
            if y == year:
                items.append(item)

        return items

    def _gregorian_date_from_badi_date(self, b_date:tuple, options, lat=0,
                                       lon=0, zone=0) -> tuple:
        """
        Get the Gregorian date from the Badi date.
        """
        jd = self._jd_from_badi_date(b_date, lat=lat, lon=lon, zone=zone,
                                     options=options)
        gd = self.gc.gregorian_date_from_jd(jd, exact=options.exact)
        g_date = self.gc.ymdhms_from_date(gd)
        return g_date

    def _calc_kvymd(self, days, k, v, y, m, data):
        year = (k - 1) * 361 + (v - 1) * 19 + y

        for d in reversed(range(1, days)):
            data.append(((k, v, y, m, d), (year, m, d)))

    def _day_of_week(self, year, month, day):
        weekday = (datetime._ymd2ord(self, year, month, day) % 7 + 7) % 7
        wd_name = datetime.DAYNAMES[weekday]
        m_name = self._MONTHNAMES[month]
        return weekday, wd_name, m_name


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test Badi date ranges."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Analyze Badi date errors when converting to jd.")
    parser.add_argument(
        '-c', '--ck-dates', action='store_true', default=False, dest='ck_dates',
        help="Check that long_date_from_short_date() works correctly.")
    parser.add_argument(
        '-e', '--leap-years', action='store_true', default=False,
        dest='leap_years', help="Convert Badi to Gregorian dates.")
    parser.add_argument(
        '-g', '--g-dates', action='store_true', default=False, dest='g_dates',
        help="Convert Badi to Gregorian dates.")
    parser.add_argument(
        '-l', '--list', action='store_true', default=False, dest='list',
        help="Generate a list of Badi dates both long and short versions.")
    parser.add_argument(
        '-p', '--precursor', action='store_true', default=False,
        dest='precursor',
        help="Dump data for determining the precursors to the coefficients.")
    parser.add_argument(
        '-q', '--coeff', action='store_true', default=False, dest='coeff',
        help="Dump data for determining coefficients.")
    parser.add_argument(
        '-r', '--range', type=int, default=0, dest='range',
        help="Dump an analysis of date ranges. Takes an integer value.")
    parser.add_argument(
        '-w', '--weekday', action='store_true', default=False,
        dest='weekday', help="Dump consecutive Badi and Gregorian weekdays.")
    parser.add_argument(
        '-t', '--twenty-four', action='store_true', default=False,
        dest='twenty_four', help="Find day length.")
    parser.add_argument(
        '-A', '--alt-leap', action='store_true', default=False,
        dest='alt_leap', help="Use alternative leap year method.")
    parser.add_argument(
        '-C', '--coff', action='store_true', default=False, dest='coff',
        help="Turn off all coefficients during an analysis.")
    parser.add_argument(
        '-D', '--dates', action='store_true', default=False, dest='dates',
        help="Test for the consecutive dates from JDs.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End Badi year of sequence.")
    parser.add_argument(
        '-G', '--graph', action='store_true', default=False, dest='graph',
        help=("Turn off all coefficients and dump output appropriate for "
              "graphing."))
    parser.add_argument(
        '-H', '--hours', action='store_true', default=False, dest='hours',
        help="Test for the consecutive hours.")
    parser.add_argument(
        '-J', '--jd', action='store_true', default=False, dest='jd',
        help=("Test for consecutive Julian Period days between start and "
              "end Badi years."))
    parser.add_argument(
        '-R', '--ref-day', type=str, default='Jalál', dest='ref_day',
        help="Change the referance day. Default is Jalál.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start Badi year of sequence.")
    parser.add_argument(
        '-X', '--exact', action='store_true', default=False, dest='exact',
        help=("Use the 4|100|400 or the 4|128 rules from Julian Calendar "
              "day one."))
    parser.add_argument(
        '-Y', '--year', action='store_true', default=False, dest='year',
        help="Test for the consecutive defined years 1 - 3004.")
    options = parser.parse_args()
    dt = DateTests()
    ret = 0

    if options.analyze: # -a
        if options.start is None or options.end is None:
            # Set default Gregorian years.
            options.start = 1  # Julian year 1
            options.end = 3005 # Gregorian year 3005

        if options.graph:
            options.coff = True
            data = dt.analyze_date_error(options)
            items = []

            for item in data:
                year, month, day = item[0][:3]
                h, m, s, ms = dt._get_hms(item[0], short_in=True)
                bjd = item[1]
                msg = (f"{year}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                       f"{s:>02} {bjd} ")
                year, month, day = item[2][:3]
                h, m, s, ms = dt._get_hms(item[2], short_in=True)
                gjd = item[3]
                msg += (f"{year}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                        f"{s:>02} {gjd} ")
                diff = item[4]
                offby = item[5]
                msg += f"{diff} {offby}"
                items.append(msg)

            [print(item) for item in items]
        else:
            data = dt.analyze_date_error(options)
            print("Badi Date     Badi JD        Gregorian Date                "
                  " Gregorian JD   Diff      Off By")

            for b_date, bjd, g_date, gjd, diff, offby in data:
                dn = '-' if diff < 0 else ' '
                on = '-' if offby < 0 else ' '
                print(f"{str(b_date):13} "
                      f"{bjd:<14} "
                      f"{str(g_date):30} "
                      f"{gjd:<14} "
                      f"{dn}{abs(diff):<8} "
                      f"{on}{abs(offby)}")

            diffs = []
            p = 0
            n = 0

            for item in data:
                if item[-1] != 0:
                    diffs.append(item[-1])

                if item[-1] > 0:
                    p += 1
                elif item[-1] < 0:
                    n += 1

            print(f"Total: {len(data)}\nPositive Errors: {p}\n"
                  f"Negative Errors: {n}\n   Total Errors: {len(diffs)}")

            if options.coff:
                coff = sum(diffs) / len(diffs)
                print(f"Average Coefficient: {coff}")
    elif options.ck_dates: # -c
        if options.start is None or options.end is None:
            print("If option -c is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.create_date_lists(options)
            bad_items = dt.check_long_date_from_short_date(data)
            bad_items = bad_items if bad_items else "All dates match."
            pprint.pprint(bad_items)
    elif options.leap_years: # -e
        if options.start is None or options.end is None:
            print("If option -e is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            [print(f"{str(date):<14} "
                   f"{str(g_date):<21} "
                   f"{weekday} "
                   f"{wd_name:<8} "
                   f"{m_name:<10} "
                   f"{str(leap):5s} "
                   f"{days:<3} "
                   f"{total:>3}"
                   ) for (date, g_date, weekday, wd_name, m_name,
                          leap, days, total) in dt.find_leap_years(options)]
    elif options.list: # -l
        if options.start is None or options.end is None:
            print("If option -l is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.create_date_lists(options)
            pprint.pprint(data)
    elif options.precursor: # -p
        if options.start is None or options.end is None:
            print("If option -p is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.find_coefficents_precursor(options)
            [print(f"{gy:> 5} {by:> 5}, {n:<1} {a:>2}")
             for gy, by, n, a in data]
            print(f"Total years: {len(data)}")
    elif options.coeff: # -q
        if options.start is None or options.end is None:
            print("If option -q is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            [print(item) for item in dt.find_coefficents(options)]
    elif options.range != 0: # -r
        data = dt.get_range(options.range)
        [print(item) for item in data]
        print(f"Total years: {len(data)}")
    elif options.g_dates: # -g
        if options.start is None or options.end is None:
            print("If option -g is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print("b_date           "
                  "bjd            "
                  "g_date                         "
                  "gjd            "
                  "diff "
                  "offby")
            [print(f"{str(b_date):<16} "
                   f"{bjd:<14} "
                   f"{str(g_date):<30} "
                   f"{gjd:<14} "
                   f"{diff:<4} "
                   f"{offby}"
                   )
             for (b_date, bjd, g_date,
                  gjd, diff, offby) in dt.find_gregorian_dates(options)]
    elif options.twenty_four: # -t
        if options.start is None or options.end is None:
            print("If option -t is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print("Badi Date         Gregorian Date        "
                  "SS1 Frac SS2 Frac SS2-SS1  HMS Diff")
            [print(f"{str(b_date):<17} "
                   f"{str(g_date):<21} "
                   f"{fss0:<8} "
                   f"{fss1:<8} "
                   f"{ss_diff:<8} "
                   f"{str(hms):19}"
                   ) for (b_date, g_date, fss0, fss1,
                          ss_diff, hms) in dt.twenty_four_hours(options)]
    elif options.weekday: # -w
        if options.start is None or options.end is None:
            print("If option -w is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            [print(f"{str(date):15} "
                   f"{r_day:8} "
                   f"{idx} "
                   f"{bwd:8} "
                   f"{gwd:9}"
                   ) for (date, r_day, idx,
                       bwd, gwd) in dt.find_day_of_week(options)]
    else:
        parser.print_help()

    sys.exit(ret)
