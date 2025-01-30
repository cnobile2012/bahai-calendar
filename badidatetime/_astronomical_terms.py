# -*- coding: utf-8 -*-
#
# badidatetime/astronomical_terms.py
#
# noqa: E201
#
__docformat__ = "restructuredtext en"


class AstronomicalTerms:

    # Planet's heliocentric ecliptical longitude L
    _L0_A = (175347046, 3341656, 34894, 3497, 3418, 3136, 2676, 2343, 1324,
             1273, 1199, 990, 902, 857, 780, 753, 505, 492, 357, 317, 284, 271,
             243, 206, 205, 202, 156, 132, 126, 115, 103, 102, 102, 99, 98, 86,
             85, 85, 80, 79, 75, 74, 74, 70, 62, 61, 57, 56, 56, 52, 52, 51,
             49, 41, 41, 39, 37, 37, 36, 36, 33, 30, 30, 25)
    _L0_B = (0, 4.6692568, 4.6261, 2.7441, 2.8289, 3.6277, 4.4181, 6.1352,
             0.7425, 2.0371, 1.1096, 5.233, 2.045, 3.508, 1.179, 2.533, 4.583,
             4.205, 2.92, 5.849, 1.899, 0.315, 0.345, 4.806, 1.869, 2.458,
             0.833, 3.411, 1.083, 0.645, 0.636, 0.976, 4.267, 6.21, 0.68, 5.98,
             1.3, 3.67, 1.81, 3.04, 1.76, 3.5, 4.68, 0.83, 3.98, 1.82, 2.78,
             4.39, 3.47, 0.19, 1.33, 0.28, 0.49, 5.37, 2.4, 6.17, 6.04, 2.57,
             1.71, 1.78, 0.59, 0.44, 2.74, 3.16)
    _L0_C = (0, 6283.07585, 12566.1517, 5753.3849, 3.5231, 77713.7715,
             7860.4194, 3930.2097, 11506.7698, 529.691, 1577.3435, 5884.927,
             26.298, 398.149, 5223.694, 5507.553, 18849.228, 775.523, 0.067,
             11790.629, 796.298, 10977.079, 5486.778, 2544.314, 5573.143,
             6069.777, 213.299, 2942.463, 20.775, 0.98, 4694.003, 15720.839,
             7.114, 2146.17, 155.42, 161000.69, 6275.96, 71430.7, 17260.15,
             12036.46, 5088.63, 3154.69, 801.82, 9437.76, 8827.39, 7084.9,
             6286.6, 14143.5, 6279.55, 12139.55, 1748.02, 5856.48, 1194.45,
             8429.24, 19651.05, 10447.39, 10213.29, 1059.38, 2352.87, 6812.77,
             17789.85, 83996.85, 1349.87, 4690.48)
    _L1_A = (628331966747, 206059, 4303, 425, 119, 109, 93, 72, 68, 67, 59,
             56, 45, 36, 29, 21, 19, 19, 17, 16, 16, 15, 12, 12, 12, 12, 11,
             10, 10, 9, 9, 8, 6, 6)
    _L1_B = (0, 2.678235, 2.6351, 1.59, 5.796, 2.966, 2.59, 1.14, 1.87, 4.41,
             2.89, 2.17, 0.4, 0.47, 2.65, 5.34, 1.85, 4.97, 2.99, 0.03, 1.43,
             1.21, 2.83, 3.26, 5.27, 2.08, 0.77, 1.3, 4.24, 2.7, 5.64, 5.3,
             2.65, 4.67)
    _L1_C = (0, 6283.07585, 12566.1517, 3.523, 26.298, 1577.344, 18849.23,
             529.69, 398.15, 5507.55, 5223.69, 155.42, 796.3, 775.52, 7.11,
             0.98, 5486.78, 213.3, 6275.96, 2544.31, 2146.17, 10977.08,
             1748.02, 5088.63, 1194.45, 4694.0, 553.57, 6286.6, 1349.87,
             242.73, 951.72, 2352.87, 9437.76, 4690.48)
    _L2_A = (52919, 8720, 309, 27, 16, 16, 10, 9, 7, 5, 4, 4, 3, 3, 3, 3, 3,
             3, 2, 2)
    _L2_B = (0, 1.0721, 0.867, 0.05, 5.19, 3.68, 0.76, 2.06, 0.83, 4.66, 1.03,
             3.44, 5.14, 6.05, 1.19, 6.12, 0.31, 2.28, 4.38, 3.75)
    _L2_C = (0, 6283.0758, 12566.152, 3.52, 26.3, 155.42, 18849.23, 77713.77,
             775.52, 1577.34, 7.11, 5573.14, 796.3, 5507.55, 242.73, 529.69,
             398.15, 553.57, 5223.69, 0.98)
    _L3_A = (289, 35, 17, 3, 1, 1, 1)
    _L3_B = (5.844, 0, 5.49, 5.2, 4.72, 5.3, 5.97)
    _L3_C = (6283.076, 0, 12566.15, 155.42, 3.52, 18849.23, 242.73)
    _L4_A = (114, 8, 1)
    _L4_B = (3.142, 4.13, 3.84)
    _L4_C = (0, 6283.08, 12566.15)
    _L5_A = (1,)
    _L5_B = (3.14,)
    _L5_C = (0,)

    # Planet's heliocentric ecliptical latitude B
    _B0_A = (280, 102, 80, 44, 32)
    _B0_B = (3.199, 5.422, 3.88, 3.70, 4.0)
    _B0_C = (84334.662, 5507.553, 5223.69, 2352.87, 1577.34)
    _B1_A = (9, 6)
    _B1_B = (3.90, 1.73)
    _B1_C = (5507.55, 5223.69)

    # The radius vector R
    _R0_A = (100013989, 1670700, 13956, 3084, 1628, 1576, 925, 542, 472, 346,
             329, 307, 243, 212, 186, 175, 110, 98, 86, 86, 65, 63, 57, 56,
             49, 47, 45, 43, 39, 38, 37, 37, 36, 35, 33, 32, 32, 28, 28, 26)
    _R0_B = (0, 3.0984635, 3.05525, 5.1985, 1.1739, 2.8469, 5.453, 4.564,
             3.661, 0.964, 5.9, 0.299, 4.273, 5.847, 5.022, 3.012, 5.055,
             0.89, 5.69, 1.27, 0.27, 0.92, 2.01, 5.24, 3.25, 2.58, 5.54, 6.01,
             5.36, 2.39, 0.83, 4.90, 1.67, 1.84, 0.24, 0.18, 1.78, 1.21, 1.9,
             4.59)
    _R0_C = (0, 6283.07585, 12566.1517, 77713.7715, 5753.3849, 7860.4194,
             11506.77, 3930.21, 5884.927, 5507.553, 5223.694, 5573.143,
             11790.629, 1577.344, 10977.079, 18849.228, 5486.778, 6069.78,
             15720.84, 161000.69, 17260.15, 529.69, 83996.85, 71430.7, 2544.31,
             775.52, 9437.76, 6275.96, 4694.0, 8827.39, 19651.05, 12139.55,
             12036.46, 2942.46, 7084.90, 5088.63, 398.15, 6286.6, 6279.55,
             10447.39)
    _R1_A = (103019, 1721, 702, 32, 31, 25, 18, 10, 9, 9)
    _R1_B = (1.10749, 1.0644, 3.142, 1.02, 2.84, 1.32, 1.42, 5.91, 1.42, 0.27)
    _R1_C = (6283.07585, 12566.1517, 0, 18849.23, 5507.55, 5223.69, 1577.34,
             10977.08, 6275.96, 5486.78)
    _R2_A = (4359, 124, 12, 9, 6, 3)
    _R2_B = (5.7846, 5.579, 3.14, 3.63, 1.87, 5.47)
    _R2_C = (6283.0758, 12566.152, 0, 77713.77, 5573.14, 18849.23)
    _R3_A = (145, 5)
    _R3_B = (4.273, 3.92)
    _R3_C = (6283.076, 12566.15)
    _R4_A = (4,)
    _R4_B = (2.56,)
    _R4_C = (6283.08,)

    # Periodic terms for the nutation in longitude and obliquity.
    # Nutation in Longitude and Obliquity referred to mean ecliptic of date.
    # Epoch J2000.0 (JD 2451 545.0 TDB) T in Julian Centuries
    #        Arguments       Period  Longitude           Obliquity
    #     LM  LS  F   D  OM   days   psi_sin     t*sin   eps_cos   t*cos
    _NUT = (
        ( 0,  0,  0,  0, 1, -6798.4, -171996.0, -174.2, 92025.0,  8.9),
        ( 0,  0,  2, -2, 2,   182.6,  -13187.0,   -1.6,  5736.0, -3.1),
        ( 0,  0,  2,  0, 2,    13.7,   -2274.0,   -0.2,   977.0, -0.5),
        ( 0,  0,  0,  0, 2, -3399.2,    2062.0,    0.2,  -895.0,  0.5),
        ( 0, -1,  0,  0, 0,  -365.3,   -1426.0,    3.4,    54.0, -0.1),
        ( 1,  0,  0,  0, 0,    27.6,     712.0,    0.1,    -7.0,  0.0),
        ( 0,  1,  2, -2, 2,   121.7,    -517.0,    1.2,   224.0, -0.6),
        ( 0,  0,  2,  0, 1,    13.6,    -386.0,   -0.4,   200.0,  0.0),
        ( 1,  0,  2,  0, 2,     9.1,    -301.0,    0.0,   129.0, -0.1),
        ( 0, -1,  2, -2, 2,   365.2,     217.0,   -0.5,   -95.0,  0.3),
        (-1,  0,  0,  2, 0,    31.8,     158.0,    0.0,    -1.0,  0.0),
        ( 0,  0,  2, -2, 1,   177.8,     129.0,    0.1,   -70.0,  0.0),
        (-1,  0,  2,  0, 2,    27.1,     123.0,    0.0,   -53.0,  0.0),
        ( 1,  0,  0,  0, 1,    27.7,      63.0,    0.1,   -33.0,  0.0),
        ( 0,  0,  0,  2, 0,    14.8,      63.0,    0.0,   -2.0,   0.0),
        (-1,  0,  2,  2, 2,     9.6,     -59.0,    0.0,   26.0,   0.0),
        (-1,  0,  0,  0, 1,   -27.4,     -58.0,   -0.1,   32.0,   0.0),
        ( 1,  0,  2,  0, 1,     9.1,     -51.0,    0.0,   27.0,   0.0),
        (-2,  0,  0,  2, 0,  -205.9,     -48.0,    0.0,    1.0,   0.0),
        (-2,  0,  2,  0, 1,  1305.5,      46.0,    0.0,  -24.0,   0.0),
        ( 0,  0,  2,  2, 2,     7.1,     -38.0,    0.0,   16.0,   0.0),
        ( 2,  0,  2,  0, 2,     6.9,     -31.0,    0.0,   13.0,   0.0),
        ( 2,  0,  0,  0, 0,    13.8,      29.0,    0.0,   -1.0,   0.0),
        ( 1,  0,  2, -2, 2,    23.9,      29.0,    0.0,  -12.0,   0.0),
        ( 0,  0,  2,  0, 0,    13.6,      26.0,    0.0,   -1.0,   0.0),
        ( 0,  0,  2, -2, 0,   173.3,     -22.0,    0.0,    0.0,   0.0),
        (-1,  0,  2,  0, 1,    27.0,      21.0,    0.0,  -10.0,   0.0),
        ( 0,  2,  0,  0, 0,   182.6,      17.0,   -0.1,    0.0,   0.0),
        ( 0,  2,  2, -2, 2,    91.3,     -16.0,    0.1,    7.0,   0.0),
        (-1,  0,  0,  2, 1,    32.0,      16.0,    0.0,   -8.0,   0.0),
        ( 0,  1,  0,  0, 1,   386.0,     -15.0,    0.0,    9.0,   0.0),
        ( 1,  0,  0, -2, 1,   -31.7,     -13.0,    0.0,    7.0,   0.0),
        ( 0, -1,  0,  0, 1,  -346.6,     -12.0,    0.0,    6.0,   0.0),
        ( 2,  0, -2,  0, 0, -1095.2,      11.0,    0.0,    0.0,   0.0),
        (-1,  0,  2,  2, 1,     9.5,     -10.0,    0.0,    5.0,   0.0),
        ( 1,  0,  2,  2, 2,     5.6,      -8.0,    0.0,    3.0,   0.0),
        ( 0, -1,  2,  0, 2,    14.2,      -7.0,    0.0,    3.0,   0.0),
        ( 0,  0,  2,  2, 1,     7.1,      -7.0,    0.0,    3.0,   0.0),
        ( 1,  1,  0, -2, 0,   -34.8,      -7.0,    0.0,    0.0,   0.0),
        ( 0,  1,  2,  0, 2,    13.2,       7.0,    0.0,   -3.0,   0.0),
        (-2,  0,  0,  2, 1,  -199.8,      -6.0,    0.0,    3.0,   0.0),
        ( 0,  0,  0,  2, 1,    14.8,      -6.0,    0.0,    3.0,   0.0),
        ( 2,  0,  2, -2, 2,    12.8,       6.0,    0.0,   -3.0,   0.0),
        ( 1,  0,  0,  2, 0,     9.6,       6.0,    0.0,    0.0,   0.0),
        ( 1,  0,  2, -2, 1,    23.9,       6.0,    0.0,   -3.0,   0.0),
        ( 0,  0,  0, -2, 1,   -14.7,      -5.0,    0.0,    3.0,   0.0),
        ( 0, -1,  2, -2, 1,   346.6,      -5.0,    0.0,    3.0,   0.0),
        ( 2,  0,  2,  0, 1,     6.9,      -5.0,    0.0,    3.0,   0.0),
        ( 1, -1,  0,  0, 0,    29.8,       5.0,    0.0,    0.0,   0.0),
        ( 1,  0,  0, -1, 0,   411.8,      -4.0,    0.0,    0.0,   0.0),
        ( 0,  0,  0,  1, 0,    29.5,      -4.0,    0.0,    0.0,   0.0),
        ( 0,  1,  0, -2, 0,   -15.4,      -4.0,    0.0,    0.0,   0.0),
        ( 1,  0, -2,  0, 0,   -26.9,       4.0,    0.0,    0.0,   0.0),
        ( 2,  0,  0, -2, 1,   212.3,       4.0,    0.0,   -2.0,   0.0),
        ( 0,  1,  2, -2, 1,   119.6,       4.0,    0.0,   -2.0,   0.0),
        ( 1,  1,  0,  0, 0,    25.6,      -3.0,    0.0,    0.0,   0.0),
        ( 1, -1,  0, -1, 0, -3232.9,      -3.0,    0.0,    0.0,   0.0),
        (-1, -1,  2,  2, 2,     9.8,      -3.0,    0.0,    1.0,   0.0),
        ( 0, -1,  2,  2, 2,     7.2,      -3.0,    0.0,    1.0,   0.0),
        ( 1, -1,  2,  0, 2,     9.4,      -3.0,    0.0,    1.0,   0.0),
        ( 3,  0,  2,  0, 2,     5.5,      -3.0,    0.0,    1.0,   0.0),
        (-2,  0,  2,  0, 2,  1615.7,      -3.0,    0.0,    1.0,   0.0),
        ( 1,  0,  2,  0, 0,     9.1,       3.0,    0.0,    0.0,   0.0),
        (-1,  0,  2,  4, 2,     5.8,      -2.0,    0.0,    1.0,   0.0),
        ( 1,  0,  0,  0, 2,    27.8,      -2.0,    0.0,    1.0,   0.0),
        (-1,  0,  2, -2, 1,   -32.6,      -2.0,    0.0,    1.0,   0.0),
        ( 0, -2,  2, -2, 1,  6786.3,      -2.0,    0.0,    1.0,   0.0),
        (-2,  0,  0,  0, 1,   -13.7,      -2.0,    0.0,    1.0,   0.0),
        ( 2,  0,  0,  0, 1,    13.8,       2.0,    0.0,   -1.0,   0.0),
        ( 3,  0,  0,  0, 0,     9.2,       2.0,    0.0,    0.0,   0.0),
        ( 1,  1,  2,  0, 2,     8.9,       2.0,    0.0,   -1.0,   0.0),
        ( 0,  0,  2,  1, 2,     9.3,       2.0,    0.0,   -1.0,   0.0),
        ( 1,  0,  0,  2, 1,     9.6,      -1.0,    0.0,    0.0,   0.0),
        ( 1,  0,  2,  2, 1,     5.6,      -1.0,    0.0,    1.0,   0.0),
        ( 1,  1,  0, -2, 1,   -34.7,      -1.0,    0.0,    0.0,   0.0),
        ( 0,  1,  0,  2, 0,    14.2,      -1.0,    0.0,    0.0,   0.0),
        ( 0,  1,  2, -2, 0,   117.5,      -1.0,    0.0,    0.0,   0.0),
        ( 0,  1, -2,  2, 0,  -329.8,      -1.0,    0.0,    0.0,   0.0),
        ( 1,  0, -2,  2, 0,    23.8,      -1.0,    0.0,    0.0,   0.0),
        ( 1,  0, -2, -2, 0,    -9.5,      -1.0,    0.0,    0.0,   0.0),
        ( 1,  0,  2, -2, 0,    32.8,      -1.0,    0.0,    0.0,   0.0),
        ( 1,  0,  0, -4, 0,   -10.1,      -1.0,    0.0,    0.0,   0.0),
        ( 2,  0,  0, -4, 0,   -15.9,      -1.0,    0.0,    0.0,   0.0),
        ( 0,  0,  2,  4, 2,     4.8,      -1.0,    0.0,    0.0,   0.0),
        ( 0,  0,  2, -1, 2,    25.4,      -1.0,    0.0,    0.0,   0.0),
        (-2,  0,  2,  4, 2,     7.3,      -1.0,    0.0,    1.0,   0.0),
        ( 2,  0,  2,  2, 2,     4.7,      -1.0,    0.0,    0.0,   0.0),
        ( 0, -1,  2,  0, 1,    14.2,      -1.0,    0.0,    0.0,   0.0),
        ( 0,  0, -2,  0, 1,   -13.6,      -1.0,    0.0,    0.0,   0.0),
        ( 0,  0,  4, -2, 2,    12.7,       1.0,    0.0,    0.0,   0.0),
        ( 0,  1,  0,  0, 2,   409.2,       1.0,    0.0,    0.0,   0.0),
        ( 1,  1,  2, -2, 2,    22.5,       1.0,    0.0,   -1.0,   0.0),
        ( 3,  0,  2, -2, 2,     8.7,       1.0,    0.0,    0.0,   0.0),
        (-2,  0,  2,  2, 2,    14.6,       1.0,    0.0,   -1.0,   0.0),
        (-1,  0,  0,  0, 2,   -27.3,       1.0,    0.0,   -1.0,   0.0),
        ( 0,  0, -2,  2, 1,  -169.0,       1.0,    0.0,    0.0,   0.0),
        ( 0,  1,  2,  0, 1,    13.1,       1.0,    0.0,    0.0,   0.0),
        (-1,  0,  4,  0, 2,     9.1,       1.0,    0.0,    0.0,   0.0),
        ( 2,  1,  0, -2, 0,   131.7,       1.0,    0.0,    0.0,   0.0),
        ( 2,  0,  0,  2, 0,     7.1,       1.0,    0.0,    0.0,   0.0),
        ( 2,  0,  2, -2, 1,    12.8,       1.0,    0.0,   -1.0,   0.0),
        ( 2,  0, -2,  0, 1,  -943.2,       1.0,    0.0,    0.0,   0.0),
        ( 1, -1,  0, -2, 0,   -29.3,       1.0,    0.0,    0.0,   0.0),
        (-1,  0,  0,  1, 1,  -388.3,       1.0,    0.0,    0.0,   0.0),
        (-1, -1,  0,  2, 1,    35.0,       1.0,    0.0,    0.0,   0.0),
        ( 0,  1,  0,  1, 0,    27.3,       1.0,    0.0,    0.0,   0.0),
        )

    _ABER_A = (
        (118.568,   87.5287,  359993.7286),
        (  2.476,   85.0561,  719987.4571),
        (  1.376,   27.8202, 4452671.1152),
        (  0.119,   73.1375,  450368.8564),
        (  0.114,  337.2264,  329644.6718),
        (  0.086,  222.54,    659289.3436),
        (  0.078,  162.8136, 9224659.7915),
        (  0.054,   82.5823, 1079981.1857),
        (  0.052,  171.5189,  225184.4282),
        (  0.034,   30.3214, 4092677.3866),
        (  0.033,  119.8105,  337181.4711),
        (  0.023,  247.5418,  299295.6151),
        (  0.023,  325.1526,  315559.556),
        (  0.021,  155.1241,  675553.2846),
        )

    _ABER_B = (
        (7.311,  333.4515,  359993.7286),
        (0.305,  330.9814,  719987.4571),
        (0.01,   328.517,  1079981.1857),
        )

    _ABER_C = (
        (0.309,  241.4518,  359993.7286),
        (0.21,   205.0482,  719987.4571),
        (0.004,  297.861,  4452671.1152),
        (0.01,   154.7066,  359993.7286),
        )

    _EQ_SO_A = (485, 203, 199, 182, 156, 136, 77, 74, 70, 58, 52, 50, 45,
                44, 29, 18, 17, 16, 14, 12, 12, 12, 9, 8)
    _EQ_SO_B = (324.96, 337.23, 342.08, 27.85, 73.14, 171.52, 222.54, 296.72,
                243.58, 119.81, 297.17, 21.02, 247.54, 325.15, 60.93, 155.12,
                288.79, 198.04, 199.76, 95.39, 287.11, 320.81, 227.73, 15.45)
    _EQ_SO_C = (1934.136, 32964.467, 20.186, 445267.112, 45036.886, 22518.443,
                65928.934, 3034.906, 9037.513, 33718.147, 150.678, 2281.226,
                29929.562, 31555.956, 4443.417, 67555.328, 4562.452, 62894.029,
                31436.921, 14577.848, 31931.756, 34777.259, 1222.114, 16859.074)
