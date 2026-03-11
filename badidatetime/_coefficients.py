# -*- coding: utf-8 -*-
#
# badidatetime/_coefficients.py
#
__docformat__ = "restructuredtext en"


class Coefficients:
    """
    Provides the coefficients used for finding the Julian Period day in the
    `badi_calendar.BahaiCalendar` class and the POSIX timestamp in the
    `datetime.datetime` class.
    """
    P1 = ((-1783, -1747), (-1651, -1615), (-1499, -1483), (-1383, -1347),
          (-1251, -1215), (-1099, -1083), (-983, -947), (-851, -815),
          (-699, -683), (-583, -547), (-451, -415), (-299, -283), (-179, -143),
          (-47, -11), (101, 117), (213, 249), (345, 381), (501, 513),
          (609, 645), (741, 777), (901, 909), (1005, 1041), (1137, 1162))
    P1100 = ((-1699, -1683), (-1299, -1283), (-899, -883), (-499, -483),
             (-99, -79), (301, 313), (701, 709), (1101, 1105))
    P1110 = ((-1799, -1783), (-1683, -1651), (-1399, -1383), (-1283, -1251),
             (-999, -983), (-883, -851), (-599, -583), (-483, -451),
             (-199, -179), (-79, -47), (201, 213), (313, 345), (601, 609),
             (709, 741), (1001, 1005), (1105, 1137))
    P2 = ((-1519, -1499), (-1119, -1099), (-719, -699), (-319, -299),
          (85, 101), (477, 501), (873, 901))
    P2111 = ((-1747, -1715), (-1615, -1583), (-1483, -1451), (-1347, -1315),
             (-1215, -1183), (-1083, -1051), (-947, -915), (-815, -783),
             (-683, -651), (-547, -515), (-415, -383), (-283, -243),
             (-143, -111), (-11, 21), (117, 149), (249, 281), (381, 413),
             (513, 545), (645, 677), (777, 809), (909, 941), (1041, 1073))
    P2211 = ((-1843, -1815), (-1715, -1699), (-1583, -1551), (-1451, -1415),
             (-1315, -1299), (-1183, -1151), (-1051, -1019), (-915, -899),
             (-783, -751), (-651, -619), (-515, -499), (-383, -351),
             (-243, -211), (-111, -99), (21, 53), (149, 185), (281, 301),
             (413, 445), (545, 577), (677, 701), (809, 841), (941, 973),
             (1073, 1101))
    P2221 = ((-1815, -1799), (-1551, -1519), (-1415, -1399), (-1151, -1119),
             (-1019, -999), (-751, -719), (-619, -599), (-351, -319),
             (-211, -199), (53, 85), (185, 201), (445, 477), (577, 601),
             (841, 873), (973, 1001))

    def _get_day_coeff(self, year: int) -> int:
        """
        Generate the coefficients for correcting Badí' vernal equinox dates.

        .. note::

           | General ranges are determined with:
           | ./contrib/misc/badi_jd_tests.py -p -S start_year -E end_year

           Where -S is the 1st year and -E is the nth year + 1 that needs to
           be process. Use the following command to test the results of each
           segment.
           ./contrib/misc/badi_jd_tests.py -qXS start_year -E end_year

           Full range is -1842 to 1161.

        :param int year: The year to find a coefficient for.
        :returns: The coefficient.
        :rtype: int
        """
        def process_segment(y, a=0, onoff0=(), b=0, onoff1=()):
            func = lambda y, onoff: 0 < y < 100 and y % 4 in onoff
            coeff = 0

            if a and func(y, onoff0):    # Whatever is passed in onoff0.
                coeff = a
            elif b and func(y, onoff1):  # Whatever is passed in onoff1.
                coeff = b

            return coeff

        def process_segments(year, pn, a=0, onoff0=(), b=0, onoff1=()):
            coeff = 0

            for start, end in pn:
                if year in range(start, end):
                    # Start to end (range -S start -E end)
                    coeff0 = process_segment(end - year, a=a, onoff0=onoff0)
                    coeff1 = process_segment(end - year, b=b, onoff1=onoff1)
                    coeff = coeff0 if coeff0 != 0 else coeff1

            return coeff

        return (process_segments(year, self.P1, -1, (0, 1, 2, 3))
                or process_segments(year, self.P1100, -1, (0, 3))
                or process_segments(year, self.P1110, -1, (0, 2, 3))
                or process_segments(year, self.P2, -2, (0, 1, 2, 3))
                or process_segments(year, self.P2111, -2, (0,), -1, (1, 2, 3))
                or process_segments(year, self.P2211, -2, (0, 3), -1, (1, 2))
                or process_segments(year, self.P2221, -2, (0, 2, 3), -1, (1,))
                or 0)
