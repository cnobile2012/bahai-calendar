#!/usr/bin/env python

import os
import sys
import math

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from bahai_calendar import BahaiCalendar, GregorianCalendar


class DumpFindMomentOfEquinoxesOrSolstices(BahaiCalendar):
    WC_DATA = { # WC Naw-Ruz Vernal Equinox (Dynamical Time (TD))
        172: ((2015, 3, 21), (2015, 3, 20, 22, 46, 16)),
        173: ((2016, 3, 20), (2016, 3, 20,  4, 31, 19)),
        174: ((2017, 3, 20), (2017, 3, 20, 10, 29, 46)),
        175: ((2018, 3, 21), (2018, 3, 20, 16, 16, 36)),
        176: ((2019, 3, 21), (2019, 3, 20, 21, 59, 34)),
        177: ((2020, 3, 20), (2020, 3, 20,  3, 50, 45)),
        178: ((2021, 3, 20), (2021, 3, 20,  9, 38, 37)),
        179: ((2022, 3, 21), (2022, 3, 20, 15, 34, 33)),
        180: ((2023, 3, 21), (2023, 3, 20, 21, 25, 35)),
        181: ((2024, 3, 20), (2024, 3, 20,  3,  7, 32)),
        182: ((2025, 3, 20), (2025, 3, 20,  9,  2, 37)),
        183: ((2026, 3, 21), (2026, 3, 20, 14, 47,  5)),
        184: ((2027, 3, 21), (2027, 3, 20, 20, 25, 49)),
        185: ((2028, 3, 20), (2028, 3, 20,  2, 18, 16)),
        186: ((2029, 3, 20), (2029, 3, 20,  8,  3,  6)),
        187: ((2030, 3, 20), (2030, 3, 20, 13, 53, 14)),
        188: ((2031, 3, 21), (2031, 3, 20, 19, 42,  6)),
        189: ((2032, 3, 20), (2032, 3, 20,  1, 23,  1)),
        190: ((2033, 3, 20), (2033, 3, 20,  7, 23, 51)),
        191: ((2034, 3, 20), (2034, 3, 20, 13, 18, 38)),
        192: ((2035, 3, 21), (2035, 3, 20, 19,  3, 52)),
        193: ((2036, 3, 20), (2036, 3, 20,  1,  3, 58)),
        194: ((2037, 3, 20), (2037, 3, 20,  6, 51, 24)),
        195: ((2038, 3, 20), (2038, 3, 20, 12, 41, 47)),
        196: ((2039, 3, 21), (2039, 3, 20, 18, 33, 11)),
        197: ((2040, 3, 20), (2040, 3, 20,  0, 12, 51)),
        198: ((2041, 3, 20), (2041, 3, 20,  6,  7, 58)),
        199: ((2042, 3, 20), (2042, 3, 20, 11, 54, 29)),
        200: ((2043, 3, 21), (2043, 3, 20, 17, 28, 58)),
        201: ((2044, 3, 20), (2044, 3, 19, 23, 21, 44)),
        202: ((2045, 3, 20), (2045, 3, 20,  5,  8, 49)),
        203: ((2046, 3, 20), (2046, 3, 20, 10, 59,  4)),
        204: ((2047, 3, 21), (2047, 3, 20, 16, 53, 53)),
        205: ((2048, 3, 20), (2048, 3, 19, 22, 35,  4)),
        206: ((2049, 3, 20), (2049, 3, 20,  4, 29, 52)),
        207: ((2050, 3, 20), (2050, 3, 20, 10, 20, 51)),
        208: ((2051, 3, 21), (2051, 3, 20, 16,  0, 28)),
        209: ((2052, 3, 20), (2052, 3, 19, 21, 57, 25)),
        210: ((2053, 3, 20), (2053, 3, 20,  3, 48, 44)),
        211: ((2054, 3, 20), (2054, 3, 20,  9, 35, 52)),
        212: ((2055, 3, 21), (2055, 3, 20, 15, 30,  3)),
        213: ((2056, 3, 20), (2056, 3, 19, 21, 12, 32)),
        214: ((2057, 3, 20), (2057, 3, 20,  3,  9, 25)),
        215: ((2058, 3, 20), (2058, 3, 20,  9,  6, 32)),
        216: ((2059, 3, 20), (2059, 3, 20, 14, 45, 48)),
        217: ((2060, 3, 20), (2060, 3, 19, 20, 40,  5)),
        218: ((2061, 3, 20), (2061, 3, 20,  2, 27, 52)),
        219: ((2062, 3, 20), (2062, 3, 20,  8,  9,  8)),
        220: ((2063, 3, 20), (2063, 3, 20, 14,  0, 51)),
        221: ((2064, 3, 20), (2064, 3, 19, 19, 40, 20)),
        }

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()

    def dump(self):
        """
        Fine the sunset after the vernal equinox.
        """
        lam = self.SPRING
        data = []

        for year, item in self.WC_DATA.items():
            wc_ss = item[0] # Get the Gregorian date
            first_of_march = (wc_ss[0], wc_ss[1], 1) # Replace day with the 1st
            first_ss_jd = self.gc.jd_from_gregorian_date(first_of_march)
            jd = self.gc.jd_from_gregorian_date(first_of_march) # Get UT Julian
            my_ve_jde = self.find_moment_of_equinoxes_or_solstices(jd)
            my_ve = my_ve_jde
            #my_ve = my_ve_jde - self.delta_t(my_ve_jde)
            my_ss = self._sun_setting(my_ve, 35.696111, 51.423056, 3.5)
            my_ss_jd = my_ve + my_ss

            if my_ve > my_ss_jd:
                my_ss = self._sun_setting(
                    my_ve + 1, 35.696111, 51.423056, 3.5)
                my_ss_jd = my_ve + 1 + my_ss

            my_g_ss = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                my_ss_jd))
            my_ve_g = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                my_ve))
            meeus_ve = item[1]
            data.append((year,           # Baha'i year
                         wc_ss,          # WC sunset
                         my_g_ss,        # My Gregorian sunset
                         my_ss_jd-my_ve, # Sunset difference
                         my_ve_g,        # My VE
                         meeus_ve,       # Meeus VE
                         ))

        return data

    def determine_year_length(self):
        data = []
        wc_pre_year = 0
        ss_pre_year = 0
        data.append("Year "
                    "VE STD Date                   "
                    "WC Naw-Ruz    "
                    #"B "
                    "Sunset                        "
                    #"SS Days "
                    "(SS-VE)>=24hrs"
                    )

        for year, item in self.WC_DATA.items():
            # World Center data
            wc_naw_ruz = item[0]
            wc_nr_day = self.gc.fixed_from_gregorian(wc_naw_ruz)
            # Meeus tables UTC times
            ve_utc_day = self.gc.jd_from_gregorian_date(item[1])
            ve_std_day = self.standard_from_universal(ve_utc_day)
            ve_std_date = self.gc.gregorian_from_jd(ve_std_day)
            # Sunset *** TODO *** Change to Julian Period day
            rd_ss_day = self.sunset(ve_utc_day)
            badi_ss_date = self.gc.gregorian_from_fixed(rd_ss_day)
            # VE more than 24 hours before sunset.
            hours = True if (rd_ss_day - ve_std_day) >= 24 else False
            data.append(f"{year}, "
                        f"{str(ve_std_date):<29} "
                        f"{wc_naw_ruz} "
                        #f"{wc_nr_day} "
                        #f"{(ve_std_day <= wc_nr_day):>1} "
                        f"{str(badi_ss_date):<29} "
                        f"{hours}"
                        #f"{(ve_std_day <= rd_ss_day):>1} "
                        #f"{wc_nr_day - wc_pre_year} "
                        #f"{rd_ss_day - ss_pre_year}"
                        )
            wc_pre_year = wc_nr_day
            ss_pre_year = rd_ss_day

        return data

    def find_coefficients(self):
        data = []
        g_lte = 1.962
        g_gt = 0.038
        d0 = 365056.574 # or 365056.575
        n0 = 730120 # 730113

        for c in range(n0-20, 730151):
            rd_lte = 716241 / d0
            rd_gt = (716241 - c) / d0
            #data.append(f"{d0:<010}, {d}, {g_lte}, {rd_lte}")
            data.append(f"{c}, {g_gt}, {rd_gt}")

        return data

    def number_of_days(self):
        for year, item in self.WC_DATA.items():
            wc_naw_ruz, vernal_equinox = item





if __name__ == "__main__":
    import pprint
    import argparse

    parser = argparse.ArgumentParser(
        description=("Find Vernal Equinox and sunset."))
    parser.add_argument(
        '-v', '--ve-ss', action='store_true', default=False, dest='ve_ss',
        help="Find the Vernal Equinox and sunset.")
    parser.add_argument(
        '-d', '--days-in-years', action='store_true', default=False,
        dest='days_in_years',
        help="Find the number of days in each of 50 years.")

    options = parser.parse_args()
    exclusive_error = (options.ve_ss, options.days_in_years)
    assert exclusive_error.count(True) <= 1, (
        "Options -v and -d  are exclusive.")

    cfmes = DumpFindMomentOfEquinoxesOrSolstices()

    if options.ve_ss:
        data = [f"{year} "
                f"{str(wc_ss):<13} "
                f"{str(my_g_ss):<41} "
                f"{diff:<19} "
                f"{str(my_ve):<43}"
                f"{str(dt_ve):<26} "
                for (year,
                     wc_ss,
                     my_g_ss,
                     diff,
                     my_ve,
                     dt_ve,
                     ) in cfmes.dump()]
        [print(line) for line in data]

    if options.days_in_years:
        data = cfmes.number_of_days()


    #data = cfmes.determine_year_length()
    #data = cfmes.find_coefficients()
    #[print(line) for line in data]
    #pprint.pprint(data) # Raw data
