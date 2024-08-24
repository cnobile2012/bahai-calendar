#!/usr/bin/env python

import os
import sys
import math

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar


class DumpFindMomentOfEquinoxesOrSolstices(BahaiCalendar):
    WC_DATA = ( # WC Naw-Ruz Vernal Equinox (Dynamical Time (UT))
        (172, (2015, 3, 21), (2015, 3, 20, 22, 48)),
        (173, (2016, 3, 20), (2016, 3, 20,  4, 37)),
        (174, (2017, 3, 20), (2017, 3, 20, 10, 26)),
        (175, (2018, 3, 21), (2018, 3, 20, 16, 16)),
        (176, (2019, 3, 21), (2019, 3, 20, 22,  5)),
        (177, (2020, 3, 20), (2020, 3, 20,  3, 54)),
        (178, (2021, 3, 20), (2021, 3, 20,  9, 43)),
        (179, (2022, 3, 21), (2022, 3, 20, 15, 32)),
        (180, (2023, 3, 21), (2023, 3, 20, 21, 22)),
        (181, (2024, 3, 20), (2024, 3, 20,  3, 11)),
        (182, (2025, 3, 20), (2025, 3, 20,  9,  0)),
        (183, (2026, 3, 21), (2026, 3, 20, 14, 49)),
        (184, (2027, 3, 21), (2027, 3, 20, 20, 38)),
        (185, (2028, 3, 20), (2028, 3, 20,  2, 28)),
        (186, (2029, 3, 20), (2029, 3, 20,  8, 17)),
        (187, (2030, 3, 20), (2030, 3, 20, 14,  6)),
        (188, (2031, 3, 21), (2031, 3, 20, 19, 55)),
        (189, (2032, 3, 20), (2032, 3, 20,  1, 44)),
        (190, (2033, 3, 20), (2033, 3, 20,  7, 24)),
        (191, (2034, 3, 20), (2034, 3, 20, 13, 23)),
        (192, (2035, 3, 21), (2035, 3, 20, 19, 12)),
        (193, (2036, 3, 20), (2036, 3, 20,  1,  1)),
        (194, (2037, 3, 20), (2037, 3, 20,  6, 50)),
        (195, (2038, 3, 20), (2038, 3, 20, 12, 40)),
        (196, (2039, 3, 21), (2039, 3, 20, 18, 29)),
        (197, (2040, 3, 20), (2040, 3, 20,  0, 18)),
        (198, (2041, 3, 20), (2041, 3, 20,  6,  7)),
        (199, (2042, 3, 20), (2042, 3, 20, 11, 56)),
        (200, (2043, 3, 21), (2043, 3, 20, 17, 46)),
        (201, (2044, 3, 20), (2044, 3, 19, 23, 35)),
        (202, (2045, 3, 20), (2045, 3, 20,  5, 24)),
        (203, (2046, 3, 20), (2046, 3, 20, 11, 13)),
        (204, (2047, 3, 21), (2047, 3, 20, 17,  2)),
        (205, (2048, 3, 20), (2048, 3, 19, 22, 52)),
        (206, (2049, 3, 20), (2049, 3, 20,  4, 41)),
        (207, (2050, 3, 20), (2050, 3, 20, 10, 30)),
        (208, (2051, 3, 21), (2051, 3, 20, 16, 19)),
        (209, (2052, 3, 20), (2052, 3, 19, 22,  8)),
        (210, (2053, 3, 20), (2053, 3, 20,  3, 58)),
        (211, (2054, 3, 20), (2054, 3, 20,  9, 47)),
        (212, (2055, 3, 21), (2055, 3, 20, 15, 36)),
        (213, (2056, 3, 20), (2056, 3, 19, 21, 25)),
        (214, (2057, 3, 20), (2057, 3, 20,  3, 14)),
        (215, (2058, 3, 20), (2058, 3, 20,  9,  4)),
        (216, (2059, 3, 20), (2059, 3, 20, 14, 53)),
        (217, (2060, 3, 20), (2060, 3, 19, 20, 42)),
        (218, (2061, 3, 20), (2061, 3, 20,  2, 31)),
        (219, (2062, 3, 20), (2062, 3, 20,  8, 20)),
        (220, (2063, 3, 20), (2063, 3, 20, 14, 10)),
        (221, (2064, 3, 20), (2064, 3, 19, 19, 59)),
        )

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()
        self.location = self.BAHAI_LOCATION[:3]
        #self.location = (35.681117, 51.4016521, 3.5)

    def dump_sunset_after_ve(self):
        """
        Fine the sunset after the vernal equinox.
        """
        lam = self.SPRING
        data = []

        for year, wc_ss, nasa_ve in self.WC_DATA:
            first_of_march = (wc_ss[0], wc_ss[1], 1) # Replace day with the 1st
            jd = self.gc.jd_from_gregorian_date(first_of_march) # Get UT Julian
            my_ve_jde = self.find_moment_of_equinoxes_or_solstices(jd, zone=3.5)
            my_ve = my_ve_jde
            #my_ve = my_ve_jde - self.delta_t(my_ve_jde)
            my_ss_jd = self._sun_setting(my_ve, *self.location)

            if my_ve > my_ss_jd:
                my_ss = self._sun_setting(my_ve + 1, *self.location)
                my_ss_jd = my_ve + 1 + my_ss

            my_g_ss = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                my_ss_jd))
            my_ve_g = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                my_ve))
            nasa_ve_jd = self.gc.jd_from_gregorian_date(nasa_ve) + self.HR(3.5)
            #print(year, my_ve, nasa_ve_jd, file=sys.stderr)
            data.append((year,                       # Baha'i year
                         wc_ss,                      # WC sunset
                         my_g_ss,                    # My Gregorian sunset
                         round(my_ss_jd-my_ve, 6),   # Sunset difference
                         nasa_ve,                    # NASA VE
                         my_ve_g,                    # My VE
                         round(my_ve-nasa_ve_jd, 6), # VE difference
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
            wc_nr_day = self.gc.jd_from_gregorian_date(wc_naw_ruz)
            # Meeus tables UTC times
            ve_utc_day = self.gc.jd_from_gregorian_date(item[1])
            ve_std_date = self.gc.gregorian_date_from_jd(ve_utc_day)
            rd_ss_day = self._sun_setting(ve_utc_day, *self.BAHAI_LOCATION[:3])
            badi_ss_date = self.gc.gregorian_date_from_jd(rd_ss_day)
            # VE more than 24 hours before sunset.
            hours = True if (rd_ss_day - ve_utc_day) >= 24 else False
            data.append((year, ve_std_date, wc_naw_ruz,
                         #wc_nr_day
                         #ve_utc_day <= wc_nr_day
                         badi_ss_date, hours,
                         #ve_utc_day <= rd_ss_day
                         #wc_nr_day - wc_pre_year
                         #rd_ss_day - ss_pre_year
                         ))
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
    data = []

    if options.ve_ss:
        print("The SS Diff is the difference between the Julian Period days "
              "of the World Centre and my sunset times in Tehran and the\nVE "
              "Diff is the difference of the Julian Period days between "
              "the NASA Vernal Equinox, after converting to Tehran time,\nand "
              "my Vernal Equinox which is already in Tehran time.\n")
        print("Year WC Sunset     My Gregorian Sunset            SS Diff  "
              "NASA's VE (Greenwich) My Vernal Equinox (Tehran)     VE Diff")
        print('-'*121)
        data = [f"{year}  "
                f"{str(wc_ss):<13} "
                f"{str(my_g_ss):<30} "
                f"{ss_diff:<8} "
                f"{str(nasa_ve):<21} "
                f"{str(my_ve):<30} "
                f"{ve_diff:>9.6f}"
                for (year, wc_ss, my_g_ss, ss_diff, nasa_ve, my_ve, ve_diff)
                in cfmes.dump_sunset_after_ve()]
        [print(line) for line in data]
    elif options.days_in_years:
        data = cfmes.number_of_days()
        [print(line) for line in data]
    else:
        parser.print_help()

    if data:
        pass
        #data = cfmes.determine_year_length()
        ## data.append(f"{year}, "
        ##             f"{str(ve_std_date):<29} "
        ##             f"{wc_naw_ruz} "
        ##             #f"{wc_nr_day} "
        ##             #f"{(ve_utc_day <= wc_nr_day):>1} "
        ##             f"{str(badi_ss_date):<29} "
        ##             f"{hours}"
        ##             #f"{(ve_utc_day <= rd_ss_day):>1} "
        ##             #f"{wc_nr_day - wc_pre_year} "
        ##             #f"{rd_ss_day - ss_pre_year}"
        ##             )

        #data = cfmes.find_coefficients()
        #pprint.pprint(data) # Raw data

    sys.exit(0)
