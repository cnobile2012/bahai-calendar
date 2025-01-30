#!/usr/bin/env python
#
# contrib/misc/determine_lat_lon.py
#

import os
import sys

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar

"""
These are the sunsets which are for the most part before the Vernal Equinox.
All dates are corrected for Tehran Iran.
Max longitude (2026): 51.5036157
Max Longitude (2059): 51.285817
Median Longitude    : 51.285817 USED
Min longitude (2026): 51.2540465
Min Longitude (2059): 51.0360365
The latitude makes little difference so we use: 35.682376

Hour calculator:
https://www.calculatorsoup.com/calculators/time/time-calculator.php
Gregorian Date of     Gregorian Date of
Vernal Equinox        Sunset of Epoch

(2015, 3, 21, 2, 15)  (2015, 3, 20, 18, 16)
(2016, 3, 20, 8, 00)  (2016, 3, 19, 18, 15)
(2017, 3, 20, 13, 59) (2017, 3, 19, 18, 15)
(2018, 3, 20, 19, 45) (2018, 3, 20, 18, 16)
(2019, 3, 21, 1, 28)  (2019, 3, 20, 18, 16)
(2020, 3, 20, 7, 20)  (2020, 3, 19, 18, 16)
(2021, 3, 20, 13, 7)  (2021, 3, 19, 18, 15)
(2022, 3, 20, 19, 3)  (2022, 3, 20, 18, 16)
(2023, 3, 21, 0, 54)  (2023, 3, 20, 18, 16)
(2024, 3, 20, 6, 36)  (2024, 3, 19, 18, 15)
(2025, 3, 20, 12, 31) (2025, 3, 19, 18, 15)
(2026, 3, 20, 18, 16) (2026, 3, 20, 18, 16)
(2027, 3, 20, 23, 55) (2027, 3, 20, 18, 16)
(2028, 3, 20, 5, 47)  (2028, 3, 19, 18, 16)
(2029, 3, 20, 11, 32) (2029, 3, 19, 18, 15)
(2030, 3, 20, 17, 22) (2030, 3, 19, 18, 15)
(2031, 3, 20, 23, 11) (2031, 3, 20, 18, 16)
(2032, 3, 20, 4, 52)  (2032, 3, 19, 18, 16)
(2033, 3, 20, 10, 52) (2033, 3, 19, 18, 15)
(2034, 3, 20, 16, 47) (2034, 3, 19, 18, 15)
(2035, 3, 20, 22, 32) (2035, 3, 20, 18, 16)
(2036, 3, 20, 4, 33)  (2036, 3, 19, 18, 16)
(2037, 3, 20, 10, 20) (2037, 3, 19, 18, 15)
(2038, 3, 20, 16, 10) (2038, 3, 19, 18, 15)
(2039, 3, 20, 22, 2)  (2039, 3, 20, 18, 16)
(2040, 3, 20, 3, 41)  (2040, 3, 19, 18, 16)
(2041, 3, 20, 9, 37)  (2041, 3, 19, 18, 15)
(2042, 3, 20, 15, 23) (2042, 3, 19, 18, 15)
(2043, 3, 20, 20, 57) (2043, 3, 20, 18, 16)
(2044, 3, 20, 2, 50)  (2044, 3, 19, 18, 16)
(2045, 3, 20, 8, 37)  (2045, 3, 19, 18, 15)
(2046, 3, 20, 14, 28) (2046, 3, 19, 18, 15)
(2047, 3, 20, 20, 22) (2047, 3, 20, 18, 16)
(2048, 3, 20, 2, 4)   (2048, 3, 19, 18, 16)
(2049, 3, 20, 7, 58)  (2049, 3, 19, 18, 15)
(2050, 3, 20, 13, 49) (2050, 3, 19, 18, 15)
(2051, 3, 20, 19, 29) (2051, 3, 20, 18, 16)
(2052, 3, 20, 1, 26)  (2052, 3, 19, 18, 16)
(2053, 3, 20, 7, 17)  (2053, 3, 19, 18, 15)
(2054, 3, 20, 13, 4)  (2054, 3, 19, 18, 15)
(2055, 3, 20, 18, 58) (2055, 3, 20, 18, 16)
(2056, 3, 20, 0, 41)  (2056, 3, 19, 18, 16)
(2057, 3, 20, 6, 38)  (2057, 3, 19, 18, 15)
(2058, 3, 20, 12, 35) (2058, 3, 19, 18, 15)
(2059, 3, 20, 18, 14) (2059, 3, 19, 18, 15) ?
(2060, 3, 20, 0, 8)   (2060, 3, 19, 18, 16)
(2061, 3, 20, 5, 56)  (2061, 3, 19, 18, 15)
(2062, 3, 20, 11, 37) (2062, 3, 19, 18, 15)
(2063, 3, 20, 17, 29) (2063, 3, 19, 18, 15)
(2064, 3, 19, 23, 9)  (2064, 3, 19, 18, 16)
"""


class DumpFindMomentOfEquinoxesOrSolstices(BahaiCalendar):
    #    Year WC Naw-Ruz     Vernal Equinox (GMT)
    WC_DATA = (
        (172, (2015, 3, 20), (2015, 3, 20, 22, 45)),
        (173, (2016, 3, 19), (2016, 3, 20,  4, 30)),
        (174, (2017, 3, 19), (2017, 3, 20, 10, 29)),
        (175, (2018, 3, 20), (2018, 3, 20, 16, 15)),
        (176, (2019, 3, 20), (2019, 3, 20, 21, 58)),
        (177, (2020, 3, 19), (2020, 3, 20,  3, 50)),
        (178, (2021, 3, 19), (2021, 3, 20,  9, 37)),
        (179, (2022, 3, 20), (2022, 3, 20, 15, 33)),
        (180, (2023, 3, 20), (2023, 3, 20, 21, 24)),
        (181, (2024, 3, 19), (2024, 3, 20,  3,  6)),
        (182, (2025, 3, 19), (2025, 3, 20,  9,  1)),
        (183, (2026, 3, 20), (2026, 3, 20, 14, 46)),
        (184, (2027, 3, 20), (2027, 3, 20, 20, 25)),
        (185, (2028, 3, 19), (2028, 3, 20,  2, 17)),
        (186, (2029, 3, 19), (2029, 3, 20,  8,  2)),
        (187, (2030, 3, 19), (2030, 3, 20, 13, 52)),
        (188, (2031, 3, 20), (2031, 3, 20, 19, 41)),
        (189, (2032, 3, 19), (2032, 3, 20,  1, 22)),
        (190, (2033, 3, 19), (2033, 3, 20,  7, 22)),
        (191, (2034, 3, 19), (2034, 3, 20, 13, 17)),
        (192, (2035, 3, 20), (2035, 3, 20, 19,  2)),
        (193, (2036, 3, 19), (2036, 3, 20,  1,  3)),
        (194, (2037, 3, 19), (2037, 3, 20,  6, 50)),
        (195, (2038, 3, 19), (2038, 3, 20, 12, 40)),
        (196, (2039, 3, 20), (2039, 3, 20, 18, 32)),
        (197, (2040, 3, 19), (2040, 3, 20,  0, 11)),
        (198, (2041, 3, 19), (2041, 3, 20,  6,  7)),
        (199, (2042, 3, 19), (2042, 3, 20, 11, 53)),
        (200, (2043, 3, 20), (2043, 3, 20, 17, 27)),
        (201, (2044, 3, 19), (2044, 3, 19, 23, 20)),
        (202, (2045, 3, 19), (2045, 3, 20,  5,  7)),
        (203, (2046, 3, 19), (2046, 3, 20, 10, 58)),
        (204, (2047, 3, 20), (2047, 3, 20, 16, 52)),
        (205, (2048, 3, 19), (2048, 3, 19, 22, 34)),
        (206, (2049, 3, 19), (2049, 3, 20,  4, 28)),
        (207, (2050, 3, 19), (2050, 3, 20, 10, 19)),
        (208, (2051, 3, 20), (2051, 3, 20, 15, 59)),
        (209, (2052, 3, 19), (2052, 3, 19, 21, 56)),
        (210, (2053, 3, 19), (2053, 3, 20,  3, 47)),
        (211, (2054, 3, 19), (2054, 3, 20,  9, 34)),
        (212, (2055, 3, 20), (2055, 3, 20, 15, 28)),
        (213, (2056, 3, 19), (2056, 3, 19, 21, 11)),
        (214, (2057, 3, 19), (2057, 3, 20,  3,  8)),
        (215, (2058, 3, 19), (2058, 3, 20,  9,  5)),
        (216, (2059, 3, 19), (2059, 3, 20, 14, 44)),
        (217, (2060, 3, 19), (2060, 3, 19, 20, 38)),
        (218, (2061, 3, 19), (2061, 3, 20,  2, 26)),
        (219, (2062, 3, 19), (2062, 3, 20,  8,  7)),
        (220, (2063, 3, 19), (2063, 3, 20, 13, 59)),
        (221, (2064, 3, 19), (2064, 3, 19, 19, 39)),
        )

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()
        self.location = self._BAHAI_LOCATION[:3]
        #self.location = (35.682376, 51.285817, 3.5)

    def dump_sunset_after_ve(self):
        """
        Find the sunset before the Vernal Equinox or the sunset after if
        it is less than or equal to 1 minute after the Vernal Equinox.

        Use -v
        """
        data = []

        for year, wc_ss, nasa_ve in self.WC_DATA:
            # We do not use my version of the date to JD formula as it
            # will not work with any of Meeus' equations.
            first_of_march = (wc_ss[0], wc_ss[1], 1)  # Replace day with the 1st
            jd = self.gc.jd_from_gregorian_date(first_of_march)  # Get Julian
            my_ve_jd = self.find_moment_of_equinoxes_or_solstices(jd, zone=3.5)
            my_ss_jd = self._sun_setting(my_ve_jd, *self.location)

            # It is allowed to have a Vernal Equinox to be up to one minute
            # before sunset and still use that sunset as the beginning of
            # the year. If a day = 1 then 1 minute is 0.0006944444444444444
            if my_ve_jd >= (my_ss_jd - 0.0006944444444444444):
                begin_of_year = my_ss_jd
            else:
                begin_of_year = self._sun_setting(my_ve_jd - 1, *self.location)

            my_g_ss = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                begin_of_year))
            my_ve_g = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                my_ve_jd))
            nasa_ve_jd = self.gc.jd_from_gregorian_date(nasa_ve) + self.HR(3.5)
            nasa_ve_3p5 = self.gc.ymdhms_from_date(
                self.gc.gregorian_date_from_jd(nasa_ve_jd))
            #print(year, my_ve, nasa_ve_jd, file=sys.stderr)
            data.append((year,                           # Baha'i year
                         wc_ss,                          # WC sunset
                         my_g_ss,                        # My Gregorian sunset
                         round(my_ss_jd-my_ve_jd, 6),    # Sunset difference
                         nasa_ve_3p5,                    # NASA VE
                         my_ve_g,                        # My VE
                         round(my_ve_jd-nasa_ve_jd, 6),  # VE difference
                         ))

        return data

    def number_of_days(self):
        """
        Find the number of days in a Badi year.

        Use -d
        """
        for year, item in self.WC_DATA.items():
            wc_naw_ruz, vernal_equinox = item

    def determine_year_length(self):
        data = []
        #wc_pre_year = 0
        #ss_pre_year = 0
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
            #wc_nr_day = self.gc.jd_from_gregorian_date(wc_naw_ruz)
            # Meeus tables UTC times
            ve_utc_day = self.gc.jd_from_gregorian_date(item[1])
            ve_std_date = self.gc.gregorian_date_from_jd(ve_utc_day)
            rd_ss_day = self._sun_setting(ve_utc_day, *self._BAHAI_LOCATION[:3])
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
            #wc_pre_year = wc_nr_day
            #ss_pre_year = rd_ss_day

        return data

    def find_coefficients(self):
        data = []
        #g_lte = 1.962
        g_gt = 0.038
        d0 = 365056.574  # or 365056.575
        n0 = 730120      # 730113

        for c in range(n0-20, 730151):
            #rd_lte = 716241 / d0
            rd_gt = (716241 - c) / d0
            #data.append(f"{d0:<010}, {d}, {g_lte}, {rd_lte}")
            data.append(f"{c}, {g_gt}, {rd_gt}")

        return data


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Find the latitude and longitude using the "
                     "Vernal Equinox and sunset."))
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
              "of the World Centre and my sunset times in Tehran and the "
              "VE Diff\nis the difference between the Julian Period days of "
              "the USNO Vernal Equinox, and my Vernal Equinox. The USNO "
              "Vernal Equinox date\nand time was originally in UTC time "
              "which I converted to Tehran standard time. (+03:30)\n")
        print("Year WC Sunset     My Gregorian Sunset            SS Diff  "
              "USNO's Vernal Equinox (Tehran) My Vernal Equinox (Tehran)     "
              "VE Diff")
        print('-'*130)
        data = [f"{year}  "
                f"{str(wc_ss):<13} "
                f"{str(my_g_ss):<30} "
                f"{ss_diff:<8} "
                f"{str(nasa_ve):<30} "
                f"{str(my_ve):<30} "
                f"{ve_diff:>9.6f}"
                for (year, wc_ss, my_g_ss, ss_diff,
                     nasa_ve, my_ve, ve_diff) in cfmes.dump_sunset_after_ve()]
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
