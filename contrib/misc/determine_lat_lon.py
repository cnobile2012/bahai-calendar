#!/usr/bin/env python
#
# contrib/misc/determine_lat_lon.py
#
# This script determines the longitude in Tehran defined by the World Centre
# for the origin of the Badi calendar.
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
(2026, 3, 20, 18, 16) (2026, 3, 20, 18, 16) ?
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
    #   Year  WC Naw-Ruz     NASA Vernal Equinox (GMT)
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
        (183, (2026, 3, 20), (2026, 3, 20, 14, 46)),  # ?
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
        (216, (2059, 3, 19), (2059, 3, 20, 14, 44)),  # ?
        (217, (2060, 3, 19), (2060, 3, 19, 20, 38)),
        (218, (2061, 3, 19), (2061, 3, 20,  2, 26)),
        (219, (2062, 3, 19), (2062, 3, 20,  8,  7)),
        (220, (2063, 3, 19), (2063, 3, 20, 13, 59)),
        (221, (2064, 3, 19), (2064, 3, 19, 19, 39)),
        )
    # The overlap years
    WC_DATA_183_216 = (
        (183, (2026, 3, 20), (2026, 3, 20, 14, 46)),
        (216, (2059, 3, 19), (2059, 3, 20, 14, 44)),
        )

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()
        self.BADI_COORD = self._BAHAI_LOCATION[:3]
        #self.BADI_COORD = (35.78, 51.2344395, 3.5)

    def dump_sunset_after_ve(self, options):
        """
        Find the sunset before the Vernal Equinox or the sunset after if
        it is less than or equal to 1 minute after the Vernal Equinox.

        Use -v
        """
        data = []

        for year, wc_ss, nasa_ve in self.WC_DATA:
            (my_g_ss, my_ss_jd, my_ve_jd, nasa_ve_3p5, my_ve_g,
             nasa_ve_jd) = self._find_data(year, wc_ss, nasa_ve, options)
            data.append((year,                           # Baha'i year
                         wc_ss,                          # WC sunset
                         my_g_ss,                        # My Gregorian sunset
                         round(my_ss_jd-my_ve_jd, 6),    # Sunset difference
                         nasa_ve_3p5,                    # Adjusted NASA VE
                         my_ve_g,                        # My VE
                         round(my_ve_jd-nasa_ve_jd, 6),  # VE difference
                         ))

        return data

    def find_longitude(self, options):
        """
        Find the best longitude for all 50 years.

        -l with -L
        Optional -F, But be ware this will take many hours to complete.
        """
        data = []
        start_lon = 51
        dec_start = 131000  # Low range longitude
        dec_end = 337200    # High range longitude
        year_dict = {}
        wc_data = self.WC_DATA if options.full else self.WC_DATA_183_216

        for year, wc_ss, nasa_ve in wc_data:
            min_max = []
            print(f"Working on year {year}", file=sys.stderr)

            for dp in range(dec_start, dec_end):
                lon = round(start_lon + dp / 1000000, 6)
                if lon > round(51 + dec_end / 1000000, 6): break
                (my_g_ss, my_ss_jd, my_ve_jd, nasa_ve_3p5, my_ve_g,
                 nasa_ve_jd) = self._find_data(year, wc_ss, nasa_ve,
                                               options, test_lon=lon)
                jd_diff = round(my_ss_jd-my_ve_jd, 6)

                if wc_ss == my_g_ss[:3]:
                    min_max.append((lon, jd_diff, wc_ss, my_g_ss[:3]))

            min_l = 100
            max_l = 0

            for lon, diff, wc_ss, my_g_ss in min_max:
                if min_l > lon:
                    min_l = lon
                    diff0 = diff
                    wc_date0 = wc_ss
                    my_date0 = my_g_ss

                if max_l < lon:
                    max_l = lon
                    diff1 = diff
                    wc_date1 = wc_ss
                    my_date1 = my_g_ss

            year_dict[year] = (wc_date0, my_date0, min_l, diff0,
                               wc_date1, my_date1, max_l, diff1)

        for year, (wc_date0, my_date0, min_l, diff0,
                   wc_date1, my_date1, max_l, diff1) in year_dict.items():
            data.append((year, wc_date0, my_date0, min_l, diff0,
                         wc_date1, my_date1, max_l, diff1))

        return data

    # Support methods

    def _find_data(self, year, wc_ss, nasa_ve, options, *, test_lon=None):
        # We do not use my version of the date to JD formula as it
        # will not work with any of Meeus' equations.
        lat, lon, zone = self.BADI_COORD
        lon = test_lon if test_lon and options.longitude else lon
        first_of_march = (wc_ss[0], wc_ss[1], 1)  # Replace day with the 1st
        jd = self.gc.jd_from_gregorian_date(first_of_march)  # Get JP day
        my_ve_jd = self._find_moment_of_equinoxes_or_solstices(jd, zone=3.5)
        my_ss_jd = self._sun_setting(my_ve_jd, lat, lon, zone)

        # It is allowed to have a Vernal Equinox to be up to one minute
        # before sunset and still use that sunset as the beginning of
        # the year. If a day = 1 then 1 minute is 0.0006944444444444444
        if my_ve_jd >= (my_ss_jd - 0.0006944444444444444):
            begin_of_year = my_ss_jd
        else:
            begin_of_year = self._sun_setting(my_ve_jd-1, lat, lon, zone)

        my_g_ss = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
            begin_of_year))
        my_ve_g = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
            my_ve_jd))
        nasa_ve_jd = self.gc.jd_from_gregorian_date(nasa_ve) + self._HR(3.5)
        nasa_ve_3p5 = self.gc.ymdhms_from_date(
            self.gc.gregorian_date_from_jd(nasa_ve_jd))
        #print(year, my_ve, nasa_ve_jd, file=sys.stderr)
        return my_g_ss, my_ss_jd, my_ve_jd, nasa_ve_3p5, my_ve_g, nasa_ve_jd


if __name__ == "__main__":
    import time
    import argparse

    parser = argparse.ArgumentParser(
        description=("Find the latitude and longitude using the "
                     "Vernal Equinox and sunset."))
    parser.add_argument(
        '-v', '--ve-ss', action='store_true', default=False, dest='ve_ss',
        help="Find the Vernal Equinox and sunset.")
    parser.add_argument(
        '-l', '--find-longitude', action='store_true', default=False,
        dest='find_longitude', help="Find the length of each of the 50 years.")
    parser.add_argument(
        '-F', '--full', action='store_true', default=False,
        dest='full', help=("Use the full spread of years from 172 to 221. "
                           "Caution, this will take many hours to complete."))
    parser.add_argument(
        '-L', '--longitude', action='store_true', default=False,
        dest='longitude', help=("If True use generated longitudes, else "
                                "if False used default longitude."))
    options = parser.parse_args()
    cfmes = DumpFindMomentOfEquinoxesOrSolstices()
    ret = 0
    basename = os.path.basename(__file__)

    if options.ve_ss:  # -v
        if options.longitude:
            print("Cannot use option -L (--longitude) with option -v "
                  "(--ve-ss).", file=sys.stderr)
            ret = 1
        else:
            data = cfmes.dump_sunset_after_ve(options)
            print("The SS Diff is the difference between the Julian Period "
                  "days of the World Centre and my sunset times in Tehran and "
                  "the VE Diff\nis the difference between the Julian Period "
                  "days of the USNO Vernal Equinox, and my Vernal Equinox. "
                  "The USNO Vernal Equinox date\nand time was originally in "
                  "UTC time which I converted to Tehran standard time. "
                  "(+03:30)\n")
            print("Year WC Sunset     My Gregorian Sunset            SS Diff  "
                  "USNO's Vernal Equinox (Tehran) My Vernal Equinox (Tehran)  "
                  "    VE Diff")
            print('-'*130)
            [print(f"{year}  "
                   f"{str(wc_ss):<13} "
                   f"{str(my_g_ss):<30} "
                   f"{ss_diff:<8} "
                   f"{str(nasa_ve):<30} "
                   f"{str(my_ve):<30} "
                   f"{ve_diff:>9.6f}"
                   ) for (year, wc_ss, my_g_ss, ss_diff,
                          nasa_ve, my_ve, ve_diff) in data]
            print(f"\n./contrib/misc/{basename} -v")
    elif options.find_longitude:  # -l
        if not options.longitude:
            print("Option -L (--longitude) must be used with option -l "
                  "(--find-longitude).",
                  file=sys.stderr)
            ret = 1
        else:
            start_time = time.time()
            F = 'F' if options.full else ''
            data = cfmes.find_longitude(options)
            print(f"./contrib/misc/{basename} -oL{F}")
            print("Year | WC Date       My Date       Min Lon   JD Diff  | "
                  "WC Date       My Date       Max Lon   JD Diff")
            print("-"*4, "|", "-"*46, "|", "-"*46)
            [print(f"{year:04} | "
                   f"{str(wc_date0):<13} "
                   f"{str(my_date0):<13} "
                   f"{min_l:<9} "
                   f"{diff0:<8} | "
                   f"{str(wc_date1):<13} "
                   f"{str(my_date1):<13} "
                   f"{max_l:<9} "
                   f"{diff1:<8} "
                   ) for (year, wc_date0, my_date0, min_l, diff0,
                          wc_date1, my_date1, max_l, diff1) in data]

            for (year, wc_date0, my_date0, min_l, diff0,
                 wc_date1, my_date1, max_l, diff1) in data:
                if min_l != 51.131:
                    r_min_l = min_l
                elif max_l != 51.3372:
                    r_max_l = max_l

            optimal_lon = (r_max_l - r_min_l) / 2 + r_min_l
            print(f"\nThe optimal longitude is: {optimal_lon}")
            end_time = time.time()
            days, hours, minutes, seconds = cfmes._dhms_from_seconds(
                end_time - start_time)
            print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
                f"{round(seconds, 6):02.6} seconds.")
    else:
        parser.print_help()

    sys.exit(ret)
