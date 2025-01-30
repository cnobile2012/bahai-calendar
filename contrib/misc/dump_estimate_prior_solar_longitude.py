#!/usr/bin/env python

import os
import sys
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar


class DumpEstimatePriorSolarLongitude(BahaiCalendar):

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()

    def dump(self):
        lam = self._SPRING
        data = []
        month = 3
        current_year = 0

        for year in range(1788, 2212):
            for day in range(19, 26):
                date = (year, month, day)
                jd = self.gc.jd_from_gregorian_date(date)
                est = self._find_moment_of_equinoxes_or_solstices(jd, lam)
                #est = self._estimate_prior_solar_longitude(lam, tee)
                moment = self.gc.gregorian_date_from_jd(est)
                if year != current_year: data.append("")
                data.append(f"{str(date):<13} {jd:<6} {est:<17} "
                            f"{str(moment):<13}")
                current_year = year

            current_year = year

        return data


if __name__ == "__main__":
    cepsl = DumpEstimatePriorSolarLongitude()
    data = cepsl.dump()
    pprint.pprint(data)
