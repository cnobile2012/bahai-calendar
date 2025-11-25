#!/usr/bin/env python
"""
Benchmark Badí‘ Calendar equinox JDs against NASA/JPL ephemerides via Skyfield.
"""

import os
import sys
# from datetime import datetime, timezone, timedelta
from skyfield import api, almanac

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime.badi_calendar import BahaiCalendar
from badidatetime.gregorian_calendar import GregorianCalendar
import numpy as np

# --- Setup Skyfield Ephemeris ---
ts = api.load.timescale()
#eph = api.load("de421.bsp")  # or "de440s.bsp" for higher precision
eph = api.load("de406.bsp")
bc = BahaiCalendar()
gc = GregorianCalendar()
GMT_COORD = (51.477928, -0.001545, 0)
TEHRAN_COORD = (35.682376, 51.285817, 3.5)


def jpl_vernal_equinox_jd(year):
    """
    Compute the JD of the vernal equinox (geocentric apparent Sun)
    using the JPL DE ephemeris (TT-based JD).
    """
    t0 = ts.utc(year, 3, 1)
    t1 = ts.utc(year, 4, 1)
    t, y = almanac.find_discrete(t0, t1, almanac.seasons(eph))
    # Get the March (vernal) equinox
    # Almanac.seasons returns: 0=Mar equinox, 1=Jun solstice, 2=Sep equinox,
    # 3=Dec solstice
    # Subtract timezone offset (hours → days)
    # UTC datetime
    # equinox_utc = equinox_t.utc_datetime().replace(tzinfo=timezone.utc)
    return t[0].tt  # , equinox_utc


def badidatetime_equinox_jd(year, coords=GMT_COORD):
    """
    Compute your package’s vernal equinox JD (or Naw-Rúz anchor)
    using your coefficient-based algorithm.
    """
    b_date = bc.badi_date_from_gregorian_date((year, 3, 25), *coords,
                                              short=True, trim=True)
    jd = bc.jd_from_badi_date(b_date, *coords)
    return bc._find_moment_of_equinoxes_or_solstices(jd, zone=coords[-1])


# --- Compare across a range of years ---
results = []

for year in range(1, 3001, 10):
    jd_jpl = jpl_vernal_equinox_jd(year)
    jd_badi = badidatetime_equinox_jd(year, GMT_COORD)
    diff_days = jd_badi - jd_jpl
    diff_sec = diff_days * 86400
    results.append((year, jd_jpl, jd_badi, diff_sec))


# --- Output summary ---
print(f"{'Year':>6} {'JPL JD':>14} {'Badí‘ JD':>14} {'Δt (s)':>10}")
print("-" * 48)

for year, jd_jpl, jd_badi, diff_sec in results:
    print(f"{year:6d} {jd_jpl:14.6f} {jd_badi:14.6f} {diff_sec:10.3f}")

diffs = np.array([abs(r[3]) for r in results])
print("\nAverage absolute diff (seconds):", diffs.mean())
print("Max absolute diff (seconds):", diffs.max())
