# %load ipython_badi.py
# %load_ext autoreload
import importlib
from badidatetime import enable_geocoder; enable_geocoder()
from badidatetime import GregorianCalendar, BahaiCalendar
from badidatetime._timedateutils import _td_utils
from zoneinfo import ZoneInfo
import math, time, datetime as dtime
datetime = importlib.import_module('badidatetime.datetime')
gc = GregorianCalendar()
bc = BahaiCalendar()
