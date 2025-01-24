# %load ipython_badi.py
# %load_ext autoreload
import importlib
from badidatetime import enable_geocoder; enable_geocoder()
datetime = importlib.import_module('badidatetime.datetime')
from badidatetime import GregorianCalendar, BahaiCalendar
from badidatetime._timedateutils import TimeDateUtils
from zoneinfo import ZoneInfo
import math, time, datetime as dtime
gc = GregorianCalendar()
bc = BahaiCalendar()
_td_utils = TimeDateUtils()
