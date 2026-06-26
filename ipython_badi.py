# %load ipython_badi.py
# %load_ext autoreload
import importlib
from badidatetime import set_local_coordinates
set_local_coordinates(35.7796, -78.6382)
from badidatetime import GregorianCalendar, BahaiCalendar
from badidatetime._timedateutils import _td_utils
from zoneinfo import ZoneInfo
import math, time, datetime as dtime
datetime = importlib.import_module('badidatetime.datetime')
import badidatetime
gc = GregorianCalendar()
bc = BahaiCalendar()
