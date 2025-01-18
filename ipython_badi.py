# %load ipython_badi.py
# %load_ext autoreload
import os; os.environ['DEBUG'] = "1"
from badidatetime import GregorianCalendar, BahaiCalendar, datetime, enable_geocoder
from badidatetime._timedateutils import TimeDateUtils
from zoneinfo import ZoneInfo
import math, time, datetime as dtime
gc = GregorianCalendar()
bc = BahaiCalendar()
_td_utils = TimeDateUtils()
