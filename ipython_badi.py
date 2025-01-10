# %load ipython_badi.py
# %load_ext autoreload
from badidatetime import GregorianCalendar, BahaiCalendar, datetime, date, time, timezone, timedelta, tzinfo
from badidatetime._timedateutils import _td_utils
from zoneinfo import ZoneInfo
import math, time, datetime as dtime
gc = GregorianCalendar()
bc = BahaiCalendar()
