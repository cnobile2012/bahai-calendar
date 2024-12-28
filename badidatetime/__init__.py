# -*- coding: utf-8 -*-
#
# badidatetime/__init__.py
#
__docformat__ = "restructuredtext en"

import os
from badidatetime.badi_calendar import BahaiCalendar
from badidatetime.gregorian_calendar import GregorianCalendar

if os.getenv('DEBUG', False):
    from badidatetime import datetime
else:
    from badidatetime.datetime import *

    __all__ = ('date', 'datetime', 'time', 'timedelta', 'timezone', 'tzinfo',
               'MINYEAR', 'MAXYEAR', 'BADI_IANA', 'BADI_COORD', 'GMT_COORD',
               'LOCAL_COORD', 'UTC', 'BADI', 'LOCAL', 'BahaiCalendar',
               'GregorianCalendar',)
