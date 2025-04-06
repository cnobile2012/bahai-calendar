# -*- coding: utf-8 -*-
#
# badidatetime/_strptime.py
#
__docformat__ = "restructuredtext en"

"""
Strptime-related classes and functions.

CLASSES:
    LocaleTime -- Discovers and stores locale-specific time information
    TimeRE -- Creates regexes for pattern matching a string of text containing
                time information
    StrpTime -- Calculates the time struct represented by the passed-in string

FUNCTIONS:
    _getlang -- Figure out what language is being used for the locale
"""

import time
import locale
from re import compile as re_compile
from re import IGNORECASE
from re import escape as re_escape
from _thread import allocate_lock as _thread_allocate_lock

from .datetime import (date as datetime_date,
                       timedelta as datetime_timedelta,
                       timezone as datetime_timezone)
from ._timedateutils import _td_utils
from .badi_calendar import BahaiCalendar


__all__ = []


def _getlang():
    # Figure out what the current language is set to.
    return locale.getlocale(locale.LC_TIME)


class LocaleTime:
    """
    Stores and handles locale-specific information related to time.

    ATTRIBUTES:
        f_weekday -- full weekday names (7-item list)
        a_weekday -- abbreviated weekday names (7-item list)
        f_month -- full month names (13-item list; dummy value in [0], which
                    is added by code)
        a_month -- abbreviated month names (13-item list, dummy value in
                    [0], which is added by code)
        am_pm -- AM/PM representation (2-item list)
        LC_date_time -- format string for date/time representation (string)
        LC_date -- format string for date representation (string)
        LC_time -- format string for time representation (string)
        timezone -- daylight- and non-daylight-savings timezone representation
                    (2-item list of sets)
        lang -- Language used by instance (2-item tuple)
    """

    def __init__(self):
        """
        Set all attributes.

        Order of methods called matters for dependency reasons.

        The locale language is set at the offset and then checked again before
        exiting.  This is to make sure that the attributes were not set with a
        mix of information from more than one locale.  This would most likely
        happen when using threads where one thread calls a locale-dependent
        function while another thread changes the locale while the function in
        the other thread is still running.  Proper coding would call for
        locks to prevent changing the locale while locale-dependent code is
        running.  The check here is done in case someone does not think about
        doing this.

        Only other possible issue is if someone changed the timezone and did
        not call tz.tzset .  That is an issue for the programmer, though,
        since changing the timezone is worthless without that call.
        """
        self.lang = _getlang()
        self.__calc_weekday()
        self.__calc_month()
        self.__calc_am_pm()
        self.__calc_timezone()
        self.__calc_date_time()

        if _getlang() != self.lang:  # pragma: no cover
            raise ValueError("locale changed during initialization")

        if (time.tzname != self.tzname or
            time.daylight != self.daylight):  # pragma: no cover
            raise ValueError("timezone changed during initialization")

    def __calc_weekday(self):
        """
        Set self.a_weekday and self.f_weekday using the _timedateutils module.
        """
        self.a_weekday = [_td_utils.DAYNAMES_ABV[i].lower() for i in range(7)]
        self.f_weekday = [_td_utils.DAYNAMES[i].lower() for i in range(7)]

    def __calc_month(self):
        """
        Set self.f_month and self.a_month using the _timedateutils module.
        """
        month_names = [v for k, v in sorted(_td_utils.MONTHNAMES_ABV.items(),
                                            key=lambda k: k)]
        self.a_month = [month_names[i].lower() for i in range(20)]
        month_names = [v for k, v in sorted(_td_utils.MONTHNAMES.items(),
                                            key=lambda k: k)]
        self.f_month = [month_names[i].lower() for i in range(20)]

    def __calc_am_pm(self):
        """
        Set self.am_pm by using time.strftime().

        The magic date (1, 1 , 1, hour, 0, 0, 0, 0, 0) is not really that
        magical, I just needed a time shortly after sunset and a time shortly
        before the next sunset.
        """
        am_pm = []

        for hour in (1, 22):
            # The short form Badí' date is used.
            time_tuple = _td_utils._build_struct_time(
                (199, 1, 1, hour, 44, 55), 0, short_in=True)
            am_pm.append(_td_utils.strftime("%p", time_tuple).lower())

        self.am_pm = am_pm

    def __calc_date_time(self):
        # Set self.date_time, self.date, & self.time by using
        # time.strftime().

        # Use (199, 3, 17, 22, 44, 55, 4, 1, 0) for magic date because the
        # amount of overloaded numbers is minimized. The order in which
        # searches for values within the format string is very important;
        # it eliminates possible ambiguity for what something represents.
        time_tuple = _td_utils._build_struct_time((199, 3, 17, 22, 4, 30), 0,
                                                  short_in=True)
        date_time = [None, None, None]
        date_time[0] = _td_utils.strftime("%c", time_tuple).lower()
        date_time[1] = _td_utils.strftime("%x", time_tuple).lower()
        date_time[2] = _td_utils.strftime("%X", time_tuple).lower()
        # date_time = ['kam jam  17 22:44:30 0199', '03/17/0199', '10:44:30']
        replacement_pairs = [('%', '%%'), (self.f_weekday[2], '%A'),
                             (self.f_month[3], '%B'), (self.a_weekday[2], '%a'),
                             (self.a_month[3], '%b'), (self.am_pm[1], '%p'),
                             ('0199', '%Y'), ('99', '%y'), ('22', '%H'),
                             ('04', '%M'), ('4', '%-M'), ('30', '%S'),
                             ('55', '%j'), ('17', '%d'), ('03', '%m'),
                             ('3', '%-m'), ('2', '%w'), ('10', '%I')]
        replacement_pairs.extend([(tz, "%Z") for tz_values in self.timezone
                                  for tz in tz_values])

        for offset, directive in ((0, '%c'), (1, '%x'), (2, '%X')):
            current_format = date_time[offset]

            for old, new in replacement_pairs:
                # Must deal with possible lack of locale info
                # manifesting itself as the empty string (e.g., Swedish's
                # lack of AM/PM info) or a platform returning a tuple of empty
                # strings (e.g., MacOS 9 having timezone as ('','')).
                if old:
                    current_format = current_format.replace(old, new)

            time_tuple = _td_utils._build_struct_time((199, 1, 1, 1, 1, 1), 0,
                                                      short_in=True)
            date_time[offset] = current_format.replace('11', '%U')

        self.LC_date_time = date_time[0]
        self.LC_date = date_time[1]
        self.LC_time = date_time[2]

    def __calc_timezone(self):
        # Set self.timezone by using time.tzname.
        # Do not worry about possibility of time.tzname[0] == time.tzname[1]
        # and time.daylight; handle that in strptime.
        try:
            time.tzset()
        except AttributeError:  # pragma: no cover
            pass

        self.tzname = time.tzname
        self.daylight = time.daylight
        no_saving = frozenset({"utc", "gmt", self.tzname[0].lower()})

        if self.daylight:
            has_saving = frozenset({self.tzname[1].lower()})
        else:  # pragma: no cover
            has_saving = frozenset()

        self.timezone = (no_saving, has_saving)


class TimeRE(dict):
    """
    Handle conversion from format directives to regexes.
    """

    def __init__(self, locale_time=None):
        """
        Create keys/values.

        Order of execution is important for dependency reasons.
        """
        if locale_time:  # pragma: no cover
            self.locale_time = locale_time
        else:
            self.locale_time = LocaleTime()

        base = super()
        base.__init__({
            # The " [1-9]" part of the regex is to make %c from ANSI C work
            'd': r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])",
            'f': r"(?P<f>[0-9]{1,6})",
            'H': r"(?P<H>2[0-3]|[0-1]\d|\d)",
            'I': r"(?P<I>1[0-2]|0[1-9]|[1-9])",
            'G': r"(?P<G>-?\d\d\d\d)",
            'j': (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]"
                  r"\d|0[1-9]|[1-9])"),
            'm': r"(?P<m>1[0-9]|0[0-9]|[0-9])",
            'M': r"(?P<M>[0-5]\d|\d)",
            'S': r"(?P<S>6[0-1]|[0-5]\d|\d)",
            'U': r"(?P<U>5[0-2]|[0-4]\d|\d)",
            'w': r"(?P<w>[0-6])",
            'u': r"(?P<u>[1-7])",
            'V': r"(?P<V>5[0-3]|0[1-9]|[1-4]\d|\d)",
            # W is set below by using 'U'
            'y': r"(?P<y>\d\d)",
            'Y': r"(?P<Y>-?\d{1,4})",  # We alow negative years.
            'z': r"(?P<z>[+-]\d\d:?[0-5]\d(:?[0-5]\d(\.\d{1,6})?)?|(?-i:Z))",
            'A': self.__seqToRE(self.locale_time.f_weekday, 'A'),
            'a': self.__seqToRE(self.locale_time.a_weekday, 'a'),
            'B': self.__seqToRE(self.locale_time.f_month, 'B'),
            'b': self.__seqToRE(self.locale_time.a_month, 'b'),
            'p': self.__seqToRE(self.locale_time.am_pm, 'p'),
            'Z': self.__seqToRE((tz for tz_names in self.locale_time.timezone
                                 for tz in tz_names), 'Z'),
            '%': '%'})
        base.__setitem__('W', base.__getitem__('U').replace('U', 'W'))
        base.__setitem__('c', self.pattern(self.locale_time.LC_date_time))
        base.__setitem__('x', self.pattern(self.locale_time.LC_date))
        base.__setitem__('X', self.pattern(self.locale_time.LC_time))

    def __seqToRE(self, to_convert, directive):
        """
        Convert a list to a regex string for matching a directive.

        We want possible matching values to be from longest to shortest. This
        prevents the possibility of a match occurring for a value that also
        a substring of a larger value that should have matched (e.g., 'abc'
        matching when 'abcdef' should have been the match).
        """
        to_convert = sorted(to_convert, key=len, reverse=True)

        for value in to_convert:
            if value != '':
                break
        else:
            return ''

        regex = '|'.join(re_escape(stuff) for stuff in to_convert)
        regex = f'(?P<{directive}>{regex}'
        return f'{regex})'

    def pattern(self, format):
        """
        Return regex pattern for the format string.

        Need to make sure that any characters that might be interpreted as
        regex syntax are escaped.
        """
        processed_format = ''
        # The sub() call escapes all characters that might be misconstrued
        # as regex syntax.  Cannot use re.escape since we have to deal with
        # format directives (%m, etc.).
        regex_chars = re_compile(r"([\\.^$*+?\(\){}\[\]|])")
        format = regex_chars.sub(r"\\\1", format)
        whitespace_replacement = re_compile(r'\s+')
        format = whitespace_replacement.sub(r'\\s+', format)

        while '%' in format:
            directive_index = format.index('%') + 1
            processed_format = (f"{processed_format}"
                                f"{format[:directive_index - 1]}"
                                f"{self[format[directive_index]]}")
            format = format[directive_index + 1:]

        return f"{processed_format}{format}"

    def compile(self, format):
        """
        Return a compiled re object for the format string.
        """
        return re_compile(self.pattern(format), IGNORECASE)


_cache_lock = _thread_allocate_lock()
# DO NOT modify _TimeRE_cache or _regex_cache without acquiring the cache lock
# first!
_TimeRE_cache = TimeRE()
_CACHE_MAX_SIZE = 5  # Max number of regexes stored in _regex_cache
_regex_cache = {}


def _calc_julian_from_U_or_W(year, week_of_year, day_of_week):
    """
    Calculate the Julian day based on the year, week of the year, and day of
    the week, with week_start_day representing whether the week of the year
    assumes the week starts on Bahá (Saturday) 0. This means that both %U and
    %W would give the same results.

    The `week_of_year` is NOT based on the ISO standard. If it's the 1st day
    of the year then it is always week 0.
    """
    first_weekday = datetime_date(year, 1, 1).weekday()
    # Need to watch out for a week 0 (when the first day of the year is not
    # the same as that specified by %U or %W).
    week_0_length = (7 - first_weekday) % 7

    if week_of_year == 0:
        if day_of_week < first_weekday:
            ret = 7 - first_weekday + day_of_week + 1
        else:
            ret = 1 + day_of_week - first_weekday
    else:
        days_to_week = week_0_length + (7 * (week_of_year - 1))
        ret = 1 + days_to_week + day_of_week

    return ret


class DotDict(dict):
    """
    Allows dot notation access to dictionary attributes.
    """
    def __init__(self, dictionary):
        super().__init__(dictionary)

        for key, value in dictionary.items():
            self[key] = DotDict(value) if isinstance(value, dict) else value

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            raise AttributeError(f"No such attribute: {key}")

    def __setattr__(self, key, value):
        self[key] = value


class StrpTime:
    """
    Return a 2-tuple consisting of a time struct and an int containing
    the number of microseconds based on the input string and the
    format string.
    """

    def __init__(self, data_string, format="%a %b %d %H:%M:%S %Y"):
        for index, arg in enumerate([data_string, format]):
            if not isinstance(arg, str):
                msg = "strptime() argument {} must be str, not {}."
                raise TypeError(msg.format(index, type(arg)))

        self.data_string = data_string
        self.format = format

    def start(self):
        found, locale_time = self._find_regex()
        dot_dict = self._parse_found_dict(found, locale_time)
        self._check_iso_week(dot_dict)
        self._miscellaneous(dot_dict)
        # Always returns a short form Badi date--for now.
        return ((dot_dict.year, dot_dict.month, dot_dict.day,
                 dot_dict.hour, dot_dict.minute, dot_dict.second,
                 dot_dict.weekday, dot_dict.julian, dot_dict.tz,
                 dot_dict.tzname, dot_dict.gmtoff),
                dot_dict.fraction, dot_dict.gmtoff_fraction)

    def _clear_cache(self):
        global _regex_cache
        _regex_cache.clear()

    def _find_regex(self):
        global _TimeRE_cache, _regex_cache

        with _cache_lock:
            locale_time = _TimeRE_cache.locale_time

            if (_getlang() != locale_time.lang or
                time.tzname != locale_time.tzname or
                time.daylight != locale_time.daylight):  # pragma: no cover
                _TimeRE_cache = TimeRE()
                self._clear_cache()
                locale_time = _TimeRE_cache.locale_time

            if len(_regex_cache) > _CACHE_MAX_SIZE:  # pragma: no cover
                self._clear_cache()

            format_regex = _regex_cache.get(self.format)

            if not format_regex:
                try:
                    format_regex = _TimeRE_cache.compile(self.format)
                # KeyError raised when a bad format is found; can be specified
                # as \\, in which case it was a stray % but with a space after
                # it.
                except KeyError as err:
                    bad_directive = err.args[0]

                    if bad_directive == "\\":
                        bad_directive = "%"

                    del err
                    raise ValueError(f"'{bad_directive}' is a bad directive "
                                     f"in format '{format}'") from None
                # IndexError only occurs when the format string is "%"
                except IndexError:
                    raise ValueError(f"stray %% in format '{self.format}'"
                                     ) from None

                _regex_cache[format] = format_regex

        found = format_regex.match(self.data_string)

        if not found:
            raise ValueError(f"Time data {self.data_string!r} does not match "
                             f"format {self.format!r}.")

        if len(self.data_string) != found.end():
            raise ValueError("Unconverted data remains: "
                             f"{self.data_string[found.end():]}")

        return found, locale_time

    def _parse_found_dict(self, found, locale_time):
        values = {'iso_year': None, 'year': None, 'month': 1, 'day': 1,
                  'hour': 0, 'minute': 0, 'second': 0, 'fraction': 0, 'tz': -1,
                  'gmtoff': None, 'gmtoff_fraction': 0, 'iso_week': None,
                  'week_of_year': None, 'weekday': None, 'julian': None}
        # Weekday and julian defaulted to None so as to signal need to
        # calculate values.
        dot_dict = DotDict(values)
        found_dict = found.groupdict()

        for group_key in found_dict.keys():
            # Directives not explicitly handled below:
            #   c, x, X
            #      handled by making out of other directives
            #   U, W
            #      worthless without day of the week
            if group_key == 'y':
                # With the year without a century an accurate year cannot
                # be derived, since the min and max years supported is from
                # -1842 to 1161.
                dot_dict.year = int(found_dict['y'])
            elif group_key == 'Y':
                dot_dict.year = int(found_dict['Y'])
            elif group_key == 'G':
                dot_dict.iso_year = int(found_dict['G'])
            elif group_key == 'm':
                dot_dict.month = int(found_dict['m'])
            elif group_key == 'B':
                dot_dict.month = locale_time.f_month.index(
                    found_dict['B'].lower())
            elif group_key == 'b':
                dot_dict.month = locale_time.a_month.index(
                    found_dict['b'].lower())
            elif group_key == 'd':
                dot_dict.day = int(found_dict['d'])
            elif group_key == 'H':
                dot_dict.hour = int(found_dict['H'])
            elif group_key == 'I':
                dot_dict.hour = int(found_dict['I'])
                ampm = found_dict.get('p', '').lower()

                # If there was no AM/PM indicator, we'll treat this like AM.
                if ampm in ('', locale_time.am_pm[0]):
                    # We're in AM so the hour is correct unless we're
                    # looking at 12 midnight.
                    # 12 midnight == 12 AM == hour 0
                    if dot_dict.hour == 12:
                        dot_dict.hour = 0
                elif ampm == locale_time.am_pm[1]:
                    # We're in PM so we need to add 12 to the hour unless
                    # we're looking at 12 noon.
                    # 12 noon == 12 PM == hour 12
                    if dot_dict.hour != 12:
                        dot_dict.hour += 12
            elif group_key == 'M':
                dot_dict.minute = int(found_dict['M'])
            elif group_key == 'S':
                dot_dict.second = int(found_dict['S'])
            elif group_key == 'f':
                s = found_dict['f']
                # Pad to always return microseconds.
                s += "0" * (6 - len(s))
                dot_dict.fraction = int(s)
            elif group_key == 'A':
                dot_dict.weekday = locale_time.f_weekday.index(
                    found_dict['A'].lower())
            elif group_key == 'a':
                dot_dict.weekday = locale_time.a_weekday.index(
                    found_dict['a'].lower())
            elif group_key == 'w':
                dot_dict.weekday = int(found_dict['w'])
            elif group_key == 'u':
                dot_dict.weekday = int(found_dict['u'])
            elif group_key == 'j':
                dot_dict.julian = int(found_dict['j'])
            elif group_key in ('U', 'W'):
                dot_dict.week_of_year = int(found_dict[group_key])
            elif group_key == 'V':
                dot_dict.iso_week = int(found_dict['V'])
            elif group_key == 'z':
                z = found_dict['z']

                if z == 'Z':
                    dot_dict.gmtoff = 0
                else:
                    if z[3] == ':':
                        z = z[:3] + z[4:]

                        if len(z) > 5:
                            if z[5] != ':':
                                msg = ("Inconsistent use of : in "
                                       f"{found_dict['z']}")
                                raise ValueError(msg)

                            z = z[:5] + z[6:]

                    dot_dict.hours = int(z[1:3])
                    dot_dict.minutes = int(z[3:5])
                    dot_dict.seconds = int(z[5:7] or 0)
                    dot_dict.gmtoff = ((dot_dict.hours * 60 * 60) +
                                       (dot_dict.minutes * 60) +
                                       dot_dict.seconds)
                    gmtoff_remainder = z[8:]
                    # Pad to always return microseconds.
                    gmtoff_remainder_padding = (
                        "0" * (6 - len(gmtoff_remainder)))
                    dot_dict.gmtoff_fraction = int(gmtoff_remainder +
                                                   gmtoff_remainder_padding)

                    if z.startswith("-"):
                        dot_dict.gmtoff = -dot_dict.gmtoff
                        dot_dict.gmtoff_fraction = -dot_dict.gmtoff_fraction
            elif group_key == 'Z':
                # Since -1 is default value only need to worry about setting
                # tz if it can be something other than -1.
                found_zone = found_dict['Z'].lower()

                for value, tz_values in enumerate(locale_time.timezone):
                    if found_zone in tz_values:
                        # Deal with bad locale setup where timezone names are
                        # the same and yet time.daylight is true; too ambiguous
                        # to be able to tell what timezone has daylight savings
                        if (time.tzname[0] == time.tzname[1]and time.daylight
                            and found_zone not in (
                                "utc", "gmt")):  # pragma: no cover
                            break
                        else:
                            dot_dict.tz = value
                            break

        # Add timezone info
        dot_dict.tzname = found_dict.get("Z")
        return dot_dict

    def _check_iso_week(self, dot_dict):
        # Deal with the cases where ambiguities arise don't assume default
        # values for ISO week/year
        if dot_dict.iso_year is not None:
            if dot_dict.julian is not None:
                raise ValueError("Day of the year directive '%j' is not "
                                 "compatible with ISO year directive '%G'. "
                                 "Use '%Y' instead.")
            elif dot_dict.iso_week is None or dot_dict.weekday is None:
                raise ValueError("ISO year directive '%G' must be used with "
                                 "the ISO week directive '%V' and a weekday "
                                 "directive ('%A', '%a', '%w', or '%u').")
        elif dot_dict.iso_week is not None:
            if dot_dict.year is None or dot_dict.weekday is None:
                raise ValueError("ISO week directive '%V' must be used with "
                                 "the ISO year directive '%G' and a weekday "
                                 "directive ('%A', '%a', '%w', or '%u').")
            else:
                raise ValueError("ISO week directive '%V' is incompatible "
                                 "with the year directive '%Y'. Use the ISO "
                                 "year '%G' instead.")

    def _miscellaneous(self, dot_dict):
        if dot_dict.year is None:  # We need to make our best guess.
            if dot_dict.month == 0 and dot_dict.day == 5:
                dot_dict.year = 1  # 1 is first leap year of 1th century
            else:
                # Year 2 is not a leap year and is picked arbitrarily.
                dot_dict.year = 2

        # If we know the week of the year and what day of that week, we can
        # figure out the Julian day of the year.
        if dot_dict.julian is None and dot_dict.weekday is not None:
            if dot_dict.week_of_year is not None:
                dot_dict.julian = _calc_julian_from_U_or_W(
                    dot_dict.year, dot_dict.week_of_year, dot_dict.weekday)

                if dot_dict.julian is not None and dot_dict.julian <= 0:
                    bc = BahaiCalendar()
                    dot_dict.year -= 1
                    yday = 365 + bc._is_leap_year(dot_dict.year)
                    dot_dict.julian += yday
            elif (dot_dict.iso_year is not None
                  and dot_dict.iso_week is not None):
                datetime_result = datetime_date.fromisocalendar(
                    dot_dict.iso_year, dot_dict.iso_week, dot_dict.weekday + 1,
                    short=True)
                dot_dict.year = datetime_result.year
                dot_dict.month = datetime_result.month
                dot_dict.day = datetime_result.day

        if dot_dict.julian is None:
            # Cannot pre-calculate datetime_date() since can change in Julian
            # calculation and thus could have different value for the day of
            # the week calculation.
            # Need to add 1 to result since first day of the year is 1, not 0.
            dot_dict.julian = (datetime_date(dot_dict.year, dot_dict.month,
                                             dot_dict.day).toordinal() -
                               datetime_date(dot_dict.year, 1, 1).toordinal()
                               + 1)
        else:
            # Assume that if they bothered to include Julian day (or if it was
            # calculated above with year/week/weekday) it will be accurate.
            datetime_result = datetime_date.fromordinal(
                (dot_dict.julian - 1) +
                datetime_date(dot_dict.year, 1, 1).toordinal(), short=True)
            dot_dict.year = datetime_result.year
            dot_dict.month = datetime_result.month
            dot_dict.day = datetime_result.day

        if dot_dict.weekday is None:
            dot_dict.weekday = datetime_date(dot_dict.year, dot_dict.month,
                                             dot_dict.day).weekday()


def _strptime_time(data_string, format="%a %b %d %H:%M:%S %Y"):
    """
    Return a time struct based on the input string and the format string.
    """
    tt = StrpTime(data_string, format).start()[0]
    return _td_utils._build_struct_time(tt[:_td_utils._SHORT_STRUCT_TM_ITEMS],
                                        0, short_in=True)


def _strptime_datetime(cls, data_string, format="%a %b %d %H:%M:%S %Y"):
    """
    Return a datetime class cls instance based on the input string and the
    format string. (cls will always be a datetime class object)
    """
    tt, fraction, gmtoff_fraction = StrpTime(data_string, format).start()
    # *** TODO *** The below needs to be fixed when long date are implemented.
    tzname, gmtoff = tt[-2:]
    args = (*tt[:3], None, None, *tt[3: 6]) + (fraction,)

    if gmtoff is not None:
        tzdelta = datetime_timedelta(seconds=gmtoff,
                                     microseconds=gmtoff_fraction)

        if tzname:
            tz = datetime_timezone(tzdelta, tzname)
        else:
            tz = datetime_timezone(tzdelta)

        args += (tz,)

    return cls(*args)
