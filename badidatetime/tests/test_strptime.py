# -*- coding: utf-8 -*-
#
# badidatetime/test/test_strptime.py
#
__docformat__ = "restructuredtext en"

import os
import re
import unittest
import importlib
import locale
from unittest.mock import patch

from .._timedateutils import _td_utils
from .._strptime import (_getlang, LocaleTime, _calc_julian_from_U_or_W,
                         DotDict, StrpTime, TimeRE, _strptime_time,
                         _strptime_datetime)

datetime = importlib.import_module('badidatetime.datetime')


class TestStrptime_Functions(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self.getlocale_patcher = patch(
            'badidatetime._strptime.locale.getlocale')
        self.mock_getlocale = self.getlocale_patcher.start()
        self.mock_getlocale.return_value = ('en_US', 'UTF-8')

    def tearDown(self):
        self.getlocale_patcher.stop()

    #@unittest.skip("Temporarily skipped")
    def test__getlang(self):
        """
        Test that the _getlang function returns the current language.
        """
        result = _getlang()
        expected = ('en_US', 'UTF-8')
        msg = (f"Expected {expected}, found {result}.")
        self. assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test__calc_julian_from_U_or_W(self):
        """
        Test that the _calc_julian_from_U_or_W function returns the day of the
        year.
        The week_of_year is from 0 to 50/51
        The day_of_week is from 0 to 6.
        The 1st day is always week 0.
        """
        data = (
            # year, week_of_year, day_of_week, expected_result
            # 22   6    Istijlāl Kamál      False 365  152
            (-1, 21, 5, 152),  # (-1, 8, 19)
            # 01   4    Fiḍāl    Bahá       True  366  001
            (1, 0, 3, 1),      # (1, 1, 1)
            # 52   5    `Idāl    Bahá       True  366  001
            (182, 0, 4, 1),    # (182, 1, 1)
            # 52   6    Istijlāl Bahá       True  366  002
            (182, 0, 5, 2),    # (182, 1, 2)
            # 52   7    Istiqlāl Bahá       True  366  003
            (182, 0, 6, 3),    # (182, 1, 3)
            # 01   1    Jalál    Bahá       True  366  004
            (182, 0, 0, 4),    # (182, 1, 4)
            )
        msg = "Expected {}, with year {}, WoY {}, and DoW {}, found {}."

        for year, woy, dow, expected_result in data:
            result = _calc_julian_from_U_or_W(year, woy, dow)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, year, woy, dow, result))

    #@unittest.skip("Temporarily skipped")
    def test__strptime_time(self):
        """
        Test that the _strptime_time function returns the correct time struct
        depending on the format data.
        """
        data = (
            ('Jal Bah 05 12:30:30 1', '',
             'structures.ShortFormStruct(tm_year=1, tm_mon=1, tm_mday=5, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=0, tm_yday=5, '
             'tm_isdst=0)'),
            ('0182-01-16', '%Y-%m-%d',
             'structures.ShortFormStruct(tm_year=182, tm_mon=1, tm_mday=16, '
             'tm_hour=0, tm_min=0, tm_sec=0, tm_wday=5, tm_yday=16, '
             'tm_isdst=0)'),
            )
        msg = "Expected {}, found {}"

        for data_string, fmt, expected_result in data:
            if fmt == '':
                result = _strptime_time(data_string)
            else:
                result = _strptime_time(data_string, fmt)

            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test__strptime_datetime(self):
        """
        Test that the _strptime_datetime function returns an instance of
        the supplyed datetime class object.
        """
        def safe_data_parse():
            is_github = os.getenv('TEST_RUNNING') == 'true'

            if not is_github:
                extra_tests = (
                    ('182-01-16T12:30:30 EST', '%Y-%m-%dT%H:%M:%S %Z',
                     '0182-01-16T12:30:30'),
                    ('182-01-16T12:30:30-0500 EST', '%Y-%m-%dT%H:%M:%S%z %Z',
                     '0182-01-16T12:30:30-05:00'),
                    )
            else:
                extra_tests = ()
                # print(f"GitHub running tests. (TEST_RUNNING == {is_github})")

            return extra_tests

        data = (
            ('Jal Bah 05 12:30:30 1', '', '0001-01-05T12:30:30'),
            ('0182-01-16', '%Y-%m-%d', '0182-01-16T00:00:00'),
            ('0 5 12:30:30', '%m %d %H:%M:%S', '0001-00-05T12:30:30'),
            ('12:30:30', '%H:%M:%S', '0002-01-01T12:30:30'),
            ('182-01-16T12:30:30-05:00', '%Y-%m-%dT%H:%M:%S%z',
             '0182-01-16T12:30:30-05:00'),
            )
        msg = "Expected {}, found {}"
        data += safe_data_parse()

        for cnt, (data_string, fmt, expected_result) in enumerate(data):
            try:
                if fmt == '':
                    result = _strptime_datetime(datetime.datetime, data_string)
                else:
                    result = _strptime_datetime(datetime.datetime, data_string,
                                                fmt)

                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, result))
            except ValueError as e:
                raise ValueError(f"With cnt '{cnt}' an error was raised, {e}")


class TestStrptime_LocaleTime(unittest.TestCase):
    """
    This test is a bit different than most test. Since all the methods in
    the class are called in the constructor we need to test the results of
    each of the methods.
    """

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        os.environ['TZ'] = 'EST+05EDT,M4.1.0,M10.5.0'
        self._lt = LocaleTime()
        self._lt.LC_date_time = 'kam jam  17 22:44:30 0199'
        self._lt.LC_date = '%m/%d/%Y'
        self._lt.LC_time = '%I:%M:%S'

    #@unittest.skip("Temporarily skipped")
    def test_a_weekday(self):
        """
        Test that the abbreviated weekdays exist ard are in the correct order.
        """
        expected = [_td_utils.DAYNAMES_ABV[i].lower() for i in range(7)]
        result = self._lt.a_weekday
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_f_weekday(self):
        """
        Test that the full weekdays exist ard are in the correct order.
        """
        expected = [_td_utils.DAYNAMES[i].lower() for i in range(7)]
        result = self._lt.f_weekday
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_a_month(self):
        """
        Test that the abbreviated months exist ard are in the correct order.
        """
        expected = [_td_utils.MONTHNAMES_ABV[i].lower() for i in range(20)]
        result = self._lt.a_month
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_f_month(self):
        """
        Test that the full months exist ard are in the correct order.
        """
        expected = [_td_utils.MONTHNAMES[i].lower() for i in range(20)]
        result = self._lt.f_month
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_am_pm(self):
        """
        Test that the am_pm designators are properly set.
        """
        expected = ['am', 'pm']
        result = self._lt.am_pm
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_LC_date_time(self):
        """
        Test that the LC_date_time variable is set properly.
        """
        expected = 'kam jam  17 22:44:30 0199'
        result = self._lt.LC_date_time
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_LC_date(self):
        """
        Test that the LC_date variable is set properly.
        """
        expected = '%m/%d/%Y'
        result = self._lt.LC_date
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_LC_time(self):
        """
        Test that the LC_time variable is set properly.
        """
        expected = '%I:%M:%S'
        result = self._lt.LC_time
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)

    #@unittest.skip("Temporarily skipped")
    def test_timezone(self):
        """
        Test that the timezone variable is set properly.
        """
        expected = (frozenset({'gmt', 'est', 'utc'}), frozenset({'edt'}))
        result = self._lt.timezone
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)


class TestStrptime_TimeRE(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._tre = TimeRE()
        self._tre.locale_time.LC_date_time = '%a %b  %d %H:%M:%S %Y'
        self._tre.locale_time.LC_date = '%m/%d/%Y'
        self._tre.locale_time.LC_time = '%I:%M:%S'

    #@unittest.skip("Temporarily skipped")
    def test_regex_set_from_constructor(self):
        """
        Test that the constructor set up 27 items in the custom dict.
        """
        expected = 27
        result = len(self._tre)
        msg = "Expected {}, found {}"
        self.assertEqual(expected, result, msg.format(expected, result))

    #@unittest.skip("Temporarily skipped")
    def test___seqToRE(self):
        """
        Test that the __seqToRE method returns an empty string when the
        `to_convert` argument has empty values.
        """
        to_convert = ('',)
        result = self._tre._TimeRE__seqToRE(to_convert, '')
        expected = ''
        msg = "Expected {}, found {}"
        self.assertEqual(expected, result, msg.format(expected, result))

    #@unittest.skip("Temporarily skipped")
    def test_pattern(self):
        """
        Test that the pattern method returns a regex pattern.
        """
        data = (
            (self._tre.locale_time.LC_date_time,
             r"(?P<a>jal|jam|kam|fiḍ|idā|isj|isq)\s+(?P<b>ayy|bah|jal|jam|"
             r"aẓa|núr|raḥ|kal|kam|asm|izz|mas|ilm|qud|qaw|mas|sha|sul|mul|"
             r"alá)\s+(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])\s+(?P<H>2[0-3]|[0-1]"
             r"\d|\d):(?P<M>[0-5]\d|\d):(?P<S>6[0-1]|[0-5]\d|\d)\s+(?P<Y>-?"
             r"\d{1,4})"),
            (self._tre.locale_time.LC_date,
             r"(?P<m>1[0-9]|0[0-9]|[0-9])/(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])"
             r"/(?P<Y>-?\d{1,4})"),
            (self._tre.locale_time.LC_time,
             r"(?P<I>1[0-2]|0[1-9]|[1-9]):(?P<M>[0-5]\d|\d):(?P<S>6[0-1]|"
             r"[0-5]\d|\d)"),
            )
        msg = "Expected {}, found {}"

        for fmt, expected_result in data:
            result = self._tre.pattern(fmt)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))


class TestStrptime_DotDict(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        aritrary_dict = {1: 1, 'a': 'a'}
        self._dd = DotDict(aritrary_dict)

    def test_get(self):
        """
        Test that the get method returns the correct object from the
        dictionary.
        """
        err_msg0 = "No such attribute: {}"
        data = (
            (1, False, 1),
            ('a', False, 'a'),
            ('b', True, err_msg0.format('b')),
            )
        msg = "Expected {}, found {}"

        for key, validity, expected_result in data:
            if validity:
                try:
                    result = getattr(self._dd, key)
                except AttributeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(f"With key '{key}' an error was not "
                                         f"raised, returned {result}.")
            else:
                result = self._dd.get(key)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, result))

    def test_set(self):
        """
        Test that the set method creates a new object in the dictionary.
        """
        key = 'c'
        expected = 'c'
        self._dd[key] = 'c'
        result = getattr(self._dd, key)
        msg = f"Expected {expected}, found {result}"
        self.assertEqual(expected, result, msg)


class TestStrptime_StrpTime(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self.locale_patcher = patch(
            'badidatetime._timedateutils.locale.nl_langinfo')
        self.mock_nl_langinfo = self.locale_patcher.start()

        def side_effect(item):
            if item == locale.LC_TIME:
                return 'en_US.UTF-8'
            elif item == locale.AM_STR:
                return 'AM'
            elif item == locale.PM_STR:
                return 'PM'
            elif item == locale.D_FMT:
                return '%m/%d/%Y'
            elif item == locale.T_FMT:
                return '%H:%M:%S'
            else:
                return 'Default Value'

        self.mock_nl_langinfo.side_effect = side_effect

    def tearDown(self):
        self.locale_patcher.stop()

    #@unittest.skip("Temporarily skipped")
    def test_constructor_exception(self):
        """
        """
        with self.assertRaises(TypeError) as cm:
            StrpTime(100)

        message = str(cm.exception)
        err_msg = "strptime() argument 0 must be str, not <class 'int'>."
        self.assertEqual(err_msg, message)

    #@unittest.skip("Temporarily skipped")
    def test__find_regex(self):
        """
        Test that the _strptime function returns a complex tuple in the form
        of ((year, month, day, None, None,
             hour, minute, second,
             weekday, julian, tz, tzname, gmroff), fraction, gmtoff_fraction)
        """
        err_msg0 = "'{}' is a bad directive in format '{}'"
        err_msg1 = "stray %% in format '{}'"
        err_msg2 = "Time data '{}' does not match format '{}'."
        err_msg3 = "Unconverted data remains: {}"
        data = (
            # The default format does not work in my code or the standard
            # Python code.
            ('Jal Bah 01 06:12:30 182', '%a %b %d %H:%M:%S %Y', False,
             "<re.Match object; span=(0, 23), match='Jal Bah "
             "01 06:12:30 182'>"),
            ('06:12:30', '%H:%M:%S', False,
             "<re.Match object; span=(0, 8), match='06:12:30'>"),
            ('1', '%U', False, "<re.Match object; span=(0, 1), match='1'>"),
            ('52', '%V', False, "<re.Match object; span=(0, 2), match='52'>"),
            ('10', '% ', True, err_msg0.format(
                '%', '<built-in function format>')),
            ('12', '%', True, err_msg1.format('%')),
            ('stuff', '%y', True, err_msg2.format('stuff', '%y')),
            ('123', '%u', True, err_msg3.format('23')),
            )
        msg = "Expected {}, found {}"

        for data_str, fmt, validity, expected_result in data:
            st = StrpTime(data_str, fmt)
            st._clear_cache()

            if validity:
                try:
                    st._find_regex()
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(f"With data_str {data_str} an error "
                                         "was not raised.")
            else:
                result = st._find_regex()
                self.assertEqual(expected_result, str(result[0]), msg.format(
                    expected_result, result))

    #@unittest.skip("Temporarily skipped")
    def test__parse_found_dict(self):
        """
        Test that the _parse_found_dict method correctly parses the arguments
        using the correct regex.
        """
        err_msg0 = "Inconsistent use of : in {}"
        data = (
            (r"(?P<y>\d\d)", 'year', '42', False, 42),
            (r"(?P<Y>-?\d\d\d\d)", 'year', '0042', False, 42),
            (r"(?P<G>-?\d\d\d\d)", 'iso_year', '0181', False, 181),
            (r"(?P<G>-?\d\d\d\d)", 'iso_year', '-1842', False, -1842),
            (r"(?P<m>1[0-9]|0[1-9]|[1-9])", 'month', '19', False, 19),
            (r"(?P<m>1[0-9]|0[1-9]|[1-9])", 'month', '09', False, 9),
            (r"(?P<m>1[0-9]|0[1-9]|[1-9])", 'month', '9', False, 9),
            (r"(?P<m>1[0-9]|0[0-9]|[0-9])", 'month', '00', False, 0),
            (r"(?P<B>ayyám\-i\-há|mashíyyat|'aẓamat|kalimát|masá’il|raḥmat|"
             r"'izzat|qudrat|sharaf|sulṭán|jalál|jamál|kamál|asmá'|'alá'|bahá|"
             r"'ilm|qawl|mulk|núr)", 'month', 'Bahá', False, 1),
            (r"(?P<B>ayyám-i-há|mashíyyat|'aẓamat|kalimát|masá’il|raḥmat|"
             r"'izzat|qudrat|sharaf|sulṭán|jalál|jamál|kamál|asmá'|'alá'|bahá|"
             r"'ilm|qawl|mulk|núr)", 'month', 'Ayyám-i-Há', False, 0),
            (r"(?P<b>ayy|bah|jal|jam|aẓa|núr|raḥ|kal|kam|asm|izz|mas|ilm|qud|"
             r"qaw|mas|sha|sul|mul|alá)", 'month', 'Alá', False, 19),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', '09', False, 9),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', '9', False, 9),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', ' 9', False, 9),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', '19', False, 19),
            (r"(?P<H>2[0-3]|[0-1]\d|\d)", 'hour', '23', False, 23),
            (r"(?P<H>2[0-3]|[0-1]\d|\d)", 'hour', '02', False, 2),
            (r"(?P<H>2[0-3]|[0-1]\d|\d)", 'hour', '2', False, 2),
            (r"(?P<I>1[0-2]|0[1-9]|[1-9])", 'hour', '12', False, 0),  # am
            (r"(?P<I>1[0-2]|0[1-9]|[1-9])", 'hour', '02', False, 2),
            (r"(?P<I>1[0-2]|0[1-9]|[1-9])", 'hour', '2', False, 2),
            (r"(?P<M>[0-5]\d|\d)", 'minute', '00', False, 0),
            (r"(?P<M>[0-5]\d|\d)", 'minute', '9', False, 9),
            (r"(?P<S>6[0-1]|[0-5]\d|\d)", 'second', '60', False, 60),
            (r"(?P<S>6[0-1]|[0-5]\d|\d)", 'second', '59', False, 59),
            (r"(?P<S>6[0-1]|[0-5]\d|\d)", 'second', '9', False, 9),
            (r"(?P<f>[0-9]{1,6})", 'fraction', '09', False, 90000),
            (r"(?P<A>istijlāl|istiqlāl|jalál|jamál|kamál|fiḍāl|`idāl)",
             'weekday', 'Jalál', False, 0),
            (r"(?P<A>istijlāl|istiqlāl|jalál|jamál|kamál|fiḍāl|`idāl)",
             'weekday', '`Idāl', False, 4),
            (r"(?P<a>jal|jam|kam|fiḍ|idā|isj|isq)", 'weekday', 'Jal', False, 0),
            (r"(?P<a>jal|jam|kam|fiḍ|idā|isj|isq)", 'weekday', 'Idā', False, 4),
            (r"(?P<w>[0-6])", 'weekday', '6', False, 6),
            (r"(?P<u>[1-7])", 'weekday', '7', False, 7),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '365', False, 365),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '350', False, 350),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '250', False, 250),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '090', False, 90),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '009', False, 9),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '9', False, 9),
            (r"(?P<U>5[0-2]|[0-4]\d|\d)", 'week_of_year', '52', False, 52),
            (r"(?P<U>5[0-2]|[0-4]\d|\d)", 'week_of_year', '40', False, 40),
            (r"(?P<U>5[0-2]|[0-4]\d|\d)", 'week_of_year', '01', False, 1),
            (r"(?P<W>5[0-2]|[0-4]\d|\d)", 'week_of_year', '52', False, 52),
            (r"(?P<W>5[0-2]|[0-4]\d|\d)", 'week_of_year', '40', False, 40),
            (r"(?P<W>5[0-2]|[0-4]\d|\d)", 'week_of_year', '01', False, 1),
            (r"(?P<V>5[0-3]|0[1-9]|[1-4]\d|\d)", 'iso_week', '52', False, 52),
            (r"(?P<Z>gmt|est|utc|edt)", 'tz', 'GMT', False, 0),
            (r"(?P<z>[+-]\d\d:?[0-5]\d(:?[0-5]\d(\.\d{1,6})?)?|(?-i:Z))",
             'gmtoff', '-0500', False, -18000),
            (r"(?P<z>[+-]\d\d:?[0-5]\d(:?[0-5]\d(\.\d{1,6})?)?|(?-i:Z))",
             'gmtoff', '-05:00:00', False, -18000),
            (r"(?P<z>[+-]\d\d:?[0-5]\d(:?[0-5]\d(\.\d{1,6})?)?|(?-i:Z))",
             'gmtoff', 'Z', False, 0),  # Zulu or GMT
            (r"(?P<z>[+-]\d\d:?[0-5]\d(:?[0-5]\d(\.\d{1,6})?)?|(?-i:Z))",
             'gmtoff', '-03:3015', True, err_msg0.format('-03:3015')),
            )
        msg = "Expected {}, with cnt {} and data_str {}, found {}"

        for cnt, (regex, variable, data_str, validity,
                  expected_result) in enumerate(data):
            st = StrpTime(data_str)
            locale_time = LocaleTime()
            cmp_regex = re.compile(regex, re.IGNORECASE)
            found = cmp_regex.match(data_str)

            if validity:
                try:
                    st._parse_found_dict(found, locale_time)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(f"With count test {cnt} an error "
                                         "was not raised.")
            else:
                dot_dict = st._parse_found_dict(found, locale_time)
                result = getattr(dot_dict, variable)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, cnt, data_str, result))

    #@unittest.skip("Temporarily skipped")
    def test_multi_char_formats(self):
        """
        Test that the _parse_found_dict method correctly parses multi
        character formats using the correct regex.
        """
        data = (
            (r"(?P<I>1[0-2]|0[1-9]|[1-9])\s*(?P<p>am|pm)", '2 pm',
             {'hour': 14}),
            (r"(?P<y>\d\d) %", '01 %', {'year': 1, }),
            (r"(?P<a>jal|jam|kam|fiḍ|idā|isj|isq)\s+(?P<b>ayy|bah|jal|jam|"
             r"aẓa|núr|raḥ|kal|kam|asm|izz|mas|ilm|qud|qaw|mas|sha|sul|mul|"
             r"alá)\s+(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])\s+(?P<H>2[0-3]|"
             r"[0-1]\d|\d):(?P<M>[0-5]\d|\d):(?P<S>6[0-1]|[0-5]\d|\d)"
             r"\s+(?P<Y>-?\d{1,4})", 'Idā Bah 01 12:30:30 182',
             {'year': 182, 'month': 1, 'day': 1, 'hour': 12, 'minute': 30,
              'second': 30}),
            (r"(?P<m>1[0-9]|0[0-9]|[0-9])/(?P<d>1[0-9]|0[1-9]|[1-9]|"
             r" [1-9])/(?P<Y>-?\d{1,4})", '19/19/0181',
             {'year': 181, 'month': 19, 'day': 19}),
            (r"(?P<I>1[0-2]|0[1-9]|[1-9]):(?P<M>[0-5]\d|\d):(?P<S>6[0-1]|"
             r"[0-5]\d|\d)", '11:30:30',
             {'hour': 11, 'minute': 30, 'second': 30}),
            )
        msg = "Expected {}, with cnt {} and data_str {}, found {}"

        for cnt, (regex, data_str, expected_result) in enumerate(data):
            st = StrpTime(data_str)
            locale_time = LocaleTime()
            cmp_regex = re.compile(regex, re.IGNORECASE)
            found = cmp_regex.match(data_str)

            try:
                dot_dict = st._parse_found_dict(found, locale_time)
            except AttributeError as e:
                raise AttributeError(f"Count {cnt}, {e}")

            for key, expected in expected_result.items():
                result = getattr(dot_dict, key)
                self.assertEqual(expected, result, msg.format(
                    expected_result, cnt, data_str, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_iso_week(self):
        """
        Test that the _check_iso_week method raises the appropreate exceptions.
        """
        err_msg0 = ("Day of the year directive '%j' is not compatible with "
                    "ISO year directive '%G'. Use '%Y' instead.")
        err_msg1 = ("ISO year directive '%G' must be used with the ISO week "
                    "directive '%V' and a weekday directive ('%A', '%a', "
                    "'%w', or '%u').")
        err_msg2 = ("ISO week directive '%V' must be used with the ISO year "
                    "directive '%G' and a weekday directive ('%A', '%a', "
                    "'%w', or '%u').")
        err_msg3 = ("ISO week directive '%V' is incompatible with the year "
                    "directive '%Y'. Use the ISO year '%G' instead.")
        data = (
            ('iso_year', None, 'julian', None, 'iso_week', None,
             'weekday', '2', 'year', '-1842', False, ''),
            ('iso_year', '-1842', 'julian', None, 'iso_week', '1',
             'weekday', '2', 'year', '-1842', False, ''),
            ('iso_year', '-1842', 'julian', '1', 'iso_week', '1',
             'weekday', '2', 'year', '-1842', True, err_msg0),
            ('iso_year', '-1842', 'julian', None, 'iso_week', None,
             'weekday', '2', 'year', '-1842', True, err_msg1),
            ('iso_year', '-1842', 'julian', None, 'iso_week', '1',
             'weekday', None, 'year', '-1842', True, err_msg1),
            ('iso_year', None, 'julian', None, 'iso_week', '1',
             'weekday', '1', 'year', None, True, err_msg2),
            ('iso_year', None, 'julian', None, 'iso_week', '1',
             'weekday', None, 'year', '182', True, err_msg2),
            ('iso_year', None, 'julian', None, 'iso_week', '1',
             'weekday', '1', 'year', '182', True, err_msg3),
            )

        for cnt, (var0, value0, var1, value1, var2, value2, var3, value3,
                  var4, value4, validity, expected_result) in enumerate(data):
            st = StrpTime("")
            dot_dict = DotDict({var0: value0, var1: value1,
                                var2: value2, var3: value3, var4: value4})

            if validity:
                try:
                    st._check_iso_week(dot_dict)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    # Raise an error when an AssertionError is not raised.
                    raise AssertionError(f"With count test {cnt} an error "
                                         "was not raised.")
            else:  # Valid tests (Nothing to assert)
                st._check_iso_week(dot_dict)

    #@unittest.skip("Temporarily skipped")
    def test__miscellaneous(self):
        """
        Test that the _miscellaneous method updates a few values.

        Both `weekday` and `week_of_year` start their count with 0 not 1.
        """
        data = (
            # Setting the julian day (day of the year) if it is None and
            # weekday is not None.
            ({'julian': None, 'weekday': 4, 'week_of_year': 0, 'year': 182},
             {'julian': 1}),
            # Updating the julian day (day of the year).
            ({'julian': None, 'weekday': 6, 'week_of_year': 0, 'year': 182,
              'iso_year': None, 'iso_week': None}, {'julian': 3}),
            # Setting the year, month, day, and julian
            ({'julian': None, 'weekday': 4, 'week_of_year': None, 'year': None,
              'month': None, 'iso_year': 182, 'iso_week': 1},
             {'year': 182, 'month': 1, 'day': 8, 'julian': 8, 'weekday': 4}),
            # Setting the year, month, and day.
            ({'julian': 1, 'weekday': None, 'year': 182},
             {'year': 182, 'month': 1, 'day': 1, 'weekday': 4}),
            # Test for leap year when Ayyám-i-Há 5 is given.
            ({'julian': None, 'weekday': None, 'year': None, 'month': 0,
              'day': 5}, {'year': 1}),
            ({'julian': None, 'weekday': 0, 'week_of_year': -1, 'year': 182},
             {'julian': 355, 'year': 181}),  # 0181-19-09
            )
        msg = "Expected {}, with variable '{}', found {}"

        for cnt, (items, expected_result) in enumerate(data):
            dd = DotDict(items)
            st = StrpTime("")

            try:
                st._miscellaneous(dd)
            except AttributeError as e:
                raise AttributeError(f"Test count {cnt}, {e}.")

            for var, expected in expected_result.items():
                result = getattr(dd, var)
                self.assertEqual(expected, result, msg.format(
                    expected, var, result))
