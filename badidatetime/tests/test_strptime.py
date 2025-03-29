# -*- coding: utf-8 -*-
#
# badidatetime/test/test_strptime.py
#
__docformat__ = "restructuredtext en"

import re
import unittest
import importlib
import locale
from unittest.mock import patch

from .._timedateutils import _td_utils
from .._strptime import (_getlang, LocaleTime, _calc_julian_from_U_or_W,
                         DotDict, _StrpTime, TimeRE, _strptime_time,
                         _strptime_datetime)

datetime = importlib.import_module('badidatetime.datetime')


class TestStrptime_Functions(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self.locale_patcher = patch('badidatetime._strptime.locale.getlocale')
        self.mock_getlocale = self.locale_patcher.start()
        self.mock_getlocale.return_value = ('en_US', 'UTF-8')

    def tearDown(self):
        self.locale_patcher.stop()

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


class TestStrptime_LocaleTime(unittest.TestCase):
    """
    This test is a bit different than most test. Since all the methods in
    the class are called in the constructor we need to test the results of
    each of the method.
    """

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._lt = LocaleTime()
        self._lt.LC_date_time = 'kam jam  17 22:44:30 0199'
        self._lt.LC_date = '%m/%d/%Y'
        self._lt.LC_time = '%I:%M:%S'

    #@unittest.skip("Temporarily skipped")
    def test_a_weekday(self):
        """
        Test that the abbreviated weekdays exist ard are in the correct order.
        """
        abv_weekdays = [_td_utils.DAYNAMES_ABV[i].lower() for i in range(7)]
        result = self._lt.a_weekday
        msg = "Expected {}, found {}"
        self.assertEqual(abv_weekdays, result, msg.format(
            abv_weekdays, result))

    #@unittest.skip("Temporarily skipped")
    def test_f_weekday(self):
        """
        Test that the full weekdays exist ard are in the correct order.
        """
        weekdays = [_td_utils.DAYNAMES[i].lower() for i in range(7)]
        result = self._lt.f_weekday
        msg = "Expected {}, found {}"
        self.assertEqual(weekdays, result, msg.format(weekdays, result))

    #@unittest.skip("Temporarily skipped")
    def test_a_month(self):
        """
        Test that the abbreviated months exist ard are in the correct order.
        """
        abv_month = [_td_utils.MONTHNAMES_ABV[i].lower() for i in range(20)]
        result = self._lt.a_month
        msg = "Expected {}, found {}"
        self.assertEqual(abv_month, result, msg.format(abv_month, result))

    #@unittest.skip("Temporarily skipped")
    def test_f_month(self):
        """
        Test that the full months exist ard are in the correct order.
        """
        month = [_td_utils.MONTHNAMES[i].lower() for i in range(20)]
        result = self._lt.f_month
        msg = "Expected {}, found {}"
        self.assertEqual(month, result, msg.format(month, result))

    #@unittest.skip("Temporarily skipped")
    def test_am_pm(self):
        """
        Test that the am_pm designators are properly set.
        """
        expected = ['am', 'pm']
        result = self._lt.am_pm
        msg = "Expected {}, found {}"
        self.assertEqual(expected, result, msg.format(expected, result))

    #@unittest.skip("Temporarily skipped")
    def test_LC_date_time(self):
        """
        Test that the LC_date_time variable is set properly.
        """
        expected = 'kam jam  17 22:44:30 0199'
        result = self._lt.LC_date_time
        msg = "Expected {}, found {}"
        self.assertEqual(expected, result, msg.format(expected, result))

    #@unittest.skip("Temporarily skipped")
    def test_LC_date(self):
        """
        Test that the LC_date variable is set properly.
        """
        expected = '%m/%d/%Y'
        result = self._lt.LC_date
        msg = "Expected {}, found {}"
        self.assertEqual(expected, result, msg.format(expected, result))

    #@unittest.skip("Temporarily skipped")
    def test_LC_time(self):
        """
        Test that the LC_time variable is set properly.
        """
        expected = '%I:%M:%S'
        result = self._lt.LC_time
        msg = "Expected {}, found {}"
        self.assertEqual(expected, result, msg.format(expected, result))

    #@unittest.skip("Temporarily skipped")
    def test_timezone(self):
        """
        Test that the timezone variable is set properly.
        """
        expected = (frozenset({'gmt', 'est', 'utc'}), frozenset({'edt'}))
        result = self._lt.timezone
        msg = "Expected {}, found {}"
        self.assertEqual(expected, result, msg.format(expected, result))


class TestStrptime_TimeRE(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self._tre = TimeRE()
        # self._tre.locale_time.LC_date_time = 'kam jam  17 22:44:30 0199'
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
             r"\d\d\d\d)"),
            (self._tre.locale_time.LC_date,
             r"(?P<m>1[0-9]|0[0-9]|[0-9])/(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])"
             r"/(?P<Y>-?\d\d\d\d)"),
            (self._tre.locale_time.LC_time,
             r"(?P<I>1[0-2]|0[1-9]|[1-9]):(?P<M>[0-5]\d|\d):(?P<S>6[0-1]|"
             r"[0-5]\d|\d)"),
            )
        msg = "Expected {}, found {}"

        for fmt, expected_result in data:
            result = self._tre.pattern(fmt)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, result))


class TestStrptime__StrpTime(unittest.TestCase):

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
            _StrpTime(100)

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
            #('Jal Bah 01 06:12:30 182', '%a %b %d %H:%M:%S %Y', ""),
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
            st = _StrpTime(data_str, fmt)
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
        data = (
            (r"(?P<y>\d\d)", 'year', '42', 42),
            (r"(?P<Y>-?\d\d\d\d)", 'year', '0042', 42),
            (r"(?P<G>-?\d\d\d\d)", 'iso_year', '0181', 181),
            (r"(?P<G>-?\d\d\d\d)", 'iso_year', '-1842', -1842),
            (r"(?P<m>1[0-9]|0[1-9]|[1-9])", 'month', '19', 19),
            (r"(?P<m>1[0-9]|0[1-9]|[1-9])", 'month', '09', 9),
            (r"(?P<m>1[0-9]|0[1-9]|[1-9])", 'month', '9', 9),
            (r"(?P<m>1[0-9]|0[0-9]|[0-9])", 'month', '00', 0),
            (r"(?P<B>ayyám\-i\-há|mashíyyat|'aẓamat|kalimát|masá’il|raḥmat|"
             r"'izzat|qudrat|sharaf|sulṭán|jalál|jamál|kamál|asmá'|'alá'|bahá|"
             r"'ilm|qawl|mulk|núr)", 'month', 'Bahá', 1),
            (r"(?P<B>ayyám-i-há|mashíyyat|'aẓamat|kalimát|masá’il|raḥmat|"
             r"'izzat|qudrat|sharaf|sulṭán|jalál|jamál|kamál|asmá'|'alá'|bahá|"
             r"'ilm|qawl|mulk|núr)", 'month', 'Ayyám-i-Há', 0),
            (r"(?P<b>ayy|bah|jal|jam|aẓa|núr|raḥ|kal|kam|asm|izz|mas|ilm|qud|"
             r"qaw|mas|sha|sul|mul|alá)", 'month', 'Alá', 19),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', '09', 9),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', '9', 9),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', ' 9', 9),
            (r"(?P<d>1[0-9]|0[1-9]|[1-9]| [1-9])", 'day', '19', 19),
            (r"(?P<H>2[0-3]|[0-1]\d|\d)", 'hour', '23', 23),
            (r"(?P<H>2[0-3]|[0-1]\d|\d)", 'hour', '02', 2),
            (r"(?P<H>2[0-3]|[0-1]\d|\d)", 'hour', '2', 2),
            # Need to set am and pm or these will fail depending on the
            # time of the day.
            (r"(?P<I>1[0-2]|0[1-9]|[1-9])", 'hour', '12', 0),  # am
            (r"(?P<I>1[0-2]|0[1-9]|[1-9])", 'hour', '02', 2),
            (r"(?P<I>1[0-2]|0[1-9]|[1-9])", 'hour', '2', 2),
            (r"(?P<M>[0-5]\d|\d)", 'minute', '00', 0),
            (r"(?P<M>[0-5]\d|\d)", 'minute', '9', 9),
            (r"(?P<S>6[0-1]|[0-5]\d|\d)", 'second', '60', 60),
            (r"(?P<S>6[0-1]|[0-5]\d|\d)", 'second', '59', 59),
            (r"(?P<S>6[0-1]|[0-5]\d|\d)", 'second', '9', 9),
            (r"(?P<f>[0-9]{1,6})", 'fraction', '09', 90000),
            (r"(?P<A>istijlāl|istiqlāl|jalál|jamál|kamál|fiḍāl|`idāl)",
             'weekday', 'Jalál', 0),
            (r"(?P<A>istijlāl|istiqlāl|jalál|jamál|kamál|fiḍāl|`idāl)",
             'weekday', '`Idāl', 4),
            (r"(?P<a>jal|jam|kam|fiḍ|idā|isj|isq)", 'weekday', 'Jal', 0),
            (r"(?P<a>jal|jam|kam|fiḍ|idā|isj|isq)", 'weekday', 'Idā', 4),
            (r"(?P<w>[0-6])", 'weekday', '6', 6),
            (r"(?P<u>[1-7])", 'weekday', '7', 7),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '365', 365),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '350', 350),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '250', 250),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '090', 90),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '009', 9),
            (r"(?P<j>36[0-6]|3[0-5]\d|[1-2]\d\d|0[1-9]\d|00[1-9]|[1-9]\d|"
             r"0[1-9]|[1-9])", 'julian', '9', 9),
            (r"(?P<U>5[0-2]|[0-4]\d|\d)", 'week_of_year', '52', 52),
            (r"(?P<U>5[0-2]|[0-4]\d|\d)", 'week_of_year', '40', 40),
            (r"(?P<U>5[0-2]|[0-4]\d|\d)", 'week_of_year', '01', 1),
            (r"(?P<W>5[0-2]|[0-4]\d|\d)", 'week_of_year', '52', 52),
            (r"(?P<W>5[0-2]|[0-4]\d|\d)", 'week_of_year', '40', 40),
            (r"(?P<W>5[0-2]|[0-4]\d|\d)", 'week_of_year', '01', 1),
            (r"(?P<V>5[0-3]|0[1-9]|[1-4]\d|\d)", 'iso_week', '52', 52),
            (r"(?P<Z>gmt|est|utc|edt)", 'tz', 'GMT', 0),
            )
        msg = "Expected {}, with data_str {}, found {}"

        for regex, variable, data_str, expected_result in data:
            st = _StrpTime(data_str)
            locale_time = LocaleTime()
            cmp_regex = re.compile(regex, re.IGNORECASE)
            found = cmp_regex.match(data_str)
            dot_dict = st._parse_found_dict(found, locale_time)
            #print(found_dict)
            result = getattr(dot_dict, variable)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, data_str, result))

    # @unittest.skip("Temporarily skipped")
    # def test__parse_found_dict_alt(self):
    #             """
    #     Test that the _parse_found_dict_alt method correctly parses multiple
    #     arguments using the correct regexs.
    #     """
    #     data = (
    #         (r"(?P<z>[+-]\d\d:?[0-5]\d(:?[0-5]\d(\.\d{1,6})?)?|(?-i:Z))",
    #          'gmtoff', '', ),
    #         #(),
    #         )
    #     msg = "Expected {}, with data_str {}, found {}"

    #     for regexs, variables, data_str, expected_result in data:

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
            st = _StrpTime("")
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
            # ({'julian': None, 'weekday': 6, 'week_of_year': 0, 'year': 182,
            #   'iso_year': None, 'iso_week': None}, {'julian': 6}),
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
            )
        msg = "Expected {}, with variable '{}', found {}"

        for cnt, (items, expected_result) in enumerate(data):
            dd = DotDict(items)
            st = _StrpTime("")

            try:
                st._miscellaneous(dd)
            except AttributeError as e:
                raise AttributeError(f"Test count {cnt}, {e}.")

            for var, expected in expected_result.items():
                result = getattr(dd, var)
                self.assertEqual(expected, result, msg.format(
                    expected, var, result))


"""
{
 'z': '(?P<z>[+-]\\d\\d:?[0-5]\\d(:?[0-5]\\d(\\.\\d{1,6})?)?|(?-i:Z))',
 'p': '(?P<p>am|pm)',
 '%': '%',
 'c': 'idā\\s+bah\\s+(?P<j>36[0-6]|3[0-5]\\d|[1-2]\\d\\d|0[1-9]\\d|00[1-9]|[1-9]\\d|0[1-9]|[1-9])\\s+(?P<H>2[0-3]|[0-1]\\d|\\d):(?P<M>[0-5]\\d|\\d):(?P<S>6[0-1]|[0-5]\\d|\\d)\\s+(?P<w>[0-6])(?P<Y>-?\\d\\d\\d\\d)',
 'x': '(?P<w>[0-6])(?P<j>36[0-6]|3[0-5]\\d|[1-2]\\d\\d|0[1-9]\\d|00[1-9]|[1-9]\\d|0[1-9]|[1-9])/(?P<w>[0-6])(?P<j>36[0-6]|3[0-5]\\d|[1-2]\\d\\d|0[1-9]\\d|00[1-9]|[1-9]\\d|0[1-9]|[1-9])/(?P<w>[0-6])(?P<Y>-?\\d\\d\\d\\d)',
 'X': '(?P<j>36[0-6]|3[0-5]\\d|[1-2]\\d\\d|0[1-9]\\d|00[1-9]|[1-9]\\d|0[1-9]|[1-9])(?P<w>[0-6]):(?P<M>[0-5]\\d|\\d):(?P<S>6[0-1]|[0-5]\\d|\\d)'
}
"""
