# -*- coding: utf-8 -*-
#
# badidatetime/test/test_datetime.py
#
__docformat__ = "restructuredtext en"

import re
import locale
import time
import pickle
import importlib
import unittest
from unittest.mock import patch, PropertyMock
from zoneinfo import ZoneInfo

from .. import enable_geocoder
from ..badi_calendar import BahaiCalendar

datetime = importlib.import_module('badidatetime.datetime')


class TestBadiDatetimeFunctions(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    @classmethod
    def setUpClass(cls):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    def setUp(self):
        self._bc = BahaiCalendar()

    #@unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns the the correct value for the
        caparison.
        """
        data = (
            (10, 10, 0),
            (10, 9, 1),
            (10, 11, -1),
            )
        msg = "Expected {} with x {} and y {}, found {}."

        for x, y, expected_result in data:
            result = datetime._cmp(x, y)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, x, y, result))

    #@unittest.skip("Temporarily skipped")
    def test__divide_and_round(self):
        """
        Test that the _divide_and_round function returns the correct result.
        """
        data = (
            ((7, 3), 2),
            ((99, 4), 25),
            ((99, 3), 33),
            )
        msg = "Expected {} with values {}, found {}."

        for values, expected_result in data:
            result = datetime._divide_and_round(*values)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, values, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_offset(self):
        """
        Test that the _check_utc_offset function returns the correct result.
        """
        err_msg0 = ("Invalid name argument '{}' must be one of "
                    "('utcoffset', 'badioffset', 'dst').")
        err_msg1 = "tzinfo.{}() must return None or timedelta, not {}"
        err_msg2 = ("{}()={}, must be strictly between -timedelta(hours=24) "
                    "and timedelta(hours=24)")
        data = (
            ('utcoffset', datetime.timedelta(hours=10), False, None),
            ('dst', datetime.timedelta(hours=1), False, None),
            ('utcoffset', None, False, None),
            ('junk', None, True, err_msg0.format('junk')),
            ('utcoffset', 10, True, err_msg1.format('utcoffset', type(10))),
            ('utcoffset', datetime.timedelta(hours=24), True,
             err_msg2.format('utcoffset', '1 day, 0:00:00')),
            )
        msg = "Expected {} with name {} and offset {}, found {}."

        for name, offset, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._check_offset(name, offset)
                except (AssertionError, TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(
                        f"With {name} and {offset} an error is "
                        f"not raised, with result {result}.")
            else:
                result = datetime._check_offset(name, offset)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, name, offset, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_tzinfo_arg(self):
        """
        Test that the _check_tzinfo_arg function returns the correct result.
        """
        err_msg0 = ("tzinfo argument must be None or of a tzinfo subclass, "
                    "found {}")
        data = (
            (datetime.BADI, False, None),
            ('JUNK', True, err_msg0.format("'JUNK'")),
            )
        msg = "Expected {} with tz {}, found {}."

        for tz, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._check_tzinfo_arg(tz)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(
                        f"With {tz} an error is not raised, with "
                        f"result {result}.")
            else:
                result = datetime._check_tzinfo_arg(tz)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, tz, result))

    #@unittest.skip("Temporarily skipped")
    def test__cmperror(self):
        """
        Test that the _cmperror
        """
        err_msg0 = "Cannot compare {} to {}"
        data = (
            ((181, 1, 1), (181, 1, 1),
             err_msg0.format("'date'", "'datetime'")),
            )
        msg = "date0 {} and date1 {}."

        for date0, date1, expected_result in data:
            d = datetime.date(*date0)
            dt = datetime.datetime(*date1)

            try:
                datetime._cmperror(d, dt)
            except TypeError as e:
                self.assertEqual(expected_result, str(e),
                                 msg.format(date0, date1))

    #@unittest.skip("Temporarily skipped")
    def test__format_time(self):
        """
        Test that the _format_time function returns the correct result.
        """
        specs = ('auto', 'hours', 'minutes', 'seconds', 'milliseconds',
                 'microseconds')
        err_msg0 = f"Invalid timespec '{{}}', must be one of {specs}."
        data = (
            ((12, 30, 30, 0), 'auto', False, '12:30:30'),
            ((12, 30, 30, 1000), 'auto', False, '12:30:30.001000'),
            ((12, 30, 30, 1000), 'milliseconds', False, '12:30:30.001'),
            ((12, 30, 30, 0), 'hours', False, '12'),
            ((12, 30, 30, 0), 'minutes', False, '12:30'),
            ((12, 30, 30, 0), 'seconds', False, '12:30:30'),
            ((12, 30, 30, 500000), 'microseconds', False, '12:30:30.500000'),
            ((12, 30, 30, 0), 'junk', True, err_msg0.format('junk')),
            )
        msg = "Expected {} with hhmmssus {} and ts {}, found {}."

        for hhmmssus, ts, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._format_time(*hhmmssus, timespec=ts)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(
                        f"With {hhmmssus} and {ts} an error is "
                        f"not raised, with result {result}.")
            else:
                result = datetime._format_time(*hhmmssus, timespec=ts)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, hhmmssus, ts, result))

    #@unittest.skip("Temporarily skipped")
    def test__format_offset(self):
        """
        Test that the _format_offset returns the correct result.
        """
        err_msg0 = "The off value '{}', must be a timedelta object or None."
        data = (
            (None, False, ''),
            (datetime.timedelta(hours=2), False, '+02:00'),
            (datetime.timedelta(hours=2, minutes=5), False, '+02:05'),
            (datetime.timedelta(hours=2, minutes=5, seconds=30), False,
             '+02:05:30'),
            (datetime.timedelta(
                hours=2, minutes=5, seconds=30, microseconds=5000), False,
             '+02:05:30.005000'),
            (datetime.timedelta(days=-1, hours=2), False, '-22:00'),
            (100, True, err_msg0.format(100)),
            )
        msg = "Expected {} with off {}, found {}."

        for off, validity, expected_result in data:
            if validity:
                try:
                    result = datetime._format_offset(off)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {off} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime._format_offset(off)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, off, result))

    #@unittest.skip("Temporarily skipped")
    def test__check_tzname(self):
        """
        Test that the _check_tzname function raises a TypeError is the name
        argument is not None or a string.
        """
        err_msg0 = "tzinfo.tzname() must return None or string, not {}"
        data = (
            ('', False, ''),
            (None, False, ''),
            (100, True, err_msg0.format(int)),
            )

        for name, validity, expected_result in data:
            if validity:
                try:
                    datetime._check_tzname(name)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    raise AssertionError(
                        f"With {name} an error is not raised.")
            else:
                datetime._check_tzname(name)

    #@unittest.skip("Temporarily skipped")
    def test__fromutc(self):
        """
        Test that the _fromutc function returns an updated datetime object.
        """
        err_msg0 = "_fromutc() requires a datetime argument."
        err_msg1 = "_fromutc() dt.tzinfo is not self"
        #err_msg2 = "_fromutc() requires a non-None utcoffset() result."
        err_msg3 = "_fromutc() requires a non-None dst() result."
        #err_msg4 = ("_fromutc(): dt.dst gave inconsistent results; cannot "
        #            "convert.")
        tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        data = (
            ((181, 14, 11), tz0, 0, False, '0181-14-11T03:30:00+03:30'),
            ((181, 14, 11), tz1, 0, False, '0181-14-11T00:00:00+00:00'),
            ((181, 14, 11), tz2, 0, False, '0181-14-10T19:00:00-05:00'),
            ((), None, 0, True, err_msg0),
            ((181, 14, 9), None, 0, True, err_msg1),
            #((181, 14, 10), datetime.UTC, 0, True, err_msg2),
            ((181, 14, 11), datetime.UTC, 0, True, err_msg3),
            #((181, 14, 12), tz1, 1, True, err_msg4),
            )
        msg = "Expected {} with date {} and timezone {}, found {}."

        for date, tz, fold, validity, expected_result in data:
            if validity:
                dt = (datetime.datetime(*date, tzinfo=tz, fold=fold)
                      if date else None)

                if tz is None and not dt: # err_msg0
                    this = None
                elif tz is None: # err_msg1
                    this = tz2
                else:
                    this = dt.tzinfo

                try:
                    result = datetime._fromutc(this, dt)
                except (TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e), f"Error: {date}")
                else:
                    raise AssertionError(f"With {date} an error is not "
                                         f"raised, with result {result}.")
            else:
                dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
                result = datetime._fromutc(dt.tzinfo, dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, result))

    #@unittest.skip("Temporarily skipped")
    def test__module_name(self):
        """
        Test that the _module_name function returns the module path without
        the base directory.
        """
        data = (
            ((181, 1, 1), '__module__', 'datetime'),
            ((181, 1, 1), '__name__', 'date'),
            ((181, 1, 1, None, None, 12, 30, 30), '__module__', 'datetime'),
            ((181, 1, 1, None, None, 12, 30, 30), '__name__', 'datetime'),
            )
        msg = "Expected {} with date {} and module {}, found {}."

        for date, mod, expected_result in data:
            d_len = len(date)

            if d_len < 6:
                obj = datetime.date(*date)
            else:
                obj = datetime.datetime(*date)

        result = datetime._module_name(getattr(obj.__class__, mod))
        self.assertEqual(expected_result, result, msg.format(
            expected_result, date, mod, result))


class TestBadiDatetime_date(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    @classmethod
    def setUpClass(cls):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        err_msg0 = ("Invalid kull-i-shay {}, it must be in the range "
                    "of [-5, 4].")
        err_msg1 = "Invalid string {} had length of {} for pickle."
        err_msg2 = ("A full short or long form Badi date must be used, found "
                    "{} fields.")
        err_msg3 = ("Failed to encode latin1 string when unpickling a date or "
                    "datetime object. pickle.load(data, encoding='latin1') is "
                    "assumed.")
        data = (
            ((1, 1, 1), False, '0001-01-01'),
            ((1, 1, 1, 1, 1), False, '01-01-01-01-01'),
            ((b'\x073\x01\x01',), False, '0001-01-01'),
            ((b'\x14\x01\x01\x01\x01',), False, '01-01-01-01-01'),
            ((b'\x073\x01\x01\x01',), True, err_msg0.format(-12)),
            ((b'\x14\x01\x01\x01\x01\x01',), True, err_msg1.format(
                b'\x14\x01\x01\x01\x01\x01', 6)),
            ((100,), True, err_msg2.format(1)),
            (('\u2190\x01\x01\x01',), True, err_msg3),
            )
        msg = "Expected {} with value {}, found {}."

        for value, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.date(*value)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.date(*value)
                self.assertEqual(expected_result, str(result),
                                 msg.format(expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test_is_short(self):
        """
        Test that the is_short property properly indicates if the Badi
        date is in the short or long form.
        """
        data = (
            ((181, 9, 16), True),
            ((1, 10, 10, 9, 16), False),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.is_short
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test_fromtimestamp(self):
        """
        Test that the fromtimestamp class method creates an instance of
        date from a POSIX timestamp.
        """
        data = (
            (0, True, '0126-16-02'),
            # Gregorian date (2024, 8, 7) this is definitly (0181, 8, 9)
            (1723057467.0619307, False, '01-10-10-08-08'),
            (1723057467.0619307, True, '0181-08-08'),
            )
        msg = "Expected {} with timestamp {}, found {}."

        for ts, short, expected_result in data:
            result = datetime.date.fromtimestamp(ts, short=short)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, ts, result))

    #@unittest.skip("Temporarily skipped")
    def test_today(self):
        """
        Test that the today class method creates an instance of date
        for today.
        """
        dt_reg = r'badidatetime\.datetime\.date\((?P<date>.+)\)'
        data = (
            (False, 5),
            (True, 3),
            )
        msg = "Expected {}, found {}."

        for short, num in data:
            result = datetime.date.today(short=short)
            date_str = re.search(dt_reg, str(result))

            if date_str:
                date = [int(num.strip())
                        for num in date_str.group('date').split(',')]
                self.assertEqual(len(date), num, msg.format(num, len(date)))
            else:
                self.assertIsNone(date_str, (
                    f"For short {short} and num {num}, could not get a "
                    "value from the regex."))

    #@unittest.skip("Temporarily skipped")
    def test_fromordinal(self):
        """
        Test that the fromordinal class method creates a date instance
        from a date ordinal number.

        local coords (35.5894, -78.7792, -5.0)
        """
        data = (
            (78, False, '-05-18-01-01-01'),
            (78, True, '-1842-01-01'),
            (444, True, '-1841-01-01'),
            (577725, True, '-261-11-09'),
            (577726, True, '-261-11-10'),
            (577735, True, '-261-11-19'),
            (577736, True, '-261-12-01'),
            (639785, True, '-091-09-16'),
            (639786, True, '-091-09-17'),
            (639796, True, '-091-10-08'),
            (639797, True, '-091-10-09'),
            (738964, True, '0181-01-01'), # 0180-19-19
            )
        msg = "Expected {} with ordinal {}, found {}."

        for n, short, expected_result in data:
            result = datetime.date.fromordinal(n, short=short)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, n, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisoformat(self):
        """
        Test that the fromisoformat class method creates a date instance
        from an ISO formatted string.
        """
        err_msg_0 = "fromisoformat: argument must be a string."
        err_msg_1 = ("A time indicator was found, this is invalid for date "
                     "parsing, isoformat string: {}.")
        err_msg_2 = "Invalid isoformat string: {}."
        err_msg_3 = (f"Year is out of range: {{}}, min {datetime.MINYEAR}, "
                     f"max {datetime.MAXYEAR}.")
        data = (
            ('0181-01', False, False, '01-10-10-01-01'),
            ('01810101', False, False, '01-10-10-01-01'),
            ('0181-01-01', False, False, '01-10-10-01-01'),
            ('0181-01-01', True, False, '0181-01-01'),
            # Test error messages.
            (10, False, True, err_msg_0),
            ('0181-01-01T00:00:00', False, True,
             err_msg_1.format("'0181-01-01T00:00:00'")),
            ('', False, True, err_msg_2.format("''")),
            # We only test one error that propigated up from
            # the _parse_isoformat_date function.
            ('-2000-01-01', False, True, err_msg_3.format(-2000)),
            )
        msg = "Expected {} with iso {} and short {}, found {}."

        for iso, short, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.date.fromisoformat(iso, short=short)
                except (TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {iso} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.date.fromisoformat(iso, short=short)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, iso, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisocalendar(self):
        """
        Test that the fromisocalendar class method creates a date instance
        from an ISO calendar date.
        """
        err_msg = "Invalid weekday: {} (range is [1, 7])"
        data = (
            # year, week, day in week
            ((181,   1,    1), False, False, '01-10-09-19-17'),
            ((181,   1,    1), True,  False, '0180-19-17'),
            ((181,  24,    7), True,  False, '0181-09-13'),
            ((181,   1,   10), False, True, err_msg.format(10)),
            )
        msg = "Expected {} with iso {} and short {}, found {}."

        for date, short, validity, expected_result in data:
            if validity:
                with self.assertRaises(AssertionError) as cm:
                    datetime.date.fromisocalendar(*date, short=short)

                message = str(cm.exception)
                self.assertEqual(expected_result, message)
            else:
                result = datetime.date.fromisocalendar(*date, short=short)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, short, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ returns the expected formatted text.
        """
        data = (
            ((181, 9, 16), 'datetime.date(181, 9, 16)'),
            ((1, 10, 10, 9, 16), 'datetime.date(1, 10, 10, 9, 16)'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = repr(d)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__short_from_long_form(self):
        """
        Test that the _short_from_long_form method returns the short form
        Badi date.
        """
        data = (
            ((1, 1, 1, 1, 1), (1, 1, 1, 0, 0, 0, 0)),
            ((1, 10, 10, 10, 12), (181, 10, 12, 0, 0, 0, 0)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d._short_from_long_form()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_ctime(self):
        """
        Test that the ctime method creates a string indicating the date.

        All days before 1752-09-14 in the Gregorian Calendar will seem wrong
        when compaired to the Badi Calendar in UK and the US. This is when
        The Gregorian Calendar was adopted and compinsated 11 days.
        """
        data = (
            # 0001-03-19 Saturday (Kamál -> Monday)
            ((-1842, 1, 1), 'Kamál Bahá  1 00:00:00 -1842'),
            # 1582-10-04 Thursday (Jalál -> Saturday)
            ((-261, 11, 7), 'Jalál Mashíyyat  7 00:00:00 -0261'),
            # 1582-10-15 Monday (Idāl -> Wednesday)
            ((-261, 11, 18), '`Idāl Mashíyyat 18 00:00:00 -0261'),
            # 1700-03-19 Tuesday (Istiqlāl -> Friday)
            ((-143, 1, 1), 'Istiqlāl Bahá  1 00:00:00 -0143'),
            # 1752-09-02 Wednesday (Istiqlāl -> Friday)
            ((-91, 9, 15), "Istiqlāl Asmá' 15 00:00:00 -0091"),
            # 1752-09-14 Thursday
            ((-91, 10, 9), "Istijlāl 'Izzat  9 00:00:00 -0091"),
            # 1800-03-20 Thursday
            ((-43, 1, 1), 'Istijlāl Bahá  1 00:00:00 -0043'),
            # 1817-11-12 Wednesday Birthday of Bahá’u’lláh
            ((-26, 13, 10), '`Idāl Qudrat 10 00:00:00 -0026'),
            # 1825-03-20 Saturday
            ((-18, 1, 1), 'Jamál Bahá  1 00:00:00 -0018'),
            # 1843-03-20 Monday
            ((0, 1, 1), 'Kamál Bahá  1 00:00:00 0000'),
            # 1844-03-19 Tuesday
            ((1, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0001'),
            # 1862-03-20 Thursday
            ((19, 1, 1), 'Istijlāl Bahá  1 00:00:00 0019'),
            # 1881-03-19 Saturday
            ((38, 1, 1), 'Jalál Bahá  1 00:00:00 0038'),
            # 1900-03-20 Tuesday
            ((57, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0057'),
            # 2014-03-20 Thursday
            ((171, 1, 1), 'Istijlāl Bahá  1 00:00:00 0171'),
            # 2024-03-19 Tuesday
            ((181, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0181'),
            # 2024-08-14 Wednesday
            ((181, 8, 16), '`Idāl Kamál 16 00:00:00 0181'),
            # 2024-08-14 Wednesday
            ((1, 10, 10, 8, 16), '`Idāl Kamál 16 00:00:00 0181'),
            # 2024-08-15 Thursday
            ((1, 10, 10, 8, 17), 'Istijlāl Kamál 17 00:00:00 0181'),
            # 2033-03-19 Saturday
            ((190, 1, 1), 'Jalál Bahá  1 00:00:00 0190'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.ctime()
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    @patch('badidatetime._timedateutils.TimeDateUtils.date_format',
           new_callable=PropertyMock)
    def test_strftime(self, mock_property):
        """
        Test that the strftime method returns a formatted date time string.
        We need to mock the date_format property because everywhere is not
        here.
        """
        mock_property.return_value = ['/', 'm', 'd', 'Y']
        data = (
            ((181, 19, 1), '%D', '19/01/81'),
            ((1, 10, 10, 19, 1), '%D', '19/01/81'),
            ((1, 1, 1), '%x', '01/01/0001'),
            ((181, 11, 17), '%c', 'Isq Mas  17 00:00:00 0181'),
            )
        msg = "Expected {} with date {} and format {}, found {}."

        for date, fmt, expected_result in data:
            d = datetime.date(*date)
            result = d.strftime(fmt)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test___format__(self):
        """
        Test that the __format__ method returns a correctly formatted string.
        """
        data = (
            ((181, 11, 17), '', '0181-11-17'),
            ((181, 11, 17), '%D', '11/17/81'),
            )
        msg = "Expected {} with date {} and format {}, found {}."

        for date, fmt, expected_result in data:
            d = datetime.date(*date)
            result = f"{d:{fmt}}"
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoformat(self):
        """
        Test that the isoformat method return the ISO formated version of
        the date represented by this class.
        """
        data = (
            ((181, 1, 1), '0181-01-01'),
            ((1, 10, 10, 8, 15), '0181-08-15'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.isoformat()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___str__(self):
        """
        Test that the __str__ method returns the ISO formated version of
        the date represented by the class object.
        """
        data = (
            ((181, 1, 1), '0181-01-01'),
            ((1, 10, 10, 8, 15), '01-10-10-08-15'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            result = datetime.date(*date)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_kull_i_shay(self):
        """
        Test that the kull_i_shay property returns the kull_i_shay value.
        """
        data = (
            ((1, 10, 10, 1, 1), 1),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.kull_i_shay
            self.assertEqual(expected_result, d.kull_i_shay,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_vahid(self):
        """
        Test that the vahid property returns the vahid value.
        """
        data = (
            ((1, 10, 10, 1, 1), 10),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.vahid
            self.assertEqual(expected_result, d.vahid,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_year(self):
        """
        Test that the year property returns the year value.
        """
        data = (
            ((181, 1, 1), 181),
            ((1, 10, 10, 1, 1), 10),
             )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.year
            self.assertEqual(expected_result, d.year,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_month(self):
        """
        Test that the month property returns the month value.
        """
        data = (
            ((181, 1, 1), 1),
            ((1, 10, 10, 1, 1), 1),
             )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.month
            self.assertEqual(expected_result, d.month,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_day(self):
        """
        Test that the day property returns the day value.
        """
        data = (
            ((181, 1, 1), 1),
            ((1, 10, 10, 1, 1), 1),
             )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.day
            self.assertEqual(expected_result, d.day,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_timetuple(self):
        """
        Test that the timetuple returns the correct long or short form object.
        """
        data = (
            ((181, 9, 6),
             "structures.ShortFormStruct(tm_year=181, tm_mon=9, tm_mday=6, "
             "tm_hour=0, tm_min=0, tm_sec=0, tm_wday=6, tm_yday=158, "
             "tm_isdst=-1)"),
            ((1, 10, 10, 9, 6),
             "structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, "
             "tm_year=10, tm_mon=9, tm_mday=6, tm_hour=0, tm_min=0, tm_sec=0, "
             "tm_wday=6, tm_yday=158, tm_isdst=-1)")
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.timetuple()
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_toordinal(self):
        """
        Test that the toordinal method returns a proleptic Badi ordinal.
        """
        data = (
            ((-1842, 1, 1), 78),
            ((1, 1, 1), 673220),
            ((181, 1, 1), 738964),
            ((181, 8, 15), 739111),
            ((1, 10, 10, 8, 15), 739111),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.toordinal()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_replace(self):
        """
        Test that the replace method returns a new date object with the
        replaced values.
        """
        def execute_replace(date2):
            if short:
                year, month, day = date2
                result = d.replace(year=year, month=month, day=day)
            else:
                kull_i_shay, vahid, year, month, day = date2
                result = d.replace(kull_i_shay=kull_i_shay, vahid=vahid,
                                   year=year, month=month, day=day)

            return result

        err_msg0 = "Cannot convert from a short to a long form date."
        err_msg1 = ("Cannot convert from a long to a short form date. The "
                    "value {} is not valid for long form dates.")
        data = (
            # Normal replace for a short date
            ((181, 1, 1), (182, None, None), True, False, '0182-01-01'),
            ((181, 1, 1), (None, 9, 12), True, False, '0181-09-12'),
            # Normal replace for a long date
            ((1, 10, 10, 1, 1), (None, None, 11, None, None), False, False,
             '01-10-11-01-01'),
            ((1, 10, 10, 1, 1), (None, 9, None, None, None), False, False,
             '01-09-10-01-01'),
            ((1, 10, 10, 1, 1), (None, 9, 10, None, None), False, False,
             '01-09-10-01-01'),
            # Error conditions.
            ((181, 1, 1), (1, 10, None, None, None), False, True, err_msg0),
            ((1, 10, 10, 1, 1), (181, 1, None), True, True,
             err_msg1.format(181)),
            )
        msg = "Expected {} with date1 {}, date2 {}, and short {}, found {}."

        for date1, date2, short, validity, expected_result in data:
            d = datetime.date(*date1)

            if validity:
                try:
                    result = execute_replace(date2)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date1}' an error is not "
                                         f"raised, with result {result}.")
            else:
                result = execute_replace(date2)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date1, date2, short, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns True if equal and False if
        not equal.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), True),
            ((181, 9, 14), (181, 9, 13), False),
            ((181, 9, 14), (181, 9, 15), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 == d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___le__(self):
        """
        Test that the __le__ method returns True if less than or equal and
        False if not less than or equal.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), True),
            ((181, 9, 14), (181, 9, 13), False),
            ((181, 9, 14), (181, 9, 15), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), True),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 <= d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___lt__(self):
        """
        Test that the __lt__ method returns True if less than and False
        if not less than.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), False),
            ((181, 9, 14), (181, 9, 13), False),
            ((181, 9, 14), (181, 9, 15), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), True),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 < d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___ge__(self):
        """
        Test that the __ge__ method returns True if greater than or equal
        and False if not greater than or equal.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), True),
            ((181, 9, 14), (181, 9, 13), True),
            ((181, 9, 14), (181, 9, 15), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 >= d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___gt__(self):
        """
        Test that the __gt__ method returns True if greater than and False
        if not greater than.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), False),
            ((181, 9, 14), (181, 9, 13), True),
            ((181, 9, 14), (181, 9, 15), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), False),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), True),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0 > d1
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns 1 if the two dates are equal, +1
        if the current date is greater than the test date, and -1 if the
        inverse.
        """
        data = (
            ((181, 9, 14), (181, 9, 14), 0),
            ((181, 9, 14), (181, 9, 13), 1),
            ((181, 9, 14), (181, 9, 15), -1),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 14), 0),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 13), 1),
            ((1, 10, 10, 9, 14), (1, 10, 10, 9, 15), -1),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, date1, expected_result in data:
            d0 = datetime.date(*date0)
            d1 = datetime.date(*date1)
            result = d0._cmp(d1)
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns a valid hash for both short
        and long form dates.
        """
        data = (
            (datetime.MINYEAR, 1, 1),
            (-5, 18, 1, 1, 1),
            (1, 1, 1),
            (1, 1, 1, 1, 1),
            )
        msg = "date {}, found {}."

        for date in data:
            d = datetime.date(*date)
            result = hash(d)
            self.assertTrue(len(str(result)) > 15, msg.format(date, result))

    #@unittest.skip("Temporarily skipped")
    def test___add__(self):
        """
        Test that the __add__ method can correctly add a date to a timedelta.
        """
        err_msg0 = "unsupported operand type(s) for +: 'date' and '{}'"
        err_msg1 = "Result out of range."
        data = (
            ((1, 1, 1), (1,), False, (1, 1, 2)),
            ((1, 1, 1), (366,), False, (2, 1, 1)),             # Leap year
            ((181, 1, 1), (365,), False, (182, 1, 1)),         # Non leap year
            ((1, 1, 1, 1, 1), (366,), False, (1, 1, 2, 1, 1)), # Leap year
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            ((-1842, 1, 1), (-1,), True, err_msg1),
            ((1161, 19, 19), (1,), True, err_msg1),
            )
        msg = "Expected {} with date {} and timedelta {}, found {}"

        for date, td, validity, expected_result in data:
            d0 = datetime.date(*date)

            if validity:
                if isinstance(td, tuple):
                    td0 = datetime.timedelta(*td)
                else:
                    td0 = td

                try:
                    result = d0 + td0
                except (OverflowError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                td0 = datetime.timedelta(*td)
                d1 = d0 + td0

                if d1.is_short:
                    result = (d1.year, d1.month, d1.day)
                else:
                    result = (d1.kull_i_shay, d1.vahid, d1.year,
                              d1.month, d1.day)

                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, td, result))

    #@unittest.skip("Temporarily skipped")
    def test___sub__(self):
        """
        Test that the __sub__ method returns the correct results of a
        timedelta object subtracted from a date object.
        """
        err_msg0 = "unsupported operand type(s) for -: 'date' and '{}'"
        data = (
            ((1, 1, 1), 1, False, (0, 19, 19)),
            ((2, 1, 1), 366, False, (1, 1, 1)),     # Leap year
            ((181, 1, 1), 365, False, (180, 1, 1)), # Non Leap year
            ((182, 1, 1), (181, 1, 1), False, (365, 0, 0)),
            ((1, 1, 1), None, True, err_msg0.format('NoneType')),
            )
        msg = "Expected {} with date {} and value {}, found {}"

        for date, value, validity, expected_result in data:
            d0 = datetime.date(*date)

            if validity:
                try:
                    result = d0 - value
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                if isinstance(value, int):
                    dt = datetime.timedelta(value)
                    d1 = d0 - dt
                    result = (d1.year, d1.month, d1.day)
                else:
                    d1 = datetime.date(*value)
                    dt = d0 - d1
                    result = (dt.days, dt.seconds, dt.microseconds)

                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, value, result))

    #@unittest.skip("Temporarily skipped")
    def test_weekday(self):
        """
        Test that the weekday method returns the correct weekday number.
        """
        data = (
            ((181, 1, 1), 3),  # Fiḍāl
            ((181, 10, 8), 6), # Istiqlāl
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.weekday()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoweekday(self):
        """
        Test that the weekday method returns the correct weekday number.
        """
        data = (
            ((181, 1, 1), 4),  # Fiḍāl -> Tuesday
            ((181, 10, 8), 7), # Istiqlāl -> Friday
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d.isoweekday()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_isocalendar(self):
        """
        Test that the isocalendar method the correct ISO Calendar tuple.
        """
        data = (
            ((181, 1, 1), (181, 1, 4)),       # Short form
            ((1, 10, 10, 1, 1), (181, 1, 4)), # Long form
            ((181, 0, 1), (181, 50, 3)),      # 0 < week < 53
            ((181, 19, 19), (181, 53, 4)),    # 0 < week < 53
            ((182, 1, 1), (181, 53, 5)),   # Week >= 52 starts in previous year
            ((183, 19, 19), (183, 52, 7)), # Week >= 52 starts in previous year
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = tuple(d.isocalendar())
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__is_pickle_data(self):
        """
        Test that the _is_pickle_data classmethod returns the correct results
        depending on the incoming data.
        """
        err_msg0 = "Invalid string {} had length of {} for pickle."
        err_msg1 = ("Failed to encode latin1 string when unpickling a date or "
                    "datetime object. pickle.load(data, encoding='latin1') is "
                    "assumed.")
        data = (
            ((b'\x073\x01\x01', None), False, True),
            ((b'\x14\x01\x01\x01\x01', None), False, False),
            ((181, 10), False, None),
            ((b'\x073\x20\x01', None), False, None),
            ((b'\x14\x01\x01\x14\x01', None), False, None),
            ((b'\x14\x01\x01\x01\x01\x01', None), True, err_msg0.format(
                b'\x14\x01\x01\x01\x01\x01', 6)),
            (('\u2190\x01\x01\x01', None), True, err_msg1),
            )
        msg = "Expected {} with value {}, found {}."

        for value, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.date._is_pickle_data(*value)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.date._is_pickle_data(*value)
                self.assertEqual(expected_result, result,
                                 msg.format(expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the state of the class.
        """
        data = (
            ((datetime.MINYEAR, 1, 1), (b'\x00\x00\x01\x01',)),
            ((-5, 18, 1, 1, 1), (b'\x0e\x12\x01\x01\x01',)),
            ((1, 1, 1), (b'\x073\x01\x01',)),
            ((1, 1, 1, 1, 1), (b'\x14\x01\x01\x01\x01',)),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime.date(*date)
            result = d._getstate()
            self.assertEqual(expected_result, result,
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___setstate(self):
        """
        Test that the __setstate method sets the date properly.
        """
        data = (
            ((datetime.MINYEAR, 1, 1), b'\x00\x00\x01\x01'),
            ((-5, 18, 1, 1, 1), b'\x0e\x12\x01\x01\x01'),
            ((1, 1, 1), b'\x073\x01\x01'),
            ((1, 1, 1, 1, 1), b'\x14\x01\x01\x01\x01'),
            )
        msg = "Expected {} with bytes_str {}, found {}."

        for date, bytes_str in data:
            d = datetime.date(*date)
            d._date__setstate(bytes_str)

            if len(date) == 3:
                result = (d._year, d._month, d._day)
            else:
                result = (d._kull_i_shay, d._vahid, d._year, d._month, d._day)

            self.assertEqual(date, result, msg.format(date, bytes_str, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce__(self):
        """
        Test that the __reduce__ method works for both short and long
        form Badi dates.
        """
        data = (
            (datetime.MINYEAR, 1, 1),
            (1, 1, 1),
            (1, 1, 1, 1, 1),
            (datetime.MAXYEAR, 1, 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date in data:
            date0 = datetime.date(*date)
            obj = pickle.dumps(date0)
            date1 = pickle.loads(obj)

            if len(date) == 3:
                b_date0 = (date0._year, date0._month, date0._day)
                b_date1 = (date1._year, date1._month, date1._day)
            else:
                b_date0 = (date0._kull_i_shay, date0._vahid,
                           date0._year, date0._month, date0._day)
                b_date1 = (date1._kull_i_shay, date1._vahid,
                           date1._year, date1._month, date1._day)

            self.assertEqual(b_date0, b_date1, msg.format(
                b_date0, date, b_date1))


class TestBadiDatetime__IsoCalendarDate(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test_year(self):
        """
        Test that the year property returns the year.
        """
        data = (
            # year, week, weekday
            ((1,    1,    1), 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = d.year
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_week(self):
        """
        Test that the week property returns the week.
        """
        data = (
            # year, week, weekday
            ((1,    1,    1), 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = d.week
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_weekday(self):
        """
        Test that the weekday property returns the weekday.
        """
        data = (
            # year, week, weekday
            ((1,    1,    1), 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = d.weekday
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce__(self):
        """
        Test that the __reduce__ method works for both short and long
        form Badi dates.
        """
        data = (
            (datetime.MINYEAR, 1, 1),
            (1, 1, 1),
            (datetime.MAXYEAR, 1, 1),
            )
        msg = "Expected {}, with date {}, found {}"

        for date in data:
            date0 = datetime._IsoCalendarDate(*date)
            obj = pickle.dumps(date0)
            date1 = pickle.loads(obj)
            b_date0 = (date0.year, date0.week, date0.weekday)
            b_date1 = (date1[0], date1[1], date1[2])
            self.assertEqual(b_date0, b_date1, msg.format(
                b_date0, date, b_date1))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ returns the expected formatted text.
        """
        data = (
            ((181, 9, 16), '_IsoCalendarDate(year=181, week=9, weekday=16)'),
            ((1, 1, 1), '_IsoCalendarDate(year=1, week=1, weekday=1)'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            d = datetime._IsoCalendarDate(*date)
            result = repr(d)
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))


class TestBadiDatetime_time(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    @classmethod
    def setUpClass(cls):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        err_msg0 = ("Failed to encode latin1 string when unpickling a time "
                    "object. pickle.load(data, encoding='latin1') is assumed.")
        data = (
            ((12, 30), None, 0, False, '12:30:00'),
            ((12, 30, 30), None, 0, False, '12:30:30'),
            ((12, 30, 30, 10), None, 0, False, '12:30:30.000010'),
            ((12, 30, 30, 50000), datetime.BADI, 0, False,
             '12:30:30.050000+03:30'),
            ((12, 30, 30, 50000), None, 0, False, '12:30:30.050000'),
            # Test errors when picking
            (('\u2190\x0c\x1e\x1e\x07\xa1', None), None, 0, True, err_msg0),
            )
        msg = "Expected {} with value {}, found {}."

        for value, tz, fold, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.time(*value, tzinfo=tz, fold=fold)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.time(*value, tzinfo=tz, fold=fold)
                self.assertEqual(expected_result, str(result),
                                 msg.format(expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test_hour(self):
        """
        Test that the hour property returns the correct value.
        """
        data = (
            ((12, 30), 12),
            ((24, 0), 24),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.hour
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_minute(self):
        """
        Test that the minute returns the correct value.
        """
        data = (
            ((12, 30), 30),
            ((24, 10), 10),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.minute
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_second(self):
        """
        Test that the second returns the correct value.
        """
        data = (
            ((12, 30, 30), 30),
            ((24, 10, 20), 20),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.second
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_microsecond(self):
        """
        Test that the microsecond returns the correct value.
        """
        data = (
            ((12, 30, 30, 1), 1),
            ((24, 10, 20, 999999), 999999),
            )
        msg = "Expected {} with time {}, found {}."

        for time, expected_result in data:
            td = datetime.time(*time)
            result = td.microsecond
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_tzinfo(self):
        """
        Test that the tzinfo returns the correct value.
        """
        data = (
            ((12, 30, 30), datetime.BADI, 'UTC+03:30'),
            ((24, 10, 20), datetime.UTC, 'UTC'),
            )
        msg = "Expected {} with time {}, found {}."

        for time, tz, expected_result in data:
            td = datetime.time(*time, tzinfo=tz)
            result = td.tzinfo
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test_fold(self):
        """
        Test that the fold returns the correct value.
        """
        data = (
            ((12, 30, 30, 1), 0, 0),
            ((24, 10, 20, 999999), 1, 1),
            )
        msg = "Expected {} with time {}, found {}."

        for time, fold, expected_result in data:
            td = datetime.time(*time, fold=fold)
            result = td.fold
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, str(result)))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns  True if equal and False if
        not equal.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), True),
            ((12, 30, 30), (12, 30, 29), False),
            ((12, 30, 30), (12, 30, 31), False),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 == t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___le__(self):
        """
        Test that the __le__ method returns  True if less than or equal and
        False if not less than or equal.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), True),
            ((12, 30, 30), (12, 30, 29), False),
            ((12, 30, 30), (12, 30, 31), True),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 <= t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___lt__(self):
        """
        Test that the __lt__ method returns True if less than and False
        if not less than.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), False),
            ((12, 30, 30), (12, 30, 29), False),
            ((12, 30, 30), (12, 30, 31), True),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 < t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___ge__(self):
        """
        Test that the __ge__ method returns True if greater than or equal
        and False if not greater than or equal.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), True),
            ((12, 30, 30), (12, 30, 29), True),
            ((12, 30, 30), (12, 30, 31), False),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 >= t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___gt__(self):
        """
        Test that the __gt__ method returns True if greater than and False
        if not greater than.
        """
        data = (
            ((12, 30, 30, 10000), (12, 30, 30, 10000), False),
            ((12, 30, 30), (12, 30, 29), True),
            ((12, 30, 30), (12, 30, 31), False),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for time0, time1, expected_result in data:
            t0 = datetime.time(*time0)
            t1 = datetime.time(*time1)
            result = t0 > t1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test__cmp(self):
        """
        Test that the _cmp method returns 1 if the two times are equal, +1
        if the current time is greater than the test time, and -1 if the
        inverse.
        """
        err_msg0 = "Invalid time module, found {}."
        err_msg1 = "Cannot compare naive and aware times."
        data = (
            ((12, 30, 30, 10000), None, 0, True,
             (12, 30, 30, 10000), None, 0, False, 0),
            ((12, 30, 30), None, 0, False, (12, 30, 29), None, 0, False, 1),
            ((12, 30, 30), None, 0, False, (12, 30, 31), None, 0, False, -1),
            ((12, 30, 30, 10000), datetime.BADI, 0, True,
             (12, 30, 30, 10000), datetime.BADI, 0, False, 0),
            # Timezone infos are different
            ((12, 30, 30, 10000), datetime.BADI, 0, False,
             (12, 30, 30, 10000), datetime.UTC, 0, False, -1),
            ((12, 30, 30, 10000), datetime.UTC, 0, False,
             (12, 30, 30, 10000), datetime.BADI, 0, False, 1),
            # If allow_mixed is True and timezones are different.
            ((12, 30, 30, 10000), datetime.BADI, 0, True,
             (12, 30, 30, 10000), None, 0, False, 2),
            # Errors
            ((12, 30, 30, 10000), None, 0, False,
             (), None, 0, True, err_msg0.format(None)),
            ((12, 30, 30, 10000), datetime.BADI, 0, False,
             (12, 30, 30, 10000), None, 0, True, err_msg1),
            )
        msg = "Expected {} with time0 {} and time1 {}, found {}."

        for (time0, tz0, fold0, am, time1, tz1, fold1,
             validity, expected_result) in data:
            t0 = datetime.time(*time0, tzinfo=tz0, fold=fold0)

            if validity:
                try:
                    if time1 == ():
                        t1 = None
                    else:
                        t1 = datetime.time(*time1, tzinfo=tz1, fold=fold1)

                    result = t0._cmp(t1, allow_mixed=am)
                except (AssertionError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time0} an error is not "
                                         f"raised, with result {result}.")
            else:
                t1 = datetime.time(*time1, tzinfo=tz1, fold=fold1)
                result = t0._cmp(t1, allow_mixed=am)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, time0, time1, result))

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns the proper hash of the class.
        """
        data = (
            ((1, 30, 30), None, 0),
            ((1, 30, 30), None, 1),
            ((1, 30, 30, 500000), None, 0),
            ((3, 30), datetime.BADI, 0),
            # Test for negative hours.
            ((0, 30), datetime.BADI, 0),
            )
        msg = "time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = str(hash(t))
            self.assertTrue(len(result) > 15, msg.format(
                time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test__tzstr(self):
        """
        Test that the _tzstr method returns a formatted timezone offset.
        """
        data = (
            ((1, 30, 30, 500000), datetime.BADI, 0, '+03:30'),
            # *** TODO *** No timezone yet
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t._tzstr()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ method returns the correct string.
        """
        data = (
            ((0, 0, 0, 0), None, 0, 'datetime.time(0, 0)'),
            ((1, 30), None, 0, 'datetime.time(1, 30)'),
            ((1, 30, 30), None, 0, 'datetime.time(1, 30, 30)'),
            ((1, 30, 30, 50000), None, 0, 'datetime.time(1, 30, 30, 50000)'),
            ((1, 30, 30, 50000), datetime.BADI, 0,
             'datetime.time(1, 30, 30, 50000, tzinfo=UTC+03:30)'),
            ((1, 30, 30, 50000), datetime.BADI, 1,
             'datetime.time(1, 30, 30, 50000, tzinfo=UTC+03:30, fold=1)'),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            d = datetime.time(*time, tzinfo=tz, fold=fold)
            result = repr(d)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoformat(self):
        """
        Test that the isoformat method an ISO formatted string.
        """
        data = (
            ((1, 30, 30), None, 0, '01:30:30'),
            ((1, 30, 30, 500000), None, 0, '01:30:30.500000'),
            # *** TODO *** Needs to be tested with a timezone.
            #((1, 30, 30, 500000), datetime.BADI, 0, '01:30:30.500000'),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.isoformat()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisoformat(self):
        """
        Test that the fromisoformat classmethod returns a correctly
        formatted ISO time string.
        """
        err_msg0 = ("Invalid isoformat string: {}, Invalid character {} in "
                    "incoming time string.")
        err_msg1 = ("Invalid isoformat string: {}, Cannot have both a 'T' "
                    "and a space or more than one of either to indicate time.")
        err_msg2 = ("Invalid isoformat string: {}, Invalid time string, 1st "
                    "character must be one of ( T), found {}")
        err_msg3 = ("Invalid isoformat string: {}, Invalid number of colons "
                    "(:), can be 0 - 2, found {}")
        err_msg4 = ("Invalid isoformat string: {}, Invalid number of dots "
                    "(.), can be 0 - 1, found {}")
        err_msg5 = "Invalid isoformat string: {}, Invalid time string, found {}"
        err_msg6 = "fromisoformat: argument must be str"
        data = (
            ('T12', False, '12:00:00'),
            ('T12.5', False, '12:30:00'),
            ('T12:30', False, '12:30:00'),
            ('T12:30.5', False, '12:30:30'),
            ('T1230', False, '12:30:00'),
            ('T1230.5', False, '12:30:30'),
            (' 12:30', False, '12:30:00'),
            (' 12:30.5', False, '12:30:30'),
            (' 1230', False, '12:30:00'),
            (' 1230.5', False, '12:30:30'),
            ('T12:30:30', False, '12:30:30'),
            ('T12:30:30.5', False, '12:30:30.5'),
            ('T123030', False, '12:30:30'),
            ('T123030.5', False, '12:30:30.5'),
            # Error conditions
            ('abcdefg', True, err_msg0.format("'abcdefg'", "'a'")),
            (' T', True, err_msg1.format("' T'")),
            ('1230.5', True, err_msg2.format("'1230.5'", "'1230.5'")),
            ('T:::', True, err_msg3.format("'T:::'", 3)),
            ('T..', True, err_msg4.format("'T..'", 2)),
            ('T014.2', True, err_msg5.format("'T014.2'", "'T014.2'")),
            (10, True, err_msg6),
            )
        msg = "Expected {} with format {}, found {}."

        for iso, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.time.fromisoformat(iso)
                except (ValueError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {iso} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.time.fromisoformat(iso)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, iso, result))

    #@unittest.skip("Temporarily skipped")
    def test_strftime(self):
        """
        Test that the strftime method returns a correctly formatting string.
        """
        data = (
            ((1, 30, 30), '%X', '01:30:30'),
            ((1, 30, 30), '%r', '01:30:30 AM'),
            ((1, 30, 30), '%c', 'Jal Bah  1 01:30:30 0001'),
            ((1, 30, 30, 500000), '%T.%f', '01:30:30.500000'),
            ((1, 30, 30, 500000), 'T%H:%M:%S.%f', 'T01:30:30.500000'),
            )
        msg = "Expected {} with time {} and format {}, found {}."

        for time, fmt, expected_result in data:
            result = datetime.time(*time).strftime(fmt)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test___format__(self):
        """
        Test that the __format__ method returns a correctly formatting string.
        """
        err_msg0 = "Must be a str, not {}."
        data = (
            ((1, 30, 30), '', False, '01:30:30'),
            ((1, 30, 30), '%X', False, '01:30:30'),
            ((1, 30, 30, 500000), '%T.%f', False, '01:30:30.500000'),
            ((1, 30, 30, 500000), 'T%H:%M:%S.%f', False, 'T01:30:30.500000'),
            ((1, 30, 30, 500000), 10, True, err_msg0.format('int')),
            )
        msg = "Expected {} with time {} and format {}, found {}."

        for time, fmt, validity, expected_result in data:
            t = datetime.time(*time)

            if validity:
                try:
                    result = t.__format__(fmt)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = t.__format__(fmt)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, time, fmt, result))

    #@unittest.skip("Temporarily skipped")
    def test_utcoffset(self):
        """
        Test that the utcoffset method returns the correct timezone offset.
        """
        tz0 = ZoneInfo('US/Eastern')
        data = (
            ((1, 30, 30, 500000), None, 0, 'None'),
            ((1, 30, 30, 500000), datetime.BADI, 0, '3:30:00'),
            ((1, 30, 30, 500000), tz0, 0, 'None'),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.utcoffset()
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_badioffset(self):
        """
        Test that the badioffset
        """
        tz0 = ZoneInfo('US/Eastern')
        data = (
            ((1, 30, 30, 500000), None, 0, 'None'),
            ((1, 30, 30, 500000), datetime.BADI, 0, '0:00:00'),
            ((1, 30, 30, 500000), tz0, 0, 'None'),
            ((1, 30, 30, 500000), datetime.UTC, 0, '-1 day, 20:30:00'),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.badioffset()
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_tzname(self):
        """
        Test that the tzname method returns the timezone name.
        """
        tz0 = ZoneInfo('US/Eastern')
        data = (
            ((12, 30, 30), None, 0, None),
            ((1, 30, 30, 500000), datetime.BADI, 0, 'UTC+03:30'),
            ((1, 30, 30, 500000), tz0, 0, None),
            ((1, 30, 30, 500000), datetime.UTC, 0, 'UTC'),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.tzname()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_dst(self):
        """
        Test that the dst method returns either 0 or 1 if DST is in effect.
        """
        tz0 = ZoneInfo('US/Eastern')
        data = (
            ((12, 30, 30), None, 0, None),
            ((1, 30, 30, 500000), datetime.BADI, 0, None),
            ((1, 30, 30, 500000), tz0, 0, None),
            ((1, 30, 30, 500000), datetime.UTC, 0, None),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t.dst()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_replace(self):
        """
        Test that the replace
        """
        data = (
            ((12, 30, 30, 500000), None, 0,
             (6, None), None, None, '06:30:30.500000'),
            ((12, 30, 30, 500000), None, 0,
             (None, 15), None, None, '12:15:30.500000'),
            ((12, 30, 30, 500000), None, 0,
             (None, None, 15), None, None, '12:30:15.500000'),
            ((12, 30, 30, 500000), None, 0,
             (None, None, None, 999999), None, None, '12:30:30.999999'),
            ((12, 30, 30, 500000), None, 0,
             (), datetime.BADI, None, '12:30:30.500000+03:30'),
            ((12, 30, 30, 500000), None, 0, (), None, 1, '12:30:30.500000'),
            )
        msg = "Expected {} with time0 {}, and time1 {}, found {} "

        for time0, tz0, fold0, time1, tz1, fold1, expected_result in data:
            t = datetime.time(*time0, tzinfo=tz0, fold=fold0)
            result = t.replace(*time1, tzinfo=tz1, fold=fold1)
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, time0, time1, str(result)))

            if tz0 != tz1:
                self.assertEqual(tz1, result.tzinfo, msg.format(
                    expected_result, time0, time1, result.tzinfo))

            if fold1 is not None and fold0 != fold1:
                self.assertEqual(fold1, result.fold, msg.format(
                    expected_result, time0, time1, result.fold))

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the state of the class.
        """
        data = (
            ((12, 30, 30, 500000), None, 0, r"(b'\x0c\x1e\x1e\x07\xa1 ',)"),
            ((24, 30, 30, 500000), None, 0, r"(b'\x18\x1e\x1e\x07\xa1 ',)"),
            ((12, 30, 30, 500000), datetime.BADI, 0,
             r"(b'\x0c\x1e\x1e\x07\xa1 ', datetime.BADI)"),
            )
        msg = "Expected {} with time {}, timezone {}, and fold {}, found {}."

        for time, tz, fold, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)
            result = t._getstate()
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, time, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test___setstate(self):
        """
        Test that the __setstate method sets the correct state for pickeling.
        """
        err_msg0 = ("tzinfo argument must be None or of a tzinfo subclass, "
                    "found {}")
        data = (
            ((12, 30, 30, 500000), None, 0, b'\x0c\x1e\x1e\x07\xa1 ',
             False, ''),
            ((24, 30, 30, 500000), None, 0, b'\x18\x1e\x1e\x07\xa1 ',
             False, ''),
            ((0, 30, 30, 500000), None, 1, b'\x80\x1e\x1e\x07\xa1 ',
             False, ''),
            ((12, 30, 30, 500000), None, 0, b'', True, err_msg0.format("''")),
            )
        msg = ("Expected {} with time {}, tz {}, fold {}, "
               "and bytes_str {}, found {}.")

        for time, tz, fold, bytes_str, validity, expected_result in data:
            t = datetime.time(*time, tzinfo=tz, fold=fold)

            if validity:
                try:
                    result = t._time__setstate(bytes_str, '')
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                t._time__setstate(bytes_str, tz)
                result = (t.hour, t.minute, t.second,
                          t.microsecond, t.tzinfo, t.fold)
                expected_result = time + (tz, fold)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, time, tz, fold, bytes_str, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce_ex__(self):
        """
        Test that the __reduce_ex__ method creates the correct pickle value
        for protocol 3.
        """
        data = (
            ((12, 30, 30, 500000), datetime.BADI, 0),
            ((12, 30, 30, 500000), datetime.UTC, 1),
            )
        msg = "Expected {}, with time {}, found {}"

        for time, tz, fold in data:
            t0 = datetime.time(*time, tzinfo=tz, fold=fold)
            obj = pickle.dumps(t0)
            t1 = pickle.loads(obj)
            t0_result = (t0.hour, t0.minute, t0.second, t0.microsecond,
                         t0.tzinfo, t0.fold)
            t1_result = (t1.hour, t1.minute, t1.second, t1.microsecond,
                         t1.tzinfo, t1.fold)
            self.assertEqual(t0_result, t1_result, msg.format(
                t0_result, time, tz, fold, t1_result))


class TestBadiDatetime_datetime(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)
        self._time_fields = ('hour', 'minute', 'second', 'microsecond')

    @classmethod
    def setUpClass(cls):
        enable_geocoder()
        datetime.LOCAL = datetime.timezone.local = datetime.timezone._create(
            datetime.timedelta(hours=-5.0))

    def _get_time(self, time):
        t_len = len(time)
        hh = time[0] if t_len > 0 else 0
        mm = time[1] if t_len > 1 else 0
        ss = time[2] if t_len > 2 else 0
        us = time[3] if t_len > 3 else 0
        return hh, mm, ss, us

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        # err_msg0 = ("A full short or long form Badi date must be used, found "
        #             "{} fields.")
        err_msg1 = ("A fractional value cannot be followed by a less "
                    "significant value.")
        data = (
            ((1, 1, 1, None, None, 12, 30, 30), None, 0, False,
             '0001-01-01T12:30:30'),
            ((1, 1, 1, None, None, 12, 30, 30, 500000), None, 0, False,
             '0001-01-01T12:30:30.500000'),
            ((1, 1, 1, 1, 1), None, 0, False, '01-01-01-01-01T00:00:00'),
            # Test _create_time
            ((181, 1, 1, None, None, 12.123456789), None, 0, False,
             '0181-01-01T12:07:24.444420'),
            ((181, 1, 1, None, None, 12, 30.123456789), None, 0, False,
             '0181-01-01T12:30:07.407407'),
            ((181, 1, 1, None, None, 12, 30, 30.123456789), None, 0, False,
             '0181-01-01T12:30:30.123457'),
            # Test pickling
            # Short form
            ((b'\x00\x00\x01\x01\x0c\x1e\x1e\x07\xa1 ',), None, 0, False,
             '-1842-01-01T12:30:30.500000'),
            # Long form
            ((b'\x0e\x12\x01\x01\x01\x0c\x1e\x1e\x07\xa1 ',), None, 0, False,
             '-05-18-01-01-01T12:30:30.500000'),
            # Short form
            ((b'\x073\x01\x01\x00\x00\x00\x00\x00\x00', datetime.BADI),
             datetime.timezone.badi, 0, False, '0001-01-01T00:00:00+03:30'),
            # Long form
            ((b'\x14\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00', datetime.BADI),
             datetime.timezone.badi, 0, False, '01-01-01-01-01T00:00:00+03:30'),
            ((181, 1, 1, None, None, 12.123, 30.5), None, 0, True, err_msg1),
            ((181, 1, 1, None, None, 12.123, 0, 0.5), None, 0, True, err_msg1),
            ((181, 1, 1, None, None, 12, 30.5, 30.5), None, 0, True, err_msg1),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.datetime(*date, tzinfo=tz, fold=fold)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {date} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.datetime(*date, tzinfo=tz, fold=fold)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_hour(self):
        """
        Test that the hour property returns the correct value.
        """
        data = (
            ((1, 1, 1, None, None, 12, 30, 30), None, 0, 12),
            ((1, 1, 1, 1, 1, 12, 30, 30), None, 0, 12)
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.hour
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_minute(self):
        """
        Test that the minute returns the correct value.
        """
        data = (
            ((1, 1, 1, None, None, 12, 30, 30), None, 0, 30),
            ((1, 1, 1, 1, 1, 12, 30, 30), None, 0, 30)
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.minute
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_second(self):
        """
        Test that the second returns the correct value.
        """
        data = (
            ((1, 1, 1, None, None, 12, 30, 30), None, 0, 30),
            ((1, 1, 1, 1, 1, 12, 30, 30), None, 0, 30)
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.second
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_microsecond(self):
        """
        Test that the microsecond returns the correct value.
        """
        data = (
            ((1, 1, 1, None, None, 12, 30, 30, 500000), None, 0, 500000),
            ((1, 1, 1, 1, 1, 12, 30, 30, 999999), None, 0, 999999)
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.microsecond
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_tzinfo(self):
        """
        Test that the tzinfo returns the correct value.
        """
        data = (
            ((1, 1, 1, None, None, 12, 30, 30), None, 0, None),
            ((1, 1, 1, 1, 1, 12, 30, 30), datetime.BADI, 0, datetime.BADI)
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.tzinfo
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_fold(self):
        """
        Test that the fold returns the correct value.
        """
        data = (
            ((1, 1, 1, None, None, 12, 30, 30), None, 0, 0),
            ((1, 1, 1, 1, 1, 12, 30, 30), None, 1, 1)
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.fold
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, tz, fold, str(result)))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test__fromtimestamp(self):
        """
        Test that the _fromtimestamp classmethod creates an instance
        of datetime.

        Note: The tests marked 'Latitude and Longitude dependent' will break
              if datetime.LOCAL_COORD is not patched.
        """
        tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        data = (
            # Latitude and Longitude dependent
            # 1969-12-31T19:00:00+00:00 -> 0126-16-02T01:46:33.168000+00:00
            (-18000, False, tz1, True, '0126-16-02T01:50:45.340800+00:00'),
            # Assume UTC as starting point.
            (0, True, tz1, True, '0126-16-02T08:03:07.718400+00:00'),
            # Latitude and Longitude dependent
            # Assume local time as starting point.
            (-18000, False, tz0, True, '0126-16-02T05:20:45.340800+03:30'),
            # Assume UTC as starting point.
            (0, True, tz0, True, '0126-16-02T11:33:07.718400+03:30'),
            # Latitude and Longitude dependent
            # Assume local time as starting point.
            (-18000, False, tz2, True, '0126-16-01T20:50:45.340800-05:00'),
            )
        msg = ("Expected {} with timestamp {}, utc {}, timezone {}, "
               "and short {}, found {}.")

        for t, utc, tz, short, expected_result in data:
            result = datetime.datetime._fromtimestamp(t, utc, tz, short=short)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, t, utc, tz, short, result))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test_fromtimestamp(self):
        """
        Test that the fromtimestamp classmethod creates an instance
        of datetime.

        Note: The tests marked 'Latitude and Longitude dependent' will break
              if datetime.LOCAL_COORD is not patched.
        """
        tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        data = (
            # Latitude and Longitude dependent
            # Assume local time as starting point.
            # 1970-01-01 -> Badi date and time relative to naive local time.
            (0, None, True, '0126-16-02T06:50:45.340800'),
            # Assume UTC as starting point.
            # 1970-01-01 -> Badi date and time relative to UTC
            (0, tz1, True, '0126-16-02T08:03:07.718400+00:00'),
            # Assume UTC as starting point.
            # 1970-01-01 -> Badi date and time relative to +03:30
            (0, tz0, True, '0126-16-02T11:33:07.718400+03:30'),
            # Local time (2024, 11, 30, 20, 24, 13, 327577)
            # *** TODO *** There is a problem with the two results below, both
            # should be the same. Checked with tz is correct. This could be
            # that only God knows what coordinates are used for IANA time
            # zones to arrive at the correct time. Off by 03:54:10.915200 hrs.
            # Latitude and Longitude dependent
            (1733016253.327577, None, True, '0181-14-10T08:21:17.532000'),
            (1733016253.327577, tz2, True, '0181-14-10T04:27:06.616800-05:00'),
            # Some long form datetimes.
            # Latitude and Longitude dependent
            (0, None, False, '01-07-12-16-02T06:50:45.340800'),
            (0, tz1, False, '01-07-12-16-02T08:03:07.718400+00:00'),
            (0, tz0, False, '01-07-12-16-02T11:33:07.718400+03:30'),
            )
        msg = ("Expected {} with timestamp {}, timezone {}, and short {}, "
               "found {}.")

        for t, tz, short, expected_result in data:
            result = datetime.datetime.fromtimestamp(t, tz, short=short)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, t, tz, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_now(self):
        """
        Test that the now classmethod creates an instance of datetime. It is
        not possible to test actual dates and time as they will change every
        second.
        """
        tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        data = (
            (None, True, 11),
            (tz0, True, 11),
            (tz1, True, 11),
            (tz2, True, 11),
            (None, False, 13),
            (tz0, False, 13),
            (tz1, False, 13),
            (tz2, False, 13),
            )
        msg = "Expected {} with timezone {} and short {}, found {}."

        for tz, short, expected_result in data:
            dt = datetime.datetime.now(tz=tz, short=short)
            ttup = dt.timetuple()
            result = len(ttup)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, tz, short, result))

    #@unittest.skip("Temporarily skipped")
    def test_combine(self):
        """
        Test that the combine classmethod creates an instance of datetime
        from an instance of a date and time object.
        """
        err_msg0 = "The date argument must be a date instance, found {}."
        err_msg1 = "The time argument must be a time instance, found {}."
        data = (
            ((181, 1, 1), (12, 30, 30), True, False, '0181-01-01T12:30:30'),
            ((1, 10, 10, 1, 1), (12, 30, 30), True, False,
             '01-10-10-01-01T12:30:30'),
            ((181, 13, 3), (12, 30, 30, 500000), True, False,
             '0181-13-03T12:30:30.500000'),
            ((181, 1, 1), (12, 30, 30), datetime.BADI, False,
             '0181-01-01T12:30:30+03:30'),
            ((), (12, 30, 30), True, True, err_msg0.format('None')),
            ((181, 1, 1), (), True, True, err_msg1.format('None')),
            )
        msg = "Expected {} with date {}, time {}, and timezone {}, found {}."

        for date, time, tz, validity, expected_result in data:
            if validity:
                if date == ():
                    d = None
                    t = datetime.time(*time,
                                      tzinfo=tz if tz is not True else None)
                elif time == ():
                    d = datetime.date(*date)
                    t = None

                try:
                    result = datetime.datetime.combine(d, t, tzinfo=tz)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {date} and {time} an error is "
                                         f"not raised, with result {result}.")
            else:
                d = datetime.date(*date)
                t = datetime.time(*time, tzinfo=tz if tz is not True else None)
                result = datetime.datetime.combine(d, t, tzinfo=tz)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, time, tz, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromisoformat(self):
        """
        Test that the fromisoformat classmethod creates an instance
        of datetime.
        """
        data = (
            ('0181-01-01T12:30:30.500000', '0181-01-01T12:30:30.500000'),
            ('0001-01-01T00:00:00.0+03:30', '0001-01-01T00:00:00+03:30'),
            ('-1842-01-01T00:00:00+03:30', '-1842-01-01T00:00:00+03:30'),
            ('1161-19-19T+03:30', '1161-19-19T00:00:00+03:30'),
            ('0181-13-09B', '0181-13-09T00:00:00+03:30'),
            ('0181-13-09Z', '0181-13-09T00:00:00+00:00'),
            )
        msg = "Expected {} with date and time {}, "

        for dt, expected_result in data:
            result = datetime.datetime.fromisoformat(dt)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, dt, result))

    #@unittest.skip("Temporarily skipped")
    def test_timetuple(self):
        """
        Test that the timetuple method returns either a short or long form
        timetuple.
        """
        tz0 = ZoneInfo('US/Eastern')
        data = (
            ((181, 13, 9, None, None, 12, 30, 30), None, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=13, tm_mday=9, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=1, tm_yday=237, '
             'tm_isdst=-1)'),
            ((1, 10, 10, 13, 9, 12, 30, 30), None, 0,
             'structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, '
             'tm_year=10, tm_mon=13, tm_mday=9, tm_hour=12, tm_min=30, '
             'tm_sec=30, tm_wday=1, tm_yday=237, tm_isdst=-1)'),
            ((181, 13, 9, None, None, 12, 30, 30), datetime.BADI, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=13, tm_mday=9, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=1, tm_yday=237, '
             'tm_isdst=-1)'),
            ((1, 10, 10, 13, 9, 12, 30, 30), datetime.BADI, 0,
             'structures.LongFormStruct(tm_kull_i_shay=1, tm_vahid=10, '
             'tm_year=10, tm_mon=13, tm_mday=9, tm_hour=12, tm_min=30, '
             'tm_sec=30, tm_wday=1, tm_yday=237, tm_isdst=-1)'),
            ((181, 2, 1), tz0, 0, 'structures.ShortFormStruct(tm_year=181, '
             'tm_mon=2, tm_mday=1, tm_hour=0, tm_min=0, tm_sec=0, tm_wday=1, '
             'tm_yday=20, tm_isdst=1)'),
            )
        msg = "Expected {} with date {}, and timezone {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz)
            result = dt.timetuple()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, result))

    @unittest.skip("Temporarily skipped")
    def test__mktime(self):
        """
        Test that the _mktime method finds the POSIX time in seconds for
        local time. *** TODO *** Fix more of these tests
        """
        # All results below indicate local time.
        data = (
            # Badi epoch (1, 1, 1) (POSIX timestamp)
            # 1844-03-19T18:16:36.7104 -3969391641 Iran/Tehran
            # 1844-03-19T14:46:37 -3969404241 UTC
            # 1844-03-19T09:46:37 -3969422241 America/New_York
            ((1, 1, 1), 0, -3969391592),
            # POSIX epoch 1970-01-01T05:00:00+00:00
            # Sunset exact JD 2440585.166985 (1969, 12, 31)
            # 0.5 - 0.166985 = 0.333015
            # (7, 59, 32.496) after sunset == 12 am 1970-01-01
            # I get 64594 off by 46594
            ## ((126, 16, 2, None, None, 6, 47, 57.12), 0, 18000),
            # Sunset exact JD 2460666.163238 (2024, 12, 23, 20, 28, 24)
            # 0.5 - 0.163238 = 0.336762
            # (8, 4, 56.2368) after sunset == 12 am 2024-12-23
            # (8, 4, 56.2368) + (20, 28, 24) ==
            # I get 1735073211 off by 69507
            ## ((181, 15, 14, None, None, 3, 21, 18.288), 0, 1735003704),

            )
        msg = "Expected {} with date {}, and fold {}, found {}."

        for date, fold, expected_result in data:
            dt = datetime.datetime(*date, fold=fold)
            result = dt._mktime()
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, fold, result))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test_timestamp(self):
        """
        Test that the timestamp method returns either the POSIX time for
        local or for the timezone provided.

        https://www.unixtimestamp.com
        https://www.epochconverter.com
        """
        tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        data = (
            # 2024-03-20T07:30:00+03:30 -> 1710907200
            # 2460387.262245 = sunset on 0181-01-01T00:00:00
            # 0.3125 = T07:30:00
            # 0.550255 = 0.5 - 0.262245 + 0.3125 = T13:12:22.032
            ((181, 1, 1, None, None, 13, 16, 8.9472), tz0, 0, 1710907200),
            # 2024-03-20T04:00:00+00:00 -> 1710907200
            ((181, 1, 1, None, None, 9, 46, 8.9472), tz1, 0, 1710907200),
            # 2024-03-20T00:00:00-05:00 -> 1710907200
            ((181, 1, 1, None, None, 5, 46, 8.9472), tz2, 0, 1710907200),
            # POSIX epoch
            ((126, 16, 2, None, None, 5, 46, 8.9472), tz0, 0, -12600),
            ((126, 16, 2, None, None, 5, 46, 8.9472), datetime.UTC, 0, 0),
            ((126, 16, 2, None, None, 5, 46, 8.9472), tz2, 0, 18000),
            # Local dates and times
            # 2024-03-20T00:00:00 -> 1710907200
            # 0.95098318518518518519 (22, 49, 24.9456) too higt
            #((181, 1, 1, None, None, 5, 46, 8.9472), None, 0, 1710907200),
            # 2024-12-10T12:40:00 -> 1733852400
            #((181, 15, 1), None, 0, 1733867535),

            #((126, 16, 2, None, None, 5, 46, 8.9472), None, 0, 18000),
            #((1, 1, 1), None, 0, -7938830703.0),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.timestamp()
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_utctimetuple(self):
        """
        Test that the utctimetuple method returns a timetuple object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=1, tm_mday=1, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=3, tm_yday=1, '
             'tm_isdst=0)'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.UTC, 0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=1, tm_mday=1, '
             'tm_hour=12, tm_min=30, tm_sec=30, tm_wday=3, tm_yday=1, '
             'tm_isdst=0)'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.utctimetuple()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test__timetuple(self):
        """
        Test that the _timetuple method returns a timetuple object.
        """
        #tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        offset0 = datetime.timedelta(0)
        offset1 = datetime.timedelta(hours=-5)
        data = (
            ((181, 14, 15), None, 0, offset0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=14, tm_mday=15, '
             'tm_hour=0, tm_min=0, tm_sec=0, tm_wday=5, tm_yday=262, '
             'tm_isdst=0)'),
            ((181, 14, 15), tz1, 0, offset0,
             'structures.ShortFormStruct(tm_year=181, tm_mon=14, tm_mday=15, '
             'tm_hour=0, tm_min=0, tm_sec=0, tm_wday=5, tm_yday=262, '
             'tm_isdst=0)'),
            ((181, 14, 15), None, 0, offset1,
             'structures.ShortFormStruct(tm_year=181, tm_mon=14, tm_mday=15, '
             'tm_hour=5, tm_min=0, tm_sec=0, tm_wday=5, tm_yday=262, '
             'tm_isdst=0)'),
            ((181, 14, 15), tz2, 0, offset1,
             'structures.ShortFormStruct(tm_year=181, tm_mon=14, tm_mday=15, '
             'tm_hour=5, tm_min=0, tm_sec=0, tm_wday=5, tm_yday=262, '
             'tm_isdst=0)'),
            )
        msg = ("Expected {} with date {}, timezone {}, fold {}, and "
               "offset {}, found {}.")

        for date, tz, fold, offset, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt._timetuple(offset)
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, offset, result))

    #@unittest.skip("Temporarily skipped")
    def test_date(self):
        """
        Test that thedate method returns a date object with the same date as
        the originating datetime object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), None, 0, '0181-01-01'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.date()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_time(self):
        """
        Test that the time method a time object with the same time as
        the originating datetime object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), None, 0, '12:30:30'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.time()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_timetz(self):
        """
        Test that the timetz method a time object with the same time and
        tzinfo as the originating datetime object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             '12:30:30+03:30'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.timetz()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_replace(self):
        """
        Test that the replace method a new datetime object with the
        replaced values.
        """
        err_msg0 = "Cannot convert from a short to a long form date."
        err_msg1 = ("Cannot convert from a long to a short form date. The "
                    "value {} is not valid for long form dates.")
        data = (
            # Normal replace for a short date
            ((181, 1, 1, None, None), None, 0, (None, None, 182, None, None),
             None, 0, False, '0182-01-01T00:00:00'),
            ((181, 1, 1, None, None), None, 0, (None, None, None, 9, 12),
             None, 0, False, '0181-09-12T00:00:00'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, 23, None, None),
             None, 0, False, '0181-01-01T23:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, 15, 15),
             None, 0, False, '0181-01-01T12:15:15'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             datetime.BADI, 0, False, '0181-01-01T12:30:30+03:30'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             None, 1, False, '0181-01-01T12:30:30'),
            # Normal replace for a long date
            ((1, 10, 10, 1, 1), None, 0, (None, None, 11, None, None), None, 0,
             False, '01-10-11-01-01T00:00:00'),
            ((1, 10, 10, 1, 1), None, 0, (None, 9, None, None, None), None, 0,
             False, '01-09-10-01-01T00:00:00'),
            ((1, 10, 10, 1, 1), None, 0, (None, 9, 10, None, None), None, 0,
             False, '01-09-10-01-01T00:00:00'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, 23, None, None), None, 0,
             False, '01-10-10-01-01T23:30:30'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, 15, 15), None, 0,
             False, '01-10-10-01-01T12:15:15'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             datetime.BADI, 0, False, '01-10-10-01-01T12:30:30+03:30'),
            ((1, 10, 10, 1, 1, 12, 30, 30), None, 0,
             (None, None, None, None, None, None, None, None,),
             None, 1, False, '01-10-10-01-01T12:30:30'),
            # Error conditions.
            ((181, 1, 1, None, None), None, 0, (1, 10, None, None, None),
             None, 0, True, err_msg0),
            ((1, 10, 10, 1, 1), None, 0, (None, None, 181, 1, None),
             None, 0, True, err_msg1.format(181)),
            )
        msg = "Expected {} with date1 {} and date2 {}, found {}."

        for (date1, tz0, fold0, date2, tz1, fold1,
             validity, expected_result) in data:
            dt = datetime.datetime(*date1, tzinfo=tz0, fold=fold0)

            if validity:
                try:
                    result = dt.replace(*date2, tzinfo=tz1, fold=fold1)
                except ValueError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date1}' an error is not "
                                         f"raised, with result {result}.")
            else:
                result = dt.replace(*date2, tzinfo=tz1, fold=fold1)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date1, date2, str(result)))
                self.assertEqual(fold1, result.fold,
                                 f"Expected fold {fold1}, found {result.fold}")

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test__local_timezone(self):
        """
        Test that the _local_timezone method returns the local time offset.
        """
        data = (
            ((181, 1, 1), None, 'UTC-05:00'),
            ((1, 1, 1), datetime.BADI, 'UTC-05:00'),
            ((1, 1, 1, 1, 1), datetime.BADI, 'UTC-05:00'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, tz, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz)
            result = dt._local_timezone()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test_astimezone(self):
        """
        Test that the astimezone method returns a datetime object with a
        replaced the timezone tzinfo object.
        """
        err_msg0 = "tz argument must be an instance of tzinfo, found {}."
        data = (
            ((126, 16, 2, None, None, 6, 47, 57.12), None, datetime.UTC, False,
             '0126-16-02T11:47:57.120000+00:00'),
            # The next two are the date and time with timezone -5 for
            # the Badi epoch.
            ((0, 19, 19, 19, 19, 15, 30), None, datetime.BADI, False,
             '01-01-01-01-01T00:00:00+03:30'),
            ((0, 19, 19, None, None, 15, 30), None, datetime.BADI, False,
             '0001-01-01T00:00:00+03:30'),
            # Test with a current timezone.
            ((0, 19, 19, None, None, 15, 30), datetime.LOCAL, datetime.BADI,
             False, '0001-01-01T00:00:00+03:30'),
            # Test with timezone as None.
            ((181, 1, 1), None, None, False, '0181-01-01T00:00:00-05:00'),
            # Errors
            ((1, 1, 1), None, 0, True, err_msg0.format("<class 'int'>")),
            )
        msg = "Expected {} with date {}, timezone {}, found {}."

        for date, tz0, tz1, validity, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz0)

            if validity:
                try:
                    result = dt.astimezone(tz=tz1)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                result = dt.astimezone(tz=tz1)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz1, result))

    #@unittest.skip("Temporarily skipped")
    def test_ctime(self):
        """
        Test that the ctime method creates a string indicating the date
        and time.

        All days before 1752-09-14 in the Gregorian Calendar will seem wrong
        when compaired to the Badi Calendar in UK and the US. This is when
        The Gregorian Calendar was adopted and compinsated 11 days.
        """
        data = (
            # 0001-03-20 Saturday (Kamál -> Monday)
            ((-1842, 1, 1), 'Kamál Bahá  1 00:00:00 -1842'),
            ((-1842, 1, 1, None, None, 12, 30, 30),
             'Kamál Bahá  1 12:30:30 -1842'),
            # 1582-10-04 Thursday (Jalál -> Saturday)
            ((-261, 11, 7), 'Jalál Mashíyyat  7 00:00:00 -0261'),
            ((-261, 11, 7, None, None, 6, 15, 15),
             'Jalál Mashíyyat  7 06:15:15 -0261'),
            # 1843-03-20 Monday
            ((0, 1, 1), 'Kamál Bahá  1 00:00:00 0000'),
            ((0, 1, 1, None, None, 12, 30, 30), 'Kamál Bahá  1 12:30:30 0000'),
            # 1844-03-19 Tuesday
            ((1, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0001'),
            ((1, 1, 1, None, None, 12, 30, 30), 'Fiḍāl Bahá  1 12:30:30 0001'),
            # 2024-03-19 Tuesday
            ((181, 1, 1), 'Fiḍāl Bahá  1 00:00:00 0181'),
            ((181, 1, 1, None, None, 12, 30, 30),
             'Fiḍāl Bahá  1 12:30:30 0181'),
            # 2024-08-14 Wednesday
            ((181, 8, 16, None, None, 12, 30, 30),
             '`Idāl Kamál 16 12:30:30 0181'),
            ((181, 8, 16), '`Idāl Kamál 16 00:00:00 0181'),
            ((181, 8, 16, None, None, 12, 30, 30),
             '`Idāl Kamál 16 12:30:30 0181'),
            # 2024-08-14 Wednesday
            ((1, 10, 10, 8, 16), '`Idāl Kamál 16 00:00:00 0181'),
            ((1, 10, 10, 8, 16, 12, 30, 30), '`Idāl Kamál 16 12:30:30 0181'),
            # 2024-08-15 Thursday
            ((1, 10, 10, 8, 17), 'Istijlāl Kamál 17 00:00:00 0181'),
            ((1, 10, 10, 8, 17, 12, 30, 30), 'Istijlāl Kamál 17 12:30:30 0181'),
            )
        msg = "Expected {} with date {}, found {}."

        for date, expected_result in data:
            dt = datetime.datetime(*date)
            result = dt.ctime()
            self.assertEqual(expected_result, str(result),
                             msg.format(expected_result, date, result))

    #@unittest.skip("Temporarily skipped")
    def test_isoformat(self):
        """
        Test that the isoformat method returns an ISO formatted date and time.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), 'T', 'auto',
             '0181-01-01T12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30), ' ', 'auto',
             '0181-01-01 12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'auto',
             '0181-01-01T12:30:30.500000'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'hours',
             '0181-01-01T12'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'minutes',
             '0181-01-01T12:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'seconds',
             '0181-01-01T12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'milliseconds',
             '0181-01-01T12:30:30.500'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), 'T', 'microseconds',
             '0181-01-01T12:30:30.500000'),
            )
        msg = "Expected {} with date {}, sep {}, and timespec {}, found {}."

        for date, sep, ts, expected_result in data:
            dt = datetime.datetime(*date)
            result = dt.isoformat(sep, ts)
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, sep, ts, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ method returns a string of the object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30), None, 0,
             'datetime.datetime(181, 1, 1, 12, 30)'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             'datetime.datetime(181, 1, 1, 12, 30, 30)'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), None, 0,
             'datetime.datetime(181, 1, 1, 12, 30, 30, 500000)'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             'datetime.datetime(181, 1, 1, 12, 30, 30, tzinfo=datetime.BADI)'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 1,
             'datetime.datetime(181, 1, 1, 12, 30, 30, '
             'tzinfo=datetime.BADI, fold=1)'),
            ((1, 10, 10, 15, 14), None, 0,
             'datetime.datetime(1, 10, 10, 15, 14, 0, 0)'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = repr(dt)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test__dt_str_conversion(self):
        """
        Test that the _dt_str_conversion method returns a string of the object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30), None, 0, '0181-01-01T12:30:00'),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             '0181-01-01T12:30:30'),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), None, 0,
             '0181-01-01T12:30:30.500000'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             '0181-01-01T12:30:30+03:30'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 1,
             '0181-01-01T12:30:30+03:30'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = str(dt)
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    @unittest.skip("Temporarily skipped")
    def test_strptime(self):
        """
        Test that the strptime method parses a date from a string.

        NOTE: This may need a rewrite of the entire _strptime package.
        """

    #@unittest.skip("Temporarily skipped")
    def test_utcoffset(self):
        """
        Test that the utcoffset method returns a timedelta object relative
        to the UTC time zone..
        """
        tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0, '3:30:00'),
            ((181, 1, 1, None, None, 12, 30, 30), tz0, 0, '3:30:00'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.UTC, 0, '0:00:00'),
            ((181, 1, 1, None, None, 12, 30, 30), tz1, 0, '0:00:00'),
            ((181, 1, 1, None, None, 12, 30, 30), tz2, 0, '-1 day, 20:00:00'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.utcoffset()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_tzname(self):
        """
        Test that the tzname method returns the timezone associated with
        the datetime object.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             'UTC+03:30'),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.UTC, 0, 'UTC'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.tzname()
            self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test_dst(self):
        """
        Test that the dst method returns the daylight savings time
        associated with the datetime object.
        """
        tz0 = ZoneInfo(datetime.BADI_IANA)
        tz1 = ZoneInfo('UTC')
        tz2 = ZoneInfo('US/Eastern')
        data = (
            ((181, 1, 1, None, None, 12, 30, 30), tz0, 0, '0:00:00'),
            ((181, 1, 1, None, None, 12, 30, 30), tz1, 0, '0:00:00'),
            ((181, 7, 1), tz2, 0, '1:00:00'),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt.dst()
            self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns True if equal and False if
        not equal.
        """
        data = (
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 30), None, False, True),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 29), None, False, False),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 31), None, False, False),
            ((181, 9, 14, None, None, 12, 30, 30), datetime.UTC,
             (181, 9, 14, None, None, 12, 30, 30), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 30), None, False, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 29), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 31), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), datetime.UTC,
             (1, 10, 10, 9, 14, 12, 30, 31), None, False, False),
            ((181, 9, 14), None, (181, 9, 14), None, True, False),
            ((181, 9, 14), None, 1.5, None, False, False),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, tz0, date1, tz1, is_date, expected_result in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz0)

            if isinstance(date1, float):
                dt1 = date1
            elif is_date:
                dt1 = datetime.date(*date1)
            else:
                dt1 = datetime.datetime(*date1, tzinfo=tz1)

            result = dt0 == dt1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___le__(self):
        """
        Test that the __le__ method returns True if less than or equal and
        False if not less than or equal.
        """
        err_msg0 = "Cannot compare 'datetime' to '{}'"
        err_msg1 = "'<=' not supported between instances of 'datetime' and '{}'"
        data = (
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 30), None, False, True),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 29), None, False, False),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 31), None, False, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 30), None, False, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 29), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 31), None, False, True),
            ((181, 9, 14), None, (181, 9, 14), None, True,
             err_msg0.format('date')),
            ((181, 9, 14), None, 1.5, None, True, err_msg1.format('float')),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, tz0, date1, tz1, validity, expected_result in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz0)

            if validity:
                if isinstance(date1, float):
                    dt1 = date1
                else:
                    dt1 = datetime.date(*date1)

                try:
                    result = dt0 <= dt1
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date0}' an error is not "
                                         f"raised, with result {result}.")
            else:
                if isinstance(date1, float):
                    dt1 = date1
                else:
                    dt1 = datetime.datetime(*date1, tzinfo=tz1)

                result = dt0 <= dt1
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___lt__(self):
        """
        Test that the __lt__ method returns True if less than and False
        if not less than.
        """
        err_msg0 = "Cannot compare 'datetime' to '{}'"
        err_msg1 = "'<' not supported between instances of 'datetime' and '{}'"
        data = (
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 30), None, False, False),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 29), None, False, False),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 31), None, False, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 30), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 29), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 31), None, False, True),
            ((181, 9, 14), None, (181, 9, 14), None, True,
             err_msg0.format('date')),
            ((181, 9, 14), None, 1.5, None, True, err_msg1.format('float')),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, tz0, date1, tz1, validity, expected_result in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz0)

            if validity:
                if isinstance(date1, float):
                    dt1 = date1
                else:
                    dt1 = datetime.date(*date1)

                try:
                    result = dt0 < dt1
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date0}' an error is not "
                                         f"raised, with result {result}.")
            else:
                dt1 = datetime.datetime(*date1, tzinfo=tz1)
                result = dt0 < dt1
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___ge__(self):
        """
        Test that the __ge__ method returns True if greater than or equal
        and False if not greater than or equal.
        """
        err_msg0 = "Cannot compare 'datetime' to '{}'"
        err_msg1 = "'>=' not supported between instances of 'datetime' and '{}'"
        data = (
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 30), None, False, True),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 29), None, False, True),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 31), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 30), None, False, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 29), None, False, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 31), None, False, False),
            ((181, 9, 14), None, (181, 9, 14), None, True,
             err_msg0.format('date')),
            ((181, 9, 14), None, 1.5, None, True, err_msg1.format('float')),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, tz0, date1, tz1, validity, expected_result in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz0)

            if validity:
                if isinstance(date1, float):
                    dt1 = date1
                else:
                    dt1 = datetime.date(*date1)

                try:
                    result = dt0 >= dt1
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date0}' an error is not "
                                         f"raised, with result {result}.")
            else:
                dt1 = datetime.datetime(*date1, tzinfo=tz1)
                result = dt0 >= dt1
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___gt__(self):
        """
        Test that the __gt__ method returns True if greater than and False
        if not greater than.
        """
        err_msg0 = "Cannot compare 'datetime' to '{}'"
        err_msg1 = "'>' not supported between instances of 'datetime' and '{}'"
        data = (
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 30), None, False, False),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 29), None, False, True),
            ((181, 9, 14, None, None, 12, 30, 30), None,
             (181, 9, 14, None, None, 12, 30, 31), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 30), None, False, False),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 29), None, False, True),
            ((1, 10, 10, 9, 14, 12, 30, 30), None,
             (1, 10, 10, 9, 14, 12, 30, 31), None, False, False),
            ((181, 9, 14), None, (181, 9, 14), None, True,
             err_msg0.format('date')),
            ((181, 9, 14), None, 1.5, None, True, err_msg1.format('float')),
            )
        msg = "Expected {} with date0 {} and date1 {}, found {}."

        for date0, tz0, date1, tz1, validity, expected_result in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz0)

            if validity:
                if isinstance(date1, float):
                    dt1 = date1
                else:
                    dt1 = datetime.date(*date1)

                try:
                    result = dt0 > dt1
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date0}' an error is not "
                                         f"raised, with result {result}.")
            else:
                dt1 = datetime.datetime(*date1, tzinfo=tz1)
                result = dt0 > dt1
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, result))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test__cmp(self):
        """
        Test that the _cmp method returns 1 if the two dates are equal, +1
        if the current date is greater than the test date, and -1 if the
        inverse.
        """
        err_msg0 = "The other must be a datetime object, found '{}'."
        err_msg1 = "Cannot compare naive and aware datetimes."
        tz0 = ZoneInfo('US/Pacific')
        data = (
            ((181, 9, 14, None, None, 12, 30, 30), None, 0,
             (181, 9, 14, None, None, 12, 30, 30), None, 0, False, False, 0),
            ((181, 9, 14, None, None, 12, 30, 30), None, 0,
             (181, 9, 14, None, None, 12, 30, 29), None, 0, False, False, 1),
            ((181, 9, 14, None, None, 12, 30, 30), None, 0,
             (181, 9, 14, None, None, 12, 30, 31), None, 0, False, False, -1),
            ((1, 10, 10, 9, 14, 12, 30, 30), None, 0,
             (1, 10, 10, 9, 14, 12, 30, 30), None, 0, False, False, 0),
            ((1, 10, 10, 9, 14, 12, 30, 30), None, 0,
             (1, 10, 10, 9, 14, 12, 30, 29), None, 0, False, False, 1),
            ((1, 10, 10, 9, 14, 12, 30, 30), None, 0,
             (1, 10, 10, 9, 14, 12, 30, 31), None, 0, False, False, -1),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (181, 1, 1, None, None, 12, 30, 30), None, 0, True, False, 0),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0, True,
             False, 2),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0,
             (181, 1, 1, None, None, 12, 30, 30), datetime.UTC, 0, True,
             False, -1),
            ((181, 1, 1, None, None, 12, 30, 30), datetime.UTC, 0,
             (181, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0, True,
             False, 1),
            # Fold on 2024-11-03T02:00:00 -> 0181-10-01T08:41:37.2768
            ((181, 10, 1, None, None, 8, 41, 37.2768), datetime.LOCAL, 0,
             (181, 10, 1, None, None, 8, 41, 37.2768), None, 0, True,
             False, 2),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0, None, None, 0,
             False, True, err_msg0.format("<class 'NoneType'>")),
            ((181, 1, 1, None, None, 12, 30, 30), None, 0,
             (182, 1, 1, None, None, 12, 30, 30), datetime.BADI, 0, False,
             True, err_msg1),
            )
        msg = "Expected {} with date0 {}, date1 {}, and mixed {}, found {}."

        for (date0, tz0, fold0, date1, tz1, fold1, mixed,
             validity, expected_result) in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz0)

            if date1 is not None:
                dt1 = datetime.datetime(*date1, tzinfo=tz1, fold=fold1)
            else:
                dt1 = date1

            if validity:
                try:
                    result = dt0._cmp(dt1, allow_mixed=mixed)
                except (AssertionError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = dt0._cmp(dt1, allow_mixed=mixed)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, date1, mixed, result))

    #@unittest.skip("Temporarily skipped")
    def test___add__(self):
        """
        Test that the __add__ method can correctly add a datetime to
        a timedelta.
        """
        err_msg0 = "Result out of range."
        err_msg1 = "unsupported operand type(s) for +: 'datetime' and '{}'"
        data = (
            ((1, 1, 1), (1,), False, (1, 1, 2)),
            ((1, 1, 1), (-1,), False, (0, 19, 19)),
            ((1, 1, 1), (366,), False, (2, 1, 1)),             # Leap year
            ((181, 1, 1), (365,), False, (182, 1, 1)),         # Non leap year
            ((1, 1, 1, 1, 1), (366,), False, (1, 1, 2, 1, 1)), # Leap year
            ((-1842, 1, 1), (-1,), True, err_msg0),
            ((1161, 19, 19), (1,), True, err_msg0),
            ((181, 1, 1), 1.5, True, err_msg1.format('float'))
            )
        msg = "Expected {} with date {} and timedelta {}, found {}"

        for date, td, validity, expected_result in data:
            dt0 = datetime.datetime(*date)

            if validity:
                if isinstance(td, tuple):
                    td0 = datetime.timedelta(*td)
                else:
                    td0 = td

                try:
                    result = dt0 + td0
                except (OverflowError, TypeError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date}' an error is not "
                                         f"raised, with result {result}.")
            else:
                td0 = datetime.timedelta(*td)
                dt1 = dt0 + td0

                if dt1.is_short:
                    result = (dt1.year, dt1.month, dt1.day)
                else:
                    result = (dt1.kull_i_shay, dt1.vahid, dt1.year,
                              dt1.month, dt1.day)

                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date, td, result))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test___sub__(self):
        """
        Test that the __sub__ method returns the correct results of a
        timedelta object subtracted from a datetime object.
        """
        err_msg0 = "Result out of range."
        err_msg1 = "unsupported operand type(s) for -: 'datetime' and '{}'"
        # err_msg2 = "Cannot mix naive and timezone-aware time."
        data = (
            ((1, 1, 1), None, (1,), (), False, (0, 19, 19)),
            ((1, 1, 1), None, (366,), (), False, (-1, 19, 19)),  # Leap year
            ((181, 1, 1), None, (365,), (), False, (180, 1, 1)), # Non leap year
            # Leap year
            ((1, 1, 1, 1, 1), None, (366,), (), False, (0, 19, 18, 19, 19)),
            ((181, 1, 2), None, (), (181, 1, 1), False, (1, 0, 0, 86400.0)),
            ((-1842, 1, 1), None, (1,), (), True, err_msg0),
            ((1161, 19, 19), None, (-1,), (), True, err_msg0),
            ((181, 1, 1), None, 1.5, (), True, err_msg1.format('float')),
            #((181, 1, 2), datetime.LOCAL, (), (181, 1, 1), True, err_msg2),
            )
        msg = "Expected {} with date0 {}, timedelta {}, and date1 {}, found {}"

        for date0, tz, td, date1, validity, expected_result in data:
            dt0 = datetime.datetime(*date0, tzinfo=tz)

            if validity:
                if isinstance(td, tuple):
                    obj = datetime.timedelta(*td)
                elif isinstance(date1, tuple) and len(date1) > 0:
                    obj = datetime.datetime(*date1)
                else:
                    obj = td

                try:
                    result = dt0 - obj
                except (TypeError, OverflowError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With '{date0}' an error is not "
                                         f"raised, with result {result}.")
            else:
                if td:
                    obj = datetime.timedelta(*td)
                else:
                    obj = datetime.datetime(*date1)

                dt1 = dt0 - obj

                if isinstance(dt1, datetime.timedelta):
                    result = (dt1.days, dt1.seconds, dt1.microseconds,
                              dt1.total_seconds())
                elif dt1.is_short:
                    result = (dt1.year, dt1.month, dt1.day)
                else:
                    result = (dt1.kull_i_shay, dt1.vahid, dt1.year,
                              dt1.month, dt1.day)

                self.assertEqual(expected_result, result, msg.format(
                    expected_result, date0, td, date1, result))

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns a valid hash for both short
        and long form datetimes.
        """
        data = (
            ((datetime.MINYEAR, 1, 1, None, None, 12, 30, 30), None, 0),
            ((-5, 18, 1, 1, 1, 12, 30, 30), None, 0),
            ((1, 1, 1, None, None, 12, 30, 30), None, 0),
            ((1, 1, 1, 1, 1, 12, 30, 30), None, 0),
            ((1, 1, 1), datetime.BADI, 0),
            ((1, 1, 1), datetime.BADI, 1),
            )
        msg = "date {}, found {}."

        for date, tz, fold in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = hash(dt)
            self.assertTrue(len(str(result)) > 15, msg.format(date, result))

    #@unittest.skip("Temporarily skipped")
    def test__is_pickle_data(self):
        """
        Test that the _is_pickle_data classmethod returns the correct results
        depending on the incoming data.
        """
        err_msg0 = "Invalid string {} had length of {} for pickle."
        err_msg1 = ("Failed to encode latin1 string when unpickling a date or "
                    "datetime object. pickle.load(data, encoding='latin1') is "
                    "assumed.")
        data = (
            ((b'\x00\x00\x01\x01\x0c\x1e\x1e\x07\xa1 ', None), False, True),
            ((b'\x0e\x12\x01\x01\x01\x0c\x1e\x1e\x07\xa1 ', None), False,
             False),
            ((181, 10), False, None),
            ((b'\x073\x01\x01\x00\x00\x00\x00\x00\x00', datetime.BADI), False,
             True),
            ((b'\x14\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00', datetime.BADI),
             False, False),
            (((181, 1, 1, None, None), None), False, None),
            (('\x00\x00\xf3\x01\x0c\x1e\x1e\x07\xa1 ', None), False, None),
            (('\x0e\x12\x01\xf3\x01\x0c\x1e\x1e\x07\xa1 ', None), False, None),
            ((b'\x14\x01\x01\x01\x01\x01', None), True, err_msg0.format(
                b'\x14\x01\x01\x01\x01\x01', 6)),
            (('\u2190\x01\x01\x011\x01\x1e\x1e\x07\xa1 ', None), True,
             err_msg1),
            )
        msg = "Expected {} with value {}, found {}."

        for value, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.datetime._is_pickle_data(*value)
                except (AssertionError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {value} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.datetime._is_pickle_data(*value)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, value, result))

    #@unittest.skip("Temporarily skipped")
    def test__getstate(self):
        """
        Test that the _getstate method returns the correct state for pickling.
        """
        data = (
            ((datetime.MINYEAR, 1, 1, None, None, 12, 30, 30, 500000), None, 0,
             (b'\x00\x00\x01\x01\x0c\x1e\x1e\x07\xa1 ',)),
            ((-5, 18, 1, 1, 1, 12, 30, 30, 500000), None, 0,
             (b'\x0e\x12\x01\x01\x01\x0c\x1e\x1e\x07\xa1 ',)),
            ((1, 1, 1, None, None), datetime.BADI, 0,
             (b'\x073\x01\x01\x00\x00\x00\x00\x00\x00',
              datetime.timezone.badi)),
            ((1, 1, 1, 1, 1), datetime.BADI, 0,
             (b'\x14\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00',
              datetime.timezone.badi)),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), datetime.BADI, 0,
             (b'\x07\xe7\x01\x01\x0c\x1e\x1e\x07\xa1 ',
              datetime.timezone.badi)),
            )
        msg = "Expected {} with date {}, timezone {}, and fold {}, found {}."

        for date, tz, fold, expected_result in data:
            dt = datetime.datetime(*date, tzinfo=tz, fold=fold)
            result = dt._getstate()
            self.assertEqual(expected_result, result, msg.format(
                expected_result, date, tz, fold, result))

    #@unittest.skip("Temporarily skipped")
    def test___setstate(self):
        """
        Test that the __setstate method sets the datetime properly.
        """
        err_msg0 = "Bad tzinfo state arg."
        data = (
            ((datetime.MINYEAR, 1, 1, None, None, 12, 30, 30, 500000), None,
             b'\x00\x00\x01\x01\x0c\x1e\x1e\x07\xa1 ', False),
            ((-5, 18, 1, 1, 1, 12, 30, 30, 500000), None,
             b'\x0e\x12\x01\x01\x01\x0c\x1e\x1e\x07\xa1 ', False),
            ((1, 1, 1, None, None, 0, 0, 0, 0), datetime.BADI,
             b'\x073\x01\x01\x00\x00\x00\x00\x00\x00', False),
            ((1, 1, 1, 1, 1, 0, 0, 0, 0), datetime.BADI,
             b'\x14\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00', False),
            ((datetime.MINYEAR, 1, 1, None, None, 12, 30, 30, 500000), None,
             b'\x00\x00\x01\x01\x0c\x1e\x1e\x07\xa1 ', True),
            )
        msg = "Expected {} with bytes_str {}, found {}."

        for date, tz, bytes_str, validity in data:
            dt = datetime.datetime(*date, tzinfo=tz)

            if validity:
                try:
                    dt._datetime__setstate(bytes_str, dt)
                except TypeError as e:
                    self.assertEqual(err_msg0, str(e))
                else:
                    raise AssertionError(f"With {date} an error is not raised.")
            else:
                dt._datetime__setstate(bytes_str, tz)

                if dt.is_short:
                    result = (dt.year, dt.month, dt.day, None, None, dt.hour,
                              dt.minute, dt.second, dt.microsecond)
                else:
                    result = (dt.kull_i_shay, dt.vahid, dt.year, dt.month,
                              dt.day, dt.hour, dt.minute, dt.second,
                              dt.microsecond)

                self.assertEqual(date, result, msg.format(
                    date, bytes_str, result))

    #@unittest.skip("Temporarily skipped")
    def test___reduce_ex__(self):
        """
        Test that the __reduce_ex__ method creates the correct pickle value
        for protocol 3.
        """
        data = (
            ((181, 1, 1, None, None, 12, 30, 30, 500000), datetime.BADI, 0),
            ((181, 1, 1, None, None, 12, 30, 30, 500000), datetime.UTC, 1),
            )
        msg = "Expected {}, with date {}, timezone {}, and fold {}, found {}"

        for date, tz, fold in data:
            dt0 = datetime.datetime(*date, tzinfo=tz, fold=fold)
            obj = pickle.dumps(dt0)
            dt1 = pickle.loads(obj)

            if dt0.is_short:
                dt0_result = (dt0.year, dt0.month, dt0.day, dt0.hour,
                              dt0.minute, dt0.second, dt0.microsecond,
                              dt0.tzinfo, dt0.fold)
                dt1_result = (dt1.year, dt1.month, dt1.day, dt1.hour,
                              dt1.minute, dt1.second, dt1.microsecond,
                              dt1.tzinfo, dt1.fold)
            else:
                dt0_result = (dt0.kull_i_shay, dt0.vahid, dt0.year, dt0.month,
                              dt0.day, dt0.hour, dt0.minute, dt0.second,
                              dt0.microsecond, dt0.tzinfo, dt0.fold)
                dt1_result = (dt1.kull_i_shay, dt1.vahid, dt1.year, dt1.month,
                              dt1.day, dt1.hour, dt1.minute, dt1.second,
                              dt1.microsecond, dt1.tzinfo, dt1.fold)

            self.assertEqual(dt0_result, dt1_result, msg.format(
                dt0_result, date, tz, fold, dt1_result))


class TestBadiDatetime_timezone(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test___new__(self):
        """
        Test that the __new__ method creates an instance from both a pickle
        object and a normal instantiation.
        """
        err_msg0 = "offset must be a timedelta"
        err_msg1 = "name must be a string"
        err_msg2 = ("offset must be a timedelta strictly between "
                    "-timedelta(hours=24) and timedelta(hours=24).")
        td = datetime.timedelta(hours=datetime.BADI_COORD[2])
        IANA = datetime.BADI_IANA
        data = (
            (td, IANA, False, IANA),
            (td, datetime.timezone._Omitted, False, 'UTC+03:30'),
            (td, '', False, ''),
            (object, IANA, True, err_msg0),
            (td, object, True, err_msg1),
            (datetime.timedelta(hours=25), '', True, err_msg2),
            )
        msg = "Expected {} with offset {} and name {}, found {}."

        for offset, name, validity, expected_result in data:
            if validity:
                try:
                    result = datetime.timezone(offset, name)
                except (TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {time} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = datetime.timezone(offset, name)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, name, result))

    #@unittest.skip("Temporarily skipped")
    def test___eq__(self):
        """
        Test that the __eq__ method returns  True if equal and False if
        not equal.
        """
        td0 = datetime.timedelta(hours=datetime.BADI_COORD[2])
        td1 = datetime.timedelta(seconds=18000)
        IANA = datetime.BADI_IANA
        data = (
            (td0, IANA, td0, IANA, True),
            (td0, IANA, td1, 'US/Eastern', False),
            )
        msg = "Expected {} with td0 {} and td1 {}, found {}."

        for offset0, name0, offset1, name1, expected_result in data:
            tz0 = datetime.timezone(offset0, name0)
            tz1 = datetime.timezone(offset1, name1)
            result = tz0 == tz1
            self.assertEqual(expected_result, result, msg.format(
                expected_result, td0, td1, result))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ method returns the correctly formatted string.
        """
        td0 = datetime.timedelta(hours=datetime.BADI_COORD[2])
        td1 = datetime.timedelta(0)
        td2 = datetime.timedelta()
        IANA = datetime.BADI_IANA
        data = (
            (td0, IANA, "datetime.timezone("
             "datetime.timedelta(seconds=12600), 'Asia/Tehran')"),
            (td1, 'UTC', "datetime.timezone(datetime.timedelta(0), 'UTC')"),
            (td2, '', "datetime.timezone(datetime.timedelta(0), '')"),
            (td2, None, "datetime.timezone(datetime.timedelta(0))"),
            ('UTC', 'UTC', 'datetime.timezone.utc'),
            ('BADI', IANA, 'datetime.BADI'),
            )
        msg = "Expected {} with offset {} and name {}, found {}."

        for offset, name, expected_result in data:
            if name is None:
                tz = datetime.timezone._create(offset, name)
            elif offset == 'UTC':
                tz = datetime.UTC
            elif offset == 'BADI':
                tz = datetime.BADI
            else:
                tz = datetime.timezone(offset, name)

            result = repr(tz)
            self.assertEqual(expected_result, result, msg.format(
                expected_result, offset, name, result))

    #@unittest.skip("Temporarily skipped")
    def test___str__(self):
        """
        Test that the __str__ method returns the correctly formatted string.
        """
        td0 = datetime.timedelta(hours=datetime.BADI_COORD[2])
        td1 = datetime.timedelta(seconds=18000)
        IANA = datetime.BADI_IANA
        data = (
            (td0, IANA, IANA),
            (td1, 'US/Eastern', 'US/Eastern'),
            )
        msg = "Expected {} with offset {}, found {}."

        for offset, name, expected_result in data:
            result = datetime.timezone(offset, name)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, offset, str(result)))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test_utcoffset(self):
        """
        Test that the utcoffset method returns the correct timezone offset
        for the UTC coordinates.
        """
        err_msg0 = "utcoffset() argument must be a datetime instance or None"
        td0 = datetime.timedelta(hours=datetime.BADI_COORD[2])
        td1 = datetime.timedelta(hours=datetime.LOCAL_COORD[2])
        data = (
            (td0, (181, 1, 1), False, '3:30:00'),
            (td1, (181, 1, 1), False, '-1 day, 19:00:00'),
            (td1, None, False, '-1 day, 19:00:00'),
            (td0, (12, 30, 30), True, err_msg0),
            )
        msg = "Expected {} with offset {}, date {}, and timezone {}, found {}."

        for offset, date, validity, expected_result in data:
            tz0 = datetime.timezone(offset)

            if validity:
                try:
                    t = datetime.time(*date)
                    result = tz0.utcoffset(t)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {offset} an error is not "
                                         f"raised, with result {result}.")
            else:
                if isinstance(date, tuple):
                    dt = datetime.datetime(*date)
                else:
                    dt = date

                result = tz0.utcoffset(dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, date, tz0, result))

    #@unittest.skip("Temporarily skipped")
    @patch.object(datetime, 'LOCAL_COORD', (35.5894, -78.7792, -5.0))
    def test_badioffset(self):
        """
        Test that the badioffset returns the correct timezone offset for the
        Badi coordinates.
        """
        err_msg0 = "badioffset() argument must be a datetime instance or None"
        td0 = datetime.timedelta(hours=datetime.BADI_COORD[2])
        td1 = datetime.timedelta(hours=datetime.LOCAL_COORD[2])
        data = (
            (td0, (181, 1, 1), False, '0:00:00'),
            (td1, (181, 1, 1), False, '-1 day, 15:30:00'),
            (td1, None, False, '-1 day, 15:30:00'),
            (td0, (12, 30, 30), True, err_msg0),
            )
        msg = "Expected {} with offset {}, date {}, and timezone {}, found {}."

        for offset, date, validity, expected_result in data:
            tz0 = datetime.timezone(offset)

            if validity:
                try:
                    t = datetime.time(*date)
                    result = tz0.badioffset(t)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {offset} an error is not "
                                         f"raised, with result {result}.")
            else:
                if isinstance(date, tuple):
                    dt = datetime.datetime(*date)
                else:
                    dt = date

                result = tz0.badioffset(dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, date, tz0, result))

    #@unittest.skip("Temporarily skipped")
    def test_tzname(self):
        """
        Test that the tzname method returns the timezone name.
        """
        err_msg0 = "tzname() argument must be a datetime instance or None"
        td = datetime.timedelta(hours=datetime.BADI_COORD[2])
        IANA = datetime.BADI_IANA
        data = (
            (td, IANA, (181, 1, 1), False, IANA),
            (td, IANA, None, False, IANA),
            (td, None, None, False, 'UTC+03:30'),
            (td, IANA, False, True, err_msg0),
            )
        msg = "Expected {} with offset {}, name {}, date {}, and , found {}."

        for offset, name, date, validity, expected_result in data:
            if name is None:
                tz0 = datetime.timezone._create(offset, name)
            else:
                tz0 = datetime.timezone(offset, name)

            if date:
                dt = datetime.datetime(*date)
            elif date is False:
                dt = datetime.time()
            else:
                dt = date

            if validity:
                try:
                    result = tz0.tzname(dt)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {tz0} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = tz0.tzname(dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, name, date, tz0, result))

    #@unittest.skip("Temporarily skipped")
    def test_dst(self):
        """
        Test that the dst method always returns None.
        """
        err_msg0 = "dst() argument must be a datetime instance or None"
        td = datetime.timedelta(hours=datetime.BADI_COORD[2])
        IANA = datetime.BADI_IANA
        data = (
            (td, IANA, (181, 1, 1), None, False, None),
            (td, IANA, None, None, False, None),
            (td, IANA, False, None, True, err_msg0),
            )
        msg = ("Expected {} with offset {}, name {}, date {}, and "
               "timezone {}, found {}.")

        for offset, name, date, tz0, validity, expected_result in data:
            tz1 = datetime.timezone(offset, name)

            if date:
                dt = datetime.datetime(*date)
            elif date is False:
                dt = datetime.time()
            else:
                dt = date

            if validity:
                try:
                    result = tz1.dst(dt)
                except TypeError as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {tz1} an error is not "
                                         f"raised, with result {result}.")
            else:
                result = tz1.dst(dt)
                self.assertEqual(expected_result, result, msg.format(
                    expected_result, offset, name, date, tz1, result))

    #@unittest.skip("Temporarily skipped")
    def test_fromutc(self):
        """
        Test that the fromutc method returns a timezone object from a UTC
        timezone.
        """
        err_msg0 = "fromutc: dt.tzinfo is not self"
        err_msg1 = "fromutc() argument must be a datetime instance or None"
        td = datetime.timedelta(hours=datetime.BADI_COORD[2])
        IANA = datetime.BADI_IANA
        data = (
            (td, IANA, (181, 1, 1), False,
             "0181-01-01T03:30:00+03:30"),
            (td, IANA, (181, 1, 1), True, err_msg0),
            (td, IANA, None, True, err_msg1),
            )
        msg = "Expected {} with offset {}, name {}, and date {} found {}."

        for offset, name, date, validity, expected_result in data:
            tz1 = datetime.timezone(offset, name)

            if not validity and date:
                dt = datetime.datetime(*date, tzinfo=tz1)
            else:
                dt = datetime.time()

            if validity:
                if date:
                    dt = datetime.datetime(*date)

                try:
                    result = tz1.fromutc(dt)
                except (TypeError, ValueError) as e:
                    self.assertEqual(expected_result, str(e))
                else:
                    result = result if result else None
                    raise AssertionError(f"With {tz1} an error is not "
                                         f"raised, with result {result}.")
            else:
                dt = datetime.datetime(*date, tzinfo=tz1)
                result = tz1.fromutc(dt)
                self.assertEqual(expected_result, str(result), msg.format(
                    expected_result, offset, name, date, result))

    #@unittest.skip("Temporarily skipped")
    def test__name_from_offset(self):
        """
        Test that the _name_from_offset returns a string indicating the
        UTC offset.
        """
        td0 = datetime.timedelta(0)
        td1 = datetime.timedelta(hours=datetime.BADI_COORD[2])
        td2 = datetime.timedelta(-1)
        td3 = datetime.timedelta(microseconds=500000)
        td4 = datetime.timedelta(seconds=50)
        data = (
            (td0, 'UTC'),
            (td1, "UTC+03:30"),
            (td2, 'UTC-24:00'),
            (td3, 'UTC+00:00:00.500000'),
            (td4, 'UTC+00:00:50'),
            )
        msg = "Expected {} with offset {} found {}."

        for offset, expected_result in data:
            tz = datetime.timezone(td0, 'UTC')
            result = tz._name_from_offset(offset)
            self.assertEqual(expected_result, str(result), msg.format(
                expected_result, offset, result))

    #@unittest.skip("Temporarily skipped")
    def test___getinitargs__(self):
        """
        Test that the __getinitargs__ method returns the arguments that the
        timezone class was instantiated with.
        """
        BADI_INFO = (datetime.BADI_COORD[2], datetime.BADI_IANA)
        data = (
            (-5, None, '(datetime.timedelta(days=-1, seconds=68400),)'),
            (*BADI_INFO, "(datetime.timedelta(seconds=12600),"
             " 'Asia/Tehran')"),
            )
        msg0 = "Expected {} with offset {} and name {}, found {}."
        msg1 = "Expected {}, found {}."

        for os, name, expected_result in data:
            offset = datetime.timedelta(hours=os)
            args = (offset, name) if name else (offset,)
            tz = datetime.timezone(*args)
            result = tz.__getinitargs__()
            self.assertEqual(expected_result, str(result), msg0.format(
                expected_result, os, name, result))
            p_obj = pickle.dumps(tz)
            up_obj = pickle.loads(p_obj)
            self.assertEqual(up_obj._offset, tz._offset, msg1.format(
                up_obj._offset, tz._offset))
            self.assertEqual(up_obj._name, tz._name, msg1.format(
                up_obj._name, tz._name))

    #@unittest.skip("Temporarily skipped")
    def test___hash__(self):
        """
        Test that the __hash__ method returns a valid hash.
        """
        td0 = datetime.timedelta(0)
        data = (
            (td0, 'UTC'),
            )
        msg = "offset {} with name {}, found {}."

        for offset, name in data:
            tz = datetime.timezone(offset, name)
            result = hash(tz)
            self.assertTrue(len(str(result)) > 15, msg.format(
                offset, name, result))
