# -*- coding: utf-8 -*-
#
# bahai_calendar/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from bahai_calendar.base_calendar import BaseCalendar
from bahai_calendar.gregorian_calendar import GregorianCalendar


class BahaiCalendar(BaseCalendar):
    """
    Implementation of the Baha'i (Badi) Calendar.
    """
    #(defconstant bahai-location
    #  ;; TYPE location
    #  ;; Location of Tehran for astronomical Baha’i calendar.
    #  (location (deg 35.696111L0) (deg 51.423056L0) (mt 0) (hr (+ 3 1/2))))
    # WGS84:          35.689252, 51.3896 1595m 3.5
    # WGS84--https://coordinates-converter.com/
    # https://whatismyelevation.com/location/35.63735,51.72569/Tehran--Iran-
    # https://en-us.topographic-map.com/map-g9q1h/Tehran/?center=35.69244%2C51.19492
    BAHAI_LOCATION = (35.696111, 51.423056, 0, 3.5)
    #BAHAI_LOCATION = (36.176768, 52.709659, 0, 3.5) # Northeast of Tehran

    #(defconstant ayyam-i-ha
    #  ;; TYPE bahai-month
    #  ;; Signifies intercalary period of 4 or 5 days.
    #  0)
    AYYAM_I_HA = 0

    #(defconstant bahai-epoch
    #  ;; TYPE fixed-date
    #  ;; Fixed date of start of Baha’i calendar.
    #  (fixed-from-gregorian (gregorian-date 1844 march 21)))
    BAHAI_EPOCH = 673222

    BADI_EPOCH = 2394645.5

    BADI_MONTHS = (
        (1, 'Bahá'), (2, 'Jalál'), (3, 'Jamál'), (4, "'Aẓamat"), (5, 'Núr'),
        (6, 'Raḥmat'), (7, 'Kalimát'), (8, 'Kamál'), (9, "Asmá'"),
        (10, "'Izzat"), (11, 'Mashíyyat'), (12, "'Ilm"), (13, 'Qudrat'),
        (14, 'Qawl'), (15, 'Masá’il'), (16, 'Sharaf'), (17, 'Sulṭán'),
        (18, 'Mulk'), (0, 'Ayyám-i-Há'), (19, "'Alá'")
        )

    def __init__(self):
        super().__init__()
        # kull_i_shay: 361-year (19^2) vahid (integer)
        # vahid: (integer) 19-year vahid
        # year: (integer) 1 - 19
        # month: (integer) 1 - 19 plus 0 for Ayyām-i-Hā
        # day: (integer) 1 - 19
        # Baha'i date: [kull_i_shay, vahid, year, month, day]
        self._bahai_date = None
        self._gc = GregorianCalendar()

    def parse_datetime(self, dt:datetime.datetime) -> None:
        self._gc.parse_datetime(dt)
        fixed = self._gc.fixed_from_gregorian(self._gc.date_representation)
        self.date_representation = self.astro_bahai_from_fixed(fixed)

    @property
    def date_representation(self) -> tuple:
        return self._bahai_date

    @date_representation.setter
    def date_representation(self, representation:tuple=None):
        if not representation:
            representation = self.BAHAI_LOCATION

        self._bahai_date = representation

    def bahai_sunset(self, date:tuple, lat:float, lon:float,
                     zone:float=0) -> float:
        """
        Return the sunset for the given Badi Day.

        *** TODO *** Change name to sunset later on.
        """
        b_date = self.b_date_from_date(date)
        jd = self.jd_from_badi_date(b_date)
        ss_coff = self._sun_rising(jd, lat, lon, zone)
        return self.date_from_jd(jd + ss_coff)

    def astro_bahai_new_year_on_or_before(self, date:int) -> float:
        """
        (defun astro-bahai-new-year-on-or-before (date)
          ;; TYPE fixed-date -> fixed-date
          ;; Fixed date of astronomical Bahai New Year on or before fixed
          ;; date.
        (let* ((approx            ; Approximate time of equinox.
                (estimate-prior-solar-longitude spring (bahai-sunset date))))
          (next day (1- (floor approx))
                (<= (solar-longitude (bahai-sunset day))
                    (+ spring (deg 2))))))
        """
        approx = self.find_moment_of_equinoxes_or_solstices(
            self.sunset(date), lam=self.SPRING)
        initial = math.floor(approx) - 1
        condition = lambda day: (
            self.alt_solar_longitude(self.sunset(day)) <= (self.SPRING + 2))
        return self._next(initial, condition)

    def fixed_from_astro_bahai(self, b_date:tuple) -> float:
        """
        (defun fixed-from-astro-bahai (b-date)
          ;; TYPE bahai-date -> fixed-date
          ;; Fixed date of Baha’i date b-date.
          (let* ((major (bahai-major b-date))
                 (cycle (bahai-cycle b-date))
                 (year (bahai-year b-date))
                 (month (bahai-month b-date))
                 (day (bahai-day b-date))
                 (years           ; Years from epoch
                  (+ (* 361 (1- major))
                     (* 19 (1- cycle))
                     year)))
            (cond ((= month 19)   ; last month of year
                   (+ (astro-bahai-new-year-on-or-before
                       (+ bahai-epoch
                          (floor (* mean-tropical-year
                                    (+ years 1/2)))))
                      -20 day))
                  ((= month ayyam-i-ha)
                                  ; intercalary month, between 18th & 19th
                   (+ (astro-bahai-new-year-on-or-before
                       (+ bahai-epoch
                          (floor (* mean-tropical-year
                                    (- years 1/2)))))
                      341 day))
                  (t (+ (astro-bahai-new-year-on-or-before
                         (+ bahai-epoch
                            (floor (* mean-tropical-year
                                      (- years 1/2)))))
                        (* (1- month) 19)
                        day -1)))))
        """
        kull_i_shay = b_date[0]
        vahid = b_date[1]
        year = b_date[2]
        month = b_date[3]
        day = b_date[4]
        years = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year

        if month == 19:
            result = self.find_moment_of_equinoxes_or_solstices(
                self.BAHAI_EPOCH + math.floor(
                    self.MEAN_TROPICAL_YEAR * (years + 0.5))) - 20 + day
        elif month == self.AYYAM_I_HA:
            result = self.find_moment_of_equinoxes_or_solstices(
                self.BAHAI_EPOCH + math.floor(
                    self.MEAN_TROPICAL_YEAR * (years - 0.5))) + 341 + day
        else:
            result = self.find_moment_of_equinoxes_or_solstices(
                self.BAHAI_EPOCH + math.floor(
                    self.MEAN_TROPICAL_YEAR * (years - 0.5))) + (
                month - 1) * 19 + day - 1

        return math.floor(result)

    def astro_bahai_from_fixed(self, date:float) -> tuple:
        """
        (defun astro-bahai-from-fixed (date)
          ;; TYPE fixed-date -> bahai-date
          ;; Astronomical Baha’i date corresponding to fixed date.
          (let* ((new-year (astro-bahai-new-year-on-or-before date))
                 (years (round (/ (- new-year bahai-epoch)
                                  mean-tropical-year)))
                 (major (1+ (quotient years 361)))
                 (cycle (1+ (quotient (mod years 361) 19)))
                 (year (1+ (mod years 19)))
                 (days        ; Since start of year
                  (- date new-year))
                 (month
                  (cond
                   ((>= date (fixed-from-astro-bahai
                              (bahai-date major cycle year 19 1)))
                              ; last month of year
                    19)
                   ((>= date
                        (fixed-from-astro-bahai
                         (bahai-date major cycle year ayyam-i-ha 1)))
                              ; intercalary month
                    ayyam-i-ha)
                   (t (1+ (quotient days 19)))))
                 (day (- date -1
                         (fixed-from-astro-bahai
                          (bahai-date major cycle year month 1)))))
            (bahai-date major cycle year month day)))
        """
        new_year = self.find_moment_of_equinoxes_or_solstices(date)
        #new_year = self.astro_bahai_new_year_on_or_before(date)
        years = round((new_year - self.BAHAI_EPOCH) / self.MEAN_TROPICAL_YEAR)
        kull_i_shay = self.QUOTIENT(years, 361) + 1
        vahid = self.QUOTIENT(years % 361, 19) + 1
        year = (years % 19) + 1
        days = date - new_year

        if date >= self.fixed_from_astro_bahai(
            (kull_i_shay, vahid, year, 19, 1)):
            month = 19
        elif date >= self.fixed_from_astro_bahai(
            (kull_i_shay, vahid, year, self.AYYAM_I_HA, 1)):
            month = self.AYYAM_I_HA
        else:
            month = self.QUOTIENT(days, 19) + 1

        day = date + 1 - self.fixed_from_astro_bahai(
            (kull_i_shay, vahid, year, month, 1))
        return (kull_i_shay, vahid, year, month, day)

    def naw_ruz(self, b_year):
        """
        Return the Badi date for Naw-Ruz from the given Badi year.
        """
        tee = self.BAHAI_EPOCH + b_year - 1
        new_year = self.find_moment_of_equinoxes_or_solstices(tee)
        years = round((new_year - self.BAHAI_EPOCH) / self.MEAN_TROPICAL_YEAR)
        kull_i_shay = self.QUOTIENT(years, 361) + 1
        vahid = self.QUOTIENT(years % 361, 19) + 1
        year = (years % 19) + 1
        days = tee - new_year

        #print(tee, new_year, years, days)
        return (kull_i_shay, vahid, year, 0, 0)

    def naw_ruz_from_gregorian_year(self, g_year):
        """
        (defun naw-ruz (g-year)
          ;; TYPE gregorian-year -> fixed-date
          ;; Fixed date of Baha’i New Year (Naw-Ruz) in Gregorian
          ;; year g-year.
          (astro-bahai-new-year-on-or-before
           (gregorian-new-year (1+ g-year))))
        """
        return self.find_moment_of_equinoxes_or_solstices(
            self._gc.gregorian_new_year(g_year + 1))

    def feast_of_ridvan(self, g_year):
        """
        (defun feast-of-ridvan (g-year)
          ;; TYPE gregorian-year -> fixed-date
          ;; Fixed date of Feast of Ridvan in Gregorian year g-year.
          (+ (naw-ruz g-year) 31))
        """
        return self.nam_ruz(g_year) + 31

    def birth_of_the_bab(self, g_year):
        """
        (defun birth-of-the-bab (g-year)
          ;; TYPE gregorian-year -> fixed-date
          ;; Fixed date of the Birthday of the Bab
          ;; in Gregorian year g-year.
          (let* ((ny              ; Beginning of Baha’i year.
                  (naw-ruz g-year))
                 (set1 (bahai-sunset ny))
                 (m1 (new-moon-at-or-after set1))
                 (m8 (new-moon-at-or-after (+ m1 190)))
                 (day (fixed-from-moment m8))
                 (set8 (bahai-sunset day)))
            (if (< m8 set8)
                (1+ day)
              (+ day 2))))
        """
        ny = self.naw_ruz(g_year)
        set1 = self.bahai_sunset(ny)
        m1 = self.new_moon_at_or_after(set1)
        m8 = self.new_moon_at_or_after(m1 + 190)
        day = self.fixed_from_moment(m8)
        set8 = self.bahai_sunset(day)
        return day + 1 if m8 < set8 else dat + 2

    def _find_month(self, tee):
        """
        Find the Badi month from an R.D. moment.
        """


        return

    def _is_leap_year(self, date) -> bool:
        """
        Return a boolean True if a Badi leap year, False if not.

        :param date: This value can be either the Badi year or a tuple
                     as in (Kull-i-Shay, Vahid, year).
        :type date: int or tuple
        :return:
        :rtype: bool
        """
        if isinstance(date, tuple):
            assert len(date) >= 3, ("If tuple it must be the Kull-i-Shay', "
                                    f"Váḥid, and year, found {date}")
            year = (date[0] - 1) * 361 + (date[1] - 1) * 19 + date[2]
        else:
            year = date

        # 1. Find the day of Naw-Ruz for the current year
        # 2. Find the day of Naw-Ruz for the next year
        # 3. Calculate the difference between the two days found above.
        # 4. If the number of days is 366 it's a leap year


        return

    def jd_from_badi_date(self, date:tuple) -> float:
        """
        Convert a Badi date to Julian day count with the 1582 10, 15
        correction.
        """
        t_len = len(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hour = date[3] if t_len > 3 and date[3] is not None else 0
        minute = date[4] if t_len > 4 and date[4] is not None else 0
        second = date[5] if t_len > 5 and date[5] is not None else 0
        #if (year, month, day) >= (-262, 12, 2): # Gregorian (1582, 10, 15)

        if month == 0: # Ayyam-i-Ha
            d = 18 * 19 + day
        elif month < 19:
            d = (month - 1) * 19 + day
        else: # month == 19:
            d = 18 * 19 + 4 + day

        print(date, self.JULIAN_YEAR * (year - 1), d)
        return self.BADI_EPOCH - 1 + math.floor(
            self.JULIAN_YEAR * (year - 1)) + d

    def date_from_b_date(self, b_date:tuple) -> tuple:
        """
        Convert a b_date to a (year, month, day, hour, minute, second) day.
        """
        self._check_valid_badi_month_day(b_date)
        t_len = len(b_date)
        kull_i_shay = b_date[0] # 361 years (19 * 19)
        vahid = b_date[1] # 19 years
        year = b_date[2]
        month = b_date[3]
        day = b_date[4]
        hour = b_date[5] if t_len > 5 and b_date[5] is not None else 0
        minute = b_date[6] if t_len > 6 and b_date[6] is not None else 0
        second = b_date[7] if t_len > 7 and b_date[7] is not None else 0
        y = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        return y, month, day, hour, minute, second

    def b_date_from_date(self, date:tuple) -> tuple:
        """
        Convert a date to a
        (kull_i_shay, vahid, year, month, day, hour, minute, second) day.
        """
        t_len = len(date)
        year = date[0]
        month = date[1]
        day = date[2]
        hour = date[3] if t_len > 3 and date[3] is not None else 0
        minute = date[4] if t_len > 4 and date[4] is not None else 0
        second = date[5] if t_len > 5 and date[5] is not None else 0
        k = year / 361
        k0 = self._truncate_decimal(k % 1, 6)
        v = k0 / 19 * 361
        kull_i_shay = math.floor(k)

        if v == 0: # If there is no fraction in v
            vahid = 19
            y = 19
        else:
            kull_i_shay += 1
            vahid = math.ceil(v)
            y = math.ceil(self._truncate_decimal(v % 1, 6) * 19)

        b_date = (kull_i_shay, vahid, y, month, day, hour, minute, second)
        self._check_valid_badi_month_day(b_date)
        return b_date

    ## def date_from_kvymdhms(self, b_date:tuple) -> tuple:
    ##     """
    ##     Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second)
    ##     into a (Kull-i-Shay, Váḥid, year, month, day.partial) date.
    ##     """
    ##     self._check_valid_badi_month_day(b_date)
    ##     t_len = len(b_date)
    ##     kull_i_shay = b_date[0] # 361 years (19 * 19)
    ##     vahid = b_date[1] # 19 years
    ##     year = b_date[2]
    ##     month = b_date[3]
    ##     day = b_date[4]
    ##     hour = b_date[5] if t_len > 5 and b_date[5] is not None else 0
    ##     minute = b_date[6] if t_len > 6 and b_date[6] is not None else 0
    ##     second = b_date[7] if t_len > 7 and b_date[7] is not None else 0
    ##     day += self.HR(hour) + self.MN(minute) + self.SEC(second)
    ##     return (kull_i_shay, vahid, year, month, day)

    ## def kvymdhms_from_date(self, b_date:tuple) -> tuple:
    ##     """
    ##     Convert ((Kull-i-Shay, Váḥid, year, month, day.partial) into
    ##     (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second).
    ##     """
    ##     self._check_valid_badi_month_day(date)
    ##     t_len = len(b_date)
    ##     kull_i_shay = b_date[0] # 361 years (19 * 19)
    ##     vahid = b_date[1] # 19 years
    ##     year = b_date[2]
    ##     month = b_date[3]
    ##     day = b_date[4]
    ##     hd = self.PARTIAL_DAY_TO_HOURS(day)
    ##     hour = math.floor(hd)
    ##     md = self.PARTIAL_HOUR_TO_MINUTE(hd)
    ##     minute = math.floor(md)
    ##     second = self.PARTIAL_MINUTE_TO_SECOND(md)
    ##     return (kull_i_shay, vahid, year, month, math.floor(day),
    ##             hour, minute, second)

    def _check_valid_badi_month_day(self, b_date:tuple) -> bool:
        """
        Check that the month and day values are valid.
        """
        cycle = 19
        t_len = len(b_date)
        kull_i_shay = b_date[0] # 361 years (19 * 19)
        vahid = b_date[1] # 19 years
        year = b_date[2]
        month = b_date[3]
        day = b_date[4]
        hour = b_date[5] if t_len > 5 and b_date[5] is not None else 0
        minute = b_date[6] if t_len > 6 and b_date[6] is not None else 0
        second = b_date[7] if t_len > 7 and b_date[7] is not None else 0
        assert 1 <= vahid <= cycle, (
            f"The number of Váḥids in a Kull-i-Shay’ should be >= 1 or <= 19, "
            f"found {vahid}")
        assert 1 <= year <= cycle, (
            f"The number of years in a Váḥid should be >= 1 or <= 19, "
            f"found {year}")
        assert 0 <= month <= cycle, (
            f"Invalid month '{month}', should be 0 - 19.")

        # This is Ayyām-i-Hā and could be 4 or 5 days depending on leap year.
        if month == 0:
            pass # *** TODO *** Test for a valid Badi leap year.
            #cycle -= 14 if self._is_leap_year(year) else 15

        assert 1 <= day <= cycle, (
            f"Invalid day '{day}' for month '{month}' and year '{year}' "
            f"should be 1 - {cycle}.")
        assert 0 <= hour < 24, (f"Invalid hour '{hour}' it must be "
                                f"0 <= {hour} < 24")
        assert 0 <= minute < 60, (f"Invalid minute '{minute}' should be "
                                  f"0 <= {minute} < 60.")

        if any((hour, minute, second)):
            assert not day % 1, ("If there is a part day then there can be no "
                                 "hours, minutes, or seconds.")

        if any((minute, second)):
            assert not hour % 1, (
                "If there is a part hour then there can be no minutes or "
                "seconds.")

        if second:
            assert not minute % 1, (
                "If there is a part minute then there can be no seconds.")

    @property
    def latitude(self):
        """
        (defun latitude (location)
          ;; TYPE location -> half-circle
          (first location))
        """
        return self.BAHAI_LOCATION[0]

    @property
    def longitude(self):
        """
        (defun longitude (location)
          ;; TYPE location -> circle
          (second location))
        """
        return self.BAHAI_LOCATION[1]

    @property
    def elevation(self):
        """
        (defun elevation (location)
          ;; TYPE location -> distance
          (third location))
        """
        return self.BAHAI_LOCATION[2]

    @property
    def zone(self):
        """
        (defun zone (location)
          ;; TYPE location -> real
          (fourth location))
        """
        return self.BAHAI_LOCATION[3]
