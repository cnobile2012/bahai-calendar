.. -*-coding: utf-8-*-

========
datetime
========

-----------------
Public Attributes
-----------------

badidatetime.datetime.MINYEAR : int
   Constant indicating the minimum year this API can represent. 

badidatetime.datetime.MAXYEAR : int
   Constant indicating the maximum year this API can represent.

badidatetime.datetime.BADI_IANA : str
   Constant indicating the IANA zone Asia/Terhan

badidatetime.datetime.BADI_COORD : tuple
   Constant indication the latitude, longitude, amd zone offset for Tehran,
   Iran.

badidatetime.datetime.GMT_COORD : tuple
   Constant indication the latitude, longitude, amd zone offset for Greenwich
   UK.

badidatetime.datetime.LOCAL_COORD : tuple
   Constant indication the latitude, longitude, amd zone offset for the local
   locale.

badidatetime.datetime.UTC : badidatetime.datetime.timezone
   Constant indication the timezone object information for Greenwich UK.

badidatetime.datetime.BADI : badidatetime.datetime.timezone
   Constant indication the timezone object information for Tehran, Iran.

badidatetime.datetime.LOCAL : badidatetime.datetime.timezone
   Constant indication the timezone object information for the local locale.

----------------
Module Functions
----------------

.. autofunction:: badidatetime.datetime._cmp
.. autofunction:: badidatetime.datetime._divide_and_round
.. autofunction:: badidatetime.datetime._check_offset
.. autofunction:: badidatetime.datetime._check_tzinfo_arg
.. autofunction:: badidatetime.datetime._cmperror
.. autofunction:: badidatetime.datetime._format_time
.. autofunction:: badidatetime.datetime._format_offset
.. autofunction:: badidatetime.datetime._check_tzname
.. autofunction:: badidatetime.datetime._fromutc
.. autofunction:: badidatetime.datetime._module_name

-------
Classes
-------

.. automodule:: badidatetime.datetime
   :members:
   :undoc-members:
   :show-inheritance:
