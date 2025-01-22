#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# .github/workflows/setup_script.py 
#

import os
import sys

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import enable_geocoder

enable_geocoder(True)
