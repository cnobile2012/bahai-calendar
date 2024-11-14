#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/chart/year_start.py
#
# Jan Greis -> jan.r.greis@gmail.com
#

import os
import re
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)


class YearStart:
    """
    This script parses a test file and creates a CSV file that can be
    imported into a spreadsheet program.

    This data shows the differences between the Badi Calendar created
    by Jan R. Greis and my code.
    """
    TXT_FILE = os.path.join(PWD, 'YearStart.txt')
    CSV_FILE = os.path.join(PWD, 'YearStart.csv')
    C_RE_TEXT = re.compile(r'(\w+) +(\w+) +(\w+) +(\w+) +(\w+) +(\w+)'
                           r' +(\w+) +(\w+) +(\w+)')
    C_RE_DAT0 = re.compile(r'(\(.+\)) +(\(.+\)) +(\d) +([\w`]+) +(\w+) +(\w+)'
                           r' +(\d{3}) +(\d{3}) +(\d)')
    C_RE_WHO = re.compile(r'(\w+) +(\w+)')
    C_RE_DAT1 = re.compile(r'(\w+) +(\w+) +([\w/]+)(?: +(\w+)? +([\w/]+)?)?')

    def start(self):
        data = []
        csv = ""

        with open(self.TXT_FILE, mode='r', newline='') as txt_file:
            for line in txt_file:
                if (item := self.C_RE_TEXT.search(line)) is not None:
                    data.append(item.groups())
                elif (item := self.C_RE_DAT0.search(line)) is not None:
                    data.append(item.groups())
                elif (item := self.C_RE_DAT1.search(line)) is not None:
                    data.append([field if field is not None else ""
                                 for field in item.groups()])
                elif (item := self.C_RE_WHO.search(line)) is not None:
                    item = item.groups()
                    data.append(["", item[0], "", item[1]])

        for item in data:
            csv += '|'.join(item) + '\n'

        with open(self.CSV_FILE, 'w') as csv_file:
            csv_file.write(csv)


if __name__ == "__main__":
    ys = YearStart()
    ys.start()
    sys.exit(0)
