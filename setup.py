import os
import re
from setuptools import setup

def version():
    regex = r'(?m)(^{}[\s]*=[\s]*(?P<ver>\d*)$)'

    with open(os.path.join(os.path.dirname(__file__), 'include.mk')) as f:
        ver = f.read()

    major = re.search(regex.format('MAJORVERSION'), ver).group('ver')
    minor = re.search(regex.format('MINORVERSION'), ver).group('ver')
    patch = re.search(regex.format('PATCHLEVEL'), ver).group('ver')
    # Look for a tag indicating a pre-release candidate. ex. rc1
    env_value = os.environ.get('PR_TAG', '')
    return "{}.{}.{}{}".format(major, minor, patch, env_value)

with open(os.path.join(os.path.dirname(__file__), 'README.rst')) as readme:
    README = readme.read()

# Allow setup.py to be run from any path.
os.chdir(os.path.normpath(os.path.join(os.path.abspath(__file__), os.pardir)))

setup(
    name='Badi Datetime Package',
    version=version(),
    packages=['badidatetime',],
    include_package_data=True,
    license='MIT',
    description=('Badi Calendar Datetime API'),
    long_description=README,
    url='https://github.com/cnobile2012/bahai-calendar',
    author='Carl J. Nobile',
    author_email='carl.nobile@gmail.com',
    classifiers=[
        #'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 3.11',
        'Programming Language :: Python :: 3.12',
        #'Topic :: Internet :: WWW/HTTP',
        #'Topic :: Internet :: WWW/HTTP :: Dynamic Content',
        ],
    install_requires=[
        'tzlocal', 'tzdata', 'geocoder',
        ],
    )
