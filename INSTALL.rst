************
Installation
************

Installing a Virtual Python Environment
=======================================

Why a Virtual Environment
-------------------------

Python virtual environments are used regularly by seasoned Python programmers
however, beginners may not know about this feature rich tool. Virtualenv is a
tool which allows the creation of isolated python environments. So what do we
get from isolated environments? Lets say you are developing a project that
needs version 1 of some library. You install it globally on the RPI. A while
later you start work on another project that requires the same library, but
version 2. If you install version 2 globally, as you did before, it will
invalidate the first project you were working on. This is where virtual
environments comes to the rescue, every project is in it's own isolated
environment and you no longer need to install python packages as sudo (root)
user. Which means the other advantage of virtual environments is that it's
installed in your user account not in the root of the system.

This API was tested to work with Python versions 3.10 to 3.13. I strongly
recommend writing any new code with the latest version of Python.

Building a Development Environment for your Projects
----------------------------------------------------

.. code-block:: console

    $ sudo apt install build-essential python3.12 python3-setuptools git \
               virtualenvwrapper

The ``virtualenvwrapper`` package is a wrapper around ``virtualenv`` that
provides easy to use tools for ``virtualenv`` and will install ``virtualenv``
for you.

.. note::

   A directory is created in the user's home directory named
   ``.virtualenvs``. In there you'll be able to find all your project
   requirements and the packages you have installed for each of them.

Configure ``.bashrc`` in your user directory to auto load the
``virtualenvwrapper`` package. 

.. code-block:: console

    $ nano .bashrc

Then add the following lines to the bottom of the ``.bashrc`` file.

.. code-block:: bash

    # Setup the Python virtual environment.
    VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
    source /usr/share/virtualenvwrapper/virtualenvwrapper.sh

    $ . .bashrc

Create a VE (Virtual Environment) for your project. The VE name can be
whatever you want and does not need to match the actual project's name, but it
might be a good idea to keep it short so that you can remember it. Use
whichever Python version you have in the command below. 

.. code-block:: console

    $ cd /path/to/your_project
    $ mkvirtualenv -p python3.11 calendar

After the initial creation of the VE you can use these commands to activate
and deactivate a VE.

.. code-block:: console

    $ workon <your_project>
    $ deactivate

Next you will need to install all the Python packages that your project
depends on. Many of them will be in the pip repository at
`PyPi Repository <https://pypi.org/>`_.

Installing badidatetime
=======================

To install ``badidatetime`` in your virtual environment enter the
following on the command line. Be sure your virtual environment is activated
before doing this.

.. code-block:: console

    $ pip install badidatetime # Not available as of 2025-01-18
    or
    $ pip install git+https://github.com/cnobile2012/bahai-calendar.git

If you are working on ``badidatetime`` itself, then ``badidatetime`` is the
project you are working on and you'll need to install the ``development.txt``
file mentioned below. You may want to fork my version first. This is advanced
usage you and will need to have your own git account for this to work properly.

.. code-block:: console

    $ cd /path/to/where/your/project/will/be/rooted
    $ git clone git@github.com:cnobile2012/bahai-calendar.git

If all the correct system packages have been installed you can now setup the
virtual environment that ``badidatetime`` requires.

.. code-block:: console

    $ workon calendar
    $ pip install -r requirements/development.txt

That should be it. If you have any issues please check all the instructions
before contacting me.
