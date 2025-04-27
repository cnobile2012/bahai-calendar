.. -*-coding: utf-8-*-

.. role:: color-violet
.. role:: color-red

************
Installation
************

=======================================
Installing a Virtual Python Environment
=======================================

-------------------------
Why a Virtual Environment
-------------------------

Python virtual environments are used regularly by seasoned Python programmers
however, beginners may not know about this feature rich tool. Virtualenv is a
tool which allows the creation of isolated python environments. So what do we
get from isolated environments? Lets say you are developing a project that
needs version 1 of some library. You install it globally on your computer. A
while later you start work on another project that requires the same library,
but version 2. If you install version 2 globally, as you did before, it may
break the first project you were working on. This is where virtual environments
comes to the rescue, every project is in it's own isolated environment and you
no longer need to install python packages as sudo (root) user. Which means the
other advantage of virtual environments is that it's installed in your user
account not in the root of the system.

This API was tested to work with Python versions 3.10 to 3.13. I strongly
recommend writing any new code with the latest version of Python.

----------------------------------------------------
Building a Development Environment for your Projects
----------------------------------------------------

This build information works on Debian derived Linux operating systems such as
Ubuntu, Linux Mint, Kaii Linux, and others.

.. code-block:: console

   $ sudo apt install build-essential python3.12 python3-setuptools \
                      git virtualenvwrapper

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

   $ . .bashrc # 'source .bashrc' Works also

Create a VE (Virtual Environment) for your project. The VE name can be
whatever you want and does not need to match the actual project's name, but it
might be a good idea to keep it short so that you can remember it. Use
whichever Python version you have in the command below.

.. code-block:: console

   $ cd /path/to/your_project
   $ mkvirtualenv -p python3.11 <VE name>

After the initial creation of the VE you can use these commands to activate
and deactivate a VE.

.. code-block:: console

   $ workon <VE name>
   $ deactivate

Next you will need to install all the Python packages that your project
depends on. Many of them will be in the pip repository at
`PyPi Repository <https://pypi.org/>`_.

=======================
Installing badidatetime
=======================

To install ``badidatetime`` in your virtual environment enter the following on
the command line. Be sure your virtual environment is activated before doing
this.

.. code-block:: console

   $ pip install badidatetime
   or
   $ pip install git+https://github.com/cnobile2012/bahai-calendar.git

If you are working on ``badidatetime`` itself, then ``badidatetime`` is the
project you are working on and you'll need to install the ``development.txt``
file mentioned below. You may want to fork my version first. This is advanced
usage so you and will need to have your own GitHub account for this to work
properly.

.. code-block:: console

   $ cd /path/to/where/your/project/will/be/rooted
   $ git clone git@github.com:cnobile2012/bahai-calendar.git

If all the correct system packages have been installed you can now setup the
virtual environment that ``badidatetime`` requires. **Change the Python version
below to the one you are using.**

.. code-block:: console

   $ mkvirtualenv -p python3.13 calendar
   $ workon calendar
   $ pip install -r requirements/development.txt

That should be it. If you have any issues please check all the instructions
before contacting me.

=====================
Building badidatetime
=====================

.. note::

   This part is only needed by me and people that have forked the repository
   and want to contribute to this project.

Versions are controlled by the **include.mk** file, so this is the only place
to change version information. This file is read by various scripts in the
repository. In other words *DO NOT* update the **pyproject.toml** directly,
there is a script (config.py) that is run from the **Makefile** that will
update everything correctly.

Follow these steps to create the correct versioning and package for uploading
to `pypi`.

   1. Run local tests.
   2. Commit and push all code relating to the new release.
   3. Check that the `GitHub` tests pass.
   4. Update the **include.mk** file with the new version information.
   5. Commit and push the **include.mk** file. :color-violet:`(Step 4 and 5
      can also be done as part of step 2.)`
   6. Check again that the `GitHub` tests pass.
   7. Create the version tag for the branch you are working in and push.

      .. code-block:: console

         $ git tag -a <tagname> -m "Comment about this tag."
         $ git push origin <tagname>

   8. Upload to the `pypi` test site.

      .. code-block:: console

         $ make upload-test TEST_TAG=rc1

   9. Go to your account on the `pypi test site <https://test.pypi.org/>`_ to
      check if it is there. :color-red:`(For errors see below.)`
   10. The `pyproject.toml` files gets updated updating to the `pypi` test and
       main sites, so it will need to be committed and pushed afterwards. This
       file is not in the `badidatetime` build so it can lag behind with no
       problems.
   11. Assuming everything went as expected then upload to the main `pypi`
       site.

       .. code-block:: console

         $ make upload

   11. Go to your account on the `pypi site <https://pypi.org/>`_ to check if
       it is there.
   12. Done, the new version is published.

If you get errors during the upload to the test `pypi` site and need to fix any
files that need to be checked in you will need to move the tag the *HEAD* of
the branch afterwards.

   1. Run local tests.
   2. Commit and push all code relating to the errors found above.
   3. Check that the `GitHub` tests pass.
   4. Move the version tag. :color-red:`The commit hash can be just the first 7
      characters of the full hash.`

      .. code-block:: console

         $ git tag -a <tagname> <HEAD commit hash> -f -m "Comment"
         $ git push origin --tags -f

   5. Then continue by redoing number 8 above.
