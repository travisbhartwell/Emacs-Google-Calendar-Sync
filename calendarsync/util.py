# Copyright (C) 2008-2010 by CiscoRx
# Copyright (C) 2010 by Travis B. Hartwell
#
# This file is part of Emacs Google Calendar Sync
#
# Emacs Google Calendar Sync is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# Emacs Google Calendar Sync is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Emacs Google Calendar Sync.  If not, see
# <http://www.gnu.org/licenses/>.

"""Various general utility functions that don't belong elsewhere."""

import sys

import calendarsync.config

# Module-level variables not used elsewhere
_logging_initialized = False
_verbose_logging = None



def init_logging(configuration):
    """Extracts necessary configuration from the global configuration
    and initializes the logging state based on this.

    Right now, it is just either on or off, depending on the verbosity
    level.
    """
    global _logging_initialized, _verbose_logging

    _verbose_logging = configuration[calendarsync.config.VERBOSE_VAR]
    _logging_initialized = True



def log(message):
    """Log the given message.

    Right now, if verbose logging is set, just print to standard out.
    Later, add logging to a file, etc."""
    if _verbose_logging:
        if _logging_initialized:
            print message
        else:
            print >> sys.stderr, "Attempted to use logging without \
first initializing."
            # Turn '3' into an appropriate constant package-wide
            sys.exit(3)
