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

"""Constants and other shared items related to configuration."""

import os.path


# Constants for command line options
VERBOSE_VAR = 'verbose'

# Default values, if any, for command line options
DEFAULT_VERBOSITY = False

# Constants for default_config items
EMACS_DIARY_PATH_VAR = 'filepath'
GCAL_USERNAME_VAR = 'username'
GCAL_PASSWORD_VAR = 'password'

# Default values for configuration, if any
DEFAULT_EMACS_DIARY_FILENAME = "diary"
DEFAULT_EMACS_DIARY_PATH = \
    os.path.expanduser(os.path.join("~",
                                    DEFAULT_EMACS_DIARY_FILENAME))

# List of known configuration items
CONFIG_KEYS = set((VERBOSE_VAR,
                   EMACS_DIARY_PATH_VAR,
                   GCAL_USERNAME_VAR,
                   GCAL_PASSWORD_VAR))



# The default configuration
DEFAULT_CONFIG = {EMACS_DIARY_PATH_VAR: DEFAULT_EMACS_DIARY_PATH,
                  VERBOSE_VAR: DEFAULT_VERBOSITY}



def update_configuration(config_dict):
    """Gets an updated configuration with the defaults applied if
    necessary.  Includes removing unknown configuration keys.
    """
    # For now, a shallow copy is sufficient
    default_copy = DEFAULT_CONFIG.copy()
    config_dict_copy = config_dict.copy()

    keys_set = set(config_dict_copy.keys())
    invalid_keys = keys_set - CONFIG_KEYS

    for key in invalid_keys:
        del config_dict_copy[key]

    default_copy.update(config_dict_copy)

    return default_copy





