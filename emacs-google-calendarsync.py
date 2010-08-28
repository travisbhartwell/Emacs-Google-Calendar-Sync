#!/usr/bin/python
# emacs-google-calendarsync revision 77
# written and maintained by CiscoRx@gmail.com
# DISCLAIMER: If this script should fail or cause any damage then I,
# ciscorx@gmail.com, assume full liability; feel free to sue me for
# every penny I've got, the number of pennies of which should be just
# enough to fit into a small envelope to mail to you.  Hopefully, it
# will also cover postage.
import datetime
import getopt
import os
import re
import shelve
import sys
import time

import atom.service
import gdata.calendar.service

# Templates (OKAY, messy, but will clean up later)
from templates import *

# optional password.
# If this value is used, make sure to change the
# permissions on this script to execute only, i.e. chmod 111
# emacs-google-calendarsync.  If this value is not supplied, the gmail
# password will be prompted upon execution.
PASSWORD = ''

# None = get gmtoffset from python time.timezone.  globalvar_GMTOFFSET
# = 6  refers to central timezone
GMT_OFFSET = None

# Time zone
TZID = 'America/Denver'

# Location of emacs diary (if this is left blank, the location will be guessed)
DIARY_FILE = ''

# Location to put the shelve.dat file, which contains the schedule
# from the last sync. (if this is left blank, the location will be the
# directory where this script resides.) The filename of the shelve
# file will contain the google calendar username
SHELVE_FILE = ''

# If no end time is provided in diary entry, then default to this
# specified event duration, which in this case is 60 min
DEFAULT_EVENT_DURATION = 60

# number of days prior to the current date before which entries get
# deleted; they wont be deleted from the google calendar, just from
# the emacs diary.  This feature is currently not implemented, and
# changing this value has no effect.
DELETE_OLD_ENTRIES_OFFSET = 90

# entry contention happens when the same diary and its respective
# google calendar entries are both modified before a sync. 0=prompt
# from list of contenders 1=automatic best guess, 0=prompt from list
# of contenders, 2=do nothing; allowing for both entries to exist in
# both gcal and diary
ENTRY_CONTENTION = 0

# this will allow for multiple read-only calendars to be viewed in the
# same dairy.  The multiple calendar support is not yet implemented
DISCARD_ENTRY_FLAG = '#@!z8#'

# which format to use for entries synced from gcal to diary?: 0 =
# monthabbreviated day, year      1 = month/day/year
DEFAULT_NON_RECURRING_FORMAT = 0

# True/False: do we want entries synced from gcal to diary to be
# written with time first then title, or vice versa
FORMAT_TIME_BEFORE_TITLE_IN_DIARY = True

# True/False: True = no modifications will be made to the google
# calendar at all, but will tell you if changes have been made,  False
# = two way sync between the Emacs diary and the Google Calendar.
# This global variable can be set by the -r option.  This is much like
# option -i, --init, only it tells you  -- No Changes if the Google
# Calendar content was the same as it was last sync
READ_FROM_GOOGLE_ONLY = False

# True/False:  True = If the descriptions, that represent recurring
# event parameters, which are written above each recurrence entry in
# the diary file, have values that have been altered from editing the
# diary file, such as a changed UNTIL date or INTERVAL values, then
# their respective event entries are also changed as such.  False =
# Recurring event descriptions become read-only and modifying them
# will have no effect on the diary entry.
CHANGING_RECURRING_EVENT_DESCRIPTIONS_EDITS_THE_ENTRY = False

# True/False: True = Include comments taken from Google Calendar to
# each diary entry   False = No comments
DISPLAY_COMMENTS = True

if GMT_OFFSET is None:
    GMT_OFFSET = time.timezone / 60 / 60

# set this to 1 when daylight savings time is NOT in efffect, starting
# in spring.  Set to 0 when daylight savings time is in effect,
# starting in autumn
GMT_OFFSET -= 1

DICTIONARY_DEFINED_TYPE = type({})

# Date and Time Format Constants
ISO_DATE_FMT = '%Y-%m-%dT%H:%M:%S.000Z'
TIMESTAMP_FMT = '%Y-%m-%dT%H:%M:%S'
ORG_DATE_FMT = '[%Y-%m-%d %a %H:%M:%S]'
YMD_DATE_FMT = '%Y-%m-%d'

# General String Constants
NEWLINE = '\n'
NEWLINE_AND_END = NEWLINE + 'end'

# General date and time constants
MONTH_ABBRS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
               'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
DAY_ABBRS = ['SU', 'MO', 'TU', 'WE', 'TH', 'FR', 'SA']
DAY_NUMBERS = dict([(abbr, i) for i, abbr in enumerate(DAY_ABBRS)])

DEFAULT_UPDATED_G_TIME = "1970-10-10T10:30:30.000Z"

# Other useful Constants
COMMENT_STATUS_ENUM = {'A': 'ACCEPTED',
                       'D': 'DECLINED',
                       'I': 'INVITED',
                       'T': 'TENTATIVE'}

EVENT_ORPHANED = "orphaned"
EVENT_CANCELED = "canceled"
EVENT_NOT_ORPHANED_OR_CANCELED = ""

def strip_list(lst):
    return [s.strip() for s in lst]


def pad_newlines_with_space(source):
    """ This function is used on the CONTENT field so that when
    written to the emacs diary, multiple lines of description can be
    recognized as pertaining to a single respective event."""
    return pad_newlines_with_n_spaces(source, 1)


def pad_newlines_with_n_spaces(source, n):
    """ This function is used on the CONTENT field so that when
    written to the emacs diary, multiple lines of description can be
    recognized as pertaining to a single respective event. N signifies
    margin space """
    lines = source.split(NEWLINE)
    if len(lines) < 2:
        return source
    rest = lines[1:]
    target = []
    target.append(lines[0] + NEWLINE)
    for line in rest:
        if len(line) > 0 and line[0] != ' ':
            target.append(''.zfill(n).replace('0', ' ') + line + NEWLINE)
    return ''.join(target)


def pad_all_newlines_with_space(source):
    """ This function is used on CONTENT of parsed gcal entry.  """
    lines = source.split(NEWLINE)
    target = []
    for line in lines:
        if len(line) > 0:
            target.append(' ' + line.strip() + NEWLINE)
    return ''.join(target)


def remove_newlines_space_padding(source):
    """ This function is used on the CONTENT field of an entry being
    sent to gcal."""
    lines = source.split(NEWLINE)
    if len(source) == 0:
        return ""
    target = []
    for line in lines:
        target.append(line.strip() + NEWLINE)
    return ''.join(target).strip()


def remove_all_extra_spaces(a_string):
    return re.sub("[ ]+", " ", a_string)


def strip_extra_newlines(a_string):
    """ Use this function on the 'fullentry' field before hashing to
    get a key, as sometimes new lines can get into the diary and mess
    up the hash."""
    pos = a_string.find('\n\n')
    while  pos != -1:
        a_string = a_string.replace('\n\n', NEWLINE)
        pos = a_string.find('\n\n')
    if a_string == NEWLINE:
        a_string = " "
    elif len(a_string) > 1 and a_string[-1] == NEWLINE:
        a_string = a_string[0:-1]
    return a_string


def escape_string(a_string):
    """ Use this function in place of re.escape(); re.escape() does
    not seem to work right...it is only used in the load_template()
    function."""
    a_str = []
    target = ''
    for i in range(len(a_string)):
        if a_string[i] in ['(', ')', '*', '+', '.', '?']:
            a_str.append('\\')
        elif a_string[i] == "%":
            a_str.append('%')
        elif a_string[i] == "&":
            a_str.append('&')
        a_str.append(a_string[i])

    target = ''.join(a_str)
    return target


def remove_whitespace(a_string):
    a_string.strip()
    pattern = re.compile(r'\s+')
    a_string = pattern.sub(' ', a_string)
    a_string = a_string.replace(') (', ')(')
    return a_string


def load_match_vars(var):
    """The _mtch file must end in a new line.  Uppercase entries are
    recognized as variable names for pattern patching.  Lowercase
    entries are not recognized as matching variables, and their values
    are substituted in verbatim.  Any variable appearing more than
    once in an entry must have a 1 digit ordinal number appende to the
    variable name, incremented for each occurrence."""
    content = globals()[locals()['var']]

    lines = content.splitlines(True)
    key = ''
    identical_keys = []
    dic_data_types = {}
    for line in lines:
        space_index = line.find(" ")
        key = line[0:space_index]
        if key.islower():
            # lowercase keys dont get var names
            dic_data_types[key] = line[space_index + 1:]
        # more than one occurance of a variable in a pattern must have
        # a 1 digit ordinal number appended to the variable name
        elif key[:-1] in identical_keys:
            dic_data_types[key] = line[space_index + 1] + '?P=' + key[:-1] + ')'
        else:
            dic_data_types[key] = \
                line[space_index + 1] + \
                '?P<' + \
                key[:] + \
                '>' +  \
                line[space_index + 2:len(line) - 1]
        identical_keys.append(key)
    return dic_data_types


def load_ref_table(table_name):
    """ loads simple one level dictionary from a global variable.  The
    format is a simple 'key:value' with newlines separating each pair,
    and a mandatory single newline at the end"""
    content = globals()[locals()['table_name']]

    lines = content.splitlines(True)
    key = ''
    value = ''
    a_dict = {}
    for line in lines:
        key, value = line.split(':')
        value = value.strip()
        a_dict[key] = value
    return a_dict


def load_template(template_name, escape=True):
    """all whitespace thats longer than a single space is removed from
       template.  If whitespace is needed to be represented in a
       pattern, create a tag for it in lowercase letters, and create
       an entry in the _mtch file indicating what to sub in verbatim.
       note: the ^ may only be used in template to indicate beginning
       of string.  The EvaluateTemplate() function is used to create
       matching patterns using loadTemplates return value as an
       argument.
      """
    content = globals()[locals()['template_name']]

    test = content.splitlines(True)
    test = strip_list(test)
    case_string = ''.join(test)
    case_pattern = re.compile(r'<(.+?)>(.+?)</')
    template_cases = case_pattern.findall(case_string)
    pattern = re.compile(r'\<(\w+?)\>')
    tt_case = {}
    a_case_name = []
    case_name = ''
    escaped_string = ''
    for i in range(len(template_cases)):
        case_name = template_cases[i][0]
        if escape:
            # escape (,),%, &
            escaped_string = escape_string(template_cases[i][1])
        else:
            escaped_string = template_cases[i][1]
        temp_string = re.sub(pattern, r'%(\1)s', escaped_string)
        # make a template
        tt_case[case_name] = temp_string
        a_case_name.append(case_name)
    return tt_case


def evaluate_templates(at_template, match_var_name):
    """makes match pattern for future matches
       This function also calls load_match_vars() """
    dic_types = load_match_vars(match_var_name)
    dic_evaluated_templates_array = {}
    string_patterns = []
    for i in at_template.keys():
        p0 = at_template[i] % dic_types
        string_patterns.append(p0)
        dic_evaluated_templates_array[i] = (re.compile(p0, re.M | re.S))
    return dic_evaluated_templates_array, string_patterns


def deep_update(db, temp_db):
    """ overwrite any non dict type value and use {}.update on dict types """
    db_keys = db.keys()
    temp_db_keys = temp_db.keys()
    for key in db_keys:
        if key in temp_db_keys:
            if type(db[key]) == DICTIONARY_DEFINED_TYPE:
                db[key].update(temp_db[key])
            else:
                db[key] = temp_db[key]
    for key in temp_db_keys:
        if key not in db_keys:
            if type(db[key]) == DICTIONARY_DEFINED_TYPE:
                db[key] = temp_db[key].copy()
            else:
                db[key] = temp_db[key]


def parse_list_to_db(pattern, l_diary_array, keys):
    """ keys argument is a list of entrypids associated with each
    string in l_diary_array, respectively.   pat is a dictionary of
    patterns to check against.  The pattern cases are matched against
    a diary string in alphabetical order of case name, accepting the
    first pattern that is matched"""
    temp_db = {}
    entry_pid = ''
    mo_case_to_diary = {}
    pattern_keys = pattern.keys()
    pattern_keys.sort()
    for i in range(len(l_diary_array)):
        for idxCase in pattern_keys:
            mo_case_to_diary = pattern[idxCase].search(l_diary_array[i])
            # find only first match
            if mo_case_to_diary is not None:
                entry = {}
                entry = mo_case_to_diary.groupdict()
                entry_pid = keys[i]
                entry['entry_pid'] = entry_pid
                case_name = 'case_name-' + idxCase
                entry[case_name] = idxCase
                temp_db[entry_pid] = entry
                break
    return temp_db


def get_time_ranges(db, keys):
    time_ranges = []
    temp_record = {}
    for key in keys:
        temp_record = db[key]
        # workaround for having created an extra variable called
        # TIMERANGEII and TIMERANGEIII in detail_template
        temp_record_keys = [key for key in temp_record.keys() \
                                if key[:9] == 'TIMERANGE']

        if len(temp_record_keys) > 0:
            time_ranges.append(temp_record[temp_record_keys[0]])
        else:
            time_ranges.append('')
    return time_ranges


def update_details(db, details, keys):
    """  """
    template_details = load_template('detail_template')
    pattern_details, _ = evaluate_templates(template_details, 'detail_template_mtch')
    temp_db = parse_list_to_db(pattern_details, details, keys)

    deep_update(db, temp_db)
    time_ranges = get_time_ranges(db, keys)

    at_times_cases = load_template('times_template')
    pattern_times, _ = evaluate_templates(at_times_cases, 'times_template_mtch')
    temp_db = parse_list_to_db(pattern_times, time_ranges, keys)
    deep_update(db, temp_db)


def e_to_g_byday(byday):
    """ converts emacs calendar BYDAY field data to the google
    calendar BYDAY equivalent"""
    if byday == '':
        return ''
    bydayg = ''
    lines = byday.split(' ')
    for line in lines:
        bydayg = bydayg + DAY_ABBRS[int(line)] + ', '
    bydayg = bydayg[:-1]
    return bydayg


def g_to_e_byday(byday):
    """  converts google calendar BYDAY field data to the emacs
    calendar BYDAY equivalent"""
    if byday == '':
        return ''
    bydaye = ''
    lines = byday.split(', ')
    for line in lines:
        bydaye = bydaye + str(DAY_ABBRS.index(line.upper())) + ' '
    bydaye = bydaye[:-1]
    return bydaye


def e_to_g_month_abbr(month_abbr):
    months = {'Jan': '01', 'Feb': '02', 'Mar': '03', 'Apr': '04', 'May': '05',
              'Jun': '06', 'Jul': '07', 'Aug': '08', 'Sep': '09', 'Oct': '10',
              'Nov': '11', 'Dec': '12'}
    return months[month_abbr]


def g_to_e_month_abbr(month):
    return MONTH_ABBRS[int(month)-1]


def g_to_e_which_week(which_week_g):
    if which_week_g[0] == '-':
        which_week = which_week_g[0:2]
        which_day = which_week_g[2:4]
    else:
        which_week = which_week_g[0]
        which_day = which_week_g[1:3]
    which_day = DAY_NUMBERS[which_day]
    return which_week, which_day


def strip_time_string(time_string):
    time_string = time_string.replace(':', '')
    time_string = time_string.replace('-', '')
    if len(time_string) > 15:
        time_string = time_string[0:15]
    return time_string


def current_datetime_string():
    now = time.strptime(time.asctime())
    now = now[0:6]
    now_datetime = datetime.datetime(*now)
    now_string = now_datetime.isoformat()
    now_string = strip_time_string(now_string)
    return now_string


def ISO_to_org_time(iso_time):
    """ taking account for GMT """
    iso_date = \
        datetime.datetime(*time.strptime(iso_time, ISO_DATE_FMT)[0:6]) - \
        datetime.timedelta(hours=GMT_OFFSET)
    return iso_date.strftime(ORG_DATE_FMT)


def ISO_to_timestamp(iso_time):
    return time.mktime(time.strptime(iso_time, ISO_DATE_FMT)) - \
        3600 * GMT_OFFSET


def delta_datetime_string(offset_days):
    return datetime.datetime.now() + datetime.timedelta(offset_days)


def handle_emacs_loose_ends(db):
    """ this function should probably be rewritten """
    gcases_template = load_template('gcases_template', escape=False)
    now = time.strptime(time.asctime())
    now_year = now[0]
    now_month = now[1]
    now_day = now[2]

    for db_key in db.keys():
        all_day_event = False
        record_keys = db[db_key].keys()
        db[db_key]['TZID'] = TZID
        db[db_key]['TZID2'] = TZID
        db[db_key]['newline'] = '\r\n'
        db[db_key]['STMONTH'] = db[db_key].setdefault('STMONTH',
                                                      str(now_month)).zfill(2)
        db[db_key]['STDAY'] = db[db_key].setdefault('STDAY',
                                                    str(now_day)).zfill(2)
        if 'BYDAY' in record_keys:
            db[db_key]['BYDAYG'] = e_to_g_byday(db[db_key]['BYDAY'])
        if 'WHICHWEEK' in record_keys and 'NUMDAYOFWEEK' in record_keys:
            which_week = int(db[db_key]['WHICHWEEK'])
            if which_week == -1:
                which_week_g = '-1'
            else:
                which_week_g = db[db_key]['WHICHWEEK'].strip()
            db[db_key]['WHICHWEEKG'] = which_week_g + \
                DAY_ABBRS[int(db[db_key]['NUMDAYOFWEEK'].strip())]
        if 'DAYOFWEEK' in record_keys:
            db[db_key]['BYDAYG'] = db[db_key]['DAYOFWEEK'].upper()[:2]
        if 'DAYOFWEEKABBR' in record_keys:
            db[db_key]['BYDAYG'] = db[db_key]['DAYOFWEEKABBR'].upper()[:2]
        if 'MONTHABBR' in record_keys:
            db[db_key]['STMONTH'] = e_to_g_month_abbr(db[db_key]['MONTHABBR'])
        if 'STDAYNOTFOLLOWEDBYCOMMA' in record_keys:
            db[db_key]['STDAY'] = db[db_key]['STDAYNOTFOLLOWEDBYCOMMA']
            record_keys.append('STDAY')
        if 'STYEAR' not in record_keys:
            db[db_key]['STYEAR'] = str(now_year)
        elif len(db[db_key]['STYEAR']) < 4:
            start_year = '2' + db[db_key]['STYEAR'].zfill(3)
            db[db_key]['STYEAR'] = start_year
        if 'STHOUR' not in record_keys:
            all_day_event = True
            db[db_key]['STHOUR'] = '01'
            db[db_key]['STMINUTE'] = '00'
        else:
            db[db_key]['STHOUR'] = db[db_key]['STHOUR'].zfill(2)
            if 'STMINUTE' not in record_keys:
                db[db_key]['STMINUTE'] = '00'

        start_day = int(db[db_key]['STDAY'])
        start_month = int(db[db_key]['STMONTH'])
        start_year = int(db[db_key]['STYEAR'])
        start_hour = int(db[db_key]['STHOUR'])
        start_minute = int(db[db_key]['STMINUTE'])

        if 'STAMPM' not in record_keys:
            db[db_key]['STAMPM'] = 'AM'
        if db[db_key]['STAMPM'].upper()[0] == 'P' and start_hour < 12:
            start_hour += 12
        if db[db_key]['STAMPM'].upper()[0] == 'A' and start_hour == 12:
            start_hour = 0
            db[db_key]['STHOUR'] = '12'

        start_datetime = datetime.datetime(start_year,
                                           start_month,
                                           start_day,
                                           start_hour,
                                           start_minute)
        start_datetime_string = start_datetime.isoformat()
        start_datetime_string = start_datetime_string.replace(':', '')
        start_datetime_string = start_datetime_string.replace('-', '')

        default_end_datetime = start_datetime + \
            datetime.timedelta(minutes=DEFAULT_EVENT_DURATION)
        default_end_datetime_tuple = default_end_datetime.timetuple()

        if 'ENDYEAR' not in record_keys:
            db[db_key]['ENDYEAR'] = \
                str(default_end_datetime_tuple[0]).zfill(4)
        elif len(db[db_key]['ENDYEAR']) < 4:
            end_year = '2' + db[db_key]['ENDYEAR'].zfill(3)
            db[db_key]['ENDYEAR'] = end_year

        end_month = str(default_end_datetime_tuple[1])
        db[db_key]['ENDMONTH'] = \
            db[db_key].setdefault('ENDMONTH', end_month).zfill(2)

        end_day = str(default_end_datetime_tuple[2])
        db[db_key]['ENDDAY'] = \
            db[db_key].setdefault('ENDDAY', end_day).zfill(2)

        # This is a case like 3:30pm - 5pm, where no minutes are given
        # in the endtime, because they are abbreviated from 00
        if 'ENDHOUR' in record_keys and 'ENDMINUTE' not in record_keys:
            db[db_key]['ENDMINUTE'] = '00'
        else:
            db[db_key]['ENDMINUTE'] = \
                db[db_key].setdefault('ENDMINUTE',
                                     str(default_end_datetime_tuple[4])).zfill(2)
        db[db_key]['ENDHOUR'] = \
            db[db_key].setdefault('ENDHOUR',
                                 str(default_end_datetime_tuple[3])).zfill(2)

        end_hour = int(db[db_key]['ENDHOUR'])
        end_day = int(db[db_key]['STDAY'])
        end_month = int(db[db_key]['STMONTH'])
        end_year = int(db[db_key]['STYEAR'])
        end_hour = int(db[db_key]['ENDHOUR'])
        end_minute = int(db[db_key]['ENDMINUTE'])

        # Assume default event duration, as provided from
        # DEFAULT_EVENT_DURATION, if one is not given
        if 'ENDAMPM' not in record_keys:
            db[db_key]['ENDAMPM'] = \
                time.strftime('%p',
                              default_end_datetime_tuple)
            db[db_key]['ENDHOUR'] = \
                time.strftime('%I',
                              default_end_datetime_tuple)

        if db[db_key]['ENDAMPM'].upper()[0] == 'P' and end_hour < 12:
            end_hour += 12

        # Check to see if end date spans into its tomorrows date
        if end_hour < start_hour:
            tomorrow = start_datetime
            tomorrow += datetime.timedelta(hours=23)
            db[db_key]['ENDDAY'] = tomorrow.strftime('%d').zfill(2)
            db[db_key]['ENDMONTH'] = tomorrow.strftime('%m').zfill(2)
            db[db_key]['ENDYEAR'] = tomorrow.strftime('%Y')
            end_day = int(db[db_key]['ENDDAY'])
            end_month = int(db[db_key]['ENDMONTH'])
            end_year = int(db[db_key]['ENDYEAR'])


        end_datetime = datetime.datetime(end_year,
                                         end_month,
                                         end_day,
                                         end_hour,
                                         end_minute)
        end_datetime_string = end_datetime.isoformat()
        end_datetime_string = end_datetime_string.replace(':', '')
        end_datetime_string = end_datetime_string.replace('-', '')

        if all_day_event:
            db[db_key]['all_day_event'] = True
            end_datetime_string = end_datetime_string[:8]
            start_datetime_string = start_datetime_string[:8]
            start_datetime_tuple = datetime.datetime(start_year,
                                                     start_month,
                                                     start_day).timetuple()
            end_datetime_tuple = datetime.datetime(end_year,
                                                   end_month,
                                                   end_day).timetuple()
            db[db_key]['timetuple_dtstart'] = start_datetime_tuple
            db[db_key]['timetuple_dtend'] = end_datetime_tuple
        else:
            db[db_key]['all_day_event'] = False
            db[db_key]['timetuple_dtstart'] = start_datetime.timetuple()
            db[db_key]['timetuple_dtend'] = end_datetime.timetuple()

        db[db_key]['STDATETIME'] = start_datetime_string
        db[db_key]['ENDDATETIME'] = end_datetime_string

        if 'UNTILYEAR' in record_keys:
            until_day = int(db[db_key]['UNTILDAY'])
            until_month = int(db[db_key]['UNTILMONTH'])
            until_year = int(db[db_key]['UNTILYEAR'])
            until_datetime = datetime.datetime(until_year,
                                               until_month,
                                               until_day)
            until_datetime_string = until_datetime.isoformat()
            until_datetime_string = until_datetime_string.replace(':', '')
            until_datetime_string = until_datetime_string.replace('-', '')
            db[db_key]['UNTILDATETIME'] = until_datetime_string[:8]

        gcase = db[db_key]['gcase']

        # write recurrence string from gcases_template
        if len(gcase) > 7 and gcase[:7] == 'caseRec':
            recurrence_string = gcases_template[gcase] % db[db_key]
            db[db_key]['recurrence_string'] = recurrence_string
            db[db_key]['caseRec'] = gcase


def print_contents(db):
    """ used for debugging purposes """
    for keys in db.keys():
        print db[keys].get('TITLE')
        print db[keys].get('CONTENT')
        print " "


def get_previous_line(file_strings, pos):
    current = pos - 2
    while current > 0:
        current -= 1
        if file_strings[current] == NEWLINE:
            return file_strings[current + 1:pos - 1] + \
                NEWLINE + \
                NEWLINE_AND_END, current + 1
    return file_strings[:pos], 0


def has_keys(d, key_or_keys):
    """Does the dictionary d have all of the keys?"""
    return all(k in d for k in keys)


def ordinal_interval_to_number(ordinal_interval):
    if ordinal_interval is None or len(ordinal_interval) == 0:
        return ""
    ordinal_interval = ordinal_interval.lower()
    interval = \
        "".join([char for _, char in \
                     zip(xrange(len(ordinal_interval)),
                         ordinal_interval) if char.isdigit()])
    if interval != "":
        return interval
    weeks_of_month = {"last": "-1",
                      "second to last": "-2",
                      "third to last": "-3",
                      "fourth to last": "-4",
                      "first": "1",
                      "second": "2",
                      "third": "3",
                      "fourth": "4",
                      "fifth": "5",
                      "other": "2", }
    return weeks_of_month.setdefault('ordinal_interval', '')


def copy_description_to_db_record(db_record, description, case_template):
    """ copy description changes to record, and flag modified if any
    changes were made """
    modified = False

    if has_keys(description, ['STDAY', 'STMONTH', 'STYEAR']):
        start_day = description.get('STDAY')
        start_month = description.get('STMONTH')
        start_year = description.get('STYEAR')
        if any(start_day != db_record['STDAY'],
               start_month != db_record['STMONTH'],
               start_year != db_record['STYEAR']):
            db_record['STDAY'] = start_day.zfill(2)
            db_record['STMONTH'] = start_month.zfill(2)
            db_record['STYEAR'] = start_year.zfill(2)
            modified = True
    if has_keys(description, ['UNTILDAY', 'UNTILMONTH', 'UNTILYEAR']):
        until_day = description.get('UNTILDAY')
        until_month = description.get('UNTILMONTH')
        until_year = description.get('UNTILYEAR')
        if any(until_day != db_record['UNTILDAY'],
               until_month != db_record['UNTILMONTH'],
               until_year != db_record['UNTILYEAR']):
            db_record['UNTILDAY'] = start_day.zfill(2)
            db_record['UNTILMONTH'] = start_month.zfill(2)
            db_record['UNTILYEAR'] = start_year.zfill(2)
            modified = True
    if 'INTERVALORDINAL' in description:
        interval = \
            ordinal_interval_to_number(description.get('INTERVALORDINAL'))
        if interval != "" and interval != db_record['INTERVAL']:
            db_record['INTERVAL'] = interval
            modified = True
    if 'ONWHATDAYS' in description:
        on_what_days = description.get('ONWHATDAYS').lower().strip()
        if on_what_days != "":
            bydayd = []
            on_what_days = on_what_days.replace(', ', '')
            on_what_days = on_what_days.replace(' and', '')
            for day in on_what_days:
                bydayd.append(DAY_NUMBERS.setdefault(day[:2], ""))
                bydayd = " ".join(bydayd)
                bydayd = remove_all_extra_spaces(bydayd)
                byday = db_record.get('BYDAY')
                if byday is not None and byday != bydayd:
                    db_record['BYDAY'] = bydayd
                    modified = True

    return db_record, modified


def strip_comments(full_entry):
    pos = full_entry.find(' * EGCSync')
    if pos == -1:
        return full_entry, None
    else:
        return full_entry[:pos-1], full_entry[pos:]


def parse_comment_owner(login, comments_text):
    """ the comments line looks like this (the content, published and
    updated fields are optional and only appear if there is a comment
    entry in the comment feed):

   * EGCSync Comments for: picnic at Some Joe's House
   ** Some Joe's comments
   *** status: INVITED
   *** email: someJoesemail@gmail.com
   *** name: Some Joe
   *** content: Yada yada yada why you coming to my house yada
   *** published: [2009-09-09]
   *** updated:   [2009-09-09]
    """
    margin_length = 5
    pos_email = comments_text.find('email: ' + login)
    if pos_email == -1:
        return '', ''
    status_line, pos_status = get_previous_line(comments_text,
                                                pos_email - margin_length)
    pos_status = status_line.find('status:')
    if pos_status != -1:
        status = \
            status_line[pos_status + 7:pos_status + 9].strip().upper()[0:1]
    else:
        status = ''
    pos = comments_text.find('content:', pos_email)
    if pos != -1:
        pos += 8
        pos2 = comments_text.find('published:', pos)
        comment = comments_text[pos:pos2 - margin_length + 1]
        comment = remove_newlines_space_padding(comment)
    else:
        comment = ''

    return comment, status


def parse_dates(f):
    dates = []
    entries = []
    entry_start = []
    date_end = []
    entry_end = []
    f = NEWLINE + f
    found_date = False
    found_date_end = False
    for i in xrange(1, len(f)):
        char = f[i]
        last_char = f[i-1]
        if last_char == NEWLINE:
            if found_date and not found_date_end:
                date_end.append(i)
                # re.search will fail unless there is text after the
                # newline, i.e. NEWLINE_AND_END
                dates.append(f[entry_start[len(entry_start)-1]:i] +
                             NEWLINE_AND_END)
                found_date_end = True

            if char != ' ' and found_date:
                entry_end.append(i-1)
                entries.append(f[entry_start[len(entry_start)-1]:i] +
                               NEWLINE_AND_END)
                found_date = False
            if char != NEWLINE and char != ' ' and not found_date:
                entry_start.append(i-1)
                found_date = True
                found_date_end = False

    if len(entry_start) > len(entry_end):
        entry_start.pop()
    return dates, entries, entry_start, date_end, entry_end


def get_emacs_diary(login,
                    emacs_diary_location,
                    initialise_shelve,
                    times_a_range_template,
                    printing_case,
                    shelve_db):
    db = {}
    cases_template = load_template('cases_template')

    pattern, _ = evaluate_templates(cases_template,
                                    'cases_template_mtch')

    description_template = \
        load_template('recurrence_event_descriptions_template')
    description_pattern, _ = \
        evaluate_templates(description_template,
                          'recurrence_event_descriptions_template_mtch')

    f = open(emacs_diary_location, "r")
    a_file = f.read()
    f.close()
    keys = []
    details = []
    delete_from_file = []
    diary_header = ""
    look_for_headers = True

    # need this or last entry wont be read
    a_file = a_file + NEWLINE_AND_END
    # preserve any header information in the diary
    while len(a_file) > 13 and look_for_headers:
        if a_file[:13] == "&%%(org-diary" or a_file[:12] == "%%(org-diary":
            newline_pos = a_file.find(NEWLINE)
            diary_header = diary_header + a_file[:newline_pos] + NEWLINE
            a_file = a_file[newline_pos + 1:]
        else:
            look_for_headers = False
    if initialise_shelve:
        # unrecognized_entries is "", the last return value
        return db, diary_header, ""

    e_to_g_case_table = load_ref_table('e_to_g_case_table')
    date_fields, \
        entries, \
        entry_start, \
        date_end, \
        entry_end = parse_dates(a_file)
    for index_diary in xrange(len(date_fields)):
        for index_case in pattern.keys():
            match = pattern[index_case].search(date_fields[index_diary])
            if match is not None:
                entry_start_pos = entry_start[index_diary]
                entry_end_pos = entry_end[index_diary]
                match = pattern[index_case].search(entries[index_diary])
                entry = {}
                entry = match.groupdict()
                full_entry = \
                    strip_extra_newlines(a_file[entry_start_pos:
                                                    entry_end_pos])
                full_entry, comments_text = strip_comments(full_entry)
                entry['DETAIL'], comments_text = \
                    strip_comments(entry.get('DETAIL'))
                entry['full_entry'] = full_entry
                # in this case a attendeeStatus without a comment is
                # still considered a comment
                if comments_text is not None:
                    entry['comments_text'] = comments_text
                    comment_owner_content, comment_owner_status = \
                        parse_comment_owner(login, comments_text)
                    entry['comment_owner_hash'] = \
                        hash(entry.get('comment_owner_content'))
                    entry['comment_owner_status'] = comment_owner_status

                # this is for a future feature
                if full_entry.find(DISCARD_ENTRY_FLAG) == -1:
                    details.append(entry['DETAIL'])
                    entry['entrycase'] = index_case
                    entry['gcase'] = e_to_g_case_table[index_case]

                    previous_line, previous_line_start_pos = \
                        get_previous_line(a_file, entry_start_pos)
                    previous_line_match = \
                        description_pattern[index_case].search(previous_line)

                    # if description doesnt match then run it against
                    # all possible desc templates, if no match either
                    # then mark for  delete, else run both against
                    # shelve to see which one was edited and then
                    # update record.   IF description does match but
                    # items are changed, then find out which is
                    # correct by running it against the shelve, then
                    # update the record
                    if previous_line_match is not None:
                        entry_start_pos = previous_line_start_pos

                    entrypid = str(hash(full_entry))
                    keys.append(entrypid)
                    entry['entrypid'] = entrypid
                    db[entrypid] = entry
                delete_from_file.append([entry_start_pos, entry_end_pos])
                # find only first match for pattern[index_case] in
                # date_fields[index_diary]
                break

    new_file = []
    # preserve unrecognized entries and put them in new_file
    if len(delete_from_file) > 0:
        delete_from_file.sort()
        last_entry_pos = 0
        for entry_pos in delete_from_file:
            new_file.append(a_file[last_entry_pos:entry_pos[0]])
            last_entry_pos = entry_pos[1]
        new_file.append(a_file[last_entry_pos:])
        new_file = ''.join(new_file)
    else:
        new_file = ''
    new_file = strip_extra_newlines(new_file)
    update_details(db, details, keys)
    handle_emacs_loose_ends(db)

    # discard unrecognized entries if there are less than 6 chars worth of them
    if (len(new_file) > 5):
        print "-- UNRECOGNIZED ENTRIES:"
        unrecognized_diary_entries = new_file[:-3]
        print unrecognized_diary_entries
    else:
        unrecognized_diary_entries = ""

    return db, diary_header, unrecognized_diary_entries


def recurrence_get_file_TZID(recurrence):
    pos = recurrence.find('TZID')
    pos = recurrence.find('=', pos)
    end_pos = recurrence.find(':', pos)
    return recurrence[pos + 1:end_pos]


def recurrence_get_field(field_name, recurrence):
    pos = recurrence.find(field_name)
    pos = recurrence.find(':', pos)
    posend = recurrence.find(NEWLINE, pos)
    return recurrence[pos + 1:posend]


def recurrence_get_recurrence_rule_field(fieldname, rule):
    pos1 = rule.find(fieldname)
    pos2 = rule.find('=', pos1)
    posend = rule.find(';', pos2)
    if posend == -1:
        posend = len(rule)
    if pos1 > -1:
        return rule[pos2 + 1:posend]
    else:
        return ''


def blank_for_none_type(a_str):
    if a_str is None:
        return ''
    else:
        return a_str


def convert_datestring_to_time_tuple(datestring):
    """ used in getGoggleCalendar() to convert dtstart from gcal to a
    time tuple """

    year = int(datestring[0:4])
    month = int(datestring[4:6])
    day = int(datestring[6:8])
    if len(datestring) > 8:
        hour = int(datestring[9:11])
        minute = int(datestring[11:13])
    else:
        hour = 0
        minute = 0
    x = datetime.datetime(year, month, day, hour, minute)
    return x.timetuple()


def find_extended_property(properties, name):
    for prop in properties:
        if prop.name == name:
            return prop.value
    return ""


def map_time_before_titles():
    detail_re = re.compile(r'<(details.+?)>(.+?)</', re.M | re.S)
    detail_strings = detail_re.findall(detail_template)
    dic_map = {}
    for i in range(0, len(detail_strings)):
        key = detail_strings[i][0]
        case = 'casename-' + key
        detail = detail_strings[i][1]
        pos_time = detail.find('TIME')
        pos_title = detail.find('TITLE')
        if pos_time < pos_title:
            dic_map[case] = '1'
        else:
            dic_map[case] = '0'
    return dic_map


def get_event_status(event):
    """ returns 2 values, first value if canceled, second value if
    orphaned """

    EVENT_CANCELED_STR = \
        'eventStatus value="http://schemas.google.com/g/2005#event.canceled'

    event_string = str(event)
    event_id = event.id.text
    result = event_string.find(EVENT_CANCELED_STR)
    if result == -1:
        if event_id[-4:] == '000Z' and \
                event_id[-8] == 'T' and \
                event_id[-17] == '_':
            # orphaned
            return EVENT_ORPHANED
        else:
            # neither orphan nor canceled
            return EVENT_NOT_ORPHANED_OR_CANCELED
    else:
        # canceled
        return EVENT_CANCELED


def convert_exceptions_to_dict(exceptions):
    """ part of  address_exceptions() """
    dict_exceptions = {}
    exceptions.sort()
    for exception in exceptions:
        event_id = exception[:-17]
        date_string = exception[-16:]
        date_month_day_year = ' '.join(date_string[4:6],
                                       date_string[6:8],
                                       date_string[:4])
        element_list = dict_exceptions.get(event_id)
        if element_list is None:
            dict_exceptions[event_id] = [date_month_day_year]
        else:
            element_list.append(date_month_day_year)
            dict_exceptions[event_id] = element_list
    return dict_exceptions


def update_full_caseRec_entry(record,
                              time_a_range_string,
                              case_template_string):
    """ part of address_exceptions() """
    format_time_before_title = record.get('format_time_before_title')
    content = record.get('CONTENT')
    title = record.get('TITLE')
    if content != "":
        content = NEWLINE + content
    if record.get('alldayevent'):
        detail = title + content
    elif format_time_before_title:
        detail = time_a_range_string % record + ' ' + title + content
    else:
        detail = title + ' ' + time_a_range_string % record + content
    record['DETAIL'] = detail
    recurrence_string = case_template_string % record
    # this is a work around for printing the escaped % char
    if recurrence_string[0] == '%':
        recurrence_string = '%' + recurrence_string
    if recurrence_string[:2] == '&%':
        recurrence_string = '&%' + recurrence_string[1:]
    record['fullentry'] = strip_extra_newlines(recurrence_string)
    return record


def add_exceptions_to_record(google_dbrecord, exceptions):
    """ part of address_exceptions """
    exception_string = ""

    dic_consolidated_exceptions = {}

    # Consolidate Days
    for exception in exceptions:
        month = exception[:2]
        day = exception[3:5]
        year = exception[-4:]
        day_list = dic_consolidated_exceptions.setdefault(month + year,
                                                          [])
        day_list.append(day)
        dic_consolidated_exceptions[month + year] = day_list

    dic_months = {}

    # Consolidate Months
    for i, month_and_year in \
            zip(xrange(len(dic_consolidated_exceptions.keys())),
                dic_consolidated_exceptions.keys()):
        day_list = dic_consolidated_exceptions[month_and_year]
        day_and_year = " ".join(day_list) + month_and_year[-4:]
        month_list = dic_months.setdefault(day_and_year, [])
        month_list.append(month_and_year[:2])
        dic_months[day_and_year] = month_list

    for day_and_year in dic_months.keys():
        # put all the dates of the same month into
        exception_string = \
            exception_string + \
            "(diary-date '(" + \
            " ".join(dic_months[day_and_year]) + \
            ") '(" + day_and_year[:-4] + ") " + \
            day_and_year[-4:] + ")"

    # if there are more than 1 exceptions, then we need them separated
    # by '(diary-date'
    exception_string = exception_string[12:-1]
    google_dbrecord['EXCEPTIONSTRING'] = exception_string
    return google_dbrecord


def address_exceptions(google_db,
                       shelve_db,
                       g_to_e_key_map,
                       exceptions,
                       time_a_range_string,
                       cases_template):
    """ adds exception strings to recurring events with exceptions.
    Also, changes the recurrence case if need be"""

    #  This function depends on the preservation of event ids for
    #  recurrence exception instance records, and that their
    #  eventStatus is marked deleted in lieu of actually being
    #  deleted.
    flag_recurrence_updates = []
    if len(exceptions) == 0:
        return google_db, flag_recurrence_updates
    dic_exceptions = convert_exceptions_to_dict(exceptions)
    google_dbkeys = \
        [key for key in google_db.keys() \
             if type(google_db[key]) == DICTIONARY_DEFINED_TYPE]
    # these event ids are normal ones, not those that are found in
    #  exceptions
    for event_id in dic_exceptions.keys():
        if event_id in google_dbkeys:
            google_dbrecord = google_db[event_id]
            caseRec_name = google_dbrecord.get('caseRec')
            if caseRec_name[-9:] != "Exception":
                caseRec_name = caseRec_name + "Exception"
                google_dbrecord['caseRec'] = caseRec_name
            google_dbrecord = \
                add_exceptions_to_record(google_dbrecord,
                                         dic_exceptions[event_id])
            google_dbrecord = \
                update_full_caseRec_entry(google_dbrecord,
                                          time_a_range_string,
                                          cases_template[caseRec_name])

            shelve_record = shelve_db.get(g_to_e_key_map.get(event_id))
            if shelve_record is not None and \
                    google_dbrecord['EXCEPTIONSTRING'] != \
                    shelve_record.get('EXCEPTIONSTRING'):
                flag_recurrence_updates = \
                    append_key(flag_recurrence_updates,
                              event_id)
            google_db[event_id] = google_dbrecord.copy()
    return google_db, flag_recurrence_updates


def handle_exceptions(read_from_google_only,
                      ENTRY_CONTENTION,
                      google_db,
                      shelve_db,
                      emacs_db,
                      g_to_e_key_map,
                      orphaned,
                      del_from_g,
                      add_to_g,
                      identical_keys,
                      e_keys_changed_in_g,
                      g_keys_changed_in_g,
                      edit_links_map):
    """ this function interactively prompts the user to determine if
    an altered orphan was intented to be edited or
    deleted.  if the -n option was invoked, assume
    deleting in lieu of editing """

    UPDATE_GCAL_PROMPT = "Were you intending on Updating or Deleting \
this gcal entry?  U for Update, D for Delete, S for show related \
recurrence event:"
    ENTRY_UPDATE_PROMPT = "Which of the above diary entries represents \
the update? S for show related recurrence event:"

    if len(orphaned) == 0 or read_from_google_only:
        return del_from_g, add_to_g, {}, [], emacs_db

    delete_orphans = []
    dic_update_orphans = {}

    modified_orphans = \
        [key for key in orphaned if g_to_e_key_map.get(key) in del_from_g]

    # del_from_g cannot contain orphaned event ids
    del_from_g = \
        [key for key in del_from_g if \
             shelve_db[key]['eventid'] not in modified_orphans]

    # check modified_orphans against add_to_g
    modified_orphans = sort_keys_by_date(google_db, modified_orphans)
    add_to_g = sort_keys_by_date(emacs_db, add_to_g)
    if len(add_to_g) > 0 and ENTRY_CONTENTION != 2:
        print "!!!! INSTANCES OF RECURRENCES WERE MODIFIED !!!!"
    for instance_num, orphan in \
            zip(xrange(0, len(modified_orphans)), modified_orphans):
        if len(add_to_g) != 0:
            if ENTRY_CONTENTION != 2:
                print "Instance #", \
                    instance_num, ":", \
                    shelve_db[g_to_e_key_map[orphan]].get('fullentry')
                answer_validated = False

                while not answer_validated:
                    answer = raw_input(UPDATE_GCAL_PROMPT)
                    answer = answer.upper()
                    if answer == 'D':
                        answer_validated = True
                    elif answer == 'U':
                        answer_validated = True
                    elif answer == 'S':
                        print "Recurrence event:", \
                            google_db[orphan[:-17]].get('fullentry')
            else:
                answer = 'D'
        else:
            answer = 'D'

        if answer == 'U':
            if len(add_to_g) > 1:
                for i, key in zip(xrange(len(add_to_g)), add_to_g):
                    print "entry", i, ":", emacs_db[key].get('fullentry')
                answer_validated = False
                while not answer_validated:
                    answer = raw_input(ENTRY_UPDATE_PROMPT)
                    if answer == "S" or answer == "s":
                        print "Recurrence event:", \
                            google_db[orphan[:-17]].get('fullentry')
                    elif len(answer) > 0 and \
                            answer[0] >= '0' and \
                            answer[0] <= '9':
                        answer_validated = True
                        answer = int(answer)
                        dic_update_orphans[orphan] = add_to_g[answer]
                        # associate the orphan to the diary entry via eventid
                        emacs_db[add_to_g[answer]]['eventid'] = orphan
                        emacs_db[add_to_g[answer]]['editlink'] = \
                            edit_links_map[orphan]
                        del add_to_g[answer]
            elif len(add_to_g) == 1:
                dic_update_orphans[orphan] = add_to_g[0]
                emacs_db[add_to_g[0]]['eventid'] = orphan
                emacs_db[add_to_g[0]]['editlink'] = edit_links_map[orphan]
                del add_to_g[0]
        elif answer == 'D':
            delete_orphans.append(orphan)

    return del_from_g, add_to_g, dic_update_orphans, delete_orphans, emacs_db


def ordinal_suffix(day):
    day = int(day)
    if 4 <= day <= 20 or 24 <= day <= 30:
        suffix = "th"
    else:
        suffix = ["st", "nd", "rd"][day % 10 - 1]
    return str(day) + suffix


def add_recurrence_descriptions(google_db, emacs_db):
    """ called from main() to add descriptions to emacs_db """

    DAYS_OF_WEEK_PLURAL = ['Sundays', 'Mondays', 'Tuesdays', 'Wednesdays',
                           'Thursdays', 'Fridays', 'Saturdays']

    WEEKS_OF_MONTH = {"-1": "Last",
                      "-2": "Second to Last",
                      "-3": "Third to Last",
                      "-4": "Fourth to Last",
                      "0": "Second to Last",
                      "1": "First",
                      "2": "Second",
                      "3": "Third",
                      "4": "Fourth",
                      "5": "Fifth", }

    db_names = ['google_db', 'emacs_db']
    db = google_db
    case_template = \
        load_template('recurrence_event_descriptions_template',
                      escape=False)

    for db_name in db_names:
        db = locals()[db_name]

        # get only recurrence records
        dbkeys = [key for key in db.keys() \
                      if type(db[key]) == DICTIONARY_DEFINED_TYPE \
                      and 'caseRec' in db[key].keys()]
        for key in dbkeys:
            record = db.get(key)
            case_record = record.get('case_record')

            if case_record.find('bydayofweek') != -1:
                record['WHICHWEEKORDINAL'] = \
                    WEEKS_OF_MONTH[record.get('WHICHWEEK')]
                record['DAYORDINAL'] = \
                    ordinal_suffix(record['NUMDAYOFWEEK'])
                record['DAYOFWEEKD'] = \
                    DAYS_OF_WEEK_PLURAL[int(record.get('NUMDAYOFWEEK'))]
            if case_record.find('Weekly') != -1:
                bydayarray = record.get('BYDAY')
                if bydayarray is None:
                    bydayarray = ''
                if  bydayarray != "":
                    bydayarray = \
                        [DAYS_OF_WEEK_PLURAL[int(kz)] \
                             for kz in bydayarray.split(" ")]
                    if len(bydayarray) > 2:
                        byday = ", ".join(bydayarray[:-1])
                        byday = byday + " and " + bydayarray[-1] + " "
                    elif len(bydayarray) == 2:
                        byday = " and ".join(bydayarray) + " "
                    elif len(bydayarray) == 1:
                        byday = " ".join(bydayarray) + " "
                    else:
                        byday = ""
                    record['ONWHATDAYS'] = ' ' + byday
                else:
                    # this handles case for weekly without byday
                    record['ONWHATDAYS'] = ' '
            if case_record.find('Interval') != -1:
                interval = record['INTERVAL']
                intervalordinal = ordinal_suffix(interval)
                if intervalordinal == "2nd":
                    intervalordinal = "Other"
                record['INTERVALORDINAL'] = intervalordinal
            record['recurrencedesc'] = \
                NEWLINE + \
                case_template[case_record] % record + \
                NEWLINE
            db[key] = record.copy()

    return google_db, emacs_db


def get_comment_href(comment):
    if comment is None:
        return ""
    commenthref = str(comment)
    start = commenthref.find('href')
    start += 6
    end = commenthref.find('/>', start)
    end -= 2
    return commenthref[start:end]


def get_attendee_status_and_name(an_event_who):
    attendee_status = {}
    attendee_name = {}
    for index_who in xrange(len(an_event_who)):
        an_event_who_entry = an_event_who[index_who]
        attendee_name[an_event_who[index_who].email] = \
            an_event_who[index_who].name
        if str(an_event_who_entry).find('attendee_status') != -1:
            attendee_status[an_event_who[index_who].email] = \
                an_event_who[index_who].attendee_status.value[0:1]
        else:
            attendee_status[an_event_who[index_who].email] = ''
    return attendee_status, attendee_name


def get_google_calendar(user_name,
                        password,
                        time_min,
                        case_time_a_range_string,
                        ap):
    canceled = []
    orphaned = []
    recurrences = []
    recurrence_keys = []

    db = {}
    gcal = gdata.calendar.service.CalendarService()
    gcal.email = user_name
    gcal.password = password
    gcal.source = 'Google-Emacs-Calendar-Sync-1.0'
    try:
        gcal.ProgrammaticLogin()
    except Exception, err:
        if err[0] == 'Incorrect user_name or password':
            print err
            # no shelve object passed in and shelve.close() is not a
            # function shelve.close()
            sys.exit(1)
        print 'connection error'
        errorstatus = err[0].get('status')
        errorbody = err[0].get('body')
        # 302 = redirect
        if errorstatus == 302:
            print errorbody, 'redirect to:', error_redirect_URI(errorbody)

        # no shelve object passed in and shelve.close() is not a function
        # shelve.close()
        sys.exit(1)

    query = \
        gdata.calendar.service.CalendarEventQuery('default',
                                                  'private',
                                                  'full')
    query.start_max = time.strftime(ISO_DATE_FMT,
                                    time.gmtime(time.time() +
                                                28800000))
    query.ctz = TZID
    query.max_results = 400
    feed = gcal.CalendarQuery(query)
    feed_updated_text = feed.updated.text
    feed_updated_text = feed_updated_text[:-5]
    db['updated-g'] = time.strptime(feed_updated_text, TIMESTAMP_FMT)
    for i, an_event in zip(xrange(len(feed.entry)), feed.entry):
        entry_pid = an_event.id.text
        # It would be nice if event_status was actually a visible
        # property but its not:P (oops need to use event_status
        # property, but thats ok since we must also identify orphaned
        # entries)
        event_status = get_event_status(an_event)
        # if event is part of a recurring event but was deleted as an
        # instance the recurring event, then discard it
        if event_status == EVENT_CANCELED:
            canceled.append(entry_pid)
            continue
        # if event is part of a recurring event, but was edited as an
        # instance of that event, process it as normal
        elif event_status == EVENT_ORPHANED:
            orphaned.append(entry_pid)

        entry = {}

        comment_emails = []
        comments = []
        if DISPLAY_COMMENTS:
            attendee_status, attendee_name = \
                get_attendee_status_and_name(an_event.who)
            if an_event.comments is not None:
                comment = an_event.comments
                comment_href = get_comment_href(comment)
                entry['calendarEventComment_href'] = comment_href
                # cant get the CalendarEventCommentQuery to work so
                # using this instead
                comment_feed = gcal.Query(comment_href)
                comment_entry = comment_feed.entry
                comment_title = comment_feed.title.text
                for a_comment in comment_entry:
                    comment_entry = {}
                    comment_entry['comment_entry'] = a_comment
                    comment_entry['author'] = a_comment.author[0]
                    comment_entry['email'] = a_comment.author[0].email.text
                    comment_emails.append(comment_entry.get('email'))
                    comment_entry['status'] = \
                        attendee_status.get(comment_entry['email'])
                    comment_entry['name'] = a_comment.author[0].name.text
                    comment_entry['content'] = \
                        remove_newlines_space_padding(a_comment.content.text)
                    comment_entry['published'] = a_comment.published.text
                    comment_entry['updated'] = a_comment.updated.text
                    comment_entry['id'] = a_comment.id.text
                    if len(a_comment.link) > 1:
                        comment_entry['edit_link'] = a_comment.link[1]
                    comments.append(comment_entry)
                    email_id = comment_entry.get('email')
                    email_id = email_id.split('@')[0]
                    if email_id == user_name:
                        entry['comment_owner_editlink'] = \
                            comment_entry.get('edit_link')
                        entry['comment_owner_entry'] = comment
                        entry['comment_owner_status'] = \
                            comment_entry.get('status')
                        entry['comment_owner_content'] = \
                            comment_entry.get('content')
                        entry['comment_owner_hash'] = \
                            hash(comment_entry.get('content'))
                        entry['comment_owner_updated'] = \
                            ISO_to_timestamp(comment_entry.get('updated'))
                entry['comment_title'] = comment_title
                entry['comment_entries'] = comments

            # append attendee_status for those who did not leave
            # comments to the comments
            attendees_without_comments = \
                [key for key in attendee_status.keys() \
                     if key not in comment_emails]
            if len(attendees_without_comments) > 0:
                for email in attendees_without_comments:
                    comment_entry = {}
                    comment_entry['name'] = attendee_name.get(email)
                    comment_entry['email'] = email
                    comment_status = attendee_status.get(email)
                    if comment_status != '':
                        comment_entry['status'] = comment_status
                        email_id = email.split('@')[0]
                        if email_id == user_name:
                            entry['comment_owner_status'] = comment_status
                        comments.append(comment_entry)
                if len(comments) > 0:
                    default_title = \
                        'Attendees for: ' + \
                        blank_for_none_type(an_event.title.text)
                    entry.setdefault('comment_title', default_title)

                    entry['comment_entries'] = comments
        entry['idxfeed'] = i
        entry['HYPHEN'] = ' - '
        entry['eventid'] = entry_pid
        entry['where'] = blank_for_none_type(an_event.where[0].text)
        entry['TITLE'] = blank_for_none_type(an_event.title.text)
        content = blank_for_none_type(an_event.content.text)
        content = strip_extra_newlines(content)
        # debug
        content = remove_newlines_space_padding(content)
        content = pad_all_newlines_with_space(content)
        # content is an empty string if its blank, else each line is
        # padded on the left with a space
        entry['CONTENT'] = content

        entry['modified'] = time.strptime(an_event.updated.text[:19],
                                          TIMESTAMP_FMT)
        edit_link = an_event.GetEditLink()
        if edit_link != "":
            entry['edit_link'] = edit_link.href

        # get extended_properties here
        #  time before title?
        format_time_before_title = \
            find_extended_property(an_event.extended_property,
                                   'format_time_before_title')
        if format_time_before_title == "":
            format_time_before_title = FORMAT_TIME_BEFORE_TITLE_IN_DIARY
        elif format_time_before_title == "1":
            format_time_before_title = True
        else:
            format_time_before_title = False
        entry['format_time_before_title'] = format_time_before_title
        # entry visibility inhibiter?
        entry['VIS'] = find_extended_property(an_event.extended_property,
                                              'VIS')
        # non recurring format : mm/dd/yyyy or Jul 4, 2009 style?
        non_recurring_format = \
            find_extended_property(an_event.extended_property,
                                   'non_recurring_format')
        if non_recurring_format == "":
            non_recurring_format = DEFAULT_NON_RECURRING_FORMAT
        else:
            non_recurring_format = int(non_recurring_format)
        entry['non_recurring_format'] = non_recurring_format
        if an_event.recurrence is not None:
            # the gcal recurrence info is never used and takes up
            # alot of space
            recurrences.append(an_event.recurrence.text)
            recurrence_keys.append(entry_pid)
        else:
            # parse non-recurring entries
            start_datetime = strip_time_string(an_event.when[0].start_time)
            end_datetime = strip_time_string(an_event.when[0].end_time)
            entry['STYEAR'] = start_datetime[0:4]
            entry['STMONTH'] = start_datetime[4:6]
            entry['STDAY'] = start_datetime[6:8]
            entry['ENDYEAR'] = end_datetime[0:4]
            entry['ENDMONTH'] = end_datetime[4:6]
            entry['ENDDAY'] = end_datetime[6:8]
            entry['timetuple_dtstart'] = \
                convert_datestring_to_time_tuple(start_datetime)
            content = entry['CONTENT']

            if len(start_datetime) > 8:
                if int(start_datetime[9:11]) >= 12:
                    entry['STAMPM'] = 'pm'
                    entry['STHOUR'] = str(int(start_datetime[9:11]) - 12)
                    if entry['STHOUR'] == '0':
                        entry['STHOUR'] = '12'
                else:
                    entry['STAMPM'] = 'am'
                    entry['STHOUR'] = str(int(start_datetime[9:11]))
                    if entry['STHOUR'] == '0':
                        entry['STHOUR'] = '12'
                entry['STMINUTE'] = start_datetime[11:13]

                if int(end_datetime[9:11]) >= 12:
                    entry['ENDAMPM'] = 'pm'
                    entry['ENDHOUR'] = str(int(end_datetime[9:11]) - 12)
                    if entry['ENDHOUR'] == '0':
                        entry['ENDHOUR'] = '12'
                else:
                    entry['ENDAMPM'] = 'am'
                    entry['ENDHOUR'] = str(int(end_datetime[9:11]))
                    if entry['ENDHOUR'] == '0':
                        entry['ENDHOUR'] = '12'
                entry['ENDMINUTE'] = end_datetime[11:13]
                if content != "":
                    content = NEWLINE + content
                if format_time_before_title:
                    entry['DETAIL'] = \
                        case_time_a_range_string % entry + \
                        ' ' + \
                        entry['TITLE'] + \
                        content
                else:
                    entry['DETAIL'] = \
                        entry['TITLE'] + \
                        " " + \
                        case_time_a_range_string % entry + \
                        content
            else:
                # all day event
                entry['alldayevent'] = True
                if content != "":
                    content = NEWLINE + content
                entry['DETAIL'] = entry['TITLE'] + content


            if non_recurring_format == 0:
                start_day = str(int(entry['STDAY']))
                if len(start_day) == 1:
                    spaces = "  "
                else:
                    spaces = " "

                month_abbr = MONTH_ABBRS[int(entry['STMONTH']) - 1]

                entry['fullentry'] = \
                    strip_extra_newlines(entry['VIS'] +
                                         month_abbr +
                                         spaces +
                                         start_day +
                                         ', ' +
                                         entry['STYEAR'] +
                                         ' ' +
                                         entry['DETAIL'])
            else:
                entry['fullentry'] = \
                    strip_extra_newlines(entry['VIS'] +
                                         entry['STMONTH'] +
                                         '/' +
                                         entry['STDAY'] +
                                         '/' +
                                         entry['STYEAR'] +
                                         ' ' +
                                         entry['DETAIL'])
        db[entry_pid] = entry

    # now parse recurrences
    for i, recurrence in enumerate(recurrences):
        case_frequency = ''
        case_general = ''
        case_interval = ''
        case_block = ''
        by_day_g = ''
        datetime_start = recurrence_get_field('DTSTART', recurrence)
        db[recurrence_keys[i]]['SOMEZING'] = '"%V"'
        db[recurrence_keys[i]]['SOMEZING2'] = '"%V"'
        db[recurrence_keys[i]]['datetime_start'] = datetime_start
        db[recurrence_keys[i]]['STYEAR'] = datetime_start[0:4]
        db[recurrence_keys[i]]['STMONTH'] = datetime_start[4:6]
        db[recurrence_keys[i]]['STDAY'] = datetime_start[6:8]
        # note: every time you add a number postfixed variable name to
        # the cases_template you must make an entry for it here
        db[recurrence_keys[i]]['STYEAR2'] = datetime_start[0:4]
        db[recurrence_keys[i]]['STMONTH2'] = datetime_start[4:6]
        db[recurrence_keys[i]]['STDAY2'] = datetime_start[6:8]
        db[recurrence_keys[i]]['STYEAR3'] = datetime_start[0:4]
        db[recurrence_keys[i]]['STYEAR4'] = datetime_start[0:4]

        datetime_end = recurrence_get_field('DTEND', recurrence)
        db[recurrence_keys[i]]['datetime_end'] = datetime_end
        db[recurrence_keys[i]]['ENDYEAR'] = datetime_end[0:4]
        db[recurrence_keys[i]]['ENDMONTH'] = datetime_end[4:6]
        db[recurrence_keys[i]]['ENDDAY'] = datetime_end[6:8]

        tzid = recurrence_get_file_TZID(recurrence)
        db[recurrence_keys[i]]['TZID'] = tzid
        db[recurrence_keys[i]]['TZID2'] = tzid

        recurrence_rule = recurrence_get_field('RRULE', recurrence)
        db[recurrence_keys[i]]['recurrence_rule'] = recurrence_rule
        case_frequency = \
            recurrence_get_recurrence_rule_field('FREQ', recurrence_rule)
        case_frequency = case_frequency.lower().capitalize()

        db[recurrence_keys[i]]['freq'] = case_frequency

        until_date = recurrence_get_recurrence_rule_field('UNTIL',
                                                          recurrence_rule)
        db[recurrence_keys[i]]['until'] = until_date
        if until_date != '':
            case_block = 'Block'
            db[recurrence_keys[i]]['UNTILYEAR'] = until_date[0:4]
            db[recurrence_keys[i]]['UNTILMONTH'] = until_date[4:6]
            db[recurrence_keys[i]]['UNTILDAY'] = until_date[6:8]

        db[recurrence_keys[i]]['wkst'] = \
            recurrence_get_recurrence_rule_field('WKST',
                                                 recurrence_rule)

        interval = recurrence_get_recurrence_rule_field('INTERVAL',
                                                        recurrence_rule)
        if interval != '' and int(interval) > 0:
            db[recurrence_keys[i]]['INTERVAL'] = interval
            case_interval = 'Interval'

        by_day_g = recurrence_get_recurrence_rule_field('BYDAY',
                                                        recurrence_rule)
        db[recurrence_keys[i]]['BYDAYG'] = by_day_g
        if len(by_day_g) > 0 and (by_day_g[0].isdigit() or \
                                      by_day_g[0] == '-'):
            case_general = 'bydayofweek'
            which_week, which_day = g_to_e_which_week(by_day_g)
            db[recurrence_keys[i]]['WHICHWEEK'] = which_week
            db[recurrence_keys[i]]['NUMDAYOFWEEK'] = which_day
        else:
            db[recurrence_keys[i]]['BYDAY'] = g_to_e_byday(by_day_g)

        case_name = \
            'caseRec' + \
            case_frequency + \
            case_general + \
            case_interval + \
            case_block
        db[recurrence_keys[i]]['caseRec'] = case_name

        content = db[recurrence_keys[i]]['CONTENT']
        if len(datetime_start) > 8:
            if int(datetime_start[9:11]) >= 12:
                db[recurrence_keys[i]]['STAMPM'] = 'pm'
                db[recurrence_keys[i]]['STHOUR'] = \
                    str(int(datetime_start[9:11]) - 12)
                if db[recurrence_keys[i]]['STHOUR'] == '0':
                    db[recurrence_keys[i]]['STHOUR'] = '12'
            else:
                db[recurrence_keys[i]]['STAMPM'] = 'am'
                db[recurrence_keys[i]]['STHOUR'] = \
                    str(int(datetime_start[9:11]))
                if db[recurrence_keys[i]]['STHOUR'] == '0':
                    db[recurrence_keys[i]]['STHOUR'] = '12'

            db[recurrence_keys[i]]['STMINUTE'] = datetime_start[11:13]

            if int(datetime_end[9:11]) >= 12:
                db[recurrence_keys[i]]['ENDAMPM'] = 'pm'
                db[recurrence_keys[i]]['ENDHOUR'] = \
                    str(int(datetime_end[9:11]) - 12)
                if db[recurrence_keys[i]]['ENDHOUR'] == '0':
                    db[recurrence_keys[i]]['ENDHOUR'] = '12'
            else:
                db[recurrence_keys[i]]['ENDAMPM'] = 'am'
                db[recurrence_keys[i]]['ENDHOUR'] = str(int(datetime_end[9:11]))
                if db[recurrence_keys[i]]['ENDHOUR'] == '0':
                    db[recurrence_keys[i]]['ENDHOUR'] = '12'

            db[recurrence_keys[i]]['ENDMINUTE'] = datetime_end[11:13]

            if db[recurrence_keys[i]]['format_time_before_title']:
                if content != "":
                    content = NEWLINE + content
                db[recurrence_keys[i]]['DETAIL'] = \
                    case_time_a_range_string % db[recurrence_keys[i]] + \
                    ' ' + \
                    db[recurrence_keys[i]]['TITLE'] + \
                    content
            else:
                db[recurrence_keys[i]]['DETAIL'] = \
                    db[recurrence_keys[i]]['TITLE'] + \
                    ' ' + \
                    case_time_a_range_string % db[recurrence_keys[i]] + \
                    db[recurrence_keys[i]]['CONTENT']
        else:
            # all day event
            db[recurrence_keys[i]]['alldayevent'] = True
            if content != "":
                content = NEWLINE + content
            db[recurrence_keys[i]]['DETAIL'] = \
                db[recurrence_keys[i]]['TITLE'] + \
                content

        recurrencestring = ap[case_name] % db[recurrence_keys[i]]

        # this is a work around for printing the escaped % char
        if recurrencestring[0] == '%':
            recurrencestring = '%' + recurrencestring
        if recurrencestring[:2] == '&%':
            recurrencestring = '&%' + recurrencestring[1:]

        db[recurrence_keys[i]]['fullentry'] = \
            strip_extra_newlines(recurrencestring)
        db[recurrence_keys[i]]['timetuple_dtstart'] = \
            convert_datestring_to_time_tuple(datetime_start)

    return db, gcal, canceled, orphaned, feed


def get_keys_to_modify_from_e(db1, db2):
    """ Returns some arrays of keys that are to be inserted into or
    deleted from Gcal.  Any edited entries are deleted and reinserted
    """
    keys1 = [key for key in db1.keys() \
                 if type(db1[key]) == DICTIONARY_DEFINED_TYPE]
    keys2 = [key for key in db2.keys() \
                 if type(db2[key]) == DICTIONARY_DEFINED_TYPE]

    # indentical_keys are hashkeys that are the same in both the
    # shelve and emacs_db, meaning the entries are unchanged
    indentical_keys = [key for key in keys1 if key in keys2]

    del_from_g = [key for key in keys2 if key not in indentical_keys]
    add_to_g = [key for key in keys1 if key not in indentical_keys]
    return indentical_keys, del_from_g, add_to_g


def get_keys_to_modify_from_g(google_db,
                              keys_to_del_from_e,
                              shelve_db,
                              identical_keys,
                              g_last_sync_time):
    """ Returns some arrays of keys that are to be inserted into or
    deleted from the emacs Diary.  Any edited entries are deleted and
    reinserted """

    # identical_keys are hashkeys that are the same in both the shelve
    # and emacs_db
    g_keys = [key for key in google_db.keys() \
                  if type(google_db[key]) == DICTIONARY_DEFINED_TYPE]
    s_keys = [key for key in shelve_db.keys() \
                  if type(shelve_db[key]) == DICTIONARY_DEFINED_TYPE]
    s_key_event_ids = [shelve_db[key].get('eventid') for key in s_keys]
    del_from_e = [key for key in identical_keys \
                      if shelve_db[key].get('eventid') not in g_keys]

    last_modified = google_db[shelve_db[key]['eventid']].get('modified')
    modified_since_last_sync = last_modified > g_last_sync_time
    add_e = [key for key in identical_keys \
                 if key not in del_from_e and \
                 modified_since_last_sync]

    add_e_in_terms_of_g_keys = [shelve_db[key]['eventid'] \
                                    for key in identical_keys \
                                    if key not in del_from_e and \
                                    modified_since_last_sync]

    # these are newly entered from gcal
    newly_added_g_keys_to_add_to_e = [key for key in g_keys \
                                          if key not in s_key_event_ids]

    del_from_e = append_to_keys(del_from_e, keys_to_del_from_e)
    return del_from_e, \
        add_e, \
        add_e_in_terms_of_g_keys, \
        newly_added_g_keys_to_add_to_e


def get_shelve_and_last_sync_times(emacs_diary_location,
                                   gmail_user,
                                   initialise_shelve):
    last_modified_g = time.strptime('1995-1-1T12:00:00', TIMESTAMP_FMT)
    last_modified_e = time.gmtime(os.stat(emacs_diary_location).st_mtime)
    shelve_file_path = SHELVE_FILE
    postfix = str(abs(hash(gmail_user)))
    shelve_file_name_fully_qualified = \
        shelve_file_path + \
        'egcsyncshelve' + \
        postfix + \
        '.dat'

    f = shelve.open(shelve_file_name_fully_qualified)

    if f == {} or initialise_shelve:
        for key in f.keys():
            del f[key]
        f['updated-e'] = time.gmtime()
        f['updated-g'] = time.strptime(DEFAULT_UPDATED_G_TIME,
                                       ISO_DATE_FMT)
    else:
        last_modified_g = f['updated-g']
        last_modified_e = f['updated-e']
    return f, last_modified_g, last_modified_e


def convert_time_tupe_to_GMT(time_tuple):
    a = datetime.datetime(time_tuple[0],
                          time_tuple[1],
                          time_tuple[2],
                          time_tuple[3],
                          time_tuple[4])
    a += datetime.timedelta(hours=GMT_OFFSET)
    return a.timetuple()


def update_orphans_in_gcal(dic_update_orphans,
                           emacs_db,
                           shelve_db,
                           gcal,
                           edit_links_map,
                           g_to_e_key_map,
                           feed):
    # all non-recurring events must be entered in terms of GMT
    dic_find_time_before_title = map_time_before_titles()

    for orphan in dic_update_orphans.keys():
        for event in feed.entry:
            if event.id.text == orphan:
                break

        entry = emacs_db[dic_update_orphans[orphan]]
        event.title = atom.Title(text=entry.get('TITLE'))
        event.title.text = entry.get('TITLE')
        event.content = atom.Content(text=entry.get('CONTENT'))
        content = entry.get('CONTENT')
        if content is not None:
            event.content.text = remove_newlines_space_padding(content)

        if 'recurrencestring' in entry:
            event.recurrence = \
                gdata.calendar.Recurrence(text=entry['recurrencestring'])
        else:
            event.recurrence = None
            timetuple_datatime_start = entry['timetuple_datatime_start']
            timetuple_datatime_start = \
                convert_time_tupe_to_GMT(timetuple_datatime_start)
            timetuple_datetime_end = entry['timetuple_datetime_end']
            timetuple_datetime_end = \
                convert_time_tupe_to_GMT(timetuple_datetime_end)

            if entry['alldayevent']:
                start_time = time.strftime(YMD_DATE_FMT, timetuple_datatime_start)
                end_time = time.strftime(YMD_DATE_FMT, timetuple_datetime_end)
            else:
                start_time = time.strftime(ISO_DATE_FMT, timetuple_datatime_start)
                end_time = time.strftime(ISO_DATE_FMT, timetuple_datetime_end)

        event.when.append(gdata.calendar.When(start_time=start_time,
                                              end_time=end_time))


        # set extended_property additions here
        entrycase = entry.get('entrycase')
        # non recurring format: mm/dd/yyyy or Jul 4, 2009 format?
        if 'caseMonthdayyear' == entrycase:
            extendeddefaultformat = \
                gdata.calendar.ExtendedProperty(name="nonrecurringformat",
                                                value="1")
            event.extended_property.append(extendeddefaultformat)
        elif entrycase in ['caseMonthABBRdayyear',
                           'caseMonthABBRdayyearwspace']:
            # Jul  4, 2009 format?
            extendeddefaultformat = \
                gdata.calendar.ExtendedProperty(name="nonrecurringformat",
                                                value="0")
            event.extended_property.append(extendeddefaultformat)
            # Time before title ?
            casenames = [key for key in entry.keys() \
                             if key[:16] == 'casename-details']
        if len(casenames) > 0:
            casename = casenames[0]
        if dic_find_time_before_title[casename] == "1":
            extendedcase = \
                gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle",
                                                value="1")
            event.extended_property.append(extendedcase)
        else:
            extendedcase = \
                gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle",
                                                value="0")
            event.extended_property.append(extendedcase)

        diary_entry_visibility = entry.get('VIS')
        # entry visibility inhibiter?
        if diary_entry_visibility == '&':
            extended = gdata.calendar.ExtendedProperty(name="VIS",
                                                       value="&")
            event.extended_property.append(extended)

        print "-- Updated recurring event instance in Gcal to:", \
            emacs_db[dic_update_orphans[orphan]].get('fullentry')
        new_event = gcal.UpdateEvent(event.GetEditLink().href, event)
        emacs_db[dic_update_orphans[orphan]]['editlink'] = \
            new_event.GetEditLink().href
        del shelve_db[g_to_e_key_map[orphan]]

    return


def insert_entry_into_gcal(entry, gcal, dic_find_time_before_title):
    # all non-recurring events must be entered in terms of GMT
    event = gdata.calendar.CalendarEventEntry()
    event.title = atom.Title(text=entry.get('TITLE'))
    event.title.text = entry.get('TITLE')
    event.content = atom.Content(text=entry.get('CONTENT'))
    content = entry.get('CONTENT')
    if content is not None:
        event.content.text = remove_newlines_space_padding(content)

    if 'recurrencestring' in entry:
        event.recurrence = \
            gdata.calendar.Recurrence(text=entry['recurrencestring'])
    else:
        event.recurrence = None
        time_tuple_datetime_start = entry['time_tuple_datetime_start']
        time_tuple_datetime_start = \
            convert_time_tupe_to_GMT(time_tuple_datetime_start)
        time_tuple_datetime_end = entry['time_tuple_datetime_end']
        time_tuple_datetime_end = \
            convert_time_tupe_to_GMT(time_tuple_datetime_end)

        if entry['alldayevent']:
            start_time = time.strftime(YMD_DATE_FMT,
                                       time_tuple_datetime_start)
            end_time = time.strftime(YMD_DATE_FMT,
                                     time_tuple_datetime_end)
        else:
            start_time = time.strftime(ISO_DATE_FMT,
                                       time_tuple_datetime_start)
            end_time = time.strftime(ISO_DATE_FMT,
                                     time_tuple_datetime_end)

        event.when.append(gdata.calendar.When(start_time=start_time,
                                              end_time=end_time))

    # set extended_property additions here
    entry_case = entry.get('entry_case')

    # non recurring format: mm/dd/yyyy or Jul 4, 2009 format?
    if 'caseMonthdayyear' == entry_case:
        extended_default_format = \
            gdata.calendar.ExtendedProperty(name="nonrecurringformat",
                                            value="1")
        event.extended_property.append(extended_default_format)
    # Jul  4, 2009 format?
    elif entry_case in ['caseMonthABBRdayyear',
                        'caseMonthABBRdayyearwspace']:
        extended_default_format = \
            gdata.calendar.ExtendedProperty(name="nonrecurringformat",
                                            value="0")
        event.extended_property.append(extended_default_format)

    # Time before title ?
    case_names = [key for key in entry.keys() \
                     if key[:16] == 'case_name-details']

    if len(case_names) > 0:
        case_name = case_names[0]
        if dic_find_time_before_title[case_name] == "1":
            extended_case = \
                gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle",
                                                value="1")
            event.extended_property.append(extended_case)
        else:
            extended_case = \
                gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle",
                                                value="0")
            event.extended_property.append(extended_case)

    diary_entry_visibility = entry.get('VIS')
    # diary entry visibility inhibiter?
    if diary_entry_visibility == '&':
        extended = gdata.calendar.ExtendedProperty(name="VIS", value="&")
        event.extended_property.append(extended)

    new_event = gcal.InsertEvent(event,
                                 '/calendar/feeds/default/private/full')
    return new_event.id.text, new_event.GetEditLink().href


def insert_entries_into_gcal(add_to_g, emacs_db, gcal, shelve_db):
    dic_find_time_before_title = map_time_before_titles()
    for key in add_to_g:
        eventid, editlink = \
            insert_entry_into_gcal(emacs_db[key],
                                   gcal,
                                   dic_find_time_before_title)
        emacs_db[key]['eventid'] = eventid
        emacs_db[key]['editlink'] = editlink
        shelve_db[key] = emacs_db[key].copy()
        print "-- inserted from Diary to Gcal: " + \
            shelve_db[key]['fullentry']
        print


def delete_entries_from_e(shelve_db, del_from_e):
    for key in del_from_e:
        record = shelve_db.get(key)
        if record is not None:
            print "-- deleted from Diary: " + record.get('fullentry')
            del shelve_db[key]


def error_redirect_URI(body):
    """ 404='Not Found'
    302='Redirect received, but redirects_remaining <= 0'
  ERROR BODY looks like this:
  <HTML>
  <HEAD>
  <TITLE>Moved Temporarily</TITLE>
  </HEAD>
  <BODY BGCOLOR="#FFFFFF" TEXT="#000000">
  <H1>Moved Temporarily</H1>
  The document has moved <A HREF="http://www.google.com/calendar/feeds/default/private/full/u393ghl85ht66jgq4kol1nf1fo/63388346242?gsessionid=V_6S0byo7eTYPKIorY-GAg">here</A>.
  </BODY>
  </HTML>
    """
    pos = body.find('here')
    pos2 = body.find('HREF')
    if pos == -1 or pos2 == -1:
        return ''
    else:
        return body[pos2 + 6:pos - 2]


def delete_entries_from_gcal(del_from_g,
                             del_from_google_db,
                             google_db,
                             gcal,
                             shelve_db,
                             edit_links_map,
                             g_to_e_key_map,
                             delete_orphans):

    for key in del_from_g:
        record = shelve_db.get(key)
        if record is not None:
            event_id = record.get('event_id')
            if event_id is not None:
                edit_link = edit_links_map.get(event_id)
                if edit_link is not None:
                    try:
                        gcal.DeleteEvent(edit_link)
                        print "-- deleted from Gcal and Diary: " + \
                            shelve_db[key]['fullentry']
                    except Exception, err:
                        # 302 = Redirect received,
                        #    but redirects_remaining <= 0
                        error_status = err[0].get('status')
                        error_body = err[0].get('body')
                        print "-- unable to delete " + \
                            shelve_db[key]['fullentry']

                        # 404 being Not Found, so if its not Not
                        # Found, then assume its redirect
                        if error_status != 404:
                            print error_body, \
                                'redirect to:', \
                                error_redirect_URI(error_body)
                            shelve_db.close()
                            sys.exit(1)

                    del shelve_db[key]

    for key in delete_orphans:
        edit_link = edit_links_map.get(key)
        try:
            gcal.DeleteEvent(edit_link)
            print "-- deleted recurring event instance from Gcal and Diary:" + \
                google_db[key].get('fullentry')
        except Exception, err:
            error_status = err[0].get('status')
            error_body = err[0].get('body')
            print "-- unable to delete from gcal (probably already deleted): " + \
                google_db[key].get('fullentry')
            # 404 being Not Found, so if its not Not Found, then
            # assume its redirect
            if error_status != 404:
                print error_body, \
                    'redirect to:', \
                    error_redirect_URI(error_body)
                shelve_db.close()
                sys.exit(1)

        del shelve_db[g_to_e_key_map[key]]


def insert_entries_edited_by_diary_to_e(add_to_e, emacs_db, shelve_db):
    for key in add_to_e:
        shelve_db[key] = emacs_db[key].copy()
        print "-- insert edit into Diary: " + shelve_db[key]['fullentry']


def insert_entries_into_e(add_g_keys_to_e, shelve_db, google_db):
    for g_key in add_g_keys_to_e:
        entry_pid = str(hash(google_db[g_key]['fullentry']))
        google_db['entry_pid'] = entry_pid
        shelve_db[entry_pid] = google_db[g_key].copy()
        print "-- inserted to Diary: " + google_db[g_key]['fullentry']


def create_index_from_shelve(db):
    """ function called from write_emacs_diary() used to sort the
    diary entries for the emacs calendar it returns a 2xn matrix
    of timestamps associated with entry starting dates and hash
    keys, primary keys of the shelve"""
    db_keys = db.keys()
    index = []
    for key in db_keys:
        if type(db[key]) == DICTIONARY_DEFINED_TYPE:
            # start date converted to timestamp for indexing
            dttimestamp = time.mktime(db[key]['timetuple_dtstart'])
            row = []
            row.append(dttimestamp)
            row.append(key)
            index.append(row)
    index.sort()
    return index


def sort_keys_by_date(db, keys):
    """ function called from handlecontentions() used to sort the
    diary entries for the emacs calendar it returns a 2xn matrix
    of timestamps associated with entry starting dates and hash
    keys, primary keys of the shelve"""
    index = []
    for key in keys:
        if type(db[key]) == DICTIONARY_DEFINED_TYPE:
            # start date converted to timestamp for indexing
            datetime_timestamp = time.mktime(db[key]['timetuple_dtstart'])
            row = []
            row.append(datetime_timestamp)
            row.append(key)
            index.append(row)
    index.sort()

    target = []
    size = len(index)
    for i in range(0, size):
        target.append(index[i][1])
    return target


def write_emacs_diary(emacs_diary_location,
                      shelve_db,
                      diary_header,
                      unrecognized_diary_entries):
    index = create_index_from_shelve(shelve_db)
    f = open(emacs_diary_location, 'w')
    f.seek(0)
    if diary_header != "":
        f.write(diary_header + NEWLINE)

    # row[1] contains the ekeys and row[0] contains the order index
    for row in index:
        f.write(shelve_db[row[1]].setdefault('recurrencedesc', '') +
                shelve_db[row[1]].get('fullentry') +
                NEWLINE)

        status = shelve_db[row[1]]['comment_entries'][0].get('status')

        if all('comment_entries' in shelve_db[row[1]],
               len(shelve_db[row[1]].get('comment_entries')) > 0,
               status is not None,
               status != ''):

            f.write(' * EGCSync ' +
                    shelve_db[row[1]].get('comment_title') +
                    NEWLINE)

            for commententry in shelve_db[row[1]].get('comment_entries'):
                f.write(' ** ' +
                        commententry.get('name') +
                        "'s comments\n")
                comment_status = commententry.get('status')

                value = COMMENT_STATUS_ENUM.setdefault(comment_status, '')
                f.write(' *** status: ' + value + NEWLINE)
                f.write(' *** email: ' +
                        commententry.get('email') +
                        NEWLINE)
                f.write(' *** name: ' +
                        commententry.get('name') +
                        NEWLINE)
                if 'published' in commententry:
                    content = commententry.get('content')
                    padded_content = \
                        pad_newline_with_n_spaces(content + NEWLINE, 4)
                    f.write(' *** content: ' + padded_content)

                    published_org_time = \
                        ISO_to_org_time(commententry.get('published'))
                    f.write(' *** published: ' +
                            published_org_time +
                            NEWLINE)

                    updated_org_time = \
                        ISO_to_org_time(commententry.get('updated'))
                    f.write(' *** updated:   ' +
                            updated_org_time +
                            NEWLINE)

    f.close()


def close_shelve_and_mark_sync_times(emacs_diary_location,
                                     shelve_db,
                                     gcal):
    query = \
        gdata.calendar.service.CalendarEventQuery('default',
                                                  'private',
                                                  'full')
    query.start_min = time.strftime(ISO_DATE_FMT, time.gmtime())
    query.start_max = time.strftime(ISO_DATE_FMT, time.gmtime())

    feed = gcal.CalendarQuery(query)

    shelve_db['updated-e'] = \
        time.gmtime(os.stat(emacs_diary_location).st_mtime)
    shelve_db['updated-g'] = \
        time.strptime(feed.updated.text, ISO_DATE_FMT)

    del gcal
    shelve_db.close()


def update_attendee_status_to_gcal(user_name,
                                   identical_keys,
                                   g_to_e_key_map,
                                   emacs_db,
                                   google_db,
                                   shelve_db,
                                   gcal,
                                   feed,
                                   edit_links_map):
    comment_owner_changed = \
        shelve_db[key].get('comment_owner_status') != \
        emacs_db[key].get('comment_owner_status')
    attendee_status_modified_in_diary = [shelve_db[key].get('eventid') \
                                             for key in identical_keys \
                                             if comment_owner_changed]

    for key in attendee_status_modified_in_diary:
        shelve_comment_owner_status = \
            shelve_db[g_to_e_key_map.get(key)].get('comment_owner_status')
        google_comment_owner_status = \
            google_db[key].get('comment_owner_status')
        comment_owner_unchanged = \
            shelve_comment_owner_status == google_comment_owner_status

        if all(key in google_db,
               'comment_owner_status' in google_db[key],
               comment_owner_unchanged):
            shelve_key = g_to_e_key_map[key]
            index_feed = google_db[key].get('index_feed')
            entry_who = feed.entry[index_feed].who

            for index_entry_who in entry_who:
                if index_entry_who.email.split('@')[0] == user_name:
                    comment_owner_status = \
                        emacs_db[shelve_key].get('comment_owner_status')
                    printed_status = \
                        COMMENT_STATUS_ENUM.get(comment_owner_status)
                    index_entry_who.attendee_status.value = printed_status

            original_status = google_db[key].get('comment_owner_status')

            if original_status is not None:
                original_status = COMMENT_STATUS_ENUM.get(original_status)
            else:
                original_status = ''

            edit_link = edit_links_map.get(key)
            if printed_status is not None:
                print "-- updated attendee status for", \
                    google_db[key].get('fullentry'), \
                    ":", \
                    google_db[key].get('comment_owner_status'), \
                    " CHANGED TO: ", \
                    printed_status
                new_event = gcal.UpdateEvent(edit_link,
                                             feed.entry[index_feed])
                new_edit_link = new_event.GetEditLink()
                edit_links_map[key] = new_edit_link
                shelve_db[shelve_key]['edit_link'] = new_edit_link

    return shelve_db, gcal, feed, edit_links_map


def update_comments_to_gcal(identical_keys,
                            g_2_e_key_map,
                            emacs_db,
                            google_db,
                            shelve_db,
                            gcal):
    """ this function will not work until google fixes its api.  The
    google calendar supplies a writable view of the comment feed to
    the api, but not the actual feed itself; updating the copy does
    not have any effect on the comments displayed in the google
    calendar web gui """
    # debug
    return shelve_db, gcal


def update_edit_links(google_db, shelve_db):
    e_keys_changed_in_g = []
    g_keys_changed_in_g = []
    # edit_links_map maps gkey to event_id
    edit_links_map = {}
    g_to_e_key_map = {}

    for key in shelve_db.keys():
        if type(shelve_db[key]) == DICTIONARY_DEFINED_TYPE:
            google_db_record = google_db.get(shelve_db[key]['event_id'])
            if google_db_record is not None:
                event_id = google_db_record.get('event_id')
                event_link_g = google_db_record.get('editlink')
                # create a keymap for editlinks from g keys
                edit_links_map[event_id] = event_link_g
                # create a keymap from g keys to e keys also
                g_to_e_key_map[event_id] = key
                if event_link_g != shelve_db[key]['editlink']:
                    e_keys_changed_in_g.append(key)
                    g_keys_changed_in_g.append(shelve_db[key]['event_id'])

    for key in google_db.keys():
        if type(google_db[key]) == DICTIONARY_DEFINED_TYPE:
            edit_links_map[key] = google_db[key]['editlink']

    return e_keys_changed_in_g, \
        g_keys_changed_in_g, \
        g_to_e_key_map, \
        edit_links_map


def append_to_keys(key_list, keys_to_insert):
    for key in keys_to_insert:
        if key not in key_list:
            key_list.append(key)
    return key_list


def append_key(key_list, key_to_insert):
    if key_to_insert not in key_list:
        key_list.append(key_to_insert)
    return key_list


def remove_keys(key_list, keys_to_remove):
    key_list2 = []
    for key in key_list:
        if key not in keys_to_remove:
            key_list2.append(key)
    return key_list2


def remove_key(key_list, key_to_remove):
    key_list2 = []
    for key in key_list:
        if key != key_to_remove:
            key_list2.append(key)
    return key_list2


def handle_contentions(read_from_google_only,
                       ENTRY_CONTENTION,
                       identical_keys,
                       del_from_g,
                       add_to_g,
                       e_keys_changed_in_g,
                       g_keys_changed_in_g,
                       shelve_db,
                       google_db,
                       emacs_db,
                       g_to_e_key_map):
    """entry contention happens when both a diary entry and its
    respective google calendar entry are modified before a sync.
    There is no way to precisely tell which diary entry was modified
    so all we can do is display the modified gcal entry along with
    perhaps a list of possibilities.  If the
    ENTRY_CONTENTION variable is set to 2 we will do nothing
    and just add both entries"""
    if read_from_google_only:
        ENTRY_CONTENTION = 2

    contending_e = [shelve_db[key].get('eventid') \
                        for key in e_keys_changed_in_g \
                        if key in del_from_g]

    emacs_db_keys = [key for key in emacs_db.keys() \
                         if type(emacs_db[key]) == DICTIONARY_DEFINED_TYPE]
    # contending entries from emacs_db will not appear in the
    # identical_keys list
    contending_emacs_db = [key for key in emacs_db_keys \
                              if key not in identical_keys]

    contending_emacs_db = sort_keys_by_date(emacs_db, contending_emacs_db)

    contending_e = sort_keys_by_date(google_db, contending_e)
    del_from_emacs_db = []

    del_from_google_db = []
    add_edit_to_e = []
    i = -1
    answer = '0'

    #  nest 2 loops for contending_e (from gcal) and contending_emacs_db
    #  (from the diary)
    for key in contending_e:
        continue_to_next_contending_e = False
        i += 1
        print "!! CONTENTION #", \
            i,\
            "!!!!!!!!! The following entry has been modified in both\
 the emacs diary as well as the google calendar:"
        print ">> gcal:", google_db[key]['fullentry']
        # prompt from list of contenders
        if ENTRY_CONTENTION == 0:
            # if the list is empty then break to the next contending_e
            if len(contending_emacs_db) == 0:
                continue
            # sort contending_emacs_db list
            j = -1
            # nested loop for contending_emacs_db
            for emacs_dbkey in contending_emacs_db:
                j += 1
                print "<< diary possibility#", \
                    j, \
                    ":", \
                    emacs_db[emacs_dbkey]['fullentry']
            if len(contending_emacs_db) > 1:
                answer_validated = False
                while not answer_validated:
                    answer = raw_input("?? Which diary possibility# \
most likely matches in contention with the aforementioned modified \
gcal entry? (n for none):")
                    if len(answer) > 0:
                        if answer[0] == 'n' or answer[0] == 'N':
                            answer_validated = True
                            continue_to_next_contending_e = True
                        elif all(answer[0] >= '0',
                                 answer[0] <= '9',
                                 int(answer) < len(contending_emacs_db)):
                            answer_validated = True
            elif len(contending_emacs_db) == 1:
                answer = '0'
            else:
                continue_to_next_contending_e = True
        # automatic best guess
        elif ENTRY_CONTENTION == 1:
            answer = '0'
        # do nothing, allowing for contending entries to be added to
        # both the diary and gcal
        elif ENTRY_CONTENTION == 2:
            continue_to_next_contending_e = True
        if continue_to_next_contending_e:
            continue_to_next_contending_e = False
            continue
        match = int(answer)
        answer_validated = False
        while not answer_validated:
            answer = raw_input("?? keep gcal entry (g) or emacs diary \
entry (e)? (b for both)")
            answer = answer.lower()
            # delete emacs_db match entry, and add the gcal
            # contending_e entry to the diary
            if answer == 'g':
                del_from_g = remove_key(del_from_g,
                                        g_to_e_key_map.get(key))
                add_edit_to_e = append_key(add_edit_to_e, key)
                add_to_g = remove_key(add_to_g, contending_emacs_db[match])
                # delete from the diary
                del_from_emacs_db = append_key(del_from_emacs_db,
                                               contending_emacs_db[match])
                # delete from the shelve
                del_from_emacs_db = append_key(del_from_emacs_db,
                                               g_to_e_key_map.get(key))

                del emacs_db[contending_emacs_db[match]]
                del contending_emacs_db[match]

                answer_validated = True
            # delete the contending_e entry, and add the emacs_db match
            # entry
            elif answer == 'e':
                del_from_g = append_key(del_from_g, contending_e[i])
                add_to_g = append_key(add_to_g,
                                      contending_emacs_db[match])

                # delete from list of edited gcal entries
                e_keys_changed_in_g = remove_key(e_keys_changed_in_g,
                                                 g_to_e_key_map.get(key))
                g_keys_changed_in_g = remove_key(g_keys_changed_in_g, key)
                del_from_google_db = append_key(del_from_google_db, key)
                del contending_emacs_db[match]
                answer_validated = True
            elif answer == 'b' or answer == 'n':
                answer_validated = True
    return identical_keys, \
        del_from_g, \
        del_from_emacs_db, \
        add_to_g, \
        del_from_google_db, \
        add_edit_to_e, \
        e_keys_changed_in_g, \
        g_keys_changed_in_g


class _Getch:
    """Gets a single character from standard input."""

    def __init__(self):
        try:
            self.impl = _GetchWindows()
        except ImportError:
            self.impl = _GetchUnix()

    def __call__(self):
        return self.impl()


class _GetchUnix:

    def __init__(self):
        import tty
        import sys

    def __call__(self):
        import sys
        import tty
        import termios
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch


class _GetchWindows:

    def __init__(self):
        import msvcrt

    def __call__(self):
        import msvcrt
        return msvcrt.getch()


getch = _Getch()


def get_passwd():
    """Uses the _Getch class to input a string from the keyboard,
    masking the characters with '*', returning the string without the
    newline char"""
    a = 'q'
    password = ''
    while a != chr(13):
        a = getch()
        password = password + a
        print '*',
    return password[0:len(password) - 1]


def get_home_dir():
    try:
        from win32com.shell import shellcon, shell
        home_dir = shell.SHGetFolderPath(0, shellcon.CSIDL_APPDATA, 0, 0)
    except ImportError:
        #  non-windows/win32com case
        home_dir = os.path.expanduser("~")
    return home_dir


def locate_emacs_diary_linux(home_dir):
    """ find the location of the diary file by first looking for
~/diary, then try looking for the diary file location in the ~/.emacs
file.  If we cant find it return None.  # OS Dependent.  """
    default_location = DIARY_FILE
    if os.path.exists(default_location):
        return default_location
    elif os.path.exists(home_dir + '/diary'):
        return home_dir + '/diary'
    elif os.path.exists(home_dir + '/.emacs'):
        f = open(home_dir + '/.emacs', "r")
        emacs_file = f.read()
        f.close()
        emacs_pattern = re.compile(r'\(setq diary-file (.+?)\)')
        matches = emacs_pattern.findall(emacs_file)
        if len(matches) > 0:
            match = matches[0]
            if len(match) > 2:
                if match[0] == '"' or match[0] == "'":
                    match = match[1:-1]
                if match[0] == "~":
                    match = match[1:]
                    match = home_dir + match
                if os.path.exists(match):
                    return match
    return None


def usage():
    print >> sys.stderr, \
"""Using this script without any options or arguments will syncronizes
the emacs and google calendars. Optionally, the gmail user name and
password may be specified as arguments; if they are not, then they
will be prompted upon execution.  Use option -i to initialise the
emacs diary by deleting the shelve and diary and then populating them
from the Google Calendar.  Consider the -i option as a one way sync
from Google to Emacs.

     Entry contention can happen when the same diary and its
respective google calendar entries are both modified before a sync; by
default the script will interactively prompt you if this occurs (use
the -p option if it does not). However, you may use option -a to make
an automatic best guess if its unclear as to which entries are in
contention (with option -a you still have to input which entry to keep
and which to discard). Use option -n to do nothing about contending
events; this allows for both entries to exist in both gcal and diary,
but eliminating user interaction, and so is preferable when using a
crond scheduler."""


def main(argv=None):
    """ Using this script without any options or arguments will
   syncronizes the emacs and google calendars.  Optionally, the gmail
   user name and password may be specified as arguments; if they are
   not, then they will be prompted upon execution.  Use option -i to
   delete the shelve when you want to initialize the emacs calendar """

   # we are dealing with 3 databases: emacs_db, shelve, and google_db.
   # emacs_db is created from the diary file.  shelve was saved from
   # the last sync and was used to write the diary file at that point
   # in time.  google_db is created from google calendar.  using
   # pigeon hole set logic we'll determine where to move the entries
   # contained in these databases.

    if argv is None:
        argv = sys.argv

    home_dir = get_home_dir()

    # OS Dependent
    emacs_diary_location = locate_emacs_diary_linux(home_dir)

    if emacs_diary_location is None:
        print >> sys.stderr, "Unable to locate the emacs diary.  \
Please create a file in your home directory called diary"
        return 1

    # Defaults
    entry_contention = ENTRY_CONTENTION
    read_from_google_only = READ_FROM_GOOGLE_ONLY
    initialise_shelve = False

    LONG_OPTS = ["help",
                 "init",
                 "autocontention",
                 "nocontention",
                 "promptcontention",
                 "readfromgoogleonly"]
    SHORT_OPTS = "hianpr"
    try:
        opts, args = getopt.getopt(argv[1:], SHORT_OPTS, LONG_OPTS)
    except getopt.GetoptError, err:
        print >> sys.stderr, str(err)
        usage()
        return 2

    if len(opts) > 0:
        option = opts[0][0]
        if option == "--init" or option == "-i":
            initialise_shelve = True
        if option == "--readgoogleonly" or option == "-r":
            read_from_google_only = True
        elif option == "--autocontention" or option == "-a":
            entry_contention = 1
        elif option == "--nocontention" or option == "-n":
            entry_contention = 2
        elif option == "--promptcontention" or option == "-p":
            entry_contention = 0
        elif option == "--help" or option == "-h":
            usage()
            return 0

    if len(args) == 2:
        gmail_user = args[0]
        gmail_passwd = args[1]
    elif len(args) == 1:
        gmail_user = args[0]

        if PASSWORD is None or PASSWORD.strip() == '':
            print('enter gmail passwd:'),
            gmail_passwd = get_passwd()
        else:
            gmail_passwd = PASSWORD
    else:
        gmail_user = raw_input('enter gmail username:')
        print('enter gmail passwd:'),
        gmail_passwd = get_passwd()

    shelve_db, last_sync_google, last_sync_emacs = \
        get_shelve_and_last_sync_times(emacs_diary_location,
                                       gmail_user,
                                       initialise_shelve)
    # OS Dependent
    last_modified_emacs = time.gmtime(os.stat(emacs_diary_location).st_mtime)

    times_template = load_template('times_template', escape=False)
    cases_template = load_template('cases_template', escape=False)

    emacs_db, diary_header, unrecognized_diary_entries = \
        get_emacs_diary(gmail_user,
                        emacs_diary_location,
                        initialise_shelve,
                        times_template['caseTimeARange'],
                        cases_template,
                        shelve_db)

    google_db, gcal, canceled, orphaned, feed = \
        get_google_calendar(gmail_user,
                            gmail_passwd,
                            last_sync_google,
                            times_template['caseTimeARange'],
                            cases_template)

    # canceled must be taken out of shelve and E; if they exist there
    # then they were once orphaned orphaned must be treated as normal
    # entrys, except that they cannot be deleted if edits were made,
    # but instead must be edited and have their eventStatus changed to
    # canceled Any exceptional recurring event, meaning containing
    # canceled or orphaned instances, may not be deleted if edits are
    # made. When they're deleted all their orphans must be deleted too
    last_modified_google = google_db['updated-g']
    diary_was_modified = last_modified_emacs > last_sync_emacs

    gcal_was_modified = last_modified_google > last_sync_google

    # e_keys_changed_in_g are edited gcal entries, not newly added ones
    e_keys_changed_in_g, \
        g_keys_changed_in_g, \
        g_to_e_key_map, \
        edit_links_map = update_edit_links(google_db, shelve_db)

   # change the casename of recurrence events that contain exceptions
   # and add the exceptions to their EXCEPTIONSTRING.  note: the term
   # 'exception', throughout the scope of this script, refers to
   # recurrence exceptions, and not error exceptions
    google_db, flag_recurrence_updates = \
        address_exceptions(google_db,
                           shelve_db,
                           g_to_e_key_map,
                           orphaned + canceled,
                           times_template['caseTimeARange'],
                           cases_template)

    google_db, emacs_db = add_recurrence_descriptions(google_db, emacs_db)

    # identical_keys are hashkeys that are the same in both the shelve
    # and emacs_db, meaning the entries are unchanged by emacs diary
    identical_keys, del_from_g, add_to_g = \
        get_keys_to_modify_from_e(emacs_db, shelve_db)

    del_from_g, \
        add_to_g, \
        dic_update_orphans, \
        delete_orphans, \
        emacs_db = handle_exceptions(read_from_google_only,
                                     entry_contention,
                                     google_db,
                                     shelve_db,
                                     emacs_db,
                                     g_to_e_key_map,
                                     orphaned,
                                     del_from_g,
                                     add_to_g,
                                     identical_keys,
                                     e_keys_changed_in_g,
                                     g_keys_changed_in_g,
                                     edit_links_map)

    identical_keys, \
        del_from_g, \
        del_from_e, \
        add_to_g, \
        del_from_google_db, \
        add_edit_to_e, \
        e_keys_changed_in_g, \
        g_keys_changed_in_g = handle_contentions(read_from_google_only,
                                                 entry_contention,
                                                 identical_keys,
                                                 del_from_g,
                                                 add_to_g,
                                                 e_keys_changed_in_g,
                                                 g_keys_changed_in_g,
                                                 shelve_db,
                                                 google_db,
                                                 emacs_db,
                                                 g_to_e_key_map)

    del_from_e, \
        add_e, \
        add_e_in_terms_of_g, \
        newly_added_g_keys_to_add_to_e = \
        get_keys_to_modify_from_g(google_db,
                                  del_from_e,
                                  shelve_db,
                                  identical_keys,
                                  last_sync_google)

    newly_added_g_keys_to_add_to_e = \
        remove_keys(newly_added_g_keys_to_add_to_e,
                   del_from_google_db)
    newly_added_g_keys_to_add_to_e = \
        append_to_keys(newly_added_g_keys_to_add_to_e,
                       add_edit_to_e)

    # if orphans are modified, their new value must be added to the shelve_db
    add_e = append_to_keys(add_e, dic_update_orphans.values())

    del_from_e = append_to_keys(del_from_e, e_keys_changed_in_g)
    newly_added_g_keys_to_add_to_e = \
        append_to_keys(newly_added_g_keys_to_add_to_e,
                       g_keys_changed_in_g)

    # if more orphans appear, then we have to change the original
    # recurrence entry to reflect the new dates to not display in the
    # diary
    add_e_in_terms_of_g = append_to_keys(add_e_in_terms_of_g,
                                         flag_recurrence_updates)
    # if more orphans appear, then we have to change the original
    # recurrence entry to reflect the new dates to not display in the
    # diary
    del_from_e = append_to_keys(del_from_e,
                                [g_to_e_key_map.get(key) \
                                     for key in flag_recurrence_updates])

    add_e_in_terms_of_g = append_to_keys(add_e_in_terms_of_g,
                                         newly_added_g_keys_to_add_to_e)

    # google calendar doesnt change its 'modified' date when an entry
    # is edited, but it does change the editlink, so we check for that
    # here
    if len(newly_added_g_keys_to_add_to_e) > 0 or len(e_keys_changed_in_g) > 0:
        gcal_was_modified = True

    if not read_from_google_only:
        shelve_db, gcal = update_comments_to_gcal(identical_keys,
                                               g_to_e_key_map,
                                               emacs_db,
                                               google_db,
                                               shelve_db,
                                               gcal)
        shelve_db, gcal, feed, edit_links_map = \
            update_attendee_status_to_gcal(gmail_user,
                                           identical_keys,
                                           g_to_e_key_map,
                                           emacs_db,
                                           google_db,
                                           shelve_db,
                                           gcal,
                                           feed,
                                           edit_links_map)

    if len(del_from_e) > 0 or \
            len(add_to_g) > 0 or \
            len(add_e) > 0 or \
            len(newly_added_g_keys_to_add_to_e) > 0 or \
            len(del_from_g) > 0 or \
            len(e_keys_changed_in_g) > 0 or \
            initialise_shelve \
            or len(dic_update_orphans) > 0 or \
            len(delete_orphans) > 0:
        delete_entries_from_e(shelve_db, del_from_e)

        if not read_from_google_only:
            delete_entries_from_gcal(del_from_g,
                                     del_from_google_db,
                                     google_db,
                                     gcal,
                                     shelve_db,
                                     edit_links_map,
                                     g_to_e_key_map,
                                     delete_orphans)
            update_orphans_in_gcal(dic_update_orphans,
                                   emacs_db,
                                   shelve_db,
                                   gcal,
                                   edit_links_map,
                                   g_to_e_key_map,
                                   feed)

        if diary_was_modified:
            if not read_from_google_only:
                insert_entries_into_gcal(add_to_g, emacs_db, gcal, shelve_db)
            insert_entries_edited_by_diary_to_e(add_e, emacs_db, shelve_db)

        if gcal_was_modified or len(flag_recurrence_updates) > 0:
            insert_entries_into_e(add_e_in_terms_of_g, shelve_db, google_db)

        write_emacs_diary(emacs_diary_location,
                          shelve_db,
                          diary_header,
                          unrecognized_diary_entries)
    else:
        print "-- No Changes"

    close_shelve_and_mark_sync_times(emacs_diary_location, shelve_db, gcal)


if __name__ == '__main__':
    sys.exit(main())
