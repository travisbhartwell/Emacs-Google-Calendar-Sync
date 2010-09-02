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

"""Templates used in parsing and generating data for Emacs Diary and
Google Calendar.  Also includes related functions for evaluation.
"""

# Items left to do:
# TODO: Handle formatting so it fits within the standards, but still
#       generates the correct templates.
# TODO: Figure out best way to generate template strings, reusing
#       constants already defined.
# TODO: Remove all regex and template fragment repetitions.
# TODO: Standardize regexs (\d{4}) vs (\d\d\d\d), fixing the
#       originals so tests will pass.
# TODO: Should the constants be in the templates module namespace or
#       some sub-namespace?  Make sure only those items that are
#       needed outside of the module are exported.
# TODO: Verify which items must be exact strings and others which can
#       be renamed to something more understandable by me.
# TODO: Once all of the above are completed, evaluate whether this is
#       a maintainable solution for changing and adding templates.
import re

# Constants used in templates
# Common strings used in templates
_RECURS_DAILY_TEMPLATE_STRING = 'Recurs Daily'
_RECURS_EVERY_WEEK_TEMPLATE_STRING = 'Recurs Every Week'
_RECURS_YEARLY_TEMPLATE_STRING = 'Recurs Yearly'
_SINGLE_DAY_EVENT_TEMPLATE_STRING = 'Single Day Event'

# Constants first used in cases template
CASE_REC_DAILY_ASTERIX_VAR = 'caseRecDailyAsterix'
CASE_REC_DAILY_VAR = 'caseRecDaily'
CASE_REC_DAILY_EXCEPTION_VAR = 'caseRecDailyException'
CASE_REC_DAILY_ASTERISK_VAR = 'caseRecDailyAsterisk'
CASE_REC_DAILY_BLOCK_VAR = 'caseRecDailyBlock'
CASE_REC_DAILY_BLOCK_EXCEPTION_VAR = 'caseRecDailyBlockException'
CASE_REC_DAILY_INTERVAL_VAR = 'caseRecDailyInterval'
CASE_REC_DAILY_INTERVAL_EXCEPTION_VAR = 'caseRecDailyIntervalException'
CASE_REC_DAILY_INTERVAL_BLOCK_VAR = 'caseRecDailyIntervalBlock'
CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR = \
    'caseRecDailyIntervalBlockException'
CASE_REC_WEEKLY_WEEKNAME_VAR = 'caseRecWeeklyWeekname'
CASE_REC_WEEKLY_ABBR_VAR = 'caseRecWeeklyAbbr'
CASE_REC_WEEKLY_VAR = 'caseRecWeekly'
CASE_REC_WEEKLY_EXCEPTION_VAR = 'caseRecWeeklyException'
CASE_REC_WEEKLY_BLOCK_VAR = 'caseRecWeeklyBlock'
CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR = 'caseRecWeeklyBlockException'
CASE_REC_WEEKLY_INTERVAL_VAR = 'caseRecWeeklyInterval'
CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR = 'caseRecWeeklyIntervalException'
CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR = 'caseRecWeeklyIntervalBlock'
CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR = \
    'caseRecWeeklyIntervalBlockException'
CASE_REC_MONTHLY_ASTERISK_VAR = 'caseRecMonthlyAsterisk'
CASE_REC_MONTHLY_VAR = 'caseRecMonthly'
CASE_REC_MONTHLY_BLOCK_VAR = 'caseRecMonthlyBlock'
CASE_REC_MONTHLY_BLOCK_EXCEPTION_VAR = 'caseRecMonthlyBlockException'
CASE_REC_MONTHLY_INTERVAL_VAR = 'caseRecMonthlyInterval'
CASE_REC_MONTHLY_INTERVAL_EXCEPTION_VAR = 'caseRecMonthlyIntervalException'
CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR = 'caseRecMonthlyIntervalBlock'
CASE_REC_MONTHLY_INTERVAL_BLOCK_EXCEPTION_VAR = \
    'caseRecMonthlyIntervalBlockException'
CASE_REC_MONTHLYBYDAYOFWEEK_VAR = 'caseRecMonthlybydayofweek'
CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR = 'caseRecMonthlybydayofweekBlock'
CASE_REC_MONTHLYBYDAYOFWEEK_EXCEPTION_VAR = \
    'caseRecMonthlybydayofweekException'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR = \
    'caseRecMonthlybydayofweekInterval'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_EXCEPTION_VAR = \
    'caseRecMonthlybydayofweekIntervalException'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR = \
    'caseRecMonthlybydayofweekIntervalBlock'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_EXCEPTION_VAR = \
    'caseRecMonthlybydayofweekIntervalBlockException'
CASE_MONTHDAYYEAR_VAR = 'caseMonthdayyear'
CASE_MONTH_A_B_B_RDAYYEAR_VAR = 'caseMonthABBRdayyear'
CASE_MONTH_A_B_B_RDAYYEARWSPACE_VAR = 'caseMonthABBRdayyearwspace'
CASE_REC_YEARLY_VAR = 'caseRecYearly'
CASE_REC_YEARLY_EXCEPTION_VAR = 'caseRecYearlyException'
CASE_REC_YEARLY_A_B_B_R_B_VAR = 'caseRecYearlyABBRB'
CASE_REC_YEARLY_MODERN_VAR = 'caseRecYearlyModern'
CASE_REC_YEARLY_INTERVAL_VAR = 'caseRecYearlyInterval'
CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR = 'caseRecYearlyIntervalException'

# Constants first used in case template match templates
BYDAY_VAR = 'BYDAY'
STDAY_VAR = 'STDAY'
EXCEPTIONSTRING_VAR = 'EXCEPTIONSTRING'
WHICHWEEK_VAR = 'WHICHWEEK'
STYEAR_VAR = 'STYEAR'
STMONTH_VAR = 'STMONTH'
STMONTHABBR_VAR = 'STMONTHABBR'
ENDDAY_VAR = 'ENDDAY'
ENDMONTH_VAR = 'ENDMONTH'
ENDYEAR_VAR = 'ENDYEAR'
UNTILDAY_VAR = 'UNTILDAY'
UNTILMONTH_VAR = 'UNTILMONTH'
UNTILYEAR_VAR = 'UNTILYEAR'
STMONTH2_VAR = 'STMONTH2'
STDAY2_VAR = 'STDAY2'
STYEAR2_VAR = 'STYEAR2'
STYEAR3_VAR = 'STYEAR3'
STYEAR4_VAR = 'STYEAR4'
SOMEZING_VAR = 'SOMEZING'
SOMEZING2_VAR = 'SOMEZING2'
INTERVAL_VAR = 'INTERVAL'
NUMDAYOFWEEK_VAR = 'NUMDAYOFWEEK'
DAYOFWEEK_VAR = 'DAYOFWEEK'
DAYOFWEEKABBR_VAR = 'DAYOFWEEKABBR'
MONTHABBR_VAR = 'MONTHABBR'
STDAYNOTFOLLOWEDBYCOMMA_VAR = 'STDAYNOTFOLLOWEDBYCOMMA'
NOTFOLLOWEDBYCOMMA_VAR = 'notfollowedbycomma'
VIS_VAR = 'VIS'
DETAIL_VAR = 'DETAIL'

# Constants first used in detail template
DETAILS_C_TITLE_NEWLINE_SPACE_TIMERANGE_CONTENT_VAR = \
    'detailsC_title_newline_space_timerange_content'
DETAILS_F_TITLE_TIMERANGE_NEWLINE_CONTENT_VAR = \
    'detailsF_title_timerange_newline_content'
DETAILS_H_TITLE_TIMERANGE_I_I_CONTENT_VAR = \
    'detailsH_title_timerangeII_content'
DETAILS_I_TITLE_TIMERANGE_I_V_CONTENT_VAR = \
    'detailsI_title_timerangeIV_content'
DETAILS_J_TIMERANGE_I_I_TITLE_NEWLINE_CONTENT_VAR = \
    'detailsJ_timerangeII_title_newline_content'
DETAILS_K_TIMERANGE_I_I_TITLE_VAR = 'detailsK_timerangeII_title'
DETAILS_M_TIMERANGE_TITLE_NEWLINE_CONTENT_VAR = \
    'detailsM_timerange_title_newline_content'
DETAILS_P_TITLE_TIMERANGE_CONTENT_VAR = 'detailsP_title_timerange_content'
DETAILS_S_TIMERANGE_TITLE_VAR = 'detailsS_timerange_title'
DETAILS_Q_TITLE_TIMERANGE_VAR = 'detailsQ_title_timerange'
DETAILS_U_TITLE_NEWLINE_CONTENT_VAR = 'detailsU_title_newline_content'
DETAILS_X_TITLE_VAR = 'detailsX_title'

# Constants first used in detail template match
TITLE_VAR = 'TITLE'
TITLEIV_VAR = 'TITLEIV'
TITLEII_VAR = 'TITLEII'
CONTENT_VAR = 'CONTENT'
TIMERANGEIV_VAR = 'TIMERANGEIV'
TIMERANGEIII_VAR = 'TIMERANGEIII'
TIMERANGEII_VAR = 'TIMERANGEII'
TIMERANGEJJ_VAR = 'TIMERANGEJJ'
TIMERANGE_VAR = 'TIMERANGE'
NEWLINEHERE_VAR = 'newlinehere'

# Constants first used in recurrence event descriptions template
CASE_REC_DAILY_ASTERIX_EXCEPTION_VAR = 'caseRecDailyAsterixException'
CASE_REC_MONTHLY_EXCEPTION_VAR = 'caseRecMonthlyException'
CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_EXCEPTION_VAR = \
    'caseRecMonthlybydayofweekBlockException'

# Constants first used in recurrence event descriptions template match
DAYORDINAL_VAR = 'DAYORDINAL'
DAYOFWEEKD_VAR = 'DAYOFWEEKD'
WHICHWEEKD_VAR = 'WHICHWEEKD'
ONWHATDAYS_VAR = 'ONWHATDAYS'
INTERVALORDINAL_VAR = 'INTERVALORDINAL'
WHICHWEEKORDINAL_VAR = 'WHICHWEEKORDINAL'

# Constants first used in gcases template
CASE_REC_YEARLY_BLOCK_VAR = 'caseRecYearlyBlock'

# Constants first used in gcases template match
UNTILGTIME_VAR = 'UNTILGTIME'
BYDAYG_VAR = 'BYDAYG'
NEWLINE_VAR = 'newline'
STDATETIME_VAR = 'STDATETIME'
ENDDATETIME_VAR = 'ENDDATETIME'
UNTILDATETIME_VAR = 'UNTILDATETIME'
TZID_VAR = 'TZID'
TZID2_VAR = 'TZID2'

# Constants first used in times template
CASE_TIME_A_RANGE_VAR = 'caseTimeARange'
CASE_TIME_B_RANGEWITH_STARTTIME_MINUTES_ONLY_VAR = \
    'caseTimeBRangewithStarttimeMinutesOnly'
CASE_TIME_C_RANGEWITH_ENDTIME_MINUTES_ONLY_VAR = \
    'caseTimeCRangewithEndtimeMinutesOnly'
CASE_TIME_D_RANGEWITHOUT_MINUTES_VAR = 'caseTimeDRangewithoutMinutes'
CASE_TIME_E_STARTTIME_ONLY_VAR = 'caseTimeEStarttimeOnly'
CASE_TIME_F_STARTTIME_ONLYWITHOUT_MINUTES_VAR = \
    'caseTimeFStarttimeOnlywithoutMinutes'

# Constants first used in times template match
TAB_VAR = 'TAB'
HYPHEN_VAR = 'HYPHEN'
STHOUR_VAR = 'STHOUR'
STMINUTE_VAR = 'STMINUTE'
STAMPM_VAR = 'STAMPM'
STAMPMHYPHEN_VAR = 'STAMPMHYPHEN'
STAMPMNOHYPHEN_VAR = 'STAMPMNOHYPHEN'
ENDHOUR_VAR = 'ENDHOUR'
ENDMINUTE_VAR = 'ENDMINUTE'
ENDAMPM_VAR = 'ENDAMPM'

# Constants first used in the Emacs Diary to Google Calendar mapping
CASE_MDY_VAR = 'caseMDY'

# Common regular expressions used in many places
_MATCH_TEMPLATE_ALREADY_SEEN = '?P=%s'
_MATCH_TEMPLATE_NORMAL = '?P<%s>'

_AM_PM_RE = '(am|pm|AM|PM)'
_DATETIME_RE = '([12]0[012]\d(T[012][\d][0-5]\d[0-5]\d)?)'
_DAY_RE = '([0-3]?\d)'
_FOUR_DIGIT_YEAR_RE = '(\d{4})'
_GENERAL_RE = '(.+?)'
_HOUR_RE = '([012]?\d)'
_MINUTE_RE = '([0-5]\d)'
_MONTHABBR_RE = '([Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec)'
_MONTH_RE = '([01]?\d)'
_ONE_OR_TWO_DIGIT_RE = '(\d?\d)'
_REPEATED_VAR_RE = '()'
_SOMEZING_RE = '(.{4})'
_STDAYNOTFOLLOWEDBYCOMMA_RE = '([0-3]?\d)(?!,)'
_TITLE_RE = '(\w[\w \?\.\(\)\'"\[\]\-]+)'
_TWO_DIGIT_RE = '(/d/d)'
_YEAR_RE = '(20[0-3]\d)'

# Common portions in the cases templates
_DETAIL_TEMPLATE_PART = ' %(DETAIL)s'
_VIS_TEMPLATE_PART = '%(VIS)s'


def _evaluate_templates(template_dict, match_template_dict):
    """Evaluates the template s in template_dict by apply the
    match_template_dict and then compiling the resulting regular
    expression.  Also returns the dictionary of the regular expression
    strings for debugging and testing."""
    evaluated_templates_dict = {}
    evaluated_template_strings_dict = {}

    for template_name, template in template_dict.items():
        string_pattern = template % match_template_dict
        evaluated_template_strings_dict[template_name] = \
            string_pattern
        # print "Compiling template %s with regex: '%s'" % (template_name,
        #                                                   string_pattern)
        evaluated_templates_dict[template_name] = \
            re.compile(string_pattern, re.S | re.M)

    return evaluated_templates_dict, evaluated_template_strings_dict


def _gen_match_templates(match_templates):
    """Generates the match templates for a given dictionary in the
    standard form, returning a new dictionary with the updated
    values."""
    new_match_templates = {}

    for template_name, template in match_templates.items():
        if template_name.islower():
            new_template = template
        else:
            if (template_name[:-1] in match_templates.keys() and
                template_name[-1].isdigit()):
                template_internal = \
                    _MATCH_TEMPLATE_ALREADY_SEEN % template_name[:-1]
            else:
                template_internal = _MATCH_TEMPLATE_NORMAL % template_name

            # The match variable name goes right inside the first parenthesis
            loc = template.find("(")
            new_template = (template[:loc + 1] +
                            template_internal +
                            template[loc + 1:])

        new_match_templates[template_name] = new_template

    return new_match_templates


def _gen_raw_cases_templates(templates):
    """Generates the raw cases templates from the template parts
    common to all.
    """
    new_templates = {}

    for template_name, template in templates.items():
        new_templates[template_name] = (_VIS_TEMPLATE_PART +
                                        template +
                                        _DETAIL_TEMPLATE_PART)

    return new_templates


# cases_template describes the total number of ways that a given emacs
# diary entry date can be formatted.  Recurring cases contain the
# letters Rec.
# Templates for cases template
cases_template_raw = _gen_raw_cases_templates({
        CASE_REC_DAILY_ASTERIX_VAR: '\* \*, \*',
        CASE_REC_DAILY_VAR: '%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\) t\) ',
        CASE_REC_DAILY_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\)',
        CASE_REC_DAILY_ASTERISK_VAR: '\*/\*/\*',
        CASE_REC_DAILY_BLOCK_VAR: '%%%%\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)',
        CASE_REC_DAILY_BLOCK_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\)',
        CASE_REC_DAILY_INTERVAL_VAR: '%%%%\(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\)',
        CASE_REC_DAILY_INTERVAL_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\)\)',
        CASE_REC_DAILY_INTERVAL_BLOCK_VAR: '%%%%\(and \(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\)',
        CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\)',
        CASE_REC_WEEKLY_WEEKNAME_VAR: '%(DAYOFWEEK)s',
        CASE_REC_WEEKLY_ABBR_VAR: '%(DAYOFWEEKABBR)s',
        CASE_REC_WEEKLY_VAR: '%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\)',
        CASE_REC_WEEKLY_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\)',
        CASE_REC_WEEKLY_BLOCK_VAR: '%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\)',
        CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\)',
        CASE_REC_WEEKLY_INTERVAL_VAR: '%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\)',
        CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR: '%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\)',
        CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR: '%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\)',
        CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR: '%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\)',
        CASE_REC_MONTHLY_ASTERISK_VAR: '\* %(STDAY)s',
        CASE_REC_MONTHLY_VAR: '%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-date t %(STDAY2)s t\)\)',
        CASE_REC_MONTHLY_BLOCK_VAR: '%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\)',
        CASE_REC_MONTHLY_BLOCK_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\)',
        CASE_REC_MONTHLY_INTERVAL_VAR: '%%%%\(and \(diary-date t %(STDAY)s t\)\(diary-cyclic 1 %(STMONTH)s %(STDAY2)s %(STYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\)',
        CASE_REC_MONTHLY_INTERVAL_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-date t %(STDAY)s t\)\(diary-cyclic 1 %(STMONTH)s %(STDAY2)s %(STYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\)',
        CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR: '%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\)',
        CASE_REC_MONTHLY_INTERVAL_BLOCK_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\)',
        CASE_REC_MONTHLYBYDAYOFWEEK_VAR: '%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\)',
        CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR: '%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\)',
        CASE_REC_MONTHLYBYDAYOFWEEK_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\)',
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR: '%%%%\(and \(= \(mod \(- \(car date\) %(STMONTH)s\) %(INTERVAL)s\) 0\)\(diary-cyclic 1 %(STMONTH2)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\)',
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(= \(mod \(- \(car date\) %(STMONTH)s\) %(INTERVAL)s\) 0\)\(diary-cyclic 1 %(STMONTH2)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\)',
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR: '%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\)',
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\)',
        CASE_MONTHDAYYEAR_VAR: '%(STMONTH)s/%(STDAY)s/%(STYEAR)s',
        CASE_MONTH_A_B_B_RDAYYEAR_VAR:
            '%(MONTHABBR)s %(STDAY)s, %(STYEAR)s',
        CASE_MONTH_A_B_B_RDAYYEARWSPACE_VAR:
            '%(MONTHABBR)s  %(STDAY)s, %(STYEAR)s',
        CASE_REC_YEARLY_VAR: '%%%%\(and \(>= \(car \(cddr date\)\) %(STYEAR)s\)\(diary-anniversary %(STMONTH)s %(STDAY)s %(STYEAR2)s\)\)',
        CASE_REC_YEARLY_EXCEPTION_VAR: '%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(>= \(car \(cddr date\)\) %(STYEAR)s\)\(diary-anniversary %(STMONTH)s %(STDAY)s %(STYEAR2)s\)\)',
        CASE_REC_YEARLY_A_B_B_R_B_VAR:
            '%(MONTHABBR)s %(STDAYNOTFOLLOWEDBYCOMMA)s',
        CASE_REC_YEARLY_MODERN_VAR: '%(STMONTH)s/%(STDAY)s/\*',
        CASE_REC_YEARLY_INTERVAL_VAR: '%%%%\(or \(diary-date %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(and \(>= \(car \(cddr date\)\) %(STYEAR2)s\)\(diary-anniversary %(STMONTH2)s %(STDAY2)s %(STYEAR3)s\)\(= \(mod \(- \(car \(nthcdr 2 date\)\) %(STYEAR4)s\) %(INTERVAL)s\) 0\)\)\)',
        CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR: '%%%%\(or \(diary-date %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(>= \(car \(cddr date\)\) %(STYEAR2)s\)\(diary-anniversary %(STMONTH2)s %(STDAY2)s %(STYEAR3)s\)\(= \(mod \(- \(car \(cddr date\)\) %(STYEAR4)s\) %(INTERVAL)s\) 0\)\)\)', })

# The regular expression match templates for the case templates.
cases_template_mtch = _gen_match_templates({
        STYEAR_VAR: '(\d?\d?\d\d)',
        STDAY_VAR: _DAY_RE,
        ENDDAY_VAR: _DAY_RE,
        ENDMONTH_VAR: _MONTH_RE,
        VIS_VAR: '(&?)',
        STYEAR4_VAR: _REPEATED_VAR_RE,
        STYEAR3_VAR: _REPEATED_VAR_RE,
        STYEAR2_VAR: _REPEATED_VAR_RE,
        INTERVAL_VAR: '(\d+)',
        UNTILMONTH_VAR: _MONTH_RE,
        MONTHABBR_VAR: _MONTHABBR_RE,
        WHICHWEEK_VAR: '(-?[0-3])',
        BYDAY_VAR: '([0-6 ]{1,13})',
        DAYOFWEEKABBR_VAR: '([Ss]un|[Mm]on|[Tt]ue|[Ww]ed|[Tt]hu|[Ff]ri|[Ss]at)',
        STMONTHABBR_VAR: _MONTHABBR_RE,
        STMONTH2_VAR: _REPEATED_VAR_RE,
        STDAYNOTFOLLOWEDBYCOMMA_VAR: _STDAYNOTFOLLOWEDBYCOMMA_RE,
        DAYOFWEEK_VAR: '([Ss]unday|[Mm]onday|[Tt]uesday|[Ww]ednesday|[Tt]hursday|[Ff]riday|[Ss]aturday)',
        SOMEZING_VAR: _SOMEZING_RE,
        STMONTH_VAR: _MONTH_RE,
        NOTFOLLOWEDBYCOMMA_VAR: '(?!,)\n',
        DETAIL_VAR: '(.*?)(?=^[\w%&\d*])',
        SOMEZING2_VAR: _REPEATED_VAR_RE,
        UNTILYEAR_VAR: _YEAR_RE,
        ENDYEAR_VAR: _YEAR_RE,
        NUMDAYOFWEEK_VAR: '([0-6])',
        STDAY2_VAR: _REPEATED_VAR_RE,
        EXCEPTIONSTRING_VAR: '([\ddiary\-ent\(\)\' ]*?)',
        UNTILDAY_VAR: _DAY_RE, })

(cases_template,
 cases_template_strings) = _evaluate_templates(cases_template_raw,
                                               cases_template_mtch)


# Mapping from the Emacs Diary formatting to Google Calendar
e_to_g_case_table = {
    CASE_MONTHDAYYEAR_VAR: CASE_MDY_VAR,
    CASE_MONTH_A_B_B_RDAYYEAR_VAR: CASE_MDY_VAR,
    CASE_MONTH_A_B_B_RDAYYEARWSPACE_VAR: CASE_MDY_VAR,
    CASE_REC_DAILY_ASTERISK_VAR: CASE_REC_DAILY_VAR,
    CASE_REC_DAILY_ASTERIX_VAR: CASE_REC_DAILY_VAR,
    CASE_REC_DAILY_ASTERIX_EXCEPTION_VAR: CASE_REC_DAILY_EXCEPTION_VAR,
    CASE_REC_DAILY_VAR: CASE_REC_DAILY_VAR,
    CASE_REC_DAILY_EXCEPTION_VAR: CASE_REC_DAILY_VAR,
    CASE_REC_DAILY_BLOCK_VAR: CASE_REC_DAILY_BLOCK_VAR,
    CASE_REC_DAILY_BLOCK_EXCEPTION_VAR: CASE_REC_DAILY_BLOCK_VAR,
    CASE_REC_DAILY_INTERVAL_VAR: CASE_REC_DAILY_INTERVAL_VAR,
    CASE_REC_DAILY_INTERVAL_EXCEPTION_VAR: CASE_REC_DAILY_INTERVAL_VAR,
    CASE_REC_DAILY_INTERVAL_BLOCK_VAR: CASE_REC_DAILY_INTERVAL_BLOCK_VAR,
    CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR:
        CASE_REC_DAILY_INTERVAL_BLOCK_VAR,
    CASE_REC_WEEKLY_WEEKNAME_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_ABBR_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_EXCEPTION_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_BLOCK_VAR: CASE_REC_WEEKLY_BLOCK_VAR,
    CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR: CASE_REC_WEEKLY_BLOCK_VAR,
    CASE_REC_WEEKLY_INTERVAL_VAR: CASE_REC_WEEKLY_INTERVAL_VAR,
    CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR: CASE_REC_WEEKLY_INTERVAL_VAR,
    CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR: CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR,
    CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR:
        CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLY_VAR: CASE_REC_MONTHLY_VAR,
    CASE_REC_MONTHLY_ASTERISK_VAR: CASE_REC_MONTHLY_VAR,
    CASE_REC_MONTHLY_EXCEPTION_VAR: CASE_REC_MONTHLY_VAR,
    CASE_REC_MONTHLY_BLOCK_VAR: CASE_REC_MONTHLY_BLOCK_VAR,
    CASE_REC_MONTHLY_BLOCK_EXCEPTION_VAR: CASE_REC_MONTHLY_BLOCK_VAR,
    CASE_REC_MONTHLY_INTERVAL_VAR: CASE_REC_MONTHLY_INTERVAL_VAR,
    CASE_REC_MONTHLY_INTERVAL_EXCEPTION_VAR: CASE_REC_MONTHLY_INTERVAL_VAR,
    CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR:
        CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLY_INTERVAL_BLOCK_EXCEPTION_VAR:
        CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_EXCEPTION_VAR:
        CASE_REC_MONTHLYBYDAYOFWEEK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR:
        CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_EXCEPTION_VAR:
        CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR:
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_EXCEPTION_VAR:
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR:
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_EXCEPTION_VAR:
        CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR,
    CASE_REC_YEARLY_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_EXCEPTION_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_A_B_B_R_B_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_MODERN_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_INTERVAL_VAR: CASE_REC_YEARLY_INTERVAL_VAR,
    CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR: CASE_REC_YEARLY_INTERVAL_VAR, }


# detail_template describes the total number of ways that a given
# DETAIL field, from that of cases template, can be formatted in the
# diary file.
# Templates for detail template
detail_template_raw = {
    DETAILS_C_TITLE_NEWLINE_SPACE_TIMERANGE_CONTENT_VAR:
        '%(TITLE)s\\n\s%(TIMERANGE)s %(CONTENT)s',
    DETAILS_F_TITLE_TIMERANGE_NEWLINE_CONTENT_VAR:
        '%(TITLE)s %(TIMERANGE)s%(newlinehere)s%(CONTENT)s',
    DETAILS_H_TITLE_TIMERANGE_I_I_CONTENT_VAR:
        '%(TITLE)s %(TIMERANGEIII)s %(CONTENT)s',
    DETAILS_I_TITLE_TIMERANGE_I_V_CONTENT_VAR:
        '%(TITLE)s %(TIMERANGEIV)s %(CONTENT)s',
    DETAILS_J_TIMERANGE_I_I_TITLE_NEWLINE_CONTENT_VAR:
        '%(TIMERANGEII)s %(TITLE)s\\n\s%(CONTENT)s',
    DETAILS_K_TIMERANGE_I_I_TITLE_VAR: '%(TIMERANGEII)s %(TITLE)s',
    DETAILS_M_TIMERANGE_TITLE_NEWLINE_CONTENT_VAR:
        '%(TIMERANGE)s %(TITLE)s%(newlinehere)s%(CONTENT)s',
    DETAILS_P_TITLE_TIMERANGE_CONTENT_VAR:
        '%(TITLE)s %(TIMERANGE)s %(CONTENT)s',
    DETAILS_S_TIMERANGE_TITLE_VAR: '%(TIMERANGE)s %(TITLE)s',
    DETAILS_Q_TITLE_TIMERANGE_VAR: '%(TITLE)s %(TIMERANGE)s',
    DETAILS_U_TITLE_NEWLINE_CONTENT_VAR:
        '%(TITLE)s%(newlinehere)s%(CONTENT)s',
    DETAILS_X_TITLE_VAR: '%(TITLE)s', }

# The regular expression match templates for detail templates.
detail_template_mtch = _gen_match_templates({
        TIMERANGEIV_VAR: '(\d{1,2}(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))',
        TITLEII_VAR: '(\w[\w ]+(?=\\n[\s\\t]))',
        TIMERANGEJJ_VAR: '(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-?\s{0,8}(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))?)',
        TITLE_VAR: _TITLE_RE,
        TIMERANGEIII_VAR: '(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?:am|pm|AM|PM))',
        TIMERANGE_VAR: '((\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-\s{0,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))|(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)))',
        TIMERANGEII_VAR: '(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))',
        CONTENT_VAR: '(.+)',
        TITLEIV_VAR: _TITLE_RE,
        NEWLINEHERE_VAR: '\\\n', })

(detail_template,
 detail_template_strings) = _evaluate_templates(detail_template_raw,
                                                detail_template_mtch)


# Recurrence event descriptions templates
# Templates for recurrence event descriptions template
recurrence_event_descriptions_template_raw = {
    CASE_MONTHDAYYEAR_VAR: _SINGLE_DAY_EVENT_TEMPLATE_STRING,
    CASE_MONTH_A_B_B_RDAYYEAR_VAR: _SINGLE_DAY_EVENT_TEMPLATE_STRING,
    CASE_MONTH_A_B_B_RDAYYEARWSPACE_VAR: _SINGLE_DAY_EVENT_TEMPLATE_STRING,
    CASE_REC_DAILY_ASTERISK_VAR: _RECURS_DAILY_TEMPLATE_STRING,
    CASE_REC_DAILY_ASTERIX_VAR: _RECURS_DAILY_TEMPLATE_STRING,
    CASE_REC_DAILY_ASTERIX_EXCEPTION_VAR: _RECURS_DAILY_TEMPLATE_STRING,
    CASE_REC_DAILY_VAR:
        'Recurs Daily, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_EXCEPTION_VAR: 'Recurs Daily, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_BLOCK_VAR: 'Recurs Daily, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_DAILY_BLOCK_EXCEPTION_VAR: 'Recurs Daily, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_DAILY_INTERVAL_VAR: 'Recurs Every %(INTERVAL)s Days, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_INTERVAL_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Days, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_INTERVAL_BLOCK_VAR: 'Recurs Every %(INTERVAL)s Days, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Days, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_WEEKNAME_VAR: _RECURS_EVERY_WEEK_TEMPLATE_STRING,
    CASE_REC_WEEKLY_ABBR_VAR: _RECURS_EVERY_WEEK_TEMPLATE_STRING,
    CASE_REC_WEEKLY_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_BLOCK_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLY_VAR:
        'Recurs Monthly, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLY_ASTERISK_VAR: 'Recurs Monthly',
    CASE_REC_MONTHLY_EXCEPTION_VAR: 'Recurs Monthly, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLY_BLOCK_VAR: 'Recurs Monthly, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLY_BLOCK_EXCEPTION_VAR: 'Recurs Monthly, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLY_INTERVAL_VAR: 'Recurs Every %(INTERVAL)s Months, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLY_INTERVAL_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Months, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR: 'Recurs Every %(INTERVAL)s Months, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLY_INTERVAL_BLOCK_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Months, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every Month, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_EXCEPTION_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every Month, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every Month, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_EXCEPTION_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every Month, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every %(INTERVALORDINAL)s Month, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_EXCEPTION_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every %(INTERVALORDINAL)s Month, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every %(INTERVALORDINAL)s Month, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_EXCEPTION_VAR: 'Recurs on the %(WHICHWEEKORDINAL)s %(DAYOFWEEKD)s of Every %(INTERVALORDINAL)s Month, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_YEARLY_VAR:
        'Recurs Yearly, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_YEARLY_EXCEPTION_VAR: 'Recurs Yearly, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_YEARLY_A_B_B_R_B_VAR: _RECURS_YEARLY_TEMPLATE_STRING,
    CASE_REC_YEARLY_MODERN_VAR: _RECURS_YEARLY_TEMPLATE_STRING,
    CASE_REC_YEARLY_INTERVAL_VAR: 'Recurs Every %(INTERVAL)s Years, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Years, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s', }

# The match regular expressions associated with the recurrence event
# description templates.
recurrence_event_descriptions_template_mtch = _gen_match_templates({
        STYEAR_VAR: '(\d\d\d\d)',
        STDAY_VAR: _ONE_OR_TWO_DIGIT_RE,
        INTERVALORDINAL_VAR: _GENERAL_RE,
        ONWHATDAYS_VAR: _GENERAL_RE,
        DAYORDINAL_VAR: _GENERAL_RE,
        WHICHWEEKORDINAL_VAR: _GENERAL_RE,
        INTERVAL_VAR: _GENERAL_RE,
        UNTILYEAR_VAR: _FOUR_DIGIT_YEAR_RE,
        DAYOFWEEKD_VAR: _GENERAL_RE,
        WHICHWEEKD_VAR: _GENERAL_RE,
        UNTILMONTH_VAR: _ONE_OR_TWO_DIGIT_RE,
        STMONTH_VAR: _ONE_OR_TWO_DIGIT_RE,
        UNTILDAY_VAR: _ONE_OR_TWO_DIGIT_RE, })

(recurrence_event_descriptions_template,
 recurrence_event_descriptions_template_strings) = \
 _evaluate_templates(recurrence_event_descriptions_template_raw,
                     recurrence_event_descriptions_template_mtch)


# gcases_template describes the total number of ways that a recursion
# entry can be formatted in a google calendar feed.
# Templates for gcases template
gcases_template_raw = {
    CASE_REC_DAILY_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=DAILY;WKST=SU%(newline)s',
    CASE_REC_DAILY_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=DAILY;UNTIL=%(UNTILDATETIME)s;WKST=SU%(newline)s',
    CASE_REC_DAILY_INTERVAL_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=DAILY;INTERVAL=%(INTERVAL)s;WKST=SU%(newline)s',
    CASE_REC_DAILY_INTERVAL_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=DAILY;INTERVAL=%(INTERVAL)s;UNTIL=%(UNTILDATETIME)s;WKST=SU%(newline)s',
    CASE_REC_MONTHLY_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;WKST=SU;BYMONTHDAY=%(STDAY)s%(newline)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;BYDAY=%(WHICHWEEKG)s;WKST=SU%(newline)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;BYDAY=%(WHICHWEEKG)s;UNTIL=%(UNTILDATETIME)s;WKST=SU%(newline)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;INTERVAL=%(INTERVAL)s;BYDAY=%(WHICHWEEKG)s;WKST=SU%(newline)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;INTERVAL=%(INTERVAL)s;BYDAY=%(WHICHWEEKG)s;UNTIL=%(UNTILDATETIME)s;WKST=SU%(newline)s',
    CASE_REC_MONTHLY_INTERVAL_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;INTERVAL=%(INTERVAL)s;WKST=SU;BYMONTHDAY=%(STDAY)s%(newline)s',
    CASE_REC_MONTHLY_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;UNTIL=%(UNTILDATETIME)s;WKST=SU;BYMONTHDAY=%(STDAY)s%(newline)s',
    CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=MONTHLY;INTERVAL=%(INTERVAL)s;WKST=SU;BYMONTHDAY=%(STDAY)s;UNTIL=%(UNTILDATETIME)s%(newline)s',
    CASE_REC_WEEKLY_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=WEEKLY;BYDAY=%(BYDAYG)s;WKST=SU%(newline)s',
    CASE_REC_WEEKLY_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=WEEKLY;WKST=SU;UNTIL=%(UNTILDATETIME)s;BYDAY=%(BYDAYG)s%(newline)s',
    CASE_REC_WEEKLY_INTERVAL_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=WEEKLY;INTERVAL=%(INTERVAL)s;BYDAY=%(BYDAYG)s;WKST=SU%(newline)s',
    CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=WEEKLY;INTERVAL=%(INTERVAL)s;BYDAY=%(BYDAYG)s;UNTIL=%(UNTILDATETIME)s;WKST=SU%(newline)s',
    CASE_REC_YEARLY_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=YEARLY;WKST=SU%(newline)s',
    CASE_REC_YEARLY_BLOCK_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=YEARLY;UNTIL=%(UNTILDATETIME)s;WKST=SU%(newline)s',
    CASE_REC_YEARLY_INTERVAL_VAR: 'DTSTART;TZID=%(TZID)s:%(STDATETIME)s%(newline)sDTEND;TZID=%(TZID2)s:%(ENDDATETIME)s%(newline)sRRULE:FREQ=YEARLY;INTERVAL=%(INTERVAL)s;WKST=SU%(newline)s', }

# The regular expression match patterns associated with the gcases
# template.
gcases_template_mtch = _gen_match_templates({
        NEWLINE_VAR: '\\n\n',
        STDAY_VAR: _DAY_RE,
        TZID2_VAR: _REPEATED_VAR_RE,
        ENDDATETIME_VAR: _DATETIME_RE,
        UNTILGTIME_VAR: '(T[0-2]\d{3}00Z?)?)',
        TZID_VAR: _GENERAL_RE,
        UNTILDATETIME_VAR:
            '([12]0[012]\d(T[012][\d][0-5]\d[0-5]\dZ)?)',
        STDATETIME_VAR: _DATETIME_RE,
        INTERVAL_VAR: _ONE_OR_TWO_DIGIT_RE,
        BYDAYG_VAR: '([1234,MOTUWEHFR]+)', })


# times_template describes the total number of ways that a given
# TIMERANGE field, from that of details_template, can be formatted in
# the diary file.
# Templates for times template
times_template_raw = {
    CASE_TIME_A_RANGE_VAR: '%(STHOUR)s:%(STMINUTE)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s:%(ENDMINUTE)s%(ENDAMPM)s',
    CASE_TIME_B_RANGEWITH_STARTTIME_MINUTES_ONLY_VAR: '%(STHOUR)s:%(STMINUTE)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s%(ENDAMPM)s',
    CASE_TIME_C_RANGEWITH_ENDTIME_MINUTES_ONLY_VAR: '%(STHOUR)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s:%(ENDMINUTE)s%(ENDAMPM)s',
    CASE_TIME_D_RANGEWITHOUT_MINUTES_VAR:
        '%(STHOUR)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s%(ENDAMPM)s',
    CASE_TIME_E_STARTTIME_ONLY_VAR: '%(STHOUR)s:%(STMINUTE)s%(STAMPM)s',
    CASE_TIME_F_STARTTIME_ONLYWITHOUT_MINUTES_VAR: '%(STHOUR)s%(STAMPM)s', }

# The match templates associated with the times template
times_template_mtch = _gen_match_templates({
        ENDHOUR_VAR: _HOUR_RE,
        ENDAMPM_VAR: _AM_PM_RE,
        HYPHEN_VAR: '(\s{0,8}-\s{0,8})',
        STAMPM_VAR: _AM_PM_RE,
        STHOUR_VAR: _HOUR_RE,
        STAMPMHYPHEN_VAR: '(am|pm|AM|PM)[\s\\t]{0,8}-[\s	]{0,8}',
        STAMPMNOHYPHEN_VAR: '(am|pm|AM|PM)(?![\s\\t]{0,8}-[\s\\t]{0,8})',
        STMINUTE_VAR: _MINUTE_RE,
        TAB_VAR: '(?:\s+?)?',
        ENDMINUTE_VAR: _MINUTE_RE,
        STDAYNOTFOLLOWEDBYCOMMA_VAR: _STDAYNOTFOLLOWEDBYCOMMA_RE, })

(times_template,
 times_template_strings) = _evaluate_templates(times_template_raw,
                                               times_template_mtch)
