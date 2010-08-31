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
# TODO: Move rest of template related functionality (such as
#       evaluate_template) here from the original code.
# TODO: Figure out best way to generate template strings, reusing
#       constants already defined.
# TODO: Remove all regex and template fragment repetitions.
# TODO: Test against old template code, verifying exact same templates
#       and compiled regular expressions have been created.
# TODO: Should the constants be in the templates module namespace or
#       some sub-namespace?  Make sure only those items that are
#       needed outside of the module are exported.
# TODO: Verify which items must be exact strings and others which can
#       be renamed to something more understandable by me.
# TODO: Once all of the above are completed, evaluate whether this is
#       a maintainable solution for changing and adding templates.
import re

# Common regular expressions used in many places
STYEAR_RE = '(?P=STYEAR)'
STDAYNOTFOLLOWEDBYCOMMA_RE = '(?P<STDAYNOTFOLLOWEDBYCOMMA>[0-3]?\d)(?!,)'
STDAY_RE = '(?P<STDAY>[0-3]?\d)'

# Constants used in templates
# Common strings used in templates
RECURS_DAILY_TEMPLATE_STRING = 'Recurs Daily'
RECURS_EVERY_WEEK_TEMPLATE_STRING = 'Recurs Every Week'
RECURS_YEARLY_TEMPLATE_STRING = 'Recurs Yearly'
SINGLE_DAY_EVENT_TEMPLATE_STRING = 'Single Day Event'

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



def _evaluate_templates(template_dict, match_template_dict):
    evaluated_templates_dict = {}
    evaluated_template_strings_dict = {}
    for template_name, template in template_dict:
        string_pattern = template % match_template_dict
        evaluated_template_strings_dict[template_name] = \
            string_pattern
        evaluated_templates_dict[template_name] = \
            re.compile(string_pattern, re.S | re.M)

    return evaluated_templates_dict, evaluated_template_strings_dict



# cases_template describes the total number of ways that a given emacs
# diary entry date can be formatted.  Recurring cases contain the
# letters Rec.

# Fields are delimited by '<' and '>'; Uppercase fields refer to
# variable names, containing regexps which are found in
# cases_template_mtch.  Lowercase fields are place holders for literal
# strings as described in the detail_template_mtch template, and do
# not act as variables.

# Each formatting case is delimited in an XML like manner.  Number
# Postfixed variable names represent the same variable without the
# postfixed number.  The postfixed numbers must increment from left to
# right with respect to their relative positions in each case entry

# Templates for cases template
cases_template = {
    CASE_REC_DAILY_ASTERIX_VAR: '%(VIS)s\* \*, \* %(DETAIL)s',
    CASE_REC_DAILY_VAR: '%(VIS)s%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\) t\)  %(DETAIL)s',
    CASE_REC_DAILY_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\) %(DETAIL)s',
    CASE_REC_DAILY_ASTERISK_VAR: '%(VIS)s\*/\*/\* %(DETAIL)s',
    CASE_REC_DAILY_BLOCK_VAR: '%(VIS)s%%%%\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\) %(DETAIL)s',
    CASE_REC_DAILY_BLOCK_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\) %(DETAIL)s',
    CASE_REC_DAILY_INTERVAL_VAR: '%(VIS)s%%%%\(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\) %(DETAIL)s',
    CASE_REC_DAILY_INTERVAL_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\)\) %(DETAIL)s',
    CASE_REC_DAILY_INTERVAL_BLOCK_VAR: '%(VIS)s%%%%\(and \(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\) %(DETAIL)s',
    CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic %(INTERVAL)s %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_WEEKNAME_VAR: '%(VIS)s%(DAYOFWEEK)s %(DETAIL)s',
    CASE_REC_WEEKLY_ABBR_VAR: '%(VIS)s%(DAYOFWEEKABBR)s %(DETAIL)s',
    CASE_REC_WEEKLY_VAR: '%(VIS)s%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_BLOCK_VAR: '%(VIS)s%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(memq \(calendar-day-of-week date\) \'\(%(BYDAY)s\)\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_INTERVAL_VAR: '%(VIS)s%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR: '%(VIS)s%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR: '%(VIS)s%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\) %(DETAIL)s',
    CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR: '%(VIS)s%%%%\(let \(\(dayname \(calendar-day-of-week date\)\)\(strtwkno \(string-to-number \(format-time-string %(SOMEZING)s \(encode-time 1 1 1 %(STDAY)s %(STMONTH)s %(STYEAR)s\)\)\)\)\(weekno \(string-to-number \(format-time-string %(SOMEZING2)s \(encode-time 1 1 1 \(car \(cdr date\)\)\(car date\)\(car \(nthcdr 2 date\)\)\)\)\)\)\)\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH2)s %(STDAY2)s %(STYEAR2)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- weekno strtwkno\) %(INTERVAL)s\) 0\)\(memq dayname \'\(%(BYDAY)s\)\)\)\) %(DETAIL)s',
    CASE_REC_MONTHLY_ASTERISK_VAR: '%(VIS)s\* %(STDAY)s %(DETAIL)s',
    CASE_REC_MONTHLY_VAR: '%(VIS)s%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-date t %(STDAY2)s t\)\) %(DETAIL)s',
    CASE_REC_MONTHLY_BLOCK_VAR: '%(VIS)s%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLY_BLOCK_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLY_INTERVAL_VAR: '%(VIS)s%%%%\(and \(diary-date t %(STDAY)s t\)\(diary-cyclic 1 %(STMONTH)s %(STDAY2)s %(STYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\) %(DETAIL)s',
    CASE_REC_MONTHLY_INTERVAL_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-date t %(STDAY)s t\)\(diary-cyclic 1 %(STMONTH)s %(STDAY2)s %(STYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\) %(DETAIL)s',
    CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR: '%(VIS)s%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\) %(DETAIL)s',
    CASE_REC_MONTHLY_INTERVAL_BLOCK_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(car \(cdr date\)\) %(STDAY2)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\) %(DETAIL)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_VAR: '%(VIS)s%%%%\(and \(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR: '%(VIS)s%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-cyclic 1 %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR: '%(VIS)s%%%%\(and \(= \(mod \(- \(car date\) %(STMONTH)s\) %(INTERVAL)s\) 0\)\(diary-cyclic 1 %(STMONTH2)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(= \(mod \(- \(car date\) %(STMONTH)s\) %(INTERVAL)s\) 0\)\(diary-cyclic 1 %(STMONTH2)s %(STDAY)s %(STYEAR)s\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR: '%(VIS)s%%%%\(and \(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\) %(DETAIL)s',
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(diary-block %(STMONTH)s %(STDAY)s %(STYEAR)s %(UNTILMONTH)s %(UNTILDAY)s %(UNTILYEAR)s\)\(= \(mod \(- \(car date\) %(STMONTH2)s\) %(INTERVAL)s\) 0\)\(diary-float t %(NUMDAYOFWEEK)s %(WHICHWEEK)s\)\) %(DETAIL)s',
    CASE_MONTHDAYYEAR_VAR: '%(VIS)s%(STMONTH)s/%(STDAY)s/%(STYEAR)s %(DETAIL)s',
    CASE_MONTH_A_B_B_RDAYYEAR_VAR: '%(VIS)s%(MONTHABBR)s %(STDAY)s, %(STYEAR)s %(DETAIL)s',
    CASE_MONTH_A_B_B_RDAYYEARWSPACE_VAR: '%(VIS)s%(MONTHABBR)s  %(STDAY)s, %(STYEAR)s %(DETAIL)s',
    CASE_REC_YEARLY_VAR: '%(VIS)s%%%%\(and \(>= \(car \(cddr date\)\) %(STYEAR)s\)\(diary-anniversary %(STMONTH)s %(STDAY)s %(STYEAR2)s\)\) %(DETAIL)s',
    CASE_REC_YEARLY_EXCEPTION_VAR: '%(VIS)s%%%%\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(>= \(car \(cddr date\)\) %(STYEAR)s\)\(diary-anniversary %(STMONTH)s %(STDAY)s %(STYEAR2)s\)\) %(DETAIL)s',
    CASE_REC_YEARLY_A_B_B_R_B_VAR: '%(VIS)s%(MONTHABBR)s %(STDAYNOTFOLLOWEDBYCOMMA)s %(DETAIL)s',
    CASE_REC_YEARLY_MODERN_VAR: '%(VIS)s%(STMONTH)s/%(STDAY)s/\* %(DETAIL)s',
    CASE_REC_YEARLY_INTERVAL_VAR: '%(VIS)s%%%%\(or \(diary-date %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(and \(>= \(car \(cddr date\)\) %(STYEAR2)s\)\(diary-anniversary %(STMONTH2)s %(STDAY2)s %(STYEAR3)s\)\(= \(mod \(- \(car \(nthcdr 2 date\)\) %(STYEAR4)s\) %(INTERVAL)s\) 0\)\)\) %(DETAIL)s',
    CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR: '%(VIS)s%%%%\(or \(diary-date %(STMONTH)s %(STDAY)s %(STYEAR)s\)\(and \(not \(or \(diary-date %(EXCEPTIONSTRING)s\)\)\)\(>= \(car \(cddr date\)\) %(STYEAR2)s\)\(diary-anniversary %(STMONTH2)s %(STDAY2)s %(STYEAR3)s\)\(= \(mod \(- \(car \(cddr date\)\) %(STYEAR4)s\) %(INTERVAL)s\) 0\)\)\) %(DETAIL)s', }


# The regular expression match templates for the case templates.
cases_template_mtch = {
    STYEAR_VAR: '(?P<STYEAR>\d?\d?\d\d)',
    STDAY_VAR: STDAY_RE,
    ENDDAY_VAR: '(?P<ENDDAY>[0-3]?\d)',
    ENDMONTH_VAR: '(?P<ENDMONTH>[01]?\d)',
    VIS_VAR: '(?P<VIS>&?)',
    STYEAR4_VAR: STYEAR_RE,
    STYEAR3_VAR: STYEAR_RE,
    STYEAR2_VAR: STYEAR_RE,
    INTERVAL_VAR: '(?P<INTERVAL>\d+)',
    UNTILMONTH_VAR: '(?P<UNTILMONTH>[01]?\d)',
    MONTHABBR_VAR: '(?P<MONTHABBR>[Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec)',
    WHICHWEEK_VAR: '(?P<WHICHWEEK>-?[0-3])',
    BYDAY_VAR: '(?P<BYDAY>[0-6 ]{1,13})',
    DAYOFWEEKABBR_VAR: '(?P<DAYOFWEEKABBR>[Ss]un|[Mm]on|[Tt]ue|[Ww]ed|[Tt]hu|[Ff]ri|[Ss]at)',
    STMONTHABBR_VAR: '(?P<STMONTHABBR>[Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ept|[Oo]ct|[Nn]ov|[Dd]ec)',
    STMONTH2_VAR: '(?P=STMONTH)',
    STDAYNOTFOLLOWEDBYCOMMA_VAR: STDAYNOTFOLLOWEDBYCOMMA_RE,
    DAYOFWEEK_VAR: '(?P<DAYOFWEEK>[Ss]unday|[Mm]onday|[Tt]uesday|[Ww]ednesday|[Tt]hursday|[Ff]riday|[Ss]aturday)',
    SOMEZING_VAR: '(?P<SOMEZING>.{4})',
    STMONTH_VAR: '(?P<STMONTH>[01]?\d)',
    NOTFOLLOWEDBYCOMMA_VAR: '(?!,)\n',
    DETAIL_VAR: '(?P<DETAIL>.*?)(?=^[\w%&\d*])',
    SOMEZING2_VAR: '(?P=SOMEZING)',
    UNTILYEAR_VAR: '(?P<UNTILYEAR>20[0-3]\d)',
    ENDYEAR_VAR: '(?P<ENDYEAR>20[0-3]\d)',
    NUMDAYOFWEEK_VAR: '(?P<NUMDAYOFWEEK>[0-6])',
    STDAY2_VAR: '(?P=STDAY)',
    EXCEPTIONSTRING_VAR: '(?P<EXCEPTIONSTRING>[\ddiary\-ent\(\)\' ]*?)',
    UNTILDAY_VAR: '(?P<UNTILDAY>[0-3]?\d)', }


# detail_template describes the total number of ways that a given
# <DETAIL> field, from that of cases_template, can be formatted in the
# diary file.  Fields are delimited by '<' and '>'; Uppercase fields
# refer to variable names, containing regexps which are found in
# detail_template_mtch.  Lowercase fields are place holders for
# literal strings as described in the detail_template_mtch template,
# and do not act as variables.  Each formatting case is delimited in
# an XML like manner.  .\n must be double escaped e.g. \\n

# Templates for detail template
detail_template = {
    DETAILS_C_TITLE_NEWLINE_SPACE_TIMERANGE_CONTENT_VAR: '%(TITLE)s\\n\s%(TIMERANGE)s %(CONTENT)s',
    DETAILS_F_TITLE_TIMERANGE_NEWLINE_CONTENT_VAR: '%(TITLE)s %(TIMERANGE)s%(newlinehere)s%(CONTENT)s',
    DETAILS_H_TITLE_TIMERANGE_I_I_CONTENT_VAR: '%(TITLE)s %(TIMERANGEIII)s %(CONTENT)s',
    DETAILS_I_TITLE_TIMERANGE_I_V_CONTENT_VAR: '%(TITLE)s %(TIMERANGEIV)s %(CONTENT)s',
    DETAILS_J_TIMERANGE_I_I_TITLE_NEWLINE_CONTENT_VAR: '%(TIMERANGEII)s %(TITLE)s\\n\s%(CONTENT)s',
    DETAILS_K_TIMERANGE_I_I_TITLE_VAR: '%(TIMERANGEII)s %(TITLE)s',
    DETAILS_M_TIMERANGE_TITLE_NEWLINE_CONTENT_VAR: '%(TIMERANGE)s %(TITLE)s%(newlinehere)s%(CONTENT)s',
    DETAILS_P_TITLE_TIMERANGE_CONTENT_VAR: '%(TITLE)s %(TIMERANGE)s %(CONTENT)s',
    DETAILS_S_TIMERANGE_TITLE_VAR: '%(TIMERANGE)s %(TITLE)s',
    DETAILS_Q_TITLE_TIMERANGE_VAR: '%(TITLE)s %(TIMERANGE)s',
    DETAILS_U_TITLE_NEWLINE_CONTENT_VAR: '%(TITLE)s%(newlinehere)s%(CONTENT)s',
    DETAILS_X_TITLE_VAR: '%(TITLE)s', }

# The regular expression match templates for detail templates.
detail_template_mtch = {
    TIMERANGEIV_VAR: '(?P<TIMERANGEIV>\d{1,2}(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))',
    TITLEII_VAR: '(?P<TITLEII>\w[\w ]+(?=\\n[\s\\t]))',
    TIMERANGEJJ_VAR: '(?P<TIMERANGEJJ>\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-?\s{0,8}(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))?)',
    TITLE_VAR: '(?P<TITLE>\w[\w \?\.\(\)\'"\[\]\-]+)',
    TIMERANGEIII_VAR: '(?P<TIMERANGEIII>\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?:am|pm|AM|PM))',
    TIMERANGE_VAR: '(?P<TIMERANGE>(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-\s{0,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))|(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)))',
    TIMERANGEII_VAR: '(?P<TIMERANGEII>\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))',
    CONTENT_VAR: '(?P<CONTENT>.+)',
    TITLEIV_VAR: '(?P<TITLEIV>\w[\w \?\.\(\)\'"\[\]\-]+)',
    NEWLINEHERE_VAR: '\\\n', }



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
    CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR: CASE_REC_DAILY_INTERVAL_BLOCK_VAR,
    CASE_REC_WEEKLY_WEEKNAME_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_ABBR_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_EXCEPTION_VAR: CASE_REC_WEEKLY_VAR,
    CASE_REC_WEEKLY_BLOCK_VAR: CASE_REC_WEEKLY_BLOCK_VAR,
    CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR: CASE_REC_WEEKLY_BLOCK_VAR,
    CASE_REC_WEEKLY_INTERVAL_VAR: CASE_REC_WEEKLY_INTERVAL_VAR,
    CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR: CASE_REC_WEEKLY_INTERVAL_VAR,
    CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR: CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR,
    CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR: CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLY_VAR: CASE_REC_MONTHLY_VAR,
    CASE_REC_MONTHLY_ASTERISK_VAR: CASE_REC_MONTHLY_VAR,
    CASE_REC_MONTHLY_EXCEPTION_VAR: CASE_REC_MONTHLY_VAR,
    CASE_REC_MONTHLY_BLOCK_VAR: CASE_REC_MONTHLY_BLOCK_VAR,
    CASE_REC_MONTHLY_BLOCK_EXCEPTION_VAR: CASE_REC_MONTHLY_BLOCK_VAR,
    CASE_REC_MONTHLY_INTERVAL_VAR: CASE_REC_MONTHLY_INTERVAL_VAR,
    CASE_REC_MONTHLY_INTERVAL_EXCEPTION_VAR: CASE_REC_MONTHLY_INTERVAL_VAR,
    CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR: CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLY_INTERVAL_BLOCK_EXCEPTION_VAR: CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_EXCEPTION_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_EXCEPTION_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_EXCEPTION_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR,
    CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_EXCEPTION_VAR: CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR,
    CASE_REC_YEARLY_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_EXCEPTION_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_A_B_B_R_B_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_MODERN_VAR: CASE_REC_YEARLY_VAR,
    CASE_REC_YEARLY_INTERVAL_VAR: CASE_REC_YEARLY_INTERVAL_VAR,
    CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR: CASE_REC_YEARLY_INTERVAL_VAR, }



# Recurrence event descriptions templates
# Templates for recurrence event descriptions template
recurrence_event_descriptions_template = {
    CASE_MONTHDAYYEAR_VAR: SINGLE_DAY_EVENT_TEMPLATE_STRING,
    CASE_MONTH_A_B_B_RDAYYEAR_VAR: SINGLE_DAY_EVENT_TEMPLATE_STRING,
    CASE_MONTH_A_B_B_RDAYYEARWSPACE_VAR: SINGLE_DAY_EVENT_TEMPLATE_STRING,
    CASE_REC_DAILY_ASTERISK_VAR: RECURS_DAILY_TEMPLATE_STRING,
    CASE_REC_DAILY_ASTERIX_VAR: RECURS_DAILY_TEMPLATE_STRING,
    CASE_REC_DAILY_ASTERIX_EXCEPTION_VAR: RECURS_DAILY_TEMPLATE_STRING,
    CASE_REC_DAILY_VAR: 'Recurs Daily, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_EXCEPTION_VAR: 'Recurs Daily, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_BLOCK_VAR: 'Recurs Daily, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_DAILY_BLOCK_EXCEPTION_VAR: 'Recurs Daily, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_DAILY_INTERVAL_VAR: 'Recurs Every %(INTERVAL)s Days, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_INTERVAL_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Days, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_DAILY_INTERVAL_BLOCK_VAR: 'Recurs Every %(INTERVAL)s Days, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Days, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_WEEKNAME_VAR: RECURS_EVERY_WEEK_TEMPLATE_STRING,
    CASE_REC_WEEKLY_ABBR_VAR: RECURS_EVERY_WEEK_TEMPLATE_STRING,
    CASE_REC_WEEKLY_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_BLOCK_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery Week, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR: 'Recurs%(ONWHATDAYS)sEvery %(INTERVAL)s Weeks, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s, Until %(UNTILMONTH)s/%(UNTILDAY)s/%(UNTILYEAR)s',
    CASE_REC_MONTHLY_VAR: 'Recurs Monthly, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
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
    CASE_REC_YEARLY_VAR: 'Recurs Yearly, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_YEARLY_EXCEPTION_VAR: 'Recurs Yearly, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_YEARLY_A_B_B_R_B_VAR: RECURS_YEARLY_TEMPLATE_STRING,
    CASE_REC_YEARLY_MODERN_VAR: RECURS_YEARLY_TEMPLATE_STRING,
    CASE_REC_YEARLY_INTERVAL_VAR: 'Recurs Every %(INTERVAL)s Years, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s',
    CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR: 'Recurs Every %(INTERVAL)s Years, With Exceptions, Beginning %(STMONTH)s/%(STDAY)s/%(STYEAR)s', }

# The match regular expressions associated with the recurrence event
# description templates.
recurrence_event_descriptions_template_mtch = {
    STYEAR_VAR: '(?P<STYEAR>\d\d\d\d)',
    STDAY_VAR: '(?P<STDAY>\d?\d)',
    INTERVALORDINAL_VAR: '(?P<INTERVALORDINAL>.+?)',
    ONWHATDAYS_VAR: '(?P<ONWHATDAYS>.+?)',
    DAYORDINAL_VAR: '(?P<DAYORDINAL>.+?)',
    WHICHWEEKORDINAL_VAR: '(?P<WHICHWEEKORDINAL>.+?)',
    INTERVAL_VAR: '(?P<INTERVAL>.+?)',
    UNTILYEAR_VAR: '(?P<UNTILYEAR>\d{4})',
    DAYOFWEEKD_VAR: '(?P<DAYOFWEEKD>.+?)',
    WHICHWEEKD_VAR: '(?P<WHICHWEEKD>.+?)',
    UNTILMONTH_VAR: '(?P<UNTILMONTH>\d?\d)',
    STMONTH_VAR: '(?P<STMONTH>\d?\d)',
    UNTILDAY_VAR: '(?P<UNTILDAY>\d?\d)', }



# gcases_template describes the total number of ways that a recursion
# entry can be formatted in a google calendar feed.
# Templates for gcases template
gcases_template = {
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
gcases_template_mtch = {
    NEWLINE_VAR: '\\n\n',
    STDAY_VAR: STDAY_RE,
    TZID2_VAR: '(?P=TZID)',
    ENDDATETIME_VAR: '(?P<ENDDATETIME>[12]0[012]\d(T[012][\d][0-5]\d[0-5]\d)?)',
    UNTILGTIME_VAR: '(?P<UNTILGTIME>T[0-2]\d{3}00Z?)?)',
    TZID_VAR: '(?P<TZID>.+?)',
    UNTILDATETIME_VAR: '(?P<UNTILDATETIME>[12]0[012]\d(T[012][\d][0-5]\d[0-5]\dZ)?)',
    STDATETIME_VAR: '(?P<STDATETIME>[12]0[012]\d(T[012][\d][0-5]\d[0-5]\d)?)',
    INTERVAL_VAR: '(?P<INTERVAL>\d?\d)',
    BYDAYG_VAR: '(?P<BYDAYG>[1234,MOTUWEHFR]+)', }


# times_template describes the total number of ways that a given
# <TIMERANGE> field, from that of details_template, can be formatted in
# the diary file.
# Templates for times template
times_template = {
    CASE_TIME_A_RANGE_VAR: '%(STHOUR)s:%(STMINUTE)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s:%(ENDMINUTE)s%(ENDAMPM)s',
    CASE_TIME_B_RANGEWITH_STARTTIME_MINUTES_ONLY_VAR: '%(STHOUR)s:%(STMINUTE)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s%(ENDAMPM)s',
    CASE_TIME_C_RANGEWITH_ENDTIME_MINUTES_ONLY_VAR: '%(STHOUR)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s:%(ENDMINUTE)s%(ENDAMPM)s',
    CASE_TIME_D_RANGEWITHOUT_MINUTES_VAR: '%(STHOUR)s%(STAMPM)s%(HYPHEN)s%(ENDHOUR)s%(ENDAMPM)s',
    CASE_TIME_E_STARTTIME_ONLY_VAR: '%(STHOUR)s:%(STMINUTE)s%(STAMPM)s',
    CASE_TIME_F_STARTTIME_ONLYWITHOUT_MINUTES_VAR: '%(STHOUR)s%(STAMPM)s', }

# The match templates associated with the times template
times_template_mtch = {
    ENDHOUR_VAR: '(?P<ENDHOUR>[012]?\d)',
    ENDAMPM_VAR: '(?P<ENDAMPM>am|pm|AM|PM)',
    HYPHEN_VAR: '(?P<HYPHEN>\s{0,8}-\s{0,8})',
    STAMPM_VAR: '(?P<STAMPM>am|pm|AM|PM)',
    STHOUR_VAR: '(?P<STHOUR>[012]?\d)',
    STAMPMHYPHEN_VAR: '(?P<STAMPMHYPHEN>am|pm|AM|PM)[\s\\t]{0,8}-[\s	]{0,8}',
    STAMPMNOHYPHEN_VAR: '(?P<STAMPMNOHYPHEN>am|pm|AM|PM)(?![\s\\t]{0,8}-[\s\\t]{0,8})',
    STMINUTE_VAR: '(?P<STMINUTE>[0-5]\d)',
    TAB_VAR: '(?P<TAB>?:\s+?)?',
    ENDMINUTE_VAR: '(?P<ENDMINUTE>[0-5]\d)',
    STDAYNOTFOLLOWEDBYCOMMA_VAR: STDAYNOTFOLLOWEDBYCOMMA_RE, }
