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

import re

# Common regular expressions used in many places
AM_PM_RE = '(am|pm|AM|PM)'
DATETIME_RE = '([12]0[012]\d(T[012][\d][0-5]\d[0-5]\d)?)'
DAY_RE = '([0-3]?\d)'
FOUR_DIGIT_YEAR_RE = '(\d{4})'
GENERAL_RE = '(.+?)'
HOUR_RE = '([012]?\d)'
MINUTE_RE = '([0-5]\d)'
MONTH_RE = '([01]?\d)'
ONE_OR_TWO_DIGIT_RE = '(\d?\d)'
SOMEZING_RE = '(.{4})'
STDAYNOTFOLLOWEDBYCOMMA_RE = '([0-3]?\d)(?!,)'
TITLE_RE = '(\w[\w \?\.\(\)\'"\[\]\-]+)'
TWO_DIGIT_RE = '(/d/d)'
YEAR_RE = '(20[0-3]\d)'

# Constants for template variables
# Constants first used in case_template_mtch
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
# Constants used in detail_template_mtch
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
# Constants first used in recurrence_event_descriptions_template_mtch
DAYORDINAL_VAR = 'DAYORDINAL'
DAYOFWEEKD_VAR = 'DAYOFWEEKD'
WHICHWEEKD_VAR = 'WHICHWEEKD'
ONWHATDAYS_VAR = 'ONWHATDAYS'
INTERVALORDINAL_VAR = 'INTERVALORDINAL'
WHICHWEEKORDINAL_VAR = 'WHICHWEEKORDINAL'
# Constants first used in gcases_template_mtch
UNTILGTIME_VAR = 'UNTILGTIME'
BYDAYG_VAR = 'BYDAYG'
NEWLINE_VAR = 'newline'
STDATETIME_VAR = 'STDATETIME'
ENDDATETIME_VAR = 'ENDDATETIME'
UNTILDATETIME_VAR = 'UNTILDATETIME'
TZID_VAR = 'TZID'
TZID2_VAR = 'TZID2'
# Constants first used in times_template_mtch
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

# Constants used in the Emacs Diary to Google Calendar mapping
CASE_MONTHDAYYEAR_VAR = 'caseMonthdayyear'
CASE_MDY_VAR = 'caseMDY'
CASE_MONTH_A_B_B_RDAYYEAR_VAR = 'caseMonthABBRdayyear'
CASE_MONTH_A_B_B_RDAYYEARWSPACE_VAR = 'caseMonthABBRdayyearwspace'
CASE_REC_DAILY_ASTERISK_VAR = 'caseRecDailyAsterisk'
CASE_REC_DAILY_ASTERIX_VAR = 'caseRecDailyAsterix'
CASE_REC_DAILY_ASTERIX_EXCEPTION_VAR = 'caseRecDailyAsterixException'
CASE_REC_DAILY_EXCEPTION_VAR = 'caseRecDailyException'
CASE_REC_DAILY_VAR = 'caseRecDaily'
CASE_REC_DAILY_BLOCK_VAR = 'caseRecDailyBlock'
CASE_REC_DAILY_BLOCK_EXCEPTION_VAR = 'caseRecDailyBlockException'
CASE_REC_DAILY_INTERVAL_VAR = 'caseRecDailyInterval'
CASE_REC_DAILY_INTERVAL_EXCEPTION_VAR = 'caseRecDailyIntervalException'
CASE_REC_DAILY_INTERVAL_BLOCK_VAR = 'caseRecDailyIntervalBlock'
CASE_REC_DAILY_INTERVAL_BLOCK_EXCEPTION_VAR = 'caseRecDailyIntervalBlockException'
CASE_REC_WEEKLY_WEEKNAME_VAR = 'caseRecWeeklyWeekname'
CASE_REC_WEEKLY_VAR = 'caseRecWeekly'
CASE_REC_WEEKLY_ABBR_VAR = 'caseRecWeeklyAbbr'
CASE_REC_WEEKLY_EXCEPTION_VAR = 'caseRecWeeklyException'
CASE_REC_WEEKLY_BLOCK_VAR = 'caseRecWeeklyBlock'
CASE_REC_WEEKLY_BLOCK_EXCEPTION_VAR = 'caseRecWeeklyBlockException'
CASE_REC_WEEKLY_INTERVAL_VAR = 'caseRecWeeklyInterval'
CASE_REC_WEEKLY_INTERVAL_EXCEPTION_VAR = 'caseRecWeeklyIntervalException'
CASE_REC_WEEKLY_INTERVAL_BLOCK_VAR = 'caseRecWeeklyIntervalBlock'
CASE_REC_WEEKLY_INTERVAL_BLOCK_EXCEPTION_VAR = 'caseRecWeeklyIntervalBlockException'
CASE_REC_MONTHLY_VAR = 'caseRecMonthly'
CASE_REC_MONTHLY_ASTERISK_VAR = 'caseRecMonthlyAsterisk'
CASE_REC_MONTHLY_EXCEPTION_VAR = 'caseRecMonthlyException'
CASE_REC_MONTHLY_BLOCK_VAR = 'caseRecMonthlyBlock'
CASE_REC_MONTHLY_BLOCK_EXCEPTION_VAR = 'caseRecMonthlyBlockException'
CASE_REC_MONTHLY_INTERVAL_VAR = 'caseRecMonthlyInterval'
CASE_REC_MONTHLY_INTERVAL_EXCEPTION_VAR = 'caseRecMonthlyIntervalException'
CASE_REC_MONTHLY_INTERVAL_BLOCK_VAR = 'caseRecMonthlyIntervalBlock'
CASE_REC_MONTHLY_INTERVAL_BLOCK_EXCEPTION_VAR = 'caseRecMonthlyIntervalBlockException'
CASE_REC_MONTHLYBYDAYOFWEEK_VAR = 'caseRecMonthlybydayofweek'
CASE_REC_MONTHLYBYDAYOFWEEK_EXCEPTION_VAR = 'caseRecMonthlybydayofweekException'
CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_VAR = 'caseRecMonthlybydayofweekBlock'
CASE_REC_MONTHLYBYDAYOFWEEK_BLOCK_EXCEPTION_VAR = 'caseRecMonthlybydayofweekBlockException'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_VAR = 'caseRecMonthlybydayofweekInterval'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_EXCEPTION_VAR = 'caseRecMonthlybydayofweekIntervalException'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_VAR = 'caseRecMonthlybydayofweekIntervalBlock'
CASE_REC_MONTHLYBYDAYOFWEEK_INTERVAL_BLOCK_EXCEPTION_VAR = 'caseRecMonthlybydayofweekIntervalBlockException'
CASE_REC_YEARLY_VAR = 'caseRecYearly'
CASE_REC_YEARLY_EXCEPTION_VAR = 'caseRecYearlyException'
CASE_REC_YEARLY_A_B_B_R_B_VAR = 'caseRecYearlyABBRB'
CASE_REC_YEARLY_MODERN_VAR = 'caseRecYearlyModern'
CASE_REC_YEARLY_INTERVAL_VAR = 'caseRecYearlyInterval'
CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR = 'caseRecYearlyIntervalException'



def _compile_match_templates(match_template_dict):
    """Compiles the regular expressions in the match template
    dictionary, reassigning the values to be the regular expression
    object.
    """
    new_match_template_dict = {}
    for template_name, regex in match_template_dict.items():
        new_match_template_dict[template_name] = \
            re.compile(re.escape(regex), re.S | re.M)

    return new_match_template_dict


### TEMPLATES

### The \n and \t characters must be double escaped in all template
###   variables, e.g. \\n \\t ,
### they must be tripple escaped in _mtch variables, e.g. \\\n  ###
###   No escape is necessary when the templates are stored in separate files

### cases_template describes the total number of ways that a given
### emacs diary entry date can be formatted.  Recurring cases contain
### the letters Rec.

### Fields are delimited by '<' and '>'; Uppercase fields refer to
### variable names, containing regexps which are found in
### cases_template_mtch.  Lowercase fields are place holders for
### literal strings as described in the detail_template_mtch template,
### and do not act as variables.

### Each formatting case is delimited in an XML like manner.
### Number Postfixed variable names represent the same variable
### without the postfixed number.  The postfixed numbers must
### increment from left to right with respect to their relative
### positions in each case entry

cases_template = """
<caseRecDailyAsterix>
<VIS>* *, * <DETAIL>
</caseRecDailyAsterix>

<caseRecDaily>
<VIS>%%(and (diary-cyclic 1 <STMONTH> <STDAY> <STYEAR>) t)  <DETAIL>
</caseRecDaily>

<caseRecDailyException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-cyclic 1 <STMONTH> <STDAY> <STYEAR>)) <DETAIL>
</caseRecDailyException>


<caseRecDailyAsterisk>
<VIS>*/*/* <DETAIL>
</caseRecDailyAsterisk>

<caseRecDailyBlock>
<VIS>%%(diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>) <DETAIL>
</caseRecDailyBlock>


<caseRecDailyBlockException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)) <DETAIL>
</caseRecDailyBlockException>


<caseRecDailyInterval>
<VIS>%%(diary-cyclic <INTERVAL> <STMONTH> <STDAY> <STYEAR>) <DETAIL>
</caseRecDailyInterval>

<caseRecDailyIntervalException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-cyclic <INTERVAL> <STMONTH> <STDAY> <STYEAR>)) <DETAIL>
</caseRecDailyIntervalException>

<caseRecDailyIntervalBlock>
<VIS>%%(and (diary-cyclic <INTERVAL> <STMONTH> <STDAY> <STYEAR>)(diary-block <STMONTH2> <STDAY2> <STYEAR2> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)) <DETAIL>
</caseRecDailyIntervalBlock>

<caseRecDailyIntervalBlockException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-cyclic <INTERVAL> <STMONTH> <STDAY> <STYEAR>)(diary-block <STMONTH2> <STDAY2> <STYEAR2> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)) <DETAIL>
</caseRecDailyIntervalBlockException>

<caseRecWeeklyWeekname>
<VIS><DAYOFWEEK> <DETAIL>
</caseRecWeeklyWeekname>

<caseRecWeeklyAbbr>
<VIS><DAYOFWEEKABBR> <DETAIL>
</caseRecWeeklyAbbr>

<caseRecWeekly>
<VIS>%%(and (diary-cyclic 1 <STMONTH> <STDAY> <STYEAR>)(memq (calendar-day-of-week date) '(<BYDAY>))) <DETAIL>
</caseRecWeekly>

<caseRecWeeklyException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-cyclic 1 <STMONTH> <STDAY> <STYEAR>)(memq (calendar-day-of-week date) '(<BYDAY>))) <DETAIL>
</caseRecWeeklyException>

<caseRecWeeklyBlock>
<VIS>%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(memq (calendar-day-of-week date) '(<BYDAY>))) <DETAIL>
</caseRecWeeklyBlock>

<caseRecWeeklyBlockException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(memq (calendar-day-of-week date) '(<BYDAY>))) <DETAIL>
</caseRecWeeklyBlockException>


<caseRecWeeklyInterval>
<VIS>%%(let ((dayname (calendar-day-of-week date))(strtwkno (string-to-number (format-time-string <SOMEZING> (encode-time 1 1 1 <STDAY> <STMONTH> <STYEAR>))))(weekno (string-to-number (format-time-string <SOMEZING2> (encode-time 1 1 1 (car (cdr date))(car date)(car (nthcdr 2 date)))))))(and (= (mod (- weekno strtwkno) <INTERVAL>) 0)(memq dayname '(<BYDAY>)))) <DETAIL>
</caseRecWeeklyInterval>

<caseRecWeeklyIntervalException>
<VIS>%%(let ((dayname (calendar-day-of-week date))(strtwkno (string-to-number (format-time-string <SOMEZING> (encode-time 1 1 1 <STDAY> <STMONTH> <STYEAR>))))(weekno (string-to-number (format-time-string <SOMEZING2> (encode-time 1 1 1 (car (cdr date))(car date)(car (nthcdr 2 date)))))))(and (not (or (diary-date <EXCEPTIONSTRING>)))(= (mod (- weekno strtwkno) <INTERVAL>) 0)(memq dayname '(<BYDAY>)))) <DETAIL>
</caseRecWeeklyIntervalException>


<caseRecWeeklyIntervalBlock>
<VIS>%%(let ((dayname (calendar-day-of-week date))(strtwkno (string-to-number (format-time-string <SOMEZING> (encode-time 1 1 1 <STDAY> <STMONTH> <STYEAR>))))(weekno (string-to-number (format-time-string <SOMEZING2> (encode-time 1 1 1 (car (cdr date))(car date)(car (nthcdr 2 date)))))))(and (diary-block <STMONTH2> <STDAY2> <STYEAR2> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (mod (- weekno strtwkno) <INTERVAL>) 0)(memq dayname '(<BYDAY>)))) <DETAIL>
</caseRecWeeklyIntervalBlock>

<caseRecWeeklyIntervalBlockException>
<VIS>%%(let ((dayname (calendar-day-of-week date))(strtwkno (string-to-number (format-time-string <SOMEZING> (encode-time 1 1 1 <STDAY> <STMONTH> <STYEAR>))))(weekno (string-to-number (format-time-string <SOMEZING2> (encode-time 1 1 1 (car (cdr date))(car date)(car (nthcdr 2 date)))))))(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-block <STMONTH2> <STDAY2> <STYEAR2> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (mod (- weekno strtwkno) <INTERVAL>) 0)(memq dayname '(<BYDAY>)))) <DETAIL>
</caseRecWeeklyIntervalBlockException>


<caseRecMonthlyAsterisk>
<VIS>* <STDAY> <DETAIL>
</caseRecMonthlyAsterisk>

<caseRecMonthly>
<VIS>%%(and (diary-cyclic 1 <STMONTH> <STDAY> <STYEAR>)(diary-date t <STDAY2> t)) <DETAIL>
</caseRecMonthly>

<caseRecMonthlyBlock>
<VIS>%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (car (cdr date)) <STDAY2>)) <DETAIL>
</caseRecMonthlyBlock>

<caseRecMonthlyBlockException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (car (cdr date)) <STDAY2>)) <DETAIL>
</caseRecMonthlyBlockException>


<caseRecMonthlyInterval>
<VIS>%%(and (diary-date t <STDAY> t)(diary-cyclic 1 <STMONTH> <STDAY2> <STYEAR>)(= (mod (- (car date) <STMONTH2>) <INTERVAL>) 0)) <DETAIL>
</caseRecMonthlyInterval>

<caseRecMonthlyIntervalException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-date t <STDAY> t)(diary-cyclic 1 <STMONTH> <STDAY2> <STYEAR>)(= (mod (- (car date) <STMONTH2>) <INTERVAL>) 0)) <DETAIL>
</caseRecMonthlyIntervalException>


<caseRecMonthlyIntervalBlock>
<VIS>%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (car (cdr date)) <STDAY2>)(= (mod (- (car date) <STMONTH2>) <INTERVAL>) 0)) <DETAIL>
</caseRecMonthlyIntervalBlock>

<caseRecMonthlyIntervalBlockException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (car (cdr date)) <STDAY2>)(= (mod (- (car date) <STMONTH2>) <INTERVAL>) 0)) <DETAIL>
</caseRecMonthlyIntervalBlockException>


<caseRecMonthlybydayofweek>
<VIS>%%(and (diary-cyclic 1 <STMONTH> <STDAY> <STYEAR>)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweek>

<caseRecMonthlybydayofweekBlock>
<VIS>%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekBlock>


<caseRecMonthlybydayofweekException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-cyclic 1 <STMONTH> <STDAY> <STYEAR>)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekException>


<caseRecMonthlybydayofweekInterval>
<VIS>%%(and (= (mod (- (car date) <STMONTH>) <INTERVAL>) 0)(diary-cyclic 1 <STMONTH2> <STDAY> <STYEAR>)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekInterval>

<caseRecMonthlybydayofweekIntervalException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(= (mod (- (car date) <STMONTH>) <INTERVAL>) 0)(diary-cyclic 1 <STMONTH2> <STDAY> <STYEAR>)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekIntervalException>



<caseRecMonthlybydayofweekIntervalBlock>
<VIS>%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (mod (- (car date) <STMONTH2>) <INTERVAL>) 0)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekIntervalBlock>

<caseRecMonthlybydayofweekIntervalBlockException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (mod (- (car date) <STMONTH2>) <INTERVAL>) 0)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekIntervalBlockException>


<caseMonthdayyear>
<VIS><STMONTH>/<STDAY>/<STYEAR> <DETAIL>
</caseMonthdayyear>

<caseMonthABBRdayyear>
<VIS><MONTHABBR> <STDAY>, <STYEAR> <DETAIL>
</caseMonthABBRdayyear>

<caseMonthABBRdayyearwspace>
<VIS><MONTHABBR>  <STDAY>, <STYEAR> <DETAIL>
</caseMonthABBRdayyearwspace>


<caseRecYearly>
<VIS>%%(and (>= (car (cddr date)) <STYEAR>)(diary-anniversary <STMONTH> <STDAY> <STYEAR2>)) <DETAIL>
</caseRecYearly>

<caseRecYearlyException>
<VIS>%%(and (not (or (diary-date <EXCEPTIONSTRING>)))(>= (car (cddr date)) <STYEAR>)(diary-anniversary <STMONTH> <STDAY> <STYEAR2>)) <DETAIL>
</caseRecYearlyException>


<caseRecYearlyABBRB>
<VIS><MONTHABBR> <STDAYNOTFOLLOWEDBYCOMMA> <DETAIL>
</caseRecYearlyABBRB>

<caseRecYearlyModern>
<VIS><STMONTH>/<STDAY>/* <DETAIL>
</caseRecYearlyModern>

<caseRecYearlyInterval>
<VIS>%%(or (diary-date <STMONTH> <STDAY> <STYEAR>)(and (>= (car (cddr date)) <STYEAR2>)(diary-anniversary <STMONTH2> <STDAY2> <STYEAR3>)(= (mod (- (car (nthcdr 2 date)) <STYEAR4>) <INTERVAL>) 0))) <DETAIL>
</caseRecYearlyInterval>

<caseRecYearlyIntervalException>
<VIS>%%(or (diary-date <STMONTH> <STDAY> <STYEAR>)(and (not (or (diary-date <EXCEPTIONSTRING>)))(>= (car (cddr date)) <STYEAR2>)(diary-anniversary <STMONTH2> <STDAY2> <STYEAR3>)(= (mod (- (car (cddr date)) <STYEAR4>) <INTERVAL>) 0))) <DETAIL>
</caseRecYearlyIntervalException>


"""

# The compiled regular expression match templates for the case templates.
cases_template_mtch = _compile_match_templates({
        BYDAY_VAR: '([0-6 ]{1,13})',
        STDAY_VAR: DAY_RE,
        EXCEPTIONSTRING_VAR: '([\ddiary\-ent\(\)\' ]*?)',
        WHICHWEEK_VAR: '(-?[0-3])',
        STYEAR_VAR: '(\d?\d?\d\d)',
        STMONTH_VAR: MONTH_RE,
        STMONTHABBR_VAR: '([Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ept|[Oo]ct|[Nn]ov|[Dd]ec)',
        ENDDAY_VAR: DAY_RE,
        ENDMONTH_VAR: MONTH_RE,
        ENDYEAR_VAR: YEAR_RE,
        UNTILDAY_VAR: DAY_RE,
        UNTILMONTH_VAR: MONTH_RE,
        UNTILYEAR_VAR: YEAR_RE,
        STMONTH2_VAR: MONTH_RE,
        STDAY2_VAR: DAY_RE,
        STYEAR2_VAR: YEAR_RE,
        STYEAR3_VAR: YEAR_RE,
        STYEAR4_VAR: YEAR_RE,
        SOMEZING_VAR: SOMEZING_RE,
        SOMEZING2_VAR: SOMEZING_RE,
        INTERVAL_VAR: '(\d+)',
        NUMDAYOFWEEK_VAR: '([0-6])',
        DAYOFWEEK_VAR: '([Ss]unday|[Mm]onday|[Tt]uesday|[Ww]ednesday|[Tt]hursday|[Ff]riday|[Ss]aturday)',
        DAYOFWEEKABBR_VAR: '([Ss]un|[Mm]on|[Tt]ue|[Ww]ed|[Tt]hu|[Ff]ri|[Ss]at)',
        MONTHABBR_VAR: '([Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec)',
        STDAYNOTFOLLOWEDBYCOMMA_VAR: STDAYNOTFOLLOWEDBYCOMMA_RE,
        NOTFOLLOWEDBYCOMMA_VAR: '(?!,)',
        VIS_VAR: '(&?)',
        DETAIL_VAR: '(.*?)(?=^[\w%&\d*])',})

### detail_template describes the total number of ways that a given <DETAIL> field, from that of cases_template, can be formatted in the diary file.
### Fields are delimited by '<' and '>'; Uppercase fields refer to variable names, containing regexps which are found in detail_template_mtch.  Lowercase fields are place holders for literal strings as described in the detail_template_mtch template, and do not act as variables.
### Each formatting case is delimited in an XML like manner.  .\n must be double escaped e.g. \\n

detail_template = """

<detailsC_title_newline_space_timerange_content>
<TITLE>\\n\s<TIMERANGE> <CONTENT>
</detailsC_title_newline_space_timerange_content>

<detailsF_title_timerange_newline_content>
<TITLE> <TIMERANGE><newlinehere><CONTENT>
</detailsF_title_timerange_newline_content>

<detailsH_title_timerangeII_content>
<TITLE> <TIMERANGEIII> <CONTENT>
</detailsH_title_timerangeII_content>

<detailsI_title_timerangeIV_content>
<TITLE> <TIMERANGEIV> <CONTENT>
</detailsI_title_timerangeIV_content>

<detailsJ_timerangeII_title_newline_content>
<TIMERANGEII> <TITLE>\\n\s<CONTENT>
</detailsJ_timerangeII_title_newline_content>

<detailsK_timerangeII_title>
<TIMERANGEII> <TITLE>
</detailsK_timerangeII_title>



<detailsM_timerange_title_newline_content>
<TIMERANGE> <TITLE><newlinehere><CONTENT>
</detailsM_timerange_title_newline_content>

<detailsP_title_timerange_content>
<TITLE> <TIMERANGE> <CONTENT>
</detailsP_title_timerange_content>

<detailsS_timerange_title>
<TIMERANGE> <TITLE>
</detailsS_timerange_title>

<detailsQ_title_timerange>
<TITLE> <TIMERANGE>
</detailsQ_title_timerange>

<detailsU_title_newline_content>
<TITLE><newlinehere><CONTENT>
</detailsU_title_newline_content>

<detailsX_title>
<TITLE>
</detailsX_title>




"""

# The compiled regular expression match templates for detail templates.

detail_template_mtch = _compile_match_templates({
        TITLE_VAR: TITLE_RE,
        TITLEIV_VAR: TITLE_RE,
        TITLEII_VAR: '(\w[\w ]+(?=\\n[\s\\t]))',
        CONTENT_VAR: '(.+)',
        TIMERANGEIV_VAR: '(\d{1,2}(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))',
        TIMERANGEIII_VAR: '(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?:am|pm|AM|PM))',
        TIMERANGEII_VAR: '(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))',
        TIMERANGEJJ_VAR: '(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-?\s{0,8}(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))?)',
        TIMERANGE_VAR: '((\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-\s{0,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))|(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)))',
        NEWLINEHERE_VAR: '\\\n',})


# Mapping from the Emacs Diary formatting to Google Calendar
e2gcase_table = {
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
    CASE_REC_YEARLY_INTERVAL_EXCEPTION_VAR: CASE_REC_YEARLY_INTERVAL_VAR,}


### recurrence event descriptions
recurrence_event_descriptions_template = """
<caseMonthdayyear>
Single Day Event
</caseMonthdayyear>

<caseMonthABBRdayyear>
Single Day Event
</caseMonthABBRdayyear>

<caseMonthABBRdayyearwspace>
Single Day Event
</caseMonthABBRdayyearwspace>

<caseRecDailyAsterisk>
Recurs Daily
</caseRecDailyAsterisk>

<caseRecDailyAsterix>
Recurs Daily
</caseRecDailyAsterix>

<caseRecDailyAsterixException>
Recurs Daily
</caseRecDailyAsterixException>

<caseRecDaily>
Recurs Daily, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecDaily>

<caseRecDailyException>
Recurs Daily, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecDailyException>

<caseRecDailyBlock>
Recurs Daily, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecDailyBlock>

<caseRecDailyBlockException>
Recurs Daily, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecDailyBlockException>

<caseRecDailyInterval>
Recurs Every <INTERVAL> Days, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecDailyInterval>

<caseRecDailyIntervalException>
Recurs Every <INTERVAL> Days, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecDailyIntervalException>

<caseRecDailyIntervalBlock>
Recurs Every <INTERVAL> Days, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecDailyIntervalBlock>

<caseRecDailyIntervalBlockException>
Recurs Every <INTERVAL> Days, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecDailyIntervalBlockException>

<caseRecWeeklyWeekname>
Recurs Every Week
</caseRecWeeklyWeekname>

<caseRecWeeklyAbbr>
Recurs Every Week
</caseRecWeeklyAbbr>

<caseRecWeekly>
Recurs<ONWHATDAYS>Every Week, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecWeekly>

<caseRecWeeklyException>
Recurs<ONWHATDAYS>Every Week, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecWeeklyException>

<caseRecWeeklyBlock>
Recurs<ONWHATDAYS>Every Week, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecWeeklyBlock>

<caseRecWeeklyBlockException>
Recurs<ONWHATDAYS>Every Week, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecWeeklyBlockException>

<caseRecWeeklyInterval>
Recurs<ONWHATDAYS>Every <INTERVAL> Weeks, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecWeeklyInterval>

<caseRecWeeklyIntervalException>
Recurs<ONWHATDAYS>Every <INTERVAL> Weeks, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecWeeklyIntervalException>

<caseRecWeeklyIntervalBlock>
Recurs<ONWHATDAYS>Every <INTERVAL> Weeks, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecWeeklyIntervalBlock>

<caseRecWeeklyIntervalBlockException>
Recurs<ONWHATDAYS>Every <INTERVAL> Weeks, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecWeeklyIntervalBlockException>

<caseRecMonthly>
Recurs Monthly, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthly>

<caseRecMonthlyAsterisk>
Recurs Monthly
</caseRecMonthlyAsterisk

<caseRecMonthlyException>
Recurs Monthly, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthlyException>

<caseRecMonthlyBlock>
Recurs Monthly, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlyBlock>

<caseRecMonthlyBlockException>
Recurs Monthly, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlyBlockException>

<caseRecMonthlyInterval>
Recurs Every <INTERVAL> Months, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthlyInterval>

<caseRecMonthlyIntervalException>
Recurs Every <INTERVAL> Months, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthlyIntervalException>

<caseRecMonthlyIntervalBlock>
Recurs Every <INTERVAL> Months, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlyIntervalBlock>

<caseRecMonthlyIntervalBlockException>
Recurs Every <INTERVAL> Months, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlyIntervalBlockException>

<caseRecMonthlybydayofweek>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every Month, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthlybydayofweek>

<caseRecMonthlybydayofweekException>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every Month, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthlybydayofweekException>

<caseRecMonthlybydayofweekBlock>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every Month, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlybydayofweekBlock>

<caseRecMonthlybydayofweekBlockException>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every Month, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlybydayofweekBlockException>

<caseRecMonthlybydayofweekInterval>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every <INTERVALORDINAL> Month, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthlybydayofweekInterval>

<caseRecMonthlybydayofweekIntervalException>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every <INTERVALORDINAL> Month, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecMonthlybydayofweekIntervalException>

<caseRecMonthlybydayofweekIntervalBlock>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every <INTERVALORDINAL> Month, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlybydayofweekIntervalBlock>

<caseRecMonthlybydayofweekIntervalBlockException>
Recurs on the <WHICHWEEKORDINAL> <DAYOFWEEKD> of Every <INTERVALORDINAL> Month, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>, Until <UNTILMONTH>/<UNTILDAY>/<UNTILYEAR>
</caseRecMonthlybydayofweekIntervalBlockException>

<caseRecYearly>
Recurs Yearly, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecYearly>

<caseRecYearlyException>
Recurs Yearly, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecYearlyException>

<caseRecYearlyABBRB>
Recurs Yearly
</caseRecYearlyABBRB>

<caseRecYearlyModern>
Recurs Yearly
</caseRecYearlyModern>

<caseRecYearlyInterval>
Recurs Every <INTERVAL> Years, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecYearlyInterval>

<caseRecYearlyIntervalException>
Recurs Every <INTERVAL> Years, With Exceptions, Beginning <STMONTH>/<STDAY>/<STYEAR>
</caseRecYearlyIntervalException>
"""

# The match compiled regular expressions associated with the
# recurrence event description templates.
recurrence_event_descriptions_template_mtch = _compile_match_templates({
        STDAY_VAR: TWO_DIGIT_RE,
        DAYORDINAL_VAR: GENERAL_RE,
        DAYOFWEEKD_VAR: GENERAL_RE,
        WHICHWEEKD_VAR: GENERAL_RE,
        ONWHATDAYS_VAR: GENERAL_RE,
        INTERVALORDINAL_VAR: GENERAL_RE,
        INTERVAL_VAR: GENERAL_RE,
        STMONTH_VAR: ONE_OR_TWO_DIGIT_RE,
        STDAY_VAR: ONE_OR_TWO_DIGIT_RE,
        STYEAR_VAR: FOUR_DIGIT_YEAR_RE,
        UNTILMONTH_VAR: ONE_OR_TWO_DIGIT_RE,
        UNTILDAY_VAR: ONE_OR_TWO_DIGIT_RE,
        UNTILYEAR_VAR: FOUR_DIGIT_YEAR_RE,
        WHICHWEEKORDINAL_VAR: GENERAL_RE, })

### gcases_template describes the total number of ways that a recursion entry can be formatted in a google calendar feed.
### Fields are delimited by '<' and '>'; Uppercase fields refer to variable names, the regexps of which are contained in gcases_template_mtch.  Lowercase fields are place holders for literal strings as described in the gcases_template_mtch template, and do not act as variables.
### Each formatting case is delimited in an XML like manner.  .
gcases_template = """
<caseRecDaily>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=DAILY;WKST=SU<newline>
</caseRecDaily>

<caseRecDailyBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=DAILY;UNTIL=<UNTILDATETIME>;WKST=SU<newline>
</caseRecDailyBlock>

<caseRecDailyInterval>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=DAILY;INTERVAL=<INTERVAL>;WKST=SU<newline>
</caseRecDailyInterval>

<caseRecDailyIntervalBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=DAILY;INTERVAL=<INTERVAL>;UNTIL=<UNTILDATETIME>;WKST=SU<newline>
</caseRecDailyIntervalBlock>

<caseRecMonthly>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;WKST=SU;BYMONTHDAY=<STDAY><newline>
</caseRecMonthly>

<caseRecMonthlybydayofweek>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;BYDAY=<WHICHWEEKG>;WKST=SU<newline>
</caseRecMonthlybydayofweek>

<caseRecMonthlybydayofweekBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;BYDAY=<WHICHWEEKG>;UNTIL=<UNTILDATETIME>;WKST=SU<newline>
</caseRecMonthlybydayofweekBlock>

<caseRecMonthlybydayofweekInterval>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;INTERVAL=<INTERVAL>;BYDAY=<WHICHWEEKG>;WKST=SU<newline>
</caseRecMonthlybydayofweekInterval>

<caseRecMonthlybydayofweekIntervalBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;INTERVAL=<INTERVAL>;BYDAY=<WHICHWEEKG>;UNTIL=<UNTILDATETIME>;WKST=SU<newline>
</caseRecMonthlybydayofweekIntervalBlock>

<caseRecMonthlyInterval>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;INTERVAL=<INTERVAL>;WKST=SU;BYMONTHDAY=<STDAY><newline>
</caseRecMonthlyInterval>

<caseRecMonthlyBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;UNTIL=<UNTILDATETIME>;WKST=SU;BYMONTHDAY=<STDAY><newline>
</caseRecMonthlyBlock>

<caseRecMonthlyIntervalBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=MONTHLY;INTERVAL=<INTERVAL>;WKST=SU;BYMONTHDAY=<STDAY>;UNTIL=<UNTILDATETIME><newline>
</caseRecMonthlyIntervalBlock>

<caseRecWeekly>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=WEEKLY;BYDAY=<BYDAYG>;WKST=SU<newline>
</caseRecWeekly>

<caseRecWeeklyBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=<UNTILDATETIME>;BYDAY=<BYDAYG><newline>
</caseRecWeeklyBlock>

<caseRecWeeklyInterval>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=WEEKLY;INTERVAL=<INTERVAL>;BYDAY=<BYDAYG>;WKST=SU<newline>
</caseRecWeeklyInterval>

<caseRecWeeklyIntervalBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=WEEKLY;INTERVAL=<INTERVAL>;BYDAY=<BYDAYG>;UNTIL=<UNTILDATETIME>;WK
ST=SU<newline>
</caseRecWeeklyIntervalBlock>

<caseRecYearly>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=YEARLY;WKST=SU<newline>
</caseRecYearly>

<caseRecYearlyBlock>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=YEARLY;UNTIL=<UNTILDATETIME>;WKST=SU<newline>
</caseRecYearlyBlock>

<caseRecYearlyInterval>
DTSTART;TZID=<TZID>:<STDATETIME><newline>
DTEND;TZID=<TZID2>:<ENDDATETIME><newline>
RRULE:FREQ=YEARLY;INTERVAL=<INTERVAL>;WKST=SU<newline>
</caseRecYearlyInterval>
"""

# Compiled regular expression patterns associated with the gcases template.
gcases_template_mtch = _compile_match_templates({
        UNTILGTIME_VAR: '(T[0-2]\d{3}00Z?)?)',
        BYDAYG_VAR: '([1234,MOTUWEHFR]+)',
        NEWLINE_VAR: '\\n',
        STDATETIME_VAR: DATETIME_RE,
        ENDDATETIME_VAR: DATETIME_RE,
        UNTILDATETIME_VAR: '([12]0[012]\d(T[012][\d][0-5]\d[0-5]\dZ)?)',
        STDAY_VAR: DAY_RE,
        TZID_VAR: GENERAL_RE,
        TZID2_VAR: GENERAL_RE,
        INTERVAL_VAR: ONE_OR_TWO_DIGIT_RE,})

### times_template describes the total number of ways that a given <TIMERANGE> field, from that of details_template, can be formatted in the diary file.
### Fields are delimited by '<' and '>'; Uppercase fields refer to variable names, containing regexps which are found in times_template_mtch.  Lowercase fields are place holders for literal strings as described in the times_template_mtch template, and do not act as variables.
### Each formatting case is delimited in an XML like manner.  .
times_template = """
<caseTimeARange>
<STHOUR>:<STMINUTE><STAMPM><HYPHEN><ENDHOUR>:<ENDMINUTE><ENDAMPM>
</caseTimeARange>

<caseTimeBRangewithStarttimeMinutesOnly>
<STHOUR>:<STMINUTE><STAMPM><HYPHEN><ENDHOUR><ENDAMPM>
</caseTimeBRangewithStarttimeMinutesOnly>

<caseTimeCRangewithEndtimeMinutesOnly>
<STHOUR><STAMPM><HYPHEN><ENDHOUR>:<ENDMINUTE><ENDAMPM>
</caseTimeCRangewithEndtimeMinutesOnly>


<caseTimeDRangewithoutMinutes>
<STHOUR><STAMPM><HYPHEN><ENDHOUR><ENDAMPM>
</caseTimeDRangewithoutMinutes>

<caseTimeEStarttimeOnly>
<STHOUR>:<STMINUTE><STAMPM>
</caseTimeEStarttimeOnly>

<caseTimeFStarttimeOnlywithoutMinutes>
<STHOUR><STAMPM>
</caseTimeFStarttimeOnlywithoutMinutes>TITLE (\w[\w ]+)
"""

# The match templates associated with the times template
times_template_mtch = _compile_match_templates({
        TAB_VAR: '(?:\s+?)?',
        HYPHEN_VAR: '(\s{0,8}-\s{0,8})',
        STHOUR_VAR: HOUR_RE,
        STMINUTE_VAR: MINUTE_RE,
        STAMPM_VAR: AM_PM_RE,
        STAMPMHYPHEN_VAR: '(am|pm|AM|PM)[\s\\t]{0,8}-[\s\t]{0,8}',
        STAMPMNOHYPHEN_VAR: '(am|pm|AM|PM)(?![\s\\t]{0,8}-[\s\\t]{0,8})',
        ENDHOUR_VAR: HOUR_RE,
        ENDMINUTE_VAR: MINUTE_RE,
        ENDAMPM_VAR: AM_PM_RE,
        STDAYNOTFOLLOWEDBYCOMMA_VAR: STDAYNOTFOLLOWEDBYCOMMA_RE,})
