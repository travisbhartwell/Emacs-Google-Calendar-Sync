import re

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

### cases_template_mtch contains the regexp patterns associated with the cases_template.  All _mtch templates must end in a new line.
cases_template_mtch_raw = """BYDAY ([0-6 ]{1,13})
STDAY ([0-3]?\d)
EXCEPTIONSTRING ([\ddiary\-ent\(\)' ]*?)
WHICHWEEK (-?[0-3])
STYEAR (\d?\d?\d\d)
STMONTH ([01]?\d)
STMONTHABBR ([Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec)
ENDDAY ([0-3]?\d)
ENDMONTH ([01]?\d)
ENDYEAR (20[0-3]\d)
UNTILDAY ([0-3]?\d)
UNTILMONTH ([01]?\d)
UNTILYEAR (20[0-3]\d)
STMONTH2 ([01]?\d)
STDAY2 ([0-3]?\d)
STYEAR2 (20[0-3]\d)
STYEAR3 (20[0-3]\d)
STYEAR4 (20[0-3]\d)
SOMEZING (.{4})
SOMEZING2 (.{4})
INTERVAL (\d+)
NUMDAYOFWEEK ([0-6])
DAYOFWEEK ([Ss]unday|[Mm]onday|[Tt]uesday|[Ww]ednesday|[Tt]hursday|[Ff]riday|[Ss]aturday)
DAYOFWEEKABBR ([Ss]un|[Mm]on|[Tt]ue|[Ww]ed|[Tt]hu|[Ff]ri|[Ss]at)
MONTHABBR ([Jj]an|[Ff]eb|[Mm]ar|[Aa]pr|[Mm]ay|[Jj]un|[Jj]ul|[Aa]ug|[Ss]ep|[Oo]ct|[Nn]ov|[Dd]ec)
STDAYNOTFOLLOWEDBYCOMMA ([0-3]?\d)(?!,)
notfollowedbycomma (?!,)
VIS (&?)
DETAIL (.*?)(?=^[\w%&\d*])
"""

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

### detail_template_mtch contains the regexp patterns associated with detail_template.  All _mtch templates must end in a new line.  \n must be tripple escaped e.g. \\\n
detail_template_mtch_raw = """TITLE (\w[\w \?\.\(\)'"\[\]\-]+)
TITLEIV (\w[\w \?\.\(\)'"\[\]\-]+)
TITLEII (\w[\w ]+(?=\\n[\s\\t]))
CONTENT (.+)
TIMERANGEIV (\d{1,2}(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))
TIMERANGEIII (\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?:am|pm|AM|PM))
TIMERANGEII (\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{1,8}-\s{1,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))
TIMERANGEJJ (\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-?\s{0,8}(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))?)
TIMERANGE ((\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)\s{0,8}-\s{0,8}\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM))|(\d{1,2}(?::[0-5]\d)?(?:am|pm|AM|PM)))
newlinehere \\\n
"""

### e2gcase_table maps an emacs diary formatting case to its equivalent Google calendar case
e2gcase_table = """caseMonthdayyear:caseMDY
caseMonthABBRdayyear:caseMDY
caseMonthABBRdayyearwspace:caseMDY
caseRecDailyAsterisk:caseRecDaily
caseRecDailyAsterix:caseRecDaily
caseRecDailyAsterixException:caseRecDailyException
caseRecDaily:caseRecDaily
caseRecDailyException:caseRecDaily
caseRecDailyBlock:caseRecDailyBlock
caseRecDailyBlockException:caseRecDailyBlock
caseRecDailyInterval:caseRecDailyInterval
caseRecDailyIntervalException:caseRecDailyInterval
caseRecDailyIntervalBlock:caseRecDailyIntervalBlock
caseRecDailyIntervalBlockException:caseRecDailyIntervalBlock
caseRecWeeklyWeekname:caseRecWeekly
caseRecWeeklyAbbr:caseRecWeekly
caseRecWeekly:caseRecWeekly
caseRecWeeklyException:caseRecWeekly
caseRecWeeklyBlock:caseRecWeeklyBlock
caseRecWeeklyBlockException:caseRecWeeklyBlock
caseRecWeeklyInterval:caseRecWeeklyInterval
caseRecWeeklyIntervalException:caseRecWeeklyInterval
caseRecWeeklyIntervalBlock:caseRecWeeklyIntervalBlock
caseRecWeeklyIntervalBlockException:caseRecWeeklyIntervalBlock
caseRecMonthly:caseRecMonthly
caseRecMonthlyAsterisk:caseRecMonthly
caseRecMonthlyException:caseRecMonthly
caseRecMonthlyBlock:caseRecMonthlyBlock
caseRecMonthlyBlockException:caseRecMonthlyBlock
caseRecMonthlyInterval:caseRecMonthlyInterval
caseRecMonthlyIntervalException:caseRecMonthlyInterval
caseRecMonthlyIntervalBlock:caseRecMonthlyIntervalBlock
caseRecMonthlyIntervalBlockException:caseRecMonthlyIntervalBlock
caseRecMonthlybydayofweek:caseRecMonthlybydayofweek
caseRecMonthlybydayofweekException:caseRecMonthlybydayofweek
caseRecMonthlybydayofweekBlock:caseRecMonthlybydayofweekBlock
caseRecMonthlybydayofweekBlockException:caseRecMonthlybydayofweekBlock
caseRecMonthlybydayofweekInterval:caseRecMonthlybydayofweekInterval
caseRecMonthlybydayofweekIntervalException:caseRecMonthlybydayofweekInterval
caseRecMonthlybydayofweekIntervalBlock:caseRecMonthlybydayofweekIntervalBlock
caseRecMonthlybydayofweekIntervalBlockException:caseRecMonthlybydayofweekIntervalBlock
caseRecYearly:caseRecYearly
caseRecYearlyException:caseRecYearly
caseRecYearlyABBRB:caseRecYearly
caseRecYearlyModern:caseRecYearly
caseRecYearlyInterval:caseRecYearlyInterval
caseRecYearlyIntervalException:caseRecYearlyInterval
"""


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

recurrence_event_descriptions_template_mtch_raw = """STDAY (/d/d)
DAYORDINAL (.+?)
DAYOFWEEKD (.+?)
WHICHWEEKD (.+?)
ONWHATDAYS (.+?)
INTERVALORDINAL (.+?)
INTERVAL (.+?)
STMONTH (\d?\d)
STDAY (\d?\d)
STYEAR (\d\d\d\d)
UNTILMONTH (\d?\d)
UNTILDAY (\d?\d)
UNTILYEAR (\d{4})
WHICHWEEKORDINAL (.+?)
"""
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

### gcases_template_mtch contains the regexp patterns associated with gcases_template.  All _mtch templates must end in a new line.
gcases_template_mtch_raw = """UNTILGTIME (T[0-2]\d{3}00Z?)?)
BYDAYG ([1234,MOTUWEHFR]+)
newline \\n
STDATETIME ([12]0[012]\d(T[012][\d][0-5]\d[0-5]\d)?)
ENDDATETIME ([12]0[012]\d(T[012][\d][0-5]\d[0-5]\d)?)
UNTILDATETIME ([12]0[012]\d(T[012][\d][0-5]\d[0-5]\dZ)?)
STDAY ([0-3]?\d)
TZID (.+?)
TZID2 (.+?)
INTERVAL (\d?\d)
"""

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

### times_template_mtch contains the regexp patterns associated with times_template.  All _mtch templates must end in a new line.
times_template_mtch_raw = """TAB (?:\s+?)?
HYPHEN (\s{0,8}-\s{0,8})
STHOUR ([012]?\d)
STMINUTE ([0-5]\d)
STAMPM (am|pm|AM|PM)
STAMPMHYPHEN (am|pm|AM|PM)[\s\\t]{0,8}-[\s\t]{0,8}
STAMPMNOHYPHEN (am|pm|AM|PM)(?![\s\\t]{0,8}-[\s\\t]{0,8})
ENDHOUR ([012]?\d)
ENDMINUTE ([0-5]\d)
ENDAMPM (am|pm|AM|PM)
STDAYNOTFOLLOWEDBYCOMMA ([0-3]?\d)(?!,)
"""

# Template functions copied over from emacs-google-calendarsync.py
def strip_list(lst):
    return [s.strip() for s in lst]

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
        if key == '':
            break

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
    string_patterns = {}
    for i in at_template.keys():
        p0 = at_template[i] % dic_types
        string_patterns[i] = p0
        dic_evaluated_templates_array[i] = (re.compile(p0, re.M | re.S))
    return dic_evaluated_templates_array, string_patterns


# Process each of the tables and templates as needed for comparison in
# testing
# The Case Ref Table
e_to_g_case_table = load_ref_table('e2gcase_table')

# The template files
cases_template_raw = load_template('cases_template')
detail_template_raw = load_template('detail_template')
recurrence_event_descriptions_template_raw = load_template('recurrence_event_descriptions_template')
gcases_template_raw = load_template('gcases_template')
times_template_raw = load_template('times_template')

# Evaluate the templates
(cases_template,
 cases_template_strings) = evaluate_templates(cases_template_raw,
                                              'cases_template_mtch_raw')
(detail_template,
 detail_template_strings) = evaluate_templates(detail_template_raw,
                                               'detail_template_mtch_raw')
(recurrence_event_descriptions_template,
 recurrence_event_descriptions_template_strings) = \
 evaluate_templates(recurrence_event_descriptions_template_raw,
                    'recurrence_event_descriptions_template_mtch_raw')
(times_template,
 times_template_strings) = evaluate_templates(times_template_raw,
                                              'times_template_mtch_raw')

# Ensure the Match Vars are loaded
cases_template_mtch = load_match_vars('cases_template_mtch_raw')
detail_template_mtch = load_match_vars('detail_template_mtch_raw')
recurrence_event_descriptions_template_mtch = \
    load_match_vars('recurrence_event_descriptions_template_mtch_raw')
gcases_template_mtch = load_match_vars('gcases_template_mtch_raw')
times_template_mtch = load_match_vars('times_template_mtch_raw')
