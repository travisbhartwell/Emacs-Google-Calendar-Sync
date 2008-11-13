<caseRecDailyAsterix>
* *, * <DETAIL>
</caseRecDailyAsterix>

<caseRecDaily>
*/*/* <DETAIL>
</caseRecDaily>

<caseRecDailyBlock>
%%(diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>) <DETAIL>
</caseRecDailyBlock>

<caseRecDailyInterval>
%%(diary-cyclic <INTERVAL> <STMONTH> <STDAY> <STYEAR>) <DETAIL>
</caseRecDailyInterval>

<caseRecDailyIntervalBlock>
%%(and (diary-cyclic <INTERVAL> <STMONTH> <STDAY> <STYEAR>)(diary-block <STMONTH2> <STDAY2> <STYEAR2> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)) <DETAIL>
</caseRecDailyIntervalBlock>



<caseRecWeeklyWeekname>
<DAYOFWEEK> <DETAIL>
</caseRecWeeklyWeekname>

<caseRecWeeklyAbbr>
<DAYOFWEEKABBR> <DETAIL>
</caseRecWeeklyAbbr>

<caseRecWeekly>  
%%(memq (calendar-day-of-week date) '(<BYDAY>)) <DETAIL>
</caseRecWeekly>


<caseRecWeeklyBlock>  
%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(memq (calendar-day-of-week date) '(<BYDAY>))) <DETAIL>
</caseRecWeeklyBlock>

<caseRecWeeklyInterval>      
%%(let ((dayname (calendar-day-of-week date))(strtwkno (string-to-number (format-time-string <SOMEZING> (encode-time 1 1 1 <STDAY> <STMONTH> <STYEAR>))))(weekno (string-to-number (format-time-string <SOMEZING2> (encode-time 1 1 1 (car (cdr date)) (car date) (car (nthcdr 2 date)))))))(and (= (mod (- weekno strtwkno) <INTERVAL>) 0)(memq dayname '(<BYDAY>)))) <DETAIL>
</caseRecWeeklyInterval>

<caseRecWeeklyIntervalBlock>      
%%(let ((dayname (calendar-day-of-week date))(strtwkno (string-to-number (format-time-string <SOMEZING> (encode-time 1 1 1 <STDAY> <STMONTH> <STYEAR>))))(weekno (string-to-number (format-time-string <SOMEZING2> (encode-time 1 1 1 (car (cdr date)) (car date) (car (nthcdr 2 date)))))))(and (diary-block <STMONTH2> <STDAY2> <STYEAR2> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (mod (- weekno strtwkno) <INTERVAL>) 0)(memq dayname '(<BYDAY>)))) <DETAIL>
</caseRecWeeklyIntervalBlock>


<caseRecMonthly>
* <STDAY> <DETAIL>
</caseRecMonthly>

<caseRecMonthlyBlock>
%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (car (cdr date)) <STDAY2>)) <DETAIL>
</caseRecMonthlyBlock>

<caseRecMonthlyInterval> 
%%(and (= (car (cdr date)) <STDAY>)(= (mod (- (car date) <STMONTH>) <INTERVAL>) 0)) <DETAIL>
</caseRecMonthlyInterval>

<caseRecMonthlyIntervalBlock> 
%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (car (cdr date)) <STDAY2>)(= (mod (- (car date) <STMONTH2>) <INTERVAL>) 0)) <DETAIL>
</caseRecMonthlyIntervalBlock>


<caseRecMonthlybydayofweek>
%%(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>) <DETAIL>
</caseRecMonthlybydayofweek>

<caseRecMonthlybydayofweekInterval>
%%(and (= (mod (- (car date) <STMONTH> ) <INTERVAL>) 0)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekInterval>

<caseRecMonthlybydayofweekIntervalBlock>
%%(and (diary-block <STMONTH> <STDAY> <STYEAR> <UNTILMONTH> <UNTILDAY> <UNTILYEAR>)(= (mod (- (car date) <STMONTH2> ) <INTERVAL>) 0)(diary-float t <NUMDAYOFWEEK> <WHICHWEEK>)) <DETAIL>
</caseRecMonthlybydayofweekIntervalBlock>


<caseMonthdayyear>
<STMONTH>/<STDAY>/<STYEAR> <DETAIL>
</caseMonthdayyear>

<caseMonthABBRdayyear>
<STMONTHABBR> <STDAY>, <STYEAR> <DETAIL>
</caseMonthABBRdayyear>


<caseRecYearly>
%%(diary-anniversary <STMONTH> <STDAY> <STYEAR>) <DETAIL>
</caseRecYearly>


<caseRecYearlyABBRB>
<MONTHABBR> <STDAYNOTFOLLOWEDBYCOMMA> <DETAIL>
</caseRecYearlyABBRB>

<caseRecYearyModern>
<STMONTH>/<STDAY>/* <DETAIL>
</caseRecYearlyModern>

<caseRecYearlyInterval>
%%(and (diary-anniversary <STMONTH> <STDAY> <STYEAR>)(= (mod (- (car (nthcdr 2 date)) <STYEAR2>) <INTERVAL>) 0)) <DETAIL>
</caseRecYearlyInterval>

