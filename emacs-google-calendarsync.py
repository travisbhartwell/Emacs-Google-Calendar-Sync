#!/usr/bin/python
# emacs-google-calendarsync revision 77
# written and maintained by CiscoRx@gmail.com
# DISCLAIMER: If this script should fail or cause any damage then I,
# ciscorx@gmail.com, assume full liability; feel free to sue me for
# every penny I've got, the number of pennies of which should be just
# enough to fit into a small envelope to mail to you.  Hopefully, it
# will also cover postage.

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
DISCARD_ENTRIES_THAT_CONTAIN_THIS_CODE = '#@!z8#'

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

import gdata.calendar.service
import gdata.service
import atom.service
import gdata.calendar
import atom
import getopt
import sys
import time
import datetime
import re
import os
import shelve

if GMT_OFFSET is None:
    GMT_OFFSET = time.timezone / 60 / 60

# set this to 1 when daylight savings time is NOT in efffect, starting
# in spring.  Set to 0 when daylight savings time is in effect,
# starting in autumn
GMT_OFFSET -= 1

DictionaryDefinedType = type({})

# Templates (OKAY, messy, but will clean up later)
from templates import *

def stripallarray(aTarget):
    for i in range(len(aTarget)):
        aTarget[i] = aTarget[i].strip()
    return aTarget


def PadNewlinesWithSpace(source):
    """ This function is used on the CONTENT field so that when written to the emacs diary, multiple lines of description can be recognized as pertaining to a single respective event.  """
    lines = source.split('\n')
    if len(lines) < 2:
        return source
    alllinesbutfirst = lines[1:]
    target = []
    target.append(lines[0] + '\n')
    for line in alllinesbutfirst:
        if len(line) > 0 and line[0] != ' ':
            target.append(' ' + line + '\n')
    return ''.join(target)


def PadNewlinesWithNSpaces(source, N):
    """ This function is used on the CONTENT field so that when written to the emacs diary, multiple lines of description can be recognized as pertaining to a single respective event. N signifies margin space """
    lines = source.split('\n')
    if len(lines) < 2:
        return source
    alllinesbutfirst = lines[1:]
    target = []
    target.append(lines[0] + '\n')
    for line in alllinesbutfirst:
        if len(line) > 0 and line[0] != ' ':
            target.append(''.zfill(N).replace('0', ' ') + line + '\n')
    return ''.join(target)


def PadAllNewlinesWithSpace(source):
    """ This function is used on CONTENT of parsed gcal entry.  """
    lines = source.split('\n')
    target = []
    for line in lines:
        if len(line) > 0:
            target.append(' ' + line.strip() + '\n')
    return ''.join(target)


def RemoveNewlinesSpacePadding(source):
    """ This function is used on the CONTENT field of an entry being sent to gcal.  """
    lines = source.split('\n')
    if len(source) == 0:
        return ""
    target = []
    for line in lines:
        target.append(line.strip() + '\n')
    return ''.join(target).strip()


def removeallextraspaces(string):
    return re.sub("[ ]+", " ", string)


def StripExtraNewLines(string):
    """ Use this function on the 'fullentry' field before hashing to get a key, as sometimes new lines can get into the diary and mess up the hash"""
    pos = string.find('\n\n')
    while  pos != -1:
        string = string.replace('\n\n', '\n')
        pos = string.find('\n\n')
    if string == '\n':
        string = " "
    elif len(string) > 1 and string[-1] == '\n':
        string = string[0:-1]
    return string


def escstring(string):
    """ Use this function in place of re.escape(); re.escape() does not seem to work right...it is only used in the load_template() function"""
    str = []
    target = ''
    for i in range(len(string)):
        if string[i] in ['(', ')', '*', '+', '.', '?']:
            str.append('\\')
        elif string[i] == "%":
            str.append('%')
        elif string[i] == "&":
            str.append('&')
        str.append(string[i])

    target = ''.join(str)
    return target


def rmwhtspc(sTarget):
    sTarget.strip()
    rmwhtspcpat = re.compile(r'\s+')
    sTarget = rmwhtspcpat.sub(' ', sTarget)
    sTarget = sTarget.replace(') (', ')(')
    return sTarget


def loadMtchvars(filename):
    """The _mtch file must end in a new line.  Uppercase entries are recognized as variable names for pattern patching.  Lowercase entries are not recognized as matching variables, and their values are substituted in verbatim.  Any variable appearing more than once in an entry must have a 1 digit ordinal number appende to the variable name, incremented for each occurrence"""
    filecontent = globals()[locals()['filename']]

    ff = filecontent.splitlines(True)
    key = ''
    identicalkeys = []
    dicDatatypes = {}
    for line in ff:
        spcloc = line.find(" ")
        key = line[0:spcloc]
        if key.islower():
            # lowercase keys dont get var names
            dicDatatypes[key] = line[spcloc + 1:]
        # more than one occurance of a variable in a pattern must have
        # a 1 digit ordinal number appended to the variable name
        elif key[:-1] in identicalkeys:
            dicDatatypes[key] = line[spcloc + 1] + '?P=' + key[:-1] + ')'
        else:
            dicDatatypes[key] = line[spcloc + 1] + '?P<' + key[:] + '>' + line[spcloc + 2:len(line) - 1]
        identicalkeys.append(key)
    return dicDatatypes


def load_ref_table(table_name):
    """ loads simple one level dictionary from a global variable.  The
    format is a simple 'key:value' with newlines separating each pair,
    and a mandatory single newline at the end"""
    filecontent = globals()[locals()['table_name']]

    ff = filecontent.splitlines(True)
    key = ''
    value = ''
    dict = {}
    for line in ff:
        key, value = line.split(':')
        value = value.strip()
        dict[key] = value
    return dict


def load_template(filename, escape=True):
    """all whitespace thats longer than a single space is removed from template.  If whitespace is needed to be represented in a pattern, create a tag for it in lowercase letters, and create an entry in the _mtch file indicating what to sub in verbatim.
       note: the ^ may only be used in template to indicate beginning of string.  The EvaluateTemplate() function is used to create matching patterns using loadTemplates return value as an argument.
      """
    filecontent = globals()[locals()['filename']]

    test = filecontent.splitlines(True)
    test = stripallarray(test)
    casestring = ''.join(test)
    casepat = re.compile(r'<(.+?)>(.+?)</')
    tpCases = casepat.findall(casestring)
    pat = re.compile(r'\<(\w+?)\>')
    ttCase = {}
    aCasename = []
    casename = ''
    escapedstring = ''
    for i in range(len(tpCases)):
        casename = tpCases[i][0]
        if escape:
            escapedstring = escstring(tpCases[i][1])  #escape (,),%, &
        else:
            escapedstring = tpCases[i][1]
        tmpstr = re.sub(pat, r'%(\1)s', escapedstring)
        ttCase[casename] = tmpstr                      #make a template
        aCasename.append(casename)
    return ttCase


def EvaluateTemplates(atTemplate, matchvarfilename):
    """makes match pattern for future matches
       This function also calls loadMtchvars() """
    dicTypes = loadMtchvars(matchvarfilename)
    dicEvaluatedTemplatesArray = {}
    stringpatterns = []
    for i in atTemplate.keys():
        p0 = atTemplate[i] % dicTypes
        stringpatterns.append(p0)
        dicEvaluatedTemplatesArray[i] = (re.compile(p0, re.M | re.S))
    return dicEvaluatedTemplatesArray, stringpatterns


def deepupdate(db, dbTmp):
    """ overwrite any non dict type value and use {}.update on dict types """
    dict = {}
    dictype = type(dict)
    dbkeys = db.keys()
    dbTmpkeys = dbTmp.keys()
    for key in dbkeys:
        if key in dbTmpkeys:
            if type(db[key]) == dictype:
                db[key].update(dbTmp[key])
            else:
                db[key] = dbTmp[key]
    for key in dbTmpkeys:
        if key not in dbkeys:
            if type(db[key]) == dictype:
                db[key] = dbTmp[key].copy()
            else:
                db[key] = dbTmp[key]


def parseList2db(pat, lDiaryArray, keys):
    """ keys argument is a list of entrypids associated with each string in lDiaryArray, respectively.   pat is a dictionary of patterns to check against.  The pattern cases are matched against a diary string in alphabetical order of case name, accepting the first pattern that is matched"""
    dbTmp = {}
    entrypid = ''
    moCase2Diary = {}
    patkeys = pat.keys()
    patkeys.sort()
    for i in range(len(lDiaryArray)):
        for idxCase in patkeys:
            moCase2Diary = pat[idxCase].search(lDiaryArray[i])
            if moCase2Diary != None:             ## find only first match
                entry = {}
                entry = moCase2Diary.groupdict()
                entrypid = keys[i]
                entry['entrypid'] = entrypid
                casename = 'casename-' + idxCase
                entry[casename] = idxCase
                dbTmp[entrypid] = entry
                break
    return dbTmp


def getTimeRanges(db, keys):
    timeranges = []
    tmpRec = {}
    for key in keys:
        tmpRec = db[key]
        tmpReckeys = [key for key in tmpRec.keys() if key[:9] == 'TIMERANGE'] ### workaround for having created an extra variable called TIMERANGEII and TIMERANGEIII in detail_template
        if len(tmpReckeys) > 0:
            timeranges.append(tmpRec[tmpReckeys[0]])
        else:
            timeranges.append('')
    return timeranges


def updateDetails(db, details, keys):
    """  """
    tDetails = load_template('detail_template')
    patDetails, sd = EvaluateTemplates(tDetails, 'detail_template_mtch')
    dbTmp = parseList2db(patDetails, details, keys)
#  for entrykey in dbTmp.keys():                                            ### workaround for having created an extra variable called TIMERANGEII in detail_template
#    if 'TIMERANGEII' in dbTmp[entrykey].keys():
#      dbTmp[entrykey]['TIMERANGE'] = dbTmp[entrykey]['TIMERANGEII']
#    if 'TIMERANGEIII' in dbTmp[entrykey].keys():
#      dbTmp[entrykey]['TIMERANGE'] = dbTmp[entrykey]['TIMERANGEIII']

    deepupdate(db, dbTmp)
    timeranges = getTimeRanges(db, keys)

    atTimescases = load_template('times_template')
    patTimes, sp = EvaluateTemplates(atTimescases, 'times_template_mtch')
    dbTmp = parseList2db(patTimes, timeranges, keys)
    deepupdate(db, dbTmp)


def e2gbyday(byday):
    """  converts emacs calendar BYDAY field data to the google calendar BYDAY equivalent"""
    if byday == '':
        return ''
    bydayg = ''
    daysofweek = ['SU', 'MO', 'TU', 'WE', 'TH', 'FR', 'SA']
    lines = byday.split(' ')
    for line in lines:
        bydayg = bydayg + daysofweek[int(line)] + ', '
    bydayg = bydayg[:-1]
    return bydayg


def g2ebyday(byday):
    """  converts google calendar BYDAY field data to the emacs calendar BYDAY equivalent"""
    if byday == '':
        return ''
    bydaye = ''
    daysofweek = ['SU', 'MO', 'TU', 'WE', 'TH', 'FR', 'SA']
    lines = byday.split(', ')
    for line in lines:
        bydaye = bydaye + str(daysofweek.index(line.upper())) + ' '
    bydaye = bydaye[:-1]
    return bydaye


def e2gmonthabbr(monthabbr):
    months = {'Jan': '01', 'Feb': '02', 'Mar': '03', 'Apr': '04', 'May': '05',
              'Jun': '06', 'Jul': '07', 'Aug': '08', 'Sep': '09', 'Oct': '10',
              'Nov': '11', 'Dec': '12'}
    return months[monthabbr]


def g2emonthabbr(month):
    months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    return months[int(month)-1]


def g2ewhichweek(whichweekg):
    days = {'SU': '0', 'MO': '1', 'TU': '2', 'WE': '3', 'TH': '4', 'FR': '5', 'SA': '6'}
    if whichweekg[0] == '-':
        whichweek = whichweekg[0:2]
        whichday = whichweekg[2:4]
    else:
        whichweek = whichweekg[0]
        whichday = whichweekg[1:3]
    whichday = days[whichday.upper()]
    return whichweek, whichday


def striptime(timestring):
    timestring = timestring.replace(':', '')
    timestring = timestring.replace('-', '')
    if len(timestring) > 15:
        timestring = timestring[0:15]
    return timestring


def sCurrentDatetime():
    now = time.strptime(time.asctime())
    now = now[0:6]
    nowdatetime = datetime.datetime(*now)
    nowstr = nowdatetime.isoformat()
    nowstr = striptime(nowstr)
    return nowstr


def ISOtoORGtime(isotime):
    """ taking account for GMT """
    isodate = datetime.datetime(*time.strptime(isotime, '%Y-%m-%dT%H:%M:%S.000Z')[0:6]) - datetime.timedelta(hours=GMT_OFFSET)
    return isodate.strftime('[%Y-%m-%d %a %H:%M:%S]')

    #return datetime.datetime(*time.strptime(isotime, '%Y-%m-%dT%H:%M:%S.000Z')[0:5]).strftime('[%Y-%m-%d %H:%M:%S]')


def ISOtoTimestamp(isotime):
    return time.mktime(time.strptime(isotime, '%Y-%m-%dT%H:%M:%S.000Z')) - 3600 * GMT_OFFSET


def sdeltaDatetime(offsetdays):
    return datetime.datetime.now() + datetime.timedelta(offsetdays)


def HandleLooseEmacsEnds(db):
    """ this function should probably be rewritten """
    gcases_template = load_template('gcases_template', escape=False)
    now = time.strptime(time.asctime())
    nowyear = now[0]
    nowmonth = now[1]
    nowday = now[2]
    daysofweek = ['SU', 'MO', 'TU', 'WE', 'TH', 'FR', 'SA']
    for dbkey in db.keys():
        alldayevent = False
        reckeys = db[dbkey].keys()
        db[dbkey]['TZID'] = TZID
        db[dbkey]['TZID2'] = TZID
        db[dbkey]['newline'] = '\r\n'
        db[dbkey]['STMONTH'] = db[dbkey].setdefault('STMONTH', str(nowmonth)).zfill(2)
        db[dbkey]['STDAY'] = db[dbkey].setdefault('STDAY', str(nowday)).zfill(2)
        if 'BYDAY' in reckeys:
            db[dbkey]['BYDAYG'] = e2gbyday(db[dbkey]['BYDAY'])
        if 'WHICHWEEK' in reckeys and 'NUMDAYOFWEEK' in reckeys:
            whichweek = int(db[dbkey]['WHICHWEEK'])
            if whichweek == -1:
                whichweekg = '-1'
            else:
                whichweekg = db[dbkey]['WHICHWEEK'].strip()
            db[dbkey]['WHICHWEEKG'] = whichweekg + daysofweek[int(db[dbkey]['NUMDAYOFWEEK'].strip())]
        if 'DAYOFWEEK' in reckeys:
            db[dbkey]['BYDAYG'] = db[dbkey]['DAYOFWEEK'].upper()[:2]
        if 'DAYOFWEEKABBR' in reckeys:
            db[dbkey]['BYDAYG'] = db[dbkey]['DAYOFWEEKABBR'].upper()[:2]
        if 'MONTHABBR' in reckeys:
            db[dbkey]['STMONTH'] = e2gmonthabbr(db[dbkey]['MONTHABBR'])
        if 'STDAYNOTFOLLOWEDBYCOMMA' in reckeys:
            db[dbkey]['STDAY'] = db[dbkey]['STDAYNOTFOLLOWEDBYCOMMA']
            reckeys.append('STDAY')
        if 'STYEAR' not in reckeys:
            db[dbkey]['STYEAR'] = str(nowyear)
        elif len(db[dbkey]['STYEAR']) < 4:
            year = '2' + db[dbkey]['STYEAR'].zfill(3)
            db[dbkey]['STYEAR'] = year
        if 'STHOUR' not in reckeys:
            alldayevent = True
            db[dbkey]['STHOUR'] = '01'
            db[dbkey]['STMINUTE'] = '00'
        else:
            db[dbkey]['STHOUR'] = db[dbkey]['STHOUR'].zfill(2)
            if 'STMINUTE' not in reckeys:
                db[dbkey]['STMINUTE'] = '00'

        stday = int(db[dbkey]['STDAY'])
        stmonth = int(db[dbkey]['STMONTH'])
        styear = int(db[dbkey]['STYEAR'])
        sthour = int(db[dbkey]['STHOUR'])
        stminute = int(db[dbkey]['STMINUTE'])
        if 'STAMPM' not in reckeys:
            db[dbkey]['STAMPM'] = 'AM'
        if db[dbkey]['STAMPM'].upper()[0] == 'P' and sthour < 12:
            sthour += 12
        if db[dbkey]['STAMPM'].upper()[0] == 'A' and sthour == 12:
            sthour = 0
            db[dbkey]['STHOUR'] = '12'
        stdatetime = datetime.datetime(styear, stmonth, stday, sthour, stminute)
        stdatetimestr = stdatetime.isoformat()
        stdatetimestr = stdatetimestr.replace(':', '')
        stdatetimestr = stdatetimestr.replace('-', '')

        defaultenddatetime = stdatetime + datetime.timedelta(minutes=DEFAULT_EVENT_DURATION)
        defaultenddatetimetuple = defaultenddatetime.timetuple()
        if 'ENDYEAR' not in reckeys:
            db[dbkey]['ENDYEAR'] = str(defaultenddatetimetuple[0]).zfill(4)
        elif len(db[dbkey]['ENDYEAR']) < 4:
            year = '2' + db[dbkey]['ENDYEAR'].zfill(3)
            db[dbkey]['ENDYEAR'] = year
        db[dbkey]['ENDMONTH'] = db[dbkey].setdefault('ENDMONTH', str(defaultenddatetimetuple[1])).zfill(2)
        db[dbkey]['ENDDAY'] = db[dbkey].setdefault('ENDDAY', str(defaultenddatetimetuple[2])).zfill(2)
        if 'ENDHOUR' in reckeys and 'ENDMINUTE' not in reckeys:   ### this is a case like 3:30pm - 5pm, where no minutes are given in the endtime, because they are abbreviated from 00
            db[dbkey]['ENDMINUTE'] = '00'
        else:
            db[dbkey]['ENDMINUTE'] = db[dbkey].setdefault('ENDMINUTE', str(defaultenddatetimetuple[4])).zfill(2)
        db[dbkey]['ENDHOUR'] = db[dbkey].setdefault('ENDHOUR', str(defaultenddatetimetuple[3])).zfill(2)

        endhour = int(db[dbkey]['ENDHOUR'])
        endday = int(db[dbkey]['STDAY'])
        endmonth = int(db[dbkey]['STMONTH'])
        endyear = int(db[dbkey]['STYEAR'])
        endhour = int(db[dbkey]['ENDHOUR'])
        endminute = int(db[dbkey]['ENDMINUTE'])

        if 'ENDAMPM' not in reckeys:       ## assume default event duration, as provided from DEFAULT_EVENT_DURATION, if one is not given
            db[dbkey]['ENDAMPM'] = time.strftime('%p', defaultenddatetimetuple)
            db[dbkey]['ENDHOUR'] = time.strftime('%I', defaultenddatetimetuple)
        if db[dbkey]['ENDAMPM'].upper()[0] == 'P' and endhour < 12:
            endhour += 12
                                  ### check to see if end date spans into its tomorrows date
        if endhour < sthour:
            tomorrowsdate = stdatetime
            tomorrowsdate += datetime.timedelta(hours=23)
            db[dbkey]['ENDDAY'] = tomorrowsdate.strftime('%d').zfill(2)
            db[dbkey]['ENDMONTH'] = tomorrowsdate.strftime('%m').zfill(2)
            db[dbkey]['ENDYEAR'] = tomorrowsdate.strftime('%Y')
            endday = int(db[dbkey]['ENDDAY'])
            endmonth = int(db[dbkey]['ENDMONTH'])
            endyear = int(db[dbkey]['ENDYEAR'])


        enddatetime = datetime.datetime(endyear, endmonth, endday, endhour, endminute)
        enddatetimestr = enddatetime.isoformat()
        enddatetimestr = enddatetimestr.replace(':', '')
        enddatetimestr = enddatetimestr.replace('-', '')

        if alldayevent:
            db[dbkey]['alldayevent'] = True
            enddatetimestr = enddatetimestr[:8]
            stdatetimestr = stdatetimestr[:8]
            startdatetimetuple = datetime.datetime(styear, stmonth, stday).timetuple()
            enddatetimetuple = datetime.datetime(endyear, endmonth, endday).timetuple()
            db[dbkey]['timetuple_dtstart'] = startdatetimetuple
            db[dbkey]['timetuple_dtend'] = enddatetimetuple
        else:
            db[dbkey]['alldayevent'] = False
            db[dbkey]['timetuple_dtstart'] = stdatetime.timetuple()
            db[dbkey]['timetuple_dtend'] = enddatetime.timetuple()
        db[dbkey]['STDATETIME'] = stdatetimestr
        db[dbkey]['ENDDATETIME'] = enddatetimestr
        if 'UNTILYEAR' in reckeys:
            untilday = int(db[dbkey]['UNTILDAY'])
            untilmonth = int(db[dbkey]['UNTILMONTH'])
            untilyear = int(db[dbkey]['UNTILYEAR'])
            untildatetime = datetime.datetime(untilyear, untilmonth, untilday)
            untildatetimestr = untildatetime.isoformat()
            untildatetimestr = untildatetimestr.replace(':', '')
            untildatetimestr = untildatetimestr.replace('-', '')
            db[dbkey]['UNTILDATETIME'] = untildatetimestr[:8]

        gcase = db[dbkey]['gcase']
                                                      ## write recurrence string from gcases_template
        if len(gcase) > 7 and gcase[:7] == 'caseRec':
            recurrencestring = gcases_template[gcase] % db[dbkey]
            db[dbkey]['recurrencestring'] = recurrencestring
            db[dbkey]['caseRec'] = gcase


def printcontents(db):
    """ used for debugging purposes """
    for keys in db.keys():
        print db[keys].get('TITLE')
        print db[keys].get('CONTENT')
        print " "


def getpreviousline(filestrings, pos):
    current = pos - 2
    while current > 0:
        current -= 1
        if filestrings[current] == '\n':
            return filestrings[current + 1:pos - 1] + '\n\nend', current + 1
    return filestrings[:pos], 0


def ordinalIntervaltonum(ordint):

    if ordint is None or len(ordint) == 0:
        return ""
    ordint = ordint.lower()
    interval = "".join([char for i, char in zip(xrange(len(ordint)), ordint) if char.isdigit()])
    if interval != "":
        return interval
    weeksofmonth = {"last": "-1",
                    "second to last": "-2",
                    "third to last": "-3",
                    "fourth to last": "-4",
                    "first": "1",
                    "second": "2",
                    "third": "3",
                    "fourth": "4",
                    "fifth": "5",
                    "other": "2", }
    return weeksofmonth.setdefault('ordint', '')


def copyDescriptiontodbrecord(dbrecord, desc, caseTemplate):
    """ copy description changes to record, and flag modified if any changes were made """
    #return dbrecord  ### debug

    modified = False

    desckeys = desc.keys()
    if 'STDAY' in desckeys and 'STMONTH' in desckeys and 'STYEAR' in desckeys:
        stday = desc.get('STDAY')
        stmonth = desc.get('STMONTH')
        styear = desc.get('STYEAR')
        if stday != dbrecord['STDAY'] or stmonth != dbrecord['STMONTH'] or styear != dbrecord['STYEAR']:
            dbrecord['STDAY'] = stday.zfill(2)
            dbrecord['STMONTH'] = stmonth.zfill(2)
            dbrecord['STYEAR'] = styear.zfill(2)
            modified = True
    if 'UNTILDAY' in desckeys and 'UNTILMONTH' in desckeys and 'UNTILYEAR' in desckeys:
        untilday = desc.get('UNTILDAY')
        untilmonth = desc.get('UNTILMONTH')
        untilyear = desc.get('UNTILYEAR')
        if untilday != dbrecord['UNTILDAY'] or untilmonth != dbrecord['UNTILMONTH']  or untilyear != dbrecord['UNTILYEAR']:
            dbrecord['UNTILDAY'] = stday.zfill(2)
            dbrecord['UNTILMONTH'] = stmonth.zfill(2)
            dbrecord['UNTILYEAR'] = styear.zfill(2)
            modified = True
    if 'INTERVALORDINAL' in desckeys:
        interval = ordinalIntervaltonum(desc.get('INTERVALORDINAL'))
        if interval != "" and interval != dbrecord['INTERVAL']:
            dbrecord['INTERVAL'] = interval
            modified = True
    if 'ONWHATDAYS' in desckeys:
        onwhatdays = desc.get('ONWHATDAYS').lower().strip()
        if onwhatdays != "":
            daysofweek = {'su': '0',
                          'mo': '1',
                          'tu': '2',
                          'we': '3',
                          'th': '4',
                          'fr': '5',
                          'sa': '6', }
            bydayd = []
            onwhatdays = onwhatdays.replace(', ', '')
            onwhatdays = onwhatdays.replace(' and', '')
            for day in onwhatdays:
                bydayd.append(daysofweek.setdefault(day[:2], ""))
                bydayd = " ".join(bydayd)
                bydayd = removeallextraspaces(bydayd)
                byday = dbrecord.get('BYDAY')
                if byday != None and byday != bydayd:
                    dbrecord['BYDAY'] = bydayd
                    modified = True

    return dbrecord, modified


def strip_comments(fullentry):
    pos = fullentry.find(' * EGCSync')
    if pos == -1:
        return fullentry, None
    else:
        return fullentry[:pos-1], fullentry[pos:]


def parseCommentOwner(login, comments_text):
    """ the comments line looks like this (the content, published and updated fields are optional and only appear if there is a comment entry in the comment feed):
   * EGCSync Comments for: picnic at Some Joe's House
   ** Some Joe's comments
   *** status: INVITED
   *** email: someJoesemail@gmail.com
   *** name: Some Joe
   *** content: Yada yada yada why you coming to my house yada
   *** published: [2009-09-09]
   *** updated:   [2009-09-09]
    """
    marginlength = 5
    pos_email = comments_text.find('email: ' + login)
    if pos_email == -1:
        return '', ''
    statusline, pos_status = getpreviousline(comments_text, pos_email - marginlength)
    pos_status = statusline.find('status:')
    if pos_status != -1:
        status = statusline[pos_status + 7:pos_status + 9].strip().upper()[0:1]
    else:
        status = ''
    pos = comments_text.find('content:', pos_email)
    if pos != -1:
        pos += 8
        pos2 = comments_text.find('published:', pos)
        comment = comments_text[pos:pos2 - marginlength + 1]
        comment = RemoveNewlinesSpacePadding(comment)
    else:
        comment = ''

    return comment, status


def parsedates(file):
    dates = []
    entries = []
    entry_start = []
    date_end = []
    entry_end = []
    file = '\n' + file
    found_date = False
    found_date_end = False
    for i in xrange(1, len(file)):
        c = file[i]
        lastc = file[i-1]
        #print c
        if lastc == '\n':
            if found_date and not found_date_end:
                date_end.append(i)
                dates.append(file[entry_start[len(entry_start)-1]:i] + '\nend')  ## re.search will fail unless there is text after the newline, i.e. '\nend'
                found_date_end = True

            if c != ' ' and found_date:
                entry_end.append(i-1)
                entries.append(file[entry_start[len(entry_start)-1]:i] + '\nend')
                found_date = False
            if c != '\n' and c != ' ' and not found_date:
                entry_start.append(i-1)
                found_date = True
                found_date_end = False

    if len(entry_start) > len(entry_end):
        entry_start.pop()
    return dates, entries, entry_start, date_end, entry_end


def get_emacs_diary(login, emacsDiaryLocation, initialiseShelve, TimesARangeTemplate, printingCase, shelve):
    db = {}
    ap = load_template('cases_template')

    pat, sp = EvaluateTemplates(ap, 'cases_template_mtch')

    descTemplate = load_template('recurrence_event_descriptions_template')
    descpat, descarray = EvaluateTemplates(descTemplate, 'recurrence_event_descriptions_template_mtch')

    f = open(emacsDiaryLocation, "r")
    file = f.read()
    f.close()
    keys = []
    details = []
    deletefromfile = []
    diaryheader = ""
    lookforheaders = True
    file = file + "\nend"            ## need this or last entry wont be read
    while len(file) > 13 and lookforheaders:   ## preserve any header information in the diary
        if file[:13] == "&%%(org-diary" or file[:12] == "%%(org-diary":
            newlinepos = file.find('\n')
            diaryheader = diaryheader + file[:newlinepos] + '\n'
            file = file[newlinepos + 1:]
        else:
            lookforheaders = False
    if initialiseShelve:
        return db, diaryheader, ""                ## unrecognized_entries is "", the last return value

    e2gcase_table = load_ref_table('e2gcase_table')
    datefields, entries, entry_start, date_end, entry_end = parsedates(file)
    for idxDiary in xrange(len(datefields)):
        for idxCase in pat.keys():
            mo = pat[idxCase].search(datefields[idxDiary])
            if mo != None:
                entry_start_pos = entry_start[idxDiary]
                entry_end_pos = entry_end[idxDiary]
                mo = pat[idxCase].search(entries[idxDiary])
                entry = {}
                entry = mo.groupdict()
                fullentry = StripExtraNewLines(file[entry_start_pos:entry_end_pos])
                fullentry, comments_text = strip_comments(fullentry)
                entry['DETAIL'], comments_text = strip_comments(entry.get('DETAIL'))
                entry['fullentry'] = fullentry
                if comments_text != None:   # in this case a attendeeStatus without a comment is still considered a comment
                    entry['comments_text'] = comments_text
                    comment_owner_content, comment_owner_status = parseCommentOwner(login, comments_text)
                    entry['comment_owner_hash'] = hash(entry.get('comment_owner_content'))
                    entry['comment_owner_status'] = comment_owner_status

                if fullentry.find(DISCARD_ENTRIES_THAT_CONTAIN_THIS_CODE) == -1:  ## this is for a future feature
                    details.append(entry['DETAIL'])
                    entry['entrycase'] = idxCase
                    entry['gcase'] = e2gcase_table[idxCase]
#                    isModified = False
                    previousline, previouslinestartpos = getpreviousline(file, entry_start_pos)
                    previouslinemo = descpat[idxCase].search(previousline)
                    if previouslinemo != None:                           ## if description doesnt match then run it against all possible desc templates, if no match either then mark for  delete, else run both against shelve to see which one was edited and then update record.   IF description does match but items are changed, then find out which is correct by running it against the shelve, then update the record
                        entry_start_pos = previouslinestartpos

                    #entry, isModified = copyDescriptiontodbrecord(entry, desc_entry, printingCase[idxCase])
#                        if isModified:
#                            entry = update_full_entry_for_caseRec_record(dbrecord, caseTemplate, timeARangeTemplate)

                    entrypid = str(hash(fullentry))
                    keys.append(entrypid)
                    entry['entrypid'] = entrypid
                    db[entrypid] = entry
                deletefromfile.append([entry_start_pos, entry_end_pos])
                break                                                  ## find only first match for pat[idxCase] in datefields[idxDiary]
    newfile = []
    if len(deletefromfile) > 0:              ### preserve unrecognized entries and put them in newfile
        deletefromfile.sort()
        last_entrypos = 0
        for entrypos in deletefromfile:
            newfile.append(file[last_entrypos:entrypos[0]])
            last_entrypos = entrypos[1]
        newfile.append(file[last_entrypos:])
        newfile = ''.join(newfile)
    else:
        newfile = ''
    newfile = StripExtraNewLines(newfile)
    updateDetails(db, details, keys)
    HandleLooseEmacsEnds(db)

    # discard unrecognized entries if there are less than 6 chars worth of them
    if (len(newfile) > 5):
        print "-- UNRECOGNIZED ENTRIES:"
        unrecognized_diary_entries = newfile[:-3]
        print unrecognized_diary_entries
    else:
        unrecognized_diary_entries = ""

    return db, diaryheader, unrecognized_diary_entries


def recGetFieldTZID(recurrence):
    pos = recurrence.find('TZID')
    pos = recurrence.find('=', pos)
    posend = recurrence.find(':', pos)
    return recurrence[pos + 1:posend]


def recGetField(fieldname, recurrence):
    pos = recurrence.find(fieldname)
    pos = recurrence.find(':', pos)
    posend = recurrence.find('\n', pos)
    return recurrence[pos + 1:posend]


def recGetRRuleField(fieldname, rule):
    pos1 = rule.find(fieldname)
    pos2 = rule.find('=', pos1)
    posend = rule.find(';', pos2)
    if posend == -1:
        posend = len(rule)
    if pos1 > -1:
        return rule[pos2 + 1:posend]
    else:
        return ''


def blankforNoneType(string):
    if string is None:
        return ''
    else:
        return string


def Convertdtstart2timetuple(datestring):
    """ used in getGoggleCalendar() to convert dtstart from gcal to a time tuple """

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


def findExtendedProperty(properties, name):
    for property in properties:
        if property.name == name:
            return property.value
    return ""


def mapTimeBeforeTitles():
    aa = re.compile(r'<(details.+?)>(.+?)</', re.M | re.S)
    bb = aa.findall(detail_template)
    dicMap = {}
    for i in range(0, len(bb)):
        key = bb[i][0]
        case = 'casename-' + key
        detail = bb[i][1]
        posTime = detail.find('TIME')
        posTitle = detail.find('TITLE')
        if posTime < posTitle:
            dicMap[case] = '1'
        else:
            dicMap[case] = '0'
    return dicMap


def get_eventStatus(event):
    """ returns 2 values, first value if canceled, second value if orphaned """
    eventstr = str(event)
    eventid = event.id.text
    result = eventstr.find('eventStatus value="http://schemas.google.com/g/2005#event.canceled')
    if result == -1:
        if eventid[-4:] == '000Z' and eventid[-8] == 'T' and eventid[-17] == '_':   ### orphaned
            return "orphaned"
        else:
            return ""  ### neither orphan nor canceled
    else:
        return "canceled"    ### canceled


def convert_exceptions_to_dict(exceptions):
    """ part of  address_exceptions() """
    dicExceptions = {}
    exceptions.sort()
    for exception in exceptions:
        eventid = exception[:-17]
        datestr = exception[-16:]
        datemdy = datestr[4:6] + ' ' + datestr[6:8] + ' ' + datestr[:4]
        elementlist = dicExceptions.get(eventid)
        if elementlist is None:
            dicExceptions[eventid] = [datemdy]
        else:
            elementlist.append(datemdy)
            dicExceptions[eventid] = elementlist
    return dicExceptions


def update_full_entry_for_caseRec_record(record, TimeARangeString, CaseTemplateString):
    """ part of address_exceptions() """
    formatTimeBeforeTitle = record.get('formatTimeBeforeTitle')
    content = record.get('CONTENT')
    title = record.get('TITLE')
    if content != "":
        content = '\n' + content
    if record.get('alldayevent'):
        detail = title + content
    elif formatTimeBeforeTitle:
        detail = TimeARangeString % record + ' ' + title + content
    else:
        detail = title + ' ' + TimeARangeString % record + content
    record['DETAIL'] = detail
    recurrencestring = CaseTemplateString % record
    if recurrencestring[0] == '%':                                # this is a work around for printing the escaped % char
        recurrencestring = '%' + recurrencestring
    if recurrencestring[:2] == '&%':
        recurrencestring = '&%' + recurrencestring[1:]
    record['fullentry'] = StripExtraNewLines(recurrencestring)
    return record


def add_exceptions_to_record(dbgrecord, exceptions):
    """ part of address_exceptions """
    exceptionstring = ""

    dicConsolidatedExceptions = {}
    for exception in exceptions:                                                               ### Consolidate Days
        month = exception[:2]
        day = exception[3:5]
        year = exception[-4:]
        dayarray = dicConsolidatedExceptions.setdefault(month + year, [])
        dayarray.append(day)
        dicConsolidatedExceptions[month + year] = dayarray

    dicMonths = {}
    # Consolidate Months
    for i, monthyear in zip(xrange(len(dicConsolidatedExceptions.keys())), dicConsolidatedExceptions.keys()):
        dayarray = dicConsolidatedExceptions[monthyear]
        dayyear = " ".join(dayarray) + monthyear[-4:]
        montharray = dicMonths.setdefault(dayyear, [])
        montharray.append(monthyear[:2])
        dicMonths[dayyear] = montharray

    for dayyear in dicMonths.keys():
        # put all the dates of the same month into
        exceptionstring = exceptionstring + "(diary-date '(" + " ".join(dicMonths[dayyear]) + ") '(" + dayyear[:-4] + ") " + dayyear[-4:] + ")"
    # if there are more than 1 exceptions, then we need them separated by '(diary-date'
    exceptionstring = exceptionstring[12:-1]
    dbgrecord['EXCEPTIONSTRING'] = exceptionstring
    return dbgrecord


def address_exceptions(dbg, shelve_data, g2ekeymap, Exceptions, timeARangeString, CasesTemplate):
    """ adds exception strings to recurring events with exceptions.  Also, changes the recurrence case if need be """
                           ###  This function depends on the preservation of event ids for recurrence exception instance records, and that their eventStatus is marked deleted in lieu of actually being deleted.
    flagRecurrenceUpdates = []
    if len(Exceptions) == 0:
        return dbg, flagRecurrenceUpdates
    dicExceptions = convert_exceptions_to_dict(Exceptions)
    dbgkeys = [key for key in dbg.keys() if type(dbg[key]) == DictionaryDefinedType]
    for eventid in dicExceptions.keys():  ## these event ids are normal ones, not those that are found in exceptions
        if eventid in dbgkeys:
            dbgrecord = dbg[eventid]
            caseRecname = dbgrecord.get('caseRec')
            if caseRecname[-9:] != "Exception":
                caseRecname = caseRecname + "Exception"
                dbgrecord['caseRec'] = caseRecname
            dbgrecord = add_exceptions_to_record(dbgrecord, dicExceptions[eventid])
            dbgrecord = update_full_entry_for_caseRec_record(dbgrecord, timeARangeString, CasesTemplate[caseRecname])

            shelverecord = shelve_data.get(g2ekeymap.get(eventid))
            if shelverecord != None and dbgrecord['EXCEPTIONSTRING'] != shelverecord.get('EXCEPTIONSTRING'):
                flagRecurrenceUpdates = appendkey(flagRecurrenceUpdates, eventid)
            dbg[eventid] = dbgrecord.copy()
    return dbg, flagRecurrenceUpdates


def handle_exceptions(readFromGoogleOnly, ENTRY_CONTENTION, dbg, shelve_data, dbe, g2ekeymap, Orphaned, delfromG, addG, identicalkeys, ekeyschangedinG, gkeyschangedinG, editlinksmap):
    """ this function interactively prompts the user to determine if an altered orphan was intented to be edited or deleted.  if the -n option was invoked, assume deleting in lieu of editing """

    if len(Orphaned) == 0 or readFromGoogleOnly:
        return delfromG, addG, {}, [], dbe
#  if len(Canceled)> 0:
#    for cancel in Canceled:

    Deleteorphans = []
    dicUpdateorphans = {}

    modifiedorphans = [key for key in Orphaned if g2ekeymap.get(key) in delfromG]
    delfromG = [key for key in delfromG if shelve_data[key]['eventid'] not in modifiedorphans]  ## delfromG cannot contain orphaned event ids
                         ### check modifiedorphans against addG
    modifiedorphans = sortkeysbydate(dbg, modifiedorphans)
    addG = sortkeysbydate(dbe, addG)
    if len(addG) > 0 and ENTRY_CONTENTION != 2:
        print "!!!! INSTANCES OF RECURRENCES WERE MODIFIED !!!!"
    for instancenum, orphan in zip(xrange(0, len(modifiedorphans)), modifiedorphans):
        if len(addG) != 0:
            if ENTRY_CONTENTION != 2:
                print "Instance #", instancenum, ":", shelve_data[g2ekeymap[orphan]].get('fullentry')
                answervalidated = False

                while not answervalidated:
                    answer = raw_input("Were you intending on Updating or Deleting this gcal entry?  U for Update, D for Delete, S for show related recurrence event:")
                    answer = answer.upper()
                    if answer == 'D':
                        answervalidated = True
                    elif answer == 'U':
                        answervalidated = True
                    elif answer == 'S':
                        print "Recurrence event:", dbg[orphan[:-17]].get('fullentry')
            else:
                answer = 'D'
        else:
            answer = 'D'

        if answer == 'U':
            if len(addG) > 1:
                for i, key in zip(xrange(len(addG)), addG):
                    print "entry", i, ":", dbe[key].get('fullentry')
                answervalidated = False
                while not answervalidated:
                    answer = raw_input("Which of the above diary entries represents the update? S for show related recurrence event:")
                    if answer == "S" or answer == "s":
                        print "Recurrence event:", dbg[orphan[:-17]].get('fullentry')
                    elif len(answer) > 0 and answer[0] >= '0' and answer[0] <= '9':
                        answervalidated = True
                        answer = int(answer)
                        dicUpdateorphans[orphan] = addG[answer]
                        dbe[addG[answer]]['eventid'] = orphan                   ### associate the orphan to the diary entry via eventid
                        dbe[addG[answer]]['editlink'] = editlinksmap[orphan]
                        del addG[answer]
            elif len(addG) == 1:
                dicUpdateorphans[orphan] = addG[0]
                dbe[addG[0]]['eventid'] = orphan
                dbe[addG[0]]['editlink'] = editlinksmap[orphan]
                del addG[0]
        elif answer == 'D':
            Deleteorphans.append(orphan)


    return delfromG, addG, dicUpdateorphans, Deleteorphans, dbe


def ordinalsuffix(day):
    day = int(day)
    if 4 <= day <= 20 or 24 <= day <= 30:
        suffix = "th"
    else:
        suffix = ["st", "nd", "rd"][day % 10 - 1]
    return str(day) + suffix


def add_recurrence_descriptions(dbg, dbe):
    """ called from main() to add descriptions to dbe """
    daysofweek = ['Sundays', 'Mondays', 'Tuesdays', 'Wednesdays', 'Thursdays', 'Fridays', 'Saturdays']
    weeksofmonth = {"-1": "Last",
                    "-2": "Second to Last",
                    "-3": "Third to Last",
                    "-4": "Fourth to Last",
                    "0": "Second to Last",
                    "1": "First",
                    "2": "Second",
                    "3": "Third",
                    "4": "Fourth",
                    "5": "Fifth", }

    dbnames = ['dbg', 'dbe']
    db = dbg
    caseTemplate = load_template('recurrence_event_descriptions_template', escape=False)

    for dbname in dbnames:
        db = locals()[dbname]

        dbkeys = [key for key in db.keys() if type(db[key]) == DictionaryDefinedType and 'caseRec' in db[key].keys()]  ## get only recurrence records
        for key in dbkeys:
            record = db.get(key)
            caseRec = record.get('caseRec')

            if caseRec.find('bydayofweek') != -1:
                record['WHICHWEEKORDINAL'] = weeksofmonth[record.get('WHICHWEEK')]
                record['DAYORDINAL'] = ordinalsuffix(record['NUMDAYOFWEEK'])
                record['DAYOFWEEKD'] = daysofweek[int(record.get('NUMDAYOFWEEK'))]
            if caseRec.find('Weekly') != -1:
                bydayarray = record.get('BYDAY')
                if bydayarray is None:
                    bydayarray = ''
                if  bydayarray != "":
                    bydayarray = [daysofweek[int(kz)] for kz in bydayarray.split(" ")]
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
                    record['ONWHATDAYS'] = ' '  ## this handles case for weekly without byday
            if caseRec.find('Interval') != -1:
                interval = record['INTERVAL']
                intervalordinal = ordinalsuffix(interval)
                if intervalordinal == "2nd":
                    intervalordinal = "Other"
                record['INTERVALORDINAL'] = intervalordinal
            record['recurrencedesc'] = "\n" + caseTemplate[caseRec] % record + "\n"
            db[key] = record.copy()

    return dbg, dbe


def get_commenthref(commentobj):
    if commentobj is None:
        return ""
    commenthref = str(commentobj)
    start = commenthref.find('href')
    start += 6
    end = commenthref.find('/>', start)
    end -= 2
    return commenthref[start:end]


def get_attendeeStatus(an_event_who):
    attendeeStatus = {}
    attendeeName = {}
    for idxWho in xrange(len(an_event_who)):
        an_event_who_entry = an_event_who[idxWho]
        attendeeName[an_event_who[idxWho].email] = an_event_who[idxWho].name
        if str(an_event_who_entry).find('attendeeStatus') != -1:
            attendeeStatus[an_event_who[idxWho].email] = an_event_who[idxWho].attendee_status.value[0:1]
        else:
            attendeeStatus[an_event_who[idxWho].email] = ''
    return attendeeStatus, attendeeName


def get_google_calendar(username, passwd, time_min, casetimeARangeString, ap):
    Canceled = []
    Orphaned = []
    recurrences = []
    recurrencekeys = []
    months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    db = {}
    gcal = gdata.calendar.service.CalendarService()
    gcal.email = username
    gcal.password = passwd
    gcal.source = 'Google-Emacs-Calendar-Sync-1.0'
    try:
        gcal.ProgrammaticLogin()
    except Exception, err:
        if err[0] == 'Incorrect username or password':
            print err
            # no shelve object passed in and shelve.close() is not a function
            # shelve.close()
            sys.exit(1)
        print 'connection error'
        errorstatus = err[0].get('status')
        errorbody = err[0].get('body')
        if errorstatus == 302:           ## 302= redirect
            print errorbody, 'redirect to:', errorRedirectURI(errorbody)

        # no shelve object passed in and shelve.close() is not a function
        # shelve.close()
        sys.exit(1)

    query = gdata.calendar.service.CalendarEventQuery('default', 'private', 'full')
    #commentquery = gdata.calendar.service.CalendarEventCommentQuery()

    #query.start_min = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time_min)
    #start_min = sdeltaDatetime(-DELETE_OLD_ENTRIES_OFFSET)
    #query.start_min= start_min.strftime('%Y-%m-%dT%H:%M:%S.00Z')
    query.start_max = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime(time.time() + 28800000))
    #query.updated_min = start_date
    query.ctz = TZID
    query.max_results = 400
    feed = gcal.CalendarQuery(query)
    feedupdatedtext = feed.updated.text
    feedupdatedtext = feedupdatedtext[:-5]
    db['updated-g'] = time.strptime(feedupdatedtext, '%Y-%m-%dT%H:%M:%S')
    for i, an_event in zip(xrange(len(feed.entry)), feed.entry):
        entrypid = an_event.id.text
        eventStatus = get_eventStatus(an_event)  ### It would be nice if eventStatus was actually a visible property but its not:P (oops need to use event_status property, but thats ok since we must also identify orphaned entries)
        if eventStatus == "canceled":                                      # if event is part of a recurring event but was deleted as an instance the recurring event, then discard it
            Canceled.append(entrypid)
            continue
        elif eventStatus == "orphaned":                                      # if event is part of a recurring event, but was edited as an instance of that event, process it as normal
            Orphaned.append(entrypid)

        entry = {}

        #commentobj = an_event.comments
        #commenthref = get_commenthref(commentobj)
        #commentquery = gdata.calendar.service.CalendarEventCommentQuery(commentobj)
        #commentquery = gdata.calendar.service.CalendarQuery(commentobj)
        #commentquery.start_max = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime(time.time() + 28800000))
        #commentquery.ctz = TZID
        #commentquery.max_results = 400
        #print commentobj
        #pdb.set_trace() #debug
        comment_emails = []
        commentsarray = []
        if DISPLAY_COMMENTS:
            attendeeStatus, attendeeName = get_attendeeStatus(an_event.who)
            if an_event.comments != None:
#      commentfeed = gcal.CalendarQuery(commentquery)
            #newcomment = gdata.calendar.CalendarEventCommentEntry()
                commentobj = an_event.comments
                commenthref = get_commenthref(commentobj)
                entry['calendarEventComment_href'] = commenthref
                commentfeed = gcal.Query(commenthref)   ## cant get the CalendarEventCommentQuery to work so using this instead
                commententry = commentfeed.entry
                comment_title = commentfeed.title.text
                for comment in commententry:
                    comment_entry = {}
                    comment_entry['comment_entry'] = comment
                    comment_entry['author'] = comment.author[0]
                    comment_entry['email'] = comment.author[0].email.text
                    comment_emails.append(comment_entry.get('email'))
                    comment_entry['status'] = attendeeStatus.get(comment_entry['email'])
                    comment_entry['name'] = comment.author[0].name.text
                    comment_entry['content'] = RemoveNewlinesSpacePadding(comment.content.text)
                    comment_entry['published'] = comment.published.text
                    comment_entry['updated'] = comment.updated.text
                    comment_entry['id'] = comment.id.text
                    if len(comment.link) > 1:
                        comment_entry['editlink'] = comment.link[1]
                    commentsarray.append(comment_entry)
                    email_id = comment_entry.get('email')
                    email_id = email_id.split('@')[0]
                    if email_id == username:
                        entry['comment_owner_editlink'] = comment_entry.get('editlink')
                        entry['comment_owner_entry'] = comment
                        entry['comment_owner_status'] = comment_entry.get('status')
                        entry['comment_owner_content'] = comment_entry.get('content')
                        entry['comment_owner_hash'] = hash(comment_entry.get('content'))
                        entry['comment_owner_updated'] = ISOtoTimestamp(comment_entry.get('updated'))
                entry['comment_title'] = comment_title
                entry['comment_entries'] = commentsarray
                                                                                                  ### append attendeeStatus for those who did not leave comments to the commentsarray
            attendees_without_comments = [key for key in attendeeStatus.keys() if key not in comment_emails]
            if len(attendees_without_comments) > 0:
                for email in attendees_without_comments:
                    comment_entry = {}
                    comment_entry['name'] = attendeeName.get(email)
                    comment_entry['email'] = email
                    comment_status = attendeeStatus.get(email)
                    if comment_status != '':
                        comment_entry['status'] = comment_status
                        email_id = email.split('@')[0]
                        if email_id == username:
                            entry['comment_owner_status'] = comment_status
                        commentsarray.append(comment_entry)
                if len(commentsarray) > 0:
                    entry.setdefault('comment_title', 'Attendees for: ' + blankforNoneType(an_event.title.text))
                    entry['comment_entries'] = commentsarray
        entry['idxfeed'] = i
        entry['HYPHEN'] = ' - '
        entry['eventid'] = entrypid
        entry['where'] = blankforNoneType(an_event.where[0].text)
        entry['TITLE'] = blankforNoneType(an_event.title.text)
        content = blankforNoneType(an_event.content.text)
        content = StripExtraNewLines(content)
        content = RemoveNewlinesSpacePadding(content)  #debug
        content = PadAllNewlinesWithSpace(content)
        entry['CONTENT'] = content                     #content is an empty string if its blank, else each line is padded on the left with a space

        entry['modified'] = time.strptime(an_event.updated.text[:19], '%Y-%m-%dT%H:%M:%S')
        editlink = an_event.GetEditLink()
        if editlink != "":
            entry['editlink'] = editlink.href
###                                                                                     ### get extended_properties here
        formatTimeBeforeTitle = findExtendedProperty(an_event.extended_property, 'formatTimeBeforeTitle') #  time before title?
        if formatTimeBeforeTitle == "":
            formatTimeBeforeTitle = FORMAT_TIME_BEFORE_TITLE_IN_DIARY
        elif formatTimeBeforeTitle == "1":
            formatTimeBeforeTitle = True
        else:
            formatTimeBeforeTitle = False
        entry['formatTimeBeforeTitle'] = formatTimeBeforeTitle
        entry['VIS'] = findExtendedProperty(an_event.extended_property, 'VIS')                             # entry visibility inhibiter?
        nonrecurringformat = findExtendedProperty(an_event.extended_property, 'nonrecurringformat')        # non recurring format : mm/dd/yyyy or Jul 4, 2009 style?
        if nonrecurringformat == "":
            nonrecurringformat = DEFAULT_NON_RECURRING_FORMAT
        else:
            nonrecurringformat = int(nonrecurringformat)
        entry['nonrecurringformat'] = nonrecurringformat
        if an_event.recurrence != None:
            #entry['recurrence_raw']= an_event.recurrence.text      ##the gcal recurrence info is never used and takes up alot of space
            recurrences.append(an_event.recurrence.text)
            recurrencekeys.append(entrypid)
        else:                                                     #parse non-recurring entries
            stdatetime = striptime(an_event.when[0].start_time)
            enddatetime = striptime(an_event.when[0].end_time)
            entry['STYEAR'] = stdatetime[0:4]
            entry['STMONTH'] = stdatetime[4:6]
            entry['STDAY'] = stdatetime[6:8]
            entry['ENDYEAR'] = enddatetime[0:4]
            entry['ENDMONTH'] = enddatetime[4:6]
            entry['ENDDAY'] = enddatetime[6:8]
            entry['timetuple_dtstart'] = Convertdtstart2timetuple(stdatetime)
            content = entry['CONTENT']

            if len(stdatetime) > 8:
                if int(stdatetime[9:11]) >= 12:
                    entry['STAMPM'] = 'pm'
                    entry['STHOUR'] = str(int(stdatetime[9:11]) - 12)
                    if entry['STHOUR'] == '0':
                        entry['STHOUR'] = '12'
                else:
                    entry['STAMPM'] = 'am'
                    entry['STHOUR'] = str(int(stdatetime[9:11]))
                    if entry['STHOUR'] == '0':
                        entry['STHOUR'] = '12'
                entry['STMINUTE'] = stdatetime[11:13]

                if int(enddatetime[9:11]) >= 12:
                    entry['ENDAMPM'] = 'pm'
                    entry['ENDHOUR'] = str(int(enddatetime[9:11]) - 12)
                    if entry['ENDHOUR'] == '0':
                        entry['ENDHOUR'] = '12'
                else:
                    entry['ENDAMPM'] = 'am'
                    entry['ENDHOUR'] = str(int(enddatetime[9:11]))
                    if entry['ENDHOUR'] == '0':
                        entry['ENDHOUR'] = '12'
                entry['ENDMINUTE'] = enddatetime[11:13]
                if content != "":
                    content = '\n' + content
                if formatTimeBeforeTitle:
                    entry['DETAIL'] = casetimeARangeString % entry + ' ' + entry['TITLE'] + content
                else:
                    entry['DETAIL'] = entry['TITLE'] + " " + casetimeARangeString % entry + content
            else:                                      #### all day event
                entry['alldayevent'] = True
                if content != "":
                    content = '\n' + content
                entry['DETAIL'] = entry['TITLE'] + content


            if nonrecurringformat == 0:
                stday = str(int(entry['STDAY']))
                if len(stday) == 1:
                    spacevar = "  "
                else:
                    spacevar = " "
                entry['fullentry'] = StripExtraNewLines(entry['VIS'] + months[int(entry['STMONTH'])-1] + spacevar + stday + ', ' + entry['STYEAR'] + ' ' + entry['DETAIL'])
            else:
                entry['fullentry'] = StripExtraNewLines(entry['VIS'] + entry['STMONTH'] + '/' + entry['STDAY'] + '/' + entry['STYEAR'] + ' ' + entry['DETAIL'])
        db[entrypid] = entry

                                                     #### now parse recurrences
    for i, recurrence in enumerate(recurrences):
        casefrequency = ''
        casegeneral = ''
        caseinterval = ''
        caseblock = ''
        bydayg = ''
        dtstart = recGetField('DTSTART', recurrence)
        db[recurrencekeys[i]]['SOMEZING'] = '"%V"'
        db[recurrencekeys[i]]['SOMEZING2'] = '"%V"'
        db[recurrencekeys[i]]['dtstart'] = dtstart
        db[recurrencekeys[i]]['STYEAR'] = dtstart[0:4]
        db[recurrencekeys[i]]['STMONTH'] = dtstart[4:6]
        db[recurrencekeys[i]]['STDAY'] = dtstart[6:8]
        db[recurrencekeys[i]]['STYEAR2'] = dtstart[0:4]      ### note: every time you add a number postfixed variable name to the cases_template you must make an entry for it here
        db[recurrencekeys[i]]['STMONTH2'] = dtstart[4:6]
        db[recurrencekeys[i]]['STDAY2'] = dtstart[6:8]
        db[recurrencekeys[i]]['STYEAR3'] = dtstart[0:4]
        db[recurrencekeys[i]]['STYEAR4'] = dtstart[0:4]
        dtend = recGetField('DTEND', recurrence)
        db[recurrencekeys[i]]['dtend'] = dtend
        db[recurrencekeys[i]]['ENDYEAR'] = dtend[0:4]
        db[recurrencekeys[i]]['ENDMONTH'] = dtend[4:6]
        db[recurrencekeys[i]]['ENDDAY'] = dtend[6:8]
        tzid = recGetFieldTZID(recurrence)
        db[recurrencekeys[i]]['TZID'] = tzid
        db[recurrencekeys[i]]['TZID2'] = tzid
        rrule = recGetField('RRULE', recurrence)
        db[recurrencekeys[i]]['rrule'] = rrule
        casefrequency = recGetRRuleField('FREQ', rrule)
        casefrequency = casefrequency.lower().capitalize()
        db[recurrencekeys[i]]['freq'] = casefrequency
        untildate = recGetRRuleField('UNTIL', rrule)
        db[recurrencekeys[i]]['until'] = untildate
        if untildate != '':
            caseblock = 'Block'
            db[recurrencekeys[i]]['UNTILYEAR'] = untildate[0:4]
            db[recurrencekeys[i]]['UNTILMONTH'] = untildate[4:6]
            db[recurrencekeys[i]]['UNTILDAY'] = untildate[6:8]
        db[recurrencekeys[i]]['wkst'] = recGetRRuleField('WKST', rrule)
        interval = recGetRRuleField('INTERVAL', rrule)
        if interval != '' and int(interval) > 0:
            db[recurrencekeys[i]]['INTERVAL'] = interval
            caseinterval = 'Interval'
        bydayg = recGetRRuleField('BYDAY', rrule)
        db[recurrencekeys[i]]['BYDAYG'] = bydayg
        if len(bydayg) > 0 and (bydayg[0].isdigit() or bydayg[0] == '-'):
            casegeneral = 'bydayofweek'
            whichweek, whichday = g2ewhichweek(bydayg)
            db[recurrencekeys[i]]['WHICHWEEK'] = whichweek
            db[recurrencekeys[i]]['NUMDAYOFWEEK'] = whichday
        else:
            db[recurrencekeys[i]]['BYDAY'] = g2ebyday(bydayg)
        casename = 'caseRec' + casefrequency + casegeneral + caseinterval + caseblock
        db[recurrencekeys[i]]['caseRec'] = casename
        content = db[recurrencekeys[i]]['CONTENT']
        if len(dtstart) > 8:
            if int(dtstart[9:11]) >= 12:
                db[recurrencekeys[i]]['STAMPM'] = 'pm'
                db[recurrencekeys[i]]['STHOUR'] = str(int(dtstart[9:11]) - 12)
                if db[recurrencekeys[i]]['STHOUR'] == '0':
                    db[recurrencekeys[i]]['STHOUR'] = '12'
            else:
                db[recurrencekeys[i]]['STAMPM'] = 'am'
                db[recurrencekeys[i]]['STHOUR'] = str(int(dtstart[9:11]))
                if db[recurrencekeys[i]]['STHOUR'] == '0':
                    db[recurrencekeys[i]]['STHOUR'] = '12'
            db[recurrencekeys[i]]['STMINUTE'] = dtstart[11:13]
            if int(dtend[9:11]) >= 12:
                db[recurrencekeys[i]]['ENDAMPM'] = 'pm'
                db[recurrencekeys[i]]['ENDHOUR'] = str(int(dtend[9:11]) - 12)
                if db[recurrencekeys[i]]['ENDHOUR'] == '0':
                    db[recurrencekeys[i]]['ENDHOUR'] = '12'
            else:
                db[recurrencekeys[i]]['ENDAMPM'] = 'am'
                db[recurrencekeys[i]]['ENDHOUR'] = str(int(dtend[9:11]))
                if db[recurrencekeys[i]]['ENDHOUR'] == '0':
                    db[recurrencekeys[i]]['ENDHOUR'] = '12'
            db[recurrencekeys[i]]['ENDMINUTE'] = dtend[11:13]
            if db[recurrencekeys[i]]['formatTimeBeforeTitle']:
                if content != "":
                    content = '\n' + content
                db[recurrencekeys[i]]['DETAIL'] = casetimeARangeString % db[recurrencekeys[i]] + ' ' + db[recurrencekeys[i]]['TITLE'] + content
            else:
                db[recurrencekeys[i]]['DETAIL'] = db[recurrencekeys[i]]['TITLE'] + ' ' + casetimeARangeString % db[recurrencekeys[i]] + db[recurrencekeys[i]]['CONTENT']
        else:             ### all day event
            db[recurrencekeys[i]]['alldayevent'] = True
            if content != "":
                content = '\n' + content
            db[recurrencekeys[i]]['DETAIL'] = db[recurrencekeys[i]]['TITLE'] + content
        recurrencestring = ap[casename] % db[recurrencekeys[i]]

        if recurrencestring[0] == '%':                                # this is a work around for printing the escaped % char
            recurrencestring = '%' + recurrencestring
        if recurrencestring[:2] == '&%':
            recurrencestring = '&%' + recurrencestring[1:]

        db[recurrencekeys[i]]['fullentry'] = StripExtraNewLines(recurrencestring)
        db[recurrencekeys[i]]['timetuple_dtstart'] = Convertdtstart2timetuple(dtstart)

    #sys.exit(0) #debug
    return db, gcal, Canceled, Orphaned, feed


def get_keys_to_modify_from_e(db1, db2):
    """ Returns some arrays of keys that are to be inserted into or deleted from Gcal.  Any edited entries are deleted and reinserted """
    dict = {}
    dictype = type(dict)
    keys1 = [key for key in db1.keys() if type(db1[key]) == dictype]
    keys2 = [key for key in db2.keys() if type(db2[key]) == dictype]

    identicalkeys = [key for key in keys1 if key in keys2]          # identicalkeys are hashkeys that are the same in both the shelve and dbe, meaning the entries are unchanged

    delfromG = [key for key in keys2 if key not in identicalkeys]
    addtoG = [key for key in keys1 if key not in identicalkeys]
    return identicalkeys, delfromG, addtoG


def getKeystomodifyfromGREDACTED(dbg, dbshelf, identicalkeys, glastsynctime):
    """ Returns some arrays of keys that are to be inserted into or deleted from the emacs Diary.  Any edited entries are deleted and reinserted """
    ### REDACTED
                                                                    # identicalkeys are hashkeys that exist in both the shelve and dbe
    dict = {}
    dictype = type(dict)
    gkeys = [key for key in dbg.keys() if type(dbg[key]) == dictype]
    skeys = [key for key in dbshelf.keys() if type(dbshelf[key]) == dictype]
    skeyeventids = [dbshelf[key].get('eventid') for key in skeys]
    delfromE = [key for key in identicalkeys if dbshelf[key].get('eventid') not in gkeys]
    addE = [key for key in identicalkeys if key not in delfromE and dbg[dbshelf[key]['eventid']].get('modified') > glastsynctime]
    #addE = [key for key in identicalkeys if key not in delfromE]


    addEinTermsofGkeys = [dbshelf[key]['eventid'] for key in identicalkeys if key not in delfromE and dbg[dbshelf[key]['eventid']].get('modified') > glastsynctime]
    #addEinTermsofGkeys = [dbshelf[key]['eventid'] for key in identicalkeys if key not in delfromE]

    delfromE += addE                    #instead of editing simply delete and create a new entry
    alsoaddtheseGkeystoE = [key for key in gkeys if key not in skeyeventids]
    return delfromE, addE, addEinTermsofGkeys, alsoaddtheseGkeystoE


def get_keys_to_modify_from_g(dbg, delfromEalso, shelve_data, identicalkeys, glastsynctime):
    """ Returns some arrays of keys that are to be inserted into or deleted from the emacs Diary.  Any edited entries are deleted and reinserted """
                                                                    # identicalkeys are hashkeys that are the same in both the shelve and dbe
    dict = {}
    dictype = type(dict)
    gkeys = [key for key in dbg.keys() if type(dbg[key]) == dictype]
    skeys = [key for key in shelve_data.keys() if type(shelve_data[key]) == dictype]
    skeyeventids = [shelve_data[key].get('eventid') for key in skeys]
    delfromE = [key for key in identicalkeys if shelve_data[key].get('eventid') not in gkeys]

    addE = [key for key in identicalkeys if key not in delfromE and dbg[shelve_data[key]['eventid']].get('modified') > glastsynctime]
    #addE = [key for key in identicalkeys if key not in delfromE]    ## debug.


    addEinTermsofGkeys = [shelve_data[key]['eventid'] for key in identicalkeys if key not in delfromE and dbg[shelve_data[key]['eventid']].get('modified') > glastsynctime]
    #addEinTermsofGkeys = [dbshelf[key]['eventid'] for key in identicalkeys if key not in delfromE]

    #delfromE += addE                    #instead of editing simply delete and create a new entry
    alsoaddtheseNewlyAddedGkeystoE = [key for key in gkeys if key not in skeyeventids]  # these are newly entered from gcal


    delfromE = append_to_keys(delfromE, delfromEalso)
    return delfromE, addE, addEinTermsofGkeys, alsoaddtheseNewlyAddedGkeystoE


def get_shelve_and_last_sync_times(emacsDiaryLocation, gmailuser, initialiseShelve):
    lastmodifiedg = time.strptime('1995-1-1T12:00:00', '%Y-%m-%dT%H:%M:%S')
    lastmodifiede = time.gmtime(os.stat(emacsDiaryLocation).st_mtime)
    shelvepath = SHELVE_FILE
    postfix = str(abs(hash(gmailuser)))
    shelvenamefq = shelvepath + 'egcsyncshelve' + postfix + '.dat'
    f = shelve.open(shelvenamefq)
    if f == {} or initialiseShelve:
        for key in f.keys():
            del f[key]
        f['updated-e'] = time.gmtime()
        f['updated-g'] = time.strptime("1970-10-10T10:30:30.000Z", '%Y-%m-%dT%H:%M:%S.000Z')
    else:
        lastmodifiedg = f['updated-g']
        lastmodifiede = f['updated-e']
    return f, lastmodifiedg, lastmodifiede


def convertTimetuple2GMT(tt):
    a = datetime.datetime(tt[0], tt[1], tt[2], tt[3], tt[4])
    a += datetime.timedelta(hours=GMT_OFFSET)
    return a.timetuple()


def update_orphans_in_gcal(dicUpdateorphans, dbe, shelve_data, gcal, editlinksmap, g2ekeymap, feed):
    ## all non-recurring events must be entered in terms of GMT
    dicFindTimeBeforeTitle = mapTimeBeforeTitles()

    for orphan in dicUpdateorphans.keys():
        for event in feed.entry:
            if event.id.text == orphan:
                break

        entry = dbe[dicUpdateorphans[orphan]]
        event.title = atom.Title(text=entry.get('TITLE'))
        event.title.text = entry.get('TITLE')
        event.content = atom.Content(text=entry.get('CONTENT'))
        content = entry.get('CONTENT')
        if content != None:
            event.content.text = RemoveNewlinesSpacePadding(content)
    #    event.where.append(gdata.calendar.Where(value_string=event['WHERE']))             ### WHERE feature not yet supported
        if 'recurrencestring' in entry:
            event.recurrence = gdata.calendar.Recurrence(text=entry['recurrencestring'])
        else:
            event.recurrence = None
            timetuple_dtstart = entry['timetuple_dtstart']
            timetuple_dtstart = convertTimetuple2GMT(timetuple_dtstart)
            timetuple_dtend = entry['timetuple_dtend']
            timetuple_dtend = convertTimetuple2GMT(timetuple_dtend)
            if entry['alldayevent']:
                start_time = time.strftime('%Y-%m-%d', timetuple_dtstart)
                end_time = time.strftime('%Y-%m-%d', timetuple_dtend)
            else:
                start_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', timetuple_dtstart)
                end_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', timetuple_dtend)

        event.when.append(gdata.calendar.When(start_time=start_time, end_time=end_time))


###                                                                                   ### set extended_property additions here

        entrycase = entry.get('entrycase')
        if 'caseMonthdayyear' == entrycase:                                                        # non recurring format: mm/dd/yyyy or Jul 4, 2009 format?
            extendeddefaultformat = gdata.calendar.ExtendedProperty(name="nonrecurringformat", value="1")
            event.extended_property.append(extendeddefaultformat)
        elif 'caseMonthABBRdayyear' == entrycase or 'caseMonthABBRdayyearwspace' == entrycase:     # Jul  4, 2009 format?
            extendeddefaultformat = gdata.calendar.ExtendedProperty(name="nonrecurringformat", value="0")
            event.extended_property.append(extendeddefaultformat)
    #  timebeforetitle = load_ref_table("timeBeforeTitleMap")
            casenames = [key for key in entry.keys() if key[:16] == 'casename-details']                # Time before title ?
        if len(casenames) > 0:
            casename = casenames[0]
        if dicFindTimeBeforeTitle[casename] == "1":
            extendedcase = gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle", value="1")
            event.extended_property.append(extendedcase)
        else:
            extendedcase = gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle", value="0")
            event.extended_property.append(extendedcase)

        diaryentryvisibility = entry.get('VIS')
        if diaryentryvisibility == '&':                                                           # entry visibility inhibiter?
            extended = gdata.calendar.ExtendedProperty(name="VIS", value="&")
            event.extended_property.append(extended)

        print "-- Updated recurring event instance in Gcal to:", dbe[dicUpdateorphans[orphan]].get('fullentry')
        new_event = gcal.UpdateEvent(event.GetEditLink().href, event)
        dbe[dicUpdateorphans[orphan]]['editlink'] = new_event.GetEditLink().href
        del shelve_data[g2ekeymap[orphan]]


    return


def InsertEntryIntoGcal(entry, gcal, dicFindTimeBeforeTitle):
    ## all non-recurring events must be entered in terms of GMT

    event = gdata.calendar.CalendarEventEntry()
    event.title = atom.Title(text=entry.get('TITLE'))
    event.title.text = entry.get('TITLE')
    event.content = atom.Content(text=entry.get('CONTENT'))
    content = entry.get('CONTENT')
    if content != None:
        event.content.text = RemoveNewlinesSpacePadding(content)
#    event.where.append(gdata.calendar.Where(value_string=event['WHERE']))             ### WHERE feature not yet supported
    if 'recurrencestring' in entry:
        event.recurrence = gdata.calendar.Recurrence(text=entry['recurrencestring'])
    else:
        event.recurrence = None
        timetuple_dtstart = entry['timetuple_dtstart']
        timetuple_dtstart = convertTimetuple2GMT(timetuple_dtstart)
        timetuple_dtend = entry['timetuple_dtend']
        timetuple_dtend = convertTimetuple2GMT(timetuple_dtend)
        if entry['alldayevent']:
            start_time = time.strftime('%Y-%m-%d', timetuple_dtstart)
            end_time = time.strftime('%Y-%m-%d', timetuple_dtend)
        else:
            start_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', timetuple_dtstart)
            end_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', timetuple_dtend)
        event.when.append(gdata.calendar.When(start_time=start_time, end_time=end_time))

###                                                                                   ### set extended_property additions here

    entrycase = entry.get('entrycase')
    if 'caseMonthdayyear' == entrycase:                                                        # non recurring format: mm/dd/yyyy or Jul 4, 2009 format?
        extendeddefaultformat = gdata.calendar.ExtendedProperty(name="nonrecurringformat", value="1")
        event.extended_property.append(extendeddefaultformat)
    elif 'caseMonthABBRdayyear' == entrycase or 'caseMonthABBRdayyearwspace' == entrycase:     # Jul  4, 2009 format?
        extendeddefaultformat = gdata.calendar.ExtendedProperty(name="nonrecurringformat", value="0")
        event.extended_property.append(extendeddefaultformat)
#  timebeforetitle = load_ref_table("timeBeforeTitleMap")
    casenames = [key for key in entry.keys() if key[:16] == 'casename-details']                # Time before title ?
    if len(casenames) > 0:
        casename = casenames[0]
        if dicFindTimeBeforeTitle[casename] == "1":
            extendedcase = gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle", value="1")
            event.extended_property.append(extendedcase)
        else:
            extendedcase = gdata.calendar.ExtendedProperty(name="formatTimeBeforeTitle", value="0")
            event.extended_property.append(extendedcase)

    diaryentryvisibility = entry.get('VIS')
    if diaryentryvisibility == '&':                                                           # diary entry visibility inhibiter?
        extended = gdata.calendar.ExtendedProperty(name="VIS", value="&")
        event.extended_property.append(extended)

    new_event = gcal.InsertEvent(event, '/calendar/feeds/default/private/full')
    return new_event.id.text, new_event.GetEditLink().href


def insert_entries_into_gcal(addG, dbe, gcal, shelve_data):
    dicFindTimeBeforeTitle = mapTimeBeforeTitles()
    for key in addG:
        eventid, editlink = InsertEntryIntoGcal(dbe[key], gcal, dicFindTimeBeforeTitle)
        dbe[key]['eventid'] = eventid
        dbe[key]['editlink'] = editlink
        shelve_data[key] = dbe[key].copy()
        print "-- inserted from Diary to Gcal: " + shelve_data[key]['fullentry']
        print


def delete_entries_from_e(shelve_data, delfromE):
    for key in delfromE:
        record = shelve_data.get(key)
        if record != None:
            print "-- deleted from Diary: " + record.get('fullentry')
            del shelve_data[key]


def errorRedirectURI(body):
    """ 404='Not Found'       302='Redirect received, but redirects_remaining <= 0'
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


def delete_entries_from_gcal(delG, delfromdbg, dbg, gcal, shelve_data, editlinksmap, g2ekeymap, DeleteOrphans):

    for key in delG:
        record = shelve_data.get(key)
        if record != None:
            eventid = record.get('eventid')
            if eventid != None:
                editlink = editlinksmap.get(eventid)
                if editlink != None:
                    try:
                        gcal.DeleteEvent(editlink)
                        print "-- deleted from Gcal and Diary: " + shelve_data[key]['fullentry']
                    except Exception, err:             ## 302=Redirect received, but redirects_remaining <= 0
                        errorstatus = err[0].get('status')
                        errorbody = err[0].get('body')
                        print "-- unable to delete " + shelve_data[key]['fullentry']
                        if errorstatus != 404:           ## 404 being Not Found, so if its not Not Found, then assume its redirect
                            print errorbody, 'redirect to:', errorRedirectURI(errorbody)
                            shelve_data.close()
                            sys.exit(1)

                    del shelve_data[key]

    for key in DeleteOrphans:
        editlink = editlinksmap.get(key)
        try:
            gcal.DeleteEvent(editlink)
            print "-- deleted recurring event instance from Gcal and Diary:" + dbg[key].get('fullentry')
        except Exception, err:
            errorstatus = err[0].get('status')
            errorbody = err[0].get('body')
            print "-- unable to delete from gcal (probably already deleted): " + dbg[key].get('fullentry')
            if errorstatus != 404:           ## 404 being Not Found, so if its not Not Found, then assume its redirect
                print errorbody, 'redirect to:', errorRedirectURI(errorbody)
                shelve_data.close()
                sys.exit(1)

        del shelve_data[g2ekeymap[key]]


def insert_entries_edited_by_diary_to_e(addE, dbe, shelve_data):
    for key in addE:
        shelve_data[key] = dbe[key].copy()
        print "-- insert edit into Diary: " + shelve_data[key]['fullentry']


def insert_entries_into_e(addGkeystoE, shelve_data, dbg):
    for gkey in addGkeystoE:
        entrypid = str(hash(dbg[gkey]['fullentry']))
        dbg['entrypid'] = entrypid
        shelve_data[entrypid] = dbg[gkey].copy()
        print "-- inserted to Diary: " + dbg[gkey]['fullentry']


def createIndexFromShelve(db):
    """ function called from write_emacs_diary() used to sort the diary entries for the emacs calendar
          it returns a 2xn matrix of timestamps associated with entry starting dates and hash keys, primary keys of the shelve"""
    dict = {}
    dictype = type(dict)
    dbkeys = db.keys()
    index = []
    for key in dbkeys:
        if type(db[key]) == dictype:
            # start date converted to timestamp for indexing
            dttimestamp = time.mktime(db[key]['timetuple_dtstart'])
            row = []
            row.append(dttimestamp)
            row.append(key)
            index.append(row)
    index.sort()
    return index


def sortkeysbydate(db, keys):
    """ function called from handlecontentions() used to sort the diary entries for the emacs calendar
          it returns a 2xn matrix of timestamps associated with entry starting dates and hash keys, primary keys of the shelve"""
    dict = {}
    dictype = type(dict)
    index = []
    for key in keys:
        if type(db[key]) == dictype:
            # start date converted to timestamp for indexing
            dttimestamp = time.mktime(db[key]['timetuple_dtstart'])
            row = []
            row.append(dttimestamp)
            row.append(key)
            index.append(row)
    index.sort()

    target = []
    size = len(index)
    for i in range(0, size):
        target.append(index[i][1])
    return target


def write_emacs_diary(emacsDiaryLocation, shelve_data, diaryheader, unrecognized_diary_entries):
    index = createIndexFromShelve(shelve_data)
    f = open(emacsDiaryLocation, 'w')
    f.seek(0)
    if diaryheader != "":
        f.write(diaryheader + '\n')
    comment_status_enum = {'A': 'ACCEPTED', 'D': 'DECLINED', 'I': 'INVITED', 'T': 'TENTATIVE'}
    for row in index:   ### row[1] contains the ekeys and row[0] contains the order index
        f.write(shelve_data[row[1]].setdefault('recurrencedesc', '') + shelve_data[row[1]].get('fullentry') + '\n')

        if 'comment_entries' in shelve_data[row[1]] and len(shelve_data[row[1]].get('comment_entries')) > 0 and shelve_data[row[1]]['comment_entries'][0].get('status') != None and shelve_data[row[1]]['comment_entries'][0].get('status') != '':
            f.write(' * EGCSync ' + shelve_data[row[1]].get('comment_title') + '\n')
            for commententry in shelve_data[row[1]].get('comment_entries'):
                f.write(' ** ' + commententry.get('name') + "'s comments\n")
                #f.write('  :PROPERTIES:\n')
                comment_status = commententry.get('status')
                f.write(' *** status: ' + comment_status_enum.setdefault(comment_status, '') + '\n')
                f.write(' *** email: ' + commententry.get('email') + '\n')
                f.write(' *** name: ' + commententry.get('name') + '\n')
                if 'published' in commententry:
                    f.write(' *** content: ' + PadNewlinesWithNSpaces(commententry.get('content') + '\n', 4))
                    f.write(' *** published: ' + ISOtoORGtime(commententry.get('published')) + '\n')
                    f.write(' *** updated:   ' + ISOtoORGtime(commententry.get('updated')) + '\n')
                #f.write('  :END:\n')

    f.close()


def close_shelve_and_mark_sync_times(emacsDiaryLocation, shelve_data, gcal):
    query = gdata.calendar.service.CalendarEventQuery('default', 'private',
          'full')
    query.start_min = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime())
    query.start_max = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime())
    #query.updated_min = start_date
    feed = gcal.CalendarQuery(query)

    shelve_data['updated-e'] = time.gmtime(os.stat(emacsDiaryLocation).st_mtime)
    shelve_data['updated-g'] = time.strptime(feed.updated.text, '%Y-%m-%dT%H:%M:%S.000Z')

    del gcal
    shelve_data.close()


def InsertCommentstoGcal(dbe, gcal):
    return #debug
        #author = gdata.atom.Author()
        #author.name = comment_entry.get('name')
        #newcomment = gdata.calendar.CalendarEventCommentEntry()
        #newcomment.author

            #comment_entry.link[1] = comment_editlink


def update_attendee_status_to_gcal(username, identicalkeys, g2ekeymap, dbe, dbg, shelve_data, gcal, feed, editlinksmap):
    #return shelve, gcal, feed
    comment_status_enum = {'A': 'ACCEPTED', 'D': 'DECLINED', 'I': 'INVITED', 'T': 'TENTATIVE'}
    attendeestatus_modified_in_diary = [shelve_data[key].get('eventid') for key in identicalkeys if shelve_data[key].get('comment_owner_status') != dbe[key].get('comment_owner_status')]
    for key in attendeestatus_modified_in_diary:
        if key in dbg and 'comment_owner_status' in dbg[key] and shelve_data[g2ekeymap.get(key)].get('comment_owner_status') == dbg[key].get('comment_owner_status'):
            shelvekey = g2ekeymap[key]
            idxfeed = dbg[key].get('idxfeed')
            entrywho = feed.entry[idxfeed].who
            for idxentrywho in entrywho:
                if idxentrywho.email.split('@')[0] == username:
                    printedstatus = comment_status_enum.get(dbe[shelvekey].get('comment_owner_status'))
                    idxentrywho.attendee_status.value = printedstatus
            originalstatus = dbg[key].get('comment_owner_status')
            if originalstatus != None:
                originalstatus = comment_status_enum.get(originalstatus)
            else:
                originalstatus = ''
            editlink = editlinksmap.get(key)
            if printedstatus != None:
                print "-- updated attendee status for", dbg[key].get('fullentry'), ":", dbg[key].get('comment_owner_status'), " CHANGED TO: ", printedstatus
                new_event = gcal.UpdateEvent(editlink, feed.entry[idxfeed])
                new_editlink = new_event.GetEditLink()
                editlinksmap[key] = new_editlink
                shelve_data[shelvekey]['editlink'] = new_editlink

    return shelve_data, gcal, feed, editlinksmap


def update_comments_to_gcal(identicalkeys, g2ekeymap, dbe, dbg, shelve_data, gcal):
    """ this function will not work until google fixes its api.  The google calendar supplies a writable view of the comment feed to the api, but not the actual feed itself; updating the copy does not have any effect on the comments displayed in the google calendar web gui """
    return shelve_data, gcal #debug
    comments_modified_in_diary = [shelve_data[key].get('eventid') for key in identicalkeys if shelve_data[key].get('comment_owner_hash') != dbe[key].get('comment_owner_hash')]
    for key in comments_modified_in_diary:
        if key in dbg and shelve_data[g2ekeymap.get(key)].get('comment_owner_hash') == dbg[key].get('comment_owner_hash'):
            shelvekey = g2ekeymap[key]
            comment_entry = dbg[key].get('comment_owner_entry')
            comment_entry.content.text = dbe[shelvekey].get('comment_owner_content')
            print "-- updated comment for ", dbg[key].get('fullentry'), ":", dbg[key].get('comment_owner_content'), " CHANGED TO: ", comment_entry.content.text
            new_commentEvent = gcal.UpdateEvent(comment_entry.GetEditLink().href, comment_entry)
            shelve_data[shelvekey]['comment_owner_editlink'] = new_commentEvent.GetEditLink()
    return shelve_data, gcal


def update_edit_links(dbg, shelve_data):
    ekeyschangedinG = []
    gkeyschangedinG = []
    editlinksmap = {}               # editlinksmap maps gkey to eventid
    g2ekeymap = {}
    for key in shelve_data.keys():
        if type(shelve_data[key]) == DictionaryDefinedType:
            dbgrecord = dbg.get(shelve_data[key]['eventid'])
            if dbgrecord != None:
                eventid = dbgrecord.get('eventid')
                editlinkg = dbgrecord.get('editlink')
                editlinksmap[eventid] = editlinkg     #create a keymap for editlinks from g keys
                g2ekeymap[eventid] = key              #create a keymap from g keys to e keys also
                if editlinkg != shelve_data[key]['editlink']:

                    ekeyschangedinG.append(key)
                    gkeyschangedinG.append(shelve_data[key]['eventid'])


    for key in dbg.keys():
        if type(dbg[key]) == DictionaryDefinedType:
            editlinksmap[key] = dbg[key]['editlink']


    return ekeyschangedinG, gkeyschangedinG, g2ekeymap, editlinksmap


def append_to_keys(keylist, keystoinsert):
    for key in keystoinsert:
        if key not in keylist:
            keylist.append(key)
    return keylist


def appendkey(keylist, keytoinsert):
    if keytoinsert not in keylist:
        keylist.append(keytoinsert)
    return keylist


def remove_keys(keylist, keystoremove):
    keylist2 = []
    for key in keylist:
        if key not in keystoremove:
            keylist2.append(key)
    return keylist2


def removekey(keylist, keytoremove):
    keylist2 = []
    for key in keylist:
        if key != keytoremove:
            keylist2.append(key)
    return keylist2


def handle_contentions(readFromGoogleOnly, ENTRY_CONTENTION, identicalkeys, delfromG, addG, ekeyschangedinG, gkeyschangedinG, shelve_data, dbg, dbe, g2ekeymap):
    """entry contention happens when both a diary entry and its
    respective google calendar entry are modified before a sync.
    There is no way to precisely tell which diary entry was modified
    so all we can do is display the modified gcal entry along with
    perhaps a list of possibilities.  If the
    ENTRY_CONTENTION variable is set to 2 we will do nothing
    and just add both entries"""
    if readFromGoogleOnly:
        ENTRY_CONTENTION = 2
    dict = {}
    dictype = type(dict)
    contendingE = [shelve_data[key].get('eventid') for key in ekeyschangedinG if key in delfromG]

    dbekeys = [key for key in dbe.keys() if type(dbe[key] == dictype)]
    contendingdbe = [key for key in dbekeys if key not in identicalkeys]    ### contending entries from dbe will not appear in the identicalkeys list

    contendingdbe = sortkeysbydate(dbe, contendingdbe)

    contendingE = sortkeysbydate(dbg, contendingE)
    delfromdbe = []

    delfromdbg = []
    addEdit2E = []
    i = -1
    answer = '0'

    for key in contendingE:                  ###  nest 2 loops for contendingE (from gcal) and contendingdbe (from the diary)
        continuetoNextContendingE = False
        i += 1
        print "!! CONTENTION #", i, "!!!!!!!!! The following entry has been modified in both the emacs diary as well as the google calendar:"
        print ">> gcal:", dbg[key]['fullentry']
        if ENTRY_CONTENTION == 0:       # prompt from list of contenders
            if len(contendingdbe) == 0:             # if the list is empty then break to the next contendingE
                continue
                                                      #sort contendingdbe list
            j = -1
            for dbekey in contendingdbe:         ### nested loop for contendingdbe
                j += 1
                print "<< diary possibility#", j, ":", dbe[dbekey]['fullentry']
            if len(contendingdbe) > 1:
                answervalidated = False
                while not answervalidated:
                    answer = raw_input("?? Which diary possibility# most likely matches in contention with the aforementioned modified gcal entry? (n for none):")
                    if len(answer) > 0:
                        if answer[0] == 'n' or answer[0] == 'N':
                            answervalidated = True
                            continuetoNextContendingE = True
                        elif answer[0] >= '0' and answer[0] <= '9' and int(answer) < len(contendingdbe):
                            answervalidated = True
            elif len(contendingdbe) == 1:
                answer = '0'
            else:
                continuetoNextContendingE = True
        elif ENTRY_CONTENTION == 1:     # automatic best guess
            answer = '0'
        elif ENTRY_CONTENTION == 2:     # do nothing, allowing for contending entries to be added to both the diary and gcal
            continuetoNextContendingE = True
        if continuetoNextContendingE:
            continuetoNextContendingE = False
            continue
        match = int(answer)
        answervalidated = False
        while not answervalidated:
            answer = raw_input("?? keep gcal entry (g) or emacs diary entry (e)? (b for both)")
            answer = answer.lower()
            if answer == 'g':                       ### delete dbe match entry, and add the gcal contendingE entry to the diary
                delfromG = removekey(delfromG, g2ekeymap.get(key))
                addEdit2E = appendkey(addEdit2E, key)
                addG = removekey(addG, contendingdbe[match])
                delfromdbe = appendkey(delfromdbe, contendingdbe[match])  ### delete from the diary
                delfromdbe = appendkey(delfromdbe, g2ekeymap.get(key))        ### delete from the shelve

                del dbe[contendingdbe[match]]
                del contendingdbe[match]

                answervalidated = True
            elif answer == 'e':                    ### delete the contendingE entry, and add the dbe match entry
                delfromG = appendkey(delfromG, contendingE[i])
                addG = appendkey(addG, contendingdbe[match])

                ekeyschangedinG = removekey(ekeyschangedinG, g2ekeymap.get(key))  ## delete from list of edited gcal entries
                gkeyschangedinG = removekey(gkeyschangedinG, key)
                delfromdbg = appendkey(delfromdbg, key)
                del contendingdbe[match]
                answervalidated = True
            elif answer == 'b' or answer == 'n':
                answervalidated = True
    return identicalkeys, delfromG, delfromdbe, addG, delfromdbg, addEdit2E, ekeyschangedinG, gkeyschangedinG


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
    """Uses the _Getch class to input a string from the keyboard, masking the characters with '*', returning the string without the newline char"""
    a = 'q'
    passwd = ''
    while a != chr(13):
        a = getch()
        passwd = passwd + a
        print '*',
    return passwd[0:len(passwd) - 1]


def get_home_dir():   ## should work for windows and linux but not mac   ## OS Dependent
    try:
        from win32com.shell import shellcon, shell
        homedir = shell.SHGetFolderPath(0, shellcon.CSIDL_APPDATA, 0, 0)

    except ImportError: #  non-windows/win32com case
        homedir = os.path.expanduser("~")
    return homedir


def locate_emacs_diary_linux(homedir):
    """ find the location of the diary file by first looking for ~/diary, then try looking for the diary file location in the ~/.emacs file.  If we cant find it return None.  ## OS Dependent.  """
    defaultlocation = DIARY_FILE
    if os.path.exists(defaultlocation):
        return defaultlocation
    elif os.path.exists(homedir + '/diary'):
        return homedir + '/diary'
    elif os.path.exists(homedir + '/.emacs'):
        f = open(homedir + '/.emacs', "r")
        emacsfile = f.read()
        f.close()
        emacspat = re.compile(r'\(setq diary-file (.+?)\)')
        matches = emacspat.findall(emacsfile)
        if len(matches) > 0:
            match = matches[0]
            if len(match) > 2:
                if match[0] == '"' or match[0] == "'":
                    match = match[1:-1]
                if match[0] == "~":
                    match = match[1:]
                    match = homedir + match
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
        print >> sys.stderr, "Unable to locate the emacs diary.  Please create a file \
in your home directory called diary"
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

    shelve_data, last_sync_google, last_sync_emacs = \
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
                        shelve_data)

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
        edit_links_map = update_edit_links(google_db, shelve_data)

   # change the casename of recurrence events that contain exceptions
   # and add the exceptions to their EXCEPTIONSTRING.  note: the term
   # 'exception', throughout the scope of this script, refers to
   # recurrence exceptions, and not error exceptions
    google_db, flag_recurrence_updates = \
        address_exceptions(google_db,
                           shelve_data,
                           g_to_e_key_map,
                           orphaned + canceled,
                           times_template['caseTimeARange'],
                           cases_template)

    google_db, emacs_db = add_recurrence_descriptions(google_db, emacs_db)

    # identical_keys are hashkeys that are the same in both the shelve
    # and emacs_db, meaning the entries are unchanged by emacs diary
    identical_keys, del_from_g, add_to_g = \
        get_keys_to_modify_from_e(emacs_db, shelve_data)

    del_from_g, \
        add_to_g, \
        dict_update_orphans, \
        delete_orphans, \
        emacs_db = handle_exceptions(read_from_google_only,
                                     entry_contention,
                                     google_db,
                                     shelve_data,
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
                                                 shelve_data,
                                                 google_db,
                                                 emacs_db,
                                                 g_to_e_key_map)

    del_from_e, \
        add_e, \
        add_e_in_terms_of_g, \
        newly_added_g_keys_to_add_to_e = \
        get_keys_to_modify_from_g(google_db,
                                  del_from_e,
                                  shelve_data,
                                  identical_keys,
                                  last_sync_google)

    newly_added_g_keys_to_add_to_e = \
        remove_keys(newly_added_g_keys_to_add_to_e,
                   del_from_google_db)
    newly_added_g_keys_to_add_to_e = \
        append_to_keys(newly_added_g_keys_to_add_to_e,
                       add_edit_to_e)

    # if orphans are modified, their new value must be added to the shelve_data
    add_e = append_to_keys(add_e, dict_update_orphans.values())

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
        shelve_data, gcal = update_comments_to_gcal(identical_keys,
                                               g_to_e_key_map,
                                               emacs_db,
                                               google_db,
                                               shelve_data,
                                               gcal)
        shelve_data, gcal, feed, edit_links_map = \
            update_attendee_status_to_gcal(gmail_user,
                                           identical_keys,
                                           g_to_e_key_map,
                                           emacs_db,
                                           google_db,
                                           shelve_data,
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
            or len(dict_update_orphans) > 0 or \
            len(delete_orphans) > 0:
        delete_entries_from_e(shelve_data, del_from_e)

        if not read_from_google_only:
            delete_entries_from_gcal(del_from_g,
                                     del_from_google_db,
                                     google_db,
                                     gcal,
                                     shelve_data,
                                     edit_links_map,
                                     g_to_e_key_map,
                                     delete_orphans)
            update_orphans_in_gcal(dict_update_orphans,
                                   emacs_db,
                                   shelve_data,
                                   gcal,
                                   edit_links_map,
                                   g_to_e_key_map,
                                   feed)

        if diary_was_modified:
            if not read_from_google_only:
                insert_entries_into_gcal(add_to_g, emacs_db, gcal, shelve_data)
            insert_entries_edited_by_diary_to_e(add_e, emacs_db, shelve_data)

        if gcal_was_modified or len(flag_recurrence_updates) > 0:
            insert_entries_into_e(add_e_in_terms_of_g, shelve_data, google_db)

        write_emacs_diary(emacs_diary_location,
                          shelve_data,
                          diary_header,
                          unrecognized_diary_entries)
    else:
        print "-- No Changes"

    close_shelve_and_mark_sync_times(emacs_diary_location, shelve_data, gcal)


if __name__ == '__main__':
    sys.exit(main())
