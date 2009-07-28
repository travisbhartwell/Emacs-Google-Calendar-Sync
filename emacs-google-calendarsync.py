#!/usr/bin/python 
# emacs-google-calendarsync revision 41
# written and maintained by CiscoRx@gmail.com
# DISCLAIMER: if this script should fail or cause any damage then I, ciscorx@gmail.com, assume full liability; feel free to sue me for every penny I've got, the number of pennies of which would be small enough to fit in an envelope to mail to you.  Hopefully, it will cover postage.

try:
  from xml.etree import ElementTree
except ImportError:
  from elementtree import ElementTree
import gdata.calendar.service
import gdata.service
import atom.service
import gdata.calendar
import atom
import getopt
import sys
import string
import time
import datetime
import fileinput
import re
import os
import shelve
import pdb
from pprint import pprint

globalvar_DIARYFILE = ''            # Location of emacs diary 
globalvar_SHELVEFILE = ''                     # Location to put the shelve.dat file, which contains the schedule from the last sync.  The name of the shelve file will automatically contain the google calendar username
globalvar_DEFAULTEVENTDURATION = 60           # If no end time default to 60 min
globalvar_TZID = 'America/Chicago'            # Time zone
globalvar_DELETE_OLD_ENTRIES_OFFSET = 90      # number of days prior to the current date before which entries get deleted; they wont be deleted from the google calendar, just from the emacs diary.  This feature is currently not implemented
globalvar_GMTOFFSET = 6                       # 6=central timezone
globalvar_GMTOFFSET -= 1                      # for some reason need to subtract 1 to get it to work.  daylight savings time?
globalvar_ENTRY_CONTENTION = 0                # entry contention happens when the same diary and its respective google calendar entries are both modified before a sync. 0=prompt from list of contenders 1=automatic best guess, 0=prompt from list of contenders, 2=do nothing; allowing for both entries to exist in both gcal and diary
globalvar_DISCARD_ENTRIES_THAT_CONTAIN_THIS_CODE =  '#@!z8#'  # this will allow for multiple read-only calendars to be viewed in the same dairy.  The multiple calendar support is not yet implemented
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
  return ''.join(target)


def StripExtraNewLines(string):
  """ Use this function on the 'fullentry' field before hashing to get a key, as sometimes new lines can get into the diary and mess up the hash""" 
  pos = string.find('\n\n')
  while  pos != -1:
    string = string.replace('\n\n','\n')
    pos = string.find('\n\n')
  if string == '\n':
    string = " "
  elif len(string) > 1 and string[-1] == '\n':
    string = string[0:-1]
  return string


def escstring(string):   
  """ Use this function in place of re.escape(); re.escape() does not seem to work right...it is only used in the loadTemplate() function"""
  str=[]
  target=''
  for i in range(len(string)):
    if string[i] in ['(',')','*','+', '.', '?']:
      str.append('\\')
    elif string[i] == "%":
      str.append('%')
    str.append(string[i])

  if string[0]=='&':
    str.insert(0,'&')
    str[1]='?'
  elif string[1]=='&':
    str.insert(1,'&')
    str[2]='?'
  
  target=''.join(str)
  return target 

def rmwhtspc(sTarget):
  sTarget.strip()
  rmwhtspcpat = re.compile(r'\s+')
  sTarget = rmwhtspcpat.sub(' ',sTarget)
  sTarget = sTarget.replace(') (',')(')
  return sTarget

 
def loadMtchvars(filename): 
  """The _mtch file must end in a new line.  Uppercase entries are recognized as variable names for pattern patching.  Lowercase entries are not recognized as matching variables, and their values are substituted in verbatim.  Any variable appearing more than once in an entry must have a 1 digit ordinal number appende to the variable name, incremented for each occurrence"""
  f = open(filename)
  ff=f.readlines()
  f.close()
  key = ''
  identicalkeys = []
  dicDatatypes = {}
  for line in ff:
    spcloc = line.find(" ")
    key = line[0:spcloc]
    if key.islower():
      dicDatatypes[ key ] = line[spcloc+1:]   ## lowercase keys dont get var names  
    elif key[:-1] in identicalkeys:  ### more than one occurance of a variable in a pattern must have a 1 digit ordinal number appended to the variable name
      dicDatatypes[ key ] = line[spcloc+1] + '?P=' + key[:-1] + ')'
    else:
      dicDatatypes[ key ] = line[spcloc+1] + '?P<' + key[:] + '>' + line[spcloc+2:len(line)-1]
    identicalkeys.append(key)
  return dicDatatypes

def loadreftable(filename):
  """ loads simple one level dictionary from a file. The file must be tab separated and end in a new line.  There cannot be more than 1 newline at the end of the file or an error will result"""
  f = open(filename)
  ff=f.readlines()
  f.close()
  key = ''
  value = ''
  dict = {}
  for line in ff:
    key,value = line.split('\t')
    value = value.strip()
    dict[ key ] = value 
  return dict




def loadTemplate(filename, Escape = True):    
  """all whitespace thats longer than a single space is removed from template.  If whitespace is needed to be represented in a pattern, create a tag for it in lowercase letters, and create an entry in the _mtch file indicating what to sub in verbatim.
     note: the ^ may only be used in template to indicate beginning of string.  The EvaluateTemplate() function is used to create matching patterns using loadTemplates return value as an argument. 
    """
  f= open(filename,"r")
  test = f.readlines()
  f.close()
  test = stripallarray(test)
  casestring = ''.join(test) 
  #casestring = rmwhtspc(casestring)
  casepat = re.compile(r'<(.+?)>(.+?)</')
  tpCases = casepat.findall(casestring)

  pat = re.compile(r'\<(\w+?)\>')
  tCase={}
  ttCase={}
  aCasename=[]
  casename=''
  escapedstring = ''          
  for i in range(len(tpCases)):
    casename=tpCases[i][0]
    if Escape == True:
      escapedstring = escstring(tpCases[i][1])  #escape (,),%, adding ? if starts with &
    else:
      escapedstring = tpCases[i][1]
    tmpstr = re.sub(pat,r'%(\1)s',escapedstring)  
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
    p0 = atTemplate[i]%dicTypes
    stringpatterns.append(p0)
    dicEvaluatedTemplatesArray[i] = (re.compile(p0,re.M | re.S))
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
   

 
def parseList2db(pat,lDiaryArray, keys):
  """ keys argument is a list of entrypids associated with each string in lDiaryArray, respectively.   pat is a dictionary of patterns to check against.  The pattern cases are matched against a diary string in alphabetical order of case name, accepting the first pattern that is matched""" 
  dbTmp = {}
  entrypid = ''
  moCase2Diary = {}
  patkeys  = pat.keys()
  patkeys.sort()
  for i in range(len(lDiaryArray)):
    for idxCase in patkeys:
      moCase2Diary = pat[idxCase].search(lDiaryArray[i])
      if moCase2Diary != None:             ## find only first match
        entry = {}
        entry = moCase2Diary.groupdict()
        entrypid =  keys[i]
        entry['entrypid'] = entrypid
        casename ='casename-' + idxCase 
        entry[casename] = idxCase
        dbTmp[entrypid] = entry
        break
  return dbTmp

def getTimeRanges(db, keys):
  timeranges= []
  tmpRec = {}
  for key in keys:
      tmpRec = db[key]
      if 'TIMERANGE' in tmpRec.keys():
        timeranges.append(tmpRec['TIMERANGE'])
      else:
        timeranges.append('')
  return timeranges 

def updateDetails(db, details, keys):
  tDetails = loadTemplate('detail_template')
  patDetails, sd = EvaluateTemplates(tDetails, 'detail_template_mtch') 
  dbTmp = parseList2db(patDetails, details, keys)
  deepupdate(db,dbTmp)
  timeranges = getTimeRanges(db, keys)

  atTimescases = loadTemplate('times_template')
  patTimes, sp = EvaluateTemplates(atTimescases, 'times_template_mtch')
  dbTmp = parseList2db(patTimes, timeranges, keys)
  deepupdate(db, dbTmp)

def e2gbyday(byday):
  """  converts emacs calendar BYDAY field data to the google calendar BYDAY equivalent""" 
  if byday == '':
    return ''
  bydayg=''
  daysofweek = ['SU','MO','TU','WE','TH','FR','SA']
  lines = byday.split(' ')
  for line in lines:
       bydayg = bydayg + daysofweek[int(line)] + ','
  bydayg = bydayg[:-1]
  return bydayg

def g2ebyday(byday):
  """  converts google calendar BYDAY field data to the emacs calendar BYDAY equivalent""" 
  if byday == '':
    return ''
  bydaye=''
  daysofweek = ['SU','MO','TU','WE','TH','FR','SA']
  lines = byday.split(',')
  for line in lines:
    bydaye = bydaye + str(daysofweek.index(line.upper())) + ' '
  bydaye = bydaye[:-1]
  return bydaye


def e2gmonthabbr(monthabbr):
  months = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05',
            'Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10',
            'Nov':'11','Dec':'12'}
  return months[monthabbr]

def g2emonthabbr(month):
  months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  return months[int(month)-1]

def g2ewhichweek(whichweekg):
  days = {'SU':'0', 'MO':'1','TU':'2','WE':'3','TH':'4','FR':'5','SA':'6'}
  if whichweekg[0] == '-':
    whichweek = whichweekg[0:2]
    whichday = whichweekg[2:4]
  else:
    whichweek = whichweekg[0]
    whichday = whichweekg[1:3]
  whichday = days[whichday.upper()]  
  return whichweek, whichday


def striptime(timestring):
  timestring = timestring.replace(':','')
  timestring = timestring.replace('-','')
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

def sdeltaDatetime(offsetdays):
  return datetime.datetime.now() + datetime.timedelta(offsetdays)

def HandleLooseEmacsEnds(db):
  """ this function should probably be rewritten """
  gcases_template = loadTemplate('gcases_template',Escape=False)
  now = time.strptime(time.asctime())
  nowyear = now[0]
  nowmonth = now[1]
  nowday = now[2]
  nowhour = now[3]
  nowminute = now[4]
  daysofweek = ['SU','MO','TU','WE','TH','FR','SA']
  for dbkey in db.keys():
    alldayevent = False
    reckeys = db[dbkey].keys()
    db[dbkey]['TZID'] = globalvar_TZID 
    db[dbkey]['TZID2'] = globalvar_TZID
    db[dbkey]['newline']='\r\n'
    db[dbkey]['STMONTH'] = db[dbkey].setdefault('STMONTH',str(nowmonth)).zfill(2)
    db[dbkey]['STDAY']= db[dbkey].setdefault('STDAY',str(nowday)).zfill(2)
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
    if db[dbkey]['STAMPM'].upper()[0] == 'P' and sthour != 12:
        sthour += 12
    stdatetime = datetime.datetime(styear,stmonth,stday,sthour,stminute)
    stdatetimestr = stdatetime.isoformat()
    stdatetimestr = stdatetimestr.replace(':','')
    stdatetimestr = stdatetimestr.replace('-','')
    defaultenddatetime = stdatetime + datetime.timedelta(minutes=globalvar_DEFAULTEVENTDURATION)
    defaultenddatetimetuple = defaultenddatetime.timetuple()
    if 'ENDYEAR' not in reckeys:
      db[dbkey]['ENDYEAR'] = str(defaultenddatetimetuple[0]).zfill(4)
    elif len(db[dbkey]['ENDYEAR']) < 4:
      year = '2' + db[dbkey]['ENDYEAR'].zfill(3)
      db[dbkey]['STYEAR'] = year 
    db[dbkey]['ENDMONTH'] = db[dbkey].setdefault('ENDMONTH',str(defaultenddatetimetuple[1])).zfill(2)
    db[dbkey]['ENDDAY'] = db[dbkey].setdefault('ENDDAY',str(defaultenddatetimetuple[2])).zfill(2)
    db[dbkey]['ENDHOUR'] = db[dbkey].setdefault('ENDHOUR',str(defaultenddatetimetuple[3])).zfill(2)
    db[dbkey]['ENDMINUTE'] = db[dbkey].setdefault('ENDMINUTE',str(defaultenddatetimetuple[4])).zfill(2)
    endday = int(db[dbkey]['ENDDAY'])
    endmonth = int(db[dbkey]['ENDMONTH'])
    endyear = int(db[dbkey]['ENDYEAR'])
    endhour = int(db[dbkey]['ENDHOUR'])
    endminute = int(db[dbkey]['ENDMINUTE'])
    if 'ENDAMPM' not in reckeys:       ## assume default from calculated delta
      db[dbkey]['ENDAMPM'] = time.strftime('%p',defaultenddatetimetuple)
      db[dbkey]['ENDHOUR'] = time.strftime('%I',defaultenddatetimetuple)
    elif db[dbkey]['ENDAMPM'].upper()[0] == 'P' and endhour != 12:
      endhour += 12
    enddatetime = datetime.datetime(endyear,endmonth,endday,endhour,endminute)
    enddatetimestr = enddatetime.isoformat()
    enddatetimestr = enddatetimestr.replace(':','')
    enddatetimestr = enddatetimestr.replace('-','')
    if alldayevent == True:
      db[dbkey]['alldayevent'] = True
      enddatetimestr = enddatetimestr[:8]
      stdatetimestr = stdatetimestr[:8]
      startdatetimetuple = datetime.datetime(styear,stmonth,stday).timetuple()
      enddatetimetuple = datetime.datetime(endyear,endmonth,endday).timetuple()
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
      untildatetime = datetime.datetime(untilyear,untilmonth,untilday)
      untildatetimestr = untildatetime.isoformat()
      untildatetimestr = untildatetimestr.replace(':','')
      untildatetimestr = untildatetimestr.replace('-','')
      db[dbkey]['UNTILDATETIME']= untildatetimestr[:8]
    gcase = db[dbkey]['gcase']
                                                  ## write recurrence string from gcases_template 
    if len(gcase) > 7 and gcase[:7] == 'caseRec':
      recurrencestring = gcases_template[gcase] % db[dbkey]
      db[dbkey]['recurrencestring'] = recurrencestring
    

    
def printcontents(db):
  """ used for debugging purposes """
  for keys in db.keys():
    print db[keys].get('TITLE')
    print db[keys].get('CONTENT')
    print " "

def getEmacsDiary(emacsDiaryLocation):
  db={}
  ap = loadTemplate('cases_template')
  pat,sp = EvaluateTemplates(ap, 'cases_template_mtch')
  f=open(emacsDiaryLocation, "r") 
  file=f.read()
  f.close()
  keys = []
  details = []
  file = file + "\nend"            ## need this or last entry wont be read
  e2gcase_table = loadreftable('e2gcase_table')
  for idxCase in pat.keys():
    mo =  pat[idxCase].search(file)
    while mo != None:
      entry = {}
      entry = mo.groupdict()
      fullentry = StripExtraNewLines(file[mo.start(0):mo.end(0)]  )
      entry['fullentry'] = fullentry
      if fullentry.find(globalvar_DISCARD_ENTRIES_THAT_CONTAIN_THIS_CODE) == -1:  ## this is for a future feature
        entrypid = str(hash(fullentry))
        keys.append(entrypid)
        details.append(entry['DETAIL'])
        entry['entrypid'] = entrypid
        entry['entrycase'] = idxCase
        entry['gcase'] = e2gcase_table[idxCase]
        db[entrypid] = entry
      file = file[:mo.start(0)] + file[mo.end(0):]
      mo= pat[idxCase].search(file)
  updateDetails(db, details, keys)
  HandleLooseEmacsEnds(db)
  if (len(file)>5):
    print "-- UNRECOGNIZED ENTRIES:"
    print file[:-3]

  return db, ap
   

def recGetFieldTZID( recurrence):
  pos = recurrence.find('TZID')
  pos = recurrence.find('=',pos)
  posend = recurrence.find(':',pos)
  return recurrence[pos+1:posend]

def recGetField(fieldname, recurrence):
  pos = recurrence.find(fieldname)
  pos = recurrence.find(':',pos)
  posend = recurrence.find('\n',pos)
  return recurrence[pos+1:posend]

def recGetRRuleField(fieldname, rule):
  pos1 = rule.find(fieldname)
  pos2 = rule.find('=',pos1)
  posend = rule.find(';',pos2)
  if posend == -1:
    posend = len(rule)
  if pos1 > -1:
    return rule[pos2+1:posend]
  else:
    return ''

   
def blankforNoneType(string):
  if string == None:
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
  x = datetime.datetime(year,month,day,hour,minute)
  return x.timetuple()  
  
def getGoogleCalendar(username,passwd,time_min):
  at = loadTemplate('times_template', Escape = False)
  ap = loadTemplate('cases_template', Escape = False)
  nowdatetime = sCurrentDatetime()
  recurrences = []
  recurrencekeys = []
  db={}
  gcal = gdata.calendar.service.CalendarService()
  gcal.email = username
  gcal.password = passwd
  gcal.source = 'Google-Emacs-Calendar-Sync-1.0'
  gcal.ProgrammaticLogin()

  query = gdata.calendar.service.CalendarEventQuery('default', 'private', 
        'full')
  #query.start_min = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time_min)
  #start_min = sdeltaDatetime(-globalvar_DELETE_OLD_ENTRIES_OFFSET)
  #query.start_min= start_min.strftime('%Y-%m-%dT%H:%M:%S.00Z')
  query.start_max = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime(time.time() + 28800000))
  #query.updated_min = start_date
  query.ctz = globalvar_TZID
  query.max_results = 400
  feed = gcal.CalendarQuery(query)
  db['updated-g']=  time.strptime(feed.updated.text,'%Y-%m-%dT%H:%M:%S.000Z')
  for i, an_event in zip(xrange(len(feed.entry)), feed.entry):
    entrypid = an_event.id.text
    entry={}
    entry['HYPHEN'] = ' - '
    entry['eventid'] = entrypid
    entry['where'] = blankforNoneType(an_event.where[0].text)
    entry['TITLE'] = blankforNoneType(an_event.title.text)
    content = blankforNoneType(an_event.content.text)
    content = StripExtraNewLines(content)
    content = RemoveNewlinesSpacePadding(content)  #debug
    content = PadAllNewlinesWithSpace(content)
    entry['CONTENT'] = content

    entry['modified'] = time.strptime(an_event.updated.text[:19],'%Y-%m-%dT%H:%M:%S')
    entry['editlink'] = an_event.GetEditLink().href
    if an_event.recurrence != None:
      entry['recurrence_raw']= an_event.recurrence.text
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
      if len(stdatetime) > 8:
        if int(stdatetime[9:11]) > 12:
          entry['STAMPM'] = 'pm'
          entry['STHOUR'] = str(int(stdatetime[9:11]) - 12)
        else:
          entry['STAMPM'] = 'am'
          entry['STHOUR'] = stdatetime[9:11]
        entry['STMINUTE'] = stdatetime[11:13]
        if int(enddatetime[9:11]) > 12:
          entry['ENDAMPM'] = 'pm'
          entry['ENDHOUR'] = str(int(enddatetime[9:11]) - 12)
        else:
          entry['ENDAMPM'] = 'am'
          entry['ENDHOUR'] = enddatetime[9:11]
        entry['ENDMINUTE'] = enddatetime[11:13]
        entry['DETAIL'] = entry['TITLE'] + ' ' + at['caseTimeARange'] % entry + entry['CONTENT']
      else:                                      #### all day event
        entry['DETAIL'] = entry['TITLE'] + '\n' + entry['CONTENT']
      entry['fullentry'] =  StripExtraNewLines(entry['STMONTH'] + '/' + entry['STDAY'] + '/' + entry['ENDYEAR'] + ' ' + entry['DETAIL']  )
    db[entrypid] = entry

                                                 #### now parse recurrences
  for i, recurrence in enumerate(recurrences):
    casefrequency = ''
    casegeneral = ''
    caseinterval = ''
    caseblock = ''
    bydayg = ''
    dtstart = recGetField('DTSTART',recurrence)
    db[recurrencekeys[i]]['SOMEZING'] = '"%V"'
    db[recurrencekeys[i]]['SOMEZING2'] = '"%V"'
    db[recurrencekeys[i]]['dtstart'] = dtstart
    db[recurrencekeys[i]]['STYEAR'] = dtstart[0:4]
    db[recurrencekeys[i]]['STMONTH'] = dtstart[4:6]
    db[recurrencekeys[i]]['STDAY'] = dtstart[6:8]
    db[recurrencekeys[i]]['STYEAR2'] = dtstart[0:4]
    db[recurrencekeys[i]]['STMONTH2'] = dtstart[4:6]
    db[recurrencekeys[i]]['STDAY2'] = dtstart[6:8]
    dtend= recGetField('DTEND',recurrence)
    db[recurrencekeys[i]]['dtend'] = dtend
    db[recurrencekeys[i]]['ENDYEAR'] = dtend[0:4]
    db[recurrencekeys[i]]['ENDMONTH'] = dtend[4:6]
    db[recurrencekeys[i]]['ENDDAY'] = dtend[6:8]
    tzid = recGetFieldTZID(recurrence)
    db[recurrencekeys[i]]['TZID'] = tzid
    db[recurrencekeys[i]]['TZID2'] = tzid    
    rrule = recGetField('RRULE',recurrence)
    db[recurrencekeys[i]]['rrule'] = rrule
    casefrequency = recGetRRuleField('FREQ',rrule)
    casefrequency = casefrequency.lower().capitalize()
    db[recurrencekeys[i]]['freq'] = casefrequency
    untildate = recGetRRuleField('UNTIL',rrule)
    db[recurrencekeys[i]]['until'] = untildate
    if untildate != '':
      caseblock = 'Block'
      db[recurrencekeys[i]]['UNTILYEAR'] = untildate[0:4]
      db[recurrencekeys[i]]['UNTILMONTH'] = untildate[4:6]
      db[recurrencekeys[i]]['UNTILDAY'] = untildate[6:8]     
    db[recurrencekeys[i]]['wkst'] = recGetRRuleField('WKST',rrule)
    interval = recGetRRuleField('INTERVAL',rrule)
    if interval != '' and int(interval) > 0:
      db[recurrencekeys[i]]['INTERVAL'] = interval
      caseinterval = 'Interval'
    bydayg = recGetRRuleField('BYDAY',rrule)
    db[recurrencekeys[i]]['BYDAYG'] = bydayg
    if len(bydayg) > 0 and ( bydayg[0].isdigit() == True or bydayg[0] == '-'):
      casegeneral = 'bydayofweek'
      whichweek,whichday= g2ewhichweek(bydayg)
      db[recurrencekeys[i]]['WHICHWEEK'] = whichweek
      db[recurrencekeys[i]]['NUMDAYOFWEEK'] = whichday
    else:
      db[recurrencekeys[i]]['BYDAY'] = g2ebyday(bydayg)
    casename = 'caseRec' + casefrequency + casegeneral + caseinterval + caseblock
    db[recurrencekeys[i]]['caseRec'] = casename
    if len(dtstart) > 8:
      if int(dtstart[9:11]) > 12:
        db[recurrencekeys[i]]['STAMPM'] = 'pm'
        db[recurrencekeys[i]]['STHOUR'] = str(int(dtstart[9:11]) - 12)
      else:
        db[recurrencekeys[i]]['STAMPM'] = 'am'
        db[recurrencekeys[i]]['STHOUR'] = dtstart[9:11]
      db[recurrencekeys[i]]['STMINUTE'] = dtstart[11:13]
      if int(dtend[9:11]) > 12:
        db[recurrencekeys[i]]['ENDAMPM'] = 'pm'
        db[recurrencekeys[i]]['ENDHOUR'] = str(int(dtend[9:11]) - 12)
      else:
        db[recurrencekeys[i]]['ENDAMPM'] = 'am'
        db[recurrencekeys[i]]['ENDHOUR'] = dtend[9:11]
      db[recurrencekeys[i]]['ENDMINUTE'] = dtend[11:13]
      db[recurrencekeys[i]]['DETAIL'] =  db[recurrencekeys[i]]['TITLE'] + ' ' + at['caseTimeARange'] % db[recurrencekeys[i]] + ' ' + db[recurrencekeys[i]]['CONTENT']
    else:
      db[recurrencekeys[i]]['DETAIL'] = db[recurrencekeys[i]]['TITLE'] + ' ' + db[recurrencekeys[i]]['CONTENT'] 
    recurrencestring = ap[casename] % db[recurrencekeys[i]]
    if recurrencestring[0] == '%':
      recurrencestring = '%' + recurrencestring
    if recurrencestring[:1] == '&%':
      recurrencesstring = '&%' + recurrencestring[1:]
    db[recurrencekeys[i]]['fullentry'] = StripExtraNewLines(recurrencestring ) 
    db[recurrencekeys[i]]['timetuple_dtstart'] = Convertdtstart2timetuple(dtstart)

  return db, gcal

def getKeystomodifyfromE(db1,db2):
  """ Returns some arrays of keys that are to be inserted into or deleted from Gcal.  Any edited entries are deleted and reinserted """
  dict = {}
  dictype = type(dict) 
  keys1 = [key for key in db1.keys() if type(db1[key])==dictype]
  keys2 = [key for key in db2.keys() if type(db2[key])==dictype]

  identicalkeys = [key for key in keys1 if key in keys2]          # identicalkeys are hashkeys that are the same in both the shelve and dbe, meaning the entries are unchanged 
  
  delfromG = [key for key in keys2 if key not in identicalkeys]
  addtoG = [key for key in keys1 if key not in identicalkeys]
  return identicalkeys, delfromG, addtoG

def getKeystomodifyfromGREDACTED(dbg,dbshelf,identicalkeys, glastsynctime):
  """ Returns some arrays of keys that are to be inserted into or deleted from the emacs Diary.  Any edited entries are deleted and reinserted """
  ### REDACTED
                                                                  # identicalkeys are hashkeys that exist in both the shelve and dbe
  dict = {}
  dictype = type(dict) 
  gkeys = [key for key in dbg.keys() if type(dbg[key])==dictype]
  skeys = [key for key in dbshelf.keys() if type(dbshelf[key])==dictype]
  skeyeventids = [dbshelf[key].get('eventid') for key in skeys]
  delfromE = [key for key in identicalkeys if dbshelf[key].get('eventid') not in gkeys]
  addE = [key for key in identicalkeys if key not in delfromE and dbg[dbshelf[key]['eventid']].get('modified') > glastsynctime]
  #addE = [key for key in identicalkeys if key not in delfromE]
 

  addEinTermsofGkeys = [dbshelf[key]['eventid'] for key in identicalkeys if key not in delfromE and dbg[dbshelf[key]['eventid']].get('modified') > glastsynctime]
  #addEinTermsofGkeys = [dbshelf[key]['eventid'] for key in identicalkeys if key not in delfromE]
   
  delfromE += addE                    #instead of editing simply delete and create a new entry
  alsoaddtheseGkeystoE = [key for key in gkeys if key not in skeyeventids]
  return delfromE, addE, addEinTermsofGkeys, alsoaddtheseGkeystoE
  
def getKeystomodifyfromG(dbg,delfromEalso,shelve,identicalkeys, glastsynctime):
  """ Returns some arrays of keys that are to be inserted into or deleted from the emacs Diary.  Any edited entries are deleted and reinserted """
                                                                  # identicalkeys are hashkeys that are the same in both the shelve and dbe
  dict = {}
  dictype = type(dict) 
  gkeys = [key for key in dbg.keys() if type(dbg[key])==dictype]
  skeys = [key for key in shelve.keys() if type(shelve[key])==dictype]
  skeyeventids = [shelve[key].get('eventid') for key in skeys]
  delfromE = [key for key in identicalkeys if shelve[key].get('eventid') not in gkeys]

  addE = [key for key in identicalkeys if key not in delfromE and dbg[shelve[key]['eventid']].get('modified') > glastsynctime]
  #addE = [key for key in identicalkeys if key not in delfromE]
 

  addEinTermsofGkeys = [shelve[key]['eventid'] for key in identicalkeys if key not in delfromE and dbg[shelve[key]['eventid']].get('modified') > glastsynctime]
  #addEinTermsofGkeys = [dbshelf[key]['eventid'] for key in identicalkeys if key not in delfromE]
   
  #delfromE += addE                    #instead of editing simply delete and create a new entry
  alsoaddtheseNewlyAddedGkeystoE = [key for key in gkeys if key not in skeyeventids]  # these are newly entered from gcal


  delfromE = appendtokeys(delfromE, delfromEalso)
  return delfromE, addE, addEinTermsofGkeys, alsoaddtheseNewlyAddedGkeystoE
 

def getShelveandLastSyncTimes(emacsDiaryLocation, gmailuser):
  lastmodifiedg = time.strptime('1995-1-1T12:00:00','%Y-%m-%dT%H:%M:%S')
  lastmodifiede = time.gmtime(os.stat(emacsDiaryLocation).st_mtime)
  shelvepath = globalvar_SHELVEFILE
  shelvenamefq = shelvepath + 'egcsyncshelve' + gmailuser + '.dat'
  f=shelve.open(shelvenamefq)
  if f!={}:
    lastmodifiedg = f['updated-g']
    lastmodifiede = f['updated-e']    
  return f,lastmodifiedg,lastmodifiede

def convertTimetuple2GMT(tt):
  a = datetime.datetime(tt[0],tt[1],tt[2],tt[3],tt[4])
  a += datetime.timedelta(hours=globalvar_GMTOFFSET)
  return a.timetuple()
  
def InsertEntryIntoGcal(entry, gcal):
  ## all non-recurring events must be entered in terms of GMT
  event = gdata.calendar.CalendarEventEntry()
  event.title = atom.Title(text=entry.get('TITLE'))
  event.title.text = entry.get('TITLE')
  event.content = atom.Content(text=entry.get('CONTENT'))
  content = entry.get('CONTENT')

  if content != None:
    event.content.text = RemoveNewlinesSpacePadding(content)
 
#    event.where.append(gdata.calendar.Where(value_string=event['WHERE']))

  if 'recurrencestring' in entry:
    event.recurrence = gdata.calendar.Recurrence(text=entry['recurrencestring'])
  else:
    event.recurrence = None
    timetuple_dtstart = entry['timetuple_dtstart']
    timetuple_dtstart = convertTimetuple2GMT(timetuple_dtstart)
    timetuple_dtend = entry['timetuple_dtend']
    timetuple_dtend = convertTimetuple2GMT(timetuple_dtend)
    if entry['alldayevent'] == True:
      start_time = time.strftime('%Y-%m-%d', timetuple_dtstart)
      end_time = time.strftime('%Y-%m-%d', timetuple_dtend)
    else:
      start_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', timetuple_dtstart)
      end_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', timetuple_dtend)
    event.when.append(gdata.calendar.When(start_time=start_time, end_time=end_time))
    
  new_event = gcal.InsertEvent(event, '/calendar/feeds/default/private/full')
  return new_event.id.text, new_event.GetEditLink().href

def InsertEntriesIntoGcal(addG,dbe,gcal,shelve):
  for key in addG:
    eventid, editlink = InsertEntryIntoGcal(dbe[key],gcal)
    dbe[key]['eventid'] = eventid
    dbe[key]['editlink'] = editlink 
    shelve[key] = dbe[key].copy()
    print "-- inserted from Diary to Gcal: " + shelve[key]['fullentry'] 

def DeleteEntriesFromE(shelve,delfromE):
  for key in delfromE:
    record = shelve.get(key)
    if record != None:
      print "-- deleted from Diary: " + record.get('fullentry') 
      del shelve[key]

def DeleteEntriesFromGcal(delG,delfromdbg,dbg,gcal,shelve, editlinksmap,g2ekeymap):
  #eventids = [shelve[ekey]['eventid'] for ekey in delG]
  #pdb.set_trace() #debug
  #editlinks = [shelve[gkey]['editlink'] for gkey in eventids]
  #for key in delfromdbg:
  #  editlink = editlinksmap.get(key)
  #  gcal.DeleteEvent(editlink)
  #  pdb.set_trace()#debug
  #  print "-- deleted from Gcal: " + dbg[key]['fullentry']
 
  for key in delG:
    record = shelve.get(key)
    if record != None:
      eventid = record.get('eventid')
      if eventid != None:
        editlink = editlinksmap.get(eventid)
        if editlink != None:
          gcal.DeleteEvent(editlink)
          print "-- deleted from Gcal and Diary: " + shelve[key]['fullentry']
          del shelve[key]



def InsertEntriesEditedbyDiarytoE(addE,dbe,shelve):
  for key in addE:
    shelve[key] = dbe[key].copy()
    print "-- insert edit into Diary: " + shelve[key]['fullentry']
    

def InsertEntriesIntoE(addGkeystoE, shelve,dbg):
  for gkey in addGkeystoE:
    entrypid = str(hash(dbg[gkey]['fullentry']))
    dbg['entrypid'] = entrypid
    shelve[entrypid] = dbg[gkey].copy()
    print "-- inserted to Diary: " + dbg[gkey]['fullentry']

def createIndexFromShelve(db):
  """ function called from WriteEmacsDiary() used to sort the diary entries for the emacs calendar
        it returns a 2xn matrix of timestamps associated with entry starting dates and hash keys, primary keys of the shelve"""
  dict = {}
  dictype = type(dict)
  dbkeys = db.keys()
  index = []
  for key in dbkeys:
    if type(db[key]) == dictype:
      dttimestamp= time.mktime(db[key]['timetuple_dtstart'])   ## start date converted to timestamp for indexing
      row = []
      row.append(dttimestamp)
      row.append(key)
      index.append(row)
  index.sort(key=lambda x:x[1] )
  return index

def sortkeysbydate(db, keys):
  """ function called from handlecontentions() used to sort the diary entries for the emacs calendar
        it returns a 2xn matrix of timestamps associated with entry starting dates and hash keys, primary keys of the shelve"""
  dict = {}
  dictype = type(dict)
  index = []
  for key in keys:
    if type(db[key]) == dictype:
      dttimestamp= time.mktime(db[key]['timetuple_dtstart'])   ## start date converted to timestamp for indexing
      row = []
      row.append(dttimestamp)
      row.append(key)
      index.append(row)
  index.sort()

  target = [];
  size = len(index)
  for i in range(0,size):
    target.append(index[i][1])
  return target
  
def WriteEmacsDiary(emacsDiaryLocation, shelve):
  dict = {}
  dictype = type(dict) 
  #keys = [key for key in shelve.keys() if type(shelve[key])==dictype]
  index = createIndexFromShelve(shelve)
  
  f = open(emacsDiaryLocation,'w')
  f.seek(0)
  for row in index: 
    f.write(shelve[row[1]].get('fullentry') + '\n')  
  f.close()

def CloseShelveandMarkSyncTimes(emacsDiaryLocation,shelve,gcal):

  query = gdata.calendar.service.CalendarEventQuery('default', 'private', 
        'full')
  query.start_min = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime())
  query.start_max = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime())
  #query.updated_min = start_date
  feed = gcal.CalendarQuery(query)
 
  shelve['updated-e'] = time.gmtime(os.stat(emacsDiaryLocation).st_mtime)
  shelve['updated-g'] = time.strptime(feed.updated.text,'%Y-%m-%dT%H:%M:%S.000Z')

  del gcal
  shelve.close()



def updateEditLinks(dbg,shelve):
  dict = {}
  dictype = type(dict)
  #pdb.set_trace()
  ekeyschangedinG = []
  gkeyschangedinG = []
  editlinksmap = {}
  g2ekeymap = {}
  for key in shelve.keys():
    if type(shelve[key]) == dictype:
      dbgrecord =  dbg.get(shelve[key]['eventid'])
      if dbgrecord != None:
        eventid = dbgrecord.get('eventid')
        editlinkg = dbgrecord.get('editlink')
        editlinksmap[eventid] = editlinkg     #create a keymap for editlinks from g keys
        g2ekeymap[eventid] = key              #create a keymap from g keys to e keys also 
        if editlinkg != shelve[key]['editlink']:
          
          ekeyschangedinG.append(key)
          gkeyschangedinG.append(shelve[key]['eventid'])
          #erecord = shelve[key]
          #erecord['editlink'] = editlinkg
          #shelve[key] = erecord.copy()

  for key in dbg.keys():
    if type(dbg[key]) == dictype:
      editlinksmap[key] = dbg[key]['editlink']

 
  return ekeyschangedinG, gkeyschangedinG, g2ekeymap, editlinksmap
        
def appendtokeys( keylist, keystoinsert):
  for key in keystoinsert:
    if key not in keylist:
      keylist.append(key)
  return keylist

def appendkey( keylist, keytoinsert):
  if keytoinsert not in keylist:
    keylist.append(keytoinsert)
  return keylist

def removekeys( keylist, keystoremove):
  keylist2 = []
  for key in keylist:
    if key not in keystoremove:
      keylist2.append(key)
  return keylist2

def removekey (keylist, keytoremove):
  keylist2 = []
  for key in keylist:
    if key != keytoremove:
      keylist2.append(key)
  return keylist2


def handleContentions(ENTRY_CONTENTION, identicalkeys, delfromG, addG, ekeyschangedinG,gkeyschangedinG,shelve,dbg, dbe):
  """entry contention happens when both a diary entry and its respective google calendar entry are modified before a sync.  There is no way to precisely tell which diary entry was modified so all we can do is display the modified gcal entry along with perhaps a list of possibilities.   If the globalvar_ENTRY_CONTENTION variable is set to 2 we will do nothing and just add both entries"""

  dict = {}
  dictype = type(dict)
  contendingE = [key for key in ekeyschangedinG if key in delfromG]
  shelvekeys = [key for key in shelve.keys() if type(shelve[key]==dictype)]
  dbekeys = [key for key in dbe.keys() if type(dbe[key]==dictype)]
  contendingdbe = [key for key in dbekeys if key not in identicalkeys]    ### contending entries from dbe will not appear in the identicalkeys list

  contendingdbe = sortkeysbydate(dbe,contendingdbe)
  print contendingdbe
  delfromdbe = []
  
  delfromdbg = []
  addEdit2E = []
  i = -1
  answer = '0'
  
  for key in contendingE:                  ###  nest 2 loops for contendingE (from gcal) and contendingdbe (from the diary)
    continuetoNextContendingE = False 
    i += 1
    print "!! CONTENTION #", i, "!!!!!!!!! The following entry has been modified in both the emacs diary as well as the google calendar:" 
    print ">> gcal:",  dbg[shelve[key]['eventid']]['fullentry']
    if ENTRY_CONTENTION == 0:       # prompt from list of contenders
      if len(contendingdbe) == 0:             # if the list is empty then break to the next contendingE
        continue
                                              #sort contendingdbe list
      j = -1
      for dbekey in contendingdbe:         ### nested loop for contendingdbe
        j += 1
        print "<< diary possibility#",j,":", dbe[dbekey]['fullentry']
      if len(contendingdbe) > 1:
        answervalidated = False
        while answervalidated == False:
          answer = raw_input ("?? Which diary possibility# most likely matches in contention with the aforementioned modified gcal entry? (n for none):")
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
    if continuetoNextContendingE == True:
      continuetoNextContendingE = False
      continue
    match = int(answer)
    answervalidated = False
    while answervalidated == False:
      answer = raw_input("?? keep gcal entry (g) or emacs diary entry (e)? (b for both)")
      answer = answer.lower()
      if answer == 'g':                       ### delete dbe match entry, and add the gcal contendingE entry to the diary
        delfromG = removekey(delfromG, key)
        record = shelve.get(contendingE[i])
        eventid = record.get('eventid')
        addEdit2E = appendkey(addEdit2E, eventid) 
        addG = removekey(addG, contendingdbe[match])
        delfromdbe = appendkey(delfromdbe, contendingdbe[match])  ### delete from the diary 
        delfromdbe = appendkey(delfromdbe, contendingE[i])        ### delete from the shelve

        del dbe[contendingdbe[match]]
        del contendingdbe[match]
        
        answervalidated = True
      elif answer == 'e':                    ### delete the contendingE entry, and add the dbe match entry
        delfromG = appendkey(delfromG, contendingE[i])
        addG = appendkey(addG, contendingdbe[match])
        
        record = {}
      
        record = shelve.get(contendingE[i])
        eventid = record.get('eventid')
        ekeyschangedinG = removekey(ekeyschangedinG, contendingE[i])  ## delete from list of edited gcal entries
        gkeyschangedinG = removekey(gkeyschangedinG, eventid)
        delfromdbg = appendkey(delfromdbg, eventid)
        del contendingdbe[match]
        answervalidated = True
      elif answer == 'b' or answer == 'n':
        answervalidated = True
  return identicalkeys, delfromG,delfromdbe, addG, delfromdbg, addEdit2E, ekeyschangedinG, gkeyschangedinG

class _Getch:
    """Gets a single character from standard input."""
    def __init__(self):
        try:
            self.impl = _GetchWindows()
        except ImportError:
            self.impl = _GetchUnix()
    def __call__(self): return self.impl()

class _GetchUnix:
    def __init__(self):
        import tty, sys
    def __call__(self):
        import sys, tty, termios
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

def getpasswd():
  """Uses the _Getch class to input a string from the keyboard, masking the characters with '*', returning the string without the newline char"""
  a = 'q'
  passwd = ''
  while a != chr(13):
    a = getch() 
    passwd = passwd + a
    print '*',
  return passwd[0:len(passwd) - 1]

def gethomedir():   ## should work for windows and linux but not mac   ## OS Dependent
  try:
    from win32com.shell import shellcon, shell            
    homedir = shell.SHGetFolderPath(0, shellcon.CSIDL_APPDATA, 0, 0)
 
  except ImportError: #  non-windows/win32com case
    homedir = os.path.expanduser("~")
  return homedir

def locateEmacsDiaryLinux(homedir):
  """ find the location of the diary file by first looking for ~/diary, then try looking for the diary file location in the ~/.emacs file.  If we cant find it return None.  ## OS Dependent.  """
  defaultlocation =  globalvar_DIARYFILE
  if os.path.exists(defaultlocation):
    return defaultlocation
  elif os.path.exists( homedir + '/diary'):
    return homedir + '/diary'
  elif os.path.exists( homedir + '/.emacs'):
    f= open( homedir + '/.emacs',"r")
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


def main(argv=None):
  """Using this script without any options or arguments will syncronizes the emacs\
 and google calendars.  Optionally, the gmail user name and password may be specified as a\
rguments; if they are not, then they will be prompted upon execution.  The emacs diary fil\
e must be one directory above the directory of this script.  Use option -i to delete the shelve when you want to initialize the emacs calendar"""
  ### we are dealing with 3 databases: dbe, shelve, and dbg.   dbe is created from the diary file.  shelve was saved from the last sync and was used to write the diary file at that point in time.   dbg is created from google calendar.  using pigeon hole set logic we'll determine where to move the entries contained in these databases.
  if argv==None:
    argv=sys.argv
  homedir = gethomedir()
  emacsDiaryLocation = locateEmacsDiaryLinux(homedir)     ## OS Dependent
  if emacsDiaryLocation == None:
    print "Unable to locate the emacs diary.  Please create a file in your home directory called diary"
    return 1
  ENTRY_CONTENTION = globalvar_ENTRY_CONTENTION
  opts, args = getopt.getopt(argv[1:], "hianp", ["help","init","autocontention","nocontention","promptcontention"])
  if len(opts) > 0:
    option = opts[0][0]
    if option == "--init" or option == "-i":
      if os.path.exists("shelve.dat"):
        os.remove('shelve.dat')
    elif option == "--autocontention" or option == "-a":
      ENTRY_CONTENTION = 1
    elif option == "--nocontention" or option == "-n":
      ENTRY_CONTENTION = 2
    elif option == "--promptcontention" or option == "-p":
      ENTRY_CONTENTION = 0

    elif option == "--help" or option == "-h":
      print "Using this script without any options or arguments will syncronizes the emacs\
 and google calendars.  Optionally, the gmail user name and password may be specified as a\
rguments; if they are not, then they will be prompted upon execution.  The emacs diary fil\
e must be one directory above the directory of this script.  Use option -i to delete the\
 shelve when you want to initialize the emacs calendar"
      print
      print "Entry contention can happen when the same diary and its respective google\
 calendar entries are both modified before a sync; by default the script will interactively\
 prompt you if this occurs (use the -p option if it does not). However, you may use option -a to make an automatic best guess if\
 its unclear as to which entries are in contention (with option -a you still have to input which\
 entry to keep and which to discard). Use option -n to do nothing about contending events; this allows for both entries to\
 exist in both gcal and diary, but eliminating user interaction, and so is preferable when using a crond scheduler"
      return


  if len(args) == 2:
    gmailuser = args[0]
    gmailpasswd = args[1]
  elif len(args) == 1:
    gmailuser = args[0]
    print('enter gmail passwd:'),
    gmailpasswd = getpasswd()
  else:
    gmailuser = raw_input('enter gmail username:')
    print('enter gmail passwd:'),
    gmailpasswd = getpasswd()
 
  shelve, lastsyncG, lastsyncE = getShelveandLastSyncTimes(emacsDiaryLocation, gmailuser)
  lastmodifiedE = time.gmtime(os.stat(emacsDiaryLocation).st_mtime)  ## OS Dependent
  dbe, ap = getEmacsDiary(emacsDiaryLocation)
  dbg, gcal = getGoogleCalendar(gmailuser,gmailpasswd, lastsyncG) 
  lastmodifiedG = dbg['updated-g']
  if lastmodifiedE > lastsyncE:
    DiaryWasModified = True
  else:
    DiaryWasModified = False   
  if lastmodifiedG > lastsyncG:    
    GcalWasModified = True
  else:
    GcalWasModified = False
  ekeyschangedinG, gkeyschangedinG, g2ekeymap, editlinksmap = updateEditLinks(dbg,shelve)   # ekeyschangedinG are edited gcal entries, not newly added ones

  identicalkeys, delfromG, addG = getKeystomodifyfromE(dbe,shelve) # identicalkeys are hashkeys that are the same in both the shelve and dbe, meaning the entries are unchanged by emacs diary

  identicalkeys, delfromG,delfromE, addG, delfromdbg, addEdit2E, ekeyschangedinG, gkeyschangedinG  = handleContentions(ENTRY_CONTENTION, identicalkeys, delfromG, addG, ekeyschangedinG,gkeyschangedinG,shelve,dbg, dbe)
  delfromE, addE, addEinTermsofG, alsoaddtheseNewlyAddedGkeystoE = getKeystomodifyfromG(dbg,delfromE, shelve,identicalkeys, lastsyncG)
  
  alsoaddtheseNewlyAddedGkeystoE = removekeys(alsoaddtheseNewlyAddedGkeystoE, delfromdbg)  
  alsoaddtheseNewlyAddedGkeystoE = appendtokeys(alsoaddtheseNewlyAddedGkeystoE, addEdit2E)    

  delfromE = appendtokeys(delfromE,ekeyschangedinG)
  alsoaddtheseNewlyAddedGkeystoE = appendtokeys(alsoaddtheseNewlyAddedGkeystoE, gkeyschangedinG)

  if len(alsoaddtheseNewlyAddedGkeystoE) > 0 or len(ekeyschangedinG) > 0: #google calendar doesnt change its 'modified' date when an entry is edited, but it does change the editlink, so we check for that here
    GcalWasModified = True

  if len(delfromE) > 0 or len(addG) > 0 or len(addE) > 0 or len(alsoaddtheseNewlyAddedGkeystoE) > 0 or len(delfromG) > 0 or len(ekeyschangedinG) > 0:
    DeleteEntriesFromE(shelve,delfromE)
    DeleteEntriesFromGcal(delfromG,delfromdbg,dbg,gcal,shelve,editlinksmap, g2ekeymap)
    if DiaryWasModified:
      InsertEntriesIntoGcal(addG,dbe,gcal,shelve)
      InsertEntriesEditedbyDiarytoE(addE,dbe,shelve)
    if GcalWasModified:
      InsertEntriesIntoE(addEinTermsofG, shelve, dbg)
      InsertEntriesIntoE(alsoaddtheseNewlyAddedGkeystoE,shelve,dbg)  
    WriteEmacsDiary(emacsDiaryLocation, shelve)
  else:  
   print "-- No Changes"
  CloseShelveandMarkSyncTimes(emacsDiaryLocation,shelve,gcal)
if __name__ == '__main__':
  main()
