#!/usr/bin/python 
# Simple Emacs-Google Calendar Sync
# written and maintained by CiscoRx@gmail.com


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
from pprint import pprint

globalvar_DIARYFILE = './../diary'            # Location of emacs diary 
globalvar_DEFAULTEVENTDURATION = 60           # If no end time default to 60 min
globalvar_TZID = 'America/Chicago'            # Time zone
globalvar_GMTOFFSET = 6                       

def stripallarray(aTarget):
  for i in range(len(aTarget)):
    aTarget[i] = aTarget[i].strip()
  return aTarget

def PadNewlinesWithSpace(string):
  """ This function is used on the CONTENT field so that when written to the emacs diary, multiple lines of description can be recognized as pertaining to a single respective event.  """
  lines = string.split('\n')
  if len(lines) < 2:
    return string
  alllinesbutfirst = lines[1:]
  target = []
  target.append(lines[0] + '\n')
  for line in alllinesbutfirst:
    if len(line) > 0 and line[0] != ' ':
      target.append(' ' + line + '\n')
  return ''.join(target)

def StripExtraNewLines(string):
  """ Use this function on the 'fullentry' field before hashing to get a key, as sometimes new lines can get into the diary and mess up the hash""" 
  pos = string.find('\n\n')
  while  pos != -1:
    string = string.replace('\n\n','\n')
    pos = string.find('\n\n')
  return string

def escstring(string):   
  """ Use this function in place of re.escape(); re.escape() does not seem to work right..."""
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
  """ loads simple one level dictionary from a file. The file must be tab separated and end in a new line"""
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
    if 'BYDAY' in reckeys:
      db[dbkey]['BYDAYG'] = e2gbyday(db[dbkey]['BYDAY'])
    if 'WHICHWEEK' in reckeys:
      whichweek = int(db[dbkey]['WHICHWEEK'])
      if whichweek == -1:
        whichweekg = '4'
      else:
        whichweekg = db[dbkey]['WHICHWEEK'].strip()
      db[dbkey]['WHICHWEEKG'] = whichweekg + daysofweek[int(db[dbkey]['DAYOFWEEK'])]
    if 'DAYOFWEEK' in reckeys:
      db[dbkey]['BYDAYG'] = db[dbkey]['DAYOFWEEK'].upper()[:2]
    if 'DAYOFWEEKABBR' in reckeys:
      db[dbkey]['BYDAYG'] = db[dbkey]['DAYOFWEEKABBR'].upper()[:2]
    if 'MONTHABBR' in reckeys:
      db[dbkey]['STMONTH'] = e2gmonthabbr(db[dbkey]['MONTHABBR'])
    if 'STDAYNOTFOLLOWEDBYCOMMA' in reckeys:
      db[dbkey]['STDAY'] = db[dbkey]['STDAYNOTFOLLOWEDBYCOMMA']
      reckeys.append('STDAY')

    
    db[dbkey]['STMONTH'] = db[dbkey].setdefault('STMONTH',str(nowmonth)).zfill(2)
    db[dbkey]['STDAY']= db[dbkey].setdefault('STDAY',str(nowday)).zfill(2)
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
    if len(gcase) > 7 and gcase[:7] == 'caseRec':
      recurrencestring = gcases_template[gcase] % db[dbkey]
      db[dbkey]['recurrencestring'] = recurrencestring
    

def getEmacsDiary():
  db={}
  ap = loadTemplate('cases_template')
  pat,sp = EvaluateTemplates(ap, 'cases_template_mtch')
  f=open(globalvar_DIARYFILE, "r") 
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
      fullentry = StripExtraNewLines(file[mo.start(0):mo.end(0)] + '\n')
      entry['fullentry'] = fullentry
      #entrypid = str(mo.start(0)).zfill(8)
      entrypid = str(hash(fullentry))
      keys.append(entrypid)
      details.append(entry['DETAIL'])
      entry['entrypid'] = entrypid
      entry['entrycase'] = idxCase
      entry['gcase'] = e2gcase_table[idxCase]
      db[entrypid] = entry
      mo= pat[idxCase].search(file,mo.end(0))
  updateDetails(db, details, keys)
  HandleLooseEmacsEnds(db)
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


def getGoogleCalendar(username,passwd,time_min):
  at = loadTemplate('times_template', Escape = False)
  ap = loadTemplate('cases_template', Escape = False)
  nowdatetime = sCurrentDatetime()
  recurrences = []
  recurrencekeys = []
  #gcal = CalendarExample(username, passwd)
  db={}
  gcal = gdata.calendar.service.CalendarService()
  gcal.email = username
  gcal.password = passwd
  gcal.source = 'Google-Emacs-Calendar-Sync-1.0'
  gcal.ProgrammaticLogin()

  query = gdata.calendar.service.CalendarEventQuery('default', 'private', 
        'full')
  query.start_min = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time_min)
  query.start_max = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime(time.time() + 28800000))
  #query.updated_min = start_date
  query.ctz = globalvar_TZID
  query.max_results = 200
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
    content = PadNewlinesWithSpace(content)
    entry['CONTENT'] = content

    entry['modified'] = time.strptime(an_event.updated.text[:19],'%Y-%m-%dT%H:%M:%S')
    entry['editlink'] = an_event.GetEditLink().href
    if an_event.recurrence != None:
      entry['recurrence_raw']= an_event.recurrence.text
      recurrences.append(an_event.recurrence.text)
      recurrencekeys.append(entrypid)
    else:
      stdatetime = striptime(an_event.when[0].start_time)
      enddatetime = striptime(an_event.when[0].end_time)
      entry['STYEAR'] = stdatetime[0:4]
      entry['STMONTH'] = stdatetime[4:6]
      entry['STDAY'] = stdatetime[6:8]
      entry['ENDYEAR'] = enddatetime[0:4]
      entry['ENDMONTH'] = enddatetime[4:6]
      entry['ENDDAY'] = enddatetime[6:8]
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
        entry['DETAIL'] = entry['TITLE'] + ' ' + at['caseTimeARange'] % entry + ' ' + entry['CONTENT']
      else:
        entry['DETAIL'] = entry['TITLE'] + ' ' + entry['CONTENT']
      entry['fullentry'] =  StripExtraNewLines(entry['STMONTH'] + '/' + entry['STDAY'] + '/' + entry['ENDYEAR'] + ' ' + entry['DETAIL'] + '\n' )
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
    if len(bydayg) > 0 and bydayg[0].isdigit() == True:
      generalcase = 'bydayofweek'
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
    db[recurrencekeys[i]]['fullentry'] = StripExtraNewLines(recurrencestring + '\n') 
  return db, gcal

def getKeystomodifyfromE(db1,db2):
  """ Returns some arrays of keys that are to be inserted into or deleted from Gcal.  Any edited entries are deleted and reinserted """
  dict = {}
  dictype = type(dict) 
  keys1 = [key for key in db1.keys() if type(db1[key])==dictype]
  keys2 = [key for key in db2.keys() if type(db2[key])==dictype]

  identicalkeys = [key for key in keys1 if key in keys2]  
  
  delfromG = [key for key in keys2 if key not in identicalkeys]
  addtoG = [key for key in keys1 if key not in identicalkeys]
  return identicalkeys, delfromG, addtoG

def getKeystomodifyfromG(dbg,dbshelf,identicalkeys, glastsynctime):
  """ Returns some arrays of keys that are to be inserted into or deleted from the emacs Diary.  Any edited entries are deleted and reinserted """
  
  dict = {}
  dictype = type(dict) 
  gkeys = [key for key in dbg.keys() if type(dbg[key])==dictype]
  skeys = [key for key in dbshelf.keys() if type(dbshelf[key])==dictype]
  skeyeventids = [dbshelf[key].get('eventid') for key in skeys]
  delfromE = [key for key in identicalkeys if dbshelf[key].get('eventid') not in gkeys]
  addE = [key for key in identicalkeys if key not in delfromE and dbg[dbshelf[key]['eventid']].get('modified') > glastsynctime]

  addEinTermsofGkeys = [dbshelf[key]['eventid'] for key in identicalkeys if key not in delfromE and dbg[dbshelf[key]['eventid']].get('modified') > glastsynctime]
   
  delfromE += addE                    #instead of editing simply delete and create a new entry
  alsoaddtheseGkeystoE = [key for key in gkeys if key not in skeyeventids]
  return delfromE, addE, addEinTermsofGkeys, alsoaddtheseGkeystoE
  

def getShelveandLastSyncTimes():
  lastmodifiedg = time.strptime('1995-1-1T12:00:00','%Y-%m-%dT%H:%M:%S')
  lastmodifiede = time.gmtime(os.stat(globalvar_DIARYFILE).st_mtime)
  f=shelve.open('shelve.dat')
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
  event.content = atom.Content(text=entry.get('CONTENT'))
#    event.where.append(gdata.calendar.Where(value_string=event['WHERE']))

  if 'recurrencestring' in entry:
    event.recurrence = gdata.calendar.Recurrence(text=entry['recurrencestring'])
  else:
    event.recurrence = None
    timetuple_dtstart = entry['timetuple_dtstart']
    timetuple_dtstart = convertTimetuple2GMT(timetuple_dtstart)
    timetuple_dtend = entry['timetuple_dtend']
    timetuple_dtend = convertTimetuple2GMT(timetuple_dtend)
       
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
    print "inserted from Diary to Gcal: " + shelve[key]['fullentry'] + '\n'

def DeleteEntriesFromE(shelve,delfromE):
  for key in delfromE:
    print "deleted from Diary: " + shelve[key]['fullentry'] + '\n'
    del shelve[key]

def DeleteEntriesFromGcal(delG,dbg,gcal,shelve):
  eventids = [shelve[ekey]['eventid'] for ekey in delG]
  editlinks = [dbg[gkey]['editlink'] for gkey in eventids]
  for editlink in editlinks:
    gcal.DeleteEvent(editlink)

  for key in delG:
    print "deleted from Gcal and Diary: " + shelve[key]['fullentry']+'\n'
    del shelve[key]


def InsertEntriesEditedbyDiarytoE(addE,dbe,shelve):
  for key in addE:
    shelve[key] = dbe[key].copy()
    print "insert edit into Diary: " + shelve[key]['fullentry'] + '\n'
    

def InsertEntriesIntoE(addGkeystoE, shelve,dbg):
  for gkey in addGkeystoE:
    entrypid = str(hash(dbg[gkey]['fullentry']))
    dbg['entrypid'] = entrypid
    shelve[entrypid] = dbg[gkey].copy()
    print "inserted to Diary: " + dbg[gkey]['fullentry'] +'\n'


def WriteEmacsDiary(shelve):
  dict = {}
  dictype = type(dict) 
  keys = [key for key in shelve.keys() if type(shelve[key])==dictype]
  f = open(globalvar_DIARYFILE,'w')
  f.seek(0)
  for key in keys: 
    f.write(shelve[key].get('fullentry') + '\n')  
  f.close()

def CloseShelveandMarkSyncTimes(gmailuser,gmailpasswd,shelve,gcal):
  #del gcal
  #gcal = gdata.calendar.service.CalendarService()
  #gcal.email = gmailuser
  #gcal.password = gmailpasswd
  #gcal.source = 'Google-Emacs-Calendar-Sync-1.0'
  #gcal.ProgrammaticLogin()

  query = gdata.calendar.service.CalendarEventQuery('default', 'private', 
        'full')
  query.start_min = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime())
  query.start_max = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime())
  #query.updated_min = start_date
  feed = gcal.CalendarQuery(query)
 
  shelve['updated-e'] = time.gmtime(os.stat(globalvar_DIARYFILE).st_mtime)
  shelve['updated-g'] = time.strptime(feed.updated.text,'%Y-%m-%dT%H:%M:%S.000Z')

  del gcal
  shelve.close()
  

class _Getch:
    """Gets a single character from standard input.  Does not echo to the
screen."""
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
  "Uses the _Getch class to input a string from the keyboard, masking the characters with '*', returning the string without the newline char"
  a = 'q'
  passwd = ''
  while a != chr(13):
    a = getch() 
    passwd = passwd + a
    print '*',
  return passwd[0:len(passwd) - 1]

def main(argv=None):
  "Using this script without any options or arguments will syncronizes the emacs\
 and google calendars.  Optionally, the gmail user name and password may be specified as a\
rguments; if they are not, then they will be prompted upon execution.  The emacs diary fil\
e must be one directory above the directory of this script.  Use option -i to delete the shelve when you want to initialize the emacs calendar"

  
  if argv==None:
    argv=sys.argv
  opts, args = getopt.getopt(argv[1:], "hi", ["help","init"])
  if len(opts) > 0:
    option = opts[0][0]
    if option == "--init" or option == "-i":
      if os.path.exists("shelve.dat"):
        os.remove('shelve.dat')
    elif option == "--help" or option == "-h":
      print "Using this script without any options or arguments will syncronizes the emacs\
 and google calendars.  Optionally, the gmail user name and password may be specified as a\
rguments; if they are not, then they will be prompted upon execution.  The emacs diary fil\
e must be one directory above the directory of this script.  Use option -i to delete the shelve when you want to initialize the emacs calendar"


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
 
  shelve, lastsyncG, lastsyncE = getShelveandLastSyncTimes()
  lastmodifiedE = time.gmtime(os.stat(globalvar_DIARYFILE).st_mtime)
  dbe, ap = getEmacsDiary()
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

  identicalkeys, delfromG, addG = getKeystomodifyfromE(dbe,shelve)

  delfromE, addE, addEinTermsofG, alsoaddtheseGkeystoE = getKeystomodifyfromG(dbg,shelve,identicalkeys, lastsyncG)

  if len(delfromE) > 0 or len(addG) > 0 or len(addE) > 0 or len(alsoaddtheseGkeystoE) > 0 or len(delfromG) > 0:
    DeleteEntriesFromE(shelve,delfromE)
    DeleteEntriesFromGcal(delfromG,dbg,gcal,shelve)
    if DiaryWasModified:
      InsertEntriesIntoGcal(addG,dbe,gcal,shelve)
      InsertEntriesEditedbyDiarytoE(addE,dbe,shelve)
    if GcalWasModified:
      InsertEntriesIntoE(addEinTermsofG, shelve, dbg)
      InsertEntriesIntoE(alsoaddtheseGkeystoE,shelve,dbg)  
    WriteEmacsDiary(shelve)
  else:  
   print "No Changes"
  CloseShelveandMarkSyncTimes(gmailuser,gmailpasswd,shelve,gcal)
if __name__ == '__main__':
  main()
