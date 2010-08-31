#!/usr/bin/python

import oldtemplates
from calendarsync import templates

def dictionaries_identical(old, new):
    old_keys_set = set(old.keys())
    new_keys_set = set(new.keys())
    empty_set = set()

    diff = old_keys_set - new_keys_set
    if diff != empty_set:
        print "Keys missing from new dictionary: %s" % (str(diff))
        return False

    for key in old_keys_set:
        if old[key] != new[key]:
            print "Values for key %s differ." % key
            print "\tOld value: |%s|" % old[key]
            print "\tNew value: |%s|" % new[key]
            return False

    return True


def test_e_to_g_table():
    assert dictionaries_identical(oldtemplates.e_to_g_case_table,
                                  templates.e_to_g_case_table)
    print "e_to_g_case_tables are identical."


def test_match_vars():
    for match_var in ['cases_template_mtch',
                      'detail_template_mtch',
                      'recurrence_event_descriptions_template_mtch',
                      'gcases_template_mtch',
                      'times_template_mtch', ]:
        print "Evaluating match template %s." % match_var
        assert dictionaries_identical(oldtemplates.__dict__[match_var],
                                      templates.__dict__[match_var])
        print "Match templates %s are identical." % match_var


def test_all():
    module_locals_copy = globals().copy()

    tests_to_run = [module_locals_copy[var] \
                        for var in module_locals_copy \
                        if var.startswith('test') and not var.endswith('_all')]

    for test in tests_to_run:
        test()


if __name__=='__main__':
    test_all()

