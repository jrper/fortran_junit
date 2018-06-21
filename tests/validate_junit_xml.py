#!/usr/bin/env python

from __future__ import print_function
import sys
import os

from lxml import etree

filepath = os.path.abspath(__file__)
dirname = os.path.dirname(filepath)

validator = etree.XMLSchema(file='./tests/junit.xsd') 

for i in range(1,len(sys.argv)):
    doc = etree.parse(sys.argv[i])
    try:
        validator.assertValid(doc)
        print("%s validates"%sys.argv[i])
    except etree.DocumentInvalid as e:
        print("%s invalid: %s"%(sys.argv[1], e))
