program test

  use iso_c_binding
  use xml2

  implicit none
  
  type(c_ptr) :: writer
  integer (c_int) :: err
    
  writer = xmlNewTextWriterFilename(c_wrap("test.xml"), 0)

  err = xmlTextWriterStartDocument(writer, c_wrap("1.0"),c_wrap("utf8"), c_wrap("no"))

  err = xmlTextWriterStartElement(writer, c_wrap("testsuites"))
  err = xmlTextWriterWriteAttribute(writer, c_wrap("tests"), c_wrap("1"))
  err = xmlTextWriterStartElement(writer, c_wrap("testsuite"))
  err = xmlTextWriterWriteAttribute(writer, c_wrap("name"), c_wrap("libxml2f"))
  err = xmlTextWriterWriteAttribute(writer, c_wrap("tests"), c_wrap("1"))
  err = xmlTextWriterStartElement(writer, c_wrap("testcase"))
  err = xmlTextWriterWriteAttribute(writer, c_wrap("name"), c_wrap("test1"))
  err = xmlTextWriterWriteAttribute(writer, c_wrap("classname"), c_wrap("libxml2f.test1"))
  err = xmlTextWriterEndElement(writer)
  err = xmlTextWriterEndElement(writer)

  err = xmlTextWriterEndDocument(writer)
  
  call xmlFreeTextWriter(writer)

end program test
