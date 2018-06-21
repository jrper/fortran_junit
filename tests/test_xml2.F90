program test

  use iso_c_binding
  use iso_fortran_env
  use xml2

  implicit none
  
  type(c_ptr) :: writer
  integer (c_int) :: err
    
  writer = xmlNewTextWriterFilename(c_wrap("test_xml.xml"), 0)
  if (.not. c_associated(writer)) write(ERROR_UNIT,*) "error creating writer."

  err = xmlTextWriterStartDocument(writer, c_wrap("1.0"), c_wrap("utf8"), c_wrap("no"))
  if (err<0) write(ERROR_UNIT,*) "error writing header"
  err = xmlTextWriterStartElement(writer, c_wrap("test_element"))
  if (err<0) write(ERROR_UNIT,*) "error writing test_element"
  err = xmlTextWriterWriteAttribute(writer, c_wrap("test_attribute"), c_wrap("1"))
  if (err<0) write(ERROR_UNIT,*) "error writing test_attribute"
  err = xmlTextWriterWriteElement(writer, c_wrap("test_write_element"), c_wrap("blah"))
  if (err<0) write(ERROR_UNIT,*) "error writing test_write_attribute"
  err = xmlTextWriterEndElement(writer)
  if (err<0) write(ERROR_UNIT,*) "error closing test_element"
  err = xmlTextWriterEndDocument(writer)
  if (err<0) write(ERROR_UNIT,*) "error closing document"
  
  call xmlFreeTextWriter(writer)

end program test
