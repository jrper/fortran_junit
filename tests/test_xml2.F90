#ifndef TEST_NAME
#define TEST_NAME test_xml_fortran_style
#endif

subroutine test_xml_c_style(retcode)

  use iso_c_binding
  use iso_fortran_env
  use xml2

  implicit none

  integer, intent(out) :: retcode

  type(c_ptr) :: writer
  integer (c_int) :: err

  retcode = 0
    
  writer = xmlNewTextWriterFilename(c_wrap("test_xml.xml"), 0)
  if (.not. c_associated(writer)) then
     write(ERROR_UNIT,*) "error creating writer."
     retcode = 1
  end if

  err = xmlTextWriterStartDocument(writer, c_wrap("1.0"), c_wrap("utf8"), c_wrap("no"))
  if (err<0) write(ERROR_UNIT,*) "error writing header"
  retcode = max(retcode, -err)
  err = xmlTextWriterStartElement(writer, c_wrap("test_element"))
  if (err<0) write(ERROR_UNIT,*) "error writing test_element"
  retcode = max(retcode, -err)
  err = xmlTextWriterWriteAttribute(writer, c_wrap("test_attribute"), c_wrap("1"))
  if (err<0) write(ERROR_UNIT,*) "error writing test_attribute"
  retcode = max(retcode, -err)
  err = xmlTextWriterWriteElement(writer, c_wrap("test_write_element"), c_wrap("blah"))
  if (err<0) write(ERROR_UNIT,*) "error writing test_write_attribute"
  retcode = max(retcode, -err)
  err = xmlTextWriterEndElement(writer)
  if (err<0) write(ERROR_UNIT,*) "error closing test_element"
  retcode = max(retcode, -err)
  err = xmlTextWriterEndDocument(writer)
  if (err<0) write(ERROR_UNIT,*) "error closing document"
  retcode = max(retcode, -err)
  
  call xmlFreeTextWriter(writer)

end subroutine test_xml_c_style

subroutine test_xml_fortran_style(retcode)
  use iso_c_binding
  use iso_fortran_env
  use xml2

  implicit none

  integer, intent(out) :: retcode

  type(xmlTextWriter) :: writer
  integer (c_int) :: err

  retcode = 0
    
  writer = xmlNewTextWriterFilename("test_xml.xml", 0)
  if (.not. c_associated(writer%ptr)) then
     write(ERROR_UNIT,*) "error creating writer."
     retcode = 1
  end if

  err = xmlTextWriterStartDocument(writer, "1.0", "utf8", "no")
  if (err<0) write(ERROR_UNIT,*) "error writing header"
  retcode = max(retcode, -err)
  err = xmlTextWriterStartElement(writer, "test_element")
  if (err<0) write(ERROR_UNIT,*) "error writing test_element"
  retcode = max(retcode, -err)
  err = xmlTextWriterWriteAttribute(writer, "test_attribute", "1")
  if (err<0) write(ERROR_UNIT,*) "error writing test_attribute"
  retcode = max(retcode, -err)
  err = xmlTextWriterWriteElement(writer, "test_write_element", "blah")
  if (err<0) write(ERROR_UNIT,*) "error writing test_write_attribute"
  retcode = max(retcode, -err)
  err = xmlTextWriterEndElement(writer)
  if (err<0) write(ERROR_UNIT,*) "error closing test_element"
  retcode = max(retcode, -err)
  err = xmlTextWriterEndDocument(writer)
  if (err<0) write(ERROR_UNIT,*) "error closing document"
  retcode = max(retcode, -err)
  
  call xmlFreeTextWriter(writer)

end subroutine test_xml_fortran_style

program test_runner

  implicit none

  integer :: retcode

  interface
     subroutine TEST_NAME(retcode)
       integer, intent(out) ::  retcode
     end subroutine TEST_NAME
  end interface

  call TEST_NAME(retcode)

  if (retcode>0) stop retcode

end program test_runner
