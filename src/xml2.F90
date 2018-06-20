module xml2

  use iso_c_binding

  implicit none

  interface 
     subroutine xmlFreeTextWriter(writer) bind(c, name="xmlFreeTextWriter") 
       use iso_c_binding
       type(c_ptr), value :: writer
     end subroutine xmlFreeTextWriter

     function xmlNewTextWriterFilename(uri, compression) bind(c, name="xmlNewTextWriterFilename") result(writer)
       use iso_c_binding
       character(c_char) :: uri(*)
       integer(c_int), value :: compression
       type(c_ptr) :: writer
     end function xmlNewTextWriterFilename

     function xmlTextWriterEndDocument(writer) bind(c, name="xmlTextWriterEndDocument") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       integer(c_int) :: retcode
     end function xmlTextWriterEndDocument

     function xmlTextWriterStartDocument(writer, version, encoding, standalone) &
          bind(c, name="xmlTextWriterStartDocument") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: version(*), encoding(*), standalone(*)
       integer(c_int) :: retcode
     end function xmlTextWriterStartDocument       

     function xmlTextWriterStartElement(writer, name) &
          bind(c, name="xmlTextWriterStartElement") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*)
       integer(c_int) :: retcode
     end function xmlTextWriterStartElement

     function xmlTextWriterWriteElement(writer, name, val) &
          bind(c, name="xmlTextWriterWriteElement") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*), val(*)
       integer(c_int) :: retcode
     end function xmlTextWriterWriteElement

     function xmlTextWriterEndElement(writer) &
          bind(c, name="xmlTextWriterEndElement") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       integer(c_int) :: retcode
     end function xmlTextWriterEndElement

     function xmlTextWriterStartAttribute(writer, name) &
          bind(c, name="xmlTextWriterStartAttribute") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*)
       integer(c_int) :: retcode
     end function xmlTextWriterStartAttribute

     function xmlTextWriterWriteAttribute(writer, name, val) &
          bind(c, name="xmlTextWriterWriteAttribute") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*), val(*)
       integer(c_int) :: retcode
     end function xmlTextWriterWriteAttribute

     function xmlTextWriterEndAttribute(writer) &
          bind(c, name="xmlTextWriterEndAttribute") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       integer(c_int) :: retcode
     end function xmlTextWriterEndAttribute

  end interface

  interface c_wrap
     module procedure c_wrap_string, c_wrap_char_array
  end interface c_wrap

  public c_wrap

  contains

    function c_wrap_string(string) result(c_wrap)
      character(len=*), intent(in) :: string
      character(len=len(string)+1) c_wrap
      
      c_wrap = string//C_NULL_CHAR

    end function c_wrap_string

    function c_wrap_char_array(string) result(c_wrap)
      character, intent(in) :: string(:)
      character(len=size(string)+1) c_wrap
      
      integer :: i

      do i=1, size(string)
         c_wrap(i:i) = string(i)
      end do
      i = len(c_wrap)
      c_wrap(i:i) = C_NULL_CHAR

    end function c_wrap_char_array

end module xml2
