module xml2

  use iso_c_binding

  implicit none

  type xmlTextWriter
     type(c_ptr) :: ptr
  end type xmlTextWriter

  interface xmlFreeTextWriter
     module procedure xmlFreeTextWriter_f
     
     subroutine xmlFreeTextWriter_c(writer) bind(c, name="xmlFreeTextWriter") 
       use iso_c_binding
       type(c_ptr), value :: writer
     end subroutine xmlFreeTextWriter_c
  end interface xmlFreeTextWriter

  interface xmlNewTextWriterFilename
     module procedure xmlNewTextWriterFilename_f

     function xmlNewTextWriterFilename_c(uri, compression) &
          bind(c, name="xmlNewTextWriterFilename") result(writer)
       use iso_c_binding
       character(c_char) :: uri(*)
       integer(c_int), value :: compression
       type(c_ptr) :: writer
     end function xmlNewTextWriterFilename_c
  end interface xmlNewTextWriterFilename

  interface xmlTextWriterEndDocument
     module procedure xmlTextWriterEndDocument_f

     function xmlTextWriterEndDocument_c(writer) bind(c, name="xmlTextWriterEndDocument") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       integer(c_int) :: retcode
     end function xmlTextWriterEndDocument_c
  end interface xmlTextWriterEndDocument

  interface xmlTextWriterStartDocument
     module procedure xmlTextWriterStartDocument_f

     function xmlTextWriterStartDocument_c(writer, version, encoding, standalone) &
          bind(c, name="xmlTextWriterStartDocument") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: version(*), encoding(*), standalone(*)
       integer(c_int) :: retcode
     end function xmlTextWriterStartDocument_c
  end interface

  interface xmlTextWriterStartElement
     module procedure xmlTextWriterStartElement_f

     function xmlTextWriterStartElement_c(writer, name) &
          bind(c, name="xmlTextWriterStartElement") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*)
       integer(c_int) :: retcode
     end function xmlTextWriterStartElement_c
  end interface xmlTextWriterStartElement

  interface xmlTextWriterWriteElement
     module procedure xmlTextWriterWriteElement_f

     function xmlTextWriterWriteElement_c(writer, name, val) &
          bind(c, name="xmlTextWriterWriteElement") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*), val(*)
       integer(c_int) :: retcode
     end function xmlTextWriterWriteElement_c
  end interface xmlTextWriterWriteElement

  interface xmlTextWriterEndElement
     module procedure xmlTextWriterEndElement_f

     function xmlTextWriterEndElement_c(writer) &
          bind(c, name="xmlTextWriterEndElement") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       integer(c_int) :: retcode
     end function xmlTextWriterEndElement_c
  end interface xmlTextWriterEndElement

  interface xmlTextWriterStartAttribute
     module procedure xmlTextWriterStartAttribute_f

     function xmlTextWriterStartAttribute_c(writer, name) &
          bind(c, name="xmlTextWriterStartAttribute") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*)
       integer(c_int) :: retcode
     end function xmlTextWriterStartAttribute_c
  end interface xmlTextWriterStartAttribute

  interface xmlTextWriterWriteAttribute
     module procedure xmlTextWriterWriteAttribute_f 

     function xmlTextWriterWriteAttribute_c(writer, name, val) &
          bind(c, name="xmlTextWriterWriteAttribute") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       character(c_char) :: name(*), val(*)
       integer(c_int) :: retcode
     end function xmlTextWriterWriteAttribute_c
  end interface xmlTextWriterWriteAttribute

  interface xmlTextWriterEndAttribute
     module procedure xmlTextWriterEndAttribute_f

     function xmlTextWriterEndAttribute(writer) &
          bind(c, name="xmlTextWriterEndAttribute") result(retcode)
       use iso_c_binding
       type(c_ptr), value :: writer
       integer(c_int) :: retcode
     end function xmlTextWriterEndAttribute
  end interface xmlTextWriterEndAttribute

  interface c_wrap
     module procedure c_wrap_string, c_wrap_char_array
  end interface c_wrap

  contains

    subroutine xmlFreeTextWriter_f(writer)
      type(xmlTextWriter) :: writer
      call xmlFreeTextWriter(writer%ptr)
    end subroutine xmlFreeTextWriter_f

    function xmlNewTextWriterFilename_f(uri, compression) result(writer)
      character (len=*) :: uri
      integer, intent(in) :: compression
      type(xmlTextWriter) :: writer

      writer%ptr = xmlNewTextWriterFilename(c_wrap(uri), compression) 
    end function xmlNewTextWriterFilename_f

    integer function xmlTextWriterStartDocument_f(writer, version, encoding, standalone) result(retcode)
       type(xmlTextWriter) :: writer
       character(len=*), intent(in) :: version, encoding, standalone

       retcode = xmlTextWriterStartDocument(writer%ptr, c_wrap(version), c_wrap(encoding), c_wrap(standalone))
     end function xmlTextWriterStartDocument_f

     integer function xmlTextWriterEndDocument_f(writer) result(retcode)
       type(xmlTextWriter) :: writer
       retcode = xmlTextWriterEndDocument(writer%ptr)
     end function xmlTextWriterEndDocument_f
     
     integer function xmlTextWriterStartElement_f(writer, name) result(retcode)
       type(xmlTextWriter), intent(in) :: writer
       character(len=*), intent(in) :: name

       retcode = xmlTextWriterStartElement(writer%ptr, c_wrap(name))
     end function xmlTextWriterStartElement_f

     integer function xmlTextWriterWriteElement_f(writer, name, val) result(retcode)
       use iso_c_binding
       type(xmlTextWriter), intent(in) :: writer
       character(len=*), intent(in) :: name, val

       retcode = xmlTextWriterWriteElement(writer%ptr, c_wrap(name), c_wrap(val))
     end function xmlTextWriterWriteElement_f

     integer function xmlTextWriterEndElement_f(writer) result(retcode)
       type(xmlTextWriter) :: writer
       retcode = xmlTextWriterEndDocument(writer%ptr)
     end function xmlTextWriterEndElement_f

     function xmlTextWriterStartAttribute_f(writer, name) result(retcode)
       type(xmlTextWriter), intent(in):: writer
       character(len=*), intent(in) :: name
       integer :: retcode

       retcode = xmlTextWriterStartAttribute(writer%ptr, c_wrap(name))
     end function xmlTextWriterStartAttribute_f

     integer function xmlTextWriterWriteAttribute_f(writer, name, val) result(retcode)
       type(xmlTextWriter), intent(in) :: writer
       character(len=*), intent(in) :: name, val

       retcode = xmlTextWriterWriteAttribute(writer%ptr, c_wrap(name), c_wrap(val))
     end function xmlTextWriterWriteAttribute_f

     integer function xmlTextWriterEndAttribute_f(writer) result(retcode)
       type(xmlTextWriter) :: writer
       retcode = xmlTextWriterEndAttribute(writer%ptr)
     end function xmlTextWriterEndAttribute_f

    function c_wrap_string(string) result(c_wrap)
      character(len=*), intent(in) :: string
      character :: c_wrap(len(string)+1)
      
      integer :: i

      do i = 1, len(string)
         c_wrap(i) = string(i:i)
      end do
      c_wrap(len(string)+1) = C_NULL_CHAR

    end function c_wrap_string

    function c_wrap_char_array(string) result(c_wrap)
      character, intent(in) :: string(:)
      character :: c_wrap(size(string)+1) 

      
      c_wrap(1:size(string)) = string
      c_wrap(size(string)+1:size(string)+1) = C_NULL_CHAR

    end function c_wrap_char_array

end module xml2
