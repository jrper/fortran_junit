module junit

  use xml2

  implicit none
  
  private

  private :: testcase

  type testcase
     character, allocatable :: name(:)
     integer :: assertions
     real    :: time
     character, allocatable :: classname(:)
     character, allocatable :: status(:)
     type(testcase), pointer :: next => null()
     logical :: failed
     character, allocatable :: message(:)
     character, allocatable :: type(:)

  end type testcase

  type testsuite
     character, allocatable :: name(:)
     integer :: tests
     integer :: failures
     integer :: errors
     real    :: time
     integer :: disabled
     integer :: skipped

     type(testcase), pointer :: first=>null()
     type(testcase), pointer :: last=>null()

  end type testsuite

  interface set

     module procedure set_integer_with_default, set_real_with_default

  end interface set

  public :: testsuite, initialise_testsuite, add_testcase, write_testsuites_to_file, &
       free_testsuite, mark_as_failure

  contains
    
    subroutine set_integer_with_default(x, val, default)
      integer, intent(inout) :: x
      integer, intent(in), optional :: val
      integer, intent(in) :: default

      if (present(val)) then
         x = val
      else
         x = default
      end if
    end subroutine set_integer_with_default

    subroutine set_real_with_default(x, val, default)
      real, intent(inout) :: x
      real, intent(in), optional :: val
      real, intent(in) :: default

      if (present(val)) then
         x = val
      else
         x = default
      end if
    end subroutine set_real_with_default

    function str(a)
      character, dimension(:) :: a
      character(len=size(a)) :: str

      integer :: i
      do i=1, size(a)
         str(i:i) = a(i)
      end do
    end function str

    subroutine copy2chars(s, a)
      character (len=*), intent(in) :: s
      character, allocatable, intent(out) :: a(:)

      integer :: i

      allocate(a(len(s)))

      do i=1, len(s)
         a(i) = s(i:i)
      end do
    end subroutine copy2chars

    pure function int2str_len(i)

      !!< Count number of digits in i.

      integer, intent(in) :: i
      integer :: int2str_len 
      
      int2str_len = 1
      if (i/=0) int2str_len = int2str_len + floor(log10(abs(real(i))))
      if (i<0) int2str_len = int2str_len + 1

    end function int2str_len
    
    function int2str (i)

      !!< Convert integer i into a c string string.

      integer, intent(in) :: i
      character(len=int2str_len(i)+1) :: int2str

      write(int2str,"(i0)") i

    end function int2str

    subroutine initialise_testsuite(suite, name)

      type(testsuite) :: suite
      character(len=*) :: name

      call copy2chars(name, suite%name)
      suite%tests = 0
      suite%failures = 0
      suite%errors = 0
      suite%time = 0.0
      suite%disabled = 0
      suite%skipped = 0

      nullify(suite%first)
      nullify(suite%last)

    end subroutine initialise_testsuite

    subroutine add_testcase(suite, name, assertions, time)
      type(testsuite), intent(inout) :: suite
      character(len=*) :: name
      integer, optional, intent(in) :: assertions
      real, optional, intent(in) :: time

      type(testcase), pointer :: test

      allocate(test)
      call copy2chars(name, test%name)
      call set(test%assertions, assertions, 0)
      call set(test%time, time, 0.0)
      test%failed = .false.

      if (associated(suite%last)) then
         suite%last%next => test
      else
         suite%first => test
      end if
      suite%last => test
      suite%tests = suite%tests +1

    end subroutine add_testcase

    subroutine mark_as_failure(test, message, type)
      type(testcase), pointer, intent(inout) :: test
      character(len=*), intent(in), optional :: message, type
      
      test%failed = .true.
      if (present(message)) call copy2chars(message, test%message)
      if (present(type)) call copy2chars(type, test%type)

    end subroutine mark_as_failure

    subroutine free_testcase(test)
      type(testcase), pointer, intent(in) :: test

      if (allocated(test%name)) deallocate(test%name)
      if (allocated(test%message)) deallocate(test%message)      
      if (allocated(test%type)) deallocate(test%type) 

      nullify(test%next)

    end subroutine free_testcase
    
    subroutine free_testsuite(suite)

      type(testsuite), intent(inout) :: suite

      type(testcase), pointer :: this, next

      this => suite%first
      
      do while (associated(this))
         next => this%next
         call free_testcase(this)
         deallocate(this)
         this => next
      end do

      nullify(suite%first)
      nullify(suite%last)

      if (allocated(suite%name)) deallocate(suite%name)      

    end subroutine free_testsuite

    subroutine write_testsuites_to_file(suites, filename)
      type(testsuite), dimension(:), intent(inout) :: suites
      character (len=*) :: filename

      type(xmlTextWriter) :: writer

      integer :: err
      integer :: i
      type(testcase), pointer :: tc
    
      writer = xmlNewTextWriterFilename(filename, 0)
      err = xmlTextWriterStartDocument(writer, "1.0","utf8", "no")
      err = xmlTextWriterStartElement(writer, "testsuites")

      do i = 1, size(suites)
         err = xmlTextWriterStartElement(writer, "testsuite")
         err = xmlTextWriterWriteAttribute(writer, "name", str(suites(i)%name))
         err = xmlTextWriterWriteAttribute(writer, "tests", int2str(suites(i)%tests))
         tc => suites(i)%first
         do while (associated(tc))
            err = xmlTextWriterStartElement(writer, "testcase")
            err = xmlTextWriterWriteAttribute(writer, "name", str(tc%name))
            if (tc%failed) then
               err = xmlTextWriterStartElement(writer, "failure")
               if (allocated(tc%message)) &
                    err = xmlTextWriterWriteAttribute(writer, "message", str(tc%message))
               if (allocated(tc%type)) &
                    err = xmlTextWriterWriteAttribute(writer, "type", str(tc%type))
               err = xmlTextWriterEndElement(writer)
            end if
            err = xmlTextWriterEndElement(writer)
            tc => tc%next 
         end do
         err = xmlTextWriterEndElement(writer)
         err = xmlTextWriterEndDocument(writer)
      end do

    end subroutine write_testsuites_to_file




end module junit
