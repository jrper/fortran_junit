module junit

  use xml2
  
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
    
    function int2cstr (i)

      !!< Convert integer i into a c string string.

      integer, intent(in) :: i
      character(len=int2str_len(i)+1) :: int2cstr

      write(int2cstr,"(i0)") i
      int2cstr=trim(int2cstr)//C_NULL_CHAR

    end function int2cstr

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

      type(c_ptr) :: writer
      integer (c_int) :: err
      integer :: i
      type(testcase), pointer :: tc
    
      writer = xmlNewTextWriterFilename(c_wrap(trim(filename)), 0)
      err = xmlTextWriterStartDocument(writer, c_wrap("1.0"),c_wrap("utf8"), c_wrap("no"))
      err = xmlTextWriterStartElement(writer, c_wrap("testsuites"))

      do i = 1, size(suites)
         err = xmlTextWriterStartElement(writer, c_wrap("testsuite"))
         err = xmlTextWriterWriteAttribute(writer, c_wrap("name"), c_wrap(suites(i)%name))
         err = xmlTextWriterWriteAttribute(writer, c_wrap("tests"), int2cstr(suites(i)%tests))
         tc => suites(i)%first
         do while (associated(tc))
            err = xmlTextWriterStartElement(writer, c_wrap("testcase"))
            err = xmlTextWriterWriteAttribute(writer, c_wrap("name"), c_wrap(tc%name))
            if (tc%failed) then
               err = xmlTextWriterStartElement(writer, c_wrap("failure"))
               if (allocated(tc%message)) &
                    err = xmlTextWriterWriteAttribute(writer, c_wrap("message"), c_wrap(tc%message))
               if (allocated(tc%type)) &
                    err = xmlTextWriterWriteAttribute(writer, c_wrap("type"), c_wrap(tc%type))
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
