program test

  use junit

  implicit none

  type(testsuite) :: ts(1)
  
  call initialise_testsuite(ts(1), "junit")
  call add_testcase(ts(1), "add_testcase")
  call add_testcase(ts(1), "mark_failures")
  call mark_as_failure(ts(1)%last)
  call write_testsuites_to_file(ts, "test.xml")
  call free_testsuite(ts(1))

end program test
