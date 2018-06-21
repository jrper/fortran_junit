.phony: clean

default: lib/libfjunit.a

AR = ar
ARFLAGS	= -cr

FC = gfortran
FCFLAGS = -Jinclude

%.o: %.mod

clean:
	rm -rf bin lib src/*.o include/*

src/%.o: src/%.F90
	mkdir -p include
	${FC} ${FCFLAGS} -c $< -o $@

bin/%: tests/%.F90 lib/libfjunit.a
	mkdir -p bin
	${FC} ${FCFLAGS} -lxml2 -Llib -lfjunit $< -o $@

bin/validate_junit_xml:
	cp tests/validate_junit_xml.py $@

tests: bin/test_xml2 bin/test_junit bin/validate_junit_xml

src/junit.o: src/xml2.o	

lib/libfjunit.a: src/junit.o
	mkdir -p lib
	$(AR) $(ARFLAGS) lib/libfjunit.a src/*.o
