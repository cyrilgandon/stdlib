! SPDX-Identifier: MIT
module test_stringbuilder
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp
    use stdlib_string_type, only : string_type
    use stdlib_stringbuilder, only : stringbuilder_type, operator(==), operator(/=)
    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_stringbuilder(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("constructor_default", test_constructor_default), &
            new_unittest("constructor_value", test_constructor_value), &
            new_unittest("constructor_capacity", test_constructor_capacity), &
            new_unittest("append_char", test_append_char), &
            new_unittest("append_growth", test_append_growth), &
            new_unittest("append_string_type", test_append_string_type), &
            new_unittest("append_builder", test_append_builder), &
            new_unittest("append_numeric", test_append_numeric), &
            new_unittest("append_logical", test_append_logical), &
            new_unittest("append_repeat", test_append_repeat), &
            new_unittest("append_line", test_append_line), &
            new_unittest("insert", test_insert), &
            new_unittest("insert_clamp", test_insert_clamp), &
            new_unittest("remove", test_remove), &
            new_unittest("replace", test_replace), &
            new_unittest("to_string_range", test_to_string_range), &
            new_unittest("set_length", test_set_length), &
            new_unittest("clear", test_clear), &
            new_unittest("char_at", test_char_at), &
            new_unittest("equality", test_equality), &
            new_unittest("ensure_capacity", test_ensure_capacity) &
            ]
    end subroutine collect_stringbuilder

    subroutine test_constructor_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        call check(error, sb%length() == 0, "default builder should be empty")
        if (allocated(error)) return
        call check(error, sb%capacity() == 0, "default builder should have no storage")
        if (allocated(error)) return
        call check(error, sb%to_string() == "", "default builder should convert to an empty string")
    end subroutine test_constructor_default

    subroutine test_constructor_value(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type("hello")
        call check(error, sb%length() == 5, "constructor with value: wrong length")
        if (allocated(error)) return
        call check(error, sb%to_string() == "hello", "constructor with value: wrong content")
        if (allocated(error)) return
        call check(error, sb%capacity() >= 5, "constructor with value: capacity too small")
    end subroutine test_constructor_value

    subroutine test_constructor_capacity(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type(capacity=32)
        call check(error, sb%length() == 0, "constructor with capacity: should be empty")
        if (allocated(error)) return
        call check(error, sb%capacity() >= 32, "constructor with capacity: capacity too small")

        sb = stringbuilder_type("abc", capacity=100)
        call check(error, sb%to_string() == "abc", "constructor with value and capacity: wrong content")
        if (allocated(error)) return
        call check(error, sb%capacity() >= 100, "constructor with value and capacity: capacity too small")
    end subroutine test_constructor_capacity

    subroutine test_append_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        call sb%append("Hello")
        call sb%append(", ")
        call sb%append("World!")
        call check(error, sb%to_string() == "Hello, World!", "append characters: wrong content")
        if (allocated(error)) return
        call check(error, sb%length() == 13, "append characters: wrong length")
        if (allocated(error)) return

        ! Appending an empty string must be a no-op
        call sb%append("")
        call check(error, sb%to_string() == "Hello, World!", "append empty string should not change content")
    end subroutine test_append_char

    subroutine test_append_growth(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb
        integer :: i

        do i = 1, 1000
            call sb%append("abcdefghij")
        end do
        call check(error, sb%length() == 10000, "growth: wrong length after 1000 appends")
        if (allocated(error)) return
        call check(error, sb%to_string(1, 10) == "abcdefghij", "growth: wrong leading content")
        if (allocated(error)) return
        call check(error, sb%to_string(9991, 10000) == "abcdefghij", "growth: wrong trailing content")
        if (allocated(error)) return
        ! Amortized doubling should not over-allocate more than about 2x
        call check(error, sb%capacity() < 40000, "growth: capacity grew far more than expected")
    end subroutine test_append_growth

    subroutine test_append_string_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb
        type(string_type) :: str

        str = string_type("from string_type")
        call sb%append(str)
        call check(error, sb%to_string() == "from string_type", "append string_type: wrong content")
    end subroutine test_append_string_type

    subroutine test_append_builder(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb, other

        call sb%append("foo")
        call other%append("bar")
        call sb%append(other)
        call check(error, sb%to_string() == "foobar", "append builder: wrong content")
        if (allocated(error)) return
        call check(error, other%to_string() == "bar", "append builder: source must not change")
        if (allocated(error)) return

        ! Appending a builder to itself must be safe
        call sb%append(sb)
        call check(error, sb%to_string() == "foobarfoobar", "append builder to itself: wrong content")
    end subroutine test_append_builder

    subroutine test_append_numeric(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        call sb%append(42_int32)
        call sb%append("|")
        call sb%append(-7_int64)
        call sb%append("|")
        call sb%append(123_int8, "(i4)")
        call check(error, sb%to_string() == "42|-7| 123", "append integers: wrong content")
        if (allocated(error)) return

        call sb%clear()
        call sb%append(3.14159_dp, "(f5.2)")
        call sb%append("|")
        call sb%append(2.5_sp, "(f4.1)")
        call check(error, sb%to_string() == " 3.14| 2.5", "append reals: wrong content")
        if (allocated(error)) return

        call sb%clear()
        call sb%append((1.0_dp, -1.0_dp), "(f4.1)")
        call check(error, sb%to_string() == "( 1.0,-1.0)", "append complex: wrong content")
    end subroutine test_append_numeric

    subroutine test_append_logical(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        call sb%append(.true.)
        call sb%append(.false.)
        call check(error, sb%to_string() == "TF", "append logicals: wrong content")
    end subroutine test_append_logical

    subroutine test_append_repeat(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        call sb%append_repeat("ab", 3)
        call check(error, sb%to_string() == "ababab", "append_repeat: wrong content")
        if (allocated(error)) return

        ! Zero or negative counts must be no-ops
        call sb%append_repeat("x", 0)
        call sb%append_repeat("x", -2)
        call check(error, sb%to_string() == "ababab", "append_repeat with count <= 0 should not change content")
    end subroutine test_append_repeat

    subroutine test_append_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb
        character(len=1), parameter :: nl = new_line('a')

        call sb%append_line("first")
        call sb%append_line()
        call sb%append("last")
        call check(error, sb%to_string() == "first" // nl // nl // "last", "append_line: wrong content")
        if (allocated(error)) return

        call sb%clear()
        call sb%append_line(string_type("typed"))
        call check(error, sb%to_string() == "typed" // nl, "append_line with string_type: wrong content")
    end subroutine test_append_line

    subroutine test_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type("Hello World")
        call sb%insert(6, ",")
        call check(error, sb%to_string() == "Hello, World", "insert in the middle: wrong content")
        if (allocated(error)) return

        call sb%insert(1, ">> ")
        call check(error, sb%to_string() == ">> Hello, World", "insert at the head: wrong content")
        if (allocated(error)) return

        call sb%insert(sb%length() + 1, " <<")
        call check(error, sb%to_string() == ">> Hello, World <<", "insert at the tail: wrong content")
        if (allocated(error)) return

        sb = stringbuilder_type("ab")
        call sb%insert(2, 42_int32)
        call check(error, sb%to_string() == "a42b", "insert integer: wrong content")
        if (allocated(error)) return

        sb = stringbuilder_type("ab")
        call sb%insert(2, string_type("--"))
        call check(error, sb%to_string() == "a--b", "insert string_type: wrong content")
    end subroutine test_insert

    subroutine test_insert_clamp(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type("abc")
        call sb%insert(-5, "x")
        call check(error, sb%to_string() == "xabc", "insert with negative index should clamp to the head")
        if (allocated(error)) return

        call sb%insert(100, "y")
        call check(error, sb%to_string() == "xabcy", "insert past the end should clamp to the tail")
    end subroutine test_insert_clamp

    subroutine test_remove(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb
        integer :: old_capacity

        sb = stringbuilder_type("Hello, World!")
        old_capacity = sb%capacity()
        call sb%remove(6, 12)
        call check(error, sb%to_string() == "Hello!", "remove in the middle: wrong content")
        if (allocated(error)) return
        call check(error, sb%capacity() == old_capacity, "remove should not change the capacity")
        if (allocated(error)) return

        ! Out of range bounds are clamped
        call sb%remove(-3, 1)
        call check(error, sb%to_string() == "ello!", "remove with negative first should clamp")
        if (allocated(error)) return
        call sb%remove(5, 100)
        call check(error, sb%to_string() == "ello", "remove past the end should clamp")
        if (allocated(error)) return

        ! Empty or inverted ranges are no-ops
        call sb%remove(3, 2)
        call check(error, sb%to_string() == "ello", "remove with inverted range should be a no-op")
        if (allocated(error)) return

        call sb%remove(1, sb%length())
        call check(error, sb%length() == 0, "remove of the whole range should empty the builder")
    end subroutine test_remove

    subroutine test_replace(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type("one, two, one, three")
        call sb%replace("one", "1")
        call check(error, sb%to_string() == "1, two, 1, three", "replace: all occurrences should be replaced")
        if (allocated(error)) return

        call sb%replace("absent", "x")
        call check(error, sb%to_string() == "1, two, 1, three", "replace without match should be a no-op")
        if (allocated(error)) return

        call sb%replace("", "x")
        call check(error, sb%to_string() == "1, two, 1, three", "replace of an empty string should be a no-op")
        if (allocated(error)) return

        ! Replacement longer than the pattern must grow the content
        call sb%replace("1", "eleven")
        call check(error, sb%to_string() == "eleven, two, eleven, three", &
            "replace with a longer replacement: wrong content")
    end subroutine test_replace

    subroutine test_to_string_range(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type("Hello, World!")
        call check(error, sb%to_string(8, 12) == "World", "to_string range: wrong content")
        if (allocated(error)) return
        call check(error, sb%to_string(-4, 5) == "Hello", "to_string range should clamp first to 1")
        if (allocated(error)) return
        call check(error, sb%to_string(8, 100) == "World!", "to_string range should clamp last to length")
        if (allocated(error)) return
        call check(error, sb%to_string(7, 3) == "", "to_string with inverted range should be empty")
    end subroutine test_to_string_range

    subroutine test_set_length(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type("Hello, World!")
        call sb%set_length(5)
        call check(error, sb%to_string() == "Hello", "set_length should truncate the content")
        if (allocated(error)) return

        call sb%set_length(8)
        call check(error, sb%to_string() == "Hello   ", "set_length should pad with blanks")
        if (allocated(error)) return
        call check(error, sb%length() == 8, "set_length: wrong length after extension")
        if (allocated(error)) return

        call sb%set_length(-3)
        call check(error, sb%length() == 0, "set_length with a negative value should clamp to 0")
    end subroutine test_set_length

    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb
        integer :: old_capacity

        sb = stringbuilder_type("some content")
        old_capacity = sb%capacity()
        call sb%clear()
        call check(error, sb%length() == 0, "clear should empty the builder")
        if (allocated(error)) return
        call check(error, sb%capacity() == old_capacity, "clear should not change the capacity")
        if (allocated(error)) return

        call sb%append("reuse")
        call check(error, sb%to_string() == "reuse", "builder should be reusable after clear")
    end subroutine test_clear

    subroutine test_char_at(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb

        sb = stringbuilder_type("abc")
        call check(error, sb%char_at(2) == "b", "char_at: wrong character")
        if (allocated(error)) return
        call check(error, sb%char_at(0) == " ", "char_at out of range should return a blank")
        if (allocated(error)) return
        call check(error, sb%char_at(4) == " ", "char_at past the end should return a blank")
        if (allocated(error)) return

        call sb%set_char_at(2, "B")
        call check(error, sb%to_string() == "aBc", "set_char_at: wrong content")
        if (allocated(error)) return

        call sb%set_char_at(0, "x")
        call sb%set_char_at(4, "x")
        call check(error, sb%to_string() == "aBc", "set_char_at out of range should be a no-op")
    end subroutine test_char_at

    subroutine test_equality(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: lhs, rhs

        lhs = stringbuilder_type("same")
        rhs = stringbuilder_type("same")
        call check(error, lhs == rhs, "builders with the same content should be equal")
        if (allocated(error)) return

        call rhs%append(" ")
        call check(error, lhs /= rhs, "trailing blanks should be significant in comparisons")
        if (allocated(error)) return

        call check(error, lhs == "same", "builder should compare equal to its character content")
        if (allocated(error)) return
        call check(error, "same" == lhs, "character should compare equal to the builder content")
        if (allocated(error)) return
        call check(error, lhs /= "same ", "builder should not compare equal to a blank padded string")
        if (allocated(error)) return

        call lhs%clear()
        call rhs%clear()
        call check(error, lhs == rhs, "empty builders should be equal")
    end subroutine test_equality

    subroutine test_ensure_capacity(error)
        type(error_type), allocatable, intent(out) :: error
        type(stringbuilder_type) :: sb
        integer :: old_capacity

        call sb%ensure_capacity(50)
        call check(error, sb%capacity() >= 50, "ensure_capacity should grow the storage")
        if (allocated(error)) return
        call check(error, sb%length() == 0, "ensure_capacity should not change the length")
        if (allocated(error)) return

        old_capacity = sb%capacity()
        call sb%ensure_capacity(10)
        call check(error, sb%capacity() == old_capacity, "ensure_capacity should never shrink the storage")
    end subroutine test_ensure_capacity

end module test_stringbuilder


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stringbuilder, only : collect_stringbuilder
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stringbuilder", collect_stringbuilder) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
