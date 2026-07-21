! SPDX-Identifier: MIT
!> Version: experimental
!>
!> This module provides `stringbuilder_type`, a mutable character buffer used to
!> build strings efficiently, ported from the .NET `System.Text.StringBuilder`
!> class. Instead of the linked list of chunks used by the original
!> implementation, the Fortran port relies on a single allocatable character
!> buffer with amortized growth (the capacity is at least doubled whenever the
!> buffer must be reallocated), which gives the same O(1) amortized append cost.
!>
!> Differences with the .NET class, chosen to stay idiomatic in Fortran:
!> - all indices are 1-based;
!> - ranges are expressed as `(first, last)` positions instead of
!>   `(startIndex, count)`;
!> - invalid indices or ranges are clamped to the valid range instead of
!>   raising exceptions;
!> - `set_length` pads with blanks instead of null characters;
!> - comparison of two builders (or a builder and a character string) is exact:
!>   trailing blanks are significant, unlike the intrinsic character `==`.
module stdlib_stringbuilder
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp
    use stdlib_string_type, only: string_type, char
    use stdlib_strings, only: to_string, replace_all
    implicit none
    private

    public :: stringbuilder_type
    public :: operator(==), operator(/=)

    !> Capacity allocated by default when the first character is appended
    integer, parameter :: default_capacity = 16

    !> Version: experimental
    !>
    !> Mutable buffer to build character strings efficiently
    type :: stringbuilder_type
        private
        !> Internal storage; its declared length is the capacity of the builder
        character(len=:), allocatable :: buffer
        !> Number of leading characters of `buffer` actually in use
        integer :: len_used = 0
    contains
        private

        procedure :: append_char
        procedure :: append_string
        procedure :: append_builder
        procedure :: append_int8
        procedure :: append_int16
        procedure :: append_int32
        procedure :: append_int64
        procedure :: append_real_sp
        procedure :: append_real_dp
        procedure :: append_complex_sp
        procedure :: append_complex_dp
        procedure :: append_logical
        !> Version: experimental
        !>
        !> Appends the representation of the value at the end of the builder.
        !> Numeric and logical values are converted with `to_string`, which
        !> accepts an optional edit descriptor (e.g. `"(f8.3)"`).
        generic, public :: append => append_char, append_string, append_builder, &
            append_int8, append_int16, append_int32, append_int64, &
            append_real_sp, append_real_dp, append_complex_sp, append_complex_dp, &
            append_logical

        !> Version: experimental
        !>
        !> Appends `count` copies of a character string at the end of the builder
        procedure, public :: append_repeat

        procedure :: append_line_char
        procedure :: append_line_string
        !> Version: experimental
        !>
        !> Appends an optional value followed by the default line terminator
        generic, public :: append_line => append_line_char, append_line_string

        procedure :: insert_char
        procedure :: insert_string
        procedure :: insert_int32
        procedure :: insert_int64
        procedure :: insert_real_sp
        procedure :: insert_real_dp
        procedure :: insert_logical
        !> Version: experimental
        !>
        !> Inserts the representation of the value at position `index`;
        !> existing characters are shifted to make room. `index` is clamped
        !> to `[1, length + 1]`.
        generic, public :: insert => insert_char, insert_string, insert_int32, &
            insert_int64, insert_real_sp, insert_real_dp, insert_logical

        procedure :: to_string_all
        procedure :: to_string_range
        !> Version: experimental
        !>
        !> Returns the content of the builder, or of the range `(first, last)`,
        !> as a character string
        generic, public :: to_string => to_string_all, to_string_range

        !> Version: experimental
        !>
        !> Removes the characters in the range `(first, last)`; the capacity
        !> is unaffected
        procedure, public :: remove

        !> Version: experimental
        !>
        !> Replaces all the occurrences of a substring with another one
        procedure, public :: replace

        !> Version: experimental
        !>
        !> Returns the number of characters currently in the builder
        procedure, public :: length

        !> Version: experimental
        !>
        !> Truncates the content, or extends it with blanks, to the given length
        procedure, public :: set_length

        !> Version: experimental
        !>
        !> Returns the number of characters the builder can hold without
        !> reallocating
        procedure, public :: capacity

        !> Version: experimental
        !>
        !> Ensures that the capacity is at least the given value
        procedure, public :: ensure_capacity

        !> Version: experimental
        !>
        !> Resets the builder to an empty content; the capacity is unaffected
        procedure, public :: clear

        !> Version: experimental
        !>
        !> Returns the character at the given position, or a blank if the
        !> position is out of range
        procedure, public :: char_at

        !> Version: experimental
        !>
        !> Sets the character at the given position; out of range positions
        !> are ignored
        procedure, public :: set_char_at

        procedure :: grow
    end type stringbuilder_type

    !> Version: experimental
    !>
    !> Constructor for stringbuilder_type, with an optional initial value
    !> and an optional initial capacity
    interface stringbuilder_type
        module procedure new_stringbuilder
    end interface stringbuilder_type

    !> Version: experimental
    !>
    !> Compares the content of two builders, or of a builder and a character
    !> string, for exact equality (trailing blanks are significant)
    interface operator(==)
        module procedure builder_eq_builder
        module procedure builder_eq_char
        module procedure char_eq_builder
    end interface operator(==)

    !> Version: experimental
    !>
    !> Compares the content of two builders, or of a builder and a character
    !> string, for inequality
    interface operator(/=)
        module procedure builder_ne_builder
        module procedure builder_ne_char
        module procedure char_ne_builder
    end interface operator(/=)

contains

    !> Creates a new builder, optionally initialized with `value`, with a
    !> capacity of at least `capacity` characters
    pure function new_stringbuilder(value, capacity) result(new)
        character(len=*), intent(in), optional :: value
        integer, intent(in), optional :: capacity
        type(stringbuilder_type) :: new

        if (present(capacity)) then
            if (capacity > 0) call new%grow(capacity)
        end if
        if (present(value)) call new%append_char(value)
    end function new_stringbuilder

    !> Ensures that the internal buffer can hold at least `required`
    !> characters, reallocating with amortized doubling when needed
    pure subroutine grow(self, required)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: required
        character(len=:), allocatable :: tmp
        integer :: new_capacity

        if (.not. allocated(self%buffer)) then
            new_capacity = max(required, default_capacity)
            allocate(character(len=new_capacity) :: self%buffer)
            return
        end if
        if (len(self%buffer) >= required) return

        new_capacity = max(required, 2*len(self%buffer))
        allocate(character(len=new_capacity) :: tmp)
        tmp(1:self%len_used) = self%buffer(1:self%len_used)
        call move_alloc(tmp, self%buffer)
    end subroutine grow

    pure subroutine append_char(self, value)
        class(stringbuilder_type), intent(inout) :: self
        character(len=*), intent(in) :: value

        if (len(value) == 0) return
        call self%grow(self%len_used + len(value))
        self%buffer(self%len_used + 1:self%len_used + len(value)) = value
        self%len_used = self%len_used + len(value)
    end subroutine append_char

    pure subroutine append_string(self, value)
        class(stringbuilder_type), intent(inout) :: self
        type(string_type), intent(in) :: value

        call self%append_char(char(value))
    end subroutine append_string

    pure subroutine append_builder(self, value)
        class(stringbuilder_type), intent(inout) :: self
        type(stringbuilder_type), intent(in) :: value
        ! Copy needed: appending a builder to itself would otherwise pass a
        ! reference into the very buffer that grow may reallocate
        character(len=:), allocatable :: tmp

        if (value%len_used == 0) return
        tmp = value%buffer(1:value%len_used)
        call self%append_char(tmp)
    end subroutine append_builder

    pure subroutine append_int8(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        integer(int8), intent(in) :: value
        character(len=*), intent(in), optional :: format

        ! to_string has no optional format for integers, hence the branching
        if (present(format)) then
            call self%append_char(to_string(value, format))
        else
            call self%append_char(to_string(value))
        end if
    end subroutine append_int8

    pure subroutine append_int16(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        integer(int16), intent(in) :: value
        character(len=*), intent(in), optional :: format

        if (present(format)) then
            call self%append_char(to_string(value, format))
        else
            call self%append_char(to_string(value))
        end if
    end subroutine append_int16

    pure subroutine append_int32(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        integer(int32), intent(in) :: value
        character(len=*), intent(in), optional :: format

        if (present(format)) then
            call self%append_char(to_string(value, format))
        else
            call self%append_char(to_string(value))
        end if
    end subroutine append_int32

    pure subroutine append_int64(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        integer(int64), intent(in) :: value
        character(len=*), intent(in), optional :: format

        if (present(format)) then
            call self%append_char(to_string(value, format))
        else
            call self%append_char(to_string(value))
        end if
    end subroutine append_int64

    pure subroutine append_real_sp(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        real(sp), intent(in) :: value
        character(len=*), intent(in), optional :: format

        call self%append_char(to_string(value, format))
    end subroutine append_real_sp

    pure subroutine append_real_dp(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        real(dp), intent(in) :: value
        character(len=*), intent(in), optional :: format

        call self%append_char(to_string(value, format))
    end subroutine append_real_dp

    pure subroutine append_complex_sp(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        complex(sp), intent(in) :: value
        character(len=*), intent(in), optional :: format

        call self%append_char(to_string(value, format))
    end subroutine append_complex_sp

    pure subroutine append_complex_dp(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        complex(dp), intent(in) :: value
        character(len=*), intent(in), optional :: format

        call self%append_char(to_string(value, format))
    end subroutine append_complex_dp

    pure subroutine append_logical(self, value, format)
        class(stringbuilder_type), intent(inout) :: self
        logical, intent(in) :: value
        character(len=*), intent(in), optional :: format

        if (present(format)) then
            call self%append_char(to_string(value, format))
        else
            call self%append_char(to_string(value))
        end if
    end subroutine append_logical

    pure subroutine append_repeat(self, value, count)
        class(stringbuilder_type), intent(inout) :: self
        character(len=*), intent(in) :: value
        integer, intent(in) :: count
        integer :: i

        if (count <= 0 .or. len(value) == 0) return
        call self%grow(self%len_used + count*len(value))
        do i = 1, count
            call self%append_char(value)
        end do
    end subroutine append_repeat

    pure subroutine append_line_char(self, value)
        class(stringbuilder_type), intent(inout) :: self
        character(len=*), intent(in), optional :: value

        if (present(value)) call self%append_char(value)
        call self%append_char(new_line('a'))
    end subroutine append_line_char

    pure subroutine append_line_string(self, value)
        class(stringbuilder_type), intent(inout) :: self
        type(string_type), intent(in) :: value

        call self%append_line_char(char(value))
    end subroutine append_line_string

    pure subroutine insert_char(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        character(len=*), intent(in) :: value
        integer :: idx, n

        n = len(value)
        if (n == 0) return
        idx = min(max(index, 1), self%len_used + 1)
        call self%grow(self%len_used + n)
        if (idx <= self%len_used) then
            self%buffer(idx + n:self%len_used + n) = self%buffer(idx:self%len_used)
        end if
        self%buffer(idx:idx + n - 1) = value
        self%len_used = self%len_used + n
    end subroutine insert_char

    pure subroutine insert_string(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        type(string_type), intent(in) :: value

        call self%insert_char(index, char(value))
    end subroutine insert_string

    pure subroutine insert_int32(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        integer(int32), intent(in) :: value

        call self%insert_char(index, to_string(value))
    end subroutine insert_int32

    pure subroutine insert_int64(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        integer(int64), intent(in) :: value

        call self%insert_char(index, to_string(value))
    end subroutine insert_int64

    pure subroutine insert_real_sp(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        real(sp), intent(in) :: value

        call self%insert_char(index, to_string(value))
    end subroutine insert_real_sp

    pure subroutine insert_real_dp(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        real(dp), intent(in) :: value

        call self%insert_char(index, to_string(value))
    end subroutine insert_real_dp

    pure subroutine insert_logical(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        logical, intent(in) :: value

        call self%insert_char(index, to_string(value))
    end subroutine insert_logical

    pure function to_string_all(self) result(string)
        class(stringbuilder_type), intent(in) :: self
        character(len=:), allocatable :: string

        if (self%len_used == 0) then
            string = ""
        else
            string = self%buffer(1:self%len_used)
        end if
    end function to_string_all

    pure function to_string_range(self, first, last) result(string)
        class(stringbuilder_type), intent(in) :: self
        integer, intent(in) :: first, last
        character(len=:), allocatable :: string
        integer :: f, l

        f = max(first, 1)
        l = min(last, self%len_used)
        if (f > l) then
            string = ""
        else
            string = self%buffer(f:l)
        end if
    end function to_string_range

    pure subroutine remove(self, first, last)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: first, last
        integer :: f, l, removed

        f = max(first, 1)
        l = min(last, self%len_used)
        if (f > l) return
        removed = l - f + 1
        if (l < self%len_used) then
            self%buffer(f:self%len_used - removed) = self%buffer(l + 1:self%len_used)
        end if
        self%len_used = self%len_used - removed
    end subroutine remove

    pure subroutine replace(self, old, new)
        class(stringbuilder_type), intent(inout) :: self
        character(len=*), intent(in) :: old, new
        character(len=:), allocatable :: replaced

        if (len(old) == 0 .or. self%len_used == 0) return
        replaced = replace_all(self%buffer(1:self%len_used), old, new)
        self%len_used = 0
        call self%append_char(replaced)
    end subroutine replace

    elemental function length(self) result(res)
        class(stringbuilder_type), intent(in) :: self
        integer :: res

        res = self%len_used
    end function length

    pure subroutine set_length(self, new_length)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: new_length
        integer :: n

        n = max(new_length, 0)
        if (n > self%len_used) then
            call self%grow(n)
            self%buffer(self%len_used + 1:n) = ""
        end if
        self%len_used = n
    end subroutine set_length

    elemental function capacity(self) result(res)
        class(stringbuilder_type), intent(in) :: self
        integer :: res

        if (allocated(self%buffer)) then
            res = len(self%buffer)
        else
            res = 0
        end if
    end function capacity

    pure subroutine ensure_capacity(self, required)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: required

        if (required > 0) call self%grow(required)
    end subroutine ensure_capacity

    pure subroutine clear(self)
        class(stringbuilder_type), intent(inout) :: self

        self%len_used = 0
    end subroutine clear

    elemental function char_at(self, index) result(res)
        class(stringbuilder_type), intent(in) :: self
        integer, intent(in) :: index
        character(len=1) :: res

        if (index >= 1 .and. index <= self%len_used) then
            res = self%buffer(index:index)
        else
            res = " "
        end if
    end function char_at

    pure subroutine set_char_at(self, index, value)
        class(stringbuilder_type), intent(inout) :: self
        integer, intent(in) :: index
        character(len=1), intent(in) :: value

        if (index >= 1 .and. index <= self%len_used) then
            self%buffer(index:index) = value
        end if
    end subroutine set_char_at

    elemental function builder_eq_builder(lhs, rhs) result(res)
        type(stringbuilder_type), intent(in) :: lhs, rhs
        logical :: res

        res = lhs%len_used == rhs%len_used
        if (res .and. lhs%len_used > 0) then
            res = lhs%buffer(1:lhs%len_used) == rhs%buffer(1:rhs%len_used)
        end if
    end function builder_eq_builder

    elemental function builder_eq_char(lhs, rhs) result(res)
        type(stringbuilder_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: res

        res = lhs%len_used == len(rhs)
        if (res .and. lhs%len_used > 0) then
            res = lhs%buffer(1:lhs%len_used) == rhs
        end if
    end function builder_eq_char

    elemental function char_eq_builder(lhs, rhs) result(res)
        character(len=*), intent(in) :: lhs
        type(stringbuilder_type), intent(in) :: rhs
        logical :: res

        res = builder_eq_char(rhs, lhs)
    end function char_eq_builder

    elemental function builder_ne_builder(lhs, rhs) result(res)
        type(stringbuilder_type), intent(in) :: lhs, rhs
        logical :: res

        res = .not. builder_eq_builder(lhs, rhs)
    end function builder_ne_builder

    elemental function builder_ne_char(lhs, rhs) result(res)
        type(stringbuilder_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: res

        res = .not. builder_eq_char(lhs, rhs)
    end function builder_ne_char

    elemental function char_ne_builder(lhs, rhs) result(res)
        character(len=*), intent(in) :: lhs
        type(stringbuilder_type), intent(in) :: rhs
        logical :: res

        res = .not. builder_eq_char(rhs, lhs)
    end function char_ne_builder

end module stdlib_stringbuilder
