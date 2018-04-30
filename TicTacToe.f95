! TicTacToe.f95
! Made by: Andrew Maklingham
! Date: Feb,2,2018

program tictactoe
! The tictactoe program. This program plays a game of tictactoe using the player's input
! and a computer respond to the move of the player.
    implicit none
    logical :: r, OVER
    character(len=1), dimension(3,3) :: TICTAC, WINNER
    TICTAC(1,1) = " "
    TICTAC(1,2) = " "
    TICTAC(1,3) = " "
    TICTAC(2,1) = " "
    TICTAC(2,2) = " "
    TICTAC(2,3) = " "
    TICTAC(3,1) = " "
    TICTAC(3,2) = " "
    TICTAC(3,3) = " "
    r = .TRUE.
    write (*,*) 'Welcome to the Game of Tic Tac Toe'
    write (*,*) 'Please enter a number from 1 to 9 in order to play.'
    write (*,*) 'Each number is linked to a box on the grid.'
    write (*,*) 'Example:'
    write (*,*) '           | 1 | 2 | 3 |'
    write (*,*) '           | 4 | 5 | 6 |'
    write (*,*) '           | 7 | 8 | 9 |'
    write (*,*) ''

    do while (r)
        call playtictactoe(TICTAC)
        call chkovr(TICTAC, OVER, WINNER)
        if (OVER .eqv. .TRUE.)then
            r = .FALSE.
            exit
        end if
        call pickMove(TICTAC)
        write (*,*) 'Computer Turn:'
        call showBoard(TICTAC)
        call chkovr(TICTAC, OVER, WINNER)
        if (OVER .eqv. .TRUE.)then
            r = .FALSE.
            exit
        end if
    end do

    if (WINNER(1,1) == 'D')then
        write (*,*) 'Game ended in a draw!'
    else if (WINNER(1,1) == 'X')then
        write (*,*) 'Player Wins!'
    else if (WINNER(1,1) == 'O')then
        write (*,*) 'Computer Wins!'
    end if

contains


logical function chkplay(TICTAC,MOVE)
! Checks to make sure the human player cannot make a play in a square that is already occupied.
    character(len=1), intent(inout), dimension(3,3) :: TICTAC
    integer, intent(in) :: MOVE

    select case (MOVE)
        case (49)
            if(TICTAC(1,1) == ' ')then
                TICTAC(1,1) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (50)
            if(TICTAC(1,2) == ' ')then
                TICTAC(1,2) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (51)
            if(TICTAC(1,3) == ' ')then
                TICTAC(1,3) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (52)
            if(TICTAC(2,1) == ' ')then
                TICTAC(2,1) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (53)
            if(TICTAC(2,2) == ' ')then
                TICTAC(2,2) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (54)
            if(TICTAC(2,3) == ' ')then
                TICTAC(2,3) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (55)
            if(TICTAC(3,1) == ' ')then
                TICTAC(3,1) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (56)
            if(TICTAC(3,2) == ' ')then
                TICTAC(3,2) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case (57)
            if(TICTAC(3,3) == ' ')then
                TICTAC(3,3) = 'X'
                chkplay = .TRUE.
            else
                chkplay = .FALSE.
            end if
        case default
            chkplay = .FALSE.
    end select
end function chkplay

logical function same(spot1, spot2, spot3)
! A logical function which tests a row, column or diagonal returns a value of true if all three elements
! are the same (‘X’ or ‘O’); otherwise returns false.
    character(len=1), intent(in) :: spot1, spot2, spot3
    if (spot1 == ' ' .OR. spot2 == ' ' .OR. spot3 == ' ')then
        same = .FALSE.
    else if (spot1 == 'X' .AND. spot2 == 'X' .AND. spot3 == 'X')then
        same = .TRUE.
    else if (spot1 == 'O' .AND. spot2 == 'O' .AND. spot3 == 'O')then
        same = .TRUE.
    else
        same = .FALSE.
    end if
end function same

! Performs the computers move. An algorithm for the computer is described below.
subroutine pickMove(TICTAC)
    implicit none
    character(len=1), intent(inout), dimension(3,3) :: TICTAC
    integer :: i, xCount, oCount, numInt
    real :: num
    logical :: valid, move

    valid = .TRUE.
    xCount = 0
    oCount = 0

! First check and see if any rows for a win chance
    do i = 1, 3
        oCount = 0
        xCount = 0
        if (TICTAC(i,1) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(i,2) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(i,3) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(i,1) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(i,2) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(i,3) == 'X') then
            xCount = xCount + 1
        end if

        if (oCount == 2 .and. xCount == 0)then
            TICTAC(i,1) = 'O'
            TICTAC(i,2) = 'O'
            TICTAC(i,3) = 'O'
            return
        end if
    end do

! If no win chance in rows, check and see if any columns for a win chance
    do i = 1, 3
        oCount = 0
        xCount = 0
        if (TICTAC(1,i) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(2,i) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(3,i) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(1,i) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(2,i) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(3,i) == 'X') then
            xCount = xCount + 1
        end if

        if (oCount == 2 .and. xCount == 0)then
            TICTAC(1,i) = 'O'
            TICTAC(2,i) = 'O'
            TICTAC(3,i) = 'O'
            return
        end if
    end do

! If no win chance in columns, check and see if any of the 2 diagonals for a win chance
!  DIAGONAL #1 (top left, to bottom right)
    oCount = 0
    xCount = 0
    if (TICTAC(1,1) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(2,2) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(3,3) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(1,1) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(2,2) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(3,3) == 'X') then
        xCount = xCount + 1
    end if

    if (oCount == 2 .and. xCount == 0)then
        TICTAC(1,1) = 'O'
        TICTAC(2,2) = 'O'
        TICTAC(3,3) = 'O'
        return
    end if

!  DIAGONAL #2 (bottom left to top right)
    oCount = 0
    xCount = 0
    if (TICTAC(3,1) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(2,2) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(1,3) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(3,1) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(2,2) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(1,3) == 'X') then
        xCount = xCount + 1
    end if

    if (oCount == 2 .and. xCount == 0)then
        TICTAC(3,1) = 'O'
        TICTAC(2,2) = 'O'
        TICTAC(1,3) = 'O'
        return
    end if

! If there is no chance to win, then computer must block player from winning
!First check rows for a block chance
    do i = 1, 3
        oCount = 0
        xCount = 0
        if (TICTAC(i,1) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(i,2) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(i,3) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(i,1) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(i,2) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(i,3) == 'X') then
            xCount = xCount + 1
        end if

        if (xCount == 2 .and. oCount == 0)then
            if (TICTAC(i,1) == ' ') then
                TICTAC(i,1) = 'O'
            end if
            if (TICTAC(i,2) == ' ') then
                TICTAC(i,2) = 'O'
            end if
            if (TICTAC(i,3) == ' ') then
                TICTAC(i,3) = 'O'
            end if
            return
        end if
    end do

! Then check columns for block chance
    do i = 1, 3
        oCount = 0
        xCount = 0
        if (TICTAC(1,i) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(2,i) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(3,i) == 'O') then
            oCount = oCount + 1
        end if
        if (TICTAC(1,i) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(2,i) == 'X') then
            xCount = xCount + 1
        end if
        if (TICTAC(3,i) == 'X') then
            xCount = xCount + 1
        end if

        if (xCount == 2 .and. oCount == 0)then
            if (TICTAC(1,i) == ' ') then
                TICTAC(1,i) = 'O'
            end if
            if (TICTAC(2,i) == ' ') then
                TICTAC(2,i) = 'O'
            end if
            if (TICTAC(3,i) == ' ') then
                TICTAC(3,i) = 'O'
            end if
            return
        end if
    end do

! If no block chance in columns or rows, check and see if any of the 2 diagonals for a block chance
! DIAGONAL #1 (top left, to bottom right)
    oCount = 0
    xCount = 0
    if (TICTAC(1,1) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(2,2) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(3,3) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(1,1) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(2,2) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(3,3) == 'X') then
        xCount = xCount + 1
    end if

    if (xCount == 2 .and. oCount == 0)then
        if (TICTAC(1,1) == ' ') then
            TICTAC(1,1) = 'O'
        end if
        if (TICTAC(2,2) == ' ') then
            TICTAC(2,2) = 'O'
        end if
        if (TICTAC(3,3) == ' ') then
            TICTAC(3,3) = 'O'
        end if
        return
    end if

! DIAGONAL #2 (bottom left to top right)
    oCount = 0
    xCount = 0
    if (TICTAC(3,1) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(2,2) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(1,3) == 'O') then
        oCount = oCount + 1
    end if
    if (TICTAC(3,1) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(2,2) == 'X') then
        xCount = xCount + 1
    end if
    if (TICTAC(1,3) == 'X') then
        xCount = xCount + 1
    end if

    if (xCount == 2 .and. oCount == 0)then
        if (TICTAC(3,1) == ' ') then
            TICTAC(3,1) = 'O'
        end if
        if (TICTAC(2,2) == ' ') then
            TICTAC(2,2) = 'O'
        end if
        if (TICTAC(1,3) == ' ') then
            TICTAC(1,3) = 'O'
        end if
        return
    end if

! If computer cannot play to block or win, then pick a valid square at random as the computer's move

    call init_random_seed()
    do while(valid)
        call RANDOM_NUMBER(num)
        num = num * 10
        numInt = int(num)
        numInt = numInt + 48
        move = chkplay(TICTAC, numInt)
        if (move .eqv. .TRUE.) then
            select case (numInt)
                case (49)
                    TICTAC(1,1) = 'O'
                case (50)
                    TICTAC(1,2) = 'O'
                case (51)
                    TICTAC(1,3) = 'O'
                case (52)
                    TICTAC(2,1) = 'O'
                case (53)
                    TICTAC(2,2) = 'O'
                case (54)
                    TICTAC(2,3) = 'O'
                case (55)
                    TICTAC(3,1) = 'O'
                case (56)
                    TICTAC(3,2) = 'O'
                case (57)
                    TICTAC(3,3) = 'O'
                case default
            end select
            exit
        end if
    end do
    return
end subroutine pickMove

subroutine init_random_seed()
! This function is the random seed generator, it will crate a new seed for the random number generator
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed

    call RANDOM_SEED(size = n)
    allocate(seed(n))

    call SYSTEM_CLOCk(COUNT = clock)

    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)

    deallocate(seed)
end subroutine init_random_seed


subroutine chkovr(TICTAC, OVER, WINNER)
!
! CHECK IF TIC-TAC-TOE IS OVER AND DETERMINE WINNER (IF ANY)
!
! ARGUMENT DEFINITIONS --
!    INPUT ARGUMENTS
!        TICTAC - REPRESENTS THE CURRENT STATE OF THE BOARD GAME
!    OUTPUT ARGUMENTS
!        OVER - INDICATES WHETHER OR NOT GAME IS OVER
!        WINNER - INDICATES THE WINNER (O OR X) OR A DRAW (D)
    character(len=1), intent(inout), dimension(3,3) :: TICTAC, WINNER
    logical, intent(inout) :: OVER
!
! SUBROUTINE PARAMETERS
      character(len=1) :: BLANK = ' ', DRAW = 'D'
!
! FUNCTIONS USED
    !LOGICAL :: SAME
!
! LOCAL VARIABLES
    LOGICAL :: DSAME
    INTEGER :: IR, IC
! ASSUME GAME IS OVER AT START
      OVER = .TRUE.
!
! CHECK FOR A WINNER
! CHECK ROWS FOR A WINNER
    DO IR = 1, 3
        IF (SAME(TICTAC(IR,1), TICTAC(IR,2), TICTAC(IR,3))) THEN
            WINNER = TICTAC(IR,1)
            RETURN
        END IF
    end do
! NO WINNER BY ROWS, CHECK COLUMNS FOR A WINNER
    DO IC = 1, 3
        IF (SAME(TICTAC(1,IC), TICTAC(2,IC), TICTAC(3,IC))) THEN
            WINNER = TICTAC(1,IC)
            RETURN
        ENDIF
    end do
! NO WINNER BY ROWS OR COLUMNS, CHECK DIAGONALS FOR A WINNER
    DSAME = SAME(TICTAC(1,1), TICTAC(2,2), TICTAC(3,3)) .OR. SAME(TICTAC(1,3), TICTAC(2,2), TICTAC(3,1))
      IF (DSAME) THEN
          WINNER = TICTAC(2,2)
          RETURN
      END IF
! NO WINNER AT ALL. SEE IF GAME IS A DRAW
! CHECK EACH ROW FOR AN EMPTY SPACE
    DO IR = 1, 3
        DO IC = 1, 3
            IF (TICTAC(IR,IC) .EQ. BLANK) THEN
                OVER = .FALSE.
                RETURN
            END IF
        end do
    end do
! NO BLANK FOUND, GRAME IS A DRAW
      WINNER = DRAW
!
    RETURN
end subroutine chkovr

subroutine showBoard(TICTAC)
! Prints out the tic-tac-toe board, each time a player moves.
    implicit none
    character(len=1), intent(inout), dimension(3,3) :: TICTAC
    write (*,*) '           | ', TICTAC(1,1), ' | ', TICTAC(1,2), ' | ', TICTAC(1,3), ' |'
    write (*,*) '           | ', TICTAC(2,1), ' | ', TICTAC(2,2), ' | ', TICTAC(2,3), ' |'
    write (*,*) '           | ', TICTAC(3,1), ' | ', TICTAC(3,2), ' | ', TICTAC(3,3), ' |'
    write (*,*) ''
end subroutine showBoard

subroutine playtictactoe(TICTAC)
! Subroutine which plays the game. Assume one player is human and the other player is a computer.
! This subroutine would include the humans move, but not the computers move
! (which is obviously a little more complicated - see function computerMove().
    implicit none
    character(len=1), intent(inout), dimension(3,3) :: TICTAC
    write (*,*) 'Player Turn:'
    call getMove(TICTAC)
    write (*,*) 'Your move:'
    call showBoard(TICTAC)
end subroutine playtictactoe

subroutine getMove(TICTAC)
! Gets the humans move. The output from getMove() is a number from 1 to 9 representing
! the chosen square. Should validate player input.
    implicit none
    character(len=1), intent(inout), dimension(3,3) :: TICTAC
    character :: r
    character(len=10) :: b
    integer :: value, length
    logical :: valid, work
    valid = .TRUE.
    do while (valid)
        write (*,*) 'PLease enter a number: '
        read (*,*) b
        r = b(1:1)
        value = iachar(r)
        length = LEN_TRIM(b)
        if (length > 1)then
            valid = .TRUE.
            write (*,*) 'Value entered is not valid.'
        else if (value > 48 .and. value < 58)then
            !
            ! If it is a valid guess, check if the spot is already filled
            !
            work = chkplay(TICTAC,value)
            if (work .eqv. .TRUE.)then
                valid = .FALSE.
            else
                write (*,*) 'Square occupied.'
                valid = .TRUE.
            end if

        else
            valid = .TRUE.
            write (*,*) 'Value entered is not valid.'
        end if
    end do
end subroutine getMove

end program
