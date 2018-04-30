all: run

run:
	gfortran TicTacToe.f95 -Wall -o play
clean:
	rm play
