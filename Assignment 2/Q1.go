package main

import (
	"fmt"
	"math/rand"
)

func main() {

	// Create n rows and m columns
	var n, m int

	// Create the channels for the Police and Thief positions, the Police and Thief controller messages, and a channel to end the program when one condition to end the game is met.
	policePositionX := make(chan int)
	policePositionY := make(chan int)
	thiefPositionX := make(chan int)
	thiefPositionY := make(chan int)
	policeMessage := make(chan int)
	thiefMessage := make(chan int)
	done := make(chan bool)

	// Generate two random number in the range [10,200] and assign one to n and one to m.
	n = rand.Intn(191) + 10
	m = rand.Intn(191) + 10

	// Find the maximum number between n and m.
	max := n
	if m > n {
		max = m
	}

	// Generate a random number in the range [2max(n,m),10max(n,m)] and assign it to s.
	s := rand.Intn(9*max) + 2*max

	// Start the goroutines for police, thief, and controller.
	go police(policePositionX, policePositionY, n, m, policeMessage)
	go thief(thiefPositionX, thiefPositionY, n, m, thiefMessage)
	go controller(policePositionX, policePositionY, thiefPositionX, thiefPositionY, s, policeMessage, thiefMessage, done)

	// Controller keeps running until the value in the channel done is true.
	for !<-done {
	}

}

func controller(policePositionX chan int, policePositionY chan int, thiefPositionX chan int, thiefPositionY chan int, s int, policeMessage chan int, thiefMessage chan int, done chan bool) {

	// Boolean to create the for loop for the controller to run until one of the conditions to end the game is met.
	var end bool
	end = false

	for !end {

		// Read values from the police and thief coordinates channels.
		policeX := <-policePositionX
		policeY := <-policePositionY
		thiefX := <-thiefPositionX
		thiefY := <-thiefPositionY

		// Prints the status of the game after each move.
		fmt.Printf("The Police Position = (%d,%d), The Thief Position = (%d,%d).\n", policeX, policeY, thiefX, thiefY)

		// Deals with the conditions to end the game.
		// The messages that will be sent into policeMessage and thiefMessage are as follows.
		// 1 - Player wins.
		// 0 - Tie.
		// -1 - Player loses.
		// 2 - Movement is sucessful and the game continues.
		// If the x and y coordinates of the police and thief are the same.

		if thiefX == 0 && thiefY == 0 && policeX == 0 && policeY == 0 {
			fmt.Println("The game ends in a tie.")
			end = true
			done <- true

			// Send 0 to the police player and 0 to the thief player.
			policeMessage <- 0
			thiefMessage <- 0
		} else if policeX == thiefX && policeY == thiefY && thiefX != 0 && thiefY != 0 && policeX != 0 && policeY != 0 {
			fmt.Printf("The Police caught the Thief at (%d,%d) and won the game.", policeX, policeY)
			end = true
			done <- true

			// Send 1 to the police player and -1 to the thief player.
			policeMessage <- 1
			thiefMessage <- -1
		} else if thiefX == 0 && thiefY == 0 && policeX != 0 && policeY != 0 {
			fmt.Println("The Thief escaped and won the game.")
			end = true
			done <- true

			// Send -1 to the police player and 1 to the thief player.
			policeMessage <- -1
			thiefMessage <- 1
		} else {
			// Decrement the counter that stores the number of police moves.
			s = s - 1

			// Check the value of s immediately after decrementing it. It is -1, that means all moves have been used up by the police player.
			if s == -1 {
				end = true
				fmt.Printf("The Police ran out of moves and the Thief won the game.")
				done <- true

				// Send -1 to the police player and 1 to the thief player.
				policeMessage <- -1
				thiefMessage <- 1
			} else {
				// Send 2 to the police player and 2 to the thief player.
				policeMessage <- 2
				thiefMessage <- 2
			}
		}

		// Done is false after every move.
		done <- false
	}
}

func police(policePositionX chan int, policePositionY chan int, n int, m int, policeMessage chan int) {

	// x and y coordinates for the position of the player. It starts off at coordinates (0,0), the top left corner of the board.
	x := 0
	y := 0

	// Boolean to create the condition for the for loop for the player to run until one of the conditions to end the game is met.
	var end bool
	end = false

	for !end {

		// Send the current values of the x and y coordinates of the police player into the police coordinates channels.
		policePositionX <- x
		policePositionY <- y

		// Randomly generate a number, 0 or 1, which will determine the way the player moves.
		// 0 will select the x axis, which will move across the columns.
		// 1  will select the y axis, which will move up and down the rows.
		direction := rand.Intn(2)

		if direction == 0 {
			if x <= 0 {
				x = x + 1
			} else if x >= m {
				x = x - 1
			} else {
				if rand.Intn(2) == 0 {
					x += 1
				} else {
					x -= 1
				}
			}
		} else {
			if y <= 0 {
				y = y + 1
			} else if y >= n {
				y = y - 1
			} else {
				if rand.Intn(2) == 0 {
					y += 1
				} else {
					y -= 1
				}
			}
		}

		// Check the value of policeMessage. If it is 1, -1, or 0, end the loop.
		message := <-policeMessage
		if message == 1 || message == -1 || message == 0 {
			end = true
		}
	}
}

func thief(thiefPositionX chan int, thiefPositionY chan int, n int, m int, thiefMessage chan int) {

	// x and y coordinates for the position of the player. It starts off at (n, m), the bottom right corner of the board.
	x := n
	y := m

	// Boolean to create the condition for the for loop for the player to run until one of the conditions to end the game is met.
	var end bool
	end = false

	for !end {

		// Send the current values of the x and y coordinates of the thief player into the thief coordinates channels.
		thiefPositionX <- x
		thiefPositionY <- y

		// Randomly generate a number, 0 or 1, which will determine the way the player moves.
		// 0 will select the x axis, which will move across the columns.
		// 1  will select the y axis, which will move up and down the rows.
		direction := rand.Intn(2)

		if direction == 0 {
			if x <= 0 {
				x = x + 1
			} else if x >= m {
				x = x - 1
			} else {
				if rand.Intn(2) == 0 {
					x += 1
				} else {
					x -= 1
				}
			}
		} else {
			if y <= 0 {
				y = y + 1
			} else if y >= n {
				y = y - 1
			} else {
				if rand.Intn(2) == 0 {
					y += 1
				} else {
					y -= 1
				}
			}
		}

		// Check the value of thiefMessage. If it is 1, -1, or 0, end the loop.
		message := <-thiefMessage
		if message == 1 || message == -1 || message == 0 {
			end = true
		}
	}
}
