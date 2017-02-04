#5x5, 1player Battleship
### NEED TO DEAL WITH BLANK RESPONSES; don't enter blanks
### >1 battleship!?
### different size 'oceans'?
### 2 player?
### session w/l ratio?
from random import randint 

board =[]
for c in range(0,5):
    board.append(["O"]*5) #5x unique 5e list
print board #debugging

def prettyprint_board(board):
    for row in board:
        print " ".join(row)

print_board(board)


def random_row(board):
    ship_row = randint(0,len(board)-1)
    return ship_row
    #return randint(0, len(board) - 1)

def random_col(board):
    ship_col = randint(0,len(board)-1)
    return ship_col
    #    return randint(0, len(board[0]) - 1)
    
random_row(board)
random_col(board)

#prev. ship_ def was local, these are global:
ship_row = random_row(board)
ship_col = random_col(board)


for turn in range(4):
    print "Turn", turn+1
    guess_row = int(raw_input("Guess Row:"))
    guess_col = int(raw_input("Guess Col:"))

    if guess_row == ship_row and guess_col == ship_col:
        print "Congratulations! You sunk my battleship!"
        break #win condition met
    else:
        if (guess_row < 0 or guess_row > 4) or (guess_col < 0 or guess_col > 4):
            print "Oops, that's not even in the ocean."
            if turn == 3:
                print "Game Over"
        elif(board[guess_row][guess_col] == "X"):
            print "You guessed that one already."
            if turn == 3:
                print "Game Over"
        else:
            print "You missed my battleship!"
            board[guess_row][guess_col] = "X"
            if turn == 3:
                print "Game Over"
        turn + 1
        # if turn == 4:
        # 	print "Game Over" #why doesn't this catch-all game overs?
	print_board(board)