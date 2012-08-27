"""
Written by Yusuf Ozenci.
This program simulates a graphics editor. A 2d list is created, wich represents a matrix of colours (i.e. the image).
Text commands can be input into the console to edit the image.
7 Commands can be used, these are:

I M N. Create a new M x N image with all pixels coloured white (O).

C. Clears the table, setting all  pixels to white (O).

L X Y C. Colours the pixel (X,Y) with colour C.

V X Y1 Y2 C. Draw a vertical segment of colour C in column X between rows Y1 and Y2 (inclusive).

H X1 X2 Y C. Draw a horizontal segment of colour C in row Y between columns X1 and X2 (inclusive).

F X Y C. Fill the region R with the colour C. R is defined as: Pixel (X,Y)
belongs to R. Any other pixel which is the same colour as (X,Y) and
shares a common side with any pixel in R also belongs to this region.

S. Show the contents of the current image

X. Terminate the session
"""

def flood(array, x, y, target, new, q, r):
    """
    Input: array, x_coordinate, y_coordinate, target_pixel_colour, new_colour, max_column_no, max_row_no
    
    Performs a flood fill from point x and y. If the pixel selected at (x, y) is the colour of the target_pixel_colour
    then the bounded region of the same colour is filled with the new_colour.
    """

    if array[x][y] == target:
        array[x][y] = new
        if x > 0:            
            flood(array, x - 1, y, target, new, q, r)
        if x < q:  
            flood(array, x + 1, y, target, new, q, r)
        if y > 0:            
            flood(array, x, y - 1, target, new, q, r)
        if y < r:
            flood(array, x ,y + 1 ,target ,new ,q ,r)

MatrixCreated = False
            
# Here we go...
while(True):
    
    a = raw_input('')
    command = a.split()

    
    # Create a new M x N image with all pixels coloured white (O).    
    if command[0] == 'I':
        MatrixCreated = True
        M=int(command[1])
        N=int(command[2])
        if M>250 or N>250 or M<1 or N<1:
            print "Today we cannot produce a matrix of that size, must be between 1 and 250. Thank you!"
            break
        imagematrix = [['O' for i in range(N)] for j in range(M)]

    # Clears the table, setting all pixels to white (O).
    elif a[0] == 'C':
        
        if MatrixCreated == False:
            print 'Please create the matrix with the I command first.'
        else:
            imagematrix = [['O' for i in range(N)] for j in range(M)]
                
    # Colours pixel x and y with colour c.
    elif command[0] == 'L':
        if MatrixCreated == False:
            print 'Please create the matrix with the I command first.'
        else:        
            X = int(command[1])
            Y = int(command[2])
            imagematrix[X - 1][Y - 1] = command[3]
        
    # Draw a vertical segment of colour C in column X between rows Y1 and Y2 (inclusive).
    # V Xpos Y1 Y2 C
    elif command[0] == 'V':
        if MatrixCreated == False:
            print 'Please create the matrix with the I command first.'
        else:
            Xpos = int(command[1])
            Y1 = int(command[2])
            Y2 = int(command[3])
            if Y1 > N and Y2 > N:
                print 'The given position does not exist in the matrix. '
                break 
            for i in range(Y1 - 1,Y2):
                imagematrix[Xpos - 1][i]=command[4]

    # Draw a horizontal segment of colour C in row Y between columns X1 and X2 (inclusive).
    # H X1 X2 Y C.
    elif command[0] == 'H':
        if MatrixCreated == False:
            print 'Please create the matrix with the I command first.'
        else:
            X1 = int(command[1])
            X2 = int(command[2])
            Ypos = int(command[3])
            if X1 > M and X2 > M:
                print 'The given position does not exist in the matrix. '
                break 
            for i in range(X1 - 1, X2):
                imagematrix[i][Ypos - 1] = command[4]

    # Fill the region R with the colour C. R is defined as: Pixel (X,Y) belongs to R.
    # Any other pixel which is the same colour as (X,Y) and shares a common side with
    # any pixel in R also belongs to this region.
    # F X Y C.    
    elif command[0] == 'F':
        if MatrixCreated == False:
            print 'Please create the matrix with the I command first.'
        else:
            S1 = int(command[1]) - 1
            S2 = int(command[2]) - 1
            q = M - 1
            r = N - 1
            flood(imagematrix, S1, S2, imagematrix[S1][S2], command[3], q, r)

    # Show the contents of the current image.
    elif command[0] == 'S':
        if MatrixCreated == False:
            print 'Please create the matrix with the I command first.'
        else:
            print '\n'
            for i in range(0,N):
                for j in range(0,M):
                    print imagematrix[j][i],
                print 

    # Terminate the session.
    elif command[0] == 'X':
        break

    # Verify illegal commands cannot be entered.
    else:
        print "This is an invalid command. Please consult the PDF for valid commands."
        





